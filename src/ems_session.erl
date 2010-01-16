-module(ems_session).
-behaviour(gen_server).
-include("erlang_media_server.hrl").
-include ("sdp.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-record(state, {id, path, owner, description, channels}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
	]).
	
-export([
  start_link/3,
  receive_rtsp_request/5]).

%% ============================================================================
%% Exports
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts a new session and returns the new session's process identifier.
%% @spec start_link(Id, Path, Desc) -> Pid
%%       Id = integer()
%%       Path = string()
%%       OwnerPid = pid()
%% @end
%% ----------------------------------------------------------------------------
start_link(Id, Path, OwnerPid) ->
  ?LOG_DEBUG("ems_session:start_link/2 - Id: ~w, Path: ~s", [Id, Path]),
  gen_server:start_link(?MODULE, {Id, Path, OwnerPid}, []).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
receive_rtsp_request(SessionPid, Request, Headers, Body, ConnectionPid) ->
  gen_server:cast(SessionPid, {rtsp_request, Request, Headers, Body, ConnectionPid}).
  
%% ============================================================================
%% Server Callbacks
%% ============================================================================

init({Id, Path, OwnerPid}) ->
  ?LOG_DEBUG("ems_session:init/3 - ~s", [Path]),
  State = #state{id=Id, path=Path, owner=OwnerPid},
  {ok, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a synchronous request from another process.
%% @spec handle_call(Request, From, State) -> Result
%%       Request =
%%       From = pid()
%%       State = 
%%       Result = {reply, Reply, NewState}
%% @end 
%% ----------------------------------------------------------------------------

%% The default implementation. Swallows the message.
handle_call(Request, From, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_cast({rtsp_request, Request, Headers, Body, Connection}, State) ->
  ?LOG_DEBUG("ems_session:handle_cast/2 - Handling RTSP request", []),

  Method = Request#rtsp_request.method,
  Sequence = Headers#rtsp_message_header.sequence,
  try
    NewState = handle_request(Method, Sequence, Request, Headers, Body, 
      Connection, State),
    {noreply, NewState}
  catch
    ems_session:Error -> 
      rtsp_connection:send_error(Connection, Error),
      {noreply, State}
  end;
  
handle_cast(Request, State) ->
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
handle_info(Info, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
terminate(Reason, State) ->
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
code_change(OldVersion, State, Extra) ->
  {ok, State}.
  
%% ============================================================================
%% Internal functions
%% ============================================================================

% Handles an RTSP ANNOUNCE request by parsing the SDP session description and 
% creating the media channels
handle_request(announce, Sequence, Request, Headers, Body, Connection, State) ->
  {_, _, _, ContentLength, ContentType} = rtsp:get_request_info(Request,Headers),
  
  if
    ContentType /= "application/sdp" ->
      throw({ems_session, bad_request});
      
    true ->
      SessionDescription = sdp:parse(Body),
      {ok, Channels} = create_channels(SessionDescription),
      NewState = State#state{description=SessionDescription, channels=Channels},
      rtsp_connection:send_response(Connection, Sequence, ok, [], <<>>),
      NewState
  end;

handle_request(setup, Sequence, Request, Headers, Body, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_request/7 - SETUP", []),
  
  Uri = Request#rtsp_request.uri,
  {_,_,_,Path} = url:parse(Uri),
  SessionPath = State#state.path,
  StreamName = string:substr(Path, length(SessionPath)+2),

  % look for the client's transport header 
  case rtsp:get_header(Headers, ?RTSP_TRANSPORT) of
    ClientHeader when is_list(ClientHeader) ->

      % Parse transport header and use the parsed data to try and set up the
      % stream
      ClientTransport = rtsp:parse_transport(ClientHeader),      
      
      case setup_stream(StreamName, ClientTransport, State) of
        {ServerTransport, NewState} ->
          ServerHeader = rtsp:format_transport(ServerTransport),
          ServerHeaders = [{?RTSP_TRANSPORT, ServerHeader}],
          rtsp_connection:send_response(Connection, Sequence, ok, ServerHeaders, <<>>),
          NewState;
          
        {error, Reason} ->
          rtsp_connection:send_response(Connection, Sequence, unsupported_transport, Headers, <<>>),
          State
      end;
       
    _ ->
      % No transport header in the setup request (or the transport header is 
      % something other than a string). That's very bad, and the client should
      % be punished. 
      throw({ems_session, bad_request})
  end;

handle_request(Method, Sequence, Request, Headers, Body, Connection, State) ->
  rtsp_connection:send_response(Connection, not_implemented, [], <<>>),
  State.

%% ----------------------------------------------------------------------------
%% @doc Creates RTP distribution channels for each stream in the session
%%      description.
%% @spec create_channels(Desc) -> Result
%%       Desc = sdp_stream_description()
%%       Result = {ok, ChannelMap} | error
%%       ChannelMap = dictionary()  
%% @end
%% ----------------------------------------------------------------------------
create_channels(_Desc = #session_description{streams = Streams, 
                                             rtp_map = RtpMap,
                                             format_map = Formats}) ->
  Result = lists:map(
    fun (Stream = #media_stream{format = FormatIndex}) ->
      RtpMapEntry = case lists:keyfind(FormatIndex, 1, RtpMap) of
        false -> throw({ems_session, missing_rtpmap_entry});
        Entry -> Entry
      end,
			
      case ems_channel:start_link(Stream, RtpMapEntry) of
        {ok, ChannelPid} -> ChannelPid
      end,
			
			{Stream#media_stream.control_uri, ChannelPid}
		end,
	  Streams),
	{ok, dict:from_list(Result)}.
	
%% ----------------------------------------------------------------------------
%% @doc Sets up an RTP data stream
%% @spec setup_stream(Direction, StreamName, Transport, State) -> Result
%%       Direction = inbound | outbound
%%       StreamName = string()
%%       ClientTransport = list()
%%       State = term()
%%       Result = {ServerTransport, NewState}
%% @end
%% ----------------------------------------------------------------------------  

% Handles the inbound stream case - setting up the stream manager and getting
% it ready to receive RTP data from the broadcaster
setup_stream(StreamName, ClientTransport, State) ->
  
  Direction = case lists:keyfind(direction, 1, ClientTransport) of
    {direction, Dir} -> Dir;
    false -> outbound
  end, 
      
  case dict:find(StreamName, State#state.channels) of
    {ok, ChannelPid} -> 
      {ok, ServerTransport} = ems_channel:configure_input(ChannelPid, ClientTransport),
      {ServerTransport, State};
      
    error -> 
      throw({ems_server, not_found})
  end.
  
%% ---------------------------------------------------------------------------- 
%%
%% ----------------------------------------------------------------------------    
fetch_stream(StreamName, State) ->
  false.
  
%% ---------------------------------------------------------------------------- 
%% @spec transport_type(TrasnportSpec) -> Result
%%       Result = interleaved | unicast | multicast
%% @end
%% ----------------------------------------------------------------------------    
transport_type(TransportSpec) ->
  case lists:keymember(interleaved, 1, TransportSpec) of
    true -> interleaved;
    false -> case lists:member(unicast) of 
      true -> unicast;
      false -> case lists:member(multicast) of
        true -> multicast
      end
    end
  end.    
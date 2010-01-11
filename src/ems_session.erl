-module(ems_session).
-behaviour(gen_server).
-include("erlang_media_server.hrl").
-include ("sdp.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-record(state, {id, path, description, channels}).

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
  setup_stream/3]).

%% ============================================================================
%% Exports
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts a new session and returns the new session's process identifier.
%% @spec start_link(Id, Path, Desc) -> Pid
%%       Id = integer()
%%       Path = string()
%%       Desc = SDP session description
%% @end
%% ----------------------------------------------------------------------------
start_link(Id, Path, Desc) ->
  ?LOG_DEBUG("ems_session:start_link/3 - Id: ~w, Path: ~s", [Id, Path]),
  gen_server:start_link(?MODULE, {Id, Path, Desc}, []).
  
%% ----------------------------------------------------------------------------
%% @doc Sets up a new stream, returning a new transport definition for the
%%      caller to return to the client.
%% @spec stream_setup(SessionPid, StreamName, Transport) -> Result
%%         Result = {ok, ServerTransport}
%%         Transport = ServerTransport = TransportSpec
%%         TrasportSpec = [TransportOption]
%% @end
%% ----------------------------------------------------------------------------
setup_stream(SessionPid, StreamName, Transport) ->
  ?LOG_DEBUG("ems_session:setup_stream/3 ~s", [StreamName]),
  try
    gen_server:call(SessionPid, {setup_stream, StreamName, Transport})
  catch
    exit:{timeout,_} -> {error, timeout};
    _Type:Err -> {error, Err}
  end.
  
%% ============================================================================
%% Server Callbacks
%% ============================================================================

init({Id, Path, Desc}) ->
  ?LOG_DEBUG("ems_session:init/2 - ~s", [Path]),
	{ok, Channels} = create_channels(Desc),
  State = #state{id=Id, path=Path, description=Desc, channels=Channels},
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

% handles a stream setup request from a client
handle_call({setup_stream, StreamName, Transport}, From, State) ->
  ?LOG_DEBUG("ems_session:handle_call/3 - Setting up stream ~s with transport ~w",
    [StreamName, Transport]),
    
  try
    case lists:keysearch(direction, 1, Transport) of
      {value, {direction, Direction}} -> 
        {ServerTransportSpec, NewState} = setup_stream(Direction, StreamName, Transport, State),
        {reply, {ok, ServerTransportSpec}, NewState};

      _ -> 
        throw({ems_session,unsupported_transport})
    end
  catch
    {ems_session, Reason} -> {reply, {error, Reason}, State}
  end;

%% The default implementation. Swallows the message.
handle_call(Request, From, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
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

%% ----------------------------------------------------------------------------
%% @doc Creates RTP distribution channels for each stream in the session
%%      description.
%% @spec create_channels(Desc) -> Result
%%       Desc = sdp_stream_description()
%%       Result = {ok, ChannelMap} | error
%%       ChannelMap = dictionary()  
%% @end
%% ----------------------------------------------------------------------------
create_channels(_Desc = #session_description{streams=Streams, 
                                             rtp_map = RtpMap,
                                             format_map = Formats}) ->
  Result = lists:map(
		fun (Stream = #media_stream{format = FormatIndex}) ->
		  RtpMapEntry = case lists:keyfind(FormatIndex, 1, RtpMap) of
        false -> throw({ems_session, missing_rtpmap_entry});
        RtpMap1 -> RtpMap1
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
%%       Transport = list()
%%       State = term()
%%       Result = {ServerTransport, NewState}
%% @end
%% ----------------------------------------------------------------------------  

% Handles the inbound stream case - setting up the stream manager and getting
% it ready to receive RTP data from the broadcaster
setup_stream(inbound, StreamName, TransportSpec, State) ->
  case dict:find(StreamName, State#state.channels) of
    {ok, ChannelPid} -> 
      {ok, ServerTransportSpec} = ems_channel:configure_input(ChannelPid, TransportSpec),
      {ServerTransportSpec, State};
      
    error -> 
      throw({ems_server, not_found})
  end;
  
setup_stream(outbound, StreamName, Transport, State) ->
  {[], State}.

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
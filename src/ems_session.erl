-module(ems_session).
-include("erlang_media_server.hrl").
-include ("sdp.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-record(state, {id, path, owner, description, channels, clients = dict:new()}).
-record(client, {id, subscriptions = []}).
-record(subscription, {pid, path}).
	
-export([
  start_link/3,
  receive_rtsp_request/5]).
  
-export([start_session/1]).

%% ============================================================================
%% Exports
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts a new session and returns the new session's process identifier.
%% @end
%% ----------------------------------------------------------------------------
-spec start_link(Id::integer(), Path::string(), OwnerPid::pid()) -> pid().
start_link(Id, Path, OwnerPid) ->
  ?LOG_DEBUG("ems_session:start_link/2 - Id: ~w, Path: ~s", [Id, Path]),
  State = #state{id = Id, path = Path, owner = OwnerPid},
  {ok, erlang:spawn_link(?MODULE, start_session, [State])}
  .
  
%% ----------------------------------------------------------------------------
%% @spec The public interface for passing a request on to the session
%% ----------------------------------------------------------------------------  
receive_rtsp_request(SessionPid, Request, Headers, Body, ConnectionPid) ->
  SessionPid ! {self(), {rtsp_request, Request, Headers, Body, ConnectionPid}}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
stop(SessionPid) ->
  SessionPid ! {self(), stop_ems_session}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
start_session(State) ->
  ?LOG_DEBUG("ems_session:start_session/1 - starting session...", []),
  process_flag(trap_exit, true),
  session_loop(State).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
session_loop(State) ->
  receive
    {From, {rtsp_request, Request, Headers, Body, ConnectionPid}} ->
      NewState = handle_rtsp_request(Request, Headers, Body, ConnectionPid, State),
      session_loop(NewState);
      
    {From, stop_ems_session} ->
      ok;
      
    Message ->
      ?LOG_DEBUG("ems_session:session_loop/1 - unexpected message ~p", [Message]),
      session_loop(State)
  end.
  
%% ============================================================================
%% Server Callbacks
%% ============================================================================

init({Id, Path, OwnerPid}) ->
  ?LOG_DEBUG("ems_session:init/3 - ~s", [Path]),
  process_flag(trap_exit, true),
  State = #state{id=Id, path=Path, owner=OwnerPid},
  {ok, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_rtsp_request(Request, Headers, Body, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_cast/2 - Handling RTSP request", []),

  Method = Request#rtsp_request.method,
  Sequence = Headers#rtsp_message_header.sequence,
  try
    NewState = handle_request(Method, Sequence, Request, Headers, Body, 
      Connection, State),
    NewState
  catch
    ems_session:Error -> 
      rtsp_connection:send_server_error(Connection, Error, Sequence),
      State
  end.
  
%% ============================================================================
%% Internal functions
%% ============================================================================

%% ----------------------------------------------------------------------------    
%% @doc Handles an RTSP request 
%% @throws {ems_session | Reason} where Reason = bad_requset.
%% @spec handle_request(Method, Sequence, Request, Headers, Body, 
%%         Connection, State) -> NewState
%% @end
%% ----------------------------------------------------------------------------    
handle_request(announce, Sequence, Request, Headers, Body, Connection, State) ->
  {_,_,_,_, ContentType} = rtsp:get_request_info(Request,Headers),
  
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
  
handle_request(describe, Sequence, Request, Headers, _Body, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_request/7 - DESCRIBE", []),
  ResponseBody = sdp:format( State#state.description ),
  rtsp_connection:send_response(Connection, Sequence, ok, [], ResponseBody),  
  State;

handle_request(setup, Sequence, Request, Headers, _Body, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_request/7 - SETUP", []),
  
  Uri = Request#rtsp_request.uri,
  {_,_,_,Path} = url:parse(Uri),
  SessionPath = State#state.path,
  StreamName = string:substr(Path, length(SessionPath)+2),
 
  % look for the client's transport header 
  case rtsp:get_header(Headers, ?RTSP_HEADER_TRANSPORT) of
    ClientHeader when is_list(ClientHeader) ->

      % Parse transport header and use the parsed data to try and set up the
      % stream
      ClientTransport = rtsp:parse_transport(ClientHeader),
      ClientAddress = rtsp_connection:get_client_address(Connection),
      
      {SessionId, ServerTransport, NewState} = 
        setup_stream(ClientAddress, Headers, StreamName, ClientTransport, State),
      ServerHeader = rtsp:format_transport(ServerTransport),
      ServerHeaders = [
        {?RTSP_HEADER_TRANSPORT, ServerHeader},
        {?RTSP_HEADER_SESSION, SessionId}],
      rtsp_connection:send_response(Connection, Sequence, ok, ServerHeaders, <<>>),
      NewState;
       
    undefined ->
      % No transport header in the setup request (or the transport header is 
      % something other than a string). That's very bad, and the client should
      % be punished. 
      throw({ems_session, bad_request})
  end;

handle_request(record, Sequence, Request, Headers, <<>>, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_request/7 - RECORD", []),
  
  Uri = Request#rtsp_request.uri,
  {_,_,_,Path} = url:parse(Uri),
  SessionPath = State#state.path,

  Client = get_client(Headers, State),
  case SessionPath of
    Path -> 
      F = fun( S ) ->
        Handler = S#subscription.pid,
        P =  S#subscription.path,
      
        ?LOG_DEBUG("ems_session:handle_request/7 - Enabling  subscription on ~s", 
          [P]),
        Handler ! enable,
    
        RtpInfoValue = io_lib:format("uri=~s/~s", [Uri, P]),
        lists:flatten(RtpInfoValue)
      end,
        
      UrlList = lists:map(F, Client#client.subscriptions),
        
      SessionId = stringutils:int_to_string(Client#client.id),
      ResponseHeaders = [
        {?RTSP_HEADER_RTP_INFO, string:join(UrlList, ",")},
        {?RTSP_HEADER_RANGE, "npt=now-"}, 
        {?RTSP_HEADER_SESSION, SessionId}
      ],
      rtsp_connection:send_response(Connection, Sequence, ok, ResponseHeaders, <<>>);
    
    _ -> 
      _StreamName = string:substr(Path, length(SessionPath)+2)
  end,
  State;
  
handle_request(teardown, Sequence, Request, Headers, Body, Connection, State) ->
  ?LOG_DEBUG("ems_session:handle_request/7 - TEARDOWN", []),
  
  Uri = Request#rtsp_request.uri,
  {_,_,_,Path} = url:parse(Uri),
  SessionPath = State#state.path,
  
  Client = get_client(Headers, State),
  NewState = case SessionPath of
    Path -> 
      F = fun(S) -> 
        Handler = S#subscription.pid,
        Handler ! stop
      end,
      lists:map(F, Client#client.subscriptions),
      Clients = dict:erase(Client#client.id, State#state.clients),
      State#state{clients = Clients};
        
    _ -> ok
  end,
  
  if
    Client#client.id == State#state.id ->
      ?LOG_DEBUG("ems_session:handle_request/7 - TEARDOWN - broadcasting session", []),
      stop(self());
      
    true -> ok
  end,
  
  rtsp_connection:send_response(Connection, Sequence, ok, [], <<>>),
  NewState;

handle_request(_Method, Sequence, _Request, _Headers, <<>>, Connection, State) ->
  rtsp_connection:send_response(Connection, Sequence, not_implemented, [], <<>>),
  State.
  
%% ----------------------------------------------------------------------------
%% @doc Creates RTP distribution channels for each stream in the session
%%      description.
%% @spec create_channels(Desc) -> Result where
%%       Desc = sdp_stream_description(),
%%       Result = {ok, ChannelMap} | error,
%%       ChannelMap = dictionary()
%% @end
%% ----------------------------------------------------------------------------
create_channels(_Desc = #session_description{streams = Streams, 
                                             rtp_map = RtpMap,
                                             format_map = _Formats}) ->
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
%%
%% ----------------------------------------------------------------------------	
destroy_channels(State) ->
  F = fun(Stream, Channel) -> ems_channel:stop(Channel) end,
  dict:map(F, State#state.channels),
  ok.
  
%% ----------------------------------------------------------------------------
%% @doc Sets up an RTP data stream
%% @spec setup_stream(ClientSessionId, Direction, StreamName, Transport, State) -> Result
%%       Direction = inbound | outbound
%%       StreamName = string()
%%       ClientTransport = list()
%%       State = term()
%%       Result = {ServerTransport, NewState}
%% @end
%% ----------------------------------------------------------------------------  

% Handles the inbound stream case - setting up the stream manager and getting
% it ready to receive RTP data from the broadcaster
setup_stream(ClientAddress, Headers, StreamName, ClientTransport, State) ->
  
  ?LOG_DEBUG("ems_session:setup_stream/5", []),
  
  case dict:find(StreamName, State#state.channels) of
    {ok, ChannelPid} -> 
      Direction = case lists:keyfind(direction, 1, ClientTransport) of
        {direction, Dir} -> Dir;
        false -> outbound
      end,
                  
      case Direction of 
        inbound ->
          ?LOG_DEBUG("ems_session:setup_stream/5 - setting up inbound stream", []),
          {Client, NewState} = get_or_create_client(Headers, State#state.id, State),
          
          {ok, ServerTransport} = ems_channel:configure_input(ChannelPid, ClientTransport, ClientAddress),
          SessionHeader = stringutils:int_to_string(Client#client.id),
          {SessionHeader, ServerTransport, save_subscription(Client, StreamName, ChannelPid, NewState)};
          
        outbound -> throw({ems_session, not_implemented})
      end;
            
    error -> 
      throw({ems_server, not_found})
  end.

%%----------------------------------------------------------------------------
%% @spec get_or_create_client(Headers, State) -> {Client, NewState}
%% @end
%%----------------------------------------------------------------------------
get_or_create_client(Headers, State) ->
  case rtsp:get_header(Headers, ?RTSP_HEADER_SESSION) of
    undefined -> create_client(State, random:uniform(99999999));
    SessionId -> {get_client(SessionId, State), State}
  end.

%%----------------------------------------------------------------------------
%% @spec
%% @end
%%----------------------------------------------------------------------------
get_or_create_client(Headers, Id, State) ->
  case rtsp:get_header(Headers, ?RTSP_HEADER_SESSION) of
    undefined -> create_client(State, Id);
    SessionId -> {get_client(SessionId, State), State}
  end.

%%----------------------------------------------------------------------------
%% @spec create_client(State) -> {Client, NewState}
%% @end
%% ----------------------------------------------------------------------------
create_client(State, Id) ->
  OldClients = State#state.clients,
  Client = #client{id = Id},
  NewState = State#state{clients = dict:store(Id, Client, OldClients)}, 
  {Client, NewState}.

%%----------------------------------------------------------------------------
%% @spec get_client_session(State) -> Client
%% @end
%% ----------------------------------------------------------------------------
get_client(Headers, State) when is_record(Headers, rtsp_message_header) ->
  case rtsp:get_header(Headers, ?RTSP_HEADER_SESSION) of
    undefined -> throw({ems_session, bad_request});
    Header -> get_client(Header, State)
  end;
  
get_client(SessionId, State) when is_list(SessionId) ->
  get_client(list_to_integer(SessionId), State);
  
get_client(SessionId, State) when is_integer(SessionId) ->
  case dict:find(SessionId, State#state.clients) of
    {ok, Client} -> Client;
    error -> throw({ems_session, session_not_found})
  end.

%%----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------      
save_subscription(Client, StreamPath, SubscribedPid, State) ->
  ClientId = Client#client.id,
  Subs = Client#client.subscriptions,
  NewSubs = [#subscription{pid=SubscribedPid, path=StreamPath} | Subs],
  NewClients = dict:update(ClientId, 
    fun( C ) -> C#client{subscriptions = NewSubs} end,
    State#state.clients),
  State#state{clients=NewClients}.
  
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
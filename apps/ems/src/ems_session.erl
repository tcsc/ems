-module(ems_session).
-behavior(gen_server).
-include ("sdp.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-type nullary_fun() :: fun(()->any()). 

-record(state, {id          :: integer(), 
                path        :: string(),  
                description :: sdp:session_description(), 
                channels    :: [any()], 
                clients     :: dict(),
                exit_funs   :: [nullary_fun()]}).
-record(client, {id, subscriptions = []}).
-record(subscription, {pid, path}).
	
-export([start/2, start_link/2, collect_channels/2, for_each_channel/2, stop/1]).

-type session() :: pid().
-export_type([session/0]).
-opaque_type([session/0]).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3 ]).

%% ============================================================================
%% Exports
%% ============================================================================

-spec start(Path :: string(), Description :: sdp:session_description()) -> 
        {'ok', session()}.
start(Path, Description) -> 
  log:debug("session:new/2 - Creating session for ~s", [Path]),
  State = #state{path = Path, description = Description},
  {ok, gen_server:start(?MODULE, State, [])}.

%% ----------------------------------------------------------------------------
%% @doc Starts a new session and returns the new session's process identifier.
%% @end
%% ----------------------------------------------------------------------------
-spec start_link(Id::integer(), Path::string()) -> pid().
start_link(Id, Path) ->
  log:debug("ems_session:start_link/2 - Id: ~w, Path: ~s", [Id, Path]),
  State = #state{id = Id, path = Path},
  {ok, gen_server:start_link(?MODULE, State, [])}.

%% ----------------------------------------------------------------------------
%% @doc Collects the streams inside the session and returns them to the caller, 
%%      together with their addresses.
%% @end
%% ----------------------------------------------------------------------------

%% a collected channel and its address (i.e. path)
-type named_channel() :: {string(), any()}.
-spec collect_channels(Session :: session(), Path :: string()) -> [named_channel()].
collect_channels(Session, Root) ->
  Channels = gen_server:call(Session, collect_chddannels),
  F = fun(Ch) -> 
        Path = ems_channel:path(Ch),
        Addr = url:join_path(Root, Path),
        {Addr, Ch}
      end,
  plists:parallel_map(F, Channels).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
stop(SessionPid) ->
  SessionPid ! {self(), stop_ems_session}.

%% ----------------------------------------------------------------------------
%% @doc applies a function to each stream in the session
%% ----------------------------------------------------------------------------
-spec for_each_channel(session(), fun((term()) -> any())) -> any().
for_each_channel(Session, F) ->
  Channels = gen_server:call(Session, get_channels),
  lists:foreach(Channels, F).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
init(State) -> {ok, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_call({collect_channels, Root}, _From, State) -> 
  {reply, State#state.channels, State};
        
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) -> 
  lists:foreach(fun(F) -> F() end, State#state.exit_funs),
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

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
    fun (Stream = #media_stream{formats = FormatIndex}) ->
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
  
  log:debug("ems_session:setup_stream/5", []),
  
  case dict:find(StreamName, State#state.channels) of
    {ok, ChannelPid} -> 
      Direction = case lists:keyfind(direction, 1, ClientTransport) of
        {direction, Dir} -> Dir;
        false -> outbound
      end,
                  
      case Direction of 
        inbound ->
          log:debug("ems_session:setup_stream/5 - setting up inbound stream", []),
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
%%
%%----------------------------------------------------------------------------
get_or_create_client(Headers, Id, State) ->
  case rtsp:get_header(Headers, ?RTSP_HEADER_SESSION) of
    undefined -> create_client(State, Id);
    SessionId -> {get_client(SessionId, State), State}
  end.

%%----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------
create_client(State, Id) ->
  OldClients = State#state.clients,
  Client = #client{id = Id},
  NewState = State#state{clients = dict:store(Id, Client, OldClients)}, 
  {Client, NewState}.

%%----------------------------------------------------------------------------
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

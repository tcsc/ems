-module(ems_session).
-behavior(gen_server).
-include ("sdp.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-record(state, {id          :: integer(), 
                path        :: string(),  
                description :: sdp:session_description(), 
                channels    :: [any()], 
                clients     :: dict()}).

-type state() :: #state{}.

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
-spec collect_channels(Session :: session(), Path :: string()) -> 
  [named_channel()].
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
%% @end
%% ----------------------------------------------------------------------------
-spec for_each_channel(session(), fun((term()) -> any())) -> any().
for_each_channel(Session, F) ->
  Channels = gen_server:call(Session, get_channels),
  lists:foreach(Channels, F).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts a media session and all of the stream processes contained within 
%%      it.
%% @end
%% ----------------------------------------------------------------------------    
-spec init(Description :: sdp:session_description()) -> 
  {ok, State :: state()} | invalid_session.

init(State = #state{description = Desc}) ->
  
  % define a lambda that we will use to map over the streams in the session 
  % description to create channels for us
  CreateChannel = 
    fun(Stream) ->
      % extract the RTP specifications for this stream from the session 
      % description - the channel will need to know about these to do its 
      % job
      F = fun(Id) ->
            case sdp:rtp_map(Id, Desc) of
              false -> throw(invalid_session);
              X -> X
            end
          end,
      RtpFormats = lists:map(F, Stream#stream.formats),

      % Start the channel for this stream. Linking to the channel means that
      % if one of the subsequent channel creations fails then the already-
      % created channels will automatically be killed when the session goes 
      % down...
      case ems_channel:start_link(Stream, RtpFormats, self()) of
        {ok, Chan} -> Chan;
        {error, _} -> throw({channel_failed, Stream}) 
      end
    end,

  try
    % map over the streams in the sesison description and create channels 
    % for each of them
    Channels = lists:map(CreateChannel, sdp:streams(Desc)),
    StateP = State#state{channels = Channels},
    {ok, StateP}
  catch
    throw:invalid_session -> 
      log:debug("Invalid session description"),
      {error, invalid_session};
    throw:Err -> 
      log:error("Stream setup failed: ~w", [Err]),
      {error, Err}
  end.


-spec activate(ems:session()) -> ok | {error, Reason :: term()}.
activate(Session) -> gen_server:call(activate, Session).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_call(activate, _From, State = #state{channels = Channels}) ->
  plists:parallel_map(fun ems_channel:activate/1, Channels),
  {reply, ok, State};
  
handle_call({collect_channels, Root}, _From, State) -> 
  {reply, State#state.channels, State};
        
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) -> 
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------	
destroy_channels(State) ->
  F = fun(Stream, Channel) -> ems_channel:stop(Channel) end,
  dict:map(F, State#state.channels),
  ok.
  
%% ----------------------------------------------------------------------------      
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

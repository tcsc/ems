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

% Public API ------------------------------------------------------------------
-export([start_link/3, get_channels/1, for_each_channel/2, stop/1]).

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

-spec start_link(User :: ems:user_info(), 
                 Path :: string(), 
                 Description :: sdp:session_description()) -> 
                   {'ok', session()}.

start_link(_User, Path, Description) -> 
  log:debug("session:new/3 - Creating session for ~s", [Path]),
  State = #state{path = Path, description = Description},
  gen_server:start_link(?MODULE, State, []).

%% ----------------------------------------------------------------------------
%% @doc Collects the streams inside the session and returns them to the caller, 
%%      together with their addresses.
%% @end
%% ----------------------------------------------------------------------------

%% a collected channel and its address (i.e. path)
-type named_channel() :: {string(), ems:channel()}.
-spec get_channels(Session :: session()) -> [named_channel()].

get_channels(Session) ->
  Channels = gen_server:call(Session, get_channels),
  F = fun(Ch) -> 
        Path = ems_channel:get_path(Ch),
        {Path, Ch}
      end,
  lists:map(F, Channels).

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
  {_, Channels} = gen_server:call(Session, get_channels),
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

init(State = #state{path = Root, description = Desc}) ->
  
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

      Path = url:join(Root, Stream#stream.control_uri),

      % Start the channel for this stream. Linking to the channel means that
      % if one of the subsequent channel creations fails then the already-
      % created channels will automatically be killed when the session goes 
      % down...
      case ems_channel:start_link(Path, Stream, RtpFormats, self()) of
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
  
handle_call(get_channels, _From, State = #state{channels = Channels}) ->
  {reply, Channels, State};

handle_call(_Request, _From, State) -> 
  {noreply, State}.

handle_cast(_Request, State) -> 
  {noreply, State}.

handle_info(_Msg, State) -> 
  {noreply, State}.

terminate(_Reason, State) -> 
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> 
  {ok, State}.

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

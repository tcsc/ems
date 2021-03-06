-module(ems_server).
-behaviour(gen_server).

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
%% Exported Functions
%% ============================================================================
-export([start_link/1, 
         stop/1, 
         create_session/5, 
         with_channel_do/2, 
         with_broadcast_rights_on_channel_do/4]).

%% ============================================================================
%% Records, macros, etc
%% ============================================================================
-record(state, {name :: atom(),
                config :: ems_config:handle()}).
-type state() :: #state{}.

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the network server on a given IP address and  port pair.
%% @end
%% ----------------------------------------------------------------------------
-spec start_link(ems_config:handle()) -> 
        {ok, pid()} | {error, Reason :: term()}.
start_link(ConfigHandle) ->
  log:info("ems_server:start_link/1", []),
  
  Config = ems_config:get_config(ConfigHandle),
  State = #state{name=ems_server, config = Config},
  
  case gen_server:start_link({local,ems_server}, ?MODULE, State, []) of
    {ok,Pid} -> 
      log:debug("ems_server:start_link/0 - server started on ~w", [Pid]),
      RtspConfig = case lists:keyfind(rtsp, 1, Config) of
                     {rtsp, Rtsp} -> Rtsp;
                     false -> []
                   end,
      configure_rtsp_server(ConfigHandle, RtspConfig),
      {ok,Pid};

    Error ->
      log:debug("ems_server:start_link/0 - server failed to start ~w", [Error]),
      Error
  end.    

%% ----------------------------------------------------------------------------
%% @doc Stops the network server.
%% @end
%% ----------------------------------------------------------------------------
stop(_State) ->
  gen_server:cast(ems_server, stop).

%% ----------------------------------------------------------------------------
%% @doc Creates a new session and registers it at the supplied path. This 
%%      function is responsible for authorising the creation request and 
%%      passing it on to the session manager for the actual object creation.
%% @end 
%% ----------------------------------------------------------------------------
-spec create_session(Config  :: config:handle(),
                     Path    :: string(),
                     User    :: ems:user_info(),
                     Desc    :: sdp:session_description(),
                     Options :: [any()] ) -> {ok, ems:session()} | 
                                             'not_found' | 
                                             'not_authorised' | 
                                             'already_exists'.

% creating a session always requires a logged-in user, so if we don't have 
% one then we need to bail right now.
create_session(_, _, anonymous, _, _) -> 
  not_authorised;

create_session(Config, Path, User, Desc, _Options) -> 
  case ems_config:get_mount_point(Config, Path) of 
    {ok, MountPoint} ->
      Rights = ems_config:get_user_rights(Config, User, MountPoint),
      case lists:member(broadcast, Rights) of
        true ->
          log:debug("ems_server: user has broadcast rights for \"~s\"", [Path]),
          case ems_session_manager:create_session(User, Path, Desc) of
            {ok, Session} -> {ok, Session};
            {error, Err} -> Err
          end;

        false ->
          log:debug("ems_server: user has no broadcast rights for \"~s\"", [Path]),
          not_authorised
      end;
      
    _ -> 
      log:debug("ems_server: no such mount point", []),
      not_found
  end.

%% ----------------------------------------------------------------------------
%% @doc Configures an inbound channel. The user must have broadcast rights on 
%%      the channel, and anonymous users are not allowed.
%% @doc 
%% ----------------------------------------------------------------------------
-spec configure_channel( Config :: ems_config:handle(),
                         User :: ems:user_info(),
                         Path :: string(),
                         SessionId :: string(),
                         TransportSpec :: ems:transport_spec() ) -> 
                            {error, unauthorized | atom() } | 
                            {ok, ems:transport_spec()}.

configure_channel(Config, anonymous, _, _, _) -> unauthorized;
configure_channel(Config, User, Path, SessionId, TransportSpec) ->
  F = fun(Channel) ->
       ok  
      end,
  with_broadcast_rights_on_channel_do(Config, Path, User, F).

%% ----------------------------------------------------------------------------
%% @doc Looks up a channel, checks to see if the user has broadcast rights on 
%%      the enclosing session and executes a channel action callback if the 
%%      channel exists and the user has the appropriate rights.
%% @end
%% ----------------------------------------------------------------------------
-type channel_action() :: fun((Channel :: ems:channel()) -> ok | no_return()).
-spec with_broadcast_rights_on_channel_do(
        Config :: config:handle(),
        Path :: string(),
        User :: ems:user_info(),
        Action :: channel_action()) -> ok | not_found | unauthorised.

with_broadcast_rights_on_channel_do(Config, Path, User, Action) ->
  case ems_session_manager:lookup_object(Path) of
    {channel, Channel} ->
      Session = ems_channel:get_session(Channel),
      SessionPath = ems_session:get_path(Session),
      Rights = ems_config:get(Config, User, SessionPath),
      case lists:member(broadcast, Rights) of
        true -> Action(Channel);
        false -> unauthorised 
      end;

    _ -> not_found
  end.

-spec with_channel_do(
        Path :: string(), 
        Action :: channel_action()) -> ok | not_found.

with_channel_do(Path, Action) ->
  case ems_session_manager:lookup_object(Path) of
    {channel, Ch} -> Action(Ch);
    _ -> not_found
  end.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Called back by the gen_server framework to do the real initialisation 
%%      work.
%% @end
%% ----------------------------------------------------------------------------
init(State) ->
  log:info("ems_server:init/1", []),
  {ok, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: state()) ->
        {reply, Reply :: term(), NewState :: state() } | 
        {noreply, NewState :: state()}.

handle_call(get_config, _From, State = #state{config = Config}) ->
  {reply, Config, State};

handle_call(_Request, _From, State) ->
  log:debug("ems_server:handle_call/3",[]),
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: state) -> 
        {noreply, Reply :: term()}.
handle_cast(_Request, State) ->
  log:debug("ems_server:handle_cast/2 ~w",[_Request]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ---------------------------------------------------------------------------- 
handle_info(_Info, State) ->
  log:debug("ems_server:handle_info/2",[]),
  {noreply, State}.
  
terminate(Reason, _State) ->
  log:debug("ems_server:terminate/2 - ~w",[Reason]),
  ok.
  
code_change(_OldVersion, State, _Extra) ->
  log:debug("ems_server:code_change/3",[]),
  {ok, State}.
  

%% ============================================================================
%% Internal utility functions
%% ============================================================================
  
configure_rtsp_server(ConfigHandle, Config) -> 
  Ports = case lists:keyfind(ports, 1, Config) of
            {ports, Ps} -> Ps;
            false -> [554]
          end,
  Handler = fun(Conn, Msg) -> 
              ems_rtsp_bridge:handle_request(ConfigHandle, Conn, Msg)
            end,
  Bind = fun(P) -> rtsp:add_listener({0,0,0,0}, P, Handler) end,
  lists:foreach(Bind, Ports).

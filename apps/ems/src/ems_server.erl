-module(ems_server).
-behaviour(gen_server).
-include("logging.hrl").

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
-export([start_link/1, stop/1, create_session/5]).

%% ============================================================================
%% Records, macros, etc
%% ============================================================================
-record(server_state, {name}).

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the network server on a given IP address & port pair.
%% @spec start_link(IpAddress,Port) -> {ok, Pid} | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
start_link(ConfigHandle) ->
  ?LOG_INFO("ems_server:start_link/1", []),
  
  State = #server_state{name=ems_server},
  Config = ems_config:get_config(ConfigHandle),
  
  case gen_server:start_link({local,ems_server}, ?MODULE, State, []) of
    {ok,Pid} -> 
      ?LOG_DEBUG("ems_server:start_link/0 - server started on ~w", [Pid]),
      RtspConfig = case lists:keyfind(rtsp, 1, Config) of
                     {rtsp, Rtsp} -> Rtsp;
                     false -> []
                   end,
      configure_rtsp_server(ConfigHandle, RtspConfig),
      {ok,Pid};

    Error ->
      ?LOG_DEBUG("ems_server:start_link/0 - server failed to start ~w", [Error]),
      Error
  end.    

%% ----------------------------------------------------------------------------
%% @doc Stops the network server.
%% @end
%% ----------------------------------------------------------------------------
stop(_State) ->
  gen_server:cast(ems_server, stop).

%% ----------------------------------------------------------------------------
%% @doc Creates a new session and registers it with the path
%% @end 
%% ----------------------------------------------------------------------------
-spec create_session(Config   :: config:handle(),
                     Path     :: string(),
                     UserInfo :: any(),
                     Desc     :: sdp:session_description(),
                     Options  :: [any()] ) -> {ok, ems:session()} | 'not_found' | 'not_authorised'.
create_session(Config, Path, UserInfo, Desc, _Options) -> 
  case ems_config:get_mount_point(Config, Path) of 
    {ok, MountPoint} ->
      Rights =  ems_config:get_user_rights(Config, UserInfo, MountPoint),
      case lists:member(broadcast, Rights) of
        true -> 
          ?LOG_DEBUG("ems_server:create_session/5 - user has broadcast rights for \"~s\"", [Path]),
          {ok, Session} = ems_session:new(Path, Desc),
          {ok, Session};
          
        false -> 
          not_authorised
      end;
      
    _ -> 
      ?LOG_DEBUG("ems_server:create_session/5 - no such mount point", []),
      not_found
  end.
  
  
%   Session = ems_session:new(Path, Desc),
%    case ems_session_manager:register_session(Path, Session, UserInfo) of
%      ok ->
%        {ok, Session};
%        
%      {error, Reason} -> 
%        ems_session:destroy(Session),
%        {error, Reason}
%    end;
%  false -> not_authorised


  
%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Called back by the gen_server framework to do the real initialisation 
%%      work.
%% @spec init(State) -> {ok,State} | 
%%                      {ok,State,Timeout} | 
%%                      {ok,State,hibernate} |
%%                      {stop, Reason} |
%%                      ignore
%% @end
%% ----------------------------------------------------------------------------
init(State) ->
  ?LOG_INFO("ems_server:init/1", []),
  {ok, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  ?LOG_DEBUG("ems_server:handle_call/3",[]),
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @spec handle_cast(Request,From,State) -> {noreply,State}
%% @end
%% ----------------------------------------------------------------------------
handle_cast(_Request, State) ->
  ?LOG_DEBUG("ems_server:handle_cast/2 ~w",[_Request]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ---------------------------------------------------------------------------- 
handle_info(_Info, State) ->
  ?LOG_DEBUG("ems_server:handle_info/2",[]),
  {noreply, State}.
  
terminate(Reason, _State) ->
  ?LOG_DEBUG("ems_server:terminate/2 - ~w",[Reason]),
  ok.
  
code_change(_OldVersion, State, _Extra) ->
  ?LOG_DEBUG("ems_server:code_change/3",[]),
  {ok, State}.
  

%% ============================================================================
%% Internal utility functions
%% ============================================================================
  
configure_rtsp_server(ConfigHandle, Config) -> 
  Ports = case lists:keyfind(ports, 1, Config) of
            {ports, Ps} -> Ps;
            false -> [554]
          end,

  Handler = 
    fun(Conn,Msg) -> 
      ems_rtsp_bridge:handle_request(ConfigHandle, Conn, Msg) 
    end,

  Bind = fun(P) -> rtsp:add_listener({0,0,0,0}, P, Handler) end,
  lists:foreach(Bind, Ports).


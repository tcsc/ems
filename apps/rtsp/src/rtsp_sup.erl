-module(rtsp_sup).
-behaviour(supervisor).

%% Public API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ============================================================================
%% API functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Called by the appliction entry point to start the RTSP supervisor 
%%      process
%% @private
%% @end
%% ----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> 
  case supervisor:start_link({local, rtsp_sup}, ?MODULE, []) of
    {ok, Pid} -> 
      log:debug("rtsp:start_link/0 - RTSP supervisor started on ~w", [Pid]),
      {ok, Pid};
      
    {error, Err} -> 
      log:err("rtsp:start_link/0 - RTSP supervisor failed to start ~w", [Err]),
      {error, Err}
  end.

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Returns a child spec list for the supervisor process to use.
%% @private
%% @end
%% ----------------------------------------------------------------------------
init(_) ->
  log:info("rtsp_sup:init/1 - Starting RTSP supervisor"),

  RtspServer = {
    rtsp_server,
    {rtsp_server, start_link, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_server]
  },
  DigestServer = {
    digest_server,
    {rtsp_digest_server, start_link, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_digest_server]
  },
  TableManager = {
    rtsp_session_table_mgr,
    {rtsp_session_manager, start_table_manager, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_session_manager]
  },  
  SessionManager = {
    rtsp_session_manager,
    {rtsp_session_manager, start_link, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_sesstion_manager]
  },
  {ok, {{one_for_one, 10, 1}, [DigestServer, TableManager, SessionManager, RtspServer]}}.


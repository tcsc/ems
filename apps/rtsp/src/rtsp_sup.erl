-module(rtsp_sup).
-behaviour(supervisor).
-include("logging.hrl").

%% Public API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ============================================================================
%% API functions
%% ============================================================================

%% @doc Called by the appliction entry point to start the RTSP supervisor 
%%      process
%% @private
%% @end
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> 
  case supervisor:start_link({local, rtsp_sup}, ?MODULE, []) of
    {ok, Pid} -> 
      ?LOG_DEBUG("rtsp:start_link/0 - RTSP supervisor started on ~w", [Pid]),
      {ok, Pid};
      
    {error, Err} -> 
      ?LOG_ERROR("rtsp:start_link/0 - RTSP supervisor failed to start ~w", [Err]),
      {error, Err}
  end.

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

%% @doc Returns a child spec list for the supervisor process to use.
%% @private
%% @end
init(_) -> 
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
  {ok, {{one_for_one, 10, 1}, [DigestServer, RtspServer]}}.


-module(ems_supervisor).
-behaviour(supervisor).
-include("logging.hrl").

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/1]).

%% ============================================================================
%% Internal Exports
%% ============================================================================
-export([init/1]).

%% ============================================================================
%% Macros
%% ============================================================================
-define(SERVER, ?MODULE).

%% ============================================================================
%% Exported Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @spec start_link() -> {ok, Pid} | Error
%% @end
%% ----------------------------------------------------------------------------
start_link(Config) ->
	?LOG_INFO("ems_supervisor:start_link/1 - Starting EMS Supervisor", []),
	case supervisor:start_link({local, ?SERVER}, ?MODULE, Config) of
		{ok, Pid} ->
			?LOG_DEBUG("ems_supervisor:start_link/1 - Supervisor started on ~w", [Pid]),
			{ok, Pid};
			
		{error, Reason} ->
		  ?LOG_DEBUG("ems_supervisor:start_link/1 - Supervisor failed to start: ~p", [Reason]),
		  {error, Reason}
	end.

%% ============================================================================
%% Server Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec init([]) -> {ok, {SupervisorFlags, ChildSpec}}
%% ----------------------------------------------------------------------------
init(ConfigHandle) ->
	?LOG_INFO("ems_supervisor:init/1 - building child spec list", []),
	
	RestartStrategy        = one_for_one,
	MaxRestarts            = 1000,
	MaxTimeBetweenRestarts = 3600,
	
	SupervisorFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},
	Config = ems_config:get_config(ConfigHandle),
	
	ChildSpec = 
	[	
		{digest_auth_server,
			{rtsp_auth, start_link, []},
			permanent,
			2000,
			worker,
			[digest_auth_server]},
		{ems_server,
			{ems_server, start_link, [Config]},
			permanent,
			2000,
			worker,
			[ems_server]
		},
		{session_manager,
		  {ems_session_manager, start_link, []},
		  permanent,
		  2000,
		  worker,
		  [ems_session_manager]
		}
	],
	
	{ok, {SupervisorFlags, ChildSpec}}.
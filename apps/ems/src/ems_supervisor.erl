-module(ems_supervisor).
-behaviour(supervisor).

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
%% ----------------------------------------------------------------------------
-spec start_link(ems_config:handle()) -> 
        {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
	log:info("ems_supervisor:start_link/1 - Starting EMS Supervisor", []),
	case supervisor:start_link({local, ?SERVER}, ?MODULE, Config) of
		{ok, Pid} ->
			log:debug("ems_supervisor:start_link/1 - Supervisor started on ~w", [Pid]),
			{ok, Pid};
			
		{error, Reason} ->
		  log:debug("ems_supervisor:start_link/1 - Supervisor failed to start: ~p", [Reason]),
		  {error, Reason}
	end.

%% ============================================================================
%% Server Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec init(ems_config:handle()) -> {ok, term()}.
init(ConfigHandle) ->
	log:info("ems_supervisor:init/1 - building child spec list", []),
	
	RestartStrategy        = one_for_one,
	MaxRestarts            = 1000,
	MaxTimeBetweenRestarts = 3600,
	
	SupervisorFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	ChildSpec = 
	[	
		{ems_server,
			{ems_server, start_link, [ConfigHandle]},
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

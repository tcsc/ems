-module(ems_supervisor).
-behaviour(supervisor).
-include("erlang_media_server.hrl").

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/0]).

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
start_link() ->
	?LOG_INFO("ems_supervisor:start_link/1 Starting Supervisor", []),
	case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
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
init(_Args) ->
	?LOG_INFO("ems_supervisor:init/1 - building child spec list", []),
	
	RestartStrategy        = one_for_one,
	MaxRestarts            = 1000,
	MaxTimeBetweenRestarts = 3600,
	
	SupervisorFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},
	
	ChildSpec = 
	[
		{ems_server,
			{ems_server, start_link, []},
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
		},
		{listener_sup,
			{ems_listener, start_link, []},
			permanent,
			2000,
			supervisor,
			[ems_listener]
		},
		{rtsp_sup,
			{rtsp_supervisor, start_link, []},
			permanent,
			2000,
			supervisor,
			[rtsp_supervisor]
		}
	],
	
	{ok, {SupervisorFlags, ChildSpec}}.
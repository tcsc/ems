-module(ems).
-export([start/0, start/2, stop/0, stop/1, shutdown/0]).
-include("logging.hrl").
-behaviour(application).

%% ============================================================================
%% External Functions
%% ============================================================================

start() ->
	?LOG_INFO("Starting EMS application", []),	
	application:start(listener),
	application:start(rtsp),
	application:start(ems).
	
%% ----------------------------------------------------------------------------
%% @doc The main entry point for the media server application
%% @spec start(Type,Args) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
start(normal, _) ->
	?LOG_DEBUG("ems:start/2", []),	

	?LOG_DEBUG("ems:start/2 - Loading configuration", []),	
	{ok, Config} = ems_config:load("local_config.erl"),

	?LOG_DEBUG("ems:start/2 - Starting overall supervisor", []),		
	case ems_supervisor:start_link(Config) of
		{ok, Pid} ->
			?LOG_DEBUG("ems_supervisor started", []), 
			{ok, Pid};

		Error ->
			?LOG_ERROR("ems:start/2 - failed to start ~w", [Error]), 
			Error
	end;

start(_,_) -> 
	{error, badarg}.

%% ----------------------------------------------------------------------------
%% @doc Called to shut down the media server application
%% @spec shutdown() -> ok
%% ----------------------------------------------------------------------------
shutdown() -> 
	?LOG_DEBUG("Shuting down ems", []),
	application:stop(erlang_media_server).

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% Called when the application terminates
%% ----------------------------------------------------------------------------
stop(_State) -> ok.	
stop() ->
	application:stop(ems).
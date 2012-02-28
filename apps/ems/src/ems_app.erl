-module(ems_app).
-export([start/2,stop/1]).
-behaviour(application).
-include("logging.hrl").

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
%% Called when the application terminates
%% ----------------------------------------------------------------------------
stop(_State) -> ok.	


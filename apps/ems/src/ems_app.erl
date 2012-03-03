-module(ems_app).
-export([start/2,stop/1]).
-behaviour(application).

%% ----------------------------------------------------------------------------
%% @doc The main entry point for the media server application
%% @spec start(Type,Args) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
start(normal, _) ->
	log:debug("ems:start/2", []),	

	log:debug("ems:start/2 - Loading configuration", []),	
	{ok, Config} = ems_config:load("local_config.erl"),

	log:debug("ems:start/2 - Starting overall supervisor", []),		
	case ems_supervisor:start_link(Config) of
		{ok, Pid} ->
			log:debug("ems_supervisor started", []), 
			{ok, Pid};

		Error ->
			log:err("ems:start/2 - failed to start ~w", [Error]), 
			Error
	end;

start(_,_) -> 
	{error, badarg}.

%% ----------------------------------------------------------------------------
%% Called when the application terminates
%% ----------------------------------------------------------------------------
stop(_State) -> ok.	


-module(erlang_media_server).
-behaviour(application).

-include("erlang_media_server.hrl").

-export([start/2, shutdown/0, stop/1]).

%% ============================================================================
%% External Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc The main entry point for the media server application
%% @spec start(Type,Args) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
start(normal, _Args) ->
	?LOG_INFO("erlang_media_server:start/2", []),
	
	case ems_supervisor:start_link([]) of
		{ok, Pid} -> {ok, Pid};
		%{ok, Pid, State} -> {ok, Pid, State};
		Error ->
			?LOG_ERROR("erlang_media_server:start/2 - failed to start ~w", [Error]), 
			Error
	end;
	
start(_,_) -> 
	{error, badarg}.
	
%% ----------------------------------------------------------------------------
%% @doc Called to shut down the media server application
%% @spec shutdown() -> ok
%% ----------------------------------------------------------------------------
shutdown() -> 
	application:stop(erlang_media_server).
	
%% ============================================================================
%% Internal Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% Called when the application terminates
%% ----------------------------------------------------------------------------
stop(State) -> 
	ok.
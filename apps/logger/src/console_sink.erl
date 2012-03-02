-module(console_sink).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_event).
-include("logger.hrl").

%% gen_event exports ----------------------------------------------------------
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ============================================================================
%% Public API
%% ============================================================================
init(_) -> {ok, {}}.

handle_event(_ = #log_msg{src = Src, level = Level, msg = Msg}, State) ->
  io:format("~w - [~c] - ~s~n", [Src, prefix(Level), Msg]),
  {ok, State}.

handle_call(_Request, State) -> {ok, State}. 

handle_info(_Info, State) -> {no, State}.

terminate(_Reason, _State) -> ok.

code_change(_,_,_) -> {error, "Hot swap not supported"}.

%% ============================================================================
%% Internal API
%% ============================================================================
-spec prefix(integer()) -> char().
prefix(?LOG_LEVEL_TRACE) -> $T;
prefix(?LOG_LEVEL_DEBUG) -> $D;
prefix(?LOG_LEVEL_INFO)  -> $I;
prefix(?LOG_LEVEL_WARN)  -> $W;
prefix(?LOG_LEVEL_ERROR) -> $E;
prefix(?LOG_LEVEL_FATAL) -> $F.

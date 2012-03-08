%% ============================================================================
%% @doc A log sink that writes the log message out to the console.
%% @end
%% ============================================================================

-module(console_sink).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_event).
-include("logger.hrl").

%% gen_event exports ----------------------------------------------------------
-export([init/1,
         handle_event/2,
         handle_call/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-type state() :: {log:level(), atom()}.

%% ============================================================================
%% Public API
%% ============================================================================

%% --------------------------------------------------------------------------
%% @doc Initialises a console sink for use with the logger.
%% @end
%% --------------------------------------------------------------------------
-spec init([term()]) -> {ok, state()}.
init(Options) -> 
  LogLevel = case lists:keyfind(log_level, 1, Options) of
                {log_level, L} -> log:level_to_int(L);
                false -> ?LOG_LEVEL_TRACE
             end,
  Stream = case lists:keyfind(stream, 1, Options) of
                {stream, S} -> S;
                false -> standard_io
           end,
 {ok, {LogLevel, Stream}}.

%% --------------------------------------------------------------------------
%% @doc Receives a logging message from the log server and writes it out to 
%%      the console.
%% @end
%% --------------------------------------------------------------------------
handle_event(_ = #log_msg{src = Src, level = Level, msg = Msg}, State) ->
  {LogLevel, Stream} = State,
  case Level of 
    N when N =< LogLevel -> 
      io:fwrite(Stream, "~w - [~c] - ~s~n", [Src, prefix(Level), Msg]);
    _ -> ok
  end,
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

-module(forwarding_sink).
-author("Trent Clarke <trent.clark@gmail.com>").
-behaviour(gen_event).
-include("logger.hrl").

%% gen_event exports ----------------------------------------------------------
-export([init/1,
         handle_event/2,
         handle_call/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

%% ============================================================================
%% Public API
%% ============================================================================

%% --------------------------------------------------------------------------
%% @doc Receives a logging message from the log server and writes it out to 
%%      the console.
%% --------------------------------------------------------------------------
init(Pid) -> {ok, Pid}.

%% --------------------------------------------------------------------------
%% @doc Receives a logging message from the log server and writes it out to 
%%      the console.
%% --------------------------------------------------------------------------
handle_event(_ = #log_msg{src = Src, level = Level, msg = Msg}, Pid) ->
  Pid ! {echo_message, Src, Level, Msg},
  {ok, Pid}.

handle_call(_Request, State) -> {ok, State}. 

handle_info(_Info, State) -> {no, State}.

terminate(_Reason, _State) -> ok.

code_change(_,_,_) -> {error, "Hot swap not supported"}.



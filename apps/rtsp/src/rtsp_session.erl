%% ============================================================================
%% @doc Implements an RTSP session process, responsible for keeping track of 
%%      all of the resources owned by a single RTSP session. Note that an 
%%      individual TCP connection can be active on multiple sessions at once, 
%%      and that sessions may last longer than the TCP connection that spawned
%%      them.
%%
%%      The one-process-per-session model may be a bit of overkill for our 
%%      purposes, so it's probably a good candidate for aggregating them if we
%%      start running low. 
%% @end
%% ============================================================================
-module(rtsp_session).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_server).

% Public API ------------------------------------------------------------------
-export([start/2, 
         stop/1]).

% get_server Exports ----------------------------------------------------------
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

% Type Definitions ------------------------------------------------------------
-record(state, {id :: string(),
                owner :: rtsp:user_id()}).

%% ============================================================================
%% Public API functions
%% ============================================================================
-spec start(Owner :: rtsp:user_id(), SessionId :: string) -> pid().

start(Owner, SessionId) ->
  State = #state{id = SessionId, owner = Owner},
  gen_server:start(?MODULE, State, []).

stop(Session) ->
  gen_server:call(Session, quit).

%% ============================================================================
%% gen_server API
%% ============================================================================

init(State) -> {ok, State}.

handle_call(_,_,State) -> {reply, {error, not_implemented}, State}.

handle_cast(_,State) -> {noreply, State}.

handle_info(_,State) -> {noreply, State}.

terminate(_, State) -> {ok, State}.

code_change(_,_,_) -> {error, "Hot update nor supported"}.

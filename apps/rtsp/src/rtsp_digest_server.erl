-module(rtsp_digest_server).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_server).
-include("digest.hrl").

-export([start_link/0, get_auth_nonce/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
  code_change/3]).

-define(SERVER_NAME, digest_auth_svr).

%% ============================================================================
%% Types
%% ============================================================================
-type svr() :: pid().
-type nonce() :: integer().
-type ctx() :: #digest_ctx{}.
-export_types([ctx/0]).
-opaque([svr/0]).

-record(state, {db :: dict(), timer :: timer:tref()}).
-type state() :: #state{}.

%% ============================================================================
%% Exported functions
%% ============================================================================
-spec start_link() -> {ok, svr()} | {error, any()}.
start_link() -> 
  gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% @doc Fetches (or creates if it's not present) the authentication context
%%      for a connection 
%% @end
%% ----------------------------------------------------------------------------
-spec get_auth_nonce(rtsp_connection:conn()) -> nonce().
get_auth_nonce(Conn) ->
  {ok, Ctx} = gen_server:call(?SERVER_NAME, {get_context, Conn}),
  Ctx#digest_ctx.nonce.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init(_) ->
  {ok, TRef} = queue_timer(),
  State = #state{db = dict:new(), timer = TRef},
  {ok, State}.

-spec handle_call(any(), rtsp_connection:conn(), state()) -> {reply, any(), state()}.
handle_call({get_context, Conn}, _, State) ->
  Db = State#state.db,
  {Ctx, DbP} = case dict:find(Conn, Db) of
                 {ok, C} -> 
                   {C, dict:update(Conn, fun touch/1, Db)};
                   
                 error -> 
                   C = new_context(),
                   {C, dict:store(Conn, C, Db)}
               end,
  StateP = State#state{db = DbP},
  {reply, Ctx, StateP};
  
handle_call(_,_,State) -> 
  {reply, [], State}.
  
handle_cast(_, State) -> 
  {noreply, State}.
  
handle_info(cull_expired, State) -> 
  {ok, TRef} = queue_timer(),
  StateP = State#state{timer = TRef},
  {noreply, StateP}.
  
terminate(_Reason, State) ->
  io:format("Terminated: ~w", [_Reason]),
  timer:cancel(State#state.timer).
  
code_change(_, State, _) -> 
  {ok, State}.

%% ============================================================================
%% Internals
%% ============================================================================
-spec new_context() -> ctx().
new_context() -> 
  Now = erlang:now(),
  Nonce = make_nonce(16),
  #digest_ctx{nonce = Nonce, created = Now, touched = Now}.

touch(Ctx) -> Ctx#digest_ctx{touched = erlang:now()}.

make_nonce(Size) ->
  F = fun(_,{P,N}) -> X = random:uniform(255),
                      {P+8, N + (X bsl P)}
      end, 
  {_,N} = lists:foldl(F, {0,0}, lists:seq(1,Size)),
  N.

queue_timer() -> 
  timer:send_after(timer:seconds(30), cull_expired).

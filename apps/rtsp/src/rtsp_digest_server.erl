-module(rtsp_digest_server).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_server).
-include("logging.hrl").
-include("digest.hrl").

-export([start_link/0, get_context/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
  code_change/3]).

-define(SERVER_NAME, digest_auth_svr).

%% ============================================================================
%% Types
%% ============================================================================
-type svr() :: pid().
-type ctx() :: #digest_ctx{}.
-export_type([svr/0, ctx/0]).
-opaque([svr/0]).

-record(state, { db               :: dict(), 
                 poll_interval_ms :: integer(),
                 age_cutoff_us    :: integer(), 
                 idle_cutoff_us   :: integer(),
                 timer            :: reference() }).
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
-spec get_context(rtsp:conn()) -> ctx().
get_context(Conn) ->
  gen_server:call(?SERVER_NAME, {get_context, Conn}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init(_) ->
  ?LOG_DEBUG("rtsp_digest_server:init/1: Starting", []),
  TimerInterval = 30000,
  TRef = queue_timer(TimerInterval),
  State = #state{db = dict:new(), 
                 poll_interval_ms = TimerInterval, 
                 age_cutoff_us    = timer:seconds(60),
                 idle_cutoff_us   = timer:seconds(10),
                 timer = TRef},
  {ok, State}.

%% ----------------------------------------------------------------------------
%% @doc Handles synchronous calls from the public API
%% @private
%% @end
%% ----------------------------------------------------------------------------

-spec handle_call(any(), pid(), state()) -> 
        {reply, any(), state()}.

handle_call({get_context, Conn}, _, State) ->
  Db = State#state.db,
  {Ctx, DbP} = case dict:find(Conn, Db) of
                 {ok, C} -> {C, dict:update(Conn, fun touch/1, Db)};
                 error ->  C = new_context(),
                           {C, dict:store(Conn, C, Db)}
               end,
  StateP = State#state{db = DbP},
  {reply, Ctx, StateP};
  
handle_call(_,_,State) -> 
  {reply, [], State}.
  
handle_cast(_, State) -> 
  {noreply, State}.
  
handle_info(cull_expired, State = #state{db = Db,
                                         poll_interval_ms = Interval,
                                         age_cutoff_us    = AgeCutoff, 
                                         idle_cutoff_us   = IdleCutoff}) -> 
  Now = erlang:now(),
  Filter = 
    fun(_,#digest_ctx{created = Created, touched = Touched}) ->
      (timer:now_diff(Now, Created) < AgeCutoff) andalso 
        (timer:now_diff(Now, Touched) < IdleCutoff)
    end,
    
  DbP    = dict:filter(Filter, Db),
  TRef   = queue_timer(Interval),
  StateP = State#state{db = DbP, timer = TRef},
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
  Now    = erlang:now(),
  Nonce  = make_nonce(8),
  Opaque = make_nonce(16),
  #digest_ctx{nonce = Nonce,
              opaque = Opaque, 
              created = Now, 
              touched = Now }.

touch(Ctx) -> Ctx#digest_ctx{touched = erlang:now()}.  

-spec make_nonce(integer()) -> string().
make_nonce(Size) ->
  F = fun(_,{P,N}) -> X = random:uniform(255),
                      {P+8, N + (X bsl P)}
      end, 
  {_,N} = lists:foldl(F, {0,0}, lists:seq(1,Size)),
  lists:flatten(io_lib:format([$~] ++ (integer_to_list(Size*2)) ++ ".16.0b", [N])).

queue_timer(Interval) -> 
  erlang:send_after(Interval, self(), cull_expired).

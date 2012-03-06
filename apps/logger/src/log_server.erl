-module(log_server).
-behaviour(gen_server).
-author("Trent Clarke <trent.clarke@gmail.com>").
-include("logger.hrl").

%% Public API ----------------------------------------------------------------
-export([start_link/0,
         add_sink/2,
         set_level/1, 
         get_level/0, 
         log_message/3, 
         stop/0]).

%% gen_server callbacks ------------------------------------------------------
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2, 
         terminate/2,
         code_change/3]).

-record( log_state, {level :: integer(), event_mgr :: pid()}).
-type log_state() :: #log_state{}.
-type log_rq() :: {log_rq, pid(), integer(), string(), [term()]}.

-type log_call() ::  get_log_level | 
                     {set_log_level, integer()} |
                     {add_sink, module(), term()} .
-type log_reply() :: ok | {ok, integer()}.

-type log_cast() :: log_rq().

%% ============================================================================
%% Public API
%% ============================================================================
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_server:start_link({local, log_server}, ?MODULE, [], []).

-spec stop() -> any().
stop() ->
  gen_server:call(log_server, quit).
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec set_level(log:log_level()) -> any().
set_level(LogLevel) when is_atom(LogLevel) ->
  case lists:member(LogLevel, [trace, debug, info, warn, err, fatal]) of
    true ->
      LevelNo = log:level_to_int(LogLevel),
      gen_server:call(log_server, {set_log_level, LevelNo});

    false -> 
      throw("bad log level")
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec get_level() -> log:log_level().
get_level() ->
  case gen_server:call(log_server, get_log_level) of
    {ok, LogLevel} -> log:int_to_level(LogLevel)
  end.

%% ----------------------------------------------------------------------------
%% @doc Adds a logging sink to the logger.
%% ----------------------------------------------------------------------------
-spec add_sink(module(), term()) -> any().
add_sink(SinkModule, Args) ->
  gen_server:call(log_server, {add_sink, SinkModule, Args}).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec log_message(log:log_level(), string(), [term()]) -> any().
log_message(Level, Fmt, Args) -> 
  gen_server:cast(log_server, {log_rq, self(), Level, Fmt, Args}).

%% ============================================================================
%% Internal API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Called on the new server process by the gen_server behaviour to 
%%      initialise the new service process
%% @end
%% ----------------------------------------------------------------------------
-spec init([]) -> {ok, log_state()}. 
init(_) ->
  case gen_event:start_link() of
    {ok, EventMgr} ->
      State = #log_state{ level = ?LOG_LEVEL_INFO, event_mgr = EventMgr },
      {ok, State};
    _ -> {stop, "Failed to create event manager"}
  end.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_call(log_call(), pid(), log_state()) ->
  {reply, Reply :: log_reply(), NewState :: log_state()}.

%% Handles setting the log level
handle_call({set_log_level, NewLevel}, _From, State) -> 
  StateP = State#log_state{level = NewLevel},
  {reply, ok, StateP};

%% Queries the current log level
handle_call(get_log_level, _From, State = #log_state{level = Level}) ->
  {reply, {ok, Level}, State};

%% Adds a new logging event sink to the logger
handle_call({add_sink, Module, Args}, _From, State) ->
  EventMgr = State#log_state.event_mgr,
  Result = gen_event:add_handler(EventMgr, Module, Args),
  {reply, Result, State};

handle_call(quit, _From, State) ->
  {stop, normal, ok, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a cast message.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_cast(log_cast(), log_state()) -> {noreply, NewState :: log_state()}.

% Handles a log request from a client
handle_cast({log_rq, Src, MsgLevel, Fmt, Args}, State) -> 
  LogLevel = State#log_state.level,
  case State#log_state.level of
    LogLevel when LogLevel >= MsgLevel ->
        EventMgr = State#log_state.event_mgr,

        {Text,Level} = case format_message(Fmt, Args) of
                         {ok, M} -> {M, MsgLevel};
                         {error, M} -> {M, ?LOG_LEVEL_ERROR}
                       end,
                        
        Msg = #log_msg{src = Src, level = Level, msg = Text},
        gen_event:notify(EventMgr, Msg);
    _ -> ok
  end,
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to an unknown message sent 
%%      directly to the server process.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_info(term(), log_state()) -> {noreply, log_state()}.
handle_info(_Info, State) -> {noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message.
%% @end
%% ----------------------------------------------------------------------------
-spec terminate(term(), log_state()) -> any().
terminate(_Reason, _State) -> ok.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a code up- or downgrade.
%% @end
%% ----------------------------------------------------------------------------
-spec code_change(any(), log_state(), any()) -> {error, term()}.
code_change(_,_,_) -> {error, "Hot swap not supported"}.


%% ----------------------------------------------------------------------------
%% @doc Safely formats a message. If formatting the message fails then the 
%%      resulting error will be caught and turned into an approprate failure 
%%      log message describing said failure.
%% ----------------------------------------------------------------------------
format_message(Fmt, Args) ->
  try
    Msg = case args of 
            [] -> Fmt;
            _ -> lists:flatten(io_lib:format(Fmt, Args))
          end,
    {ok, Msg}
  catch
    error:_ -> {error, "Failure when trying to format log message"}
  end.


%% ============================================================================
%% @doc Implements the session manager for the RTSP server. The session manager 
%%      is a simple server that keeps track of the active RTSP sessions. The 
%%      list of sessions is maintained as an ETS table that other process can
%%      query - the session manager process is only involved on session
%%      creation and deletion.
%% @end
%% ============================================================================
-module(rtsp_session_manager).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behavior(gen_server).

% Public API ------------------------------------------------------------------
-export([start_link/0, lookup/1]).

% get_server Exports ----------------------------------------------------------
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-export([start_table_manager/0, table_manager_main/0]).


% Type Definitions ------------------------------------------------------------
-record(state, {table :: ems:tid()}).
-type session_id() :: string().

%% ============================================================================
%% Public API functions
%% ============================================================================

start_link() ->
  log:debug("rtsp_session_mgr - starting session manager"),
  gen_server:start_link({local, rtsp_session_manager}, ?MODULE, {}, []).

%% ----------------------------------------------------------------------------
%% @doc Creates a session and registers it with the session manager.
%% @end
%% ----------------------------------------------------------------------------
create_session(Owner) ->
  gen_server:call({local, rtsp_session_manager}, {create_session, Owner}).

%% ----------------------------------------------------------------------------
%% @doc Looks up or creates a session.
%% @end
%% ----------------------------------------------------------------------------
-spec lookup(SessionId :: session_id()) -> false | pid().
lookup(SessionId) ->
  case ets:lookup(rtsp_session_list, SessionId) of
    [S|_] -> S;
    [] -> false
  end.

%% ============================================================================
%% Gen Server API functions
%% ============================================================================

init(_) ->
  log:debug("rtsp_session_mgr - entering session manager"),
  {A1,A2,A3} = now(),
  random:seed(A1,A2,A3),

  log:debug("rtsp_session_mgr - requesting table from table manager"),
  Tid = acquire_table(rtsp_session_table_manager),
  {ok, #state{table = Tid}}.
           
%% ----------------------------------------------------------------------------
%% @doc Handles a synchronous request from another process. 
%% @end
%% ----------------------------------------------------------------------------
handle_call({create_session, Owner}, _, State = #state{table = Tid}) ->
  NewSession = create_new_session(Tid, Owner),
  erlang:monitor(process, NewSession),
  ets:insert(Tid, NewSession),
  {reply, NewSession, State};

handle_call(_, _From, State) -> {reply, [], State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, State) -> {noreply, State}.

code_change(_,_,_) -> {error, "Hot upgrade not supported"}.

%% ============================================================================
%% Internal functions
%% ============================================================================

create_new_session(Tid, UserInfo) ->
  Id = generate_id(),
  case ets:lookup(Tid, Id) of
    [] -> rtsp_session:start(UserInfo, Id);
    _ -> create_new_session(Tid, UserInfo)
  end.

%% ----------------------------------------------------------------------------
%% @doc Generates a potential session ID string for a new session. Makes no 
%%      assertions about the uniqueness of the ID.
%% @end
%% ----------------------------------------------------------------------------
-spec generate_id() -> string().
generate_id() ->
  Id = utils:for_seq(fun(_,L) -> [random:uniform(256)-1 | L] end, [], 1, 16),
  utils:hex_string(Id).

%% ----------------------------------------------------------------------------
%% @doc Acquires the sessions table from the table manager process. Behaviour 
%%      when the table manager isn't the current owner is undefined. 
%% @end.
%% ----------------------------------------------------------------------------
acquire_table(TableManager) ->
  TableManager ! {get_table, self()},
  receive
    {'ETS-TRANSFER', TableId, From, _} -> 
      log:debug("rtsp_session_mgr - acquired table from ~w", [From]),
      TableId
  after
    500 ->
      {error, "Failed to acquire table"}
  end.

%% ----------------------------------------------------------------------------
%% @doc Starts the session table manager process. The table manager's job is to 
%%      wait around until the session manager crashes and to catch the 
%%      session-id-to-session mapping table when it does - prolonging the 
%%      lifespan of the table until the session manager is restarted.
%% @end
%% ----------------------------------------------------------------------------
start_table_manager() ->
  log:debug("rtsp_session_mgr - spawning table manager process"),
  Pid = erlang:spawn_link(?MODULE, table_manager_main, []),

  log:debug("rtsp_session_mgr - registering table manager process"),
  true = erlang:register(rtsp_session_table_manager, Pid),
  {ok, Pid}.

%% ----------------------------------------------------------------------------
%% @doc The main routine for the table manager. Creates the ETS table to store 
%%      sessions in and starts the message handling loop.
%% @end
%% ----------------------------------------------------------------------------
table_manager_main() ->
  log:debug("rtsp_session_mgr - entering table manager process"),
  Tid = ets:new(rtsp_session_list, [set, protected, named_table, 
                                    {keypos,1}, 
                                    {heir, self(), []}]),
  table_manager_loop(Tid).

%% ----------------------------------------------------------------------------
%% @doc The message-handling loop for the table manager. This process should be
%%      as minimal possible to reduce the chances of it crashing to as low 
%%      as possible  
%% @end
%% ----------------------------------------------------------------------------
table_manager_loop(Tid) ->
  receive
    {get_table, Pid} -> 
      ets:give_away(Tid, Pid, []),
      log:debug("rtsp_session_mgr - giving table to ~w", [Pid]),
      table_manager_loop(Tid);

    {'ETS-TRANSFER', TableId, FromPid, _} ->
      log:debug("rtsp_session_mgr - table has returned from ~w", [FromPid]),
      table_manager_loop(TableId);

    Msg -> 
      log:warn("rtsp_sesson_mgr - unexpected msg: ~w", [Msg]),
      table_manager_loop(Tid)
  end.



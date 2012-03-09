-module(ems_session_manager).
-behaviour(gen_server).

%% ============================================================================
%% Definitions
%% ============================================================================
-define(EMS_SESSION_MANAGER, ems_session_manager).

-type object_type() :: session | channel.
-record(named_object, {type :: object_type(), path :: string(), pid :: pid()}).
-type named_object() :: #named_object{type :: object_type(),
                                      path :: string(),
                                      pid :: pid()}.

-record(state, {table :: ets:tid(), config :: ems_config:handle()}).
-type state() :: #state{table :: ets:tid(),
                        config :: ems_config:handle()}.

%% gen_server exports --------------------------------------------------------- 
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
	
%% Public API exports --------------------------------------------------------- 
-export([start_link/0,
         create_session/3,
         lookup_object/1]).

%% ============================================================================
%% Public Interface
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the session manager
%% @end
%% ----------------------------------------------------------------------------
start_link() -> 
  log:debug("ems_session_manager:start_link/0",[]),
  gen_server:start_link({local,ems_session_manager}, ?MODULE, {}, []).

%% ----------------------------------------------------------------------------
%% @doc Creates and registers a session with the session manager. It is assumed 
%%      that the user has already been authenticated and validated by this 
%%      point.
%% @end
%% ----------------------------------------------------------------------------
-spec create_session(User :: ems:user_info(), 
                     Path :: string(), 
                     Desc :: sdp:session_description()) -> 
        {ok, ems:session()} | {error, term()}.

create_session(User, Path, Desc) ->
  log:debug("session_manager:create_session/3 - Creating session at ~w", [Path]),
  case ems_session:new(User, Path, Desc) of
    {ok, Session} -> 
      log:debug("session_manager:create_session/3 - Created."),
      Channels = ems_session:collect_channels(Path, Session),
      NamedSession = #named_object{type = session, path = Path, pid = Session},
      Fun = fun({P,C}) -> 
              #named_object{type = channel, path = P, pid = C} 
            end, 
      Names = [NamedSession | lists:map(Channels, Fun)],
      
      log:debug("session_manager:create_session/3 - Registering."),
      case gen_server:call(ems_session_manager, {register, Names}) of
        ok ->
          % monitor the sessions so that we can remove the registrations when 
          % the processes they reference die 
          Monitor = fun(_ = #named_object{pid = Pid}) -> 
                      erlang:monitor(Pid)
                    end,
          list:foreach(Monitor, Names),
          {ok, Session};

        {error, Err} -> 
          log:error("session_manager:create_session/2 - Registration failed"),
          ems_session:stop(Session),
          {error, Err}
      end;
    Err -> {error, Err}
  end.


%% ----------------------------------------------------------------------------
%% @doc 
%% @end
%% ----------------------------------------------------------------------------
-spec lookup_object(Path :: string()) -> 
        {session, pid()} | {channel, pid()} | false.

lookup_object(Path) ->
  case ets:lookup(ems_session_list, Path) of
    [H|_] -> 
      Type = H#named_object.type,
      Pid = H#named_object.pid,
      {Type, Pid};
    [] -> false
  end.

%% ============================================================================
%% Server Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Startup function for the session manager
%% @end
%% ----------------------------------------------------------------------------
init(_Args) ->
  log:debug("ems_session_manager:init/1"),
  process_flag(trap_exit, true),
    
  % seed the random number generator for this process
  log:debug("ems_session_manager:init/1 - Seeding RNG"),
  {A1,A2,A3} = now(),
  random:seed(A1,A2,A3),
  
  % create the ETS table for mapping sessions to processes
  log:debug("ems_session_manager:init/1 - Creating ETS tables"),
  Tid = ets:new(ems_session_list, [set,protected,named_table,{keypos,3}]),
  {ok, #state{table = Tid}}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a synchronous request
%% @end
%% ----------------------------------------------------------------------------
  
handle_call({register, NamedItems}, _, State = #state{table = Table}) ->
  case ets:insert_new(Table, NamedItems) of
    true -> {reply, ok, State};
    false -> {reply, error, "Insert failed"}
  end;

% the default implementation - does nothing
handle_call(_Request, _From, State) ->
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles an asynchronous request
%% @end
%% ----------------------------------------------------------------------------

% The default implementation. Does nothing.
handle_cast(_Request, State) ->
  log:debug("ems_session_manager:handle_cast/2 - ~w", [_Request]),
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a message not sent by the call or cast methods
%% @end
%% ----------------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, _Reason},  
            State = #state{table = Table}) ->
  log:debug("ems_session_manager:handle_info/2 - Pid ~p has quit", [Pid]),
  ets:delete_match(Table, {named_object, '_', '_', Pid}),
  {noreply, State};
  
handle_info(_Info, State) ->
  log:debug("ems_session_manager:handle_info/2 - ~w", [_Info]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Signals termination of the server process
%% @end
%% ----------------------------------------------------------------------------
terminate(Reason, _State) ->
  log:debug("ems_session_manager:terminate/2 ~w", [Reason]),
  ok.
  
%% ----------------------------------------------------------------------------
%% @doc Signals a code upgrade to the server process
%% @end
%% ----------------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
  log:debug("ems_session_manager:upgrade/3 - Upgrading from ~w", [_OldVersion]),
  {ok, State}.

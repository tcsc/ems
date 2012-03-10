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
%%      po%% @end
%% ----------------------------------------------------------------------------
-spec create_session(User :: ems:user_info(), 
                     Path :: string(), 
                     Desc :: sdp:session_description()) -> 
        {ok, ems:session()} | {error, term()}.

create_session(User, Path, Desc) ->
  log:debug("session_manager:create_session/3 - Creating session at ~s", [Path]),
  gen_server:call(ems_session_manager, {create_session, User, Path, Desc}).

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

% the internal handler for creating a session
handle_call({create_session, User, Path, Desc}, 
            _From, 
            State = #state{table = Table}) ->
  case ems_session:start_link(User, Path, Desc) of
    {ok, Session} ->

      % collect the steams and he session name and prepare them for being 
      % registered
      NamedSession = #named_object{type = session, path = Path, pid = Session},
      Fun = fun({P,C}) -> 
              #named_object{type = channel, path = P, pid = C} 
            end, 
      Channels = ems_session:get_channels(Session),
      NamedObjects = [NamedSession | lists:map(Fun, Channels)],
      
      % monitor the sessions so that we can remove the registrations when 
      % the processes they reference die 
      Monitor = 
        fun(_ = #named_object{pid = Pid}) -> 
          erlang:monitor(process, Pid)
        end,
      lists:foreach(Monitor, NamedObjects),

      % Add the set of names we want to register, explicitly failing if 
      % there is an entry already with that name
      log:debug("session_manager:handle_call/3 - Registering."),
      case ets:insert_new(Table, NamedObjects) of
        true -> {reply, {ok, Session}, State};
        false -> 
          log:debug("session_manager:handle_call/3 - Registration failed."),
          ems_session:stop(Session),
          {reply, error, "Insert failed"}
      end;

    Err -> 
      log:debug("session_manager:handle_call/3 - failed to create session: ~w", 
                [Err]),
      {reply, {error, Err}, State}
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

% handles the notification that one of the processes we're monitoring has died
% by removing the process from the sessions and channels ets lookup table
handle_info({'DOWN', _Ref, process, Pid, _Reason},  
            State = #state{table = Table}) ->
  log:debug("ems_session_manager:handle_info/2 - Pid ~p has quit", [Pid]),
  ets:match_delete(Table, {named_object, '_', '_', Pid}),
  {noreply, State};

% handles the notification that a session process has died. Any session-specific 
% cleanup code goes here.
handle_info({'EXIT', Pid, _Reason}, State) ->
  log:debug("ems_session_manager:handle_info/2 - Session ~p has exited", [Pid]),
  {noreply, State};

% Generic handler for all other unexpected messages
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

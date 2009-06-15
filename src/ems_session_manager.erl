-module(ems_session_manager).
-behaviour(gen_server).
-include("erlang_media_server.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-define(EMS_SESSION_MANAGER, ems_session_manager).
-record(session_info, {path, id, pid}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([
	init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
	]).

%% ============================================================================
%% External Functions
%% ============================================================================
-export([start_link/0, create_session/2, get_session_process/1]).

%% ============================================================================
%% Public Interface
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the session manager
%% @end
%% ----------------------------------------------------------------------------
start_link() -> 
  ?LOG_DEBUG("ems_session_manager:start_link/0",[]),
  gen_server:start_link({local,ems_session_manager}, ?MODULE, {}, []).

%% ----------------------------------------------------------------------------
%% @doc
%% @spec create_session(Path, Desc) -> Result
%%       Result -> {error, Reason} | {ok, Pid}
%%       Reason -> timeout | ErrorContext 
%% @end
%% ----------------------------------------------------------------------------
create_session(Path, Desc) ->
  try 
    case gen_server:call(ems_session_manager, {create_session, Path, Desc}) of
      {ok, Session} -> {ok, Session};
      {error, already_exists} -> {error, already_exists};
      {error, Reason} -> {error, Reason}
    end
  catch
    exit:{timeout,_} -> {error, timeout};
    _Type:Err -> {error, Err}
  end.

%% ----------------------------------------------------------------------------
%% @doc Maps a uri to a given session process.
%% @spec get_session_process(Path) -> false | Pid
%%       Path = string()
%%       Pid = pid()
%% @end
%% ----------------------------------------------------------------------------
get_session_process(Path) ->
  case find_session_by_uri(Path) of
    Session when is_record(Session, session_info) ->
      Session#session_info.pid;
    
    false -> false;
    
    Any -> false
  end.
  
%% ============================================================================
%% Server Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Startup function for the session manager
%% @end
%% ----------------------------------------------------------------------------
init(_Args) ->
  ?LOG_DEBUG("ems_session_manager:init/1",[]),
  
  % seed the random number generator for this process
  {A1,A2,A3} = now(),
  random:seed(A1,A2,A3),
  
  % create the ETS table for mapping sessions to processes
  ets:new(ems_session_list, [set,protected,named_table,{keypos,2}]),
  {ok, []}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a synchronous request
%% @end
%% ----------------------------------------------------------------------------

handle_call({create_session, Path, Desc}, _From, State) ->
  ?LOG_DEBUG("ems_session_manager:handle_call/3 - create_session ~s",[Path]),  
  {reply, internal_create_session(Path, Desc), State};
  
% the default implementation - does nothing
handle_call(_Request, _From, State) ->
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles an asynchronous request
%% @end
%% ----------------------------------------------------------------------------

% The default implementation. Does nothing.
handle_cast(_Request, State) ->
  ?LOG_DEBUG("ems_session_manager:handle_cast/2 - ~w", [_Request]),
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a message not sent by the call or cast methods
%% @end
%% ----------------------------------------------------------------------------
handle_info(_Info, State) ->
  ?LOG_DEBUG("ems_session_manager:handle_info/2 - ~w", [_Info]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Signals termination of the server process
%% @end
%% ----------------------------------------------------------------------------
terminate(Reason, _State) ->
  ?LOG_DEBUG("ems_session_manager:terminate/2 ~w", [Reason]),
  ok.
  
%% ----------------------------------------------------------------------------
%% @doc Signals a code upgrade to the server process
%% @end
%% ----------------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
  ?LOG_DEBUG("ems_session_manager:upgrade/3 - Upgrading from ~w", [_OldVersion]),
  {ok, State}.
  
%% ============================================================================
%% Internal functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Does the actual work of creating a session in response to a
%%      create_session message
%% @spec internal_create_session(Uri,Desc) -> {ok, Pid} | {error, Reason}
%%       Uri = string()
%%       Desc = session_description()
%%       Pid = pid()
%%       Reason = already_exists 
%% @end
%% ----------------------------------------------------------------------------
internal_create_session(Path, Desc) ->
  case find_session_by_uri(Path) of
    Session when is_record(Session, session_info) ->
      ?LOG_DEBUG("ems_session_manager: session already exists on this url", []),
      {error, already_exists};
      
    false -> 
      Id = random:uniform(99999999),
      {ok, Pid} = ems_session:start_link(Id, Path, Desc),

      ?LOG_DEBUG("ems_session_manager: New session id ~w on process ~w", [Id, Pid]),
      ets:insert(ems_session_list, #session_info{path = Path, id = Id, pid = Pid} ),
      
      {ok, Pid}
  end.
    
%% ----------------------------------------------------------------------------
%% @doc Finds a session process using its uri as a key
%% @spec find_session_by_uri(Uri) -> false | Pid
%%       Uri = string()
%%       Pid = pid()
%% @end
%% ----------------------------------------------------------------------------
find_session_by_uri(Uri) ->
  case ets:lookup(ems_session_list, Uri) of
    [Session|_] -> Session;
    [] -> false
  end.
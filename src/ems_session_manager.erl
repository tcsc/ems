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
	
-export([receive_rtsp_request/4]).

%% ============================================================================
%% External Functions
%% ============================================================================
-export([start_link/0, get_session_info/1]).

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
%% @doc Receives an RTSP request from the connection, does some preliminary
%%      parsing on the request and flings the semi-parsed results at the
%%      session manager process. 
%% @end
%% ----------------------------------------------------------------------------
receive_rtsp_request(Sequence, Request, Headers, Body) ->
  {Method, Uri, _, _, _} = rtsp:get_request_info(Request, Headers),
  {_,_,_,Path} = url:parse(Uri),
  ?LOG_DEBUG("ems_session_manager:receive_rtsp_request/4 - receving ~w ~s", 
    [Method, Uri]),
  gen_server:cast(ems_session_manager,
    {rtsp_request, self(),  {Method, Path, Sequence, Request, Headers, Body}}).

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
  
% the default implementation - does nothing
handle_call(_Request, _From, State) ->
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles an asynchronous request
%% @end
%% ----------------------------------------------------------------------------

handle_cast({rtsp_request, SenderPid, RequestArgs}, State ) ->
  {Method, Path, Sequence, Request, Headers, Body} = RequestArgs,
  NewState = handle_request(Method, Path, Sequence, Request, Headers, Body, SenderPid, State),
  {noreply, NewState};

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
%% @doc Looks up the session associated with the url and forwards the request
%%      to it. In some circumstances (e.g. an RTSP ANNOUNCE) this method will
%%      create the session first.
%% @end
%% ----------------------------------------------------------------------------

%% Specific RTSP ANNOUNCE case - the session will be created before being
%% passed the request
handle_request(announce, Path, Sequence, Request, Headers, Body, SenderPid, State) ->
  case create_session(Path, self()) of
    {ok, Pid} ->
      ems_session:receive_rtsp_request(Pid, Request, Headers, Body, SenderPid);
      
    {error, already_exists} ->
      rtsp_connection:send_response(SenderPid, Sequence, method_not_valid, [], <<>>)
  end,
  State;

%% The generic case - the request is simply passed on to the session 
%% (if it exists)
handle_request(_Method, Path, Sequence, Request, Headers, Body, ConnectionPid, State) ->
  case lookup_session_process(Path) of 
    {false, Reason} ->
      Status = case Reason of
        not_found -> not_found;
        invalid_path -> bad_request;
        
        _ -> server_error
      end,
      rtsp_connection:send_response(ConnectionPid, Sequence, Status, [], <<>>);
    
    {SessionPid, _} ->
      ems_session:receive_rtsp_request(SessionPid, Request, Headers, Body, ConnectionPid)
  end,
  State.
  
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
create_session(Path, Owner) ->
  case lookup_session_process(Path) of
    Session when is_record(Session, session_info) ->
      ?LOG_DEBUG("ems_session_manager: session ~s already exists on this url", [Path]),
      {error, already_exists};
      
    false -> 
      Id = random:uniform(99999999),
      {ok, Pid} = ems_session:start_link(Id, Path, Owner),

      ?LOG_DEBUG("ems_session_manager: New session for ~s: id ~w on process ~w",
        [Path, Id, Pid]),
      ets:insert(ems_session_list, #session_info{path = Path, id = Id, pid = Pid} ),
      
      {ok, Pid}
  end.

%% ----------------------------------------------------------------------------
%% @doc Finds a session by recursively trying each sub path - i.e. first it 
%%      tries the full path, then (if no session is found on the full path) it 
%%      tries the path minus everything after the last separator, and so on.  
%% @spec find_session(Path) -> Result
%%       Result = false | {SessionPid, SessionPath}
%% @end
%% ----------------------------------------------------------------------------
lookup_session_process([$/]) -> 
  false;

lookup_session_process(Path) ->
  case get_session_info(Path) of
    {false, not_found} -> 
      case string:rchr(Path, $/) of
        0 -> false;
        N -> 
          SubPath = string:substr(Path,1,N-1),
          lookup_session_process(SubPath)
      end;

    SessionInfo ->
      Pid = SessionInfo#session_info.pid,
      {Pid, Path} 
  end.

%% ----------------------------------------------------------------------------
%% @doc Finds a session record using its uri as a key
%% @spec get_session(Path) -> {error, Reason} | SessionPid
%%       Uri = string()
%%       SessionPid = pid()
%%       Reason = not_found | invalid_path
%% @end
%% ----------------------------------------------------------------------------
get_session_info($/) ->
  {false, invalid_path };
  
get_session_info(Path) ->
  case ets:lookup(ems_session_list, Path) of
    [Session|_] -> Session;
    [] -> {false, not_found}
  end.
-module(rtsp_server).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(gen_server).
-include("logging.hrl").
-include("rtsp.hrl").
-define(RTSP_SVR, rtsp_server).

%% ============================================================================
%% Type definitions
%% ============================================================================
-type svr() :: pid().

-record(listener, { 
  address  :: inet:ip_address(),
  port     :: integer(),
  listener :: listener:listener()
  }).
-type listener() :: #listener{}.

-record(state, { server_string :: string(),
                 listeners     :: [listener()],
                 auth_svr      :: rtsp_auth:svr(),
                 connections   :: [rtsp_connection:conn()]
               }).
-type state() :: #state{}.
-type_export([svr/0]).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3 ]).

%% ============================================================================
%% Public API
%% ============================================================================
-export([start_link/0, add_listener/3]).

%% ----------------------------------------------------------------------------
%% @doc Starts the RTSP server
%% @end
%% ----------------------------------------------------------------------------
-spec start_link() -> {ok, svr()} | {error, any()}.
start_link() ->
  ?LOG_DEBUG("rtsp_server:start_link/1", []),

  State = #state{ server_string = "EMS RTSP Service/0.1",
                  listeners     = [],
                  connections   = [] },
  
  gen_server:start_link({local,?RTSP_SVR}, ?MODULE, State, []).
    
%% ----------------------------------------------------------------------------
%% @doc Adds a local network binding to the RTSP server. The RTSP server 
%%      attempts to bind to the supplied local address and port and if 
%%      successful, will apply the request callback to any RTSP requests 
%%      from connections that are accepted from this binding.
%%
%%      Note that the callback handler is guaranteed <em>not</em> to be invoked 
%%      on the underlying connection's process, making calls back to the RTSP 
%%      connection deadlock-safe.
%% @end
%% ----------------------------------------------------------------------------
-spec add_listener(inet:ip_address(), integer(), rtsp:request_callback()) -> 
        {ok, listener:listener() } | {error, any()}.
add_listener(Address, Port, Callback) ->
  ?LOG_DEBUG("rtsp_server:add_listener/3 - ~w:~w", [Address, Port]),
  gen_server:call(rtsp_server, {bind, Address, Port, Callback}).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Handles a new connection from the TCP Listener. Invoked via a lambda 
%%      by the listener process.
%% @private
%% @end
%% ----------------------------------------------------------------------------
-spec new_connection(inet:socket(), 
                     inet:ip_address(), 
                     rtsp:request_callback()) -> ok.
new_connection(Socket, _Addr, RequestCallback) ->
  ?LOG_DEBUG("rtsp_server:new_connection/3 - spawning process to handle connection", []),
  {ok, Conn} = gen_server:call(rtsp_server, {spawn_connection, RequestCallback}),
  
  ?LOG_DEBUG("rtsp_server:new_connection/3 - forwarding socket to connection", []),
  % NB: must be called from the process that currently owns the socket (i.e. the 
  % listener process in this case), otherwise re-assigning ownership of the 
  % socket will fail.
  rtsp_connection:take_socket(Conn, Socket),
  ok.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
  
%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server framework to initialise the rtsp server
%% @spec init(State) -> {ok,State}
%% @end
%% ----------------------------------------------------------------------------
init(State) ->
  ?LOG_DEBUG("rtsp_server:init/1",[]),
  process_flag(trap_exit, true),
  {ok, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message.
%% @private
%% @end
%% ----------------------------------------------------------------------------
-spec handle_call(any(), pid(), state()) -> {reply, any(), state()}.
handle_call({spawn_connection, Callback}, _From, State) ->
  ?LOG_DEBUG("rtsp_server:new_connection/1 - spawning connection handler", []),
  ServerString = State#state.server_string,
  case rtsp_connection:new(self(), ServerString, Callback) of 
    {ok, Pid} -> Cs = [ Pid | State#state.connections ],
                 StateP = State#state{ connections = Cs },
                 {reply, {ok,Pid}, StateP};
    {error, _} -> {reply, error, State}
  end;

handle_call({bind, Address, Port, Callback}, _From, State) ->
  ?LOG_DEBUG("rtsp_server:handle_call/3 - attempting to bind to ~w:~w", [Address, Port]),
  AcceptCallback = fun(Socket, RemoteAddr) ->
    new_connection(Socket, RemoteAddr, Callback) 
  end,
  case listener:add(Address, Port, AcceptCallback) of
    {ok, L} -> Listener = #listener{ address  = Address,
                                     port     = Port,
                                     listener = L },
               Ls = [ Listener | State#state.listeners ],
               StateP = State#state{listeners = Ls},
               {reply, {ok, L}, StateP};
        
    Err -> ?LOG_ERROR("rtsp_server:handle_call/3 - bind failed with ~w", [Err]),
           {reply, {error, Err}, State}
  end;
  
handle_call(_Request, _From, State) ->
  ?LOG_DEBUG("rtsp_server:handle_call/3 ~w",[_Request]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @end
%% ----------------------------------------------------------------------------
handle_cast(_Request, State) ->
  ?LOG_DEBUG("rtsp_server:handle_cast/2 ~w",[_Request]),
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ---------------------------------------------------------------------------- 

handle_info({'EXIT', Pid, _Reason}, State) ->
  Cs = lists:delete(Pid, State#state.connections),
  StateP = State#state{connections = Cs},
  {noreply, StateP};

handle_info(_Info, State) ->
  ?LOG_DEBUG("rtsp_server:handle_info/2 - ~w",[_Info]),
  {noreply, State}.

terminate(Reason, _State) ->
  ?LOG_DEBUG("rtsp_server:terminate/2 - ~w",[Reason]),
  ok.

code_change(_OldVersion, State, _Extra) ->
  ?LOG_DEBUG("rtsp_server:code_change/3",[]),
  {ok, State}.
-module(rtsp_server).
-behaviour(gen_server).

-include("logging.hrl").
-include("rtsp.hrl").
-define(RTSP_SVR, rtsp_server).

%% ============================================================================
%% Type definitions
%% ============================================================================
-type_export([svr/0]).

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
-type rtsp_server_state() :: #state{}.

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
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
	?LOG_DEBUG("rtsp_server:start_link/1", []),

	State = #state{ server_string = "EMS RTSP Service/0.1",
	                listeners     = [],
									connections   = [] },
	
	gen_server:start_link({local,?RTSP_SVR}, ?MODULE, State, []).
		
%% ----------------------------------------------------------------------------
%% @doc Adds a local network binding to the RTSP server
%% @end
%% ----------------------------------------------------------------------------
-spec add_listener(inet:ip_addr(), integer(), rtsp:request_callback()) -> {ok, listener:listener() } | 
                                                                          {error, any()}.
add_listener(Address, Port, Callback) ->
	?LOG_DEBUG("rtsp_server:add_listener/3 - ~w:~w", [Address, Port]),
	gen_server:call(rtsp_server, {bind, Address, Port, Callback}).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Handles 
%% @spec new_connection(Socket) -> ok
%% @end
%% ----------------------------------------------------------------------------
new_connection(Svr, Socket, _Addr, Callback) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - spawning process to handle connection, ~w", [Svr]),
	{ok, Conn} = gen_server:call(Svr, {spawn_connection, Callback}),
	
	?LOG_DEBUG("rtsp_server:new_connection/1 - forwarding socket to connection", []),
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
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call({spawn_connection, Callback}, _From, State) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - spawning connection handler", []),
	ServerString = State#state.server_string,
	case rtsp_connection:new(self(), ServerString, Callback) of 
		{ok, Pid} -> Cs = [ Pid | State#state.connections ],
		             StateP = State#state{ connections = Cs },
		             {reply, {ok,Pid}, StateP};
		err -> {reply, error, State}
	end;

handle_call({bind, Address, Port, Callback}, _From, State) ->
	?LOG_DEBUG("rtsp_server:handle_call/3 - attempting to bind to ~w:~w", [Address, Port]),
	Me = self(),
	AcceptCallback = fun(Socket, RemoteAddr) ->
		new_connection(Me, Socket, RemoteAddr, Callback) 
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

handle_call({request, _Request, _Body}, _From, State) ->
	{noreply, State};

handle_call(_Request, _From, State) ->
	?LOG_DEBUG("rtsp_server:handle_call/3 ~w",[_Request]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @spec handle_cast(Request,From,State) -> {noreply,State}
%% @end
%% ----------------------------------------------------------------------------
handle_cast({new_connection, Socket}, State) ->
	?LOG_DEBUG("rtsp_server:handle_cast/2 (new_request) on ~w",[Socket]),
	rtsp_connection:start_link(Socket),
	{noreply, State};

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

%% ----------------------------------------------------------------------------
%% @doc Generates a dictionary of the default set of headers for any given 
%%      response.
%% @spec default_headers(State) -> dictionary()
%% @end
%% ---------------------------------------------------------------------------- 
-spec default_headers(rtsp_server_state()) -> any(). 
default_headers(_State = #state{server_string=Server}) ->
  dict:append(?RTSP_HEADER_SERVER, Server, dict:new()).
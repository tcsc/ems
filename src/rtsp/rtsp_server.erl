-module(rtsp_server).
-behaviour(gen_server).

-include("erlang_media_server.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% Type definitions
%% ============================================================================
-type rtsp_svr() :: pid().
-export_type([rtsp_svr/0]).

-record(state, { server_string :: string(),
                 listener_mgr  :: ems_listener:listener_sup(),
                 listeners     :: ems_listener:listener()
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
    code_change/3
	]).

%% ============================================================================
%% Exported Functions
%% ============================================================================
-export([start_link/2, add_listener/3]).

-compile(inline).
well_known() -> rtsp_server. 

%% ----------------------------------------------------------------------------
%% @doc Starts the RTSP server
%% @end
%% ----------------------------------------------------------------------------
-spec start_link(ems_listener:mgr(), any()) -> {ok, rtsp_svr()} | {error, any()}.
start_link(Lm, {rtsp, Config}) ->
	?LOG_DEBUG("rtsp_server:start_link/1 - Config: {~w, {rtsp, ~w}}", [Lm, Config]),

	State = #state{ server_string = "EMS RTSP Service/0.1",
	                listener_mgr  = Lm,
	                listeners     = [] },
	
	case gen_server:start_link({local,well_known()}, ?MODULE, State, []) of
		{ok, Pid} -> create_listeners(Pid, Lm, Config),
								 {ok, Pid};
		Err -> Err
	end.
		
create_listeners(RtspSvr, Lm, Config) ->
	Ports = case lists:keyfind(ports, 1, Config) of
						{ports, Ps} -> Ps;
						false -> [554]
					end,

	Bind = fun(P) -> 
		{ok, _Pid} = add_listener(RtspSvr, {0,0,0,0}, P)
	end,
	
	lists:foreach(Bind, Ports).
	

%% ----------------------------------------------------------------------------
%% @doc Adds a local network binding to the RTSP server
%% @end
%% ----------------------------------------------------------------------------
-spec add_listener(rtsp_svr(), inet:ip_addr(), integer()) -> {ok, ems_listener:listener() }| {error, any()}.
add_listener(Svr, Address, Port) ->
	?LOG_DEBUG("rtsp_server:add_listener/3 - ~w:~w", [Address, Port]),
	gen_server:call(Svr, {bind, Address, Port}).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Handles 
%% @spec new_connection(Socket) -> ok
%% @end
%% ----------------------------------------------------------------------------
new_connection(Svr, Socket, _Addr) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - spawning process to handle connection, ~w", [Svr]),
	{ok, Conn} = gen_server:call(Svr, spawn_connection),
	
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
	{ok, State}.
	
%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call(spawn_connection, _From, State) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - spawning connection handler", []),
	{ok, Pid} = rtsp_connection:new(self()),
	{reply, {ok,Pid}, State};

handle_call({bind, Address, Port}, _From, State = #state{ listener_mgr = Lm }) ->
	?LOG_DEBUG("rtsp_server:handle_call/3 - attempting to bind to ~w:~w", [Address, Port]),
	Me = self(),
	Callback = fun(Socket, RemoteAddr) -> new_connection(Me, Socket, RemoteAddr) end,
	case ems_listener:add(Lm, Address, Port, Callback) of
		{ok, L} -> Ls = [ L | State#state.listeners ],
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

handle_info(_Info, State) ->
	?LOG_DEBUG("rtsp_server:handle_info/2",[]),
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
-module(rtsp_server).
-behaviour(gen_server).

-include("erlang_media_server.hrl").
-include("rtsp.hrl").

-record(rtsp_server_state, {server_string}).

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
-export([start_link/0, new_connection/1]).

%% ----------------------------------------------------------------------------
%% @doc Starts the RTSP server
%% @spec start_link() -> {ok,Pid}
%% @end
%% ----------------------------------------------------------------------------
start_link() ->
	?LOG_DEBUG("rtsp_server:start_link/0",[]),
	State = #rtsp_server_state{server_string="EMS RTSP Service/0.1"},
	gen_server:start_link({local,rtsp_server}, ?MODULE, State, []).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Handles 
%% @spec new_connection(Socket) -> ok
%% @end
%% ----------------------------------------------------------------------------
new_connection(Socket) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - signalling ", []),
	{ok, Pid} = gen_server:call(rtsp_server, spawn_connection),
	
	?LOG_DEBUG("rtsp_server:new_connection/1 - reassigning socket ownership to ~w", [Pid]),
	gen_tcp:controlling_process(Socket, Pid),
	
	?LOG_DEBUG("rtsp_server:new_connection/1 - forwarding socket to connection", []),
	gen_fsm:send_event(Pid, {socket, Socket}),
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
	ems_listener:add({0,0,0,0}, 4321, ?MODULE),
	{ok, State}.
	
%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call(spawn_connection, _From, State) ->
	?LOG_DEBUG("rtsp_server:new_connection/1 - spawning connection handler", []),
	{ok, Pid} = rtsp_connection:start_link(self()),
	{reply,{ok,Pid},State};

handle_call({request, Request, Body}, _From, State) ->
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
default_headers(State = #rtsp_server_state{server_string=Server}) ->
  dict:append(?RTSP_HEADER_SERVER, Server, dict:new()).
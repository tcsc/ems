-module(ems_listener).
-author('Trent Clarke <trent.clarke@gmail.com>').
-behaviour(supervisor).

-include("erlang_media_server.hrl").

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/0, init/1, add/3]).

%% ============================================================================
%% Internal Exports
%% ============================================================================
-export([init_listener/1, run_listener/1, accept/1]).

%% ============================================================================
%% Records, Macros, etc.
%% ============================================================================
-record(listener_state, {name, ip, port, pid, socket, callback_module}).

%% ----------------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% ----------------------------------------------------------------------------
start_link() ->
	?LOG_DEBUG("ems_listener:start_link/0 - Starting Listener Supervisor", []),
	case supervisor:start_link({local, ems_listeners}, ?MODULE, []) of
		{ok, Pid} -> 
			?LOG_DEBUG("ems_listener:start_link/0 - Listener Supervisor started on ~w", [Pid]),
			{ok, Pid}
	end.
	
%% ----------------------------------------------------------------------------
%% @doc Called by the supervisor framework to find out about the child 
%%      process(es) of the supervisor and their restart strategy.
%%
%% @spec init(Args) -> {ok, {{Strategy,MaxRetries,Timeout}, [ChildSpec]}}
%%
%% @end  
%% ----------------------------------------------------------------------------
init(_Args) ->
	?LOG_DEBUG("ems_listener:init/1 ~w", [_Args]),
	{ok, {{one_for_one, 10, 1}, []}}.
	
%% ----------------------------------------------------------------------------	
%% @doc Starts an individual TCP listener process
%% @spec start_listener(LocalAddress,Port,CallbackModule) -> {ok, Pid} | {error, Reason}
%% where Callback = {Module,Function,Args}
%%       Module = Callback module
%%       Function = fun(Socket,Args) -> {ok} | {error, Reason}
%%       Args = [term()]
%% @end
%% ----------------------------------------------------------------------------	
add(LocalAddress, Port, CallbackModule) -> 
	?LOG_DEBUG("ems_listener:add_listener/2 - starting listener for ~w:~w", [LocalAddress, Port]),
	Text = io_lib:format("ems_listener_~w:~w", [LocalAddress,Port]),
	Name = Name = list_to_atom(lists:flatten(Text)),
	State = #listener_state{name=Name, ip=LocalAddress, port=Port, callback_module=CallbackModule},
	ChildSpec = {
		Name,
		{?MODULE, init_listener, [State]},
		transient,
		brutal_kill,
		worker,
		[?MODULE]},
	supervisor:start_child(ems_listeners, ChildSpec).

%% ----------------------------------------------------------------------------		
%% @spec init_listener(State) -> {ok,Pid} | {error, Reason}.
%% ----------------------------------------------------------------------------	
init_listener(State) ->
	?LOG_DEBUG("ems_listener:init_listener/1",[]),
	case proc_lib:start_link(?MODULE, run_listener, [State]) of
		{ok,Pid} ->
			?LOG_DEBUG("ems_listener:init_listener/1 - started listener on ~w",[Pid]),
			{ok, Pid};
			
		{error, Reason} ->
			?LOG_DEBUG("ems_listener:init_listener/1 - listener starup failed ~w",[Reason]), 
			{error, Reason}
	end.

%% ----------------------------------------------------------------------------	
%% @doc Entry point for the TCP listener process. Creates listener socket and 
%%      then starts the acceptor loop.
%% @spec run_listener(State) -> 
%% @end
%% ----------------------------------------------------------------------------	
run_listener(State = #listener_state{ip=Address, port=Port}) ->
	?LOG_DEBUG("ems_listener:run_listener/1 - entering new listener process", []),
	
	TcpOptions = [
		binary,
		{packet, 0},
		{active, false},
		{reuseaddr, true},
		{ip, Address}],
	
	case gen_tcp:listen(Port, TcpOptions) of
		{ok, Socket} ->
			% let the parent process know that we're alive 
			proc_lib:init_ack({ok, self()}),
			NewState = State#listener_state{socket=Socket},
			
			% start the acceptor loop
			accept(NewState);
			
		{error, Reason} ->
			?LOG_DEBUG("ems_listener:run_listener/1 - listen failed ~w", [Reason]),
			throw({Reason, Port})
	end.

%% ----------------------------------------------------------------------------
%% @doc Implements the accept loop for the listener process.
%% @spec Accept(State) -> State.
%% @end
%% ----------------------------------------------------------------------------
accept(State = #listener_state{socket=Socket, callback_module=CallbackModule}) ->
	%?LOG_DEBUG("ems_listener:accept/1 - accept", []),

	case gen_tcp:accept(Socket) of
		{ok, Connection} ->
			{ok, PeerAddress} = inet:peername(Connection),
			?LOG_DEBUG("ems_listener:accept/1 - new connection from ~w", [PeerAddress]),			
			new_connection(Connection, CallbackModule),
			accept(State);
			
		{error, Reason} ->
			?LOG_ERROR("ems_listener:accept/1 - accept on ~w ailed ~w)", [Socket, Reason]),
			accept(State)
	end.
	
new_connection(Socket, CallbackModule) ->
	?LOG_DEBUG("ems_listener:new_connection/2: Callback is ~w", [CallbackModule]),
	CallbackModule:new_connection(Socket).
	
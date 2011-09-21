-module(ems_listener).
-author('Trent Clarke <trent.clarke@gmail.com>').
-behaviour(supervisor).

-include("erlang_media_server.hrl").

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/0, init/1, add/3, remove/1]).

%% ============================================================================
%% Internal Exports
%% ============================================================================
-export([well_known/0, init_listener/1, run_listener/1, accept/1]).

%% ============================================================================
%% Records, Macros, etc.
%% ============================================================================
-type mgr() :: pid().
-type listener() :: {term(), pid()}.
-type accept_callback() :: fun((inet:socket(), inet:ip_address()) -> any()).
-export_type([mgr/0, listener/0, accept_callback/0]).

-record(listener_state, { name     :: string(), 
                          ip       :: inet:ip_address(), 
												  port     :: integer(),	
												  pid      :: pid(), 
												  socket   :: inet:socket(), 
												  callback :: accept_callback() }).
-type listener_state() :: #listener_state{}.

-compile(inline).
well_known() -> ems_listeners.

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------
-spec start_link() -> {'ok', mgr()} | {'ignore'} | {'error', any()}.
start_link() ->
	?LOG_DEBUG("ems_listener:start_link/0 - Starting Listener Supervisor", []),
	case supervisor:start_link({local, well_known()}, ?MODULE, []) of
		{ok, Pid} -> 
			?LOG_DEBUG("ems_listener:start_link/0 - Listener Supervisor started on ~w", [Pid]),
			{ok, Pid}
	end.

%% ----------------------------------------------------------------------------	
%% @doc Starts an individual TCP listener process
%% @end
%% ----------------------------------------------------------------------------
-spec add(inet:ip_address(), integer(), accept_callback()) -> {'ok', pid()} | {'error', any()}.
add(LocalAddress, Port, Callback) -> 
	Supervisor = well_known(),
	
	?LOG_DEBUG("ems_listener:add_listener/2 - starting listener for ~w:~w", [LocalAddress, Port]),
	Text = io_lib:format("ems_listener_~w:~w", [LocalAddress,Port]),
	Name = Name = list_to_atom(lists:flatten(Text)),
	State = #listener_state{name=Name, ip=LocalAddress, port=Port, callback=Callback},
	ChildSpec = {
		Name,
		{?MODULE, init_listener, [State]},
		transient,
		brutal_kill,
		worker,
		[?MODULE]},
	
	case supervisor:start_child(Supervisor, ChildSpec) of
		{ok, Pid} -> 
			{Id, _, _, _} = lists:keyfind(Pid, 2, supervisor:which_children(Supervisor)),
			{ok, {Id, Pid}};
		
		Err -> Err
	end.
	
-spec remove(listener()) -> ok.
remove({Id, Pid}) ->
	Supervisor = well_known(),
	
	?LOG_DEBUG("ems_listener:remove/2 - terminating listener ~w", [Id]),
	supervisor:terminate_child(Supervisor, Id),
	
	?LOG_DEBUG("ems_listener:remove/2 - deleting listener child spec", []),
	supervisor:terminate_child(Supervisor, Id),
	ok.
	
%% ----------------------------------------------------------------------------
%% @doc Called by the supervisor framework to find out about the child 
%%      process(es) of the supervisor and their restart strategy.
%%
%% @spec init(Args) -> {ok, {{Strategy,MaxRetries,Timeout}, [ChildSpec]}}
%%
%% @end  
%% ----------------------------------------------------------------------------
init(_Args) ->
	?LOG_DEBUG("ems_listener:init/1 - ~w", [_Args]),
	{ok, {{one_for_one, 10, 1}, []}}.

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
%% @end
%% ----------------------------------------------------------------------------
-spec accept(listener_state()) -> any().
accept(State = #listener_state{socket=Socket, callback=Callback}) ->
	%?LOG_DEBUG("ems_listener:accept/1 - accept", []),

	case gen_tcp:accept(Socket) of
		{ok, NewConnection} ->
			{ok, PeerAddress} = inet:peername(NewConnection),
			?LOG_DEBUG("ems_listener:accept/1 - new connection from ~w", [PeerAddress]),			
			Callback(NewConnection, PeerAddress),
			accept(State);
			
		{error, Reason} ->
			?LOG_ERROR("ems_listener:accept/1 - accept on ~w failed ~w)", [Socket, Reason]),
			accept(State)
	end.
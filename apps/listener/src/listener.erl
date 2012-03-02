-module(listener).
-author('Trent Clarke <trent.clarke@gmail.com>').
-behaviour(supervisor).

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/0, stop/0, init/1, add/3, remove/1]).

%% ============================================================================
%% Internal Exports
%% ============================================================================
-export([init_listener/1, run_listener/1, accept/1]).

%% ============================================================================
%% Records, Macros, etc.
%% ============================================================================
-opaque listener() :: {term(), pid()}.
-type accept_callback() :: fun((inet:socket(), {inet:ip_address(), integer()}) -> any()).
-export_type([listener/0, accept_callback/0]).

-record(listener_state, { name     :: string(), 
                          ip       :: inet:ip_address(), 
												  port     :: integer(),	
												  pid      :: pid(), 
												  socket   :: inet:socket(), 
												  callback :: accept_callback() }).
-type listener_state() :: #listener_state{}.

-define(LISTENER_PROC, tcp_listener).

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', any()}.
start_link() ->
	log4erl:info("listener:start_link/0 - Starting Listener Supervisor", []),
	case supervisor:start_link({local, ?LISTENER_PROC}, ?MODULE, []) of
		{ok, Pid} -> 
      log4erl:info("listener:start_link/0 - Listener Supervisor started on ~w", [Pid]),
			{ok, Pid};
			
		E ->
		  log4erl:fatal("listener:start_link/0 - Listener Supervisor failed to start ~w", [E]),
			E
	end.

stop() -> ok.

%% ----------------------------------------------------------------------------	
%% @doc Starts an individual TCP listener process
%% @end
%% ----------------------------------------------------------------------------
-spec add(inet:ip_address(), integer(), accept_callback()) -> {'ok', listener()} | {'error', any()}.
add(LocalAddress, Port, Callback) -> 
	log4erl:debug("listener:add_listener/3 - starting listener for ~w:~w", [LocalAddress, Port]),
	Text = io_lib:format("listener_~w:~w", [LocalAddress,Port]),
	Name = Name = list_to_atom(lists:flatten(Text)),
	State = #listener_state{name=Name, ip=LocalAddress, port=Port, callback=Callback},
	ChildSpec = {
		Name,
		{?MODULE, init_listener, [State]},
		transient,
		brutal_kill,
		worker,
		[?MODULE]},
	
	case supervisor:start_child(?LISTENER_PROC, ChildSpec) of
		{ok, Pid} -> 
			{Id, _, _, _} = lists:keyfind(Pid, 2, supervisor:which_children(?LISTENER_PROC)),
			{ok, {Id, Pid}};
		
		{error, Err} ->
		  log4erl:error("listener:add_listener/3 - failed to start listener: ~w", [Err]), 
		  {error, Err}
	end.
	
-spec remove(listener()) -> ok.
remove({Id, _Pid}) ->
	log4erl:debug("listener:remove/2 - terminating listener ~w", [Id]),
	supervisor:terminate_child(?LISTENER_PROC, Id),
	
	log4erl:debug("listener:remove/2 - deleting listener child spec", []),
	supervisor:terminate_child(?LISTENER_PROC, Id),
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
	log4erl:debug("listener:init/1 - ~w", [_Args]),
	{ok, {{one_for_one, 10, 1}, []}}.

%% ----------------------------------------------------------------------------		
%% @spec init_listener(State) -> {ok,Pid} | {error, Reason}.
%% ----------------------------------------------------------------------------	
init_listener(State) ->
	log4erl:debug("listener:init_listener/1",[]),
	case proc_lib:start_link(?MODULE, run_listener, [State]) of
		{ok,Pid} ->
      log4erl:debug("listener:init_listener/1 - started listener on ~w",[Pid]),
			{ok, Pid};
			
		{error, Reason} ->
      log4erl:error("listener:init_listener/1 - listener starup failed ~w",[Reason]), 
			{error, Reason}
	end.

%% ----------------------------------------------------------------------------	
%% @doc Entry point for the TCP listener process. Creates listener socket and 
%%      then starts the acceptor loop.
%% @end
%% ----------------------------------------------------------------------------
run_listener(State = #listener_state{ip=Address, port=Port}) ->
	log4erl:debug("listener:run_listener/1 - entering new listener process", []),
	
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
      log4erl:error("listener:run_listener/1 - listen failed ~w", [Reason]),
			throw({Reason, Port})
	end.

%% ----------------------------------------------------------------------------
%% @doc Implements the accept loop for the listener process.
%% @end
%% ----------------------------------------------------------------------------
-spec accept(listener_state()) -> any().
accept(State = #listener_state{socket=Socket, callback=Callback}) ->
	case gen_tcp:accept(Socket) of
		{ok, NewConnection} ->
			{ok, {RemoteAddr, Port}} = inet:peername(NewConnection),
      log4erl:debug("listener:accept/1 - new connection from ~w:~w", [RemoteAddr, Port]),
			accept(State);
			
		{error, Reason} ->
      log4erl:error("listener:accept/1 - accept on ~w failed ~w)", [Socket, Reason]),
			accept(State)
	end.

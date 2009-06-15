-module(ems_server).
-behaviour(gen_server).
-include("erlang_media_server.hrl").

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
-export([start_link/0]).

%% ============================================================================
%% Records, macros, etc
%% ============================================================================
-record(server_state, {name}).

%% ----------------------------------------------------------------------------
%% @doc Starts the network server on a given IP address & port pair.
%% @spec start_link(IpAddress,Port) -> {ok, Pid} | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
start_link() ->
	?LOG_INFO("ems_server:start_link/0", []),
	State = #server_state{name=ems_server},
	
	case gen_server:start_link({local,ems_server}, ?MODULE, State, []) of
		{ok,Pid} -> 
			?LOG_DEBUG("ems_server:start_link/0 - server started on ~w", [Pid]),
			{ok,Pid};
		Error ->
			?LOG_DEBUG("ems_server:start_link/0 - server failed to start ~w", [Error]),
			Error
	end.
			

%% ----------------------------------------------------------------------------
%% @doc Stops the network server.
%% @end
%% ----------------------------------------------------------------------------
stop(_State) ->
	gen_server:cast(ems_server, stop).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Called back by the gen_server framework to do the real initialisation 
%%      work.
%% @spec init(State) -> {ok,State} | 
%%                      {ok,State,Timeout} | 
%%                      {ok,State,hibernate} |
%%                      {stop, Reason} |
%%                      ignore
%% @end
%% ----------------------------------------------------------------------------
init(State) ->
	?LOG_INFO("ems_server:init/1", []),
	{ok, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	?LOG_DEBUG("ems_server:handle_call/3",[]),
	{noreply, State}.
	
%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @spec handle_cast(Request,From,State) -> {noreply,State}
%% @end
%% ----------------------------------------------------------------------------
handle_cast(_Request, State) ->
	?LOG_DEBUG("ems_server:handle_cast/2 ~w",[_Request]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
handle_info(_Info, State) ->
	?LOG_DEBUG("ems_server:handle_info/2",[]),
	{noreply, State}.
	
terminate(Reason, State) ->
	?LOG_DEBUG("ems_server:terminate/2 - ~w",[Reason]),
	ok.
	
code_change(_OldVersion, State, _Extra) ->
	?LOG_DEBUG("ems_server:code_change/3",[]),
	{ok, State}.
	
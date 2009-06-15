-module(rtsp_supervisor).
-behaviour(supervisor).
-include("erlang_media_server.hrl").

%% ============================================================================
%% External Exports
%% ============================================================================
-export([start_link/0]).

%% ============================================================================
%% Supervisor Callbacks
%% ============================================================================
-export([init/1]).

-export([new_connection/1,new_connection/2]).

%% ----------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @spec start_link(Args) -> {ok, Pid} | Error
%% @end
%% ----------------------------------------------------------------------------
start_link() ->
	?LOG_INFO("rtsp_supervisor:start_link/1 Starting RTSP Supervisor", []),
	case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
		{ok, Pid} -> {ok, Pid}
	end.

new_connection(Socket) -> 
	?LOG_INFO("rtsp_supervisor:new_connection/1 - (~w)", [Socket]),
	ok.
new_connection(Arg1,Arg2) -> 
	?LOG_INFO("rtsp_supervisor:new_connection/2 - (~w,~w)", [Arg1,Arg2]),
	ok.

%% ----------------------------------------------------------------------------
%% @doc Called by the supervisor framework to find out the details of the 
%%      default child processes for this supervisor.
%% @spec init(Args) -> {ok, {SupervisorFlags, [ChildSpec]}}
%% @end
%% ----------------------------------------------------------------------------
init(_Args) -> 
	?LOG_DEBUG("rtsp_supervisor:init/1 - building child spec list", []),
	
	RestartStrategy        = one_for_one,
	MaxRestarts            = 1000,
	MaxTimeBetweenRestarts = 3600,

	SupervisorFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	ChildSpec = 
	[
		{rtsp_server,
			{rtsp_server, start_link, []},
			permanent,
			2000,
			supervisor,
			[rtsp_server]}
	],

	{ok, {SupervisorFlags, ChildSpec}}.
	
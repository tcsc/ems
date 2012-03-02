
-module(logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  ChildSpec = [
    {log_server, 
      {log_server, start_link, []}, 
      permanent, 
      2000, 
      worker, 
      [log_server]}
  ],
  {ok, { {one_for_one, 1000, 3600}, ChildSpec} }.


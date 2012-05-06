-module(ems).
-export([start/0, stop/0, shutdown/0]).
-include("common.hrl").

%% ============================================================================
%% Types & Type Exports
%% ============================================================================
-type user_rights() :: 'view' | 'broadcast'.
-type config() :: any().
-type user_info() :: #user_info{}.
-type mount_point() :: #mount_point{}.
-type transport_spec() :: rtsp:transport_spec().

-export_type([config/0, 
              user_rights/0,
              user_info/0,
              mount_point/0,
              transport_spec/0]).

%% ============================================================================
%% External Functions
%% ============================================================================

start() ->
	io:format("Starting EMS application~n", []),
  application:start(logger),
  log:add_sink(console_sink, []),
  log:set_level(trace),

  application:start(utils),
  application:start(listener),
	application:start(rtsp),
	application:start(erlang_media_server).
	
%% ----------------------------------------------------------------------------
%% @doc Called to shut down the media server application
%% @end
%% ----------------------------------------------------------------------------
shutdown() -> 
	log:info("Shuting down ems"),
	application:stop(erlang_media_server).

stop() ->
	application:stop(ems).

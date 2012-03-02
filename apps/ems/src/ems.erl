-module(ems).
-export([start/0, stop/0, shutdown/0]).
-include("logging.hrl").
-include("common.hrl").

%% ============================================================================
%% Types & Type Exports
%% ============================================================================
-type user_rights() :: 'view' | 'broadcast'.
-type config() :: any().
-type user_info() :: #user_info{}.
-type mount_point() :: #mount_point{}.
-exported_type([user_rights/0, user_info/0, mount_point/0]).

%% ============================================================================
%% External Functions
%% ============================================================================

start() ->
	?LOG_INFO("Starting EMS application", []),	
  application:start(log4erl),
  log4erl:add_console_appender("Console", {debug, "[%L]~t- %l%n"}),

  application:start(utils),
  application:start(listener),
	application:start(rtsp),
	application:start(erlang_media_server).
	
%% ----------------------------------------------------------------------------
%% @doc Called to shut down the media server application
%% @spec shutdown() -> ok
%% ----------------------------------------------------------------------------
shutdown() -> 
	?LOG_DEBUG("Shuting down ems", []),
	application:stop(erlang_media_server).

stop() ->
	application:stop(ems).

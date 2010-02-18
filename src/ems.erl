-module(ems).
-export([start/0, stop/0, debug/1]).
-include("erlang_media_server.hrl").

debug([FileName]) ->
  ?LOG_INFO("Starting debugger", []),
  debugger:start(FileName),
  start().
  
start() ->
	?LOG_INFO("Starting ems", []),
	application:start(erlang_media_server).
	
stop() ->
	application:stop(erlang_media_server).
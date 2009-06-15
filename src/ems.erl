-module(ems).
-export([start/0, stop/0]).
-include("erlang_media_server.hrl").

start()->
	?LOG_INFO("Starting ems", []),
	application:start(erlang_media_server).
	
stop()->
	application:stop(erlang_media_server).
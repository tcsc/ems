-module (listener_app).
-behaviour (application).
-export([start/2, stop/1]).

start(normal,_) -> 
	listener:start_link().

stop(_) -> 
	listener:stop().
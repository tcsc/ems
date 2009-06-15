-module(ems_logger).
-author('Trent Clarke <trent.clarke@gmail.com>').
-export([debug/2, info/2, warn/2, error/2]).

debug(Format, Args) ->
	write($D, io_lib:format(Format,Args)).

info(Format, Args) ->
	write($I, io_lib:format(Format,Args)).

warn(Format, Args) ->
	write($W, io_lib:format(Format,Args)).	
	
error(Format, Args) ->
	error($E, io_lib:format(Format,Args)).	
	
write(Prefix, Message) ->
	io:format("~w - [~c] - ~s~n", [self(), Prefix, Message]).
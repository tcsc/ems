-module(log).
-export([trace/1, trace/2, 
         debug/1, debug/2, 
         info/1,  info/2, 
         warn/1,  warn/2, 
         err/1,   err/2,  
         fatal/1, fatal/2]).

-include("logger.hrl").
-type log_level() :: trace | debug | info | warn | err | fatal.
-type log_msg() :: #log_msg{}.
-export_type([log_level/0, log_msg/0]).

trace(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_TRACE, Fmt, Args).
trace(Fmt) -> trace(Fmt, []).

debug(Fmt, Args) -> log_server:log_message(?LOG_LEVEL_DEBUG, Fmt, Args).
debug(Fmt) -> debug(Fmt,[]).

info(Fmt, Args) -> log_server:log_message(?LOG_LEVEL_INFO, Fmt, Args).
info(Fmt) -> info(Fmt, []).

warn(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_WARN, Fmt, Args).
warn(Fmt) -> warn(Fmt,[]).

err(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_ERROR, Fmt, Args).
err(Fmt) -> err(Fmt,[]).

fatal(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_FATAL, Fmt, Args).
fatal(Fmt) -> fatal(Fmt,[]).

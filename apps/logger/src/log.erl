-module(log).
-export([add_sink/2,
         set_level/1,
         get_level/0,
         trace/1, trace/2, 
         debug/1, debug/2, 
         info/1,  info/2, 
         warn/1,  warn/2, 
         err/1,   err/2,  
         fatal/1, fatal/2,
         level_to_int/1,
         int_to_level/1]).

-include("logger.hrl").
-type log_level() :: trace | debug | info | warn | err | fatal.
-type log_msg() :: #log_msg{}.
-export_type([log_level/0, log_msg/0]).

%% --------------------------------------------------------------------------
%% @doc Adds a sink to the default log. 
%% --------------------------------------------------------------------------
-spec add_sink(atom(), term()) -> any().
add_sink(Module, Args) ->
  log_server:add_sink(Module, Args).

set_level(Level) -> log_server:set_level(Level).

get_level() -> log_server:get_level().

%% --------------------------------------------------------------------------
%% @doc Formats and writes a trace message to the default log. 
%% --------------------------------------------------------------------------
-spec trace(string(), [term()]) -> any().
trace(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_TRACE, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes a trace message to the default log.
%% --------------------------------------------------------------------------
-spec trace(string()) -> any().
trace(Fmt) -> trace(Fmt, []).
 
%% --------------------------------------------------------------------------
%% @doc Formats and writes a debug-level message to the default log.
%% --------------------------------------------------------------------------
-spec debug(string(), [term()]) -> any().
debug(Fmt, Args) -> log_server:log_message(?LOG_LEVEL_DEBUG, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes a debug-level message to the default log.
%% --------------------------------------------------------------------------
-spec debug(string()) -> any().
debug(Fmt) -> debug(Fmt,[]).

%% --------------------------------------------------------------------------
%% @doc Writes a debug-level message to the default log.
%% --------------------------------------------------------------------------
-spec info(string(), [term()]) -> any().
info(Fmt, Args) -> log_server:log_message(?LOG_LEVEL_INFO, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes an info-level message to the default log.
%% --------------------------------------------------------------------------
-spec info(string()) -> any().
info(Fmt) -> info(Fmt, []).

%% --------------------------------------------------------------------------
%% @doc Formats and writes a warning-level message to the default log.
%% --------------------------------------------------------------------------
-spec warn(string(), [term()]) -> any().
warn(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_WARN, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes an warning-level message to the default log.
%% --------------------------------------------------------------------------
-spec warn(string()) -> any().
warn(Fmt) -> warn(Fmt,[]).

%% --------------------------------------------------------------------------
%% @doc Formats and writes an error-level message to the default log.
%% --------------------------------------------------------------------------
-spec err(string(), [term()]) -> any().
err(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_ERROR, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes an error-level message to the default log.
%% --------------------------------------------------------------------------
-spec err(string()) -> any().
err(Fmt) -> err(Fmt,[]).

%% --------------------------------------------------------------------------
%% @doc Formats & writes a fatal-level message to the default log.
%% --------------------------------------------------------------------------
-spec fatal(string(), [term()]) -> any().
fatal(Fmt,Args) -> log_server:log_message(?LOG_LEVEL_FATAL, Fmt, Args).

%% --------------------------------------------------------------------------
%% @doc Writes a fatal-level message to the default log.
%% --------------------------------------------------------------------------
-spec fatal(string()) -> any().
fatal(Fmt) -> fatal(Fmt,[]).

%% --------------------------------------------------------------------------
%%
%% --------------------------------------------------------------------------
-spec level_to_int(atom()) -> integer().
level_to_int(trace) -> ?LOG_LEVEL_TRACE;
level_to_int(debug) -> ?LOG_LEVEL_DEBUG;
level_to_int(info)  -> ?LOG_LEVEL_INFO;
level_to_int(warn)  -> ?LOG_LEVEL_WARN;
level_to_int(err)   -> ?LOG_LEVEL_ERROR;
level_to_int(fatal) -> ?LOG_LEVEL_FATAL.

%% --------------------------------------------------------------------------
%%
%% --------------------------------------------------------------------------
-spec int_to_level(integer()) -> atom().
int_to_level(?LOG_LEVEL_TRACE) -> trace;
int_to_level(?LOG_LEVEL_DEBUG) -> debug;
int_to_level(?LOG_LEVEL_INFO)  -> info;
int_to_level(?LOG_LEVEL_WARN)  -> warn;
int_to_level(?LOG_LEVEL_ERROR) -> err;
int_to_level(?LOG_LEVEL_FATAL) -> fatal.

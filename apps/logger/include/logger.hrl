-define(LOG_LEVEL_TRACE, 6).
-define(LOG_LEVEL_DEBUG, 5).
-define(LOG_LEVEL_INFO,  4).
-define(LOG_LEVEL_WARN,  3).
-define(LOG_LEVEL_ERROR, 2).
-define(LOG_LEVEL_FATAL, 1).
-define(LOG_LEVEL_NONE,  0).

-record(log_msg, {src :: pid(), level :: integer(), msg :: string() }).

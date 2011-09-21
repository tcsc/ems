-define(LOG_MODULE, ems_logger).

-define(LOG_DEBUG(Format, Args), ?LOG_MODULE:debug(Format,Args)).
-define(LOG_INFO(Format, Args), ?LOG_MODULE:info(Format,Args)).
-define(LOG_WARN(Format, Args), ?LOG_MODULE:warn(Format,Args)).
-define(LOG_ERROR(Format, Args), ?LOG_MODULE:error(Format,Args)).

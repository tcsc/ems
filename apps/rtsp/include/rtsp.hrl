
-define(RTSP_VERSION, {1,0}).

%% ----------------------------------------------------------------------------
%% @doc The common parts of an rtsp message
%% ----------------------------------------------------------------------------
-record(rtsp_message_header, {
          sequence             :: integer(), 
          content_length = 0   :: integer(), 
          content_type         :: string(), 
          headers = dict:new() :: dict()}).

%% ----------------------------------------------------------------------------
%% @doc The basic structure of an RTSP request
%% @end
%% ----------------------------------------------------------------------------
-record(rtsp_request, {
         method          :: string(), 
         uri             :: string(), 
         version = {1,0} :: {integer(), integer()}}).

-record(rtsp_response, {
         status :: integer(), 
         version = {1,0} :: {integer(), integer()}}).

-record(rtsp_user_info, {
  id :: integer(), 
  username :: string(),
  password :: string() }).
  
%% ----------------------------------------------------------------------------
%% @doc The encapsulation of a parsed RTSP message
%% @end
%% ----------------------------------------------------------------------------
-record(rtsp_message, { message :: #rtsp_request{} | #rtsp_response{}, 
                        headers :: #rtsp_message_header{},
                        body    :: binary() }).  

%% ============================================================================
%% RTSP constants
%% ============================================================================
-define(RTSP_METHOD_OPTIONS,  "OPTIONS").
-define(RTSP_METHOD_ANNOUNCE, "ANOUNCE").
-define(RTSP_METHOD_DESCRIBE, "DESCRIBE").
-define(RTSP_METHOD_SETUP,    "SETUP").
-define(RTSP_METHOD_PLAY,     "PLAY").
-define(RTSP_METHOD_PAUSE,    "PAUSE").
-define(RTSP_METHOD_TEARDOWN, "TEARDOWN").
-define(RTSP_METHOD_RECORD,   "RECORD").

-define(RTSP_HEADER_SEQUENCE,       "CSeq").
-define(RTSP_HEADER_AUTHENTICATE,   "WWW-Authenticate").
-define(RTSP_HEADER_AUTHORISATION,  "Authorization").
-define(RTSP_HEADER_SERVER,         "Server").
-define(RTSP_HEADER_CONTENT_LENGTH, "Content-Length").
-define(RTSP_HEADER_CONTENT_TYPE,   "Content-Type").
-define(RTSP_HEADER_TRANSPORT,      "Transport").
-define(RTSP_HEADER_SESSION,        "Session").
-define(RTSP_HEADER_RANGE,          "Range").
-define(RTSP_HEADER_RTP_INFO,       "RTP-Info").

-define(RTSP_STATUS_OK, {200, "OK"}).
-define(RTSP_STATUS_SERVER_ERROR, {500, "Internal Server Error"}).
-define(RTSP_STATUS_NOT_IMPLEMENTED, {501, "Not Implemented"}).




-define(RTSP_VERSION, {1,0}).

%% ----------------------------------------------------------------------------
%% @doc The encapsulation of a parsed RTSP message
%% @end
%% ----------------------------------------------------------------------------
-record(rtsp_message, {type, specific, headers, body}).

%% ----------------------------------------------------------------------------
%% @doc The common parts of an rtsp message
%% ----------------------------------------------------------------------------
-record(rtsp_message_header, {
  sequence, 
  content_length = 0, 
  content_type = "", 
  headers = dict:new()}).

%% ----------------------------------------------------------------------------
%% @doc The basic structure of an RTSP request
%% @end
%% ----------------------------------------------------------------------------
-record(rtsp_request, {
  method, 
  uri, 
  version = {1,0}}).

-record(rtsp_response, {
  status, 
  version = {1,0}}).

%% ============================================================================
%% RTSP constants
%% ============================================================================
-define(RTSP_METHOD_OPTIONS,  "OPTIONS").
-define(RTSP_METHOD_ANNOUNCE, "ANOUNCE").
-define(RTSP_METHOD_DESCRIBE, "DESCRIBE").
-define(RTSP_METHOD_SETUP,    "SETUP").
-define(RTSP_METHOD_PLAY,     "PLAY").
-define(RTSP_METHOD_TEARDOWN, "TEARDOWN").

-define(RTSP_SEQUENCE,       "cseq").
-define(RTSP_CONTENT_LENGTH, "content-length").
-define(RTSP_CONTENT_TYPE,   "content-type").
-define(RTSP_TRANSPORT,      "transport").
-define(RTSP_SESSION,        "session").

-define(RTSP_STATUS_OK, {200, "OK"}).
-define(RTSP_STATUS_SERVER_ERROR, {500, "Internal Server Error"}).
-define(RTSP_STATUS_NOT_IMPLEMENTED, {501, "Not Implemented"}).

-define(RTSP_HEADER_SERVER, "Server").
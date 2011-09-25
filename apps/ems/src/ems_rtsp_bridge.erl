-module (ems_rtsp_bridge).
-export([handle_request/2]).
-include("rtsp.hrl").

-spec handle_request(rtsp_connection:conn(), rtsp:message()) -> any().
handle_request(Conn, Request = #rtsp_message{headers = Headers, body = Body}) ->
	{Method, _Uri, Sequence, _, _} = rtsp:get_request_info(Request),
  handle_request(Conn, Method, Sequence, Request, Headers, Body).

%% ----------------------------------------------------------------------------
%% @spec handle_request(Method, Request,Headers,Body,State) -> Result
%%       Method = options | accounce | setup | play | teardown
%% @end
%% ----------------------------------------------------------------------------  
handle_request(Conn, "OPTIONS", Sequence, _, _, _) ->
  PublicOptions = [?RTSP_METHOD_ANNOUNCE,
                   ?RTSP_METHOD_DESCRIBE,
                   ?RTSP_METHOD_SETUP,
                   ?RTSP_METHOD_PLAY,
                   ?RTSP_METHOD_PAUSE,
                   ?RTSP_METHOD_TEARDOWN,
                   ?RTSP_METHOD_RECORD],
  Headers = [{"Public", string:join(PublicOptions, ", ")}],
  rtsp_connection:send_response(Conn, Sequence, ok, Headers, << >>);

%handle_request(Conn, describe, Request, Headers, Body) -> 
%	Uri = Request#rtsp_request.uri,
%	{_,_,_,Path} = url:parse(Uri),
%	Config = get_config_handle(Conn),
%	MountPoint = case ems_config:get_mount_point(Config, Path) of 

handle_request(Conn, _Method, Sequence, _Request, _Headers, _Body) ->
  rtsp_connection:send_response(Conn, Sequence, not_implemented, [], << >>).


-module (ems_rtsp_bridge).
-export([handle_request/3]).
-include("rtsp.hrl").
-include("config.hrl").

-spec handle_request(ems_config:handle(), rtsp:conn(), rtsp:message()) -> any().
handle_request(Config, Conn, Msg) ->
	{Method, Uri, Sequence, _, _} = rtsp:get_request_info(Msg),
  handle_request(Config, Conn, Sequence, Method, Uri, Msg).

%% ----------------------------------------------------------------------------
%% @spec handle_request(Method, Request,Headers,Body,State) -> Result
%%       Method = options | accounce | setup | play | teardown
%% @end
%% ----------------------------------------------------------------------------  
handle_request(_, Conn, Seq, "OPTIONS", _, _) ->
  PublicOptions = [?RTSP_METHOD_ANNOUNCE,
                   ?RTSP_METHOD_DESCRIBE,
                   ?RTSP_METHOD_SETUP,
                   ?RTSP_METHOD_PLAY,
                   ?RTSP_METHOD_PAUSE,
                   ?RTSP_METHOD_TEARDOWN,
                   ?RTSP_METHOD_RECORD],
  Headers = [{"Public", string:join(PublicOptions, ", ")}],
  rtsp:send_response(Conn, Seq, ok, Headers, << >>);

handle_request(Config, Conn, Seq, "ANNOUNCE", Uri, Msg = #rtsp_message{message = Rq}) ->
  LookupUser = fun(UserName) -> get_user_info(Config, UserName) end,
  
  Handler = 
    fun(UserInfo) ->
      rtsp:send_response(Conn, Seq, internal_server_error, [], << >>)
    end,
    
  rtsp:with_authenticated_user_do(Conn, Msg, LookupUser, Handler);
  
%	Uri = Request#rtsp_request.uri,
%	{_,_,_,Path} = url:parse(Uri),
%	Config = get_config_handle(Conn),
%	MountPoint = case ems_config:get_mount_point(Config, Path) of 

handle_request(_, Conn, Seq, _Method, _, _) ->
  rtsp:send_response(Conn, Seq, not_implemented, [], << >>).

%% -----------------------------------------------------------------------------
%% Utils
%% -----------------------------------------------------------------------------

get_user_info(Config, UserName) ->
  case ems_config:get_user_info(Config, UserName) of
    false -> false;
    User -> {ok, translate_user(User)}
  end.
  
translate_user(#user_info{id = Id, login = LoginName, password = Pwd}) ->
  #rtsp_user_info{id = Id, username = LoginName, password = Pwd}.
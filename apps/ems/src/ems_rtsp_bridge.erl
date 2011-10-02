-module (ems_rtsp_bridge).

%% ============================================================================
%% @author Trent Clarke <trent.clarke@gmail.com>
%% @doc Acts as a bridge between the Internet-facing RTSP service and the 
%%      internal media server. The idea is that the code in this module will 
%%      translate the RTSP requests into something that the ems will understand
%%      and be able to act upon, and translate the ems' responsed back into 
%%      valid RTSP responsef for transmission backto the client.
%% @end
%% ============================================================================

-export([handle_request/3]).
-include("rtsp.hrl").
-include("config.hrl").

%% ----------------------------------------------------------------------------
%% @doc Handles inbound requests from an RTSP server. This should be called
%%      from a lambda passed to the RTSP server during setup. We're guaranteed
%%      be the RTSP server that this function will be invoked on a process 
%%      other than the one managing the connection or the server, so all
%%      RTSP connection or server functions can be called without fear of a 
%%      deadlock.
%% @end
%% ----------------------------------------------------------------------------
-spec handle_request(ems_config:handle(), rtsp:conn(), rtsp:message()) -> any().
handle_request(Config, Conn, Msg) ->
	{Method, Uri, Sequence, _, _} = rtsp:get_request_info(Msg),
  handle_request(Config, Conn, Sequence, Method, Uri, Msg).

%% ----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------
-spec handle_request(Config :: ems_config:handle(), 
                     Conn   :: rtsp:conn(), 
                     Seq    :: integer(), 
                     Method :: string(),
                     Uri    :: string(),
                     Msg    :: rtsp::message()) -> any().

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

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
-include("sdp.hrl").
-include("common.hrl").

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

%% Just unpacks the message a bit and passes it down to the internal 
%% handle_request/6 function
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
                     Msg    :: rtsp:message()) -> any().

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

handle_request(Config, Conn, Seq, "ANNOUNCE", Uri, Msg) ->
  LookupUser = fun(UserName) -> get_user_info(Config, UserName) end,  
  Handler = 
    fun(Uid) ->
      UserInfo = translate_user(Uid),
      Desc = parse_sdp(Msg),
      {_, _, _, Path} = url:parse(Uri),
      Response = 
        case ems_server:create_session(Config, Path, UserInfo, Desc, []) of
          ok -> ok;
          already_exists -> method_not_valid;
          unathorised -> throw(unathorized);
          not_found -> not_found
        end,
      rtsp:send_response(Conn, Seq, Response, [], << >>)
    end,
    
  rtsp:with_authenticated_user_do(Conn, Msg, LookupUser, Handler);
  
%	Uri = Request#rtsp_request.uri,
%	{_,_,_,Path} = url:parse(Uri),
%	Config = get_config_handle(Conn),
%	MountPoint = case ems_config:get_mount_point(Config, Path) of 

handle_request(_, Conn, Seq, _Method, _, _) ->
  rtsp:send_response(Conn, Seq, not_implemented, [], << >>).

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
-spec parse_sdp(rtsp:message()) -> sdp:session_description() | no_return().
parse_sdp(Msg = #rtsp_message{body = Body}) ->
  case rtsp:message_content_type(Msg) of
    "application/sdp" -> 
      case sdp:parse(Body) of 
        {ok, Desc} -> Desc;
        _ -> throw(bad_request)
      end;
    _ -> throw(bad_request)
  end.

%% ============================================================================
%% Utils
%% ============================================================================

get_user_info(Config, UserName) ->
  case ems_config:get_user_info(Config, UserName) of
    {ok, User} -> {ok, translate_user(User)};
    _ -> false
  end.

%% -----------------------------------------------------------------------------
%% @doc Translates back & forth between the EMS user records and the format the 
%%      RTSP service uses.
%% @end
%% -----------------------------------------------------------------------------  
-spec translate_user(User :: #user_info{} | #rtsp_user_info{}) -> 
  #user_info{} | #rtsp_user_info{}.

translate_user(User = #user_info{id = Id, login = LoginName, password = Pwd}) 
    when is_record(User, user_info) ->
  #rtsp_user_info{id = Id, username = LoginName, password = Pwd};
  
translate_user(User = #rtsp_user_info{id = Id, username = LoginName, password = Pwd}) 
    when is_record(User, rtsp_user_info) ->
  #user_info{id = Id, login = LoginName, password = Pwd}.
  

-module(local_config).

-include("apps/ems/include/common.hrl").

%% exports that EMS looks for
-export([init/0, get_config/1, get_mount_point/2, get_user/2, get_rights/3]).

init() -> 
	{ok, cookie}.

get_config(_) -> [
	{rtsp, [ {ports, [4321,4320,4319]} ]}
	].
	
-spec get_mount_point(any(), string()) -> {'ok', #mount_point{}} | 'false'.
get_mount_point(_Cookie, Name) -> 
	case Name of
		"/default" -> {ok, {mount_point, 1, "default", "default", "Main Entry Point"}};
		"/test.sdp" -> {ok, {mount_point, 1, "test", "testing", "Test Mount Point"}};
		_ -> false
	end.
	
-spec get_user(any(), string()) -> {'ok', #user_info{}} | 'false'.
get_user(_Cookie, Name) ->
  case Name of
    "trent" -> {ok, #user_info{ id  = 1, login = "trent", password = "pwd", name = "Trent Clarke"}};
    _ -> false
  end.
  
-spec get_rights(Cookie :: any(), UserId :: integer(), MountPointId :: integer()) -> [atom()].
get_rights(Cookie, UserId, MountPointId) -> 
	case MountPointId of
	  1 -> case UserId of 
  	        0 -> [view];
  	        1 -> [broadcast, view];
  	        _ -> []
	       end;
	  _ -> []
	end.

%%barf() -> ok.
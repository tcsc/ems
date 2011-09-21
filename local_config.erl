-module(local_config).

-include("include/config.hrl").

%% exports that EMS looks for
-export([init/0, get_config/1, get_mount_point/2]).

init() -> 
	{ok, cookie}.

get_config(_) -> [
	{rtsp, [ {ports, [4321,4320,4319]} ]}
	].
	
get_mount_point(_Cookie, Name) -> 
	case Name of
		"default" -> {mount_point, 1, "default", "default", "Main Entry Point"};
		"test" -> {mount_point, 1, "test", "testing", "Test Mount Point"};
		_ -> false
	end.
		
	

%%barf() -> ok.
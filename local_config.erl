-module(local_config).

%% exports that EMS looks for
-export([init/0, get_config/1]).

init() -> 
	{ok, cookie}.

get_config(_) -> [
	{rtsp, [ {ports, [4321,4320,4319]} ]}
	].

%%barf() -> ok.
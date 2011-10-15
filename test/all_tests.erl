-module (all_tests).
-export([start/0]).

start() ->
  eunit:test([rtsp_tests, authentication_tests, sdp_tests], [verbose]).
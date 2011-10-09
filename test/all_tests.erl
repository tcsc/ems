-module (all_tests).
-export([start/0]).

start() ->
  eunit:test([authentication_tests, sdp_tests], [verbose]).
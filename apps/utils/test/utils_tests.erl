-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

hex_string_test() ->
  Cs = "0123456789abcdef",
  T = utils:hex_string(lists:seq(0,255)),
  ?assertEqual(T, lists:flatten([[X,Y] || X <- Cs, Y <- Cs])).

-module(stringutils_tests).
-include_lib("eunit/include/eunit.hrl").

find_in_binary_simple_test() ->
  ?assertEqual(3, stringutils:find_in_binary(<<"abcdefghi">>, <<"def">>)).

find_in_binary_identity_test() ->
  ?assertEqual(0, stringutils:find_in_binary(<<"abcdefghi">>, <<"abcdefghi">>)).

find_in_binary_empty_string_test() ->
  ?assertEqual(false, stringutils:find_in_binary(<<>>,<<"abcdef">>)).

find_in_binary_no_match_test() ->
  ?assertEqual(false, stringutils:find_in_binary(<<"abcdefghi">>, <<"abe">>)).

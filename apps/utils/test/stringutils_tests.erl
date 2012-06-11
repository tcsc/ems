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

to_int_def_test() ->
  ?assertEqual(42,  stringutils:to_int_def("42",-1)),
  ?assertEqual(-1,  stringutils:to_int_def("-1",0)),
  ?assertEqual(42,  stringutils:to_int_def("narf", 42)),
  ?assertEqual(-42, stringutils:to_int_def("zort", -42)).


split_on_first_test() ->
  ?assertEqual({"abc","def"},    stringutils:split_on_first($,, "abc,def")),
  ?assertEqual({"","abcdef"},    stringutils:split_on_first($,, ",abcdef")),
  ?assertEqual({"abcdef",""},    stringutils:split_on_first($,, "abcdef,")),
  ?assertEqual({"abc","d,e,f,"}, stringutils:split_on_first($,, "abc,d,e,f,")),
  ?assertEqual({"abcdef",""},    stringutils:split_on_first($,, "abcdef")).

split_on_last_test() ->
  ?assertEqual({"abc","def"}, stringutils:split_on_last($,, "abc,def")),
  ?assertEqual({"","abcdef"},  stringutils:split_on_last($,, ",abcdef")),
  ?assertEqual({"abcdef",""},  stringutils:split_on_last($,, "abcdef,")),
  ?assertEqual({"ab,c","def"}, stringutils:split_on_last($,, "ab,c,def")),
  ?assertEqual({"abcdef",""},    stringutils:split_on_first($,, "abcdef")).

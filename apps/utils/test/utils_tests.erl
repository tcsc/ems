-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

dictionary_equivalence_identity_test() ->
  D = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  ?assert(utils:dicts_are_equivalent(D,D)).

dictionary_equivalence_equality_test() ->
  D1 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  D2 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  ?assert(utils:dicts_are_equivalent(D1,D2)).

dictionary_equivalence_nearly_similar_test() ->
  D1 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  D2 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Incorrect Value"}, {d, "Delta"}]),
  ?assertNot(utils:dicts_are_equivalent(D1,D2)).

dictionary_equivalence_different_test() ->
  D1 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  D2 = dict:from_list([{e, "Epsilon"}, {f, "Zeta"}, {h, "Eta"}, {i, "Theta"}]),
  ?assertNot(utils:dicts_are_equivalent(D1,D2)).

dictionary_equivalence_different_size_test() ->
  D1 = dict:from_list([{a, "Alpha"}, {b, "Beta"}, {c, "Gamma"}, {d, "Delta"}]),
  D2 = dict:from_list([{e, "Epsilon"}]),
  ?assertNot(utils:dicts_are_equivalent(D1,D2)).

hex_string_test() ->
  Cs = "0123456789abcdef",
  T = utils:hex_string(lists:seq(0,255)),
  ?assertEqual(T, lists:flatten([[X,Y] || X <- Cs, Y <- Cs])).

fold_seq_test() ->
  L = lists:reverse(utils:fold_seq(fun(N,Acc) -> [N|Acc] end, [], 10, 20)),
  ?assertEqual(lists:seq(10,20), L).

array_foreach_test() -> 
  Me = self(),
  A = array:from_list(lists:seq(10,20)),
  F = fun(X) -> Me ! {element, X} end,
  proc_lib:spawn(
    fun() -> 
      utils:array_foreach(F, A),
      Me ! done
    end),
  L = lists:reverse(collect([])),
  ?assertEqual(lists:seq(10,20),L).

array_foreach_sparse_test() ->
  Me = self(),
  A = lists:foldl(fun(N,Array) -> array:set(N, N, Array) end,
                  array:new(31),
                  lists:seq(1,10) ++ lists:seq(20,30)),
  F = fun(X) -> Me ! {element, X} end,
  proc_lib:spawn(
    fun() -> 
      utils:array_foreach(F, A),
      Me ! done
    end),
  L = lists:reverse(collect([])),
  Expected = [undefined] ++ lists:seq(1,10) ++ lists:duplicate(9, undefined) ++ lists:seq(20,30),
  ?assertEqual(Expected, L).

array_sparse_foreach_test() ->
  Me = self(),
  A = array:from_list(lists:seq(10,20)),
  F = fun(X) -> Me ! {element, X} end,
  proc_lib:spawn(
    fun() -> 
      utils:array_sparse_foreach(F, A),
      Me ! done
    end),
  L = lists:reverse(collect([])),
  ?assertEqual(lists:seq(10,20),L).


array_sparse_foreach_sparse_test() ->
  Me = self(),
  A = lists:foldl(fun(N,Array) -> array:set(N, N, Array) end,
                  array:new(31),
                  lists:seq(1,10) ++ lists:seq(20,30)),
  F = fun(X) -> Me ! {element, X} end,
  proc_lib:spawn(
    fun() -> 
      utils:array_sparse_foreach(F, A),
      Me ! done
    end),
  L = lists:reverse(collect([])),
  Expected = lists:seq(1,10) ++ lists:seq(20,30),
  ?assertEqual(Expected, L).



collect(L) ->
  receive 
    {element, N} -> collect([N|L]);
    done -> L
  end.

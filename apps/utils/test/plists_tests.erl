-module(plists_tests).
-include_lib("eunit/include/eunit.hrl").

check_pmap(I, [{N,Pid}|T]) ->
  ?assertEqual(I, N),
  ?assertNot( Pid =:= self() ),
  check_pmap( I + 1, T );
check_pmap(_, []) -> ok.

parallel_map_test() ->
  F = fun(N) -> {N, self()} end,
  Ls = plists:parallel_map(F, lists:seq(1,100)),
  check_pmap(1, Ls).
  

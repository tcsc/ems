-module(log_tests).
-include_lib("eunit/include/eunit.hrl").
-include("logger.hrl").

with_logger_do(Fun) -> 
  {ok, _} = log_server:start_link(),
  log:add_sink(forwarding_sink, self()),
  Fun(),
  log_server:stop().

setup_and_teardown_test() ->
  {ok, _} = log_server:start_link(),
  ?assert(whereis(log_server) /= undefined),
  log_server:stop(),
  ?assert(whereis(log_server) =:= undefined).

formatting_test() -> 
  F = fun() -> 
        log:info("Hello world: ~w ~w ~w", [1,2,3]),
        receive
          {echo_message, Src, Level, Msg} ->
            ?assertEqual(self(), Src),
            ?assertEqual(?LOG_LEVEL_INFO, Level),
            ?assertEqual("Hello world: 1 2 3", Msg)
        after 1000 ->
          ?assert(false)
        end
      end,
  with_logger_do(F).

set_level_test() ->
  F = fun() ->
        ?assertEqual(info, log:get_level()),
        log:set_level(trace),
        ?assertEqual(trace, log:get_level()),
        log:set_level(debug),
        ?assertEqual(debug, log:get_level()),
        log:set_level(info),
        ?assertEqual(info, log:get_level()),
        log:set_level(warn),
        ?assertEqual(warn, log:get_level()),
        log:set_level(err),
        ?assertEqual(err, log:get_level()),
        log:set_level(fatal),
        ?assertEqual(fatal, log:get_level())
      end,
  with_logger_do(F).

order_test() ->
  F = fun() ->
        L = lists:seq(1,100),
        lists:foreach(fun(N) -> log:info("Item #~w", [N]) end, L),
        lists:foreach(
          fun(N) -> 
            Exp = lists:flatten(io_lib:format("Item #~w", [N])),
            receive
              {echo_message, Src, Level, Msg} ->
                ?assertEqual(self(), Src),
                ?assertEqual(?LOG_LEVEL_INFO, Level),
                ?assertEqual(Exp, Msg)
            after 1000 ->
                ?assertEqual(timeout, true)
            end
          end,
          L)
      end,
  with_logger_do(F).

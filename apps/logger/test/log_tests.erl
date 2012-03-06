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

  

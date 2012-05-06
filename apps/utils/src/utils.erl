-module(utils).
-export([dicts_are_equivalent/2, fold_seq/4, hex_string/1]).


dicts_are_equivalent(A,B) ->
  SizeOfA = size(A),
  case size(B) of
    SizeOfB when SizeOfB =:= SizeOfA ->
      F = fun(Key, ValA, Result) -> 
            case Result of 
              true -> 
                {ok, ValB} = dict:find(Key, B),
                ValA =:= ValB;

              false -> false
            end
      end,
      dict:fold(F, true, A);
    
    _ -> false
  end.

%% ----------------------------------------------------------------------------
%% @doc Emulates the befaviour of folding over a sequence without generating 
%%      the full sequence. Effectively implements a for-loop in Erlang. Note 
%%      that the sequence includes both the start and end values.
%% @end
%% ----------------------------------------------------------------------------

-type acc() :: term().
-spec fold_seq( F :: fun((N :: integer(), Acc :: acc()) -> NewAcc :: acc()),
                Acc :: acc(),
                Start :: integer(),
                End   :: integer() ) -> acc().

fold_seq(_, Acc, N, End) when N == (End+1) ->
  Acc;

fold_seq(F, Acc, N, End) ->
  AccP = F(N, Acc),
  fold_seq(F, AccP, N+1, End).

hex_string(L) -> [hex_char(C) || C <- lists:flatten([hex_byte(N) || N <-L])].
hex_byte(B) -> [B bsr 4, B band 15].
hex_char(N) when N < 10 -> $0 + N;
hex_char(N) when N > 9 -> $a + (N - 10).

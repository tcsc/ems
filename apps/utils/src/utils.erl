-module(utils).
-export([array_foreach/2,
         array_sparse_foreach/2,
         dicts_are_equivalent/2, 
         fold_seq/4, 
         hex_string/1]).


dicts_are_equivalent(A,B) ->
  SizeOfA = dict:size(A),
  case dict:size(B) of
    SizeOfB when SizeOfB =:= SizeOfA ->
      F = fun(Key, ValA, Result) -> 
            case Result of 
              true ->
                case dict:find(Key, B) of
                  {ok, ValB} -> ValA =:= ValB;
                  _ -> false
                end;

              false -> false
            end
      end,
      dict:fold(F, true, A);
    
    _ -> false
  end.

%% ----------------------------------------------------------------------------
%% @doc Emulates the behaviour of folding over a sequence without generating 
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

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
hex_string(L) -> [hex_char(C) || C <- lists:flatten([hex_byte(N) || N <-L])].
hex_byte(B) -> [B bsr 4, B band 15].
hex_char(N) when N < 10 -> $0 + N;
hex_char(N) when N > 9 -> $a + (N - 10).

%% ----------------------------------------------------------------------------
%% @doc Invoked the supplied function on every element in an array, including
%%      all default-valued items.
%% @end
%% ----------------------------------------------------------------------------
-type unary_func() :: fun((term()) -> term()).
-spec array_foreach(F :: unary_func(), A :: array()) -> any().
array_foreach(F, A) ->
  array_foreach(F, A, 0, array:size(A)).

array_foreach(_, _, Idx, Size) when Idx =:= Size -> ok;
array_foreach(F, A, Idx, Size) ->
  F(array:get(Idx, A)),
  array_foreach(F, A, Idx+1, Size).

%% ----------------------------------------------------------------------------
%% @doc Invoked the supplied function on every element in an array, excluding
%%      all default-valued items.
%% @end
%% ----------------------------------------------------------------------------
-spec array_sparse_foreach(F :: unary_func(), A :: array()) -> any().
array_sparse_foreach(F, A) ->
  array_sparse_foreach(F, A, 0, array:default(A), array:size(A)).

array_sparse_foreach(_, _, Idx, _, Size) when Idx =:= Size -> ok;
array_sparse_foreach(F, A, Idx, DefaultVal, Size) ->
  case array:get(Idx, A) of 
    DefaultVal -> pass;
    Val -> F(Val)
  end,
  array_sparse_foreach(F, A, Idx+1, DefaultVal, Size).

-module(utils).
-export([dicts_are_equivalent/2]).


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



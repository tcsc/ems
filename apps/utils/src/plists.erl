%% A parallel list operations library

-module(plists).
-export([parallel_map/2]).

%% @doc Parallelizes a map standard map operation. Will spawn a separate 
%%      process for each element in the list, run the mapping function on
%%      it and then gather the results
%% @end
-spec parallel_map(Fun :: fun((term()) -> term()), List :: [term()]) -> [term()].
parallel_map(Fun, List) ->
  S = self(),
  Thunk = fun(Item) -> 
            spawn(fun() -> pmap_fun(S, Fun, Item) end) 
          end,
  Pids = lists:map(Thunk, List),
  gather(Pids).

%% @doc Fun the supplied function and return the result to the parent 
%%      process
%% @end
pmap_fun(Parent, Fun, Item) -> Parent ! {pmap, self(), catch(Fun(Item))}.

%% @doc Run through the list of PIDs that we spawned and get the response from 
%%      each one. A simple list walk makes perfect sense that we will be 
%%      blocked waiting for the function that takes the longest at some 
%%      point anyway.
%% @end
-spec gather([pid()]) -> [any()].
gather([H|T]) -> 
  receive
    {pmap, H, RVal} -> [RVal | gather(T)]
  end;
gather([]) -> [].

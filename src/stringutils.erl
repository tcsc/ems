%%% ============================================================================
%%% A string & bitstring utility library  
%%% ============================================================================
-module (stringutils).
-export([
  extract_token/3, 
  to_int_def/2,
  int_to_string/1,
  index_of/2, 
  split_on_first/2, 
  split_on_last/2,
  unquote/1]).

%% ----------------------------------------------------------------------------
%% @doc
%% @spec extract_token(Bin, Offset, Separator, Results) -> Result
%%       Bin = binary()
%%       Offset = int
%%       Separator = 
%%       Results = [Temp]
%% @end
%% ----------------------------------------------------------------------------
extract_token(Bin, Offset, Separator) ->
	extract_token(Bin, Offset, Separator, []).
	
extract_token(Bin, Offset, Separator, Result) ->
	case Bin of
		<<_:Offset/binary, Separator:8, _/binary>> ->
			% Separator found - extract and return
			{lists:reverse(Result), Offset};
		
		<<_:Offset/binary, N:8, _/binary>> ->
			% not found yet - go arount again
			extract_token(Bin, Offset+1, Separator, [N|Result]);
			
		_ ->
			% scanned the entire binary, didn't find it. Time to bail
			{lists:reverse(Result), Offset} 
	end.
	
%% ----------------------------------------------------------------------------
%% @doc Parses a string into an integer, returning the default if the parsing
%%      fails.
%% @spec to_int_def(String, Default) -> Result
%%       String = string()
%%       Default = term()
%%       Result = integer() | Default
%% ----------------------------------------------------------------------------
to_int_def(String, Default) ->
  try
    list_to_integer(String)
  catch
    error:badarg -> Default
  end.

%% ----------------------------------------------------------------------------
%% @doc Splits a string on the first occurrence of a character
%% @spec split_on_first(Char,String) -> {Prefix,Suffix}
%%      Char = 
%%      String =  
%%      Prefix =
%%      Suffix = 
%% @end
%% ----------------------------------------------------------------------------
split_on_first(Char, String) ->
  case index_of(Char, String) of
    0 -> {String, []};
    Index -> 
      A = string:sub_string(String,1,Index-1),
      B = string:sub_string(String,Index+1),
      {A,B}
  end.

%% ----------------------------------------------------------------------------
%% @doc Splits a string on the last occurrence of a character
%% @spec split_on_last(Char,String) -> {Prefix,Suffix}
%% @end
%% ----------------------------------------------------------------------------
split_on_last(Char, String) ->
  {A,B} = split_on_first(Char, lists:reverse(String)),
  {lists:reverse(B), lists:reverse(A)}.

%% ----------------------------------------------------------------------------
%% @doc Returns the index of the first occurrence of Char in String.
%% @spec index_of(Char,String) -> 0 | Index
%%       Index = integer() when > 0
%% @end
%% ----------------------------------------------------------------------------
index_of(Char, String) ->
  index_of(1, Char, String).
  
index_of(Index, Char, [H|_]) when Char =:= H ->
  Index;

index_of(Index, Char, [H|T]) when Char =/= H ->
  index_of(Index+1, Char, T);
  
index_of(_, _, []) -> 0.

%% ----------------------------------------------------------------------------
%% @doc
%% @end
%% ----------------------------------------------------------------------------  
unquote(Text) ->
  Parts = string:tokens(Text, [$"]),
  string:join(Parts, []).

%% ----------------------------------------------------------------------------
%% @spec int_to_string(Value) -> Result
%%         Value = int()
%%         Result = string()
%% @end
%% ----------------------------------------------------------------------------  
int_to_string(Value) ->
  lists:flatten(io_lib:format("~p", [Value])).
  

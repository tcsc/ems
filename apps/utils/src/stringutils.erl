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
%% @doc Extracts a series of bytes from a binary, staring at a given offset up
%%      to (but not including) a given delimiter. Returns a list contianing the 
%%      bytes before the delimiter, and the offset of the delimiter itself.
%% @end
%% ----------------------------------------------------------------------------
-spec extract_token(binary(), integer(), byte()) -> 
        {Token :: [byte()], NewOffset :: integer()}.
        
extract_token(Bin, Offset, Separator) ->
	extract_token(Bin, Offset, Separator, []).
	
extract_token(Bin, Offset, Separator, Result) ->
	case Bin of
		<<_:Offset/binary, Separator:8, _/binary>> ->
			% Separator found - extract and return
			{lists:reverse(Result), Offset};
		
		<<_:Offset/binary, N:8, _/binary>> ->
			% not found yet - go around again
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
%% @end
%% ----------------------------------------------------------------------------
to_int_def(String, Default) ->
  try
    list_to_integer(String)
  catch
    error:badarg -> Default
  end.

%% ----------------------------------------------------------------------------
%% @doc Splits a string on the first occurrence of a character
%% @end
%% ----------------------------------------------------------------------------
-spec split_on_first(char(),string()) -> {Prefix :: string(), Suffix :: string()}.
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
%% @end
%% ----------------------------------------------------------------------------
-spec split_on_last(char(),string()) -> {Prefix :: string(), Suffix :: string()}.
split_on_last(Char, String) ->
  {A,B} = split_on_first(Char, lists:reverse(String)),
  {lists:reverse(B), lists:reverse(A)}.

%% ----------------------------------------------------------------------------
%% @doc Returns the index of the first occurrence of Char in String. Returns 0 
%%      if the specified character is not found. 
%% @end
%% ----------------------------------------------------------------------------
-spec index_of(char(), string()) -> integer().
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
%% @doc Formats an integer as a string
%% @end
%% ----------------------------------------------------------------------------  
-spec int_to_string(integer()) -> string().
int_to_string(Value) ->
  lists:flatten(io_lib:format("~p", [Value])).

-module(utf).

-export([utf8_to_string/1, string_to_utf8/1]).

%% ============================================================================
%% Exported functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Converts a utf-8 encoded binary buffer into a text string.
%% @end
%% ----------------------------------------------------------------------------
-spec utf8_to_string(binary()) -> string().
utf8_to_string(Utf8) ->
	utf8_to_string(Utf8,[]).

%% ----------------------------------------------------------------------------
%% @doc Converts a raw text buffer into a UTF-8 buffer.
%% @spec string_to_utf8(Text) -> binary()
%%       Text = string()
%% @end
%% ----------------------------------------------------------------------------
string_to_utf8(Text) ->
  string_to_utf8(Text, []).

%% ============================================================================
%% Internal functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Implements the back end of the UTF-8 to Erlang-style string conversion,
%%      building the text string code point by code point. 
%% @spec utf8_to_string(Utf8,Text) -> string()
%% @end
%% ----------------------------------------------------------------------------	

%% 0-7f - leading bit is 0
utf8_to_string(<<2#0:1, CodePoint:7, Remainder/binary>>, Text) ->
	utf8_to_string(Remainder, [CodePoint|Text]);

%% 110yyyyy 10zzzzzz
utf8_to_string(<<2#110:3, Y:5, 2#10:2, Z:6, Remainder/binary>>, Text) ->
	<<CodePoint:16>> = <<0:5, Y:5, Z:6>>,
	utf8_to_string(Remainder, [CodePoint|Text]); 

%% 1110xxxx 10yyyyyy 10zzzzzz
utf8_to_string(<<2#1110:4, X:4, 2#10:2, Y:6, 2#10:2, Z:6, Remainder/binary>>, 
               Text) ->
	<<CodePoint:16>> = <<X:4, Y:6, Z:6>>,
	utf8_to_string(Remainder, [CodePoint|Text]);
	
%% 11110www 10xxxxxx 10yyyyyy 10zzzzzz
utf8_to_string(<<2#11110:5, W:3, 2#10:2, X:6, 2#10:2, Y:6, 2#10:2, Z:6, Remainder/binary>>, 
			   Text)->
	<<CodePoint:24>> = <<0:3,W:3,X:6,Y:6,Z:6>>,
	utf8_to_string(Remainder, [CodePoint|Text]);

%% The character is invalid UTF-8 but is a valid 8859-1 character
utf8_to_string(<<CodePoint:8, Remainder/binary>>, Text) 
  when CodePoint < 16#FF ->
  utf8_to_string(Remainder, [CodePoint|Text]);

%% nothing left in the source, return the text
utf8_to_string(<<>>, Text) ->
	lists:reverse(Text).

%% ----------------------------------------------------------------------------
%% @doc Implements the back end of the Erlang-style string to UTF-8 conversion,
%%      building the text string code point by code point. 
%% @spec utf8_to_string(Utf8,Text) -> string()
%% @end
%% ----------------------------------------------------------------------------	

% End of input. Reverse the list, convert it to a binary and return it 
string_to_utf8([], Utf8) ->
  list_to_binary(lists:reverse(Utf8));
  
% The charac=ter range 0-7f - encode as itself
string_to_utf8([Char|Text], Utf8) when Char < 16#80 ->
  string_to_utf8(Text, [Char|Utf8]);

% The character range 80-7FF - encode as a 2-byte character as 
% yyy:yyzzzzzz -> 110yyyyy:10zzzzzz
string_to_utf8([Char|Text], Utf8) when Char < 16#800 ->
  <<_:5,Y:5,Z:6>> = <<Char:16>>,
  string_to_utf8(Text, [<<2#110:3, Y:5, 2#10:2, Z:6>>|Utf8]);
  
% The range 800-FFFF - encoode in a 3-byte sequence as
% xxxxyyyy:yyzzzzzz -> 1110xxxx:10yyyyyy:10zzzzzz
string_to_utf8([Char|Text], Utf8) when Char < 16#10000 ->
  <<X:4,Y:6,Z:6>> = <<Char:16>>,
  string_to_utf8(Text, [<<2#1110:4,X:4,2#10:2,Y:6,2#10:2,Z:6>>|Utf8]);
  
% The range 10000-10FFFF - encode as a 4-byte sequence as
% wwwxx:xxxxyyyy:yyzzzzzz -> 11110www:10xxxxxx:10yyyyyy:10zzzzzz
string_to_utf8([Char|Str], Utf8) when Char < 16#110000 ->
  <<0:3,W:3,X:6,Y:6,Z:6>> = <<Char:24>>,
  EncodedChar = <<2#11110:5,W:3,2#10:2,X:6, 2#10:2,Y:6,2#10:2,Z:6>>,
  string_to_utf8(Str, [EncodedChar|Utf8]);
  
% bad character
string_to_utf8([Char|_], _Utf8) ->
  throw({badarg, Char}).

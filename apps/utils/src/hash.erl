-module (hash).
-author("Trent Clarke <trent.clarke@gmail.com>").
-export([md5_str/1]).

-spec md5_str(string()) -> string().
md5_str(S) -> hex_string(erlang:md5(S)).

hex_string(<<X:128/big-unsigned-integer>>) -> lists:flatten(io_lib:format("~32.16.0b", [X]));
hex_string(B) -> [Hex || <<N:4>> <= B, Hex <- hex_char(N)].

hex_char(N) when (N < 10) -> $0 + N;
hex_char(N) -> $a + (N - 10).

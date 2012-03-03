-module (rtsp_authentication).
-author ("Trent Clarke <trent.clarke@gmail.com>").
-include_lib("eunit/include/eunit.hrl").
-include("rtsp.hrl").
-include("digest.hrl").

-export([parse/1, get_user_name/1, validate/4, get_headers/3, authenticate_digest/4]).

-compile(export_all).

%% ============================================================================
%% Types
%% ============================================================================
-type auth_info() :: #basic{} | #digest{}.
-opaque_type([auth_info/0]).

%% ============================================================================
%% Public API
%% ============================================================================
-spec parse(string()) -> {ok, auth_info()} | {error, any()}.
parse(Header) ->
  case string:tokens(Header, ", ") of
    ["Basic" | Tokens] -> parse_basic(Tokens);
    ["Digest" | Tokens] -> parse_digest(Tokens);
    _ -> {error, bad_request}
  end.

get_user_name(AuthInfo) when is_record(AuthInfo, digest) ->
  AuthInfo#digest.user_name.
  
-spec validate(rtsp:conn(), 
               rtsp:message(), 
               auth_info(), 
               rtsp:user_info()) ->  'ok' | 'fail' | 'stale'.

validate(Conn, #rtsp_message{message = Rq}, AuthInfo, UserInfo) 
    when is_record(Rq, rtsp_request),
         is_record(AuthInfo, digest) -> 
  Ctx = rtsp_digest_server:get_context(Conn),
  authenticate_digest(Rq, AuthInfo, UserInfo, Ctx).
  
-spec get_headers(rtsp:conn(), string(), [rstp:auth_flags()]) -> 
        [{string(), string()}].
get_headers(Conn, Realm, Flags) -> [
    {?RTSP_HEADER_AUTHENTICATE, get_digest_header(Conn, Realm, [Flags])}
  ].

%% ============================================================================
%% Private API
%% ============================================================================
parse_basic(_Elements) -> #basic{}.

%% ============================================================================
%% Digest Authentication
%% ============================================================================

-spec get_digest_header(rtsp:conn(), string(), [rtsp:auth_flags()]) -> string().
get_digest_header(Conn, Realm, Flags) ->
  Ctx      = rtsp_digest_server:get_context(Conn),
  Nonce    = Ctx#digest_ctx.nonce,
  Opaque   = Ctx#digest_ctx.opaque,
  Elements = lists:map(
              fun({N,V}) -> N ++ "=" ++ V end,
              [ {"realm",  Realm},
                {"nonce",  quote(Nonce)}, 
                {"opaque", quote(Opaque)},
                {"qop",    "auth"},
                {"algorithm", "MD5"},
                {"stale",  atom_to_list(lists:member(stale, Flags))} ] ),
                
  "Digest " ++ string:join(Elements, ",").

-spec parse_digest([string()]) -> {ok, #digest{}} | {error, bad_header}.
parse_digest(Elements) ->
  Tokeniser = fun(S) -> case string:tokens(S, "=") of
                          [N,V] -> {N, string:strip(V, both, $")};
                          [N] -> {N,""}
                        end
              end,
  Items = lists:map(Tokeniser, Elements),
  try
    Digest = lists:foldl(fun parse_digest_element/2, #digest{}, Items),
    % validate digest record
    {ok, Digest}
  catch
    _ -> {error, bad_header}
  end.

%% ----------------------------------------------------------------------------
%% @doc Parses a single element of a digest header and "updates" the digest 
%%      record. Ignores any elements it doesn't understand, but will throw a
%%      bad_header if a known item fails to parse.
%% @throws bad_header 
%% @private
%% @end
%% ----------------------------------------------------------------------------
-spec parse_digest_element({string(),string()}, #digest{}) -> #digest{}.

parse_digest_element({"username", V}, D) -> D#digest{user_name    = V};  
parse_digest_element({"realm",    V}, D) -> D#digest{realm        = V};
parse_digest_element({"nonce",    V}, D) -> D#digest{nonce        = V};
parse_digest_element({"uri",      V}, D) -> D#digest{uri          = V};
parse_digest_element({"cnonce",   V}, D) -> D#digest{client_nonce = V};
parse_digest_element({"nc",       V}, D) -> D#digest{nonce_count  = V};
parse_digest_element({"opaque",   V}, D) -> D#digest{opaque       = V};

parse_digest_element({"response", V}, D) -> 
  D#digest{response = string:to_lower(V)};

parse_digest_element({"qop", V}, D) -> 
  case V of 
    "auth"     -> D#digest{qop = auth};
    "auth-int" -> D#digest{qop = auth_int};
    _ -> throw(bad_header)
  end;
  
parse_digest_element(_, D) -> D.

%% ----------------------------------------------------------------------------
%% @doc Validates a digest response by computing the "expected" value and
%%      returned by the comparing it to the one returned by the client
%% @private
%% @end
%% ----------------------------------------------------------------------------
-spec authenticate_digest(rtsp:request(), 
                          #digest{}, 
                          rtsp:user_info(),
                          rtsp_digest_server:ctx()) -> ok | fail | stale.
authenticate_digest(
  _Request   = #rtsp_request{ method = Method }, 
  AuthInfo   = #digest{ user_name = UserName, realm = Realm, qop = QoP, 
                        uri = Uri, nonce = Nonce }, 
  _UserInfo  = #rtsp_user_info{ password = Pwd },
  DigestCtx) ->
  
  A1 = hash(string:join([UserName, Realm, Pwd], ":")),
  A2 = hash(string:join([Method, Uri], ":")),  % ignoring auth_int for now
  
  Secret = if 
    (QoP == auth) or (QoP == auth_int) ->
      CNonce = AuthInfo#digest.client_nonce,
      Nc = AuthInfo#digest.nonce_count,
      string:join([A1, Nonce, Nc, CNonce, atom_to_list(QoP), A2], ":");
      
    (QoP == undefined) -> 
      string:join([A1, Nonce, A2], ":")
  end,
  Expected = hash(Secret),
  
  case Expected == AuthInfo#digest.response of
    false -> fail;
    true -> case Nonce == DigestCtx#digest_ctx.nonce of
              true -> ok;
              false -> stale
            end
  end.
  
%% ============================================================================
%% Utilities
%% ============================================================================  
-spec hash(string()) -> string().
hash(S) -> hex_string(erlang:md5(S)).

-spec hex_string(binary()) -> string().
hex_string(B) -> 
  H = case B of 
        <<X:128/big-unsigned-integer>> -> lists:flatten(io_lib:format("~32.16.0b", [X]))
      end,
  H.

-spec hex_char(integer()) -> integer(). 
hex_char(N) when (N < 10) -> $0 + N;
hex_char(N) -> $a + (N - 10).

quote(S) -> "\"" ++ S ++ "\"".

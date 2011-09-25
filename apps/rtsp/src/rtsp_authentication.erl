-module (rtsp_authentication).
-author ("Trent Clarke <trent.clarke@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

-include("../include/rtsp.hrl").
-include("../include/digest.hrl").

-export([parse/1, get_user_name/1]).

-compile(export_all).

%% ============================================================================
%% Types
%% ============================================================================
-record (basic, {}).
-record (digest, { user_name    :: string(),
                   realm        :: string(),
                   nonce        :: string(),
                   uri          :: string(),
                   qop          :: 'auth' | 'auth_int',
                   nonce_count  :: string(),
                   client_nonce :: string(),
                   response     :: string(),
                   opaque       :: string() }).
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
  
-spec validate(rtsp_connection:conn(), 
               rtsp:request(), 
               auth_info(), 
               rtsp:user_info()) ->  'ok' | 'fail'.
validate(_Conn, _Request, _AuthInfo, _UserInfo) -> fail.

%% ============================================================================
%% Private API
%% ============================================================================
parse_basic(_Elements) -> #basic{}.

%% ============================================================================
%% Digest Authentication
%% ============================================================================

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
parse_digest_element({"response", V}, D) -> D#digest{response     = V};
parse_digest_element({"opaque",   V}, D) -> D#digest{opaque       = V};

parse_digest_element({"qop", V}, D) -> 
  case V of 
    "auth"     -> D#digest{qop = auth};
    "auth-int" -> D#digest{qop = auth_int};
    _ -> throw(bad_header)
  end;
parse_digest_element(_, D) -> D.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec authenticate_digest(rtsp:request(), 
                          #digest{}, 
                          rtsp:user_info(),
                          rtsp_digest_server:ctx()) -> ok | fail | stale.
authenticate_digest(
  _Request   = #rtsp_request{ method = Method }, 
  AuthInfo  = #digest{ user_name = UserName, realm = Realm, qop = QoP, 
                       uri = Uri, nonce = Nonce }, 
  UserInfo  = #rtsp_user_info{ password = Pwd },
  DigestCtx) ->
  
  A1 = string:join([UserName, Realm, Pwd], ":"),
  A2 = string:join([Method, Uri], ":"),  % ignoring auth_int for now
  
  Secret = if 
    (QoP == auth) or (QoP == auth_int) ->
      ClientNonce = AuthInfo#digest.client_nonce,
      Nc = AuthInfo#digest.nonce_count,
      string:join([hash(A1), Nonce, Nc, ClientNonce, atom_to_list(QoP), hash(A2)], ":");
      
    (QoP == undefined) -> 
      string:join([hash(A1), Nonce, hash(A2)], ":")
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
hex_string(<<X:128/big-unsigned-integer>>) -> lists:flatten(io_lib:format("~32.16.0b", [X])).

-spec hex_char(integer()) -> integer(). 
hex_char(N) when (N < 10) -> $0 + N;
hex_char(N) -> $a + (N - 10).  
  
%% ============================================================================
%% Unit tests
%% ============================================================================
rfc_digest_example() ->
  #digest{ user_name    = "Mufasa",
           realm        = "testrealm@host.com",
           nonce        = "dcd98b7102dd2f0e8b11d0f600bfb0c093", 
           uri          = "/dir/index.html",
           qop          = auth,
           nonce_count  = "00000001",
           client_nonce = "0a4f113b",
           response     = "6629fae49393a05397450978507c4ef1",
           opaque       = "5ccc069c403ebaf9f0171e9517f40e41"}.
           
parse_digest_rfc_test() ->
  Header = "Digest username=\"Mufasa\"," ++ 
                  "realm=\"testrealm@host.com\"," ++
                  "nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\"," ++ 
                  "uri=\"/dir/index.html\"," ++
                  "qop=auth," ++ 
                  "nc=00000001," ++ 
                  "cnonce=\"0a4f113b\"," ++ 
                  "response=\"6629fae49393a05397450978507c4ef1\"," ++
                  "opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"",
  Expected = rfc_digest_example(),
  ?assertEqual({ok, Expected}, parse(Header)).
  
digest_auth_test() ->
  AuthInfo = rfc_digest_example(),
  UserInfo = #rtsp_user_info{id = 1, username = "Mufasa", password = "Circle Of Life"},
  Ctx      = {digest_ctx, "dcd98b7102dd2f0e8b11d0f600bfb0c093", now(), now()},
  Request  = #rtsp_request{method = "GET", uri = "/dir/index.html"},
  ?assertEqual(ok, authenticate_digest(Request, AuthInfo, UserInfo, Ctx)).
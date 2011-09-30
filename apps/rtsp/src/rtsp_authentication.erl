-module (rtsp_authentication).
-author ("Trent Clarke <trent.clarke@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

-include("../include/rtsp.hrl").
-include("../include/digest.hrl").
-include("../../../include/config.hrl").

-export([parse/1, get_user_name/1, validate/4, get_headers/3]).

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
%% @provate
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
  
parse_qt_response_test() ->
  Text = "Digest username=\"trent\", realm=\"rtsp-server\", " ++
         "nonce=\"e309342f55fd940f\", uri=\"/trent.sdp\", qop=\"auth\", " ++ 
         "nc=00000001, cnonce=\"22db2b8d\", " ++
         "response=\"f0e72c9fdc58d15dddf6781b9f1ca42e\", " ++ 
         "opaque=\"e4e0ab649c90fc0e0d0705cf4154d4d6\"",
  Expected = #digest{ user_name    = "trent", 
                      realm        = "rtsp-server", 
                      nonce        = "e309342f55fd940f",
                      uri          = "/trent.sdp",
                      qop          = auth, 
                      nonce_count  = "00000001",
                      client_nonce = "22db2b8d",
                      response     = "f0e72c9fdc58d15dddf6781b9f1ca42e", 
                      opaque       = "e4e0ab649c90fc0e0d0705cf4154d4d6" },
  ?assertEqual({ok, Expected}, parse(Text)).

digest_qt_auth_test() ->
  Text = lists:concat([
    "Digest username=\"trent\", realm=\"rtsp-server\", ",  
            "nonce=\"ea995080f2b97218\", uri=\"/trent.sdp\", qop=\"auth\", ",
            "nc=00000001, cnonce=\"3ce8b1e6\", ",
            "response=\"9848b05e17a00db2afadd399f53fcc80\", algorithm=\"MD5\", ",
            "opaque=\"4f677a90026c75378f29b23625997aab\""]),
  {ok, AuthInfo} = parse(Text),
  Ctx = {digest_ctx, "ea995080f2b97218", now(), now()},
  UserInfo = #rtsp_user_info{id = 1, username = "trent", password = "password"},
  Request = #rtsp_request{method = "ANNOUNCE", uri = "/trent.sdp"}, 
  ?assertEqual(ok, authenticate_digest(Request, AuthInfo, UserInfo, Ctx)).

-module (authentication_tests).
-include_lib("eunit/include/eunit.hrl").
-include("digest.hrl").
-include("rtsp.hrl").

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
  ?assertEqual({ok, Expected}, rtsp_authentication:parse(Header)).
  
digest_auth_test() ->
  AuthInfo = rfc_digest_example(),
  UserInfo = #rtsp_user_info{id = 1, username = "Mufasa", password = "Circle Of Life"},
  Ctx      = #digest_ctx{nonce = "dcd98b7102dd2f0e8b11d0f600bfb0c093", 
                         created = {0,0,0}, 
                         touched = {0,0,0}},
  Request  = #rtsp_request{method = "GET", uri = "/dir/index.html"},
  ?assertEqual(ok, rtsp_authentication:authenticate_digest(Request, AuthInfo, UserInfo, Ctx)).
  
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
  ?assertEqual({ok, Expected}, rtsp_authentication:parse(Text)).

digest_qt_auth_test() ->
  Text = lists:concat([
    "Digest username=\"trent\", realm=\"rtsp-server\", ",  
            "nonce=\"ea995080f2b97218\", uri=\"/trent.sdp\", qop=\"auth\", ",
            "nc=00000001, cnonce=\"3ce8b1e6\", ",
            "response=\"9848b05e17a00db2afadd399f53fcc80\", algorithm=\"MD5\", ",
            "opaque=\"4f677a90026c75378f29b23625997aab\""]),
  {ok, AuthInfo} = rtsp_authentication:parse(Text),
  Ctx = #digest_ctx{ nonce = "ea995080f2b97218", created = now(), touched = now() },
  UserInfo = #rtsp_user_info{id = 1, username = "trent", password = "password"},
  Request = #rtsp_request{method = "ANNOUNCE", uri = "/trent.sdp"}, 
  ?assertEqual(ok, rtsp_authentication:authenticate_digest(Request, AuthInfo, UserInfo, Ctx)).
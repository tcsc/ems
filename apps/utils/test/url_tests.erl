-module(url_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Unit Tests
%% ============================================================================

parse_rtsp_localhost_test() ->
  ?assertEqual({rtsp, "localhost", 4321, "/some/path/or/other"}, 
               url:parse("rtsp://localhost:4321/some/path/or/other")).

parse_rtsp_ip4_host_test() ->
  ?assertEqual({rtsp, "127.0.0.1", 4321, "/some/path"},
               url:parse("rtsp://127.0.0.1:4321/some/path")).

parse_rtsp_default_port_test() ->
  ?assertEqual({rtsp, "localhost", 554, "/path"}, 
               url:parse("rtsp://localhost/path")).

parse_http_default_port_test() ->
  ?assertEqual({http, "localhost", 80, "/path.html"},
               url:parse("http://localhost/path.html")).

parse_unknown_scheme_test() ->
  ?assertEqual({error,unknown}, url:parse("narf://something@strange")).

simple_concat_test() ->
  ?assertEqual("http://localhost/root/leaf", 
               url:join("http://localhost/root", "leaf")).

trailing_separator_test() ->
  ?assertEqual("http://localhost/root/leaf", 
               url:join("http://localhost/root/", "leaf")).

query_string_test() ->
  ?assertEqual("http://localhost/root?name=value",
               url:join("http://localhost/root", "?name=value")).

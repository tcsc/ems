-module (sdp_tests).
-include_lib("eunit/include/eunit.hrl").
-include("sdp.hrl").
-compile(export_all).

%% ============================================================================
%% Unit Tests
%% ============================================================================

invalid_parse_test() -> 
  ?assertEqual(fail, sdp:parse("v=0\r\nnarf\r\n")).

rfc_text() -> 
  "v=0\r\n"                                              ++ 
  "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\r\n"   ++
  "s=SDP Seminar\r\n"                                    ++ 
  "i=A Seminar on the session description protocol\r\n"  ++
  "u=http://www.example.com/seminars/sdp.pdf\r\n"        ++
  "e=j.doe@example.com (Jane Doe)\r\n"                   ++
  "c=IN IP4 224.2.17.12/127\r\n"                         ++
  "t=2873397496 2873404696\r\n"                          ++
  "a=recvonly\r\n"                                       ++
  "m=audio 49170 RTP/AVP 0\r\n"                          ++
  "a=control:streamid=2\r\n"                             ++   
  "m=video 51372/2 RTP/AVP 97 98 99\r\n"                 ++
  "a=rtpmap:99 h263-1998/90000\r\n"                      ++
  "a=fmtp:99 1234567890ABCDEF\r\n"                       ++
  "a=control:streamid=1\r\n"                             ++
  "a=name:value".
  
rfc_parse_test() ->
  Vids = #stream{type = video,
                 ports = [51372, 51373],
                 transport = "RTP/AVP",
                 formats = [97, 98, 99],
                 attributes = [{"name", "value"}],
                 control_uri = "streamid=1",
                 bandwidth_info = undefined },
  Auds = #stream{type = audio,
                 ports = [49170],
                 transport = "RTP/AVP",
                 formats = [0],
                 attributes = [],
                 control_uri = "streamid=2",
                 bandwidth_info = undefined },
  Expected = #description{ name = "SDP Seminar",
                           info = "A Seminar on the session description protocol",
                           attributes = [{"recvonly", ""}],
                           streams = [Auds, Vids],
                           rtp_map = [{99, #rtp_map{id = 99, 
                                                    encoding = "h263-1998",  
                                                    clock_rate = 90000, 
                                                    options = []}}],
                           format_map = [{99, "1234567890ABCDEF"}] },
  ?assertEqual({ok, Expected}, sdp:parse(rfc_text())).
  
  
qt_text() ->
  "v=0\r\n"                                              ++
  "o=- 10 806656362 IN IP4 127.0.0.0\r\n"                    ++
  "s=QuickTime\r\n"                                          ++
  "c=IN IP4 127.0.0.1\r\n"                                   ++
  "t=0 0\r\n"                                                ++
  "a=x-qt-text-an.:Test Broadcast\r\n"                       ++
  "a=x-qt-text-ua.:Trent Clarke\r\n"                         ++
  "a=isma-compliance:2,2.0,2\r\n"                            ++
  "m=audio 0 RTP/AVP 96\r\n"                                 ++
  "b=AS:16\r\n"                                              ++
  "a=rtpmap:96 mpeg4-generic/22050/1\r\n"                    ++
  "a=fmtp:96 profile-level-id=15;mode=AAC-hbr;sizelength=13;indexlength=3;indexdeltalength=3;config=1388\r\n" ++
  "a=mpeg4-esid:101\r\n"                                     ++
  "a=control:trackid=1\r\n"                                  ++
  "m=video 0 RTP/AVP 97\r\n"                                 ++
  "b=AS:1372\r\n"                                            ++
  "a=rtpmap:97 H264/90000\r\n"                               ++
  "a=fmtp:97 packetization-mode=1;profile-level-id=4D401E;sprop-parameter-sets=J01AHqkYFAe2ANQYBBrbCte98BA=,KN4JF6A=\r\n" ++
  "a=mpeg4-esid:201\r\n"                                     ++
  "a=cliprect:0,0,480,640\r\n"                               ++
  "a=framesize:97 640-480\r\n"                               ++
  "a=control:trackid=2".
  
qt_parse_test() -> 
  Auds = #stream{ type = audio,
                  ports = [0],
                  transport = "RTP/AVP",
                  formats = [96],
                  attributes = [ {"mpeg4-esid", "101"} ],
                  control_uri = "trackid=1",
                  bandwidth_info = {application_specific,16} },
                        
  AudsRtp = #rtp_map{ id = 96, 
                      encoding = "mpeg4-generic",  
                      clock_rate = 22050, 
                      options = "1"},
                        
  Vids = #stream{ type = video,
                  ports = [0],
                  transport = "RTP/AVP",
                  formats = [97],
                  attributes = [ {"mpeg4-esid", "201"}, 
                                 {"cliprect", "0,0,480,640"}, 
                                 {"framesize", "97 640-480"} ],
                  control_uri = "trackid=2",
                  bandwidth_info = {application_specific, 1372} },
                        
  VidsRtp = #rtp_map{ id = 97, 
                      encoding = "H264",  
                      clock_rate = 90000, 
                      options = ""},

  Expected = #description{ name = "QuickTime",
                           attributes = [
                             {"x-qt-text-an.",   "Test Broadcast"},
                             {"x-qt-text-ua.",   "Trent Clarke"},
                             {"isma-compliance", "2,2.0,2"} ],
                           streams = [Auds, Vids],
                           rtp_map = [{96, AudsRtp}, {97, VidsRtp}],
                           format_map = [{96, "profile-level-id=15;mode=AAC-hbr;sizelength=13;indexlength=3;indexdeltalength=3;config=1388"},
                                         {97, "packetization-mode=1;profile-level-id=4D401E;sprop-parameter-sets=J01AHqkYFAe2ANQYBBrbCte98BA=,KN4JF6A="}]
                                  },
  
  ?assertEqual({ok, Expected}, sdp:parse(qt_text())).

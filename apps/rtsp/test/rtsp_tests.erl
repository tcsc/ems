-module (rtsp_tests).
-include_lib("eunit/include/eunit.hrl").
-include("rtsp.hrl").

parse_request_test() -> 
  Text = list_to_binary(
          "DESCRIBE rtsp://example.com/index RTSP/1.0\r\n" ++
          "CSeq: 42\r\n"                                   ++
          "User-Agent: dummy_agent/1.0.0.0\r\n"           ),
  Request = #rtsp_request{method = "DESCRIBE", uri = "rtsp://example.com/index", version = {1,0}},
  Headers = #rtsp_message_header{ sequence = 42, 
                                  content_length = 0,
                                  content_type = undefined, 
                                  headers = dict:append("User-Agent", "dummy_agent/1.0.0.0", 
                                              dict:append("CSeq", "42", dict:new())) },
  Expected = #rtsp_message{message = Request, headers = Headers, body = undefined},
  Value =  rtsp:parse_message(Text),
  ?assertEqual(Expected, Value).
  
parse_request_with_body_test() ->
  Text = list_to_binary(
          "DESCRIBE rtsp://example.com/index RTSP/1.0\r\n" ++
          "CSeq: 42\r\n"                                   ++
          "User-Agent: dummy_agent/1.0.0.0\r\n"            ++
          "Content-Type: text/plain\r\n"                   ++
          "Content-Length: 100"),
  Request = #rtsp_request{method = "DESCRIBE", uri = "rtsp://example.com/index", version = {1,0}},
  Headers = #rtsp_message_header{ sequence = 42, 
                                  content_length = 100,
                                  content_type = "text/plain", 
                                  headers = dict:append("Content-Length", "100",
                                              dict:append("Content-Type", "text/plain", 
                                                dict:append("User-Agent", "dummy_agent/1.0.0.0", 
                                                  dict:append("CSeq", "42", 
                                                    dict:new() )))) },
  Expected = #rtsp_message{message = Request, headers = Headers, body = undefined},
  Value =  rtsp:parse_message(Text),
  ?assertEqual(Expected, Value).
  
parse_interleaved_transport_test() ->
  Expected = [{protocol, rtp}, {profile, avp}, {lower_transport, tcp}, {interleaved, [0,1]}, {direction, inbound}],
  ?assertEqual(Expected, rtsp:parse_transport("RTP/AVP/TCP;interleaved=0-1;mode=\"RECORD\"")).
  
parse_udp_unicast_transport_test() ->
  Expected = [{protocol, rtp}, {profile, avp}, {lower_transport, udp}, unicast, {client_port, [1234,1235]}, {direction, inbound}],
  ?assertEqual(Expected, rtsp:parse_transport("RTP/AVP/UDP;unicast;client_port=1234-1235;mode=\"RECORD\"")).
  
parse_unspecified_unicast_transport_test() ->
  Expected = [{protocol, rtp}, {profile, avp}, {lower_transport, udp}, unicast, {client_port, [1234,1235]}, {direction, inbound}],
  ?assertEqual(Expected, rtsp:parse_transport("RTP/AVP;unicast;client_port=1234-1235;mode=\"RECORD\"")).
  
format_interleaved_transport_test() ->
  Transport = [{protocol, rtp}, {profile, avp}, {lower_transport, tcp}, {direction, inbound}, {interleaved, [0,1]}],
  ?assertEqual("RTP/AVP/TCP;interleaved=0-1;mode=\"RECORD\"", rtsp:format_transport(Transport)).
  
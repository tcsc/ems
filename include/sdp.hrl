-define(SDP_MIME_TYPE, "application/sdp").
-record(session_description, {attributes, streams, rtp_map, format_map}).
-record(media_stream, 
  {type, port, transport, format, attributes, control_uri, bandwidth_info}).
-record(rtp_map, {id, encoding, clock_rate, options}).
-record(address, {host, ttl, count}).


-type session_description() :: #session_description{}.
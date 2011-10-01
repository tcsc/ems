-define(SDP_MIME_TYPE, "application/sdp").

-record(address, {host, ttl, count}).

-record(media_stream, {
  type, 
  ports :: [integer()], 
  transport, 
  formats :: [integer()], 
  attributes = [], 
  control_uri :: string(), 
  bandwidth_info}).

-record(rtp_map, {
  id :: integer(), 
  encoding :: string(), 
  clock_rate :: integer(), 
  options}).

-record(session_description, {
  attributes :: [{string(), string()}], 
  streams    :: [#media_stream{}], 
  rtp_map    :: [{integer(), #rtp_map{}}], 
  format_map :: [{integer(), string()}] }).
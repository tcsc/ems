-module(ems_media_stream).

-record(stream_state, {}).

-export([new/1]).

new() ->
  #stream_state{}.
-module(ems_media_stream).

-record(stream_state, {}).

-export([new/0]).

new() ->
  #stream_state{}.

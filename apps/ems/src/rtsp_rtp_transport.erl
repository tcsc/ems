-module(rtsp_rtp_transport).
-author("Trent Clarke <trent.clarke@gmail.com>").
-behaviour(rtp_transport).

% Behaviour exports -----------------------------------------------------------
-export([init/1, describe/1, send/2, set_callback/2, destroy/1]).

% public API exports ----------------------------------------------------------
-export([new/3]).

%% ============================================================================
%% Type definitions
%% ============================================================================
-record(rtsp_transport, {
    rtp_channel  :: rtsp:channel(), 
    rtcp_channel :: rtsp:channel()}).
-type rtsp_transport() :: #rtsp_transport{}.

%% ============================================================================
%% Public API
%% ============================================================================
new(Conn, Session, TransportSpec) ->
  rtp_transport:new({Conn, Session, TransportSpec}).

%% ============================================================================
%% Internal API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Creates a new instance of an RTP-over-RTSP transport. Because this 
%%      transport is intrinsically tied to an RTSP connection, it registers
%%      a disconnection handler with the connection so that we can propagate 
%%      the loss of the transport up the chain. 
%% @end
%% ----------------------------------------------------------------------------
-spec init(Arguments :: {rtsp:conn(), rtsp:transport_spec()}) -> 
  {ok, rtsp_transport()} | {error, term()}.

init({Conn, TransportSpec}) -> 
  case lists:keyfind(interleaved, 1, TransportSpec) of
    {interleaved, [RtpIndex,RtcpIndex]} ->
      ChannelArgs = [{RtpIndex, undefined},{RtcpIndex, undefined}],
      case rtsp_connection:create_channels(Conn, ChannelArgs) of
        {ok, [RtpChannel,RtcpChannel]} ->  
          Transport = #rtsp_transport{rtp_channel = RtpChannel, 
                                      rtcp_channel = RtcpChannel},
          Dtor = fun() -> self_destruct(Transport) end, 
          rtsp_connection:add_disconnection_event(Dtor), 
          {ok, Transport};

        _ -> {error, "Failed to create channels"}
      end;
    _ -> {error, "Bad interleaved transport spec"}
  end.

%% ----------------------------------------------------------------------------
%% @doc Describes the transport by producing a partial Transport Spec for it. 
%% @end
%% ----------------------------------------------------------------------------
-spec describe(rtsp_transport()) -> ems:transport_spec().
describe(#rtsp_transport{rtp_channel = Rtp, rtcp_channel = Rtcp} = _Transport) -> 
  [{interleaved, [rtsp:channel_id(Rtp), rtsp:channel_id(Rtcp)]}].

send(Transport, Packet) -> ok.

set_callback(Transport, Callback) -> ok.

destroy(Transport) -> ok.

%% ----------------------------------------------------------------------------
%% @doc Called by the underlying RTSP connection when it goes down while a 
%%      transport is still using it. If this happens 
%% ----------------------------------------------------------------------------
self_destruct(_Transport) -> ok.

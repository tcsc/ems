-module(rtp).
-include ("rtp.hrl").
-export ([parse/1]).

%% ----------------------------------------------------------------------------
%% @doc Parses a binary data buffer into an RTP packet record.
%% @spec parse(Packet) -> Result
%%         Packet = binary()
%%         Result = false | {ok, RtpPacket}
%%         Rtp_Packet = rtp_packet
%% @end
%% ----------------------------------------------------------------------------
parse(Packet) when is_binary(Packet) ->
  case Packet of 
    <<2:2, _Padding:1, Extension:1, SyncSrcCount:4, Marker:1, PayloadType:7,
      Sequence:16/big, Timestamp:32/big, SyncSource:32, _/binary>> ->
    
      RtpHeaderLen = (12 + (4*SyncSrcCount)),
      StartOfPayload = RtpHeaderLen + case Extension of
        1 ->
          <<_:RtpHeaderLen/binary, _:16/big, Length:16/big>> = Packet,
          4 + Length;
        0 -> 0
      end,
  
      case Packet of
        <<_:StartOfPayload/binary, Payload/binary>> ->
          ParsedPacket = #rtp_packet{timestamp = Timestamp,
                                     sync_src = SyncSource,
                                     marker = Marker, 
                                     payload_type = PayloadType,
                                     sequence = Sequence,
                                     payload = Payload},
                                     
          {ok, ParsedPacket};
                                     
        _ -> false
      end;
      
    _ -> false
  end.
    
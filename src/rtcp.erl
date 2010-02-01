-module (rtcp).
-include ("rtcp.hrl").
-export ([parse/1]).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
parse(Data) ->
  parse(Data, 0, []).
  
parse(Data, Offset, Packets) when Offset < size(Data) ->
  case Data of
    <<_:Offset/binary, 2:2, _Padding:1, _ReceptionCount:5, PacketType:8, WordCount:16/big>> ->
      NewPackets = case parse_packet(PacketType, Data, Offset) of
        {ok, RtcpPacket } -> [RtcpPacket | Packets];
        _ -> Packets
      end,
      parse(Data, Offset + (4 * (WordCount+1)), Packets);
  
    _ -> % failed to extract an RTCP header - bail out with what we've got
      lists:reverse(Packets)
  end.

format_rr() -> <<>>.
    
format_short_rr(SyncSrc) ->
  <<2:2, 0:1, 0:5, ?RTCP_RECEIVER_REPORT:8, 1:16/big, SyncSrc:32/big>>.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
parse_packet(?RTCP_SENDER_REPORT, Data, Offset) ->
  {ok, undefied};
  
parse_packet(_PacketType, Data, Offset) ->
  false.
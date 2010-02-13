-module (rtcp).
-include ("rtcp.hrl").
-export ([parse/1]).

%% ----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------
-spec parse(binary()) -> {ok, [rtcp_packet()]} | false.
parse(Data) ->
  parse(Data, 0, []).

%% ----------------------------------------------------------------------------
%% ---------------------------------------------------------------------------- 
-spec parse(binary(), integer(), [rtcp_packet()]) -> 
  {ok, [rtcp_packet()]} | false .
  
parse(Data, Offset, Packets) when Offset < size(Data) ->
  case Data of
    <<_:Offset/binary, 2:2, _Padding:1, _ReceptionCount:5, PacketType:8, WordCount:16/big, _/binary>> ->
      PayloadSize = WordCount * 4,
      NewPackets = case parse_packet(PacketType, Data, Offset, Offset + PayloadSize) of
        {ok, RtcpPacket} -> [RtcpPacket | Packets];
        _ -> Packets
      end,
      parse(Data, Offset + PayloadSize + 4, NewPackets);
  
    _ -> % failed to extract an RTCP header - fail whale!
      false
  end;
  
parse(Data, Offset, Packets) ->
  {ok, lists:reverse(Packets)}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
format_rr() -> <<>>.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
format_short_rr(SyncSrc) ->
  <<2:2, 0:1, 0:5, ?RTCP_RECEIVER_REPORT:8, 1:16/big, SyncSrc:32/big>>.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
parse_packet(?RTCP_SENDER_REPORT, Data, Offset, _EndOfPacket) ->
  StartOfPayload = Offset + 4,
  <<_:StartOfPayload/binary,
    SyncSrc:32/big, 
    NtpTimeMSW:32/big, NtpTimeLSW:32/big, 
    RtpTime:32/big,
    PacketCount:32/big, 
    OctetCount:32/big,
    _/binary>> = Data,
  SenderReport = #rtcp_sr{sync_src = SyncSrc, 
                          ntp_time = (NtpTimeMSW bsl 32) + NtpTimeLSW,
                          rtp_time = RtpTime,
                          packet_count = PacketCount,
                          octet_count = OctetCount},
  {ok, SenderReport};
  
parse_packet(?RTCP_SDES, Data, Offset, _EndOfPacket) ->
  Sdes = parse_sdes_group(Offset+4, Data, []),
  {ok, Sdes};
  
parse_packet(_PacketType, _Data, _Offset, _WordCount) ->
  false.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
parse_sdes_group(Offset, Data, Groups) ->
  <<_:Offset/binary, SyncSrc:32/big, _/binary>> = Data,
  #rtcp_sdes{sync_src = SyncSrc, 
             items = parse_sdes_element(Offset + 4, Data, [])}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
parse_sdes_element(Offset, Data, Items) ->
  <<_:Offset/binary, Type:8, _/binary>> = Data,
  case Type of
    0 -> lists:reverse(Items);
      
    _ ->
      <<_:Offset/binary, _:8, Length:8, Text:Length/binary, _/binary>> = Data,
      NewItem = {Type, Text},
      NewItems = [NewItem | Items],
      parse_sdes_element(Offset + Length + 2, Data, NewItems)
  end.
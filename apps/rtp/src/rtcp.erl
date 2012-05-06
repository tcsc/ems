-module (rtcp).
-include ("rtp.hrl").
-export ([parse/1, format_rr/7]).

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
  
parse(_Data, _Offset, Packets) ->
  {ok, lists:reverse(Packets)}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec format_rr(SyncSrc :: non_neg_integer(), 
                MaxSequence :: non_neg_integer(),
                ExpectedPackets :: non_neg_integer(),
                LostPackets :: non_neg_integer(),
                Jitter :: integer(),
                LastSR :: ntp_timestamp(), 
                Delay :: wall_clock_time() ) -> binary().

format_rr(SyncSrc, MaxSequence, ExpectedPackets, LostPackets, Jitter, LastSR, Delay) -> 
  LossFraction = if
    LostPackets =< 0 -> 0;
    true -> trunc((ExpectedPackets / LostPackets) * 256)
  end,
  
  JitterP = trunc(Jitter),
  
  SrId = (LastSR bsr 16),

  <<2:2,                             % version 
    0:1,                             % padding
    1:5,                             % RC
    ?RTCP_RECEIVER_REPORT:8,         % PacketType
    7:16/big-unsigned-integer,       % PacketLength
    SyncSrc:32/big-unsigned-integer, % SyncSrc
    
    SyncSrc:32/big-unsigned-integer, % SyncSrc
    LossFraction:8,
    LostPackets:24/big-unsigned-integer,
    MaxSequence:32/big-unsigned-integer,
    JitterP:32/big-unsigned-integer,
    SrId:32/big-unsigned-integer,
    Delay:32/big-unsigned-integer>>.
                     
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec parse_packet(integer(), binary(), integer(), integer()) -> 
  {ok, rtcp_packet()}.

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
parse_sdes_group(Offset, Data, _Groups) ->
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

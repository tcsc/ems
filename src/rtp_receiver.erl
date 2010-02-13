-module (rtp_receiver).
-export ([start_link/3, receiver_entrypoint/4, enable/1]).

-include_lib("kernel/include/inet.hrl").
-include ("erlang_media_server.hrl").
-include ("rtp.hrl").
-include ("rtcp.hrl").

-type wall_clock_time() :: non_neg_integer().

-record(stats, {rx_packets          = 0 :: non_neg_integer(),
                rx_bytes            = 0 :: non_neg_integer(),
                wrap_count_ts       = 0 :: non_neg_integer(),
                wrap_count_seq      = 0 :: non_neg_integer(),
                max_seq             :: undefined | sequence_number(),
                max_ts              :: undefined | timestamp(), 
                jitter              :: undefined | float(),
                last_packet_seq     :: undefined | sequence_number(),
                last_packet_ts      :: undefined | timestamp(),
                last_packet_arrival :: undefined | wall_clock_time()}).
-type stats() :: #stats{}.

-record(state, {owner_pid        :: pid(), 
                enabled          = false :: boolean(), 
                sync_src         :: non_neg_integer(),
                rtcp_timer       :: undefined | reference(),
                last_rr          :: undefined | timestamp(),
                client_addr      :: ip_address(),
                client_rtp_port  :: non_neg_integer(),
                client_rtcp_port :: non_neg_integer(),
                clock_rate       :: non_neg_integer(),
                stats            = #stats{} :: stats()}).
-type state() :: #state{}.

%% ---------------------------------------------------------------------------- 
%% @spec start_link(ClockRate, TransportSpec, ClientAddress) -> Result
%%         Result = {Pid, ServerTransportSpec}
%% @end
%% ---------------------------------------------------------------------------- 
start_link(ClockRate, TransportSpec, ClientAddress) ->
  ?LOG_DEBUG("rtp_receiver:start_link/2 - starting RTP receiver", []),
  
  case transport_type(TransportSpec) of
    unicast ->
      ?LOG_DEBUG("rtp_receiver:start_link/2 - starting receiver process", []),
      {ok, {Pid, ServerTransport}} = proc_lib:start_link(?MODULE, 
        receiver_entrypoint, 
        [ClockRate, TransportSpec, ClientAddress, self()]),
      {Pid, ServerTransport};
      
    _ ->
      ?LOG_DEBUG("rtp_receiver:start_link/2 - Unsupported transport", []),
      throw({rtp_receiver, unsupported_transport})
  end.

%% ---------------------------------------------------------------------------- 
%% @spec receiver_entrypoint(TransportSpec, OwnerPid) -> ok.
%% ----------------------------------------------------------------------------  
receiver_entrypoint(ClockRate, TransportSpec, RemoteAddress, OwnerPid) ->
  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/3 - creating RTP sockets", []),
  
  ClientAddress = case lists:keyfind(source, 1, TransportSpec) of
    {source, Source} -> Source;
    false -> RemoteAddress
  end,
  
  SyncSource = case lists:keyfind(ssrc, 1, TransportSpec) of
    {ssrc, S} -> S;
    false -> random:uniform(99999999)
  end,
  
  {client_port, [ClientRtpPort, ClientRtcpPort]} = 
    lists:keyfind(client_port, 1, TransportSpec),
  
  {ok, RtpSocket, RtcpSocket} = rtp:create_socket_pair([binary, {active, true}]),
  {ok, {Host, RtpPort}} = inet:sockname(RtpSocket),
  {ok, RtcpPort} = inet:port(RtcpSocket),
  
  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/3 - Server Ports are ~w:~w-~w", 
    [Host, RtpPort, RtcpPort]),
    
  ServerTransportSpec = [
    {protocol, rtp},
    {profile, avp},
    {lower_transport, udp},
    unicast,
    {source, ClientAddress},
    {ssrc, SyncSource},
    {client_port, [ClientRtpPort, ClientRtcpPort]},
    {server_port, [RtpPort, RtcpPort]},
    {direction, inbound}
  ],

  proc_lib:init_ack({ok, {self(), ServerTransportSpec}}),

  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/1 - Starting RTP receiver loop", []),  
  receiver_loop(
    #state{owner_pid=OwnerPid, 
           sync_src=SyncSource,
           client_addr=ClientAddress,
           client_rtp_port=ClientRtpPort,
           client_rtcp_port=ClientRtcpPort,
           clock_rate = ClockRate}, 
    RtpSocket, 
    RtcpSocket).
  
%% ---------------------------------------------------------------------------- 
%% @spec receiver_loop(State, RtpSocket, RtcpSocket) ->
%% where
%%   State = state(),
%%   RtpSocket = socket(),
%%   RtcpSocket = socket()
%% @end
%% ----------------------------------------------------------------------------   
receiver_loop(State, RtpSocket, RtcpSocket) ->
  try
    receive
      enable -> 
        NewState = handle_enable(State),
        receiver_loop(NewState, RtpSocket, RtcpSocket);
      
  %    {timeout, TimerRef, send_rtcp_rr} ->
  %      NewState = send_rtcp_rr(State, RtcpSocket),
  %      receiver_loop(NewState, RtpSocket, RtcpSocket);
      
  %    send_rtcp_rr ->
  %      NewState = send_rtcp_rr(State, RtcpSocket),
  %      receiver_loop(NewState, RtpSocket, RtcpSocket);
        
      {udp, RtpSocket, Host, Port, Data} ->
        NewState = handle_rtp_packet(State, Host, Port, Data),
        receiver_loop(NewState, RtpSocket, RtcpSocket);
      
      {udp, RtcpSocket, Host, Port, Data} ->
        NewState = handle_rtcp_packet(State, Host, Port, Data),
        receiver_loop(NewState, RtpSocket, RtcpSocket);
        
      terminate -> 
        cleanup(State);
      
      Message ->      
        ?LOG_DEBUG("rtp_receiver:receiver_loop/3 - something happened (~w)", [Message]),
        receiver_loop(State, RtpSocket, RtcpSocket)
    end
  catch
    Error ->
      ?LOG_ERROR("rtp_receiver:receiver_loop/3 - error ~p", [Error]),
      cleanup(State)
  end.

enable(Receiver) -> Receiver ! enable.

%% ---------------------------------------------------------------------------- 
%% @spec handle_enable(State) -> NewState
%% ----------------------------------------------------------------------------   
handle_enable(State) ->
  ?LOG_DEBUG("rtp_receiver:handle_enable/1 - starting RR timer", []),
  Timer = erlang:start_timer(1000, self(), send_rtcp_rr),
  % self() ! send_rtcp_rr,
  NewState = State#state{rtcp_timer = Timer, enabled = true},
  NewState.

%% ---------------------------------------------------------------------------- 
%% @spec handle_enable(State) -> NewState
%% ----------------------------------------------------------------------------
send_rtcp_rr(State = #state{stats = Stats}, RtcpSocket) ->
  ?LOG_DEBUG("rtp_receiver:send_rtcp_rr/2", []),
  case Stats#stats.rx_packets of
    0 -> 
      Packet = rtcp:format_short_rr(State#state.sync_src),
      gen_udp:send(RtcpSocket,
                   State#state.client_addr, 
                   State#state.client_rtcp_port,
                   Packet);
    _ -> 
      ok
  end,
  State.
  
ntp_now(Now) ->
  {MSec, Sec, USec} = Now,
  Fraction = round( 16#FFFFFFFF * (USec / 1000000) ),
  ((MSec + Sec) bsl 32) + Fraction.

%% ---------------------------------------------------------------------------- 
%%
%% ----------------------------------------------------------------------------
-spec handle_rtp_packet(state(), ip_address(), integer(), binary()) -> state().
handle_rtp_packet(State, Host, Port, Data) ->
  ArrivalTime = get_time(),
  
  case rtp:parse(Data) of
    % looks like an RTP packet at first blush 
    {ok, RtpPacket} when is_record(RtpPacket, rtp_packet) ->
      % expand the rtp sequence number & timestamp, and do a rudimentary
      % duplicate packet test. Obvious duplicates will be swallowed.
      case expand_seq(RtpPacket, State) of
         duplicate -> 
           State;

         {ExpSeq, NewMaxSeq, NewWrapCountSeq} ->
           {ExpTS, NewMaxTS, NewWrapCountTS} = expand_ts(RtpPacket, State),
           
           Jitter = jitter(RtpPacket, State, ArrivalTime),
           
           % generate a new state record with the updated values
           Stats = State#state.stats,
           NewStats = Stats#stats{max_seq = NewMaxSeq,
                                  wrap_count_seq = NewWrapCountSeq,
                                  max_ts = NewMaxTS,
                                  wrap_count_ts =  NewWrapCountTS,
                                  rx_bytes = Stats#stats.rx_bytes + size(Data),
                                  rx_packets = Stats#stats.rx_packets + 1,
                                  jitter = Jitter,
                                  last_packet_arrival = ArrivalTime,
                                  last_packet_ts = ExpTS,
                                  last_packet_seq = ExpSeq},
          State#state{stats = NewStats}
       end;
      
    false -> %  obviously not an RTP packet
      State
  end.
  
%% ---------------------------------------------------------------------------- 
%%
%% ---------------------------------------------------------------------------- 
handle_rtcp_packet(State, Host, Port, Data) ->
  case rtcp:parse(Data) of
    {ok, Packets} -> State;
    false -> State
  end.

%% ---------------------------------------------------------------------------- 
%% @doc Tidies up as the process exits
%% ---------------------------------------------------------------------------- 
cleanup(State) ->
  case State#state.rtcp_timer of
    undefined -> false;
    TimerRef -> elrang:cancel_timer(TimerRef)
  end. 

%% ----------------------------------------------------------------------------
%% @doc Expands the RTP pacet timestamp, accounting for wrapping and suchlike.
%% @end
%% ----------------------------------------------------------------------------
-spec expand_ts(rtp_packet(), state()) -> 
  {integer(), integer(), integer()}.
  
expand_ts(RtpPacket = #rtp_packet{timestamp = PacketTS}, 
          State = #state{stats = Stats}) ->  
  case Stats#stats.max_ts of 
    undefined ->
      {PacketTS, PacketTS, 0};
      
    MaxTS ->  
      WrapCount = Stats#stats.wrap_count_ts,
      Delta = PacketTS - MaxTS,
  
      if
        % same or forwards jump - handle normally...
        Delta >= 0 ->
          ExpandedTS = (WrapCount bsl 32) + PacketTS,
          {ExpandedTS, PacketTS, WrapCount};
            
        % minor backwards jump - assume that we have a late-arriving packet
        -Delta < 16#7FFFFFFF ->
          ExpandedTS = (WrapCount bsl 32) + PacketTS,
          {ExpandedTS, MaxTS, WrapCount};

        % massive backwards jump - assume that we have wrapped 
        true ->
          NewWrapCount = WrapCount + 1,
          ExpandedTS = (NewWrapCount bsl 32) + PacketTS,
          {ExpandedTS, PacketTS, NewWrapCount}
      end
  end.
  
%% ----------------------------------------------------------------------------
%% {ExpandedSequence, HighestSequence, WrapCount}  
%% ---------------------------------------------------------------------------- 
-spec expand_seq(rtp_packet(), state()) -> 
  duplicate | {sequence_number(), sequence_number(), non_neg_integer()}.

expand_seq(_Packet = #rtp_packet{sequence = PacketSequence}, 
           _State = #state{stats = Stats}) ->
                         
  case Stats#stats.max_seq of
    undefined ->
      {PacketSequence, PacketSequence, 0};
      
    MaxSeq ->
      WrapCount = Stats#stats.wrap_count_seq,
      Delta = PacketSequence - MaxSeq,
  
      if 
        Delta == 0 ->
          duplicate;
      
        Delta >= 1 ->
          ExpandedSeq = (WrapCount bsl 16) + PacketSequence,
          {ExpandedSeq, PacketSequence, WrapCount};
      
        % small(ish) jump backwards - probably just a missing packet or two
        -Delta =< 16#7FFF ->
          ExpandedSeq = (WrapCount bsl 16) + PacketSequence,
          {ExpandedSeq, MaxSeq, WrapCount};
  
        % massive jump backwards - probably a wrap
        true ->
          NewWrapCount = WrapCount + 1,
          ExpandedSeq = (NewWrapCount bsl 16) + PacketSequence,
          {ExpandedSeq, PacketSequence, NewWrapCount}
      end
  end.

%% ----------------------------------------------------------------------------
%% @doc Calculates the jitter value for this packet
%% @spec jitter(Packet, State, ArrivalTime) -> Jitter
%% where
%%   Packet = rtp_packet(),
%%   State = state(),
%%   ArrivalTime = integer(),
%%   Jitter = Number
%% @end 
%% ----------------------------------------------------------------------------
-spec jitter(rtp_packet(), state(), wall_clock_time()) -> float().
jitter(Packet = #rtp_packet{timestamp = ThisPacketTS}, 
       State = #state{stats = Stats}, 
       ThisPacketArrivalTime) ->
  case Stats#stats.jitter of 
    undefined ->
      0.0;
      
    Jitter ->
      LastPacketArrivalTime = Stats#stats.last_packet_arrival,
      LastPacketTS =  Stats#stats.last_packet_ts,
      RtpInterval = ThisPacketTS - LastPacketTS,
      RealInterval = clock_time_to_rtp_time(ThisPacketArrivalTime - LastPacketArrivalTime, State),
      Delta = abs(RealInterval - RtpInterval),
      Jitter + ((Delta - Jitter) / 16)
  end.

%% ----------------------------------------------------------------------------
%% @doc Gets the current time in microseconds
%% @end
%% ----------------------------------------------------------------------------   
-spec get_time() -> wall_clock_time().
get_time() ->
  {MegaSeconds, Seconds, MicroSeconds} = now(),
  (((MegaSeconds * 1000000) + Seconds) * 1000000) + MicroSeconds.

%% ---------------------------------------------------------------------------- 
%% @doc Translates a wall-clock time (i.e. in microseconds) to the same time in
%%      RTP clock units
%% @spec clock_time_to_rtp_time(Timestamp, State) -> RtpTime
%% where
%%  Timestamp = integer(),
%%  State = state(),
%%  RtpTime = number()
%% @end
%% ---------------------------------------------------------------------------- 
clock_time_to_rtp_time(Timestamp, State = #state{clock_rate = ClockRate}) ->
  (Timestamp * ClockRate) / 1000000.

%% ---------------------------------------------------------------------------- 
%% @spec transport_type(TrasnportSpec) -> Result
%%       Result = interleaved | unicast | multicast
%% @end
%% ----------------------------------------------------------------------------    
transport_type(TransportSpec) ->
  case lists:keymember(interleaved, 1, TransportSpec) of
    true -> interleaved;
    false -> case lists:member(unicast, TransportSpec) of 
      true -> unicast;
      false -> case lists:member(multicast, TransportSpec) of
        true -> multicast
      end
    end
  end.
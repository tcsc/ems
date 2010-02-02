-module (rtp_receiver).
-export ([start_link/2, receiver_entrypoint/3, enable/1]).

-include ("erlang_media_server.hrl").
-include ("rtp.hrl").
-include ("rtcp.hrl").

-record(state, {owner_pid, 
                enabled = false, 
                sync_src,
                rtcp_timer,
                last_rr,
                client_addr,
                client_rtp_port,
                client_rtcp_port,
                packets_received = 0}).

%% ---------------------------------------------------------------------------- 
%% @spec start_link(TransportSpec) -> Result
%%         Result = {Pid, ServerTransportSpec}
%% @end
%% ---------------------------------------------------------------------------- 
start_link(TransportSpec, ClientAddress) ->
  ?LOG_DEBUG("rtp_receiver:start_link/2 - starting RTP receiver", []),
  
  case transport_type(TransportSpec) of
    unicast ->
      ?LOG_DEBUG("rtp_receiver:start_link/2 - starting receiver process", []),
      {ok, {Pid, ServerTransport}} = proc_lib:start_link(?MODULE, 
        receiver_entrypoint, 
        [TransportSpec, ClientAddress, self()]),
      {Pid, ServerTransport};
      
    _ ->
      ?LOG_DEBUG("rtp_receiver:start_link/2 - Unsupported transport", []),
      throw({rtp_receiver, unsupported_transport})
  end.

%% ---------------------------------------------------------------------------- 
%% @spec receiver_entrypoint(TransportSpec, OwnerPid) -> ok.
%% ----------------------------------------------------------------------------  
receiver_entrypoint(TransportSpec, RemoteAddress, OwnerPid) ->
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
           client_rtcp_port=ClientRtcpPort}, 
    RtpSocket, 
    RtcpSocket).
  
%% ---------------------------------------------------------------------------- 
%% 
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
  
 % ?LOG_DEBUG("rtp_receiver:handle_enable/1 - posting initial rr", []),
 self() ! send_rtcp_rr,
 State#state{rtcp_timer=Timer, enabled=true}.

%% ---------------------------------------------------------------------------- 
%% @spec handle_enable(State) -> NewState
%% ----------------------------------------------------------------------------
send_rtcp_rr(State, RtcpSocket) ->
  ?LOG_DEBUG("rtp_receiver:send_rtcp_rr/2", []),
  case State#state.packets_received of
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
handle_rtp_packet(State, Host, Port, Data) ->
  case rtp:parse(Data) of
    {ok, RtpPacket} when is_record(RtpPacket, rtp_packet) ->
      ?LOG_DEBUG("rtp_receiver:handle_rtp_packet/4 - type ~w #~w ~w bytes", 
        [RtpPacket#rtp_packet.payload_type, 
         RtpPacket#rtp_packet.sequence,
         size(Data)]),
         
      State;
    
    false -> % not obviously an RTP packet
      State
  end.
  
%% ---------------------------------------------------------------------------- 
%%
%% ---------------------------------------------------------------------------- 
handle_rtcp_packet(State, Host, Port, Data) ->
  ?LOG_DEBUG("rtp_receiver:handle_rtcp_packet/4 - ~p bytes from ~p:~p", 
    [size(Data), Host, Port]),
  State.
%  case rtcp:parse(Data) of
%    {ok, Packets} -> State;
%    false -> State
%  end.

%% ---------------------------------------------------------------------------- 
%% @doc Tidies up as the process exits
%% ---------------------------------------------------------------------------- 
cleanup(State) ->
  case State#state.rtcp_timer of
    Timer when is_pid(Timer) -> timer:stop(Timer);
    undefined -> ok
  end.

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
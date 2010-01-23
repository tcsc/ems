-module (rtp_receiver).
-record(state, {owner_pid}).
-export ([start_link/1, receiver_entrypoint/2]).

-include ("erlang_media_server.hrl").
-include ("rtp.hrl").

%% ---------------------------------------------------------------------------- 
%% @spec start_link(TransportSpec) -> Result
%%         Result = {Pid, {IpAddress, RtpPort, RtcpPort}}
%%         
%% @end
%% ---------------------------------------------------------------------------- 
start_link(TransportSpec) ->
  ?LOG_DEBUG("rtp_receiver:start_link/1 - starting RTP receiver", []),
  
  case transport_type(TransportSpec) of
    unicast ->
      ?LOG_DEBUG("rtp_receiver:start_link/1 - starting receiver process", []),
      {ok, {Pid, Ports}} = proc_lib:start_link(?MODULE, receiver_entrypoint, [TransportSpec, self()]),
      {Pid, Ports};
      
    _ ->
      ?LOG_DEBUG("rtp_receiver:start_link/1 - Unsupported transport", []),
      throw({rtp_receiver, unsupported_transport})
  end.
  
receiver_entrypoint(TransportSpec, OwnerPid) ->
  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/1 - creating RTP sockets", []),
  
  {ok, RtpSocket} = gen_udp:open(0, [binary, {active, true}]),
  {ok, RtcpSocket} = gen_udp:open(0, [binary, {active, true}]),

  {ok, {Host, RtpPort}} = inet:sockname(RtpSocket),
  {ok, RtcpPort} = inet:port(RtcpSocket),
  

  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/1 - Server Ports are ~w:~w-~w", 
    [Host, RtpPort, RtcpPort]),

  proc_lib:init_ack({ok, {self(), {Host, RtpPort, RtcpPort}}}),

  ?LOG_DEBUG("rtp_receiver:receiver_entrypoint/1 - Starting RTP receiver loop", []),  
  receiver_loop(#state{owner_pid = OwnerPid}, RtpSocket, RtcpSocket).
  
%% ---------------------------------------------------------------------------- 
%% 
%% ----------------------------------------------------------------------------   
receiver_loop(State,RtpSocket,RtcpSocket) ->
  receive
    {udp, RtpSocket, Host, Port, Data} ->
      NewState = handle_rtp_packet(State, Host, Port, Data),
      receiver_loop(NewState, RtpSocket, RtcpSocket);
      
    {udp, RtcpSocket, Host, Port, Data} ->
      NewState = handle_rtp_packet(State, Host, Port, Data),
      receiver_loop(NewState, RtpSocket, RtcpSocket);
      
    Message ->      
      ?LOG_DEBUG("rtp_receiver:receiver_loop/3 - something happened (~w)", [Message]),
      receiver_loop(State, RtpSocket, RtcpSocket)
  end.

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
  ?LOG_DEBUG("rtp_receiver:handle_rtcp_packet/4 - (~p bytes from ~w:~p)", 
    [size(Data), Host, Port]),
  State.
  
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
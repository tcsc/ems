-module(rtp_transport).
-author("Trent Clarke <trent.clarke@gmail.com>").
-export([behaviour_info/1, new/2, send/2, destroy/1]).

%% ============================================================================
%% Type Definitions
%% ============================================================================

-type packet_handler() :: fun(binary()->any()).
%% A function for handling packets received by the RTP transport.

-record(state, {module :: atom(), transport}).
-type rtp_transport() :: #state{}.
-opaque_types([rtp_transport/0]).

%% ============================================================================
%%
%% ============================================================================
behaviour_info(callbacks) ->
  [ {init, 3}, {send, 2}, {set_callback, 2}, {destroy, 1} ];
behaviour_info(_) -> 
  undefined.

new(Module, Args) -> 
  {ok, State} = Module:init(Args),
  #state{module = Module, transport = State}.

-spec send(RtpTransport :: rtp_transport(), Packet :: term()) -> rtp_transport().

send(State = #state{module = Module, transport = Transport}, Packet) ->
  TransportP = Module:send(Transport, Packet),
  State#state{transport = TransportP}.

destroy(_State = #state{module = Module, transport = Transport}) ->
  Module:destroy(Transport).

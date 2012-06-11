-module(rtp_transport).
-author("Trent Clarke <trent.clarke@gmail.com>").
-export([behaviour_info/1, new/2, send/2, destroy/1]).

%% =============================================================================
%% Type Definitions
%% =============================================================================
-type packet_handler() :: fun((binary()) -> any()).
%% A function for handling packets received by the RTP transport.
-record(state, {module :: atom(), transport}).
-type rtp_transport() :: #state{}.
-opaque_types([rtp_transport/0]).

%% =============================================================================
%% Exported Behaviour Specification
%% =============================================================================
behaviour_info(callbacks) ->
  [ {init, 1}, {send, 2}, {set_callback, 2}, {destroy, 1} ];

behaviour_info(_) -> 
  undefined.

%% =============================================================================
%% External API

%% =============================================================================

% ------------------------------------------------------------------------------
% @doc Creates a new RTP transport using the the supplied module to implement 
%      the actual, low-level networking
% @end
% ------------------------------------------------------------------------------
-spec new(Module :: module(), 
	        Args :: term()) -> 
  {ok, rtp_transport()} | {error, Reason :: term()}.

new(Module, Args) -> 
  case Module:init(Args) of
    {ok, State} -> 
      Handle = #state{module = Module, transport = State},
      {ok, Handle};

    Err -> Err  
end.

% ------------------------------------------------------------------------------
% @doc Sends a 
% @end
% ------------------------------------------------------------------------------
-spec send(RtpTransport :: rtp_transport(), 
	   Packet :: term()) -> rtp_transport().

send(State = #state{module = Module, transport = Transport}, Packet) ->
  TransportP = Module:send(Transport, Packet),
  State#state{transport = TransportP}.

destroy(_State = #state{module = Module, transport = Transport}) ->
  Module:destroy(Transport).

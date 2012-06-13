-module(rtsp_connection).
-behaviour(gen_fsm).

-include("rtsp.hrl").

%% ============================================================================
%% Public API
%% ============================================================================
-export([new/3, 
         close/1,
         add_disconnection_handler/2,
         create_channels/2,
         write_channel/2,
         channel_index/1,
         active_channels/1,
         set_channel_handler/2,
         take_socket/2, 
         get_client_address/1, 
         send_response/5, 
         with_authenticated_user_do/4]).

%% ============================================================================
%% gen_fsm exports
%% ============================================================================
-export([init/1,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3,
         code_change/4]).

%% ============================================================================
%% machine state exports
%% ============================================================================
-export([
  waiting_for_socket/2, 
  ready/2 ]).

%% ============================================================================
%% Internal exports
%% ============================================================================
-export([handle_data/3]).
-export([sender_loop/2]).


%% ============================================================================
%% Type definitions
%% ============================================================================

-type buffer_handler() :: fun((binary())->any()).
-type nullary_function() :: fun(()->any()).
-record(channel, {queue = queue:new() :: queue(),
                  handler             :: buffer_handler()} ).
-type channel() :: {rtsp:conn(), integer()}.

%% ----------------------------------------------------------------------------
%% @doc The state record for an RTSP connection.
%% @end
%% ----------------------------------------------------------------------------

-record(state, { server_str                  :: string(),
                 socket                      :: inet:socket(),
                 sender                      :: pid(),
                 pending_message             :: rtsp:message(),
                 pending_requests            :: dict(),
                 pending_data                :: binary(),
                 callback                    :: rtsp:request_callback(),
                 channels = array:new()      :: array(),
                 last_channel = -1           :: integer(),
                 message_queue = queue:new() :: queue(),
                 sender_waiting = false      :: boolean(),
                 disconnection_handlers = [] :: [{integer(), 
                                                  nullary_function()}],
                 id_seed = 0                 :: integer()
               }).

-define(SP,16#20).

%% Set to infinity for debugging
-define(timeout, infinity).

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------
-spec new(rtsp:svr(), string(), rtsp:request_callback()) -> 
  {'ok', rtsp:conn()} | {'error', any()}.
new(_Owner, ServerStr, Callback) -> 
  log:debug("rtsp_connection:new/1", []),
  State = #state{ server_str       = ServerStr,
                  pending_data     = << >>,
                  pending_requests = dict:new(),
                  callback         = Callback },
  gen_fsm:start_link(?MODULE, State, []).


%% ----------------------------------------------------------------------------
%% @doc Closes a connection and shuts down the socket. Closing a non-existent 
%%      connection is not considered an error.
%% @end 
%% ----------------------------------------------------------------------------
close(Conn) ->
  catch gen_fsm:sync_send_all_state_event(Conn, quit),
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
take_socket(Conn, Socket) ->
  log:debug("rtsp_connection:take_socket/2 - reassigning socket ownership to ~w", 
            [Conn]),
  ok = gen_tcp:controlling_process(Socket, Conn),
  
  log:debug("rtsp_connection:take_socket/2 - forwarding socket to connection"),
  gen_fsm:send_event(Conn, {socket, Socket}),
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
get_client_address(Conn) ->
  gen_fsm:sync_send_all_state_event(Conn, get_client_address, ?timeout).

%% ----------------------------------------------------------------------------
%% @doc Reserves a set of channels on the RTSP connection. The connection will
%%      attempt to reserve all the channels requested, and will fail if one of 
%%      them is not available.
%%
%%      The buffer handler callback will be invoked whenever a new buffer has 
%%      arrived for the registered channel. Receiving a zero-length buffer
%%      indicates that the connection is going (or has gone) down, and you 
%%      should start whatever cleanup work is necessary. 
%% @end 
%% ----------------------------------------------------------------------------
-spec create_channels(Conn :: rtsp:conn(), 
                      Indices :: [{integer(), buffer_handler()}]) ->
  {ok, [channel()]} | {error, term()}.
  
create_channels(Conn, Indices) ->
  gen_fsm:sync_send_all_state_event(Conn, {create_channels, Indices}, ?timeout).

%% ----------------------------------------------------------------------------
%% @doc Returns the index of the specified channel.
%% @end
%% ----------------------------------------------------------------------------
-spec channel_index(channel()) -> integer().
channel_index({_,Index}) -> 
  Index.

set_channel_handler({Conn,Index}, Handler) ->
 gen_fsm:sync_send_all_state_event(Conn, 
                                   {set_channel_handler, Index, Handler},
                                   ?timeout).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec active_channels(rtsp:conn()) -> [integer()].
active_channels(Conn) -> 
  gen_fsm:sync_send_all_state_event(Conn, get_active_channels, ?timeout).

%% ----------------------------------------------------------------------------
%% @doc Writes a data packet to the supplied channel
%% @end
%% ----------------------------------------------------------------------------
-spec write_channel(Channel::channel(), Data::binary()) -> ok | {error, term()}.
write_channel({Conn, Index}, Data) ->
  gen_fsm:send_all_state_event(Conn, {write_channel, Index, Data}).

%% ----------------------------------------------------------------------------
%% @doc Registers a callback function to be invoked when the connection is
%%      lost. Returns a cookie value uniquely identifying the registration.
%% @end 
%% ----------------------------------------------------------------------------
-spec add_disconnection_handler(rtsp:conn(), nullary_function()) ->
        Cookie :: term().

add_disconnection_handler(Conn, Handler) ->
  gen_fsm:sync_send_all_state_event(Conn, 
                                    {add_disconnection_handler, Handler},
                                    ?timeout).

%% ============================================================================
%% gen_fsm API
%% ============================================================================
init(State) -> 
  log:debug("rtsp_connection:init/1"),
  {ok, waiting_for_socket, State}.

%% ----------------------------------------------------------------------------
%% @spec handle_event(Event, StateName, StateData) -> Result
%%       Result = {next_state,NextStateName,NewStateData} |
%%                {next_state,NextStateName,NewStateData,Timeout} |
%%                {next_state,NextStateName,NewStateData,hibernate} | 
%%                {stop,Reason,NewStateData}
%% @end
%% ----------------------------------------------------------------------------
handle_event({send_response, Sequence, Status, ExtraHeaders, Body}, 
             StateName, 
             State) ->

  log:debug("rtsp_connection:handle_event/3 - send_response to request ~w (~w)", 
    [Sequence, Status]),

  StateP = 
    case deregister_pending_request(Sequence, State) of 
      {Msg, NewState} -> 
        Rq = Msg#rtsp_message.message,
        RtspVersion = Rq#rtsp_request.version,
        Size = byte_size(Body),
        AllHeaders = build_response_headers(Sequence, Size, ExtraHeaders),
        Response = #rtsp_response{status = Status, version = RtspVersion}, 
        Bytes = rtsp:format_message(Response, AllHeaders, Body),
        queue_message(Bytes, NewState);

      _ -> 
        State
    end,
  {next_state, StateName, StateP};

handle_event({write_channel, Index, Data}, StateName, State) ->
  NewState = queue_packet(Index, Data, State),
  FinalState = 
    case NewState#state.sender_waiting of 
      true -> send_item(NewState);
      false -> NewState
    end,
  {next_state, StateName, FinalState};

handle_event(_Event, StateName, StateData) -> 
  log:debug("rtsp_connection:handle_event/3 - ~w", [StateName]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%% @doc Handles process events
%% @end
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, 
            State, 
            StateData = #state{pending_data = PendingData}) ->
  log:trace("rtsp_connection:handle_info/3"),

  try
    % combine the newly-arrived data with the stuff leftover from the last 
    % reqest
    AccumulatedData = list_to_binary([PendingData, Data]),
  
    % process what we can of the data and get whatever's left back
    {ok, NewState, NewStateData, Leftovers} = 
      handle_data(AccumulatedData, StateData, State),
    
    % update the connection state with the leftovers from the processing
    NewNewStateData = 
      NewStateData#state{pending_data=Leftovers},
    
    % reset the socket so that it pings us again when more data arrives
    inet:setopts(Socket, [{active,once}]),
    {next_state, NewState, NewNewStateData}
  catch
    throw:bad_request ->
      send_response(self(), -1, bad_request, [], << >>),
      {stop, normal, StateData};

    _:_ -> 
      send_response(self(), -1, internal_server_error, [], << >>),
      {stop, normal, StateData}
  end;

handle_info({tcp_closed, _Socket}, _StateName, State) ->
  log:debug("rtsp_connection:handle_info/3 - tcp connection closed",[]),
  {stop, normal, State};

% Handles the sender thread asking for more data to send
handle_info({sender_waiting, _SenderPid}, StateName, State) ->
  {next_state, StateName, send_item(State)};

handle_info(Info, StateName, StateData) ->
  log:debug("rtsp_connection:handle_info/3 ~w, ~w, ~w", 
            [Info, StateName, StateData]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_sync_event(get_client_address, _From, StateName, State) ->
  log:debug("rtsp_connection:handle_sync_event/4"),
  {ok, {Host, _}} = inet:sockname(State#state.socket),
  {reply, Host, StateName, State};

handle_sync_event({create_channels, Indices}, _From, StateName, State) ->
  case register_channels(self(), Indices, State) of
    {ok, NewState, NewChannels} ->
      {reply, {ok, NewChannels}, StateName, NewState};
    Err ->
      {reply, Err, StateName, State}
  end;

handle_sync_event({set_channel_handler, Index, Handler}, 
                  _From, 
                  StateName, 
                  State) ->
  {Result, StateP} = 
    case set_channel_handler(Index, Handler, State) of
      {ok, NewState} -> {ok, NewState};
      Err -> {Err, State}
    end,
  {reply, Result, StateName, StateP};
   
handle_sync_event(get_active_channels, _From, StateName, State) ->
  Channels = State#state.channels,
  Result = array:sparse_foldl(fun(Idx,_,A) -> [Idx|A] end, [], Channels),
  {reply, lists:reverse(Result), StateName, State};

handle_sync_event({add_disconnection_handler, Handler}, 
                  _From, 
                  StateName, 
                  State = #state{id_seed = Id, 
                                 disconnection_handlers = Handlers} ) ->
  HandlersP = [{Id, Handler} | Handlers],
  StateP = State#state{id_seed = Id + 1, disconnection_handlers = HandlersP},
  {reply, Id, StateName, StateP}; 

handle_sync_event(quit, _, _, State) ->
  log:debug("rtsp_connection:handle_sync_event/4 - quit"),
  {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, StateData) -> 
  log:debug("rtsp_connection:handle_sync_event/4 - ~w", [_Event]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
terminate(_Reason, 
          _StateName, 
          _State = #state{sender = Sender, 
                          disconnection_handlers = Callbacks,
                          channels = Channels}) ->
  log:debug("rtsp_connection:terminate/3 - ~w", [_Reason]),
  lists:foreach(fun({_,Callback}) -> Callback() end, Callbacks),
  utils:array_sparse_foreach(
    fun(Channel) -> 
      case Channel#channel.handler of 
        undefined -> ok;
        F -> F(<< >>) 
      end
    end,
    Channels),
  stop_sender(Sender),
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) -> 
  {ok, StateName, StateData}.

%% ----------------------------------------------------------------------------
%% @doc Sends a send_response message to the supplied process. The process will 
%%      pick up the response from the receive loop, format it and queue it for
%%      sending 
%% @end
%% ----------------------------------------------------------------------------
-spec send_response(rtsp:conn(), integer(), any(), [any()], binary()) -> any().
send_response(Conn, Sequence, Status, ExtraHeaders, Body) -> 
  Event = {send_response, Sequence, Status, ExtraHeaders, Body},
  gen_fsm:send_all_state_event(Conn, Event).

%% ============================================================================
%% State callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
waiting_for_socket({socket, Socket}, State) -> 
  log:debug("rtsp_connection:waiting_for_socket/2 - got socket"),
  ok = inet:setopts(Socket, [{active,once}]),
  Sender = start_sender(self(), Socket),
  NewState = State#state{socket=Socket, sender=Sender},
  {next_state, ready, NewState};
  
waiting_for_socket(Message, State) -> 
  log:debug("rtsp_connection:waiting_for_socket/2 - ~w, ~w", [Message, State]),
  {next_state, waiting_for_socket, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
ready(Event, State) ->
  log:debug("rtsp_connection:ready/2 - ~w, ~w", [Event, State]),
  {next_state, ready, State}. 

%% ============================================================================
%% Internal Functions 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc
%% @end
%% ----------------------------------------------------------------------------
-spec queue_message(Message :: binary(), State :: #state{}) -> 
        NewState :: #state{}.
queue_message(Message, #state{message_queue = Q} = State) ->
  NewQ = queue:in(Message, Q),
  NewState = State#state{message_queue = NewQ},
  case State#state.sender_waiting of
    true -> send_item(NewState);
    false -> NewState
  end.

%% ----------------------------------------------------------------------------
%% @doc
%% @end
%% ----------------------------------------------------------------------------
-spec queue_packet(Id :: pos_integer(),
                   Data :: binary(), 
                   State :: #state{}) -> NewState :: #state{}. 

queue_packet(Id, Data, #state{channels = Channels} = State) ->
  Ch = array:get(Id, Channels),
  Q = queue:in(Data, Ch#channel.queue),
  NewChannels = array:set(Id, Ch#channel{queue = Q}, Channels),
  State#state{channels = NewChannels}.
                          
%% ----------------------------------------------------------------------------
%% @doc
%% @end
%% ----------------------------------------------------------------------------
-spec send_item(State :: #state{}) -> NewState :: #state{}.
send_item(#state{message_queue = Messages, sender = TxPid} = State) ->
  case queue:out(Messages) of
    {{value, M}, Q} ->
      TxPid ! {send_item, self(), M},
      State#state{message_queue = Q, sender_waiting = false};

    {empty, _} ->
      case get_packet_to_send(State) of
        {ok, Packet, NewState} ->
          TxPid ! {send_item, self(), Packet},
          NewState#state{sender_waiting = false};

        false ->
          State#state{sender_waiting = true}
      end
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-spec get_packet_to_send(#state{}) -> {ok, binary(), #state{}} | false.

get_packet_to_send(#state{last_channel = LastCh, channels = Chs} = State) ->
  case array:size(Chs) of
    0 -> false;
    ChCount -> 
      ChIndex = (LastCh + 1) rem ChCount,
      case get_packet_to_send(ChIndex, Chs, ChCount, 0) of
        {ok, ChId, Packet, NewChannels} ->
          NewState = State#state{last_channel = ChId, channels = NewChannels},
          {ok, Packet, NewState};
        false -> false
      end
  end.

%% ----------------------------------------------------------------------------
%% @doc Implements the actual round-robin packet scheduler for the interleaved 
%%      RTSP channels. Also formats the interleaved packet for sending.
%% @end. 
%% ----------------------------------------------------------------------------
-spec get_packet_to_send(ChIndex :: integer(), 
                         Channels :: array(),
                         ChCount :: integer(),
                         PollCount :: integer()) -> 
    {ok, ChannelId :: integer(), Data :: binary(), NewChannels :: array()} | 
    false. 

% handles the case when we've tested all available channels 
get_packet_to_send(_, _, ChCount, PollCount) when ChCount == PollCount -> 
  false;

% handles the case 
get_packet_to_send(ChIndex, Channels, ChCount, PollCount) ->
  NextCh = (ChIndex + 1) rem ChCount,
  case array:get(ChIndex, Channels) of
    undefined -> 
      get_packet_to_send(NextCh, Channels, ChCount, PollCount + 1);

    Channel when is_record(Channel, channel) -> 
      case queue:out(Channel#channel.queue) of
        {{value, B}, NewQ} ->
          Size = byte_size(B),
          Data = <<$$:8, ChIndex:8, Size:16/big-integer, B/binary>>,
          NewChannel = Channel#channel{queue = NewQ},
          NewChannels = array:set(ChIndex, NewChannel, Channels),
          {ok, ChIndex, Data, NewChannels};

        {empty, _} ->
          get_packet_to_send(NextCh, Channels, ChCount, PollCount + 1)
      end
  end.

%% ----------------------------------------------------------------------------
%% @doc Handles inbound data, leaving any leftover data in the StateData block
%%      returned in the result tuple.
%%
%% @spec handle_data(StateName, Data, StateData) -> Result
%%       Result = {ok, NewState, NewStateData, Remainder}
%%       NewState = term()
%%       NewStateData = state()
%%
%% @end
%% ----------------------------------------------------------------------------
handle_data(<<$$:8, Channel:8, Size:16/big-unsigned-integer, Data/binary>>, 
            StateData, ready) ->

  if byte_size(Data) >= Size -> 
    % extract the packet from the data stream;
    <<Packet:Size/binary, Remainder/binary>> = Data,
    
    % deliver the packet to whoever wants it
    deliver_packet(StateData, Channel, Packet),

    % do what you have to with the packet data
    handle_data(Remainder, StateData, ready);

    true -> 
      {ok, ready, StateData, Data}
  end;
    
handle_data(Data, StateData, ready) ->
  case rtsp:find_eom(Data) of
    {ok, Message, Remainder} ->             
      % attempt to parse the message and deal with it before going 
      % around again
      case rtsp:parse_message(Message) of
        RtspMsg ->
          case rtsp:message_content_length(RtspMsg) of
            % content length is zero, so we can just pass the message on to the 
            % server without further ado  
            0 -> 
              NewState = dispatch_message(RtspMsg, StateData),
              {ok, ready, NewState, Remainder};
          
            % content length is nonzero, so we need to read the body data. Save
            % the request into the connection state and go around again - but
            % this time in the reading_body state.  
            _ -> 
              NewState = StateData#state{pending_message = RtspMsg},
              handle_data(Remainder, NewState, reading_body)
          end
      end;
      
    notfound ->
      % no End-Of-Message marker is found, so wait for another batch of
      % data to come in and try again  
      {ok, ready, StateData, Data}
  end;

handle_data(Data, State, reading_body) ->
  Msg = State#state.pending_message,
  ContentLength = rtsp:message_content_length(Msg),
  
  {Body, Remainder} = 
    case byte_size(Data) of 
      % if we have enough data in the buffer to satisfy the content length, then 
      % extact the body and return it with the remainder. 
      Len when Len =< ContentLength -> 
        <<A:ContentLength/binary, B/binary>> = Data,
        {A,B};

      _ -> 
        {<< >>, Data}
    end,

  case Body of
    << >> -> {ok, reading_body, State, Data};
    _ -> 
      MsgP = Msg#rtsp_message{body = Body},
      StateP = dispatch_message(MsgP, State#state{pending_message = undefined}),
      handle_data(Remainder, StateP, ready)
  end;

% the generic handle data
handle_data(Data, StateData, _State) ->
  log:trace("rtsp_connection:handle_data/3 - ~w ~w", [StateData,_State]),
  {ok, ready, StateData, Data}.

%% ----------------------------------------------------------------------------
%% @doc Delivers a packet to the registered callback   
%% @end
%% ----------------------------------------------------------------------------
deliver_packet(#state{channels = Channels} = _State, ChannelIndex, Buffer) ->
  case array:get(ChannelIndex, Channels) of
    Channel when is_record(Channel, channel) ->
      case Channel#channel.handler of 
        undefined -> ok;
        Handler -> Handler(Buffer)
      end;

    undefined -> 
      log:warning("Received packet on unregistered channel ~w", [ChannelIndex]);
  
    Err ->
      log:error("Unexpected data: ~w", [Err])
  end.

%% ============================================================================
%% Message handling routines 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Dispatches an RTSP message for handling on a new process. 
%% @end
%% ---------------------------------------------------------------------------- 
-spec dispatch_message(rtsp:message(), #state{}) -> #state{}.
dispatch_message(Msg, State) when is_record(Msg, rtsp_message) ->
  case element(1, Msg#rtsp_message.message) of
    rtsp_request ->
      {Method, Uri, Seq, _, _} = rtsp:get_request_info(Msg),
    
      log:debug("rtsp_connection:dispatch_message/2 - handling #~w ~s ~s",
        [Seq,Method,Uri]),
  
      StateP   = register_pending_request(Seq, Msg, State),
      Me       = self(),
      Callback = State#state.callback, 
      Handler  =
        fun() ->           
          try 
            Callback(Me, Msg)
          catch
            {unauthorised, stale} -> send_auth_response(Me, Seq, [stale]);
            {unauthorised, _} -> send_auth_response(Me, Seq);
            bad_request  -> send_response(Me, Seq, bad_request, [], << >>);
            Err -> send_server_error(Me, Err, Seq)
          end 
        end,
      erlang:spawn(Handler),
      StateP
  end.
  
%% ----------------------------------------------------------------------------
%% @doc Generates an RTSP response for a failure and forwards it to the
%%      supplied RTSP connection for transmission back to the client.
%% @spec send_server_error(Connection, Reason, Sequence) -> ok
%%         Connection = pid()
%%         Reason = atom() | string()
%%         Sequence = int()
%% @end
%% ----------------------------------------------------------------------------
send_server_error(Pid, Reason, Sequence) when is_list(Reason) ->
  log:debug("rtsp_connection:send_server_error/3 - ~w", [Reason]),
  Message = lib_io:format("Error ~w", [Reason]),
  Body = utf:string_to_utf8(Message),
  Headers = [{content_type, "text/plain; charset=utf-8"}],
  send_response(Pid, Sequence, internal_server_error, Headers, Body),
  ok;
  
send_server_error(Pid, Reason, Sequence) ->
  Text = io_lib:format("Internal Server Error: ~w", Reason),
  send_response(Pid, Sequence, Text, [], << >>),
  ok.

%% ----------------------------------------------------------------------------
%% @doc Generates and sends an RTSP 401-unauthorised response and sends it back
%%      to the client.
%% @end
%% ----------------------------------------------------------------------------
send_auth_response(Conn, Seq) ->  send_auth_response(Conn, Seq, []).

send_auth_response(Conn, Seq, Flags) ->
  Headers = rtsp_authentication:get_headers(Conn, "rtsp-server", Flags),
  send_response(Conn, Seq, unauthorised, Headers, << >>).

%% ----------------------------------------------------------------------------
%% @spec build_response_headers(Sequence, ContentLength, Headers) -> Result
%%       Result = rtsp_message_headers()
%%       Headers = [Header]
%%       Header = {Name,Value} | {content_type, string()} | {sequence, int}
%%       Name = string()
%%       Value = string()
%% @end
%% ----------------------------------------------------------------------------
build_response_headers(Sequence, ContentLength, Headers) ->
  ContentType = 
    case lists:keyfind(content_type, 1, Headers) of
      {content_type,CType} when is_list(CType) -> CType;
      _ -> ""
    end,
  
  % filter out any of the headers that we'll be setting explicitly
  FilteredHeaders = lists:filter(
    fun(H) ->
      case H of
        {content_type,_} -> false;
        {sequece,_} -> false;
        _ -> true
      end 
    end, 
    Headers),
  
  % create a dictionary of headers by merging the supplied list and any that we
  % want to explicitly set here.
  ServerHeader = dict:append(?RTSP_HEADER_SERVER, "EMS RTSP Service/0.1", 
    dict:from_list(FilteredHeaders)),
  
  #rtsp_message_header{
    sequence = Sequence, 
    content_length = ContentLength, 
    content_type = ContentType, 
    headers = ServerHeader
  }.  
  
%% ============================================================================
%% Utility functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc registers the a pending request in a list of pending requests
%% @spec register_pending_request(Seq, Request, State) -> NewState
%%         Request = rtsp_request()
%%         State = state()
%% @end
%% ----------------------------------------------------------------------------
register_pending_request(Seq, Request, State) ->
  PendingRequests = State#state.pending_requests,
  State#state{pending_requests = dict:append(Seq, Request, PendingRequests)}.

%% ----------------------------------------------------------------------------
%% @doc Removes a pending request from the list of requests awaiting an
%%      outgoing response, returning a new state record and the dequeued
%%      request
%% @end
%% ----------------------------------------------------------------------------
-spec deregister_pending_request(integer(), #state{}) ->
        {rtsp:message(), #state{}}  | 
        #state{}.

deregister_pending_request(Seq, State) ->
  PendingRequests = State#state.pending_requests,
  case dict:find(Seq, PendingRequests) of 
    {ok, [Request]} -> 
      NewDict = dict:erase(Seq, PendingRequests),
      {Request, State#state{ pending_requests=NewDict }};
      
    _ -> State
  end.

%% ----------------------------------------------------------------------------
%% @doc Recursively registers a new channel on the connection, failing if 
%%      the requested channel is already in use. 
%% @end
%% ----------------------------------------------------------------------------
register_channels(Conn, Channels, State) -> 
  register_channels(Conn, Channels, State, []).

register_channels(_Conn, [], State, NewChannels) -> 
  {ok, State, lists:reverse(NewChannels)};

register_channels(Conn, [Channel|Rest], State, NewChannels) ->
  {Index, Callback} = Channel,
  Channels = State#state.channels,
  case Index of 
    n when n > 255 -> {error, "Invalid channel index"};
    _ -> 
      case array:get(Index, Channels) of
        % Undefined means that the channel is free. Who-hoo, we can use it.
        undefined -> 
          NewChannel = #channel{handler = Callback},
          Handle = {Conn, Index},
          NewState = State#state{channels = 
                                   array:set(Index, NewChannel, Channels)},
          register_channels(Conn, Rest, NewState, [Handle | NewChannels]);

        % anythng apart from "undefined" means the channel is in use. This is an 
        % error
        _ -> 
          {error, "Channel index in use"}
      end
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------

-spec set_channel_handler(Index :: integer(),
                          Handler :: buffer_handler(),
                          State :: #state{} ) ->
  {ok, NewState :: #state{}} | 
  {error, Reason :: invalid_channel_index | no_such_channel}.

set_channel_handler(Index, _Handler, _State) when Index > 255 ->
  {error, invalid_channel_index};

set_channel_handler(Index, _Handler, _State) when Index < 0 ->
  {error, invalid_channel_index};

set_channel_handler(Index, Handler, State) ->
  Channels = State#state.channels,
  case array:get(Index, Channels) of 
    undefined -> {error, no_such_channel};
    Channel ->
      ChannelP = Channel#channel{handler = Handler},
      ChannelsP = array:set(Index, ChannelP, Channels),
      StateP = State#state{channels = ChannelsP},
      {ok, StateP}
  end.

%% ----------------------------------------------------------------------------
%% @doc Attempts to authenticate a request and executes an action if the 
%%      authentication succeeds.
%%
%%      If no authenitcation header is present in the request, the action will
%%      still be executed on the atom 'anonymous', rather than a user info 
%%      record.
%%      
%% @throws bad_request |
%%         {unauthorised, missing_header} | 
%%         {unauthorised, auth_failed} | 
%%         {unauthorised, stale} 
%% @end
%% ----------------------------------------------------------------------------
-spec with_authenticated_user_do(rtsp:conn(),
                                 rtsp:message(),
                                 rtsp:user_info_callback(),
                                 rtsp:authenticated_action()) -> 
                                 'ok' | no_return().
with_authenticated_user_do(Conn, Rq, PwdCb, Action) ->
  case rtsp:get_message_header(?RTSP_HEADER_AUTHORISATION, Rq) of

    % There's no authentication header. Execute the action on the "anonymous"
    % user
    undefined ->
      Action(anonymous);

    % there *is* an authentication header - use it to try and authenticate the 
    % user 
    [AuthHeader|_] ->
      AuthInfo = case rtsp_authentication:parse(AuthHeader) of
                   {ok, I} -> I;
                   _ -> throw(bad_request)
                 end,
      UserName = rtsp_authentication:get_user_name(AuthInfo),
      log:debug("rtsp_connection - authenticating user \"~s\"", [UserName]),
      
      % ask whoever called us what the password for this user is
      case PwdCb(UserName) of 
        false -> 
          log:debug("rtsp_connection - no such user \"~s\"", [UserName]),
          throw({unauthorised, no_such_user});

        {ok, UserInfo} ->
          case rtsp_authentication:validate(Conn, Rq, AuthInfo, UserInfo) of
            ok ->
              log:debug("rtsp_connection - authenticated user \"~s\"", [UserName]),
              Action(UserInfo),
              ok;

            fail ->
              throw({unauthorised, auth_failed});

            stale ->
              throw({unauthorised, stale})
          end
      end
  end.

%% ============================================================================
%% RTSP Sender implementation
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the writer process for this connection
%% @end
%% ----------------------------------------------------------------------------
-spec start_sender(pid(), inet:socket()) -> pid().
start_sender(ConnectionPid, Socket) ->
  spawn_link(?MODULE, sender_loop, [ConnectionPid, Socket]).

%% ----------------------------------------------------------------------------
%% @doc Signals the sender process to exit and waits for it to acknowledge that 
%%      it is going down. Note that you can only stop the sender process if
%%      you're the process that started it.
%% @spec stop_sender(SenderPid) -> ok | timed_out
%% @end
%% ----------------------------------------------------------------------------
stop_sender(SenderPid) ->
  SenderPid ! {exit_sender, self()},
  receive 
    sender_exited -> ok
  after
    1000 -> timed_out
  end.

%% ----------------------------------------------------------------------------
%% @doc Implements the writing loop for an rtsp conection. Sits in a receive 
%%      loop and send any data it recieves until it receives the signal to 
%%      quit.
%% @end
%% ----------------------------------------------------------------------------
sender_loop(Conn, Socket) ->
  Conn ! {sender_waiting, self()},
  receive
    {send_item, Sender, Data} when is_binary(Data) and (Sender == Conn) ->
      gen_tcp:send(Socket, Data),
      sender_loop(Conn, Socket);
      
    {exit_sender, Sender} when Sender == Conn ->
      Conn ! sender_exited,
      ok;

    _Other ->
      sender_loop(Conn, Socket)
  end.

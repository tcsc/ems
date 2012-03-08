-module(rtsp_connection).
-behaviour(gen_fsm).

-include("rtsp.hrl").

%% ============================================================================
%% Public API
%% ============================================================================
-export([new/3, close/1, take_socket/2, get_client_address/1, send_response/5, 
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

%% ----------------------------------------------------------------------------
%% @doc The state record for an RTSP connection.
%% @end
%% ----------------------------------------------------------------------------

-record(state, { server_str        :: string(),
                 socket            :: inet:socket(),
                 sender            :: pid(),
                 pending_message   :: rtsp:message(),
                 pending_requests  :: dict(),
                 pending_data      :: binary(),
                 callback          :: rtsp:request_callback()
               }).
-type state() :: #state{}.

-define(SP,16#20).


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
                  callback         = Callback
                },
  gen_fsm:start_link(?MODULE, State, []).

close(Conn) ->
  gen_fsm:sync_send_all_state_event(Conn, quit).

take_socket(Conn, Socket) ->
  log:debug("rtsp_connection:take_socket/2 - reassigning socket ownership to ~w", [Conn]),
  ok = gen_tcp:controlling_process(Socket, Conn),
  
  log:debug("rtsp_connection:take_socket/2 - forwarding socket to connection"),
  gen_fsm:send_event(Conn, {socket, Socket}),
  ok.

get_client_address(Conn) ->
  gen_fsm:sync_send_all_state_event(Conn, get_client_address).

%% ============================================================================
%% gen_fsm API
%% ============================================================================
init(State) -> 
  log:debug("rtsp_connection:init/1", []),
  {ok, waiting_for_socket, State}.

%% ----------------------------------------------------------------------------
%% @spec handle_event(Event, StateName, StateData) -> Result
%%       Result = {next_state,NextStateName,NewStateData} |
%%                {next_state,NextStateName,NewStateData,Timeout} |
%%                {next_state,NextStateName,NewStateData,hibernate} | 
%%                {stop,Reason,NewStateData}
%% @end
%% ----------------------------------------------------------------------------
handle_event({send_response, Sequence, Status, ExtraHeaders, Body}, StateName, State) ->
  log:debug("rtsp_connection:handle_info/3 - send_response to request ~w (~w)", 
    [Sequence, Status]),

  StateP = case deregister_pending_request(Sequence, State) of 
    {Msg, S} -> Rq = Msg#rtsp_message.message,
                RtspVersion = Rq#rtsp_request.version,
                AllHeaders = build_response_headers(Sequence, size(Body), ExtraHeaders),
                Response = #rtsp_response{status = Status, version = RtspVersion}, 
                Bytes = rtsp:format_message(Response, AllHeaders, Body),
                send_data(S, Bytes),
                S;
    _ -> State
  end,
  {next_state, StateName, StateP};

handle_event(_Event, StateName, StateData) -> 
  log:debug("rtsp_connection:handle_event/3 - ~w", [StateName]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%% @doc Handles process events
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, 
            State, 
            StateData = #state{pending_data = PendingData}) ->
  log:trace("rtsp_connection:handle_info/3"),

  % combine the newly-arrived data with the stuff leftover from the last reqest
  AccumulatedData = list_to_binary([PendingData, Data]),
  
  % process what we can of the data and get whatever's left back
  {ok, NewState, NewStateData, Leftovers} = 
    handle_data(AccumulatedData, StateData, State),
    
  % update the connection state with the leftovers from the processing
  NewNewStateData = 
    NewStateData#state{pending_data=Leftovers},
    
  % reset the socket so that it pings us again when more data arrives
  inet:setopts(Socket, [{active,once}]),
  {next_state, NewState, NewNewStateData};

handle_info({tcp_closed, _Socket}, _StateName, State) ->
  log:debug("rtsp_connection:handle_info/3 - tcp connection closed",[]),
  {stop, normal, State};

handle_info({sender_waiting, _SendingPid}, StateName, StateData) ->
  log:debug("rtsp_connection:handle_info/2 - sender waiting"),
  {next_state, StateName, StateData};

handle_info(Info, StateName, StateData) ->
  log:debug("rtsp_connection:handle_info/3 ~w, ~w, ~w", [Info, StateName, StateData]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_sync_event(get_client_address, _From, StateName, State) ->
  log:debug("rtsp_connection:handle_sync_event/4"),
  {ok, {Host, _}} = inet:sockname(State#state.socket),
  {reply, Host, StateName, State};
  
handle_sync_event(quit, _, _, State) ->
  log:debug("rtsp_connection:handle_sync_event/4 - quit"),
  {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, StateData) -> 
  log:debug("rtsp_connection:handle_sync_event/4 - ~w", [_Event]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
  log:debug("rtsp_connection:terminate/3 - ~w", [_Reason]),
  Sender = State#state.sender,
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
handle_data(<<$$:8, _Channel:8, Size:16/big, Data/binary>>, StateData, ready) ->
  if size(Data) >= Size -> 
    % extract the packet from the data stream;
    <<_Packet:Size/binary, Remainder/binary>> = Data,
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
            % content length is zero, so we can just pass the message on to the server
            % without further ado  
            0 -> 
              NewState = dispatch_message(RtspMsg, StateData),
              {ok, ready, NewState, Remainder};
          
            % content length is nonzero, so we need to read the body data. Save the
            % request into the connection state and go around again - but this time
            % in the reading_body state.  
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
    case size(Data) of 
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

%% ============================================================================
%% Message handling routines 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc
%% @end
%% ---------------------------------------------------------------------------- 
-spec dispatch_message(rtsp:message(), state()) -> state().
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
            {unauthorized, stale} -> send_auth_response(Me, Seq, [stale]);
            {unauthorized, _} -> send_auth_response(Me, Seq);
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
%% @doc Generates and sends an RTSP 401-unauthorized response and sends it back
%%      to the client.
%% @end
%% ----------------------------------------------------------------------------
send_auth_response(Conn, Seq) ->  send_auth_response(Conn, Seq, []).

send_auth_response(Conn, Seq, Flags) ->
  Headers = rtsp_authentication:get_headers(Conn, "rtsp-server", Flags),
  send_response(Conn, Seq, unauthorized, Headers, << >>).

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
  ContentType = case lists:keyfind(content_type, 1, Headers) of
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
-spec deregister_pending_request(integer(),state()) -> {rtsp:message(),state()} 
                                                       | state().
deregister_pending_request(Seq, State) ->
  PendingRequests = State#state.pending_requests,
  case dict:find(Seq, PendingRequests) of 
    {ok, [Request]} -> 
      NewDict = dict:erase(Seq, PendingRequests),
      {Request, State#state{ pending_requests=NewDict }};
      
    _ -> State
  end.

%% ----------------------------------------------------------------------------
%% @doc Attempts to authenticate a request and executes an action if and only
%%      if the athentication succeeds.
%% @throws bad_request | unauthorized | stale
%% @end
%% ----------------------------------------------------------------------------
-spec with_authenticated_user_do(rtsp:conn(),
                                 rtsp:message(),
                                 rtsp:user_info_callback(),
                                 rtsp:authenticated_action()) -> 
                                 'ok' | no_return().
with_authenticated_user_do(Conn, Request, PwdCallback, Action) ->
  case rtsp:get_message_header(Request, ?RTSP_HEADER_AUTHORISATION) of
    undefined -> throw({unauthorized, missing_header});  

    [AuthHeader|_] -> 
      AuthInfo = case rtsp_authentication:parse(AuthHeader) of
                   {ok, I} -> I;
                   _ -> throw(bad_request)
                 end,
      UserName = rtsp_authentication:get_user_name(AuthInfo),
      
      log:debug("rtsp_connection:with_authenticated_user_do/4 - authenticating user \"~s\"", 
        [UserName]),
      case PwdCallback(UserName) of
        false ->
          log:debug("rtsp_connection:with_authenticated_user_do/4 - no such user \"~s\"", [UserName]), 
          throw({unauthorized, no_such_user});
          
        {ok, UserInfo} -> 
          case rtsp_authentication:validate(Conn, Request, AuthInfo, UserInfo) of
            ok ->
              log:debug("rtsp_connection:with_authenticated_user_do/4 - authenticated", []),
              Action(UserInfo), 
              ok;
              
            fail -> throw({unauthorized, auth_failed});
            stale -> throw({unauthorized, stale})
          end
      end
  end.

%% ============================================================================
%% RTSP Sender implementation
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Enqueues data with the sender process for transmission to the client
%% @end
%% ----------------------------------------------------------------------------
send_data(_State = #state{sender=SenderPid}, Data) ->
  SenderPid ! {rtsp_send, self(), Data},
  ok.
  
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
    {sender_exited, ok} -> ok
  after
    1000 -> timed_out
  end.

%% ----------------------------------------------------------------------------
%% @doc Implements the writing loop for an rtsp conection. Sits in a receive 
%%      loop and send any data it recieves until it receives the signal to 
%%      quit.
%% @end
%% ----------------------------------------------------------------------------
sender_loop(ConnectionPid, Socket) ->
  receive
    {rtsp_send, _Sender, Data} when is_binary(Data)->
      gen_tcp:send(Socket,Data),
      ConnectionPid ! {sender_waiting, self()},
      sender_loop(ConnectionPid, Socket);
      
    {exit_sender, Sender} when Sender == ConnectionPid ->
      Sender ! {sender_exited, ok},
      ok
   end.

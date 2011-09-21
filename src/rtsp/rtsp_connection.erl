-module(rtsp_connection).
-behaviour(gen_fsm).

-include("erlang_media_server.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% exports
%% ============================================================================
-export([new/3, take_socket/2, get_client_address/1]).

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
  ready/2, 
  send_response/5, 
  send_server_error/3]).

%% ============================================================================
%% Internal exports
%% ============================================================================
-export([handle_data/3]).
-export([sender_loop/2]).

%% ----------------------------------------------------------------------------
%% @doc The state record for an RTSP connection.
%% @end
%% ----------------------------------------------------------------------------
-type conn() :: pid().
-type_export([conn/0]).

-record(state, { server_str        :: string(),
                 socket            :: inet:socket(),
								 sender            :: pid(),
                 pending_message,
                 pending_requests,
                 pending_data      :: binary(),
								 config_handle     :: ems_config:handle()
               }).

-define(SP,16#20).

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------
-spec new(rtsp_svr:svr(), string(), ems_config:handle()) -> {'ok', conn()} | {'error', any()}.
new(_Owner, ServerStr, ConfigHandle) -> 
  ?LOG_DEBUG("rtsp_connection:new/1", []),
  State = #state{ server_str       = ServerStr,
									config_handle    = ConfigHandle,
                  pending_data     = << >>,
                  pending_requests = dict:new()
                },
  gen_fsm:start_link(?MODULE, State, []).

take_socket(Conn, Socket) ->
	?LOG_DEBUG("rtsp_connection:take_socket/2 - reassigning socket ownership to ~w", [Conn]),
	ok = gen_tcp:controlling_process(Socket, Conn),
	
	?LOG_DEBUG("rtsp_connection:take_socket/2- forwarding socket to connection", []),
	gen_fsm:send_event(Conn, {socket, Socket}),
	ok.

%% ---------------------------------------------------------------------------- 
%%
%% ---------------------------------------------------------------------------- 

get_config_handle(Conn) -> 
	gen_fsm:sync_send_all_state_event(Conn, get_config_handle).
  
get_client_address(Conn) ->
	gen_fsm:sync_send_all_state_event(Conn, get_client_address).

%% ---------------------------------------------------------------------------- 
%% 
%% ----------------------------------------------------------------------------
init(State) -> 
  ?LOG_DEBUG("rtsp_connection:init/1", []),
  {ok, waiting_for_socket, State}.

%% ----------------------------------------------------------------------------
%% @spec handle_event(Event, StateName, StateData) -> Result
%%       Result = {next_state,NextStateName,NewStateData} |
%%                {next_state,NextStateName,NewStateData,Timeout} |
%%                {next_state,NextStateName,NewStateData,hibernate} | 
%%                {stop,Reason,NewStateData}
%% ----------------------------------------------------------------------------
handle_event({send_response, Sequence, Status, ExtraHeaders, Body}, StateName, State) ->
	?LOG_DEBUG("rtsp_connection:handle_info/3 - send_response to request ~w (~w)", 
		[Sequence, Status]),

	StateP = case deregister_pending_request(Sequence, State) of 
		{Rq, S} -> RtspVersion = Rq#rtsp_request.version,
	             AllHeaders = build_response_headers(Sequence, size(Body), ExtraHeaders),
	             Response = #rtsp_response{status = Status, version = RtspVersion}, 
	             Bytes = rtsp:format_message(Response, AllHeaders, Body),
	             send_data(S, Bytes);
	    _ -> State
  end,
	{next_state, StateName, StateP};

handle_event(_Event, StateName, StateData) -> 
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%% @doc Handles process events
%% @spec
%% @end
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, 
            State, 
            StateData = #state{pending_data = PendingData}) ->
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
  ?LOG_DEBUG("rtsp_connection:handle_info/3 - tcp connection closed",[]),
  {stop, normal, State};

handle_info({sender_waiting, _SendingPid}, StateName, StateData) ->
  {next_state, StateName, StateData};

handle_info(Info, StateName, StateData) ->
  ?LOG_DEBUG("rtsp_connection:handle_info/3 ~w, ~w, ~w", [Info, StateName, StateData]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_sync_event(get_client_address, _From, StateName, State) ->
  {ok, {Host, _}} = inet:sockname(State#state.socket),
  {reply, Host, StateName, State};

handle_sync_event(get_config_handle, _From, StateName, State) ->
	Config = State#state.config_handle,
	{reply, Config, StateName, State};

handle_sync_event(_Event, _From, StateName, StateData) -> 
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) -> 
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
-spec send_response(conn(), integer(), any(), [any()], binary()) -> any().
send_response(Conn, Sequence, Status, ExtraHeaders, Body) -> 
	Event = {send_response, Sequence, Status, ExtraHeaders, Body},
	gen_fsm:send_all_state_event(Conn, Event).

%% ============================================================================
%% State callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec waiting_for_socket(Event,SenderPid,State) -> 
%% ----------------------------------------------------------------------------
waiting_for_socket({socket, Socket}, State) -> 
  ?LOG_DEBUG("rtsp_connection:wating_for_socket/2 - got socket", []),
  inet:setopts(Socket, [{active,once}]),
  Sender = start_sender(self(), Socket),
  NewState = State#state{socket=Socket, sender=Sender},
  {next_state, ready, NewState};
  
waiting_for_socket(Message, State) -> 
  ?LOG_DEBUG("rtsp_connection:wating_for_socket/2 - ~w, ~w", [Message, State]),
  {next_state, waiting_for_socket, State}.

%% ----------------------------------------------------------------------------
%% @spec ready() -> {next_state,NewStateName,NewStateData}
%% @end
%% ----------------------------------------------------------------------------
ready(Event, State) ->
  ?LOG_DEBUG("rtsp_connection:ready/2 - ~w, ~w", [Event, State]),
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
      {_MessageType, RtspMessage, Headers} = rtsp:parse_message(Message),
      case Headers#rtsp_message_header.content_length of
        % content length is zero, so we can just pass the message on to the server
        % without further ado  
        0 -> 
          NewState = dispatch_message(RtspMessage, Headers, << >>, StateData),
          {ok, ready, NewState, Remainder};
          
        % content length is nonzero, so we need to read the body data. Save the
        % request into the connection state and go around again - but this time
        % in the reading_body state.  
        _ -> 
          NewState = StateData#state{
            pending_message={RtspMessage, Headers}},
          handle_data(Remainder, NewState, reading_body)
      end;
      
    notfound ->
      % no End-Of-Message marker is found, so wait for another batch of
      % data to come in and try again  
      {ok, ready, StateData, Data}
  end;

handle_data(Data, StateData, reading_body) ->
  {Message,Header} = StateData#state.pending_message,
  ContentLength = Header#rtsp_message_header.content_length,
  
  DataSize = size(Data),
  
  if 
    ContentLength =< DataSize ->
      if  
        ContentLength < size(Data) ->
          {Body, Remainder} = split_binary(Data, ContentLength);
  
        ContentLength =:= size(Data) ->
          {Body, Remainder} = {Data, << >>}
      end,

      NewState = dispatch_message(Message, Header, Body, 
        StateData#state{pending_message = undefined}),
      handle_data(Remainder, NewState, ready);
    
    true ->  {ok, reading_body, StateData, Data}
  end;

% the generic handle data
handle_data(Data, StateData, _State) ->
  {ok, ready, StateData, Data}.

%% ============================================================================
%% Message handling routines 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec dispatch_message(Message, Headers, Body, State) -> NewState
%%       Message = Request | Response
%%       Headers = rtsp_message_header()
%% @end
%% ---------------------------------------------------------------------------- 
dispatch_message(Request,Headers,Body,State) when is_record(Request,rtsp_request) ->
  {Method, Uri, Sequence, _, _} = rtsp:get_request_info(Request, Headers),
    
  ?LOG_DEBUG("rtsp_connection:dispatch_message/4 - handling #~w ~s ~s",
    [Sequence,Method,Uri]),
  
  StateP = register_pending_request(Sequence, Request, State),
	Me = self(),
	Handler = fun() -> 
		          try 
								handle_request(Me, Method, Sequence, Request, Headers, Body)
							catch
								Err -> send_server_error(Me, Err, Sequence)
							end
						end,
	erlang:spawn(Handler),
	StateP.
	
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
  Message = lib_io:format("Error ~w", [Reason]),
  Body = utf:string_to_utf8(Message),
  Headers = [{content_type, "text/plain; charset=utf-8"}],
  send_response(Pid, Sequence, internal_server_error, Headers, Body),
  ok;
  
send_server_error(Pid, Reason, Sequence) ->
  send_response(Pid, Sequence, Reason, [], << >>),
  ok.

%% ----------------------------------------------------------------------------
%% @spec build_response_headers(Sequence, ContentLength, Headers) -> Result
%%       Result = rtsp_message_headers()
%%       Headers = [Header]
%%       Header = {Name,Value} | {content_type, string()} | {sequence, int}
%%       Name = string()
%%       Value = string()
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
  
%% ----------------------------------------------------------------------------
%% @spec handle_request(Method, Request,Headers,Body,State) -> Result
%%       Method = options | accounce | setup | play | teardown
%% @end
%% ----------------------------------------------------------------------------  
handle_request(Conn, options, Sequence, _, _, _) ->
  PublicOptions = [?RTSP_METHOD_ANNOUNCE,
                   ?RTSP_METHOD_DESCRIBE,
                   ?RTSP_METHOD_SETUP,
                   ?RTSP_METHOD_PLAY,
                   ?RTSP_METHOD_PAUSE,
                   ?RTSP_METHOD_TEARDOWN,
                   ?RTSP_METHOD_RECORD],
  Headers = [{"Public", string:join(PublicOptions, ", ")}],
  send_response(Conn, Sequence, ok, Headers, << >>);

handle_request(Conn, describe, Sequence, _Request, _Headers, _Body) -> 
	_Config = get_config_handle(Conn),
	send_response(Conn, Sequence, not_implemented, [], << >>);

%handle_request(Conn, describe, Request, Headers, Body) -> 
%	Uri = Request#rtsp_request.uri,
%	{_,_,_,Path} = url:parse(Uri),
%	Config = get_config_handle(Conn),
%	MountPoint = case ems_config:get_mount_point(Config, Path) of 

handle_request(Conn, _Method, Sequence, _Request, _Headers, _Body) ->
  send_response(Conn, Sequence, not_implemented, [], << >>).

%%with_authenticated_user_do() ->
	
%% ============================================================================
%% Utilility functions
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
%% @spec deregister_pending_request(Seq, State) -> Result
%%         Seq = int()
%%         State = state()
%%         Result = NewState | {Request, NewState}
%% @end
%% ----------------------------------------------------------------------------
deregister_pending_request(Seq, State) ->
  PendingRequests = State#state.pending_requests,
  case dict:find(Seq, PendingRequests) of 
    {ok, [Request]} -> 
      NewDict = dict:erase(Seq, PendingRequests),
      {Request, State#state{ pending_requests=NewDict }};
      
    _ -> State
  end.

%% ============================================================================
%% RTSP Sender implementation
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc 
%% @spec send_data(State, Data) -> Result
%%       Result = ok | {error, Reason}
%% @end
%% ----------------------------------------------------------------------------
send_data(_State = #state{sender=SenderPid}, Data) ->
  SenderPid ! {rtsp_send, self(), Data},
  ok.
  
%% ----------------------------------------------------------------------------
%% @doc Starts the writer process for this connection
%% @spec start_sender(ConnectionPid) -> SenderProcessPid
%% @end
%% ----------------------------------------------------------------------------
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
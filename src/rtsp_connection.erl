-module(rtsp_connection).
-behaviour(gen_fsm).

-include("erlang_media_server.hrl").
-include("rtsp.hrl").

%% ============================================================================
%% exports
%% ============================================================================
-export([start_link/1]).

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
-export([waiting_for_socket/2, ready/2]).

%% ============================================================================
%% Internal exports
%% ============================================================================
-export([handle_data/3]).
-export([sender_loop/2]).

%% ----------------------------------------------------------------------------
%% @doc The state record for an RTSP connection.
%% @end
%% ----------------------------------------------------------------------------

-record(rtsp_connection_state, {
  server_pid,
  socket,
  sender,
  pending_message,
  pending_requests,
  pending_data
  }).

-define(SP,16#20).

%% ----------------------------------------------------------------------------
%% @spec start_link(State) -> {ok,Pid} | ignore | {error,Reason}
%%       Reason = {already_started,Pid} | term()
%% ----------------------------------------------------------------------------
start_link(ServerPid) -> 
  ?LOG_DEBUG("rtsp_connection:start_link/1", []),
  State = #rtsp_connection_state{
    server_pid = ServerPid, 
    pending_data = << >>,
    pending_requests = dict:new()},
  gen_fsm:start_link(?MODULE, State, []).

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
handle_event(_Event, StateName, StateData) -> 
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%% @doc Handles process events
%% @spec
%% @end
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, 
            State, 
            StateData = #rtsp_connection_state{pending_data = PendingData}) ->
  % combine the newly-arrived data with the stuff leftover from the last reqest
  AccumulatedData = list_to_binary([PendingData, Data]),
  
  % process what we can of the data and get whatever's left back
  {ok, NewState, NewStateData, Leftovers} = 
    handle_data(State, AccumulatedData, StateData),
    
  % update the connection state with the leftovers from the processing
  NewNewStateData = 
    NewStateData#rtsp_connection_state{pending_data=Leftovers},
    
  % reset the socket so that it pings us again when more data arrives
  inet:setopts(Socket, [{active,once}]),
  {next_state, NewState, NewNewStateData};

handle_info({tcp_closed, Socket}, StateName, State) ->
  ?LOG_DEBUG("rtsp_connection:handle_info/3 - tcp connection closed",[]),
  {stop, normal, State};

handle_info({sender_waiting, SendingPid}, StateName, StateData) ->
  {next_state, StateName, StateData};

handle_info(Info, StateName, StateData) ->
  ?LOG_DEBUG("rtsp_connection:handle_info/3 ~w, ~w, ~w", [Info, StateName, StateData]),
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) -> 
  {next_state, StateName, StateData}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
terminate(Reason, StateName, StateData) -> 
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) -> 
  {ok, StateName, StateData}.
  
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
  NewState = State#rtsp_connection_state{socket=Socket, sender=Sender},
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
%%       NewStateData = rtsp_connection_state()
%%
%% @end
%% ----------------------------------------------------------------------------
handle_data(ready, <<$$,Channel:8,Size:16/big,Data/binary>>, StateData) ->
  if size(Data) >= Size -> 
    % extract the packet from the data stream;
    <<Packet:Size/binary, Remainder/binary>> = Data,
      % do what you have to with the packet data
      handle_data(ready, Remainder, StateData);
    true -> 
      {ok, ready, StateData, Data}
  end;
    
handle_data(ready, Data, StateData) ->
  case rtsp:find_eom(Data) of
    {ok, Message, Remainder} ->             
      % attempt to parse the message and deal with it before going 
      % around again
      {MessageType, RtspMessage, Headers} = rtsp:parse_message(Message),
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
          NewState = StateData#rtsp_connection_state{
            pending_message={RtspMessage, Headers}},
          handle_data(reading_body, Remainder, NewState)
      end;
      
    notfound ->
      % no End-Of-Message marker is found, so wait for another batch of
      % data to come in and try again  
      {ok, ready, StateData, Data}
  end;

handle_data(reading_body, Data, StateData) ->
  {Message,Header} = StateData#rtsp_connection_state.pending_message,
  ContentLength = Header#rtsp_message_header.content_length,
  
  DataSize = size(Data),
  
  if 
    ContentLength =< DataSize ->
      if  
        ContentLength < size(Data) ->
          {Body, Remainder} = split_binary(ContentLength,Data);
  
        ContentLength =:= size(Data) ->
          {Body, Remainder} = {Data, << >>}
      end,

      NewState = dispatch_message(Message, Header, Body, 
        StateData#rtsp_connection_state{pending_message = undefined}),
      handle_data(ready, Remainder, NewState);
    
    true ->  {ok, reading_body, StateData, Data}
  end;

% the generic handle data
handle_data(State, Data, StateData) ->
  NewState = ready,
  NewStateData = StateData,
  {ok, NewState, NewStateData, Data}.

%% ============================================================================
%% Message handling routines 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec dispatch_message(Message,Headers,Body) -> Result
%%       Message = Request | Response
%%       Result = State
%% ---------------------------------------------------------------------------- 
dispatch_message(Request,Headers,Body,State) when is_record(Request,rtsp_request) ->
  {Uri, Sequence, _, _} = get_request_info(Request, Headers),
  Method = Request#rtsp_request.method,
    
  ?LOG_DEBUG("rtsp_connection:dispatch_message/4 - handling #~w ~s ~s",
    [Sequence,Method,Uri]),
  
  try
    {StatusCode, ResponseHeaders, ResponseBody, NewState} = case Method of
      "OPTIONS" -> handle_options(Request,Headers,Body,State);
      "ANNOUNCE" -> handle_announce(Request,Headers,Body,State);
      "SETUP" -> handle_setup(Request,Headers,Body,State);
      _ -> handle_unknown_request(Request,Headers,Body,State)
    end,
    
    FullResponseHeader = build_response_headers(Sequence, 
      size(ResponseBody), ResponseHeaders),

    Response = #rtsp_response{status = ?RTSP_STATUS_OK,
                              version = Request#rtsp_request.version},
    
    send_response(Response, FullResponseHeader, ResponseBody, NewState)
  catch
    {rtsp_error, Reason} ->
      server_error(Reason, Sequence, State);
      
    Any -> 
      server_error(Any, Sequence, State)
  end;
  
dispatch_message(Response,Headers,Body,State) when is_record(Response,rtsp_response) ->
  ?LOG_DEBUG("rtsp_connection:dispatch_message/4 - sending message to server", []),
  ServerPid = State#rtsp_connection_state.server_pid,
  gen_server:cast(ServerPid,{response,self(),Response,Headers,State}),
  State.

%% ----------------------------------------------------------------------------
%% @doc Formats an RTSP response message and sends it to the client
%% @spec send_response(Response,Headers,Body,State) -> NewState
%% @end
%% ----------------------------------------------------------------------------
send_response(Response,Headers,Body,State) ->
  FormattedResponse = rtsp:format_message(Response,Headers,Body),
  send_data(State,FormattedResponse),
  State.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
server_error(Reason, Sequence, State) when is_list(Reason)->
  Message = lib_io:format("Error ~w", [Reason]),
  Body = utf:to_utf8(Message),
  Headers = build_response_headers(Sequence, length(Message), 
    [{content_type, "text/plain; charset=utf-8"}]),
  Response = #rtsp_response{
    status = rtsp:translate(internal_server_error),
    version = {1,1}},
  send_response(Response,Headers,Body,State),
  State;
  
server_error(Reason, Sequence, State) ->
  Headers = build_response_headers(Sequence, 0, []),
  Response = #rtsp_response{
      status = rtsp:translate_status(Reason),
      version = {1,1}},
  send_response(Response, Headers, << >>, State),
  State.  

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
%% @spec handle_request(Request,Headers,Body,State) -> Result
%%       Result = {Response,ResponseHeaders,ResponseBody,NewState}
%% ----------------------------------------------------------------------------  
handle_options(Request, Headers, Body, State) ->
  PublicOptions = [?RTSP_METHOD_DESCRIBE, 
                   ?RTSP_METHOD_SETUP,
                   ?RTSP_METHOD_PLAY,
                   ?RTSP_METHOD_TEARDOWN],                   
  {ok, [{"Public", PublicOptions}], << >>, State}.

%% ----------------------------------------------------------------------------
%% @doc Processes an RTSP ANNOUNCE request
%% @spec handle_anounce(Request, Headers, Body, State) -> Result
%%       Result = {Response, ResponseHeaders, ResponseBody, NewState}
%% @end
%% ----------------------------------------------------------------------------
handle_announce(Request, Headers, Body, State) ->
  {Uri, _, ContentLength, ContentType} = get_request_info(Request, Headers),
  {_,_,_,Path} = url:parse(Uri),
  
  if
    ContentType /= "application/sdp" -> 
      throw({rtsp_error, unsupported_media_type});
    
    ContentLength =:= 0 -> 
      throw({rtsp_error, length_required});
      
    true -> ok
  end,
  
  SessionDescription = sdp:parse(Body),

  case ems_session_manager:create_session(Path,SessionDescription) of
    {error, timeout} -> throw({rtsp_error, server_unavailable});
    {error, Reason}  -> throw({rtsp_error, internal_server_error});
    _ -> ok
  end,
  
  {ok, [], << >>, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles an RTSP SETUP request
%% @spec
%% @end
%% ----------------------------------------------------------------------------
handle_setup(Request, Headers, Body, State) ->
  {Uri,_,_,_} = get_request_info(Request, Headers),
  {_,_,_,Path} = url:parse(Uri),
  
  case find_session(Path) of
    {SessionPid, SessionPath} ->
      StreamName = string:substr(Path, length(SessionPath)+2),
      
      ?LOG_DEBUG("rtsp_connection:handle_setup/4 - stream ~s on session ~s", 
        [StreamName, SessionPath]),
      
      % look for the client's transport header 
      case rtsp:get_header(Headers, ?RTSP_TRANSPORT) of
        ClientHeader when is_list(ClientHeader) ->
          % ok - we have a transport header, so parse it and fling the parsed 
          % transport spec off to the session for processing
          ClientTransport = rtsp:parse_transport(ClientHeader),      
          case ems_session:setup_stream(SessionPid, StreamName, ClientTransport) of

            ServerTransport when is_list(ServerTransport) ->
              % right - the session liked the transport and has set up the 
              % stream for us. Now we need to format the server-side transport
              % spec and send it back to the client in the RTSP response  
              ServerHeader = rtsp:format_transport(ServerTransport),
              {ok, [{?RTSP_TRANSPORT, ServerHeader}], << >>, State};
              
            {error, Reason} ->
              % The session didn't like what we gave it, or couldn't do what we 
              % asked at this time. Map the session error reason to an RTSP 
              % status code and bail.
              throw({rtsp_error,session_error_to_rtsp_status(Reason)})
          end;
        
        false -> 
          % No transport header in the setup request. That's very bad, and the 
          % client should be punished.
          throw({rtsp_error,bad_request})
      end,
      ok;
      
    false -> 
      throw({rtsp_error,not_found})
  end.
    
%% ----------------------------------------------------------------------------
%% @doc The default case - returns "not implemented" regardless of the method 
%%      or URI
%% ----------------------------------------------------------------------------
handle_unknown_request(Request, Headers, Body, State) ->
  {not_implemented, [], << >>, State}.    

%% ============================================================================
%% Utilility functions
%% ============================================================================
get_request_info(Request, Headers) ->
  Uri = Request#rtsp_request.uri,
  Sequence = Headers#rtsp_message_header.sequence,
  ContentLength = Headers#rtsp_message_header.content_length,
  ContentType = Headers#rtsp_message_header.content_type,
  {Uri, Sequence, ContentLength, ContentType}.
  
%% ----------------------------------------------------------------------------
%% @doc Finds a session by recursively trying each sub path - i.e. first it 
%%      tries the full path, then (if no session is found on the full path) it 
%%      tries the path minus everything after the last separator, and so on.  
%% @spec find_session(Path) -> Result
%%       Result = false | {SessionPid, SessionPath}
%% @end
%% ----------------------------------------------------------------------------
find_session([$/]) -> 
  false;

find_session(Path) ->
  case ems_session_manager:get_session_process(Path) of
    false -> 
      case string:rchr(Path, $/) of
        0 -> false;
        N -> 
          SubPath = string:substr(Path,1,N-1),
          find_session(SubPath)
      end;
    Pid -> {Pid, Path} 
  end.

%% ----------------------------------------------------------------------------
%% @doc Maps an ems session error reason to an appropriate RTSP status code.
%% @end
%% ----------------------------------------------------------------------------  
session_error_to_rtsp_status(Error) ->
  case Error of
    already_exists        -> method_not_valid_in_this_state;
    unsupported_transport -> unsupported_transport;
    timeout               -> service_unavailable;
    _                     -> bad_request
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
send_data(State = #rtsp_connection_state{sender=SenderPid}, Data) ->
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
    {rtsp_send, Sender, Data} when is_binary(Data)->
      gen_tcp:send(Socket,Data),
      ConnectionPid ! {sender_waiting, self()},
      sender_loop(ConnectionPid, Socket);
      
    {exit_sender, Sender} when Sender == ConnectionPid ->
      Sender ! {sender_exited, ok},
      ok
   end.
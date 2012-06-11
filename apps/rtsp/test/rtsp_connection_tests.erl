-module (rtsp_connection_tests).
-include_lib("eunit/include/eunit.hrl").
-include("rtsp.hrl").
-import(stringutils, [find_in_binary/2]).

request_handler(_Conn, _Req) -> ok.

%% ----------------------------------------------------------------------------
%% @doc Test receiving an RTSP message from the client
%% @end
%% ----------------------------------------------------------------------------
receive_message_test() ->
  Me = self(),
  
  Handler = 
    fun(_Conn, Req) -> Me ! {received_request, Req} end,
  
  Test = 
    fun(_Conn, {Client,_Server}) ->
      gen_tcp:send(Client, <<"DESCRIBE rtsp://localhost/ RTSP/1.0", 13, 10,
                             "CSeq: 42", 13, 10,
                             "Content-Length: 12", 13, 10,
                             "Content-Type: text/plain", 13, 10, 13, 10,
                             "Hello, world">>),
      receive
        {received_request, Msg} ->
          ?assertEqual("text/plain", rtsp:message_content_type(Msg)),
          ?assertEqual(12, rtsp:message_content_length(Msg)),
          ?assertEqual(<<"Hello, world">>, rtsp:message_body(Msg)),
          ?assertEqual(42, rtsp:message_sequence(Msg)),

          case Msg#rtsp_message.message of
            Rq when is_record(Rq, rtsp_request) -> 
              ?assert(Rq#rtsp_request.method == "DESCRIBE")
          end
      after
        500 ->
          ?debugMsg("Timed out"),
          ?assert(false)
      end
    end,
  run(Test,Handler).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
receive_and_reply_test() ->
  Handler = 
    fun(Conn, Req) ->
      rtsp:send_response(Conn, rtsp:message_sequence(Req), ok, [], 
                         <<"abcdefghijk">>)
    end,

  Test = 
    fun(_Conn, {Client, _Server}) ->
      inet:setopts(Client, [{active, true}]),
    
      gen_tcp:send(Client, <<"DESCRIBE rtsp://localhost/ RTSP/1.0", 13, 10,
                             "CSeq: 42", 13, 10,
                             "Content-Length: 12", 13, 10,
                             "Content-Type: text/plain", 13, 10, 13, 10,
                             "Hello, world">>),

      Buf = read_data(Client, <<>>),
  
      Expected = <<"RTSP/1.0 200 OK", 13, 10,
                   "CSeq: 42", 13, 10,
                   "Content-Length: 11", 13, 10,
                   "Server: EMS RTSP Service/0.1", 13, 10,
                   13, 10,
                   "abcdefghijk">>,

      ?assertEqual(Expected, Buf)
    end,
  run(Test, Handler).

%% ----------------------------------------------------------------------------
%% @doc Tests creating and using interleaved channels in an rtsp connection.
%% @end
%% ----------------------------------------------------------------------------
create_channel_test() ->
  Test = 
    fun(Conn, {Client, _Server}) ->
      Me = self(),
      Handler0 = fun(Buf) -> Me ! {got_buffer, 0, Buf} end,
      Handler1 = fun(Buf) -> Me ! {got_buffer, 1, Buf} end,
      ChannelSpec = [{0, Handler0}, {1, Handler1}],
      case rtsp_connection:create_channels(Conn, ChannelSpec) of
        {ok, [Ch0, Ch1]} ->
          ?assertEqual(0, rtsp_connection:channel_index(Ch0)),
          ?assertEqual(1, rtsp_connection:channel_index(Ch1)),
          ?assertEqual([0,1], rtsp_connection:active_channels(Conn)),
        
          gen_tcp:send(Client, <<$$,0,10:16/big-unsigned-integer,"hello ch 0">>),
          check_response(0, <<"hello ch 0">>),

          gen_tcp:send(Client, <<$$,1,10:16/big-unsigned-integer,"hello ch 1">>),
          check_response(1, <<"hello ch 1">>)
      end
    end,
  run(Test).

%% ----------------------------------------------------------------------------
%% @doc Tests the channel disconnection notifications
%% @end
%% ----------------------------------------------------------------------------
channel_disconnection_test() ->
  Me = self(),
  Handler = fun(B) -> Me ! {got_buffer, B} end,
  Test =
    fun(Conn, {Client, _}) ->
      {ok, [_Ch]} = rtsp_connection:create_channels(Conn, [{4,Handler}]),
      gen_tcp:close(Client),
      receive
        {got_buffer, B} when byte_size(B) =:= 0 -> ?assert(true)
      after
        100 -> 
          ?debugMsg("Timed out"),
          ?assert(false)
      end
    end,
  run(Test).

%% ----------------------------------------------------------------------------
%% @doc Tests adding a disconnection handler and making sure it actually
%%      executes.
%% @end
%% ----------------------------------------------------------------------------
disconnection_handler_test() ->
  Me = self(),
  Handler = fun() -> Me ! disconnect_invoked end,
  Test = 
    fun(Conn, {Client,_}) ->
      rtsp_connection:add_disconnection_handler(Conn, Handler),
      gen_tcp:close(Client),
      receive
        disconnect_invoked -> ?assert(true)
      after
        100 -> ?assert(false)
      end
    end,
  run(Test).

%% ----------------------------------------------------------------------------
%% @doc A skeleton for running a test that requires an RTSP connection and the
%%      sockets on either end.
%% @end
%% ----------------------------------------------------------------------------
run(TestFun) -> run(TestFun, fun(_,_) -> ok end).

run(TestFun, RqHandler) ->
  {ok, Conn} = rtsp_connection:new(dummy, "Test", RqHandler),
  try
    {Client, Server} = create_socket_pair(),
    try
      rtsp_connection:take_socket(Conn, Server),
      TestFun(Conn, {Client, Server})
    after
      gen_tcp:close(Client)
    end
  after
    rtsp_connection:close(Conn)
  end. 

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
check_response(ExpectedChannel, ExpectedData) ->
  receive
    {got_buffer, Ch, Buf} ->
      ?assertEqual(ExpectedChannel, Ch),
      ?assertEqual(ExpectedData, Buf)
  after
    1000 -> 
      ?debugMsg("Timed out"),
      ?assert(false)
  end.

%% ----------------------------------------------------------------------------
%% @doc Tests sending data on interleaved channels from the server to the 
%%      client.
%% @doc
%% ----------------------------------------------------------------------------
write_channel_test() ->
  Me = self(),
  Test = 
    fun(Conn, {Client,Server}) ->
      Handler0 = fun(Buf) -> Me ! {got_buffer, 0, Buf} end,
      Handler3 = fun(Buf) -> Me ! {got_buffer, 3, Buf} end,
      ChannelSpec = [{0, Handler0}, {3, Handler3}],
    
      inet:setopts(Client, [{active, true}]),

      case rtsp_connection:create_channels(Conn, ChannelSpec) of
        {ok, [Ch0, Ch3]} ->
          ?assertEqual([0,3], rtsp_connection:active_channels(Conn)),

          rtsp_connection:write_channel(Ch3, <<"hello on channel three">>),
          rtsp_connection:write_channel(Ch0, <<"hello on channel zero">>),
          rtsp_connection:write_channel(Ch3, <<"goodbye from channel three">>),
          rtsp_connection:write_channel(Ch0, <<"goodbye from channel zero">>),
        
          Buf = read_data(Client, <<>>),

          ?assert(is_in_buffer(Buf, <<$$, 3, 0, 22, "hello on channel three">>)),
          ?assert(is_in_buffer(Buf, <<$$, 0, 0, 21, "hello on channel zero">>)),
          ?assert(is_in_buffer(Buf, <<$$, 3, 0, 26, "goodbye from channel three">>)),
          ?assert(is_in_buffer(Buf, <<$$, 0, 0, 25, "goodbye from channel zero">>))
      end
    end,
  run(Test).

is_in_buffer(Str, Pattern) ->
  case find_in_binary(Str, Pattern) of
    N when is_integer(N) -> true;
    false -> false
  end.

read_data(S, B) ->
  receive  
    {tcp, S, Data} -> read_data(S, <<B/binary, Data/binary>>);
    {tcp_closed, S} -> B;
    {tcp_error, S, E} -> 
      ?debugFmt("Unexpected error code: ~w", [E]),
      ?assert(false)
  after
    % assume that we've got whatever we're going to get...
    100 -> B
  end.

%% ----------------------------------------------------------------------------
%% @doc Creates a connected socket pair to help with testing.
%% @end
%% ----------------------------------------------------------------------------
create_socket_pair() ->
  Me = self(),
  Listener = 
    fun() ->
      case gen_tcp:listen(0, [{active, false}]) of
        {ok, Sock} ->
          {ok, Port} = inet:port(Sock),
          Me ! {server_port, Port},
          
          case gen_tcp:accept(Sock) of 
            {ok, ServerSocket} ->
              gen_tcp:controlling_process(ServerSocket, Me),
              Me ! {server_socket, ServerSocket}
          end,

          gen_tcp:close(Sock)
      end
    end,

  proc_lib:spawn(Listener),

  Port = receive {server_port, P} -> P end,

  case gen_tcp:connect("localhost", Port, [{active, false}, binary]) of
    {ok, Client} -> 
      Server = receive {server_socket, S} -> S end,
      {Client, Server}
  end.

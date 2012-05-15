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
  ?debugMsg("*** Entering receive_message_test ***"),
  Me = self(),
  Handler = fun(_Conn, Req) -> Me ! {received_request, Req} end,
  {ok, Conn} = rtsp_connection:new(test, "Testing", Handler),
  try
    ?debugMsg("Creating socket pair"),
    {Client,Server} = create_socket_pair(),
    rtsp_connection:take_socket(Conn,Server),
    gen_tcp:send(Client, <<"DESCRIBE rtsp://localhost/ RTSP/1.0", 13, 10,
                           "CSeq: 42", 13, 10,
                           "Content-Length: 12", 13, 10,
                           "Content-Type: text/plain", 13, 10, 13, 10,
                           "Hello, world">>),
    ?debugMsg("Waiting for request"),
    receive
      {received_request, Msg} ->
        ?debugMsg("Received message"),
        ?assertEqual("text/plain", rtsp:message_content_type(Msg)),
        ?assertEqual(12, rtsp:message_content_length(Msg)),
        ?assertEqual(<<"Hello, world">>, rtsp:message_body(Msg)),
        ?assertEqual(42, rtsp:message_sequence(Msg)),

        case Msg#rtsp_message.message of
          Rq when is_record(Rq, rtsp_request) -> 
            ?debugMsg("Received request"),
            ?assert(Rq#rtsp_request.method == "DESCRIBE");

          _ -> 
            ?debugMsg("Invalid record type"),
            ?assert(false)
        end
    after
      500 ->
        ?debugMsg("Timed out"),
        ?assert(false)
    end
  after
    ?debugMsg("Cleaning up"),
    rtsp_connection:close(Conn)
  end. 

%% ----------------------------------------------------------------------------
%% @doc Tests creating and using interleaved channels in an rtsp connection.
%% @end
%% ----------------------------------------------------------------------------
create_channel_test() ->
  ?debugMsg("*** Entering create_channel_test() ***"),
  {ok, Conn} = rtsp_connection:new(dummy, "Testing", fun request_handler/2),
  try
    Me = self(),
    Handler0 = fun(Buf) -> Me ! {got_buffer, 0, Buf} end,
    Handler1 = fun(Buf) -> Me ! {got_buffer, 1, Buf} end,
    ChannelSpec = [{0, Handler0}, {1, Handler1}],
    case rtsp_connection:create_channels(Conn, ChannelSpec) of
      {ok, [Ch0, Ch1]} ->
        ?assertEqual(0, rtsp_connection:channel_index(Ch0)),
        ?assertEqual(1, rtsp_connection:channel_index(Ch1)),
        
        ?debugMsg("Creating socket pair"),
        {Client, Server} = create_socket_pair(),
        
        ?debugMsg("Giving socket to rtsp connection"),
        rtsp_connection:take_socket(Conn, Server),

        ?debugMsg("Sending Channel 0 Message"),
        gen_tcp:send(Client, <<$$,0,10:16/big-unsigned-integer,"hello ch 0">>),
        check_response(0, <<"hello ch 0">>),

        ?debugMsg("Sending Channel 1 Message"),
        gen_tcp:send(Client, <<$$,1,10:16/big-unsigned-integer,"hello ch 1">>),
        check_response(1, <<"hello ch 1">>);

      Other ->
        io:format("Unexpected return: ~w~n", [Other])
    end
  after
    rtsp_connection:close(Conn)
  end.

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
  ?debugMsg("*** Entering write_channel_test() ***"),
  Me = self(),
  {ok, Conn} = rtsp_connection:new(dummy, "Test", fun request_handler/2),
  try
    Handler0 = fun(Buf) -> Me ! {got_buffer, 0, Buf} end,
    Handler3 = fun(Buf) -> Me ! {got_buffer, 3, Buf} end,
    ChannelSpec = [{0, Handler0}, {3, Handler3}],
    {Client,Server} = create_socket_pair(),
    
    ?debugMsg("Setting up setup sockets"),
    rtsp_connection:take_socket(Conn, Server),
    inet:setopts(Client, [{active, true}]),

    case rtsp_connection:create_channels(Conn, ChannelSpec) of
      {ok, [Ch0, Ch3]} ->
        ?debugMsg("Writing Channel Data"),
        rtsp_connection:write_channel(Ch3, <<"hello on channel three">>),
        rtsp_connection:write_channel(Ch0, <<"hello on channel zero">>),
        rtsp_connection:write_channel(Ch3, <<"goodbye from channel three">>),
        rtsp_connection:write_channel(Ch0, <<"goodbye from channel zero">>),
        
        ?debugMsg("Reading data"),
        Buf = read_data(Client, <<>>),

        ?debugMsg("Checking assertions"),
        ?assert(is_in_buffer(Buf, <<$$, 3, 0, 22, "hello on channel three">>)),
        ?assert(is_in_buffer(Buf, <<$$, 0, 0, 21, "hello on channel zero">>)),
        ?assert(is_in_buffer(Buf, <<$$, 3, 0, 26, "goodbye from channel three">>)),
        ?assert(is_in_buffer(Buf, <<$$, 0, 0, 25, "goodbye from channel zero">>)),
    
        ?debugMsg("Assertions checked")
    end
  after
    ?debugMsg("Cleaning up"),
    rtsp_connection:close(Conn)
  end.

is_in_buffer(Str, Pattern) ->
  case find_in_binary(Str, Pattern) of
    N when is_integer(N) -> true;
    false -> false
  end.

read_data(S, B) ->
  receive  
    {tcp, S, Data} -> 
      ?debugFmt("Read ~w bytes", [byte_size(Data)]),
      read_data(S, <<B/binary, Data/binary>>);
    
    {tcp_closed, S} ->
      ?debugMsg("Socket Closed"),
      B;
    
    {tcp_error, S, E} -> 
      ?debugFmt("Unexpected error code: ~w", [E]),
      ?assert(false)
  after
    100 ->
      ?debugMsg("Timing out read"),
      B
  end.

%% ----------------------------------------------------------------------------
%% @doc Creates a connected socket pair to help with testing.
%% @end
%% ----------------------------------------------------------------------------
create_socket_pair() ->
  Me = self(),
  Listener = 
    fun() ->
      ?debugMsg("Starting listener"),
      case gen_tcp:listen(0, [{active, false}]) of
        {ok, Sock} ->
          {ok, Port} = inet:port(Sock),
          Me ! {server_port, Port},
          ?debugFmt("Listening on port ~w", [Port]),
          
          case gen_tcp:accept(Sock) of 
            {ok, ServerSocket} ->
              ?debugMsg("client connected"), 
              gen_tcp:controlling_process(ServerSocket, Me),
              Me ! {server_socket, ServerSocket};

            Err ->
              io:format("Failed to accept socket: ~w~n", [Err])
          end,

          gen_tcp:close(Sock)
      end
    end,

  ?debugMsg("Spawning listener"),
  proc_lib:spawn(Listener),

  ?debugMsg("Waiting for port"), 
  Port = receive {server_port, P} -> P end,

  case gen_tcp:connect("localhost", Port, [{active, false}, binary]) of
    {ok, Client} -> 
      Server = receive {server_socket, S} -> S end,
      {Client, Server};

    Err ->
      io:format("Failed to connect: ~w~n", [Err]),
      Err
  end.

-module (rtsp_connection_tests).
-include_lib("eunit/include/eunit.hrl").
-include("rtsp.hrl").

request_handler(_Conn, _Req) -> ok.

%% ----------------------------------------------------------------------------
%% @doc Tests creating and using interleaved channels in an rtsp connection.
%% @end
%% ----------------------------------------------------------------------------
create_channel_test() ->
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

  case gen_tcp:connect("localhost", Port, []) of
    {ok, Client} -> 
      Server = receive {server_socket, S} -> S end,
      {Client, Server};

    Err ->
      io:format("Failed to connect: ~w~n", [Err]),
      Err
  end.

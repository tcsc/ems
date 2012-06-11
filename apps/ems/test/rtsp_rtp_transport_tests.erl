-module(rtsp_rtp_transport_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

creation_test() -> 
  {ok, Conn} = rtsp_connection:new(dummy, "testing", fun request_handler/2),
  try
    {Client,Server} = create_connection(),
    try
      rtsp_connection:take_socket(Conn, Server),
      Spec = [{protocol, rtp}, 
              {profile, avp}, 
              {interleaved, [3,4]}],

      Transport = rtp_transport:new(rtsp_rtp_transport, {Conn, Spec}),

      ?assertEqual([3,4], rtsp_connection:active_channels(Conn))
    after
      gen_tcp:close(Client)
    end
  after
    rtsp_connection:close(Conn)
  end.

request_handler(_Conn, _Req) -> ok.

%% ----------------------------------------------------------------------------
%% @doc Creates a connected socket pair and connection object to help with 
%%      testing.
%% @end
%% ----------------------------------------------------------------------------
create_connection() ->
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

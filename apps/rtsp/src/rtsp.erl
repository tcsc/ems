-module(rtsp).
-behaviour(application).
-behaviour(supervisor).
-include("rtsp.hrl").
-include("logging.hrl").

%% ============================================================================
%% Behaviour exports
%% ============================================================================
-export([start/2, stop/1, init/1]).

%% ============================================================================
%% Public API
%% ============================================================================
-export([
  start/0,
  start_link/0,
  parse_message/1,
  format_message/3,
  format_transport/1,
  find_eom/1,
  translate_status/1,
  parse_transport/1,
  get_header/2, 
  get_request_info/1,
  is_request/1,
  is_response/1,
  message_content_length/1,
  message_content_type/1,
  with_authenticated_user_do/5]).

%% ============================================================================
%%
%% ============================================================================
-type message() :: #rtsp_message{}.
-type request() :: #rtsp_request{}.
-type header() :: #rtsp_message_header{}.
-type user_info() :: #rtsp_user_info{}.
-type request_callback() :: fun((rtsp_connection:conn(), message()) -> any()).

-export_type([message/0, request/0, header/0, request_callback/0, user_info/0]).

                                 
-type user_info_callback() :: fun((string()) -> false | {ok, user_info()}).
-type authenticated_action() :: fun((user_info()) -> any()).
-export_type([user_info_callback/0, authenticated_action/0]).

%% ============================================================================
%% Definitions
%% ============================================================================
-define(SP, 16#20).
-define(RTSP_PID, rtsp_server).

%% ============================================================================
%% Application callbacks
%% ============================================================================
start() ->
  application:start(listener),
  application:start(rtsp).
  
start(normal,_) ->
  rtsp:start_link().
  
stop(_) -> 
  Stop = fun({Id, _Pid, _, _}) ->
    supervisor:terminate_child(rtsp_sup, Id),
    supervisor:delete_child(rtsp_sup, Id)
  end,
  lists:foreach(Stop, supervisor:which_children(rtsp_sup)).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> 
  case supervisor:start_link({local, rtsp_sup}, ?MODULE, []) of
    {ok, Pid} -> 
      ?LOG_DEBUG("rtsp:start_link/0 - RTSP supervisor started on ~w", [Pid]),
      {ok, Pid};
      
    {error, Err} -> 
      ?LOG_ERROR("rtsp:start_link/0 - RTSP supervisor failed to start ~w", [Err]),
      {error, Err}
  end.

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================
init(_) -> 
  RtspServer = {
    rtsp_server,
    {rtsp_server, start_link, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_server]
  },
  DigestServer = {
    digest_server,
    {rtsp_digest_server, start_link, []},
    permanent,
    brutal_kill,
    worker,
    [rtsp_digest_server]
  },
  {ok, {{one_for_one, 10, 1}, [DigestServer, RtspServer]}}.


%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Attempts to authenticate a request and executes an action if and only
%%      if the athentication succeeds.  
%% @throws unauthorised | bad_request
%% @end
%% ----------------------------------------------------------------------------
-spec with_authenticated_user_do(rtsp_connection:conn(),
                                 request(),
                                 header(),
                                 user_info_callback(),
                                 authenticated_action()) -> any().
with_authenticated_user_do(Conn, Request, Headers, PwdCallback, Action) ->
  case get_header(Headers, ?RTSP_HEADER_AUTHORISATION) of
    undefined -> throw(unauthorised);  
    [AuthHeader|_] -> 
      AuthInfo = case rtsp_authentication:parse(AuthHeader) of
                   {ok, I} -> I;
                   _ -> throw(bad_request)
                 end,
      UserName = rtsp_authentication:get_user_name(AuthInfo),
      case PwdCallback(UserName) of
        false -> throw(unauthorised);
        {ok, UserInfo} -> 
          case rtsp_authentication:validate(Conn, Request, AuthInfo, UserInfo) of
            ok -> Action(UserInfo);
            fail -> throw(unauthorised)
          end
      end
  end.

%% ----------------------------------------------------------------------------
%% @doc Parses a binary as an RTSP message. 
%% @end
%% ----------------------------------------------------------------------------
-spec parse_message(binary()) -> message().
parse_message(Data) when is_binary(Data) ->
  [FirstLine | Lines] = re:split(Data, <<"\r\n">>),

  % look at the first line of the message, which will tell us if this is an 
  % inbound request or response
  {MessageType,Message} = parse_first_line(FirstLine),

  % parse the common parts of the message, including headers, sequence,
  % content length, etc.
  Headers = parse_headers(Lines),
  #rtsp_message{message = Message, headers = reify_headers(Headers)}.

%% -----------------------------------------------------------------------------
%% @doc 
%% @spec get_header(Headers, Header) -> Header | undefined
%%       Header = string()
%% @end
%% -----------------------------------------------------------------------------
-spec get_header(header(), string()) -> [string()] | 'undefined'.
get_header(Headers, Header) when is_record(Headers, rtsp_message_header) ->
  case dict:find(Header, Headers#rtsp_message_header.headers) of
    {ok, [Value]} -> Value;
    _ -> undefined
  end.
  
%% -----------------------------------------------------------------------------
%% @doc Parses an RTSP transport header into a list of name/value options.
%% @end
%% -----------------------------------------------------------------------------
parse_transport(Text) ->
  [Transport|Items] = string:tokens(Text, ";"),
  parse_transport(Items,  parse_transport_spec(Transport)).

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
parse_transport([ItemText|Remainder], Result) ->
  {Name,Value} = stringutils:split_on_first($=, ItemText),
  Item = case string:to_lower(Name) of 
    "multicast"   -> multicast;
    "unicast"     -> unicast;
    "mode"        -> {direction, parse_transport_mode(Value)};
    "destination" -> {destination, Value};
    "ttl"         -> {ttl, parse_int(Value)};
    "interleaved" -> {interleaved, parse_number_list(Value)};
    "client_port" -> {client_port, parse_number_list(Value)};
    "server_port" -> {server_port, parse_number_list(Value)};
    "port"        -> {port, parse_number_list(Value)};
    "layers"      -> {layers, Value};
    "ssrc"        -> {sync_source, Value};
    "source"      -> {source, Value};
    "append"      -> append;
    _             -> {Name,Value}
  end,
  parse_transport(Remainder, [Item|Result]);

parse_transport([], Result) ->
  lists:reverse(Result).

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
parse_transport_spec(Spec) ->
  Elements = string:tokens(Spec, "/"),
  
  Protocol = parse_transport_protocol(lists:nth(1, Elements)),
  Profile = parse_transport_profile(lists:nth(2, Elements)),
  
  if
    length(Elements) > 2 ->
      LowerTransport = parse_lower_transport(lists:nth(3, Elements)),
      [{protocol,Protocol}, {profile,Profile}, {lower_transport, LowerTransport}];
    
    true -> 
      [{protocol,Protocol}, {profile,Profile}]
  end.

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------  
parse_transport_protocol(Protocol) ->
  case string:to_lower(Protocol) of
    "rtp" -> rtp;
    _ -> Protocol
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
parse_transport_profile(Profile) ->
  case string:to_lower(Profile) of 
    "avp" -> avp;
    _ -> Profile
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
parse_lower_transport(Transport) ->
  case string:to_lower(Transport) of 
    "udp" -> udp;
    "tcp" -> tcp;
    _ -> Transport
  end.

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------  
parse_transport_mode(Mode) ->
  case stringutils:unquote(string:to_lower(Mode)) of
    "play" -> outbound;
    "record" -> inbound;
    _ -> Mode
  end.

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------    
parse_int(Text) ->
  try
    list_to_integer(Text)
  catch
    error:badarg -> throw({rtsp_error,bad_request})
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------  
parse_number_list(Text) when is_list(Text) ->
  Numbers = string:tokens(Text, "-"),
  try
    lists:map(fun list_to_integer/1, Numbers)
  catch
    error:badarg -> throw({rtsp_error, bad_request})
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
format_number_list(Numbers) ->
  format_number_list(Numbers, []).
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------
format_number_list( [Number | Remainder], Result) ->
  Text = io_lib:format("~w", [Number]),
  format_number_list( Remainder, [Text | Result]);
  
format_number_list([], Result) ->
  string:join( lists:reverse(Result), "-").

%% -----------------------------------------------------------------------------
%% @doc Formats an RTSP message for sending
%% @spec format_message(Message,Headers,Body) -> Result
%%       Message = rtsp_request() | rtsp_response() 
%%       Headers = rtsp_message_header()
%%       Body = binary()
%% @end
%% ----------------------------------------------------------------------------- 
format_message(_Message,_Headers,_Body) when is_record(_Message,rtsp_request) ->
  << >>;
  
format_message(Message,Headers,Body) when is_record(Message,rtsp_response) ->
  % format the Respone line of the message and the common headers
  ResponseLine = format_response_line(Message),
  HeaderText = format_headers(Headers),
  
  % convert the list of strings into a single string
  ResponseText = lists:flatten([ResponseLine | HeaderText]),
  
  ?LOG_DEBUG("~n~s", [ResponseText]),
  
  % encode the response as UTF-8
  Result = utf:string_to_utf8(lists:append(ResponseText, "\r\n")),
  
  case Body of 
    << >> -> Result;
    _ -> list_to_binary([Result, Body])
  end.

%% -----------------------------------------------------------------------------
%% @doc Parses the first line of an RTSP message and determines if the message
%%      is a request or a response.
%% @spec parse_first_line(Line) -> {request,Request} | {response,Response}
%% @end
%% -----------------------------------------------------------------------------
parse_first_line(Line) ->
  case Line of
    % The line starts with an RTSP version marker, so it looks like a 
    % response and we will parse it like one
    <<"RTSP/", _/binary>> -> parse_response_line(Line);

    % The line isn't a response, so it must be a request...
    _ -> parse_request_line(Line)
  end.

%% -----------------------------------------------------------------------------
%% @doc Parses an RTSP request line
%% @spec parse_request_line(Data) -> {request,Request}
%%       Request = is_record(rtsp_request) 
%% @end
%% -----------------------------------------------------------------------------  
parse_request_line(Data) ->
  {Method,EndOfMethod} = stringutils:extract_token(Data, 0, ?SP),
  {Uri,EndOfUri} = stringutils:extract_token(Data, EndOfMethod+1, ?SP),
  StartOfVersion = EndOfUri + 1,
  case Data of
    <<_:StartOfVersion/binary, "RTSP/", VerMajor:8, ".", VerMinor:8>> ->
      % the line is a valid request line, so we build a valid request 
      % record and return it
      Request = #rtsp_request{
        method = Method,
        uri = Uri, 
        version = {
          list_to_integer([VerMajor]),
          list_to_integer([VerMinor])} },
      {request, Request};

    _ ->
      % oops - not a properly-formatted request line. Bail.
      throw({error, bad_request})
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------  
parse_response_line(_Data) ->
  {response, {}}.

%% -----------------------------------------------------------------------------
%% @doc Formats the first line of a response and returns it as a binary object 
%%      ready for sending.
%% @spec format_response_line(Response) -> Result
%%       Response = rtsp_response()
%%       Result = string()
%% @end
%% -----------------------------------------------------------------------------
format_response_line(Response) when is_record(Response, rtsp_response) ->
  {Status,Text} = translate_status(Response#rtsp_response.status),
  {VersionMajor,VersionMinor} = Response#rtsp_response.version,
  io_lib:format("RTSP/~w.~w ~3..0w ~s\r\n", [VersionMajor, VersionMinor, Status, Text]).

%% -----------------------------------------------------------------------------
%% @doc Parses the common parts of an rtsp message and returns it 
%% @spec parse_header(Lines) -> Result
%% @end
%% -----------------------------------------------------------------------------
parse_headers(Lines) ->
  parse_headers(Lines, dict:new()).

%% -----------------------------------------------------------------------------
%% @doc Constructs a dictionary containing the header values.
%% @spec parse_header(Lines,Headers) -> Result
%%       Lines = [iolist()]
%%       Headers = dictionary()
%%       Result = dictionary()
%% @end
%% -----------------------------------------------------------------------------  
parse_headers([Line|Leftover], Headers) -> 
  {NameText,EndOfName} = stringutils:extract_token(Line,0,$:),
  {ValueText,_} = stringutils:extract_token(Line,EndOfName+1,$\n),
  Name = string:strip(NameText),
  Value = lists:flatten(string:strip(ValueText)),
  NewHeaders = dict:append(Name,Value,Headers),
  parse_headers(Leftover, NewHeaders);

parse_headers([], Headers) -> 
  Headers.

%% ----------------------------------------------------------------------------
%% @doc Creates a parsed message header out of a set of headers.
%% @spec reify_headers(Headers) -> rtsp_message_header
%% @end
%% ----------------------------------------------------------------------------
reify_headers(Headers) ->
  {ok, [CSeq]} = dict:find("CSeq", Headers),

  ContentLength = 
    case dict:find("Content-Length", Headers) of
      {ok, [ContentLengthText]} -> list_to_integer(ContentLengthText);
      _ -> 0
    end,

  ContentType = 
    case dict:find("Content-Type", Headers) of
      {ok, [ContentTypeText]} -> ContentTypeText;
      _ -> ""
    end,

  {Sequence,_} = string:to_integer(CSeq),

  #rtsp_message_header{
    sequence = Sequence, 
    content_length = ContentLength,
    content_type = ContentType,
    headers = Headers}.

%% ----------------------------------------------------------------------------    
%% @doc Formats the headers as a list of strings. Note that ani multivalued 
%%      headers must have already bee coalesced into a single string by the 
%%      time they get here.
%% @spec format_headers(Hedaers) -> [string()]
%% end
%% ----------------------------------------------------------------------------
format_headers(Headers = #rtsp_message_header{
    sequence = Sequence, content_length = ContentLength }) ->
  
  % format the generic headers into a list of strings
  Formatter = fun({Name,Value}) ->
      io_lib:format("~s: ~s\r\n", [Name, Value]) 
  end,
  
  FormattedHeaders = lists:map(
    Formatter,
    dict:to_list(Headers#rtsp_message_header.headers)),

  % format the sequence value 
  SequenceHeader = io_lib:format("CSeq: ~w\r\n", [Sequence]),
  
  AllHeaders = 
    if 
      ContentLength > 0 -> 
        [io_lib:format("Content-Length: ~w\r\n", [ContentLength]) | FormattedHeaders];
      true ->  FormattedHeaders
    end,
  
  % combine the formatted headers and the explicitly built headers and return 
  % the lot
  [SequenceHeader | AllHeaders].

%% ----------------------------------------------------------------------------
%% @doc Finds the RTSP end-of-message marker (i.e two CRLF sequences in a row) 
%%      in a binary. 
%% @spec find_eom(Data) -> {ok, Message, Remainder} | notfound
%% @end
%% ----------------------------------------------------------------------------
find_eom(Data) ->
  find_eom(0, Data).

%% ----------------------------------------------------------------------------
%% @doc The internal implementation of find_eom/1.
%% @spec find_eom(Offset,Data) -> {ok,Message,Remainder} | notfound.
%% @end
%% ----------------------------------------------------------------------------
find_eom(Offset, Data) when Offset < size(Data) ->
  case Data of 
    <<Message:Offset/binary, $\r, $\n, $\r, $\n, Remainder/binary>> ->
      {ok, Message, Remainder};

    _ -> find_eom(Offset+1,Data)
  end;

find_eom(_Offset, _Data) ->
  notfound.

%% ----------------------------------------------------------------------------  
%% @doc Formats a trasport spec as an RTSP transport header
%% @spec format_transport(TransportSpec) -> Result
%%         TransportSpec = [TransportOption]
%%         TransportOption = atom() | {Name, Value}
%%         Name = Value = string() | atom()
%%         Result = string()
%% @end
%% ----------------------------------------------------------------------------  
format_transport(TransportSpec) ->
  {protocol, Protocol} = lists:keyfind(protocol, 1, TransportSpec),
  {profile, Profile} = lists:keyfind(profile, 1, TransportSpec),
  
  LowerTransport = case lists:keyfind(lower_transport, 1, TransportSpec) of
    {lower_transport, T} ->
      io_lib:format("/~s", [format_lower_transport(T)]);

    _ -> ""
  end,

  Attributes = 
    lists:keydelete(lower_transport, 1, 
      lists:keydelete(profile, 1, 
        lists:keydelete(protocol, 1, TransportSpec))),

  lists:flatten(io_lib:format("~s/~s~s;~s", [
    format_protocol(Protocol), 
    format_profile(Profile), 
    LowerTransport, 
    format_transport_attributes(Attributes)])).
        
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
format_transport_attributes(AttributeList) ->
  String = format_transport_attributes(AttributeList, []),
  String.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
format_transport_attributes([Attribute|Remainder], Result) ->
  Text = case format_single_attribute(Attribute) of
    {Name,Value} -> io_lib:format("~s=~s", [Name, Value]);
    F -> F
  end, 
  format_transport_attributes(Remainder, [Text | Result]);
  
format_transport_attributes([],Result) ->
  string:join(Result, ";").

%% ----------------------------------------------------------------------------  
%% @doc Translates a single element of a transport header, returning either
%%      the single attribute, or a 2-element tuple describing a name/value
%%      pair.
%%
%% @spec format_single_attribute(Attribute) -> Result 
%%         Result = string() | {Name, Value}
%%         Name = Value = string().
%% @end
%% ----------------------------------------------------------------------------  
format_single_attribute(Attribute) ->
  case Attribute of
    multicast                  -> "multicast";
    unicast                    -> "unicast";
    append                     -> "append";
    {interleaved, Channels}    -> {"interleaved", format_number_list(Channels)};
    {client_port, ClientPorts} -> {"client_port", format_number_list(ClientPorts)};
    {server_port, ServerPorts} -> {"server_port", format_number_list(ServerPorts)};
    {source, Source}           -> {"source", format_ip_address(Source)};
    {ssrc, SyncSrc}            -> {"ssrc", io_lib:format("~.16b", [SyncSrc])};
    {direction, Direction}     -> {"mode", format_transport_mode(Direction)};
    {layers, Layers}           -> {"layers", Layers};
    {ttl, TimeToLive}          -> {"ttl", TimeToLive};
    {Name,Value}               -> {Name, Value}
  end.  


format_ip_address({A, B, C, D}) ->
  io_lib:format("~p.~p.~p.~p", [A,B,C,D]);
  
format_ip_address(Address) ->
  Address.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
format_protocol(Protocol) ->
  case Protocol of 
    rtp -> "RTP";
    _ -> Protocol
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
format_profile(Profile) ->
  case Profile of 
    avp -> "AVP";
    ProfileName -> ProfileName
  end.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
format_lower_transport(Transport) ->
  case Transport of 
    tcp -> "TCP";
    udp -> "UDP";
    _ -> Transport
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
format_transport_mode(Mode) ->
  case Mode of 
    inbound -> "record";
    outbound -> "play";
    _ -> Mode
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
translate_status(Status) ->
  case Status of
    ok                    -> {200, "OK"};
    bad_request           -> {400, "Bad Request"};
    not_found             -> {404, "Not Found"};
    length_required       -> {411, "Length Required"};
    session_not_found     -> {454, "Session Not Found"};
    method_not_valid      -> {455, "Method Not Valid in this State"};
    unsupported_transport -> {461, "Unsupported transport"};
    internal_server_error -> {500, "Internal Server Error"};
    not_implemented       -> {501, "Not Implemented"};
    service_unavailable   -> {503, "Service Unavailable"}
  end.

is_request(Msg = #rtsp_message{message = Rq}) when is_record(Msg, rtsp_message) ->
  is_record(Rq, rtsp_request).

is_response(Msg = #rtsp_message{message = Rs}) when is_record(Msg, rtsp_message) ->
  is_record(Rs, rtsp_response).

-spec message_content_length(message()) -> integer().
message_content_length(#rtsp_message{headers = Hdrs}) -> 
  Hdrs#rtsp_message_header.content_length.

-spec message_content_type(message()) -> string().
message_content_type(#rtsp_message{headers = Hdrs}) -> 
  Hdrs#rtsp_message_header.content_type.


%% ----------------------------------------------------------------------------
%% @doc Extracts some commonly-used bits out of an RTSP request and returns 
%%      them to the caller.
%% @end
%% ----------------------------------------------------------------------------  
-spec get_request_info(message()) -> 
        {string(), string(), integer(), integer(), string()}.
get_request_info(#rtsp_message{message = Rq, headers = Hdrs}) when is_record(Rq, rtsp_request) ->
  M   = Rq#rtsp_request.method,
  Uri = Rq#rtsp_request.uri,
  Seq = Hdrs#rtsp_message_header.sequence,
  Cl  = Hdrs#rtsp_message_header.content_length,
  Ct  = Hdrs#rtsp_message_header.content_type,
  {M, Uri, Seq, Cl, Ct}.
  

-module (rtsp).
-include("rtsp.hrl").

%% ============================================================================
%% Parsing Exports 
%% ============================================================================
-export([
  parse_message/1,
  format_message/3,
  find_eom/1,
  translate_status/1,
  parse_transport/1]).

%% ============================================================================
%% Formatting Exports
%% ============================================================================
-export([
  format_transport/1]).

%% ============================================================================
%% Utility Exports
%% ============================================================================
-export([get_header/2, get_request_info/2]).

%% ============================================================================
%% Definitions
%% ============================================================================
-define(SP, 16#20).

%% ----------------------------------------------------------------------------
%% @doc Parses a binary as an RTSP message. 
%% @spec parse_message(Data,State) -> Result
%%       Result = {MessageType, Message, Headers} | error
%%       MessageType = request | response
%%       Message = Request | Response
%%       Request = rtsp_request()
%%       Response = rtsp_response()
%% @end
%% ----------------------------------------------------------------------------
parse_message(Data) when is_binary(Data) ->
  [FirstLine | Lines] = re:split(Data, <<"\r\n">>),

  % look at the first line of the message, which will tell us if this is an 
  % inbound request or response
  {MessageType,Message} = parse_first_line(FirstLine),

  % parse the common parts of the message, including headers, sequence,
  % content length, etc.
  Headers = parse_headers(Lines),
  {MessageType, Message, reify_headers(Headers)}.

%% -----------------------------------------------------------------------------
%% @doc 
%% @spec get_header(Headers, Header) -> Header | false
%%       Header = string()
%% @end
%% -----------------------------------------------------------------------------
get_header(Headers, Header) when is_record(Headers, rtsp_message_header) ->
  case dict:find(Header, Headers#rtsp_message_header.headers) of
    {ok, [Value]} -> Value;
    _ -> false
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
    "receive" -> inbound;
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
    lists:map(fun(N) -> list_to_integer(N) end, Numbers)
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
format_message(Message,Headers,Body) when is_record(Message,rtsp_request) ->
  << >>;
  
format_message(Message,Headers,Body) when is_record(Message,rtsp_response) ->
  % format the Respone line of the message and the common headers
  ResponseLine = format_response_line(Message),
  HeaderText = format_headers(Headers),
  
  % convert the list of strings into a single string
  ResponseText = lists:flatten([ResponseLine | HeaderText]),
  
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
        method = parse_method(Method),
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
%% @spec parse_method(MethodName) -> Method | MethodName
%%         Method = options | announce | describe | setup | play | teardown
%% @end 
%% -----------------------------------------------------------------------------
parse_method(MethodName) ->
  case MethodName of
    "OPTIONS" -> options;
    "ANNOUNCE" -> announce;
    "DESCRIBE" -> describe;
    "SETUP" -> setup;
    "PLAY" -> play;
    "TEARDOWN" -> teardown;
    _ -> MethodName
  end.
  
%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------  
parse_response_line(Data) ->
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
  {Status,Text} = Response#rtsp_response.status,
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
  Name = string:to_lower(string:strip(NameText)),
  Value = string:strip(ValueText),
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
  {ok, [CSeq]} = dict:find(?RTSP_SEQUENCE, Headers),

  ContentLength = 
    case dict:find(?RTSP_CONTENT_LENGTH, Headers) of
      {ok, [ContentLengthText]} -> list_to_integer(ContentLengthText);
      _ -> 0
    end,

  ContentType = 
    case dict:find(?RTSP_CONTENT_TYPE, Headers) of
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

find_eom(Offset, Data) ->
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
  
  Attributes = lists:keydelete(profile, 1, 
    lists:keydelete(protocol, 1, TransportSpec)),

  lists:flatten(io_lib:format("~s/~s~s;~s", [
    format_protocol(Protocol), 
    format_profile(Profile), 
    LowerTransport, 
    format_transport_attributes(Attributes)])).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
format_transport_attributes(AttributeList) ->
  String = format_transport_attributes(AttributeList, []).

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
    {source, Source}           -> {"source", Source};
    {ssrc, SyncSrc}            -> {"ssrc", SyncSrc};
    {direction, Direction}     -> {"mode", format_transport_mode(Direction)};
    {layers, Layers}           -> {"layers", Layers};
    {ttl, TimeToLive}          -> {"ttl", TimeToLive};
    {Name,Value}               -> {Name, Value}
  end.  


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
    udp -> "TCP";
    _ -> Transport
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
format_transport_mode(Mode) ->
  case Mode of 
    inbound -> "receive";
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
    method_not_valid      -> {455, "Method Not Valid in this State"};
    unsupported_transport -> {461, "Unsupported transport"};
    internal_server_error -> {500, "Internal Server Error"};
    not_implemented       -> {501, "Not Implemented"};
    service_unavailable   -> {503, "Service Unavailable"}
  end.

%% ----------------------------------------------------------------------------
%% @doc Extracts some commonly-used bits out of an RTSP request and returns 
%%      them to the caller.
%% @spec get_request_info(Request, Headers) -> Result
%%         Request = rtsp_request()
%%         Headers = rtsp_message_header()
%%         Result = {Method, Uri, Sequence, ContentLength, ContentType}
%%         Method = announce | options | setup | play | teardown | string()
%%         Uri = string()
%%         Sequence = int()
%%         ContentLength = int()
%%         ContentType = string()
%% @end
%% ----------------------------------------------------------------------------  
get_request_info(Request, Headers) ->
  Method = Request#rtsp_request.method,
  Uri = Request#rtsp_request.uri,
  Sequence = Headers#rtsp_message_header.sequence,
  ContentLength = Headers#rtsp_message_header.content_length,
  ContentType = Headers#rtsp_message_header.content_type,
  {Method, Uri, Sequence, ContentLength, ContentType}.
  
-module (sdp).

-export([parse/1]).
-include("sdp.hrl").

% =============================================================================
% Exported functions
% =============================================================================  

parse(Body) when is_binary(Body) ->
  Text = utf:utf8_to_string(Body),
  parse(Text);
  
parse(Text) ->
  Parser = fun(Line) -> parse_line(Line) end,
  Lines = lists:map(Parser, re:split(Text, <<"\r\n">>, [trim, {return,list}])),
  
  SessionBlock = extract_session_block(Lines),
  Attributes = extract_attributes(SessionBlock),
  
  RtpMap = extract_rtp_map(Lines),
  FormatMap = extract_format_map(Lines),
  Streams = collect_media_streams(Lines),
  
  #session_description{
    attributes = Attributes,
    streams = Streams, 
    rtp_map = RtpMap, 
    format_map = FormatMap}.
      
% =============================================================================
% Internal Fuctions
% =============================================================================  

extract_attributes(Lines) ->
  AttributeList = lists:filter(
    fun(Item) -> element(1,Item) == attribute end,
    Lines),
  
  lists:map(
    fun({attribute, Name, Value}) -> {Name, Value} end,
    AttributeList).
    
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_rtp_map(Lines) ->
  lists:map(
    fun(Item) -> 
      ID = Item#rtp_map.id,
      {id, Item}
    end,
    lists:filter(fun(Item) -> is_record(Item,rtp_map) end, Lines)).
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_format_map(Lines) ->
  Formats = lists:filter(
    fun(Item) -> 
      case Item of 
        {format_p, Id, Parameters} -> true; 
        _ -> false 
      end
    end, 
    Lines),
  
  FormatMap = lists:map(
    fun(Item) -> 
      case Item of 
        {format_p, Id, Parameters} -> {Id, Parameters}
      end
    end,
    Formats).
    
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_session_block(Lines) ->
  lists:reverse(extract_session_block(Lines, [])).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_session_block(Line, SessionBlock) when is_record(Line, media_stream) ->
  SessionBlock;

extract_session_block([], SessionBlock) ->
  SessionBlock;
  
extract_session_block([Line|Remainder], SessionBlock) ->
  extract_session_block(Remainder, [Line|SessionBlock]).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
parse_line([$v, $= | Version]) ->
  {version, list_to_integer(Version)};
  
parse_line([$o, $= | Line]) ->
  [Username,SessionId,VersionText,NetType,AddrType,Address] = 
    string:tokens(Line, [16#20]),
    
  Version = list_to_integer(VersionText),
  NetworkType = case NetType of "IN" -> internet end,
  AddressType = case AddrType of
      "IP4" -> ip4;
      "IP6" -> ip6
    end,
    
  {origin, {Username,SessionId,Version,NetworkType,AddressType,Address}};
    
parse_line([$s, $= | Line]) ->
  {name, Line};
  
parse_line([$u, $= | Line]) ->
  {uri, Line};

parse_line([$e, $= | Line]) ->
  {email, Line};

parse_line([$p, $= | Line]) ->
  {phone, Line};

parse_line([$z, $= | Line]) ->
  {timezone, Line};

parse_line([$k, $= | Line]) ->
  {encryption_key, Line};

parse_line([$a, $= | Line]) ->
  case Line of
    [$r,$t,$p,$m,$a,$p, $: | RtpMap] ->
      parse_rtp_map(RtpMap);
    
    [$f,$m,$t,$p, $: | FormatP ] ->
      parse_format_parameter(FormatP);
      
    [$c,$o,$n,$f,$i,$g, $: | Config] ->
      {config, Config};
      
    [$c,$o,$n,$t,$r,$o,$l,$: | Uri] ->
      {control, Uri};

    _ -> 
      {Name, Value} = stringutils:split_on_first($:, Line),
      {attribute, string:strip(Name), string:strip(Value)}
  end;
  
parse_line([$t, $= | Line]) ->
  {session_time, Line};
  
parse_line([$r, $= | Line]) ->
  {repeat_time, Line};

parse_line([$m, $= | MediaStream]) ->
  case string:tokens(MediaStream, [16#20]) of
    [MediaType,Port,Transport,Format] ->
      #media_stream{ type = media_type(MediaType),
                     port = Port, 
                     transport = Transport,
                     format = stringutils:to_int_def(Format,Format)};

    [MediaType,Port,Transport] ->
      #media_stream{ type = media_type(MediaType),
                     port = Port, 
                     transport = Transport}
  end;
  
parse_line([$i, $= | Line]) ->
  {stream_title, Line};
  
parse_line([$c, $= | Line]) ->
  case string:tokens(Line, [16#20, $/]) of
    [NetName,AddrName,Addr] ->
      Net = case NetName of 
        "IN" -> internet;
        _ -> NetName
      end,
      
      AddressType = case AddrName of 
        "IP4" -> ip4;
        "IP6" -> ip6;
        _ -> AddrName
      end,
      
      Address = case string:tokens(Addr, [$/]) of
        [Host]     -> #address{host=Host};
        [Host,TTL] -> #address{host=Host,ttl=TTL};
        [Host,TTL,Count] -> #address{host=Host,ttl=TTL,count=Count}
      end,
      
      {connection_info, {Net, AddressType, Address}}
  end;

parse_line([$b, $= | Line]) ->
  case string:tokens(Line, [$:]) of
    ["CT", Value] ->
      {bandwidth_info, conference_total, list_to_integer(Value)};
      
    ["AS", Value] -> 
      {bandwidth_info, application_specific, list_to_integer(Value)};
      
    [Modifier, Value] -> 
      {bandwidth_info, Modifier, list_to_integer(Value)}
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
collect_media_streams(Lines) ->
  lists:reverse(collect_media_streams(Lines, [])).

%% ----------------------------------------------------------------------------
%% @doc 
%% @end
%% ----------------------------------------------------------------------------
collect_media_streams([Stream|Lines], Streams) 
    when is_record(Stream, media_stream) ->
  
  StreamBlock = extract_stream_block(Lines),
  
  Attributes = extract_attributes(Lines),
    
  ControlUri = case lists:keyfind(control, 1, StreamBlock) of
    {control, Uri} -> Uri;
    false -> ""
  end,
  
  BandwidthInfo = case lists:keyfind(bandwidth_info, 1, StreamBlock) of
    false -> undefined;
    Any -> Any
  end,   
  
  StreamRecord = Stream#media_stream{
    control_uri = ControlUri, 
    bandwidth_info = BandwidthInfo},
    
  collect_media_streams(Lines, [StreamRecord | Streams]);
       
collect_media_streams([Line|Lines], Streams) ->
  collect_media_streams(Lines, Streams);

collect_media_streams([], Streams) ->
  lists:reverse(Streams).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_stream_block(Lines) ->
  lists:reverse(extract_stream_block(Lines, [])).
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------

extract_stream_block([], StreamBlock) ->
  StreamBlock;
  
extract_stream_block([Line|Lines], StreamBlock) 
    when is_record(Line, media_stream) ->
  StreamBlock;
  
extract_stream_block([Line|Lines], StreamBlock) ->
  extract_stream_block(Lines,[Line|StreamBlock]).
  
%% ----------------------------------------------------------------------------
%% @doc Parses an RtpMap attribute into a record
%% @end
%% ----------------------------------------------------------------------------
parse_rtp_map(Text) ->
  case string:tokens(Text, [16#20, $/]) of
    [Entry, Encoding, ClockRate] ->
      #rtp_map{id = list_to_integer(Entry), 
               encoding = Encoding,
               clock_rate = list_to_integer(ClockRate)};
    [Entry, Encoding, ClockRate, Options] ->
        #rtp_map{id = list_to_integer(Entry), 
                 encoding = Encoding,
                 clock_rate = list_to_integer(ClockRate),
                 options = Options}
  end.

%% ----------------------------------------------------------------------------
%% @doc Parses an SDP media type string
%% @spec media_type(MediaType) -> Result
%%       MediaType = string() | MediaTypeAtom
%%       Result = MediaTypeAtom | string()
%%       MediaTypeAtom = audio | video | application | data | control
%% @end
%% ----------------------------------------------------------------------------

media_type(MediaType) when is_list(MediaType) ->
  case string:to_lower(MediaType) of
    "audio" -> audio;
    "video" -> video;
    "application" -> application;
    "data" -> data;
    "control" -> control
  end;
  
media_type(MediaType) when is_atom(MediaType) ->
  case MediaType of 
    audio -> "audio";
    video -> "video";
    application -> "application";
    data -> "data";
    control -> "control"
  end. 
  
%% ----------------------------------------------------------------------------
%% Parses a FormatP parameter
%% @spec parse_format_parameter(FormatP) -> Result
%%       Result = {MediaId, Parameters}
%%       Parameters = [Parameter]
%%       Parameter = {Name, Value}
%%       Name = string()
%%       Value = string()
%% @end
%% ----------------------------------------------------------------------------

parse_format_parameter(FormatP) when is_list(FormatP) ->
  [MediaId | Params] = string:tokens(FormatP, [16#20, $;]),
  Parameters = 
    lists:map(
      fun(Param) -> stringutils:split_on_first($=, Param) end,
      Params),
  {format_p, list_to_integer(MediaId), Parameters}.
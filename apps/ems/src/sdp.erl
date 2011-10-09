-module (sdp).
-author ("Trent Clarke <trent.clarke@gmail.com>").
-export([parse/1, format/1]).
-include("sdp.hrl").

% =============================================================================
% Types
% =============================================================================

-type media_type() :: 'audio' | 'video' | 'application' | 'data' | 'control'.
-type session_description() :: #session_description{}.
-export_type([media_type/0, session_description/0]).

% =============================================================================
% Exported functions
% =============================================================================  

-spec parse(string() | binary()) -> {'ok', session_description()} | 'fail'.

parse(Body) when is_binary(Body) ->
  Text = utf:utf8_to_string(Body),
  parse(Text);
  
parse(Text) ->
  try
    Lines = lists:map(fun parse_line/1, re:split(Text, <<"\r\n">>, [trim, {return,list}])),
  
    SessionBlock = extract_til_stream(Lines),
    Attributes = extract_attributes(SessionBlock),
    {name, Name} = find_def(name, SessionBlock, undefined),
    {info, Info} = find_def(info, SessionBlock, undefined),
  
    RtpMap = extract_rtp_map(Lines),
    FormatMap = extract_format_map(Lines),
    Streams = collect_media_streams(Lines),
  
    Desc = #session_description{ name = Name,
                                 info = Info,
                                 attributes = Attributes,
                                 streams = Streams, 
                                 rtp_map = RtpMap, 
                                 format_map = FormatMap },
    {ok, Desc}
  catch
    fail -> fail
  end.
      
-spec format( session_description() ) -> binary().
format(Description) ->
  <<>>.
  
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
%% @doc Scans the semi-parsed list of SDP lines and builds a list of the 
%%      rtp_map records.
%% @spec extract_rtp_map(Lines) -> RtpMap
%%         Lines = [Line]
%%         Line = rtp_map() | media_stream() | term() 
%%         RtpMap = [RtpMapEntry]
%%         RtpMapEntry = {int(), rtp_map()}
%% @end
%% ----------------------------------------------------------------------------
extract_rtp_map(Lines) ->
  lists:map(
    fun(Item) -> 
      Id = Item#rtp_map.id,
      {Id, Item}
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
  [MediaType, Ports, Transport | Formats] = string:tokens(MediaStream, [16#20]),
  {media_stream, media_type(MediaType), 
                 ports(Ports),
                 Transport,
                 lists:map(fun list_to_integer/1, Formats)};
                    
parse_line([$i, $= | Line]) ->
  {info, Line};
  
parse_line([$c, $= | Line]) ->
  case string:tokens(Line, [16#20]) of
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
  end;
  
parse_line([]) -> {};

parse_line(_) -> throw(fail).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
collect_media_streams(Lines) ->
  lists:reverse(collect_media_streams(Lines, [])).

%% ----------------------------------------------------------------------------
%% @doc 
%% @end
%% ----------------------------------------------------------------------------
collect_media_streams([Line|Lines], Streams) 
    when element(1, Line) == media_stream ->

  {media_stream, Type, Ports, Transport, Formats} = Line,
  StreamBlock = extract_til_stream(Lines),
  Attributes = extract_attributes(StreamBlock),
  {control, ControlUri} = find_def(control, StreamBlock, ""),
  BwInfo = case lists:keyfind(bandwidth_info, 1, StreamBlock) of
             {bandwidth_info, Modifier, Rate} -> {Modifier, Rate};
             false -> undefined
           end,
  
  StreamRecord = #media_stream{ type = Type,
                               ports = Ports,
                               transport = Transport,   
                               formats = Formats,
                               attributes = Attributes,
                               control_uri = ControlUri, 
                               bandwidth_info = BwInfo },
  collect_media_streams(Lines, [StreamRecord | Streams]);

collect_media_streams([Line|Lines], Streams) ->
  collect_media_streams(Lines, Streams);
  
collect_media_streams([], Streams) -> Streams.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
find_def(Name, List, Default) ->
  case lists:keyfind(Name, 1, List) of
    false -> {Name, Default};
    Value -> Value    
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
extract_til_stream(Lines) -> lists:reverse(extract_til_stream(Lines, [])).

extract_til_stream([], StreamBlock) ->
  StreamBlock;
  
extract_til_stream([Line|Lines], StreamBlock) 
    when element(1, Line) =:= media_stream ->
  StreamBlock;
  
extract_til_stream([Line|Lines], StreamBlock) ->
  extract_til_stream(Lines,[Line|StreamBlock]).
  
%% ----------------------------------------------------------------------------
%% @doc Parses an RtpMap attribute into a record
%% @end
%% ----------------------------------------------------------------------------
parse_rtp_map(Text) ->
  case string:tokens(Text, [16#20, $/]) of
    [Entry, Encoding, ClockRate] ->
      #rtp_map{id = list_to_integer(Entry), 
               encoding = Encoding,
               clock_rate = list_to_integer(ClockRate),
               options = []};

    [Entry, Encoding, ClockRate, Options] ->
        #rtp_map{id = list_to_integer(Entry), 
                 encoding = Encoding,
                 clock_rate = list_to_integer(ClockRate),
                 options = Options}
  end.

%% ----------------------------------------------------------------------------
%% @doc Parses an SDP media type string
%% @end
%% ----------------------------------------------------------------------------
-spec media_type( atom() | string() ) -> media_type().
media_type(MediaType) when is_atom(MediaType) ->
  case MediaType of 
    audio -> "audio";
    video -> "video";
    application -> "application";
    data -> "data";
    control -> "control"
  end;

media_type(MediaType) when is_list(MediaType) ->
  case string:to_lower(MediaType) of
    "audio" -> audio;
    "video" -> video;
    "application" -> application;
    "data" -> data;
    "control" -> control
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
  
-spec ports(string()) -> [integer()].
ports(Ports) -> 
  case lists:splitwith(fun(X) -> X /= $/ end, Ports) of
    {Ps, []} -> [list_to_integer(Ps)];
    {Ps, [$/ | Ns]} -> P = list_to_integer(Ps),
                       N = list_to_integer(Ns),
                       lists:seq(P, (P+N)-1)
  end.
  
%% ----------------------------------------------------------------------------
%% Parses a FormatP parameter
%% @end
%% ----------------------------------------------------------------------------
-spec parse_format_parameter(string()) -> {'format_p', integer(), string()}.
parse_format_parameter(FormatP) when is_list(FormatP) ->
  {MediaId, Params} = stringutils:split_on_first(16#20, FormatP),
  {format_p, list_to_integer(MediaId), Params}.
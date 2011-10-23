-module(url).
-export([parse/1]).

%% ----------------------------------------------------------------------------
%% Data types
%% ----------------------------------------------------------------------------
-type scheme() :: 'rtsp' | 'http' | 'ftp' | 'file'.
-type error() :: 'unknown'.
% a semi-parsed URL with the same format as an http url
-type http_url() :: {scheme(), string(), integer(), string()}.
-export_type([scheme/0, error/0]).

%% ----------------------------------------------------------------------------
%% @doc parses an url
%% @end
%% ----------------------------------------------------------------------------
-spec parse(string()) -> http_url() | {error(), atom()}. 
parse([$h,$t,$t,$p,$:,$/,$/ | Text]) -> 
  parse_http(http, 80, Text);
  
parse([$r,$t,$s,$p,$:,$/,$/ | Text]) -> 
  parse_http(rtsp, 554, Text);
  
parse(_) -> {error,unknown}.  

%% ----------------------------------------------------------------------------
%% @doc Parses an http (or equivalently formated) url 
%% @end
%% ----------------------------------------------------------------------------  
-spec parse_http(scheme(), integer(), string()) -> http_url(). 
parse_http(Scheme, DefaultPort, Uri) ->
  case string:chr(Uri, $/) of
    0 ->
      %% no path component in the uri and the uri isn't terminated by a slash - 
      %% append the slash and try again 
      parse_http(Scheme, DefaultPort, Uri ++ "/");
    N -> 
      %% Everything between the start of the string and the first slash is the 
      %% host (possibly includng the port) - the rest is the path to the file.
      Host = string:substr(Uri, 1, N-1),
      Path = string:substr(Uri, N, length(Uri)),
      
      %% Check to see if the host string includes the port specification and
      %% parse it if necessary
      case string:chr(Host, $:) of
        0 -> 
          %% No port spec is present in the url - assume that it's port 80 and
          %% return it
          {Scheme, Host, DefaultPort, Path};
          
        M ->
          %% Port spec if present - extract it out and parse it, assuming port
          %% 80 if anything goes wrong.
          Server = string:substr(Host,1,M-1),
          PortStr = string:substr(Host,M+1,length(Host)),
          Port = stringutils:to_int_def(PortStr,80),
          {Scheme, Server, Port, Path}
      end
  end.

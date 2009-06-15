-module(url).
-export([parse/1]).

%% ----------------------------------------------------------------------------
%% @doc parses an url
%% @spec parse(Url) -> Result
%%       Url = string
%%       Result = {error, Reason} | {Scheme,Host,Port,Path}
%%       Scheme = http | rtsp | file | string()
%% @end
%% ----------------------------------------------------------------------------

parse([$h,$t,$t,$p,$:,$/,$/ | Text]) -> 
  parse_http(http, Text);
  
parse([$r,$t,$s,$p,$:,$/,$/ | Text]) -> 
  parse_http(rtsp, Text);
  
parse(_Url) -> 
  {error,unkown}.  

%% ----------------------------------------------------------------------------
%% @doc Parses an http url 
%% @end
%% ----------------------------------------------------------------------------  
parse_http(Scheme, Uri) ->
  case string:chr(Uri, $/) of
    0 ->
      %% no path component in the uri and the uri isn't terminated by a slash - 
      %% append the slash and try again 
      parse_http(Scheme, Uri ++ "/");
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
          {Scheme, Host, 80, Path};
          
        M ->
          %% Port spec if present - extract it out and parse it, assuming port
          %% 80 if anything goes wrong.
          Server = string:substr(Host,1,M-1),
          PortStr = string:substr(Host,M+1,length(Host)),
          Port = stringutils:to_int_def(PortStr,80),
          {Scheme, Server, Port, Path}
      end
  end.
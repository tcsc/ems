-module(rtsp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


%% @doc Called by the application framework to start the RTSP server 
%%      application.
%% @private
%% @end
start(normal, _StartArgs) ->
    rtsp_sup:start_link().

%% @doc Called by the application framework to stop the RTSP server 
%%      application. Explicitly kills the supervisor's children and 
%%      removes their specifications from the supervisor's child 
%%      list.
%% @todo Work out what to do about the supervisor itself.
%% @end  
stop(_) -> 
  Stop = fun({Id, _Pid, _, _}) ->
    supervisor:terminate_child(rtsp_sup, Id),
    supervisor:delete_child(rtsp_sup, Id)
  end,
  lists:foreach(Stop, supervisor:which_children(rtsp_sup)).

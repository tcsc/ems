-module (ems_channel).
-include ("erlang_media_server.hrl").
-include ("sdp.hrl").
-record (state, {pid, stream, rtpmap}).
-behaviour (gen_server).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3
	]).
	
-export ([start_link/2, subscribe/3, notify/2]).

%% ============================================================================
%% @doc Creates and starts a new media distribution channel
%% @spec new(StreamDesc) -> {ok, Pid, State} | error
%% @end
%% ============================================================================
start_link(Stream, RtpMap) ->
	?LOG_DEBUG("ems_channel:start_link/2", []),
	State = #state{stream = Stream, rtpmap=RtpMap},
	case gen_server:start_link(?MODULE, State, []) of
		{ok, Pid} -> {ok, Pid};
		Error -> 
			?LOG_ERROR("ems_channel:start_link/2 failed to start: ~w", [Error]),
			Error
	end.

%% ============================================================================
%% @doc Notifies all the subscribed event handlers of an event
%% @spec notify(State,Event) ->
%%         State = Opaque state data, as returned from start_link/1,
%%         Event = term()
%% @end 
%% ============================================================================
notify(_State = #state{pid=Pid}, Event) ->
	gen_event:notify(Pid, Event).
	
%% ============================================================================
%% @doc Creates a new event handler from the supplied module and binds it to
%%      the event manager.
%% @spec subscribe(State,Module,Args) ->
%%         State = Opaque state data as returned from start_link/1
%%         Module = 
%%         Args = list() 
%% @end
%% ============================================================================
subscribe(_State = #state{pid=Pid}, Module, Args) ->
	gen_event:add_handler(Pid, Module, Args).
	
%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
init(Args = #state{stream=Stream, rtpmap=RtpMap}) ->
  ?LOG_DEBUG("ems_channel:init/1 - starting channel for ~s", 
    [Stream#media_stream.control_uri]),
  {ok, Args}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	?LOG_DEBUG("ems_channel:handle_call/3",[]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @spec handle_cast(Request,From,State) -> {noreply,State}
%% @end
%% ----------------------------------------------------------------------------
handle_cast(_Request, State) ->
	?LOG_DEBUG("ems_channel:handle_cast/2 ~w",[_Request]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
handle_info(_Info, State) ->
	?LOG_DEBUG("ems_channel:handle_info/2",[]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
terminate(Reason, State) ->
	?LOG_DEBUG("ems_channel:terminate/2 - ~w",[Reason]),
	ok.
	
%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
code_change(_OldVersion, State, _Extra) ->
	?LOG_DEBUG("ems_channel:code_change/3",[]),
	{ok, State}.
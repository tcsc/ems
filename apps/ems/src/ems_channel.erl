-module (ems_channel).
-behaviour (gen_server).

-include ("sdp.hrl").
-record (state, {pid, stream, rtpmap, receiver}).

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
	
-export ([start_link/2, configure_input/3, stop/1]).

%% ============================================================================
%% @doc Creates and starts a new media distribution channel
%% ============================================================================
-spec start_link(Stream :: term(), RtpMap :: term()) ->
        {ok, pid(), term()} | error.

start_link(Stream, RtpMap) ->
	log:debug("ems_channel:start_link/2", []),
	State = #state{stream = Stream, rtpmap=RtpMap},
	case gen_server:start_link(?MODULE, State, []) of
		{ok, Pid} -> {ok, Pid};
		Error -> 
			log:err("ems_channel:start_link/2 failed to start: ~w", [Error]),
			Error
	end.

%% ============================================================================
%% @doc Configures the chanel with the given settings 
%% @end 
%% ============================================================================
-spec configure_input(pid(), term(), term()) -> {ok, term()}. 
configure_input(Pid, Transport, ClientAddress) ->
  try 
    log:debug("ems_channel:configure_input/3", []),
    gen_server:call(Pid, {configure, Transport, ClientAddress})
  catch
    exit:{timeout,_} -> {error, timeout};
	  _Type:Err -> {error, Err}
  end.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
stop(Pid) ->
  log:debug("ems_channel:stop/1", []),
  gen_server:cast(Pid, {stop_chanel, self()}).
  
%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
init(Args = #state{stream = Stream, rtpmap = _RtpMap}) ->
  log:debug("ems_channel:init/1 - starting channel for ~s", 
    [Stream#media_stream.control_uri]),
  process_flag(trap_exit, true),
  {ok, Args}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @spec handle_call(Request,From,State) -> {noreply, State}
%% @end
%% ----------------------------------------------------------------------------
handle_call({configure, TransportSpec, ClientAddress}, _From, State) ->
  log:debug("ems_channel:handle_call/3 - handling stream configure", []),

  {_,RtpInfo} = State#state.rtpmap,
  
  {RtpReceiverPid, ServerTransportSpec} = 
    rtp_receiver:start_link(RtpInfo#rtp_map.clock_rate, TransportSpec, ClientAddress),
                          
  log:debug("ems_channel:handle_call/3 - Server Transport Spec: ~w", [ServerTransportSpec]),  
  NewState = State#state{receiver = RtpReceiverPid},
  {reply, {ok, ServerTransportSpec}, NewState};
  
handle_call(_Request, _From, State) ->
	log:debug("ems_channel:handle_call/3",[]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen server in response to a cast (i.e. asynchronous)
%%      request.
%% @spec handle_cast(Request,State) -> {noreply,State}
%% @end
%% ----------------------------------------------------------------------------  
handle_cast(_Request, State) ->
	log:debug("ems_channel:handle_cast/2 ~w",[_Request]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
handle_info(enable, State) ->
	log:debug("ems_channel:handle_info/2 - starting rtp receiver",[]),
	rtp_receiver:enable(State#state.receiver),
	{noreply, State};

handle_info(_Info, State) ->
	log:debug("ems_channel:handle_info/2 - ~p",[_Info]),
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
terminate(Reason, _State) ->
	log:debug("ems_channel:terminate/2 - ~w",[Reason]),
	ok.
	
%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------	
code_change(_OldVersion, State, _Extra) ->
	log:debug("ems_channel:code_change/3",[]),
	{ok, State}.

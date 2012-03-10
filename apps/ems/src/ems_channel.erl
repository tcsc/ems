-module (ems_channel).
-behaviour (gen_server).
-include ("sdp.hrl").

% gen_server exports ----------------------------------------------------------
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3
	]).
	
% public API exports ----------------------------------------------------------
-export ([start_link/3,
          get_path/1,
          configure_input/3, 
          activate/1, 
          stop/1]).

%% ============================================================================
%% Type definitions
%% ============================================================================
-record (state, {parent :: ems:session(), 
                 stream :: sdp:stream(), 
                 rtp_map :: sdp:rtp_map(),
                 uri :: string(),
                 receiver :: term()}).

-type state() :: #state{}.

%% ============================================================================
%% Public API 
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Creates and starts a new media distribution channel
%% @end
%% ----------------------------------------------------------------------------
-spec start_link(Stream :: sdp:media_stream(),
                 RtpMap :: [sdp:rtp_map()],
                 Parent :: ems:session()) ->
        {ok, pid(), term()} | error.

start_link(Stream, RtpMap, Parent) ->
	log:debug("ems_channel:start_link/3 - creating channel"),
	State = #state{parent = Parent, stream = Stream, rtp_map = RtpMap},
	case gen_server:start_link(?MODULE, State, []) of
		{ok, Pid} -> {ok, Pid};
		Error -> 
			log:err("ems_channel:start_link/2 failed to start: ~w", [Error]),
			Error
	end.

%% ----------------------------------------------------------------------------
%% @doc Configures the channel with the given settings. I still need to work 
%%      out exactly what it is I need to know at this point... 
%% @end 
%% ----------------------------------------------------------------------------
-spec configure_input(pid(), term(), term()) -> {ok, term()}. 

configure_input(Pid, Transport, ClientAddress) ->
  try 
    log:debug("ems_channel:configure_input/3", []),
    gen_server:call(Pid, {configure, Transport, ClientAddress})
  catch
    exit:{timeout,_} -> {error, timeout};
	  _Type:Err -> {error, Err}
  end.

-spec activate(Channel :: ems:channel()) -> ok | {error, Reason::term()}.
activate(Channel) ->
  gen_server:call(Channel, activate).
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
stop(Channel) ->
  log:debug("ems_channel:stop/1", []),
  gen_server:cast(Channel, {stop_chanel, self()}).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
get_path(Channel) ->
  {ok, Path} = gen_server:call(Channel, get_path),
  Path.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
init(Args = #state{uri = Uri}) ->
  log:debug("ems_channel:init/1 - starting channel for ~s", [Uri]),
  {ok, Args}.

%% ----------------------------------------------------------------------------
%% @doc Called by the gen_server in response to a call message. Does nothing at 
%%      this point.
%% @end
%% ----------------------------------------------------------------------------
-type call_request() :: {configure, [term()], term()} | activate.
-spec handle_call(call_request(), From :: pid(), State :: state()) ->
  {reply, Reply :: term(), NewState :: state()}.

handle_call({configure, TransportSpec, ClientAddress}, _From, State) ->
  log:debug("ems_channel:handle_call/3 - handling stream configure", []),

  {_,RtpInfo} = State#state.rtp_map,
  
  {RtpReceiverPid, ServerTransportSpec} = 
    rtp_receiver:start_link(RtpInfo#rtp_map.clock_rate, 
                            TransportSpec, 
                            ClientAddress),
                          
  log:debug("ems_channel:handle_call/3 - Server Transport Spec: ~w", 
            [ServerTransportSpec]),  
  NewState = State#state{receiver = RtpReceiverPid},
  {reply, {ok, ServerTransportSpec}, NewState};

% Handles an activation request from a client
handle_call(activate, _From, State) ->
  {reply, ok, State};

handle_call(get_path, _From, State) ->
  Stream = State#state.stream,
  Path = Stream#stream.control_uri,
  {reply, {ok, Path}, State};

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

-module(ems_session).
-behaviour(gen_server).
-include("erlang_media_server.hrl").

%% ============================================================================
%% Definitions
%% ============================================================================
-record(session_state, {id, path, description}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================
-export([init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
	]).
	
-export([
  start_link/3,
  setup_stream/3]).

%% ============================================================================
%% Exports
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts a new session and returns the new session's process identifier.
%% @spec start_link(Id, Path, Desc) -> Pid
%%       Id = integer()
%%       Path = string()
%%       Desc = SDP session  description
%% @end
%% ----------------------------------------------------------------------------
start_link(Id, Path, Desc) ->
  ?LOG_DEBUG("ems_session:start_link/2 - Id: ~w, Path: ~s", [Id, Path]),
  gen_server:start_link(?MODULE, {Id, Path, Desc}, []).
  
%% ----------------------------------------------------------------------------
%% @doc Sets up a new stream, returning a new transport definition for the
%%      caller to return to the client.
%% @spec stream_setup(SessionPid, StreamName, Transport) -> Result
%% @end
%% ----------------------------------------------------------------------------
setup_stream(SessionPid, StreamName, Transport) ->
  ?LOG_DEBUG("ems_session:setup_stream/3 ~s", [StreamName]),
  try
    gen_server:call(SessionPid, {setup_stream, StreamName, Transport})
  catch
    exit:{timeout,_} -> {error, timeout};
    _Type:Err -> {error, Err}
  end.
  
%% ============================================================================
%% Server Callbacks
%% ============================================================================

init({Id, Path, Desc}) ->
  ?LOG_DEBUG("ems_session:init/2 - ~s", [Path]),
  State = #session_state{id=Id, path=Path, description=Desc},
  {ok, State}.
  
%% ----------------------------------------------------------------------------
%% @doc Handles a synchronous request from another process.
%% @spec handle_call(Request, From, State) -> Result
%%       Request =
%%       From = pid()
%%       State = 
%%       Result = {reply, Reply, NewState}
%% @end 
%% ----------------------------------------------------------------------------

% handles a stream setup request from a client
handle_call({setup_stream, StreamName, Transport}, From, State) ->
  ?LOG_DEBUG("ems_session:handle_call/3 - Setting up stream ~s with transport ~w",
    [StreamName, Transport]),
    
  try
    {Result, NewState} = case lists:keysearch(direction, 1, Transport) of
      {value, {direction, inbound}} -> 
        setup_stream(inbound, StreamName, Transport, State);
        
      {value, {direction, outbound}} -> 
        setup_stream(outbound, StreamName, Transport, State);
        
      _ -> 
        throw({ems_session,unsupported_transport})
    end,
    {reply, Result, NewState}
  catch
    {ems_session, Reason} -> {reply, {error, Reason}, State}
  end;

%% The default implementation. Swallows the message.
handle_call(Request, From, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
handle_cast(Request, State) ->
  {noreply, State}.
  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
handle_info(Info, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
terminate(Reason, State) ->
  ok.

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------    
code_change(OldVersion, State, Extra) ->
  {ok, State}.
  
%% ============================================================================
%% Internal functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Sets up an RTP data stream
%% @spec setup_stream(Direction, StreamName, Transport, State) -> Result
%%       Direction = inbound | outbound
%%       StreamName = string()
%%       Transport = list()
%%       State = term()
%%       Result = {ServerTransport, NewState}
%% @end
%% ----------------------------------------------------------------------------  

% Handles the inbound stream case - setting up the stream manager and getting it
% ready to receive RTP data from the broadcaster
setup_stream(inbound, StreamName, TransportSpec, State) ->
  case fetch_stream(StreamName, State) of 
    {ok, _} -> throw({ems_session,already_exists});
    false -> ok
  end,
  
  {Transport, ServerSpec} = case transport_type(TransportSpec) of
    unicast ->
      T = udp_transport:new(TransportSpec),
      S = udp_transport:get_spec(T),
      {T,S};
    _ -> 
      throw({ems_server,unsupported_transport})
  end,
  {ServerSpec, State};
  
setup_stream(outbound, StreamName, Transport, State) ->
  {[], State}.

%% ---------------------------------------------------------------------------- 
%%
%% ----------------------------------------------------------------------------    
fetch_stream(StreamName, State) ->
  false.
  
%% ---------------------------------------------------------------------------- 
%% @spec transport_type(TrasnportSpec) -> Result
%%       Result = interleaved | unicast | multicast
%% @end
%% ----------------------------------------------------------------------------    
transport_type(TransportSpec) ->
  case lists:keymember(interleaved, 1, TransportSpec) of
    true -> interleaved;
    false -> case lists:member(unicast) of 
      true -> unicast;
      false -> case lists:member(multicast) of
        true -> multicast
      end
    end
  end.    
-module (ems_config).
-include("logging.hrl").
-include("common.hrl").

%% Client-facing exports
-export([load/1, get_config/1, get_mount_point/2, get_user_info/2, get_user_rights/3]).

%% ============================================================================
%% Types & Type exports for the dialyzer
%% ============================================================================
-type cookie() :: any().
-type handle() :: {cookie(), module()}. 
-export_type([handle/0]).
-opaque([handle/0]).

%% ============================================================================
%% Public API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Loads an erlang source file as a configuration file, compiles it and 
%%      performs some basic validation on the resulting code module.
%% @end
%% ----------------------------------------------------------------------------

-spec load(Path :: string()) -> {ok, handle()} | {error, any()}.

load(Path) ->
	?LOG_DEBUG("config:load/1 - Compiling local configuration: ~s", [Path]),
	try
		{Module, ObjectCode} = 
			case compile:file(Path, [binary, native, return_errors]) of
				{ok, M, Obj} -> {M, Obj};
			  {error, Es, Ws} -> throw({compile_error, Es, Ws})
	    end,

		?LOG_DEBUG("config:load/1 - Loading local configuration module: ~w", [Module]),					
		case code:load_binary(Module, Path, ObjectCode) of
			{module, Module} -> ok;
			{error, Le} -> throw ({load_error, Le})
		end,
		
		?LOG_DEBUG("config:load/1 - Verifying local configuration", []),
		Exports = Module:module_info(exports),
		lists:foreach( fun(F) -> case lists:member(F, Exports) of 
				                   false -> throw ({validation_error, F}); 
				                   true -> ok
				                 end
					   end, 
					   [{init,0}, {get_config,1}, {get_mount_point,2}, {get_user,2}, {get_rights,3}]),
		
		?LOG_DEBUG("config:load/1 - Running configuration initialiser...", []),
		case Module:init() of
			{ok, Cookie} -> {ok, {Cookie, Module}};
			{error, Ie} -> throw ({init_error, Ie})
		end
	catch 
		throw:{compile_error, Errs, _} -> 
			?LOG_ERROR("config:load/1 - Failed to compile", []),
			lists:foreach(fun(E) -> log_compiler_error(E) end, Errs),
			{error, fail};

		throw:{load_error, Err} -> 
			?LOG_ERROR("config:load/1 - Failed to load: ~w", [Err]),
			{error, fail};
			
		throw:{validation_error, {N,A}} -> 
			?LOG_ERROR("config:load/1 - Missing required callback: ~w/~w", [N,A]),
			{error, fail};
			
		throw:{init_error, Err} -> 
			?LOG_ERROR("config:load/1 - Failed to initialise configuration: ~w", [Err]),
			{error, fail}
	end.

%% ----------------------------------------------------------------------------
%% @doc Fetches the global configuration from the local config unit and returns
%%      it to the caller.
%% @end
%% ----------------------------------------------------------------------------

-spec get_config(handle()) -> ems:config().

get_config({Cookie,Module}) -> Module:get_config(Cookie).

%% ----------------------------------------------------------------------------
%% @doc Fetches the information about a mount point from the local 
%%      configuration module and returns it to the caller.
%% @end
%% ----------------------------------------------------------------------------

-spec get_mount_point(handle(), Name :: string()) ->
  {'ok', ems:mount_point()} | 'false'.
  
get_mount_point({Cookie, Module}, Name) -> 
  Module:get_mount_point(Cookie, Name).

%% ----------------------------------------------------------------------------
%% @doc Fetches a given user's information from the local configuration module 
%%      and retrns it to the caller.
%% @end
%% ----------------------------------------------------------------------------

-spec get_user_info(handle(), Name :: string()) -> 
  {'ok', ems:user_info()} | 'false'.

get_user_info({Cookie, Module}, Name) ->
  Module:get_user(Cookie, Name).

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------  
-spec get_user_rights( Config     :: handle(), 
                       User       :: ems:user_info() | integer(),
                       MountPoint :: ems:mount_point() | integer() ) -> [atom()].
                       
get_user_rights(Config, #user_info{id = UserId}, 
                        #mount_point{id = MountPointId}) ->
  get_user_rights(Config, UserId, MountPointId);
  
get_user_rights(Config, #user_info{id = UserId}, MountPointId) ->
  get_user_rights(Config, UserId, MountPointId);  

get_user_rights(Config, UserId, #mount_point{id = MountPointId}) ->
  get_user_rights(Config, UserId, MountPointId);  

get_user_rights({Cookie, Module}, UserId, MountPointId) ->
  Module:get_rights(Cookie, UserId, MountPointId).
  
%% ============================================================================
%% Private utilities
%% ============================================================================

log_compiler_error( {File, FileErrors} ) ->
	lists:foreach( fun({Line, Module, Desc}) -> log_line_error(File, Line, Module, Desc) end, 
	               FileErrors).
	
log_line_error(File, Line, Module, Desc) ->
	?LOG_ERROR("\t~s:~w ~s", [File, Line, Module:format_error(Desc)]).

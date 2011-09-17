-module (ems_config).
-include("erlang_media_server.hrl").
-include("config.hrl").

%% Client-facing exports
-export([load/1, get_config/1]).

-type config_handle() :: {any(), module()}.

-spec load(FileName::string()) -> {ok, config_handle()} | {error, any()}.
load(Path) ->
	?LOG_DEBUG("config:load/1 - Compiling local configuration: ~s", [Path]),
	try
		{Module, ObjectCode} = case compile:file(Path, [binary, native, return_errors]) of
								 {ok, M, Obj} -> {M, Obj};
								 {error, Es, Ws} -> throw({compile_error, Es, Ws})
							   end,

		?LOG_DEBUG("config:load/1 - Loading local configuration: ~w", [Module]),					
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
					   [{init,0}, {get_config,1}]),
		
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
	
log_compiler_error( {File, FileErrors} ) ->
	lists:foreach( fun({Line, Module, Desc}) -> log_line_error(File, Line, Module, Desc) end, 
	               FileErrors).
	
log_line_error(File, Line, Module, Desc) ->
	?LOG_ERROR("\t~s:~w ~s", [File, Line, Module:format_error(Desc)]).

-spec get_config(config_handle()) -> ems_config().
get_config({Cookie,Module}) -> Module:get_config(Cookie).

-record(mount_point, { id :: integer(),
                       path :: string(),
                       name :: string(), 
											 description :: string() }).

-record(user_info, { id :: integer(),
									   login :: string(),
										 password :: string(),
										 name :: string() }).

-type ems_config() :: any().
-type user_rights() :: 'view' | 'broadcast'.
-type user_info() :: #user_info{}.
-type mount_point() :: #mount_point{}.
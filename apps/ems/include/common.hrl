
-record(mount_point, { id :: integer(),
                       path :: string(),
                       name :: string(), 
											 description :: string() }).

-record(user_info, { id :: integer(),
									   login :: string(),
										 password :: string(),
										 name :: string() }).

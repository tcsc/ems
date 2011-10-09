-record(digest_ctx, { nonce   :: string(), 
                      opaque  :: string(),
                      created :: calendar:t_now(),
                      touched :: calendar:t_now() }).

-record (basic, {}).
-record (digest, { user_name    :: string(),
                   realm        :: string(),
                   nonce        :: string(),
                   uri          :: string(),
                   qop          :: 'auth' | 'auth_int',
                   nonce_count  :: string(),
                   client_nonce :: string(),
                   response     :: string(),
                   opaque       :: string() }).
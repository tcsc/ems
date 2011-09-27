-record(digest_ctx, { nonce   :: string(), 
                      opaque  :: string(),
                      created :: calendar:t_now(),
                      touched :: calendar:t_now() }).

-record(digest_ctx, { nonce   :: rstp_digest_server:nonce(), 
                      created :: calendar:t_now(),
                      touched :: calendar:t_now() }).

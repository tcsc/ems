-type timestamp() :: non_neg_integer().
-type sequence_number() :: non_neg_integer().

-record(rtp_packet, {timestamp            :: timestamp(), 
                     sync_src             :: non_neg_integer(), 
                     extension_headers    :: boolean(), 
                     marker               :: 0 | 1, 
                     payload_type         :: non_neg_integer(), 
                     sequence             :: sequence_number(),
                     contributing_sources = 0 :: integer(),
                     payload              :: binary()}).
                     
-type rtp_packet() :: #rtp_packet{}.
                
-record(rtp_packet, {timestamp, 
                     sync_src, 
                     extension_headers, 
                     marker, 
                     payload_type, 
                     sequence,
                     contributing_sources,
                     payload}).
-define (RTCP_SENDER_REPORT,   200).
-define (RTCP_RECEIVER_REPORT, 201).
-define (RTCP_SDES,            202).
-define (RTCP_BYE,             203).
-define (RTCP_APP,             204).

-record(rtcp_rr, {sync_src, 
                  loss_fraction, 
                  packets_lost, 
                  highest_sequence, 
                  jitter, 
                  last_sr, 
                  last_sr_timestamp}).

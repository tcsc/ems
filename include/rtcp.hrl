-define (RTCP_SENDER_REPORT,   200).
-define (RTCP_RECEIVER_REPORT, 201).
-define (RTCP_SDES,            202).
-define (RTCP_BYE,             203).
-define (RTCP_APP,             204).

-define (RTCP_SDES_END,   0).
-define (RTCP_SDES_CNAME, 1).
-define (RTCP_SDES_NAME,  2).
-define (RTCP_SDES_EMAIL, 3).
-define (RTCP_SDES_PHONE, 4).
-define (RTCP_SDES_LOC,   5).
-define (RTCP_SDES_TOOL,  6).
-define (RTCP_SDES_NOTE,  7).
-define (RTCP_SDES_PRIV,  8).

-record (rtcp_sr, {sync_src,
                  ntp_time,
                  rtp_time,
                  packet_count,
                  octet_count}).
                  
-record (rtcp_sdes, {sync_src,
                     items}).

-record (rtcp_rr, {sync_src, 
                   loss_fraction, 
                   packets_lost, 
                   highest_sequence, 
                   jitter, 
                   last_sr, 
                   last_sr_timestamp}).

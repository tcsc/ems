%% ----------------------------------------------------------------------------
%% Common definitions
%% ----------------------------------------------------------------------------
-type timestamp() :: non_neg_integer().
-type wall_clock_time() :: non_neg_integer().
-type ntp_timestamp() :: non_neg_integer().
-type sequence_number() :: non_neg_integer().

%% ----------------------------------------------------------------------------
%% RTCP definitions
%% ----------------------------------------------------------------------------
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

%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-record (rtcp_sr, {sync_src     :: non_neg_integer(),
                   ntp_time     :: ntp_timestamp(),
                   rtp_time     :: timestamp(),
                   packet_count :: non_neg_integer(),
                   octet_count  :: non_neg_integer()
                   }).
-type rtcp_sr() :: #rtcp_sr{}.
                  
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-record (rtcp_sdes, {sync_src :: non_neg_integer(),
                     items    :: [{term(), string()}]
                    }).
-type rtcp_sdes() :: #rtcp_sdes{}.
                     
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------
-record (rtcp_rr, {sync_src, 
                   loss_fraction, 
                   packets_lost, 
                   highest_sequence, 
                   jitter, 
                   last_sr, 
                   last_sr_timestamp}).
-type rtcp_rr() :: #rtcp_rr{}.
                   
%% ----------------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------------   
-type rtcp_packet() :: rtcp_sr() | rtcp_sdes() | rtcp_rr().

-record(rtp_packet, {timestamp            :: timestamp(), 
                     sync_src             :: non_neg_integer(), 
                     extension_headers    :: boolean(), 
                     marker               :: 0 | 1, 
                     payload_type         :: non_neg_integer(), 
                     sequence             :: sequence_number(),
                     contributing_sources = 0 :: integer(),
                     payload              :: binary()}).
                     
-type rtp_packet() :: #rtp_packet{}.
                
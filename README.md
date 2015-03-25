# ems
An attempt at a media server in Erlang

# history
This was written ages ago as an experiment in Erlang, before I knew of erlyvideo 
(which has since become something else). From memory there is a relatively complete 
RTSP implementation, with a nearly complete implemention of RTP.

# If I knew then ...
This code is pretty process-heavy; at the time I was so used to the OO paradigm that I 
was basically treating processes as objects, and almost anything that manipulated data
was turned into a `gen_server` of some kind. 

If I were to resuuect this today, I'd probably try and think a bit harder about 
what actually needed to be a process and what didn't.

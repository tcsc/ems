{application, 'rtsp',
 [
  % A quick description of the application.
  {description, "An RTSP server"},
  {vsn, 0.1},
  {modules, [rtsp, rtsp_sup, rtsp_server, rtsp_authentication, rtsp_digest_server]},
  {applications, [kernel, stdlib, listener]},
  {registered, [rtsp_server]},
  {mod, {rtsp, []}}
 ]
}.


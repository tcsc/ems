{application, 'ems',
 [
  % A quick description of the application.
  {description, "An Erlang Application."},

  % The version of the applicaton. This is automatically populated by OTP Base
  {vsn, 0.1},

  {modules, [config,
			 ems, 
             ems_channel,
			 ems_logger,
			 ems_media_stream,
			 ems_server,
			 ems_session,
			 ems_session_manager,
			 ems_supervisor,
			 rtcp,
			 rtp,
			 rtp_receiver,
			 rtsp,
			 rtsp_connection,
			 rtsp_server,
			 sdp,
			 stringutils,
			 url,
			 utf]},

  % This is a list of the applications that your application depends on. This list must be 
  % filled out carefully so that dependency resolution systems can function properly.
  {applications, [kernel, stdlib, listener]},

  % A list of the registered processes in your application.  Used to prevent collisions. 
  {registered, [ems_server]},

  % The Module and Args used to start this application.
  % Uncomment this and fill it out if you wish to start 
  % your application via the otp framework.
  % Note* if you uncomment this make sure to add a comma after 
  %       tuple above.
  {mod, {ems, []}}
 ]
}.


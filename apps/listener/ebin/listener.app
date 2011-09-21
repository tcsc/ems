{application, 'listener',
 [
  % A quick description of the application.
  {description, "A generic TCP socket listener "},

  % The version of the applicaton.
  {vsn, 0.1},

  {modules, [listener_app, listener]},

  % This is a list of the applications that your application depends on. This list must be 
  % filled out carefully so that dependency resolution systems can function properly.
  {applications, [kernel, stdlib]},

  % A list of the registered processes in your application.  Used to prevent collisions. 
  {registered, [tcp_listener]},

  % The Module and Args used to start this application.
  % Uncomment this and fill it out if you wish to start 
  % your application via the otp framework.
  % Note* if you uncomment this make sure to add a comma after 
  %       tuple above.
  {mod, {listener_app, []}}
 ]
}.


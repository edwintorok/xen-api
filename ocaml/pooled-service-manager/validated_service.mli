open Types

(** A {!Service} that validates the configuration prior to performing [start] and [reload] actions.

  Although the configuration module should ensure that the configuration is
  always syntactically valid, there are other checks that can be done on the
  configuration as a whole (e.g. is it missing an IP address configuration
  entry).

  This is especially important for [reload] where we don't want to disrupt a
  working service with a new configuration that is not valid, if it can be
  easily prevented.

  [stop] is also changed to perform a forced stop if the supplied configuration
  is not valid ([stop] may need a valid configuration if the service is part of a
  pool to perform a graceful stop).
*)
module Make (Svc : Service) : Service with type Config.t = Svc.Config.t

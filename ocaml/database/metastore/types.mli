type error = [Rresult.R.msg | Rresult.R.exn_trap ]
(** an error message, or an exception with a backtrace *)

type ('a, +'b) action = Id.t -> 'a -> 'b
(** [an action taking ['a] as input and returning ['b] on instance [id] *)

(** the level of service health monitoring *)
type level =
  | Readonly (** query only, without the service noticing in any way.
   E.g. for monitor an appropriate read-only check would be process running
   state, and socket existence.

      Equivalent to [OCF_CHECK_LEVEL=0]
   *)
  | Active (** an active health check - e.g. query the service through its protocol, validate that ports can be bound to in validate. Should be a lightweight check.
  Equivalent to [OCF_CHECK_LEVEL=10].
  *)
  | Thorough (** a more detailed health check, not to be run too often
    Equivalent to [OCF_CHECK_LEVEL=20].
  *)

(** Actions that can be performed on a service. All actions must be idempotent. *)
module type Action = sig
  type 'a config (** configuration type of the service *)

  val equal_config: 'a config -> 'a config -> bool
  (** [equal_config a b] compares 2 configurations for equality. *)

  type metadata (** metadata about the service *)

  type diagnostic (** diagnostic data about a running service *)

  val start_exn: (_ config, unit) action
  (** [start_exn id config] starts an instance of the service with the specified configuration.

        Upon return the service must be immediately ready to service requests:
          any network ports / sockets must be bound and ready to accept connections.
    *)

  val stop_exn: (_ config option, unit) action
  (** [stop_exn id config_opt] ensure the instance is not running.

      @param config_opt when [None] a forced stop should be performed,
        otherwise a graceful stop should be attempted first.
    *)

  val monitor_exn: (level, diagnostic option) action
  (** [monitor_exn id level] checks the [id] service's current state.
      It does not alter the service's current state, and the service remains
      available for other operation while its state is checked.

    @param level
    {ul
    {- [ReadOnly] check process running state and socket existence}
    {- [Active] is a lightweight check through the service's protocol.}
    {- [Thorough] is a heavy check, not to be run too often}
    }

    @return None when the service is not running, [Some diagnostic] otherwise
   *)

  val metadata_exn: unit -> metadata
  (** [metadata_exn ()] queries {!type:metadata} about the service's implementation *)
end

(** Actions that are implementable as no-ops or composing {!module:Action}s as primitives.
    All actions must be idempotent.
 *)
module type OptionalAction = sig
  type 'a config (** configuration type of the service *)

  val validate_exn: (_ config * level, unit) action
  (** [validate_exn id (config, level)] checks that the [config] is valid for [id].

    @param level
    {ul
     {- [ReadOnly] checks that the config fields are consistent, where that is
        not already ensured by construction through types (e.g. dependencies between
        fields)}
     {- [Active] checks whether ports that are configured to be listened on are available}
     {- [Thorough] other more heavy checks, e.g. that data on disk is not corrupted}
    }
   *)

  val reload_exn: (_ config, unit) action
  (** [reload_exn id config] ensures the service is running with the new configuration *)
end

(** Mandatory and optional actions for a service *)
module type FullAction = sig
  include Action
  include OptionalAction with type 'a config := 'a config
end

(** result type for service actions *)
type ('a, 'e) result = ('a, [> error] as 'e) Result.t

module type Service = sig
  type 'a config (** service configuration *)

  type diagnostic  (** diagnostic information about a service *)

  val get_state: Id.t -> level -> (diagnostic option, _) result
  (** [get_state instance level] retrieves the current state of the instance [id].
      The detail of checking is specified by [level], see {!type:level}.

    @returns [Ok None] if the instance is stopped
    @returns [Ok (Some diagnostic)] if the instance is running with [config]
    @returns [Error err] if we failed to determine status
   *)

  val set_state: Id.t -> 'a config option -> (unit, _) result
  (** [set_state id desired_config] changes the state of instance [id] to
  [desired_config] on the current host.

    It will attempt to perform minimally disruptive changes:
      * invalid configurations will be rejected early where possible
      * configuration will be reloaded through runtime reconfiguration where
        possible
      * service will be stopped and started as needed
      * service stop will be forced if a regular stop fails

    @param desired_config when None the instance will be stopped
    @param desired_config when [Some config] the instance will be reloaded or
      started to match [config]

    @returns [Ok ()] when successful
    @returns [Error err] when changing state failed
  *)

end

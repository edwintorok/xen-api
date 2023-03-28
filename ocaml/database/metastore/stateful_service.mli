(**
  See {{!page-metastore_design.section-"Service-interface"} the design}.
*)

open Types

val  run_path: Fpath.t ref
(** [run_path] is the path used to store service runtime state. *)

(** Fallback(Basic) implements all [OptionalAction] actions using [Basic] actions, or as no-ops.

  Recommended usage:

    {[module MyService = struct
      include Fallback(struct
        let start_exn ... =

        ...
      end)

      let <override_optional_action1> = ...
    end
    }]

 *)
module Fallback(A: Action): FullAction with type config = A.config

(** Make(Actions) creates a service that can [execute] the specified action. *)
module Make(A: FullAction) : Service with type config = A.config

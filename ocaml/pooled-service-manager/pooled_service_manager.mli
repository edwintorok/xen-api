open Types
open Errors

module MakeLocal
    (Conf : MemberConfig)
    (L : Lifecycle) (B : sig
      val build_config : Conf.t -> L.Config.t
      (** [build_config memberconfig] given a {!MemberConfig.Local.t} and {!MemberConfig.Global.t} configuration
    constructs a configuration that can be used to start the local member *)
    end) : Lifecycle with type Config.t = Conf.t

module MakeGlobal
    (Conf : MemberConfig)
    (L : Lifecycle with type Config.t = Conf.t) (B : sig
      val build_global_config :
        Conf.Global.t -> Conf.Local.t Map.Make(L.Id).t -> Conf.Global.t
      (** [build_global_config settings localmap]
    constructs a global configuration given a map of member {!Id.t} to their
    {!Local.t} configuration.
    The constructed global configuration will typically include a list of all members.

    @param settings global configuration that does not depend on member list
  *)
    end) :
  StatefulService
    with type Id.t = Set.Make(L.Id).t
     and type Config.t = Conf.Global.t * Conf.Local.t Map.Make(L.Id).t
     and type ValidConfig.t = (L.ValidConfig.t, error) result Map.Make(L.Id).t

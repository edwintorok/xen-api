open Types

module Make (L : Lifecycle) :
  StatefulService
    with type Id.t = L.Id.t
     and type Config.t = L.Config.t
     and type ValidConfig.t = L.Config.t * L.ValidConfig.t
     and type 'a Task.t = 'a L.Task.t

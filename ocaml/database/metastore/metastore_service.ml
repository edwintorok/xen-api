type ('a, 'e) result = ('a, [> Rresult.R.msg | Rresult.R.exn_trap] as 'e) Result.t

let template = "xapi-etcd"
let service_of_instance instance =
  Printf.sprintf "%s@%s" template instance

let set_state ~instance =
  Rresult.R.trap_exn @@ fun desired ->
  let instance = Id.to_string instance in
  let service = service_of_instance instance in
  match desired with
  | None ->
      let (_:Fe_systemctl.status) = Fe_systemctl.stop ~service in
      ()
  | Some conf ->
      let env = Config.to_dict conf in
      Fe_systemctl.set_properties ~env ~service ();
      if Fe_systemctl.is_active ~service then
        let (_:Fe_systemctl.status) = Fe_systemctl.stop ~service in ();
      Fe_systemctl.start_templated ~template ~instance
      (* TODO: health check *)

(* round to power of 2 *)
let all_possible_tests = [(3, 30); (3, 20); (3, 12); (3, 0)]

let round_down_to_pow2 ~round_pow2 size =
  Int64.(shift_left (shift_right size round_pow2) round_pow2)

let one rpc session_id host host_free vm (n, round_pow2) =
  let n = Int64.of_int n in
  let rec adjust_for_overhead iter overhead =
    let value_raw = Int64.(div (sub host_free (mul n overhead)) n) in
    let value = round_down_to_pow2 ~round_pow2 value_raw in
    Printf.printf
      "Host free: %Ld, overhead: %Ld, n: %Ld, VM memory: %Ld, VM memory \
       rounded to 2^%d: %Ld\n%!"
      host_free overhead n value_raw round_pow2 value ;
    Client.Client.VM.set_memory ~rpc ~session_id ~self:vm ~value ;
    let overhead =
      Client.Client.VM.compute_memory_overhead ~rpc ~session_id ~vm
    in
    Printf.printf "VM memory: %Ld, overhead: %Ld\n%!" value overhead ;
    (* by changing the memory size, the overhead changes too.
       For now do a few iterations *)
    let iter = iter - 1 in
    if iter > 0 then
      adjust_for_overhead iter overhead
  in
  Client.Client.VM.set_VCPUs_max ~rpc ~session_id ~self:vm ~value:6L ;
  Client.Client.VM.set_VCPUs_at_startup ~rpc ~session_id ~self:vm ~value:6L ;
  adjust_for_overhead 3 0L ;
  let n = Int64.to_int n in
  Printf.printf "Creating %d clones\n%!" n ;
  let clones =
    List.init n @@ fun i ->
    let new_name = Printf.sprintf "memtest-clone-%d" i in
    Client.Client.VM.clone ~rpc ~session_id ~vm ~new_name
  in

  Printf.printf "Starting %d VMs in parallel\n%!" n ;
  let tasks =
    List.map
      (fun vm ->
        Client.Client.Async.VM.start_on ~rpc ~session_id ~vm ~host
          ~start_paused:true ~force:false
      )
      clones
  in
  Printf.printf "Waiting for %d VM starts to finish\n%!" n ;
  Tasks.wait_for_all ~rpc ~session_id ~tasks ;

  Printf.printf "Hard rebooting %d VMs in parallel\n%!" n ;
  let tasks =
    List.map
      (fun vm -> Client.Client.Async.VM.hard_reboot ~rpc ~session_id ~vm)
      clones
  in
  Printf.printf "Waiting for %d VM hard reboots to finish\n%!" n ;
  Tasks.wait_for_all ~rpc ~session_id ~tasks ;

  Printf.printf "Hard rebooting %d VMs in parallel\n%!" n ;
  let tasks =
    List.map
      (fun vm -> Client.Client.Async.VM.hard_shutdown ~rpc ~session_id ~vm)
      clones
  in
  Printf.printf "Waiting for %d VM shutdowns to finish\n%!" n ;
  Tasks.wait_for_all ~rpc ~session_id ~tasks ;
  Printf.printf "Deleting %d clones\n%!" n ;
  List.iter (fun self -> Client.Client.VM.destroy ~rpc ~session_id ~self) clones

let test rpc session_id vm_template iso_info () =
  let expr =
    Printf.sprintf {|field "SR" = "%s"|} (Ref.string_of iso_info.Qt.sr)
  in
  let prefix = "memtest" in
  let isos =
    Client.Client.VDI.get_all_records_where ~rpc ~session_id ~expr
    |> List.filter (fun (_, iso) ->
           String.starts_with ~prefix iso.API.vDI_name_label
       )
    |> List.sort (fun (_, a) (_, b) ->
           -String.compare a.API.vDI_name_label b.API.vDI_name_label
       )
  in
  match isos with
  | [] ->
      Printf.eprintf "No ISO found with prefix %S\n%!" prefix
  | (_, iso) :: _ ->
      Printf.eprintf "Choosing ISO %S\n%!" iso.API.vDI_name_label ;
      let host =
        Client.Client.Host.get_by_uuid ~rpc ~session_id ~uuid:Qt.localhost_uuid
      in
      let host_free =
        Client.Client.Host.compute_free_memory ~rpc ~session_id ~host
      in
      Qt.VM.with_new rpc session_id ~template:vm_template ~iso (fun vm ->
          List.iter (one rpc session_id host host_free vm) all_possible_tests
      )

let tests () =
  let open Qt_filter in
  [
    [("VM memory tests", `Slow, test)]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> sr SR.(all |> is_iso)
  ]
  |> List.concat

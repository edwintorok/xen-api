module StringSet = Set.Make (String)

let read_rrd rrd ~inputs ~outputs =
  (* see rrddump.ml *)
  let open Rrd in
  let all = StringSet.union inputs outputs in
  let filter_name (_, ds_name) = StringSet.mem ds_name all in
  let partition_by_name (_, ds_name) = StringSet.mem ds_name inputs in
  let input_dss, output_dss =
    rrd.rrd_dss
    |> Array.to_seq
    |> Seq.mapi (fun i ds -> (i, ds.ds_name))
    |> Seq.filter filter_name
    |> Seq.partition partition_by_name
  in
  Format.printf "RRAs: %d" (Array.length rrd.rrd_rras);
 
  let data =
    rrd.rrd_rras
    |> Array.to_seq
    |> Seq.flat_map @@ fun rra ->
       let start =
         rrd.last_updated
         -. Int64.to_float
              (Int64.mul
                 (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt))
                 rrd.timestep
              )
       in
       let rra_timestep =
         Int64.mul rrd.timestep (Int64.of_int rra.rra_pdp_cnt)
       in
       (* Get the last and first times of the CDPs to be returned *)
       let last_cdp_time, _age = get_times rrd.last_updated rra_timestep in
       let time i =
         Int64.sub last_cdp_time (Int64.mul (Int64.of_int i) rra_timestep)
       in
       let start = Int64.of_float start in
       Seq.ints 0
       |> Seq.take_while (fun i -> time i >= start && i < rra.rra_row_cnt )
       |> Seq.filter_map @@ fun i ->
          let inp, out =
            ( Seq.append
                (Seq.return @@ Int64.to_float @@ Int64.sub (time i) start)
                (input_dss
                |> Seq.map (fun (j, _) -> Fring.peek rra.rra_data.(j) i)
                )
              |> Array.of_seq
            , output_dss
              |> Seq.map (fun (j, _) -> Fring.peek rra.rra_data.(j) i)
              |> Array.of_seq
            )
          in
          if
            Array.for_all Float.is_finite inp
            && Array.for_all Float.is_finite out
          then
            Some (inp, out)
          else
            None
  in
  let inputs, outputs = Seq.unzip data in
  (Array.of_seq inputs, Array.of_seq outputs)

let normalise data =
  let open Owl in
  let m = Mat.mean ~axis:0 data in
  let r = Mat.(sub (max ~axis:0 data) (min ~axis:0 data)) in
  Mat.((data - m) / r)

let () =
  let input = Xmlm.make_input (`Channel stdin) in
  let rrd = Rrd.from_xml input in
  let inputs = StringSet.of_list ["pool_task_count"] in
  let outputs = StringSet.of_list ["xapi_open_fds"] in
  let inputs, outputs = read_rrd rrd ~inputs ~outputs in
  let open Owl in
  let inputs, outputs = (Mat.of_arrays inputs, Mat.of_arrays outputs) in
  Mat.print inputs ;
  Mat.print outputs ;
(*   let inputs = normalise inputs in *)

  let theta = Regression.D.ols ~i:true inputs outputs in

  Format.printf "Regression result: %a@." (Fmt.array Owl_pretty.pp_dsnda) theta ;
  ()

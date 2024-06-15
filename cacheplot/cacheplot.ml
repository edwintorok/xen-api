open Simple_workloads
open Bechamel

module Rdtsc = struct
  type witness = unit
  let make = ignore
  let load = ignore
  let unload = ignore
  let label () = "RDTSC"
  let unit () = "cycle"

  let serialize =  Ocaml_intrinsics.Fences.load_fence

  let get () =
    serialize ();
    Ocaml_intrinsics.Perfmon.rdtsc () |> Int64.to_float
end

let rdtsc =
  let measure = Measure.register (module Rdtsc) in
  Measure.instance (module Rdtsc) measure

let rec build_strides ~n ~stride_size_bytes results =
  if stride_size_bytes > n / 2 then results
  else build_strides ~n ~stride_size_bytes:(stride_size_bytes * 2) (stride_size_bytes :: results)

let test_strided_read ~linesize ~n =
  Test.make_indexed_with_resource ~name:(string_of_int n)
    ~args:(build_strides ~n ~stride_size_bytes:(linesize/2) [])
    Test.uniq
    ~allocate:(fun _ -> Operations.StridedRead.allocate n)
    ~free:(fun _ -> output_char stderr '.'; flush stderr)
    (fun stride_size_bytes -> Staged.stage (Operations.StridedRead.read ~stride_size_bytes))

let rec make_tests ~linesize ~lo ~hi tests =
  if lo > hi then tests
  else begin
    let test = test_strided_read ~linesize ~n:lo in
    make_tests ~linesize ~lo:(lo*2) ~hi (test :: tests)
  end
  
let tests =
  let caches = Cachesize.caches () in
  let hi = (List.hd caches).Cachesize.size * 2
  and lo = (caches |> List.rev |> List.hd).Cachesize.size / 2 in
  let linesize = List.fold_left Int.max 0 (List.map (fun c -> c.Cachesize.linesize) caches) in
  make_tests ~lo ~hi ~linesize []
  |> Bechamel.Test.make_grouped ~name:"strided_read"

let predictors = [|Measure.run|]
let instances = [ rdtsc ]

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:false ~bootstrap:0 ~predictors
  in
  let results =
    List.map (fun instance ->
      Analyze.all ols instance raw_results
    ) instances
  in
  Analyze.merge ols instances results

let print_suffix n =
  if n < 1 lsl 10 then
    Printf.printf "%6d" n
  else if n < 1 lsl 20 then
    Printf.printf "%3dKiB" (n lsr 10)
  else Printf.printf "%3dMiB" (n lsr 20)

module IntSet = Set.Make(Int)

let print_results results =
  let tbl = Hashtbl.create 47 in
  let () =
    results |> Hashtbl.iter @@ fun _ result ->
    result |> Hashtbl.iter @@ fun k' ols ->
    ols |> Analyze.OLS.estimates |> Option.iter @@ fun estimates ->
    estimates |> List.iter @@ fun est ->
    Scanf.sscanf k' "strided_read/%d:%d" @@ fun n stride ->
    let ops = n / stride in
    Hashtbl.replace tbl (stride, n) (est /. float ops)
  in
  let strides, columns = Hashtbl.fold (fun (stride, n) _ (strides, columns) ->
    IntSet.add stride strides, IntSet.add n columns
  ) tbl (IntSet.empty, IntSet.empty) in

  Printf.printf "%8s" "STRIDE";
  columns |> IntSet.iter (fun n ->
    print_char '\t';
    print_suffix n
  );
  print_char '\n';

  strides |> IntSet.iter @@ fun stride ->
  Printf.printf "%8d" stride;
  let () =
    columns |> IntSet.iter @@ fun n ->
    print_char '\t';
    Hashtbl.find_opt tbl (stride, n)
    |> Option.iter @@ fun result ->
    Printf.printf "%6.1f" result
  in
  print_char '\n'


let () =
  let cfg = Bechamel.Benchmark.cfg ~quota:(Time.second 0.1) ~start:2 ~stabilize:false () in
  tests |> Bechamel.Benchmark.all cfg instances |> analyze
  |> print_results

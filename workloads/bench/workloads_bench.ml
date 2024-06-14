open Simple_workloads

let test1 =
  Bench.make ~name:"yield overhead" ~args:[1]
    ~workers:512
    ~worker:Operations.Yield.worker
    ~allocate:ignore
    ~free:ignore
    @@ fun _ _ ->
    Operations.Yield.perform ()

let caches = Cachesize.caches ()
let linesize = List.fold_left Int.max 0 (List.map (fun c -> c.Cachesize.linesize) caches)

let test_stride stride_size_bytes =
  let args = List.map (fun c -> c.Cachesize.size) caches in
  (* we want to measure cache misses for all levels, so we need a final one  that is larger than all *)
  let args = ((List.hd caches).Cachesize.sum * 2) :: args in
  Bechamel.Test.make_indexed_with_resource ~name:"stride_read" ~args
    Bechamel.Test.uniq
  ~allocate:Operations.StridedRead.allocate
  ~free:ignore
  (fun _ -> Bechamel.Staged.stage (Operations.StridedRead.read ~stride_size_bytes))

let test2 = test_stride linesize

let test_cycle stride_size_bytes =
  let args = List.concat_map (fun c -> let s = c.Cachesize.size in [s / 2; s; s*2]) caches in
  (* we want to measure cache misses for all levels, so we need a final one  that is larger than all *)
  Bechamel.Test.make_indexed_with_resource ~name:"random_read" ~args
    Bechamel.Test.uniq
  ~allocate:(Operations.CycleRead.allocate ~stride_size_bytes)
  ~free:ignore
  (fun _ -> Bechamel.Staged.stage (Operations.CycleRead.read))

let test3 = test_cycle linesize

let test_copy =
  let args = List.map (fun c -> c.Cachesize.size) caches in
  (* we want to measure cache misses for all levels, so we need a final one  that is larger than all *)
  let args = ((List.hd caches).Cachesize.sum * 2) :: args in
  Bechamel.Test.make_indexed_with_resource ~name:"copy" ~args
    Bechamel.Test.uniq
  ~allocate:(Operations.Copy.allocate)
  ~free:ignore
  (fun _ -> Bechamel.Staged.stage (Operations.Copy.copy))


let () =
  Bechamel_simple_cli.cli ~predictors:[|Bench.yields_name;|] ~instances:[Bechamel.Toolkit.Instance.monotonic_clock; Bench.yields] test1;
  Bechamel_simple_cli.cli  test2;
  Bechamel_simple_cli.cli  test3;
  Bechamel_simple_cli.cli  test_copy

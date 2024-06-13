let getconf conf =
  let ch = Unix.open_process_in (Filename.quote_command "getconf" [conf]) in
  let finally () = let (_:Unix.process_status) = Unix.close_process_in ch in () in
  Fun.protect ~finally @@ fun () ->
  input_line ch

let word_size_bytes = Sys.word_size / 8
let stride_size_bytes = getconf "LEVEL2_CACHE_LINESIZE" |> int_of_string
let stride_size_words = stride_size_bytes / word_size_bytes
let make_cycle n =
  let a = Array.init (n / word_size_bytes) Fun.id in
  (* todo: individual alloc, cachielinesize apart? *)
  for i = (Array.length a -1) / stride_size_words downto 1 do
  (* can be any f, what value would result in most cache misses, furthest apart? *)
    let j = Random.int i in (* [0 <= j < i] *)
    let swap = a.(stride_size_words*i) in
    a.(stride_size_words*i) <- a.(stride_size_words*j);
    a.(stride_size_words*j) <- swap
  done;
  Sys.opaque_identity a  

let rec read_cycle ~data ~pos =
  if pos <> 0 then
    let pos = Array.unsafe_get data pos in
    read_cycle ~data ~pos
  else 
    Sys.opaque_identity pos

let read_cycle data = read_cycle ~data ~pos:data.(0)


let measure_read data =
  let start = Mtime_clock.counter () in
  let r = read_cycle data in
  let elapsed = Mtime_clock.count start in
  let _ = Sys.opaque_identity r in
  elapsed

let measure_read ?(pre=ignore) data =
  let t = Array.init 10 (fun _ -> pre (); measure_read data) in
  let sum = Array.fold_left (Mtime.Span.add) Mtime.Span.zero t in
  let n = Array.length t in
  Format.printf "%a@," (Fmt.Dump.array Mtime.Span.pp) t;
  Mtime.Span.to_float_ns sum *. 1e-9 /. float n

let test_size sum n =
  let data = make_cycle n
  and other = make_cycle sum in
  (* fill cache with other data *)
  let fill_cache () =  let (_:int) = read_cycle other in () in
  let uncached_read = measure_read ~pre:fill_cache data in
  let cached_read = measure_read data in
  let overhead = uncached_read -. cached_read in
  Format.printf "%d bytes: uncached: %.7fs, cached: %.7fs, overhead: %.7fs@." n uncached_read cached_read overhead

let rec test_caches sum n =
  let size = getconf (Printf.sprintf "LEVEL%d_CACHE_SIZE" n) |> int_of_string in
  if size > 0 then
   begin
     let sum = sum + size in
     test_size sum size;
     test_caches sum (n+1)
   end
  
let () =
  Random.init 42;
  let l1_dcache = getconf "LEVEL1_DCACHE_SIZE" |> int_of_string in
  test_size l1_dcache l1_dcache;
  test_caches l1_dcache 2



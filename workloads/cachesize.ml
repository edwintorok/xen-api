let getconf conf =
  let ch = Unix.open_process_in (Filename.quote_command "getconf" [conf]) in
  let finally () = let (_:Unix.process_status) = Unix.close_process_in ch in () in
  Fun.protect ~finally @@ fun () ->
  input_line ch |> int_of_string

(* These are glibc specific, but not arch specific. Will likely fail if [getconf] was compiled with musl. *)

type t = { size: int; sum : int; linesize: int; assoc: int }

let get_cache_conf sum name =
  let size = getconf (name ^ "_SIZE") in
  if size > 0 then begin
    let linesize = getconf (name ^ "_LINESIZE")
    and assoc = getconf (name ^ "_ASSOC") in
    Some {size; sum = sum + size; linesize; assoc}
  end
  else None

let rec levels sum caches n =
  match get_cache_conf sum (Printf.sprintf "LEVEL%d_CACHE" n) with
  | None -> caches
  | Some cache ->
    levels (sum + cache.sum) (cache :: caches) (n+1)

let caches () =
  match get_cache_conf 0 "LEVEL1_DCACHE" with
  | None -> []
  | Some l1 ->
    levels l1.sum [l1] 2


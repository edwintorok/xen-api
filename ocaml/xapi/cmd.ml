module D = Debug.Make (struct let name = "xapi-cmd" end)

(* inspired by Bos.Cmd and Fe_argv, but implemented with Seq *)

type t = string Seq.t

let v = Seq.return

let empty = Seq.empty

let ( %% ) = Seq.append

let ( % ) args arg = args %% v arg

let on cond args = if cond then args else empty

let pp = Fmt.seq Fmt.string

let wrap f x = x |> f |> v

let run runner ~cmd args = args |> List.of_seq |> runner cmd

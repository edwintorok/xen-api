module T = struct
  type 'a t = A : int -> int t

  let eq = Int.equal

  let pp = Fmt.int

  let run (type a) : a t -> a = function A n -> n

  type poly_a = int

  let gena = Crowbar.int

  let gen agen = Crowbar.(map [agen] @@ fun n -> A n)
end

module F = Freer.Make (T)

module Test = struct include T module Gen = T include F end

let () = Test_categories.test_applicative_ops (module Test)

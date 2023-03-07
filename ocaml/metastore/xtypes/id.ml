type 'a t = 'a Ref.t

let compare = Ref.compare

let to_string = Ref.string_of

let dump ppf = Fmt.using Ref.really_pretty_and_small Fmt.string ppf

module Key = struct
  type t = K : _ Ref.t -> t (* hide type *)

  let compare (K a) (K b) = Ref.gcompare a b
end

let key r = Key.K r

module Map = Map.Make (Key)
module Set = Set.Make (Key)

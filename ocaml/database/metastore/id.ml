type t = Uuidm.t

let compare = Uuidm.compare

module Map = struct
  include Map.Make (Uuidm)

  let dump pp_val =
    Fmt.Dump.iter_bindings iter (Fmt.any "Id.Map") Uuidm.pp pp_val
end

module Set = struct
  include Set.Make (Uuidm)

  let dump = Fmt.Dump.iter iter (Fmt.any "Id.Set") Uuidm.pp
end

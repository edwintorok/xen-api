type 'a t = 'a Ref.t

let compare = Ref.compare

let to_string = Ref.string_of

let dump ppf = Fmt.using Ref.really_pretty_and_small Fmt.string ppf

module Key = struct
  type t
end

let key _ = failwith "TODO"

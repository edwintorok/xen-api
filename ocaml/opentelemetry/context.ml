type t = {map: Hmap.t; active: Span.t option}

type 'a key = 'a Hmap.key * string

let empty = {map = Hmap.empty; active = None}

let create_key name : 'a key = (Hmap.Key.create (), name)

let get_value t (key, _) = Hmap.find key t.map

let set_value t (key, _) v = {t with map= Hmap.add key v t.map}

let get_active t = t.active

let set_active t active = {t with active}

(*
 * Copyright (C) 2023 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* TODO: select based on compiler version, for now just 5.1+ version *)

type span_begin =
{ parent: Id.t
; span_id: Id.t
  ... attributes...

  has to be a statically allocated buffer with mutable fields,
  using DLS to get a unique buffer per domain (but is that enough, what if thread switches...)
}

type span_end =
{ span_id: Id.t
}



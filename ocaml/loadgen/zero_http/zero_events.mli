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

(** Zero-copy, low-overhead event recording.

 On OCaml 5.1+ this uses the built-in runtime-events.
 On OCaml <5.1 it uses its own ring-buffer and bechamel.monotonic_clock
   (mtime would allocate an int64 every time and not give us the absolute value, and gettimeofday would use a float)
*)



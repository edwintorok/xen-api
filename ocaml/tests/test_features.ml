(*
 * Copyright (C) Citrix Systems Inc.
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

open Test_highlevel
open Features

module OfAssocList = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = Features.feature list

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t features =
      String.concat "," (List.map name_of_feature features)
  end

  let transform = of_assoc_list

  (* Xen_motion and AD are enabled unless explicitly disabled. All other features
     	   are disabled unless explitly enabled. *)
  let tests =
    `QuickAndAutoDocumented
      [
        ([], [Pooling; Xen_motion; AD; Updates])
      ; ( [
            ("restrict_pooling", "true")
          ; ("restrict_xen_motion", "true")
          ; ("restrict_ad", "true")
          ; ("restrict_updates", "true")
          ]
        , []
        )
      ; ([("restrict_xen_motion", "true")], [Pooling; AD; Updates])
      ; ([("restrict_xen_motion", "false")], [Pooling; Xen_motion; AD; Updates])
      ; ( [("restrict_xen_motion", "false"); ("restrict_dmc", "false")]
        , [Pooling; DMC; Xen_motion; AD; Updates]
        )
      ; ( [
            ("restrict_xen_motion", "false")
          ; ("restrict_ad", "true")
          ; ("restrict_dmc", "false")
          ]
        , [Pooling; DMC; Xen_motion; Updates]
        )
      ; ( [("enable_xha", "true"); ("restrict_xen_motion", "true")]
        , [Pooling; HA; AD; Updates]
        )
      ; ([("restrict_updates", "true")], [Pooling; Xen_motion; AD])
      ]
end)

let tests = [("pool_license_of_assoc_list", OfAssocList.tests)]

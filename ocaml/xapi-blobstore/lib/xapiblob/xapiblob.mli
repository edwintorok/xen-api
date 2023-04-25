open Xapi_blobstore_core

type config = {vm: Uuidm.t; target: Uri.t; uname: string; pwd: string}
include Types.S with type 'a io = 'a Lwt.t
    and type config := config

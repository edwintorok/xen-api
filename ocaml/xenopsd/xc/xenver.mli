(** Common interface to all supported Xen versions.

  A function declared here must be implemented by all supported Xen versions.
  If it is not available on a particular version then the return type should be
  [option], so that the caller can handle the absence of the functionality
  correctly.
*)

val current : string

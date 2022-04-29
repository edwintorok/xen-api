(** the type of command-line arguments *)
type t

val v : string -> t
(** [v arg] is the single command-line argument [arg] *)

val empty : t
(** [empty] is an empty list of command-line arguments *)

val ( % ) : t -> string -> t
(** [args % arg] appends [arg] to the command-line arguments [arg] *)

val ( %% ) : t -> t -> t
(** [args1 %% args2] concatenates the command-line arguments [args1] and [args2] *)

val on : bool -> t -> t
(** [on cond t] is [t] if [cond] is true and [empty] otherwise *)

val wrap : ('a -> string) -> 'a -> t
(** [wrap conv] is a convenience wrapper to build command-line arguments out of arbitrary ['a]
values, given a string conversion function [conv] *)

val pp : t Fmt.t
(** [pp t] pretty prints the arguments [t] *)

val run : (string -> string list -> 'a) -> cmd:string -> t -> 'a
(** [run runner ~cmd args] builds a command-line and runs it using [runner] *)

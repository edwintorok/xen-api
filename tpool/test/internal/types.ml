module type FIFO = sig
  type 'a t

  val create : unit -> 'a t

  val is_empty : 'a t -> bool

  val push : 'a t -> 'a -> unit

  val pop_opt : 'a t -> 'a option
end


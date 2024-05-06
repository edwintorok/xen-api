module type FifoS = sig
  type 'a t

  val create : unit -> 'a t

  val is_empty : 'a t -> bool

  val push : 'a t -> 'a -> unit

  val pop_opt : 'a t -> 'a option
end

module MakeLinSpec (F : FifoS) : Lin.Spec = struct
  open Lin

  type t = int F.t

  let init = F.create

  let cleanup _ = ()

  let api =
    let int = nat_small in
    [ val_ "is_empty" F.is_empty (t @-> returning bool)
    ; val_ "push" F.push (t @-> int @-> returning unit)
    ; val_ "pop_opt" F.pop_opt (t @-> returning @@ option int) ]
end

(** A functional FIFO queue.
  STM needs an immutable model implementation, but one that is still reasonably efficient
  (i.e. we don't reverse the entire list every time we pop)
*)
module FifoModel = struct
  type !+'a non_empty =
    {push: 'a list (* in reverse order *); pop: 'a * 'a list (* in FIFO order *)}

  type !+'a t = 'a non_empty option

  let empty = None

  let is_empty = Option.is_none

  let push t x =
    match t with
    | None ->
        Some {push= []; pop= (x, [])}
    | Some t ->
        Some {t with push= x :: t.push}

  let make ~pop ~push =
    match pop with
    | hd :: tl ->
        Some {pop= (hd, tl); push}
    | [] -> (
      match List.rev push with
      | [] ->
          None
      | hd :: tl ->
          Some {pop= (hd, tl); push= []} )

  let pop_opt = function
    | None ->
        None
    | Some {pop= _, tail; push} ->
        make ~pop:tail ~push

  let peek = function None -> None | Some {pop= head, _; _} -> Some head
end

module MakeSTMSpec (F : FifoS) : STM.Spec = struct
  open STM

  type sut = int F.t

  let init_sut = F.create

  let cleanup _ = ()

  type cmd = Is_empty | Push of int | Pop_opt
  [@@deriving show {with_path= false}]

  let run cmd sut =
    match cmd with
    | Is_empty ->
        Res (bool, F.is_empty sut)
    | Push x ->
        Res (unit, F.push sut x)
    | Pop_opt ->
        Res (option int, F.pop_opt sut)

  type state = int FifoModel.t

  let init_state = FifoModel.empty

  let next_state cmd state =
    match cmd with
    | Is_empty ->
        state
    | Push x ->
        FifoModel.push state x
    | Pop_opt ->
        FifoModel.pop_opt state

  let precond _cmd _state = true

  let postcond cmd (state : state) res =
    match (cmd, res) with
    | Is_empty, Res ((Bool, _), b) ->
        b = (state = init_state)
    | Push _, Res ((Unit, _), ()) ->
        true
    | Pop_opt, Res ((Option Int, _), popped) ->
        FifoModel.peek state = popped
    | _ ->
        false

  let arb_cmd _state =
    QCheck.(
      make ~print:show_cmd
        (Gen.oneof
           [ Gen.return Is_empty
           ; Gen.small_int |> Gen.map (fun x -> Push x)
           ; Gen.return Pop_opt ] ) )
end

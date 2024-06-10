
let make_cycle n =
  let a = Array.init n Fun.id in
  (* todo: individual alloc, cachielinesize apart? *)
  for i = Array.length a - 1 downto 1 do
  (* can be any f, what value would result in most cache misses, furthest apart? *)
    let j = Random.int i in (* [0 <= j < i] *)
    let swap = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- swap
  done;
  a  

let rec print_cycle a i =
  if i <> 0 then begin
    Printf.printf "%d\n" i;
    let next = a.(i) in
    Printf.printf "+%d\n" (next - i);
    print_cycle a next
  end

let () =
  Random.self_init ();
  let a = make_cycle 32 in
  Printf.printf "0\n";
  print_cycle a a.(0)

module Yield = struct
  let count = Atomic.make 0

  let perform () =
    Thread.yield ();
    Atomic.incr count

  let worker = perform
end

let word_size = Sys.word_size / 8

module StridedRead = struct

  let allocate n = Array.make (n / word_size) 1

  let rec read_from ~stride_size_words data len pos =
    if pos < len then
      (* prevent the optimizer from dropping the read:
        - either because the result is unused
        - or because the value would be statically known at compile time
       *)
      let _ = Sys.opaque_identity (Array.unsafe_get (Sys.opaque_identity data) pos) in
      read_from ~stride_size_words data len (pos + stride_size_words)    

  let[@inline always] read ~stride_size_words data =
    read_from ~stride_size_words data (Array.length data) 0 
end

module CycleRead = struct
  let[@inline always] swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp

  let allocate ~stride_size_bytes n = 
    let stride_size_words = stride_size_bytes / word_size in
    assert (stride_size_words >= 1);
    let n = n / word_size in
    let a = Array.init n Fun.id in
    (* Sattolo's algorithm for maximal length cycles *)
    for i = (n / stride_size_words - 1) downto 1 do
      let j = Random.int i in (* [0 <= j < i] *)
      swap a (stride_size_words * i) (stride_size_words * j)
    done;
    a

  let rec read data pos =
    (* random reads, but at least stride_size_bytes apart *)
    if pos <> 0 then
      let pos = Sys.opaque_identity (Array.unsafe_get data pos) in
      read data pos

  let read data = read data data.(0)
end

module Copy = struct
  let allocate n = String.make n 'x', Bytes.create n

  let copy (src, dst) =
    Bytes.blit_string (Sys.opaque_identity src) 0 dst 0 (String.length src)
end

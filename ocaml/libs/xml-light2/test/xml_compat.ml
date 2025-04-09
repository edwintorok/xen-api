open QCheck2

module type XML = sig
  type xml

  val make_element : string -> (string * string) list -> xml list -> xml

  val make_pcdata : string -> xml

  val parse_string : string -> xml

  val to_string : xml -> string

  val equal : xml -> xml -> bool

  val print : xml Print.t

  val pp : xml Fmt.t

  module Convert : sig
    type t

    val of_xml : xml -> t
  end

  val name : string
end

module Old = struct
  include Old_xml.Xml

  let equal (a : xml) (b : xml) = a = b

  let make_element tag attrs children = Element (tag, attrs, children)

  let make_pcdata s = PCData s

  let print = Print.(contramap to_string string)

  let pp = Fmt.(using to_string_fmt text)

  module Convert = struct
    type t = Xml.xml

    let attr_of (name, v) = (("", name), v)

    let rec of_xml = function
      | PCData d ->
          Xml.pcdata d
      | Element (tag, attrs, children) ->
          Xml.element tag (List.map attr_of attrs) (List.map of_xml children)
  end

  let name = "old"
end

module New = struct
  include Xml

  let equal (a : xml) (b : xml) = a = b

  let make_pcdata = pcdata

  let make_attr (name, value) = (("", name), value)

  let make_element tag attrs children =
    element tag (List.map make_attr attrs) children

  let print = Print.(contramap to_string string)

  let pp = Fmt.(using to_string_fmt text)

  module Convert = struct
    type t = Old_xml.Xml.xml

    let attr_of ((_, name), v) = (name, v)

    let rec of_xml = function
      | `Data d ->
          Old_xml.Xml.PCData d
      | `El (((_, tag), attrs), children) ->
          Old_xml.Xml.Element
            (tag, List.map attr_of attrs, List.map of_xml children)
  end

  let name = "new"
end

(**
  "XML processor must accept any character in the range specified for Char"
    https://www.w3.org/TR/xml/#charsets

  shrinks towards [a]
  
  We only generate valid characters.
  Invalid characters are treated differently between old and new XML serializers.
  Old one drops the characters, whereas the new one replaces it with [UChar.rep],
  the unicode replacement character.
*)
let xml_char =
  Gen.(
    oneof
      [
        int_range ~origin:(Char.code 'a') 0x20 0xD7FF
      ; oneofa [|0x9; 0xA; 0xD|]
      ; 0xE000 -- 0xFFFD
      ; 0x10000 -- 0x10FFFF
      ]
    |> map Uchar.of_int
  )

let string_of_uchars uchars =
  let b = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar b) uchars ;
  Buffer.contents b

(** https://www.w3.org/TR/xml/#sec-common-syn *)
let xml_name_start_char =
  Gen.(
    oneof
      [
        oneof
          [
            char_range 'A' 'Z'
          ; oneofa [|(*':' only for namespaces, which we don't support; *) '_'|]
          ; char_range 'a' 'z'
          ]
        |> map Uchar.of_char
      ; oneof
          [
            0xC0 -- 0xD6
          ; 0xD8 -- 0xF6
          ; 0xF8 -- 0x2FF
          ; 0x370 -- 0x37D
          ; 0x37F -- 0x1FFF
          ; 0x200C -- 0x200D
          ; 0x2070 -- 0x218F
          ; 0x2C00 -- 0x2FEF
          ; 0x3001 -- 0xD7FF
          ; 0xF900 -- 0xFDCF
          ; 0xFDF0 -- 0xFFFD
          ; 0x10000 -- 0xEFFFF
          ]
        |> map Uchar.of_int
      ]
  )

let xml_name_char =
  Gen.(
    oneof
      [
        xml_name_start_char
      ; oneof [oneofa [|'-'; '.'|]; char_range '0' '9'] |> map Uchar.of_char
      ; oneof [return 0xB7; 0x0300 -- 0x036F; 0x203F -- 0x2040]
        |> map Uchar.of_int
      ]
  )

let xml_name =
  let open Gen in
  let+ start = xml_name_start_char and+ rest = small_list xml_name_char in
  string_of_uchars (start :: rest)

let xml_chars = Gen.(small_list xml_char |> map string_of_uchars)

let xml_pcdata = xml_chars

let xml_att_value = xml_chars

let attrs = Gen.(small_list @@ pair xml_name xml_att_value)

let xml (type xml) (module X : XML with type xml = xml) =
  let open Gen in
  let element children = function
    | 0 ->
        map X.make_pcdata xml_pcdata
    | n ->
        frequency
          [
            (1, map X.make_pcdata xml_pcdata)
          ; (2, map3 X.make_element xml_name attrs (children @@ (n - 1)))
          ]
  in
  let children =
    fix @@ fun self -> function -1 | 0 -> return [] | n -> (
        sized_size (0 -- (n - 1)) @@ function
        | 0 ->
            return []
        | length ->
            list_repeat length @@ element self (n / length)
      )
  in
  (* root must be an element, NOT pcdata *)
  sized_size small_nat @@ fun n ->
  map3 X.make_element xml_name attrs (children @@ (n - 1))

module Make
    (A : XML)
    (B : XML with type xml = A.Convert.t and type Convert.t = A.xml) =
struct
  let a = xml (module A)

  let b = xml (module B)

  let test_reparse (type a) gen (module A : XML with type xml = a) =
    let name = Printf.sprintf "%s.reparse" A.name in
    Test.make ~name gen ~print:A.print @@ fun orig ->
    let str = A.to_string orig in
    prerr_endline str ;
    let a = A.parse_string str in
    (* whitespace can b different here *)
    let str' = A.to_string a in
    let a' = A.parse_string str' in
    if not (A.equal a a') then
      Test.fail_reportf
        "@[<v>XML reparse mismatch.@,XML:@ %a@,a:@ %a@,str':@ %a@,a':@ %a@]"
        Fmt.text str A.pp a Fmt.text str' A.pp a' ;
    true

  let test_parse_same (type a b) gen
      (module A : XML with type xml = a and type Convert.t = b)
      (module B : XML with type xml = b and type Convert.t = a) =
    let name = Printf.sprintf "%s.parse_same" A.name in
    Test.make ~name gen ~print:A.print @@ fun orig ->
    (* we don't expect to match original exactly, because whitespace might get stripped,
       so convert to a string, and parse with both A and B, and compare that
    *)
    let a_str = A.to_string orig in
    let a = A.parse_string a_str and b = B.parse_string a_str in
    let a' = B.Convert.of_xml b in
    if not (A.equal a a') then
      Test.fail_reportf
        "@[<v>XML parse mismatch.@,XML:@ %a@,a:@ %a@,<>@,b:@ %a@,a':@ %a@]"
        Fmt.text a_str A.pp a B.pp b A.pp a' ;
    true

  let test_string_equiv (type a b) gen
      (module A : XML with type xml = a and type Convert.t = b)
      (module B : XML with type xml = b and type Convert.t = a) =
    let name = Printf.sprintf "%s.string_equiv" A.name in
    Test.make ~name gen ~print:A.print @@ fun orig ->
    let a_str = A.to_string orig
    and b_str = A.Convert.of_xml orig |> B.to_string in
    let a_parsed = A.parse_string a_str and b_parsed = B.parse_string b_str in
    let b_parsed' = B.Convert.of_xml b_parsed in
    if not (A.equal a_parsed b_parsed') then
      Test.fail_reportf
        "@[<v>XML to_string mismatch.@,\
         XML:@ %a@,\
         a_str:@ %a@,\
         b_str:@ %a@,\
         a_parsed:@ %a@,\
         b_parsed:@ %a@]" A.pp orig Fmt.text a_str Fmt.text b_str A.pp a_parsed
        B.pp b_parsed ;
    true

  let tests =
    [
      test_reparse a (module A)
    ; test_reparse b (module B)
    ; test_parse_same a (module A) (module B)
    ; test_parse_same b (module B) (module A)
    ; test_string_equiv a (module A) (module B)
    ; test_string_equiv b (module B) (module A)
    ]
end

module T = Make (Old) (New)

let () =
  Alcotest.V1.run __FILE__
    [
      ( "qcheck"
      , T.tests
        |> List.map (QCheck_alcotest.to_alcotest ~verbose:true ~long:true)
      )
    ]
(*  QCheck_base_runner.run_tests_main T.tests*)

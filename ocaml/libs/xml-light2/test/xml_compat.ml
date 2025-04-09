open QCheck2

module Old = struct
  let element tag attrs children = Old_xml.Xml.Element (tag, attrs, children)

  let pcdata data = Old_xml.Xml.PCData data

  let tag = Gen.(string_size ~gen:Gen.(char_range 'a' 'z') (1 -- 10))

  (* for now, TODO: generate UTF-8, also test space and \n *)
  let str = Gen.(string_small_of @@ char_range (Char.chr 33) (Char.chr 126))

  let attrs = Gen.(small_list @@ pair tag str)

  let xml =
    Gen.(
      sized
      @@ fix
      @@ fun self n ->
      match n with
      | 0 ->
          map pcdata str
      | n ->
          let children =
            sized_size (0 -- (n - 1)) @@ function
            | 0 ->
                return []
            | length ->
                list_repeat length @@ self (n / length)
          in
          frequency
            [(1, map pcdata str); (2, map3 element tag attrs children)]
    )
end

let test_new_to_old =
  Test.make ~name:__FUNCTION__
    ~print:Print.(contramap Old_xml.Xml.to_string_fmt string)
    Old.xml
  @@ fun old_xml ->
  match old_xml with
  | Old_xml.Xml.PCData _ ->
      assume_fail ()
  | _ ->
      let old_str = Old_xml.Xml.to_string old_xml in
      let new_xml = Xml.parse_string old_str in
      let new_str = Xml.to_string new_xml in
      let equal = String.equal old_str new_str in
      (* won't match exactly <a></a> vs <a/> *)
      if not equal then
        Test.fail_reportf "@[<v>Xml str mismatch:@,%s@,<>@,%s@]" old_str new_str ;
      equal

let tests = [test_new_to_old]

let () =
  Alcotest.V1.run __FILE__
    [
      ( "qcheck"
      , tests |> List.map (QCheck_alcotest.to_alcotest ~verbose:true ~long:true)
      )
    ]

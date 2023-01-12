(** Parse a .ml file, extract all 'external ...' primitives,
    and print prototypes of bytecode C functions based on their number of arguments.

    Uses compiler-libs, which has an unstable API that can change between
    compiler versions, so extract only the minimal information needed here.
    If this breaks with newer compiler versions then
    ocaml-migrate-parsetree could be used.
    Currently require a 4.08 AST minimum (although this could be relaxed with
    migrate-parsetree).

    [ocamlc -dparsetree foo.ml] can be used to see how the parsetree looks
    like.
 *)

(** [arity_of_type typ] returns the number of arguments for the function type [typ].

    Type aliases are not expanded, and we only recurse on right hand side of
    the type arrow.
    @see <https://v2.ocaml.org/manual/intfc.html#ss:c-prim-decl> examples in the manual.
 *)
let rec arity_of_type =
  let open Parsetree in
  function
  | {ptyp_desc= Ptyp_arrow (_, _t1, t2); _} -> 1 + arity_of_type t2 | _ -> 0

(** [value_description _ vd] is invoked by the AST iterator for value
    descriptions, including primitives ('external ...').

    @see <https://v2.ocaml.org/api/compilerlibref/Parsetree.html#2_Valuedescriptions>
*)
let value_description _ vd =
  let open Parsetree in
  let arity = arity_of_type vd.pval_type in
  match vd.pval_prim with
  | [] ->
      () (* not a primitive *)
  | builtin :: _ when builtin = "" || builtin.[0] = '%' ->
      () (* call to builtin primitive, no prototypes to print *)
  | bytecode_c_function :: _ ->
      (* print prototype only for bytecode function.
         To correctly print the prototype for the native function we'd need
         to process the typedtree, which the full static analyzer will do.
         Processing just the AST has fewer dependencies on the compiler
         version.
      *)
      let args =
        if arity <= 5 then
          List.init arity @@ fun _ -> "value"
        else
          ["value *argv"; "int argn"]
      in
      Printf.printf "CAMLprim value %s(%s);" bytecode_c_function
        (String.concat ", " args)

let () =
  let tool_name = Sys.executable_name in
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    let usage_msg = Printf.sprintf "%s [FILE.ml...]" tool_name in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in
  (* [CAML_NAME_SPACE] is recommended by the manual *)
  print_endline "#define CAML_NAME_SPACE" ;
  (* get the definition of [value] *)
  print_endline "#include <caml/mlvalues.h>" ;

  try
    files
    |> List.iter @@ fun path ->
       let open Ast_iterator in
       (* use the AST iterator, because primitives might be declared inside a
          module, not necessarily at top level. *)
       let primitives_iterator = {default_iterator with value_description} in
       path
       (* have to parse the implementation, because the .mli may hide that it
          is a C stub by defining a 'val name ...' instead of 'external name ...'. *)
       |> Pparse.parse_implementation ~tool_name
       |> primitives_iterator.structure primitives_iterator
  with e ->
    (* if there are any syntax errors, or other exceptions escaping from
       compiler-libs this will report them properly *)
    Location.report_exception Format.err_formatter e

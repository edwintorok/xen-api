Test rules from https://v2.ocaml.org/manual/intfc.html#s:c-gc-harmony

Rule 1. CAMLparam
  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <goblint.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam0();
  >   CAMLreturn0;
  > }
  > EOF

  $ lintcstubs --set mainfun[+] foo --set ana.activated '["ocamlcstubs", "escape"]' --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <goblint.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam3(v1, v2, v3);
  >   CAMLreturn0;
  > }
  > EOF
  $ lintcstubs --set mainfun[+] foo --set ana.activated '["ocamlcstubs", "escape"]' --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > void foo (value v1, value v2, value v3)
  > {
  > }
  > EOF
  $ lintcstubs --set mainfun[+] foo --set ana.activated '["ocamlcstubs", "escape"]' --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam3(v1,v2,v3);
  >   CAMLlocal1(result);
  >   result = caml_alloc(3, 0);
  >   CAMLreturn(result);
  > }
  > EOF
  $ lintcstubs --set mainfun[+] foo --set ana.activated '["ocamlcstubs", "escape"]' --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c

  TODO: this generates a lot of warnings

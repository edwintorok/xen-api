BEGIN { SYMADDR=ARGV[1]; ARGV[1]=""; }

/^<.*/ { SYMBOL=$1 }
match($0, /mov.*0x([0-9a-fA-F]*),%rax/, addr) { RAX=addr[1]; }
/call.*<caml_c_call>/ { if (RAX == SYMADDR) { print SYMBOL }  }

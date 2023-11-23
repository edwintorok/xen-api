BEGIN { n = 0; }
# A function definition and its address
# Remember its address and update current symbol
# 0000000000850330 <unix_select>:
match($0, /^0*([0-9a-fA-F]+) <([^>]+)>/, symdef) {
 SYMBOL = symdef[2];
 ADDR = symdef[1];

 SYMADDR[ADDR] = SYMBOL;

 if (ADDR in LOADED) {
   for (idx in LOADED[ADDR]) {
     caller = LOADED[ADDR][idx]
     CALLS[symdef[2]][n++] = caller 
   }
 }
}

# Indirect calls (mostly used for C stubs)
# mov    $0x850330,%rax
# call   872834 <caml_c_call>
match($0, /mov.*0x([0-9a-fA-F]*),/, addr) {
  # this will have gaps, but the indexes will be unique
  LOADED[addr[1]][n++] = SYMBOL 
}

match($0, /call.*<([^>]+)>/, call) {
  CALLS[call[1]][n++] = SYMBOL 
}

END {
  SYM = "unix_select"
  for (idx in CALLS[SYM]) {
    caller = CALLS[SYM][idx];
    print caller
    for (idx2 in CALLS[caller]) {
      caller2 = CALLS[caller][idx2];
      print caller2
      for (idx3 in CALLS[caller2]) {
        caller3 = CALLS[caller2][idx3];
        print caller3
        for (idx4 in CALLS[caller3]) {
          caller4 = CALLS[caller3][idx4];
          print caller4
        }
      }
    }
  }
}

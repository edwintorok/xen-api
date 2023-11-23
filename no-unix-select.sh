#!/bin/sh
set -eu

EXE="$1"
DISASM="${EXE}.disasm"

# Find all calls to 'unix_select', which is done by
#  mov $0x...,%rax
#  call <caml_c_call>

# disassemble, but without sources
# (source lookup doesn't work for all dependencies, and is very slow on a large binary)
# To make debugging easier the disassembly is saved to a file instead of piping
objdump --show-all-symbols --wide -d --no-show-raw-insn "${EXE}" >"${DISASM}"

# Prints a list of functions that call unix_select
# But we won't know whether it is dead code or not
gawk -f unix_select.gawk "${DISASM}" "${DISASM}"

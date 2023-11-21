#!/bin/sh
set -eu

EXE="$1"
ADDR="${EXE}.unix_select.address"
DISASM="${EXE}.disasm"

# Find address of 'unix_select' symbol
addr2line -pae "${EXE}" unix_select \
  | sed -e "s/^0x0*\\([0-9a-fA-F]*\\):.*/\\1/" \
  | tr -d '\n' >"${ADDR}"

# Find all calls to 'unix_select', which is done by
#  mov $0x...,%rax
#  call <caml_c_call>

# disassemble, but without sources
# (source lookup doesn't work for all dependencies, and is very slow on a large binary)
# To make debugging easier the disassembly is saved to a file instead of piping
objdump -d --no-show-raw-insn --no-addresses "${EXE}" >"${DISASM}"

SYMADDR=$(cat "${ADDR}")
# Prints a list of functions that call unix_select
# But we won't know whether it is dead code or not
gawk -f unix_select.gawk "${SYMADDR}" <"${DISASM}"

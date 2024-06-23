#!/bin/sh
set -eu
BT=2

run () {
  NAME=$1
  shift
  time OCAMLRUNPARAM=b taskset -c 2,3,4 dune exec ./cacheplot.exe --profile=release -- --benchtime $BT --bootstrap --auto "$@" >|"auto_${NAME}.dat"
}

run p  --process
run t
run pr --process --random
run tr --random
for i in p t pr tr; do
  cp auto_${i}.dat auto.dat && gnuplot auto.gnuplot >"auto_${i}.svg"
done

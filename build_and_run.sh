#!/bin/bash

modules_with_sigs=("color" "rank" "piece" "space" "util" "board" "IllegalMoveReason" "GameState")
modules_without_sigs=("run_chess")
for m in ${modules_with_sigs[@]}; do
    echo "Building module " $m;
    ocamlopt -c $m.mli $m.ml;
done
for m in ${modules_without_sigs[@]}; do
    echo "Building module " $m;
    ocamlopt -c $m.ml;
done
ocamlopt -o run_chess ${modules_with_sigs[@]/%/.cmx} ${modules_without_sigs[@]/%/.cmx}
./run_chess

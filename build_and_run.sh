#!/bin/bash

ocamlfind ocamlopt -o ./build/run_chess \
    -I ./src/ \
    -linkpkg -package oUnit \
    ./src/octet.mli ./src/octet.ml \
    ./src/color.mli ./src/color.ml \
    ./src/rank.mli ./src/rank.ml \
    ./src/piece.mli ./src/piece.ml \
    ./src/space.mli ./src/space.ml \
    ./src/util.mli ./src/util.ml \
    ./src/board.mli ./src/board.ml \
    ./src/IllegalMoveReason.mli ./src/IllegalMoveReason.ml \
    ./src/GameState.mli ./src/GameState.ml \
    ./src/run_chess.ml

./build/run_chess

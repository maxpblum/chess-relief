#!/bin/bash

ocamlfind ocamlopt -o run_chess \
    -linkpkg -package oUnit \
    octet.mli octet.ml \
    color.mli color.ml \
    rank.mli rank.ml \
    piece.mli piece.ml \
    space.mli space.ml \
    util.mli util.ml \
    board.mli board.ml \
    IllegalMoveReason.mli IllegalMoveReason.ml \
    GameState.mli GameState.ml \
    run_chess.ml

./run_chess

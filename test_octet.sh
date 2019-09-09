ocamlfind ocamlopt \
    -o ./build/TestOctet \
    -I ./src/ \
    -linkpkg \
    -package oUnit \
    ./src/Octet.mli ./src/Octet.ml \
    ./tests/TestOctet.ml \
    && ./build/TestOctet

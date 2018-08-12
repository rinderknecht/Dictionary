#!/bin/sh
set -x
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Dict.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -w -40 -c Dict.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Main.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Main.ml
ocamlfind ocamlopt -o Main.opt Dict.cmx Main.cmx

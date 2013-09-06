.PHONY: all clean

all: pp jspp jspp.js

pp: pp.ml
	ocamlfind ocamlopt -o pp -linkpkg -package jslib pp.ml

jspp: jspp.ml
	ocamlfind ocamlc -o jspp -syntax camlp4o -linkpkg -package jslib,js_of_ocaml,js_of_ocaml.syntax jspp.ml

jspp.js: jspp
	js_of_ocaml jspp

clean:
	rm -f pp jspp jspp.js *.cm[ixo] *.o

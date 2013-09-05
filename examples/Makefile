.PHONY: all clean

all: jspp

jspp: jspp.ml
	ocamlfind ocamlopt -o jspp -linkpkg -package jslib jspp.ml

clean:
	rm -f jspp *.cm[ixo] *.o

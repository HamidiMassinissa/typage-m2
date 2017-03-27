
all: native byte tests

native:
	ocamlbuild -use-ocamlfind typage.native

byte:
	ocamlbuild -use-ocamlfind -tag debug typage.byte

tests:
	ocamlbuild -use-ocamlfind runtests.native

clean:
	ocamlbuild -clean

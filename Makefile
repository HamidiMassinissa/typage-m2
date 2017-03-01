
all:
	ocamlbuild -use-ocamlfind typage.native

byte:
	ocamlbuild -use-ocamlfind -tag debug typage.byte

clean:
	ocamlbuild -clean

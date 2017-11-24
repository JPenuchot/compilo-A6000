
.PHONY: 	all clean native

OCB_FLAGS   = -cflag -w -cflag -40 -use-ocamlfind -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) Main.native
	mv Main.native Compilo

SRC=$(wildcard *.ml *.mll)

.SUFFIXES:
.PHONY: clean

main.native: $(SRC)
	ocamlbuild -verbose 0 -pp camlp4o -r $@

clean:
	ocamlbuild -clean

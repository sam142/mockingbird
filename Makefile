.PHONY: native byte clean
OCAMLBUILD = ocamlbuild -use-ocamlfind

native:
	$(OCAMLBUILD) src/lambda.native

byte:
	$(OCAMLBUILD) src/lambda.byte

clean:
	$(OCAMLBUILD) -clean


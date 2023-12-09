.PHONY: test check

build:
	dune build

code:
	-dune build
	code .

test:
	OCAMLRUNPARAM=b dune exec test/test.exe
	
game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
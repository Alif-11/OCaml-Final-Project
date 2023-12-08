code:
	-dune build
	code .

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
	
game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

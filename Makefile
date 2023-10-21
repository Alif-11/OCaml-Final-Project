code:
	-dune build
	code .
utop: 
	OCAMLRUNPARAM=b dune exec test/main.exec
game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

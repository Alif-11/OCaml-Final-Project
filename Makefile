code:
	-dune build
	code .
	| dune build --watch
utop: 
	OCAMLRUNPARAM=b dune exec test/main.exec
game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

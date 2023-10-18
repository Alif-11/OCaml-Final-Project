build:
	-dune build
	code .
	| dune build --watch

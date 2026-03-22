init:
	@opam switch create . 4.14.1 --yes
	@eval $$(opam env) && opam install graphics -y
	@echo "Initialization complete. Please run 'eval $$(opam env)' to set up the environment variables."

build:
	@dune build

clean:
	@dune clean

prune: clean
	@rm -rf _opam

run:
	@dune exec ./ocamlez.exe

PHONY: init build clean prune run

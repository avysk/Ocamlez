all:
	ocamlbuild ocamlez.native -libs graphics
clean:
	rm -rf _build *.cmi *.cmo *.native

compile:
	@ocamlc.opt          -c lib/matrix.mli
	@ocamlc.opt   -I lib -c lib/matrix.ml
	@ocamlopt.opt -I lib -c lib/matrix.ml

clean:
	@find \
		./lib \
			-name '*.o' \
		-or -name '*.cmi' \
		-or -name '*.cmo' \
		-or -name '*.cmx' \
		| xargs rm -f

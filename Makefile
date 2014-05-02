
main: main.cmo
	ocamlc -o $@ $<

clean:
	git clean -fdX

%.cmo: %.ml
	ocamlc -c -o $@ $<

%.cmi: %.mli
	ocamlc -c -o $@ $<

MLS  = $(wildcard *.ml)
MLIS = $(wildcard *.mli)

.depend: $(MLS) $(MLIS)
	ocamldep $^ > $@

include .depend

#
# vim: ts=2 sw=2 ai noet
#

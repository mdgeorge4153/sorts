
main.native: *.ml *.mli sorts/*.ml
	corebuild -I sorts main.native

clean:
	git clean -fdX

#
# vim: ts=2 sw=2 ai noet
#

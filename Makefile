
main.native: *.ml *.mli
	corebuild -I sorts main.native

clean:
	git clean -fdX

#
# vim: ts=2 sw=2 ai noet
#

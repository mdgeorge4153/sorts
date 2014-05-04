
main.native: *.ml *.mli sorts/*.ml
	corebuild -I sorts main.native

run: main.native
	while true; do echo "hello"; sleep 0.1; done \
	  | ./main.native quicksort randqsort heapsort -n 30
	

clean:
	git clean -fdX

#
# vim: ts=2 sw=2 ai noet
#

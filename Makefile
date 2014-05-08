
default: all
all: main.native complete.sh

main.native: *.ml *.mli sorts/*.ml
	corebuild -I sorts main.native

run: main.native
	while true; do echo "hello"; sleep 0.1; done \
	  | ./main.native quicksort randqsort heapsort -n 30
	
complete.sh: main.native
	COMMAND_OUTPUT_INSTALLATION_BASH=1 ./main.native > $@

clean:
	git clean -fdX

#
# vim: ts=2 sw=2 ai noet
#

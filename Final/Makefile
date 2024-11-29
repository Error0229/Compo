
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s && ./a.out

test: minipython.exe
	cd tests-mini-python && bash test -all ../minipython.exe

type: minipython.exe
	cd tests-mini-python && bash test -2 ../minipython.exe

vtype: minipython.exe
	cd tests-mini-python && bash test -v2 ../minipython.exe

parse: minipython.exe
	cd tests-mini-python && bash test -1 ../minipython.exe

vparse: minipython.exe
	cd tests-mini-python && bash test -v1 ../minipython.exe

vcompile: minipython.exe
	cd tests-mini-python && bash test -v3 ../minipython.exe

compile: minipython.exe
	cd tests-mini-python && bash test -3 ../minipython.exe

minipython.exe:
	dune build minipython.exe

clean:
	dune clean

.PHONY: all clean minipython.exe tests type parse vparse vtype

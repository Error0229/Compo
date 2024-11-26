
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s && ./a.out

tests: minipython.exe
	bash test -2 "dune exec ./minipython.exe"

type: minipython.exe
	bash test -2 "dune exec ./minipython.exe"

minipython.exe:
	dune build minipython.exe

clean:
	dune clean

.PHONY: all clean minipython.exe tests type

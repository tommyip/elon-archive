.PHONY: default build test run clean

default: build

build:
	dune build

test:
	dune runtest -f

run:
	dune exec ./bin/elonc.exe $(args)

clean:
	dune clean

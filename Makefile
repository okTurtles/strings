SHELL := /usr/bin/env bash

build:
	opam exec -- dune build src/cli/strings.exe
	cp _build/default/src/cli/strings.exe .
	strip strings.exe

clean:
	opam exec -- dune clean

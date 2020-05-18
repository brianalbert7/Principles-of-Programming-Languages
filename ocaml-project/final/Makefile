build: main

main: main.byte

%.native: src/*.*
	@ocamlbuild $@

%.byte: src/*.*
	@ocamlbuild -tag debug $@

clean:
	@ocamlbuild -clean

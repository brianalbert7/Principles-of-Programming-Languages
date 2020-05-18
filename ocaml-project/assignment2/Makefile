build: assignment2

assignment2: assignment2.byte

%.native: src/*.*
	@ocamlbuild $@

%.byte: src/*.*
	@ocamlbuild -tag debug $@

clean:
	@ocamlbuild -clean

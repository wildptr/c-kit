.PHONY: all clean FORCE

MENHIR          := menhir

# We use the table back-end, and link against menhirLib.
# We assume that menhirLib has been installed in such a
# way that ocamlfind knows about it.

MENHIRFLAGS     := --table --explain
# -- infer is automatically added by ocamlbuild.

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -package extlib -package menhirLib -package ppx_deriving.std

all: cpp.native parse.native

%.native: FORCE
	$(OCAMLBUILD) $*.native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

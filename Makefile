.PHONY: all clean FORCE

MENHIR          := menhir

# We use the table back-end, and link against menhirLib.
# We assume that menhirLib has been installed in such a
# way that ocamlfind knows about it.

MENHIRFLAGS     := --table --explain
# -- infer is automatically added by ocamlbuild.

EXTLIB := extlib

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -package menhirLib -package $(EXTLIB) -package ppx_deriving.std

MAIN            := cpp

all: $(MAIN).native

%.native: FORCE
	$(OCAMLBUILD) $*.native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

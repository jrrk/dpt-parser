#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adaption to .dpt file parsing by Jonathan Kimmitt                      *)
#*  Copyright 2017 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

MENFLAGS= --explain --trace
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt

Program.top: Program.cmi keywords.cmo Program.cmo Program_lex.cmo Program_main.cmo
	ocamlmktop -o $@ unix.cma keywords.cmo Program.cmo Program_lex.cmo Program_main.cmo

Program_lex.ml: Program_lex.mll
	ocamllex Program_lex.mll

Program.ml Program.mli: Program.mly
	menhir $(MENFLAGS) Program.mly

%.cmo: %.ml
	$(OCAMLC) -g -c $<

%.cmi: %.mli
	$(OCAMLC) -g -c $<

test: ./Program.top
	env LD_LIBRARY_PATH=/usr/local/lib64 /usr/local/bin/gfortran -fdump-parse-tree -c mt19937-64.f95
	echo 'rslt;;' | ./Program.top |& tee test.log

clean:
	rm -f *.o *.cmi *.cmo *.cmx *.dpt *.dpt.err *.mod test.log Program.ml Program.mli Program_lex.ml Program.top Program.conflicts

depend:
	ocamldep *.ml *.mli | sed 's=Program_types.cm.==g' >.depend

include .depend

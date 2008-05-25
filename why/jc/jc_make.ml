(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: jc_make.ml,v 1.22 2008/05/20 15:57:42 filliatr Exp $ i*)

open Format
open Pp

(*
let files = Hashtbl.create 97 

let add ~file ~f =
  let l = try Hashtbl.find files file with Not_found -> [] in
  Hashtbl.replace files file (f :: l)
*)

let simplify fmt f = fprintf fmt "simplify/%s_why.sx" f
let coq_v fmt f = fprintf fmt "coq/%s_why.v" f
let coq_vo fmt f = fprintf fmt "coq/%s_why.vo" f
let pvs fmt f = fprintf fmt "pvs/%s_why.pvs" f
let cvcl fmt f = fprintf fmt "cvcl/%s_why.cvc" f
let harvey fmt f = fprintf fmt "harvey/%s_why.rv" f
let zenon fmt f = fprintf fmt "zenon/%s_why.znn" f
let smtlib fmt f = fprintf fmt "smtlib/%s_why.smt" f
let ergo fmt f = fprintf fmt "why/%s_why.why" f
let why_goals fmt f = fprintf fmt "why/%s_ctx.why" f
let wpr fmt f = fprintf fmt "why/%s.wpr" f
let isabelle fmt f = fprintf fmt "isabelle/%s_why.thy" f 

let print_files = print_list (fun fmt () -> fprintf fmt "\\@\n  ")

let generic f targets =
  print_in_file 
    (fun fmt -> 
       fprintf fmt 
       "# this makefile was automatically generated; do not edit @\n@\n";
       fprintf fmt "TIMEOUT ?= 10@\n@\n";	    
       fprintf fmt "WHY=why --no-arrays %s -explain -locs %s.loc@\n@\n" (Jc_options.why_opt) f;
       fprintf fmt "GWHY=gwhy-bin --no-arrays %s -explain -locs %s.loc@\n@\n" (Jc_options.why_opt) f;
       fprintf fmt "JESSIELIBFILE=%s@\n@\n" 
	 (String.escaped (Filename.concat Jc_options.libdir 
	    (Filename.concat "why" Jc_options.libfile)));

       fprintf fmt ".PHONY: all coq pvs simplify cvcl harvey smtlib zenon@\n@\n";
       fprintf fmt "all: %a@\n@\n" 
	 (print_files simplify) targets;

       fprintf fmt "project: %a@\n@\n" (print_files wpr) targets;
       fprintf fmt "why/%%.wpr: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why --project [...] why/$*.why' && $(WHY) --project -dir why $(JESSIELIBFILE) why/$*.why@\n@\n";

       fprintf fmt "goals: %a@\n@\n" (print_files why_goals) targets;
       fprintf fmt "why/%%_ctx.why: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why --multi-why [...] why/$*.why' && $(WHY) --multi-why -dir why $(JESSIELIBFILE) why/$*.why@\n@\n";

       fprintf fmt "coq: %a@\n@\n" (print_files coq_vo) targets;

(*
       fprintf fmt "coq/%s_spec_why.v: coq/%s_ctx_why.v@\n" f f;
       fprintf fmt "\tcp coq/%s_ctx_why.v coq/%s_spec_why.v@\n@\n" f f;

       fprintf fmt "coq/%s_spec.why: coq/%s_ctx.why@\n" f f;
       fprintf fmt "\tcp coq/%s_ctx.why coq/%s_spec.why@\n@\n" f f;
*)

       fprintf fmt "coq-goals: goals coq/%s_ctx_why.vo@\n" f;
       fprintf fmt "\tfor f in why/%s_po*.why; do make -f %s.makefile coq/`basename $$f .why`_why.v ; done@\n@\n" f f;

       
       fprintf fmt "coq/%s_ctx_why.v: why/%s_ctx.why@\n" f f;
       fprintf fmt "\t@@echo 'why -coq-v8 [...] why/%s_ctx.why' &&$(WHY) -no-prelude -coq-v8 -dir coq -coq-preamble \"Require Export jessie_why.\" -coq-tactic \"intuition\" why/%s_ctx.why@\n@\n" f f;

       fprintf fmt "coq/%%_why.v: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -coq-v8 [...] why/$*.why' &&$(WHY) -no-prelude -coq-v8 -dir coq -coq-preamble \"Require Export %s_ctx_why.\" -coq-tactic \"intuition\" why/%s_ctx.why why/$*.why@\n@\n" f f;
       fprintf fmt "coq/%%.vo: coq/%%.v@\n\tcoqc -I coq $<@\n@\n";
       
       fprintf fmt "pvs: %a@\n@\n" (print_files pvs) targets;

       fprintf fmt "pvs/%%_why.pvs: why/%%.why@\n";
       fprintf fmt "\t$(WHY) -pvs -dir pvs -pvs-preamble \"importing jessie_why\" $(JESSIELIBFILE) why/$*.why@\n@\n";

       fprintf fmt "pvs/caduceus_why.pvs:@\n";
       fprintf fmt "\t$(WHY) -pvs -dir pvs $(JESSIELIBFILE)@\n@\n";
       
       fprintf fmt "isabelle: %a@\n@\n" (print_files isabelle) targets;

       fprintf fmt "isabelle/%%_why.thy: why/%%.why@\n";
       fprintf fmt "\t$(WHY) -isabelle -dir isabelle -isabelle-base-theory jessie_why $(JESSIELIBFILE) why/$*.why@\n";
       fprintf fmt "\tcp -f %s/isabelle/jessie_why.thy isabelle/@\n@\n" 
	 Jc_options.libdir;

       fprintf fmt "simplify: %a@\n" (print_files simplify) targets;
       fprintf fmt "\t@@echo 'Running Simplify on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "simplify/%%_why.sx: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -simplify [...] why/$*.why' && $(WHY) -simplify -no-simplify-prelude -dir simplify $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "ergo: %a@\n" (print_files ergo) targets;
       fprintf fmt "\t@@echo 'Running Ergo on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "why/%%_why.why: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why --why [...] why/$*.why' && $(WHY) --why -dir why $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "cvcl: %a@\n@\n" (print_files cvcl) targets;
       fprintf fmt "\t@@echo 'Running CVC Lite on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "cvcl/%%_why.cvc: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -cvcl [...] why/$*.why' && $(WHY) -cvcl -dir cvcl $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "harvey: %a@\n" (print_files harvey) targets;
       fprintf fmt "\t@@echo 'Running haRVey on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "harvey/%%_why.rv: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -harvey [...] why/$*.why' && $(WHY) -harvey -dir harvey $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "zenon: %a@\n" (print_files zenon) targets;
       fprintf fmt "\t@@echo 'Running Zenon on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "zenon/%%_why.znn: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -zenon [...] why/$*.why' && $(WHY) -zenon -dir zenon $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "smtlib: %a@\n" (print_files smtlib) targets;
       fprintf fmt "\t@@echo 'Running Yices on proof obligations' && (dp -timeout $(TIMEOUT) $^)@\n@\n";
       fprintf fmt "smtlib/%%_why.smt: why/%%.why@\n";
       fprintf fmt "\t@@echo 'why -smtlib [...] why/$*.why' && $(WHY) -smtlib --encoding sstrat --exp goal -dir smtlib $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "gui stat: %s@\n" 
	 (match targets with f::_ -> f^".stat" | [] -> "");
       fprintf fmt "@\n";
       fprintf fmt "%%.stat: why/%%.why@\n";
       fprintf fmt "\t@@echo 'gwhy-bin [...] why/$*.why' && $(GWHY) $(JESSIELIBFILE) why/$*.why@\n@\n";
       
       fprintf fmt "-include %s.depend@\n@\n" f;
       fprintf fmt "depend: %a@\n" (print_files coq_v) targets;
       fprintf fmt "\t-$(COQDEP) -I coq coq/%s*_why.v > %s.depend@\n@\n" f f;
       fprintf fmt "clean:@\n";
       fprintf fmt "\trm -f coq/*.vo@\n@\n";
    )
    (f ^ ".makefile")

let makefile f = generic f [f]



(*
Local Variables: 
compile-command: "make -C .. bin/jessie.byte"
End: 
*)

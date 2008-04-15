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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: jc_make.ml,v 1.43 2008/11/05 14:03:15 filliatr Exp $ i*)

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

let generic full f targets =
  print_in_file 
    (fun fmt -> 
       let out x = fprintf fmt x in
       out "# this makefile was automatically generated; do not edit @\n@\n";
       out "TIMEOUT ?= 10@\n@\n";	    
       out "DP ?= why-dp -timeout $(TIMEOUT)@\n";
       out "WHYEXEC ?= why@\n";
       out "GWHYEXEC ?= gwhy-bin@\n";
       out "WHYLIB ?= %s@\n@\n" (String.escaped Jc_options.libdir);
       out "WHY=WHYLIB=$(WHYLIB) $(WHYEXEC) --no-arrays %s -explain -locs %s.loc@\n@\n" (Jc_options.why_opt) f;
       out "GWHY=WHYLIB=$(WHYLIB) $(GWHYEXEC) --no-arrays %s -explain -locs %s.loc@\n@\n" (Jc_options.why_opt) f;
       out "JESSIELIBFILES ?=";
       List.iter (fun s -> 
		    out " %s"
		      (String.escaped (Filename.concat "$(WHYLIB)" 
					 (Filename.concat "why" s))))
	 (Jc_options.libfiles ());
       out "@\n@\n";
       out "COQDEP = coqdep@\n@\n";

       out ".PHONY: all coq pvs simplify cvcl harvey smtlib zenon@\n@\n";
       out "all: %a@\n@\n" 
	 (print_files simplify) targets;

       out "project: %a@\n@\n" (print_files wpr) targets;
       out "why/%%.wpr: why/%%.why@\n";
       out "\t@@echo 'why --project [...] why/$*.why' && $(WHY) --project -dir why $(JESSIELIBFILES) why/$*.why@\n@\n";

       out "goals: %a@\n@\n" (print_files why_goals) targets;
       out "why/%%_ctx.why: why/%%.why@\n";
       out "\t@@echo 'why --multi-why [...] why/$*.why' && $(WHY) --multi-why -dir why $(JESSIELIBFILES) why/$*.why@\n@\n";

       out "coq: %a@\n@\n" (print_files coq_vo) targets;

       out "coq/%s_why.v: why/%s.why@\n" f f;
       out "\t@@echo 'why -coq-v8 [...] why/%s.why' &&$(WHY) -coq-v8 -dir coq -coq-preamble \"Require Export jessie_why.\" -coq-tactic \"intuition\" $(JESSIELIBFILES) why/%s.why@\n@\n" f f;


       out "coq-goals: goals coq/%s_ctx_why.vo@\n" f;
       out "\tfor f in why/%s_po*.why; do make -f %s.makefile coq/`basename $$f .why`_why.v ; done@\n@\n" f f;

       
       out "coq/%s_ctx_why.v: why/%s_ctx.why@\n" f f;
       out "\t@@echo 'why -coq-v8 [...] why/%s_ctx.why' &&$(WHY) -no-prelude -coq-v8 -dir coq -coq-preamble \"Require Export jessie_why.\" -coq-tactic \"intuition\" why/%s_ctx.why@\n@\n" f f;

       out "coq/%%_why.v: why/%%.why@\n";
       out "\t@@echo 'why -coq-v8 [...] why/$*.why' &&$(WHY) -no-prelude -coq-v8 -dir coq -coq-preamble \"Require Export %s_ctx_why.\" -coq-tactic \"intuition\" why/%s_ctx.why why/$*.why@\n@\n" f f;
       out "coq/%%.vo: coq/%%.v@\n\tcoqc -I coq $<@\n@\n";
       
       out "pvs: %a@\n@\n" (print_files pvs) targets;

       out "pvs/%%_why.pvs: why/%%.why@\n";
       out "\t$(WHY) -pvs -dir pvs -pvs-preamble \"IMPORTING why@@jessie\" $(JESSIELIBFILES) why/$*.why@\n@\n";

       out "pvs/jessie_why.pvs:@\n";
       out "\t$(WHY) -pvs -dir pvs -pvs-preamble \"IMPORTING why@@why\" $(JESSIELIBFILES)@\n@\n";
       
       out "isabelle: %a@\n@\n" (print_files isabelle) targets;

       out "isabelle/%%_why.thy: why/%%.why@\n";
       out "\t$(WHY) -isabelle -dir isabelle -isabelle-base-theory jessie_why $(JESSIELIBFILES) why/$*.why@\n";
       out "\tcp -f %s/isabelle/jessie_why.thy isabelle/@\n@\n" 
	 Jc_options.libdir;

       out "simplify: %a@\n" (print_files simplify) targets;
       out "\t@@echo 'Running Simplify on proof obligations' && ($(DP) $^)@\n@\n";
       out "simplify/%%_why.sx: why/%%.why@\n";
       out "\t@@echo 'why -simplify [...] why/$*.why' && $(WHY) -simplify -dir simplify $(JESSIELIBFILES) why/$*.why@\n@\n";
       
       out "alt-ergo ergo: %a@\n" (print_files ergo) targets;
       out "\t@@echo 'Running Alt-Ergo on proof obligations' && ($(DP) $^)@\n@\n";
       out "why/%%_why.why: why/%%.why@\n";
       out "\t@@echo 'why -alt-ergo [...] why/$*.why' && $(WHY) -alt-ergo -dir why $(JESSIELIBFILES) why/$*.why@\n@\n";

       out "cvcl: %a@\n@\n" (print_files cvcl) targets;
       out "\t@@echo 'Running CVC Lite on proof obligations' && ($(DP) $^)@\n@\n";
       out "cvcl/%%_why.cvc: why/%%.why@\n";
       out "\t@@echo 'why -cvcl [...] why/$*.why' && $(WHY) -cvcl -dir cvcl $(JESSIELIBFILES) why/$*.why@\n@\n";
       
       out "harvey: %a@\n" (print_files harvey) targets;
       out "\t@@echo 'Running haRVey on proof obligations' && ($(DP) $^)@\n@\n";
       out "harvey/%%_why.rv: why/%%.why@\n";
       out "\t@@echo 'why -harvey [...] why/$*.why' && $(WHY) -harvey -dir harvey $(JESSIELIBFILES) why/$*.why@\n@\n";
       
       out "zenon: %a@\n" (print_files zenon) targets;
       out "\t@@echo 'Running Zenon on proof obligations' && ($(DP) $^)@\n@\n";
       out "zenon/%%_why.znn: why/%%.why@\n";
       out "\t@@echo 'why -zenon [...] why/$*.why' && $(WHY) -zenon -dir zenon $(JESSIELIBFILES) why/$*.why@\n@\n";
       
       out "smtlib: %a@\n" (print_files smtlib) targets;
       out "\t@@echo 'Running Z3 on proof obligations' && ($(DP) $^)@\n@\n";
       out "smtlib/%%_why.smt: why/%%.why@\n";
       out "\t@@echo 'why -smtlib [...] why/$*.why' && $(WHY) -smtlib --encoding sstrat --exp goal -dir smtlib $(JESSIELIBFILES) why/$*.why@\n@\n";

       out "z3: %a@\n" (print_files smtlib) targets;
       out "\t@@echo 'Running Z3 on proof obligations' && ($(DP) -smt-solver z3 $^)@\n@\n";

       out "yices: %a@\n" (print_files smtlib) targets;
       out "\t@@echo 'Running Yices on proof obligations' && ($(DP) -smt-solver yices $^)@\n@\n";
       
       out "cvc3: %a@\n" (print_files smtlib) targets;
       out "\t@@echo 'Running CVC3 on proof obligations' && ($(DP) -smt-solver cvc3 $^)@\n@\n";

       out "gui stat: %s@\n" 
	 (match targets with f::_ -> f^".stat" | [] -> "");
       out "@\n";
       out "%%.stat: why/%%.why@\n";
       out "\t@@echo 'gwhy-bin [...] why/$*.why' && $(GWHY) $(JESSIELIBFILES) why/$*.why@\n@\n";
       
       out "-include %s.depend@\n@\n" f;
       out "depend: %a@\n" (print_files coq_v) targets;
       out "\t-$(COQDEP) -I coq coq/%s*_why.v > %s.depend@\n@\n" f f;
       out "clean:@\n";
       out "\trm -f coq/*.vo@\n@\n";
    )
    (full ^ ".makefile")

let makefile f = 
  let c = Filename.basename f in
  generic f c [c]



(*
Local Variables: 
compile-command: "make -C .. bin/jessie.byte"
End: 
*)

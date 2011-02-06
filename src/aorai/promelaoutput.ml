(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* $Id: promelaoutput.ml,v 1.3 2008-12-19 15:30:56 uid588 Exp $ *)

open Aorai_option
open Pretty_utils
open Promelaast
open Format open Pervasives
open Data_for_aorai
open Bool3


let out_fmt=ref (formatter_of_out_channel stdout)




(*let string_of_condition_arith = function
    | PVar s           -> "Var("^s^")"
    | PConst c         -> "Const("^(string_of_int c)^")"
*)
let rec string_of_condition = function
    | PCall s          -> "Call("^s^")"
    | PReturn s        -> "Return("^s^")"
    | PCallOrReturn s  -> "CallOrReturn("^s^")"
    | POr  (c1,c2)     -> "("^(string_of_condition c1)^" or "^(string_of_condition c2)^")"
    | PAnd (c1,c2    ) -> "("^(string_of_condition c1)^" and "^(string_of_condition c2)^")"
    | PNot c           -> "not "^(string_of_condition c)
    | PTrue            -> "True"
    | PFalse           -> "False"
(*    | PGt (c1,c2)      -> (string_of_condition_arith c1)^">" ^(string_of_condition_arith c2)
    | PGe (c1,c2)      -> (string_of_condition_arith c1)^">="^(string_of_condition_arith c2)
    | PLt (c1,c2)      -> (string_of_condition_arith c1)^"<" ^(string_of_condition_arith c2)
    | PLe (c1,c2)      -> (string_of_condition_arith c1)^"<="^(string_of_condition_arith c2)
    | PEq (c1,c2)      -> (string_of_condition_arith c1)^"=" ^(string_of_condition_arith c2)
    | PNeq (c1,c2)     -> (string_of_condition_arith c1)^"<>"^(string_of_condition_arith c2)
    | PBoolVar (s)     -> "BoolVar("^s^")"*)
    | PIndexedExp (s)      -> Data_for_aorai.get_str_exp_from_tmpident s
    | PFuncReturn (s, f)   -> "("^f^"()."^(Data_for_aorai.get_str_exp_from_tmpident s)^")"
    | PFuncParam (s, f, _) -> "("^f^"()."^(Data_for_aorai.get_str_exp_from_tmpident s)^")"

(*let c_string_of_condition_arith = function
    | PVar s           -> s
    | PConst c         -> string_of_int c
*)
let rec c_string_of_condition = function
    | PCall s          -> "(("^s^"=="^curOp^") && ("^curOpStatus^"=="^callStatus^"))"
    | PReturn s        -> "(("^s^"=="^curOp^") && ("^curOpStatus^"=="^termStatus^"))"
    | PCallOrReturn s  -> "("^s^"=="^curOp^")"
    | POr  (c1,c2)     -> "("^(c_string_of_condition c1)^" || "^(c_string_of_condition c2)^")"
    | PAnd (c1,c2    ) -> "("^(c_string_of_condition c1)^" && "^(c_string_of_condition c2)^")"
    | PNot c           -> "!"^(c_string_of_condition c)
    | PTrue            -> "1"
    | PFalse           -> "0"
(*    | PGt (c1,c2)      -> (c_string_of_condition_arith c1)^">" ^(c_string_of_condition_arith c2)
    | PGe (c1,c2)      -> (c_string_of_condition_arith c1)^">="^(c_string_of_condition_arith c2)
    | PLt (c1,c2)      -> (c_string_of_condition_arith c1)^"<" ^(c_string_of_condition_arith c2)
    | PLe (c1,c2)      -> (c_string_of_condition_arith c1)^"<="^(c_string_of_condition_arith c2)
    | PEq (c1,c2)      -> (c_string_of_condition_arith c1)^"==" ^(c_string_of_condition_arith c2)
    | PNeq (c1,c2)     -> (c_string_of_condition_arith c1)^"!="^(c_string_of_condition_arith c2)
    | PBoolVar (s)     -> s*)
    | PIndexedExp (s) -> Data_for_aorai.get_str_exp_from_tmpident s
    | PFuncReturn (s, _) 
    | PFuncParam (s, _, _) -> Data_for_aorai.get_str_exp_from_tmpident s


let trans_label num = "tr"^string_of_int(num)
let string_of_trans num cross=
  (trans_label num) ^" : "^(string_of_condition cross)

let state_label num = "st"^string_of_int(num)
let string_of_state st = (state_label st.nums) ^" : "^(st.name)

let print_bool3 b =
  Format.print_string (match b with
    | True -> "True"
    | False -> "False"
    | Undefined -> "Undef"
  )


let rec print_cross cr =
  printf "%s" (string_of_condition cr)

let print_transition tr =
  Format.print_string ("  { ");
  Format.print_int tr.numt;
  Format.print_string (": "^tr.start.name^" ") ;
  print_cross tr.cross ;
  Format.print_string (" "^tr.stop.name ^ " }\n")

let print_transitionl trl =
  Format.print_string ("Transitions : \n") ;
  List.iter print_transition trl


let print_state st =
  Format.print_string ("   "^st.name^" (acc=");
  print_bool3 st.acceptation ;
  Format.print_string (";init=");
  print_bool3 st.init;
  Format.print_string (";num=");
  Format.print_int st.nums;
  Format.print_string (")\n")

let print_statel stl =
  Format.print_string ("States : \n") ;
  List.iter print_state stl


let print_raw_automata (stl,trl) =
  Format.print_string ("Transitions : \n") ;
  (print_statel stl) ;
  (print_transitionl trl)



(*
let print_automata_axiomatization (_ (*states_l*), trans_l) =
  (* Generation des transitions *)
  fprintf !out_fmt "logic %s : int -> int \n" transStart ;
  fprintf !out_fmt "\n" ;
  List.iter (fun t ->
    fprintf !out_fmt "axiom %s_%d : (%s(%d) = %d)\n" transStart t.numt transStart t.numt t.start.nums
  ) trans_l;
  fprintf !out_fmt "\n" ;


  fprintf !out_fmt "logic %s : int -> int \n" transStop ;
  fprintf !out_fmt "\n" ;
  List.iter (fun t ->
    fprintf !out_fmt "axiom %s_%d : (%s(%d) = %d)\n" transStop t.numt transStop t.numt t.stop.nums
  ) trans_l;
  fprintf !out_fmt "\n" ;

  fprintf !out_fmt "predicate %s(%s: int, %s: int, num: int) = \n" transCond curOpStatus curOp;
  let first=(List.hd trans_l) in
    fprintf !out_fmt "  ((num=%d) -> (%s)) " first.numt (string_of_condition first.cross);
      List.iter (fun t ->
	printf "and\n  ((num=%d) -> (%s)) " t.numt (string_of_condition   t.cross)
      ) (List.tl trans_l);
    fprintf !out_fmt "\n)\n"


*)










let print_start_block title =
  fprintf !out_fmt "//========================\n// BEGIN %s\n//\n" title

let print_end_block title =
  fprintf !out_fmt "//\n// END %s\n//========================\n\n" title




let print_operations_list opl =
  print_start_block "Operations list";
  fprintf !out_fmt "#define %s = %d \n" nbOp (List.length opl);
  fprintf !out_fmt "enum %s {" listOp;
  let v=ref "" in
    List.iter (fun op -> fprintf !out_fmt "%s\n  %s" !v op;v:=",") opl;
  fprintf !out_fmt "\n}\n";
  print_end_block "Operations list"



let print_operations_constants (states_l,trans_l) =
  print_start_block "Operations status";
  fprintf !out_fmt "enum Status_list {\n";
  fprintf !out_fmt "  %s,\n" callStatus;
  fprintf !out_fmt "  %s\n" termStatus;
  fprintf !out_fmt "}\n";
  print_end_block "Operations status";

  print_start_block "Some constants about the Buchi automata";
  fprintf !out_fmt "# define %s=%d\n" nbStates (List.length states_l);
  fprintf !out_fmt "# define %s=%d\n" nbTrans (List.length trans_l);
  fprintf !out_fmt "# define %s=%d\n" nbAcceptSt (List.fold_left (fun i s -> if s.acceptation=True then i+1 else i) 0 states_l);
  print_end_block "Some constants about the Buchi automata"




let print_ghosts_declaration main states_l=
  print_start_block "State ghosts variables declaration";
  fprintf !out_fmt "//%c ghost int %s = %s \n" '@' curOp main ;
  fprintf !out_fmt "//%c ghost int %s = %s \n" '@' curOpStatus callStatus ;
  fprintf !out_fmt "//%c ghost int %s[%s] \n" '@' curState nbStates;
  fprintf !out_fmt "//%c ghost int %s[%s] \n" '@' curTrans nbTrans;
  let acc = ref "" in
  let sep = ref "{" in
    (List.iter
	(fun st ->
	  if st.acceptation=True then
	    begin
	      acc:=(!acc)^(!sep)^(string_of_int st.nums);
	      sep:=","
	    end)
	states_l);
    fprintf !out_fmt "const int %s[%s] = %s}\n" acceptSt nbAcceptSt !acc;
    print_end_block "State ghosts variables declaration"

(*
let print_automata (_ (*states_l*),trans_l) =
  print_start_block "Automata definition";

  fprintf !out_fmt "// Starting state of each transition\n//\n";
  fprintf !out_fmt "//%c logic int %s (int tr) reads tr\n" '@' transStart ;
  List.iter (fun t ->
    fprintf !out_fmt "//%c axiom %s_%d : %s(%d) == %d\n" '@' transStart t.numt transStart t.numt t.start.nums
  ) trans_l;


  fprintf !out_fmt "//\n// Ending state of each transition\n//\n";
  fprintf !out_fmt "//%c logic int %s (int tr) reads tr\n" '@' transStop ;
  List.iter (fun t ->
    fprintf !out_fmt "//%c axiom %s_%d : %s(%d) == %d\n" '@' transStop t.numt transStop t.numt t.stop.nums
  ) trans_l;

  fprintf !out_fmt "//\n// Cross condition of each transition\n//\n";
  fprintf !out_fmt "/*%c predicate %s (int TransNum, int %s, int %s) = \n" '@' transCondP curOp curOpStatus;
  fprintf !out_fmt "  %c   ((TransNum==%d) => (%s)) " '@' (List.hd trans_l).numt (c_string_of_condition (List.hd trans_l).cross);
  List.iter (fun t ->
    fprintf !out_fmt "&&\n  %c   ((TransNum==%d) => (%s)) " '@' t.numt (c_string_of_condition t.cross)
  ) (List.tl trans_l);
  fprintf !out_fmt "\n)\n*/\n" ;

  fprintf !out_fmt "//%c predicate %s (int TransNum) = %s (TransNum,%s,%s)\n" '@' transCond transCondP curOp curOpStatus;


  fprintf !out_fmt "//\n// Some invariants\n//\n";
  fprintf !out_fmt "//%c invariant inv_buch_range : \\valid_range(%s,0,%s-1) \n" '@' curState nbStates;
  fprintf !out_fmt "//%c invariant inv_buch_accept_valid: \\valid_range(%s,0,%s-1) \n" '@' acceptSt  nbAcceptSt;
  fprintf !out_fmt "//%c invariant inv_buch_accept_correct: \\forall int st ; 0<=st<%s => 0<=%s[st]<%s \n" '@' nbAcceptSt acceptSt nbStates;

  print_end_block "Automata definition"
*)

















let print_macros (states_l,trans_l) =
  print_start_block "Some macros factorizing pre/post-conditions predicates";

  fprintf !out_fmt "# define %s=(op,st) \\\n" macro_ligth;
  fprintf !out_fmt "      %s == op\\\n" curOp;
  fprintf !out_fmt "  &&  %s == st\\\n" curOpStatus;
  fprintf !out_fmt "  && (%s[ 0] != 0" curState;
  for i=1 to (List.length states_l)-1 do
      fprintf !out_fmt " ||\\\n      %s[%2d] != 0" curState i
  done;
  fprintf !out_fmt ")\\\n";
  fprintf !out_fmt "  && (%s[ 0] != 0" curTrans;
  for i=1 to (List.length trans_l)-1 do
    fprintf !out_fmt " ||\\\n      %s[%2d] != 0" curTrans i
  done ;
  fprintf !out_fmt ")\n";
  fprintf !out_fmt "//\n";


  fprintf !out_fmt "# define %s=(op,st) \\\n" macro_full;
  fprintf !out_fmt "  %s(op,et) \\\n" macro_ligth;
  fprintf !out_fmt "  && (\\forall int tr ; 0<=tr<%s && %s[tr]!=0 => %s[%s(tr)]!=0 && %s(tr)) \\\n" nbTrans curTrans curState transStop transCond;
  fprintf !out_fmt "  && (\\forall int st ; 0<=st<%s && %s[st]!=0=> \\\n" nbStates curState;
  fprintf !out_fmt "        (\\exists int tr ; 0<=tr<%s && %s(tr) && %s(tr)==st && %s[tr]!=0)) \\\n" nbTrans transCond transStop curTrans;
  fprintf !out_fmt "  && (\\forall int st ;  \\\n";
  fprintf !out_fmt "        0<=st<%s && \\\n" nbStates;
  fprintf !out_fmt "        (\\forall int tr ;  \\\n";
  fprintf !out_fmt "            0<=tr<%s => \\\n" nbTrans;
  fprintf !out_fmt "                (%s[tr]==0 ||  \\\n" curTrans;
  fprintf !out_fmt "                 %s(tr)!=st ||  \\\n" transStop;
  fprintf !out_fmt "                 !%s(tr)) \\\n" transCond;
  fprintf !out_fmt "        ) \\\n";
  fprintf !out_fmt "       => %s[st]==0 \\\n" curState;
  fprintf !out_fmt "     )\n";
  fprintf !out_fmt "//\n";


  fprintf !out_fmt "# define %s \\\n"  macro_pure ;
  fprintf !out_fmt "     (\\forall int st ; 0<=st<%s && %s[st]==0 =>                                     \\\n" nbStates curState;
  fprintf !out_fmt "          (\\forall int tr ; 0<=tr<%s =>                                                          \\\n" nbTrans;
  fprintf !out_fmt "             (   %s[tr]==0 || !%s(tr)                                                   \\\n" curTrans transCond;
  fprintf !out_fmt "              || %s(tr)!=st || \\old(%s[%s(tr)]==0))))                    \\\n" transStop curState transStart;
  fprintf !out_fmt "  && (\\forall int end ; 0<=end<%s && %s[end]!=0 =>                                  \\\n" nbStates curState;
  fprintf !out_fmt "          (\\exists int tr ; 0<=tr<%s && %s[tr]!=0 && %s(tr) &&                     \\\n" nbTrans curTrans transCond;
  fprintf !out_fmt "                            end==%s(tr) && \\old(%s[%s(tr)]!=0)))\n" transStop curState transStart;
  print_end_block "Some macros factorizing pre/post-conditions predicates"
















let print_buch_synch () =
  print_start_block "Function of synchronisation between C code and Buchi automata";

  fprintf !out_fmt "/*%c requires \n" '@';
  fprintf !out_fmt "  %c   \\forall int st ; 0<= st < %s && %s[st]!=0 => \n" '@' nbStates curState;
  fprintf !out_fmt "  %c      (\\exists int tr ; 0<=tr<%s && %s(tr)==st && %s(tr,CurOp,Case))\n" '@' nbTrans transStart transCondP;
  fprintf !out_fmt "  %c assigns %s[..], %s[..], %s, %s\n" '@' curState curTrans curOp curOpStatus;
  fprintf !out_fmt "  %c ensures \n" '@';
  fprintf !out_fmt "  %c   %s(CurOp,Case) &&  \n" '@' macro_ligth;
  fprintf !out_fmt "  %c\n" '@';
  fprintf !out_fmt "  %c   // Each crossable transition is crossed.\n" '@';
  fprintf !out_fmt "  %c   (\\forall int tr ; 0<=tr<%s && \\old(%s[%s(tr)])!=0 && %s(tr)=> \n" '@' nbTrans curState transStart transCond;
  fprintf !out_fmt "  %c          %s[tr]!=0 && %s[%s(tr)]!=0\n" '@' curTrans curState transStop;
  fprintf !out_fmt "  %c   ) &&\n" '@';
  fprintf !out_fmt "  %c   // Non-crossable transition are not crossed over.\n" '@';
  fprintf !out_fmt "  %c   (\\forall int tr ; 0<=tr<%s && (\\old(%s[%s(tr)])==0 || !%s(tr)) =>\n" '@' nbTrans curState transStart transCond;
  fprintf !out_fmt "  %c          %s[tr]==0\n" '@' curTrans;
  fprintf !out_fmt "  %c   ) &&\n" '@';
  fprintf !out_fmt "  %c   // Each transition annotated as crossable is crossable\n" '@' ;
  fprintf !out_fmt "  %c   // -- Interesting for preconditions that follow the operation call --\n"  '@';
  fprintf !out_fmt "  %c   (\\forall int tr ; 0<=tr<%s && %s[tr]!=0 => \n" '@' nbTrans curTrans;
  fprintf !out_fmt "  %c          \\old(%s[%s(tr)])!=0 && %s (tr) && %s[%s(tr)]!=0\n" '@' curState transStart transCond curState transStop;
  fprintf !out_fmt "  %c   ) &&\n" '@' ;
  fprintf !out_fmt "  %c\n" '@' ;
  fprintf !out_fmt "  %c   // If a state is annotated as not reachable, then no crossable transition reaches it is crossable\n" '@';
  fprintf !out_fmt "  %c   (\\forall int st ; \n" '@' ;
  fprintf !out_fmt "  %c       0<=st<%s &&\n" '@' nbStates;
  fprintf !out_fmt "  %c       (\\forall int tr ; \n" '@' ;
  fprintf !out_fmt "  %c           0<=tr<%s => \n" '@' nbTrans;
  fprintf !out_fmt "  %c               (%s[tr]==0 || \n" '@' curTrans;
  fprintf !out_fmt "  %c                %s(tr)!=st || \n" '@' transStop;
  fprintf !out_fmt "  %c                !%s(tr) || \n" '@' transCond;
  fprintf !out_fmt "  %c                \\old(%s[%s(tr)])==0)\n" '@' curState transStart;
  fprintf !out_fmt "  %c       )\n" '@' ;
  fprintf !out_fmt "  %c      => %s[st]==0\n" '@' curState;
  fprintf !out_fmt "  %c   ) &&\n" '@' ;
  fprintf !out_fmt "  %c\n" '@' ;
  fprintf !out_fmt "  %c   // Each non-active state is not reachable\n" '@' ;
  fprintf !out_fmt "  %c   (\\forall int st ; 0<=st<%s && %s[st]==0 => \n" '@' nbStates curState;
  fprintf !out_fmt "  %c       (\\forall int tr ; 0<=tr<%s => \n" '@' nbTrans;
  fprintf !out_fmt "  %c           (   %s[tr]==0 || !%s(tr) \n" '@' curTrans transCond;
  fprintf !out_fmt "  %c            || %s(tr)!=st || \\old(%s[%s(tr)]==0)))) && \n" '@' transStop curState transStart;
  fprintf !out_fmt "  %c   // Each active state is reachable \n" '@' ;
  fprintf !out_fmt "  %c   (\\forall int st ; 0<=st<%s && %s[st]!=0 =>  \n" '@' nbStates curState;
  fprintf !out_fmt "  %c       (\\exists int tr ; 0<=tr<%s && %s[tr]!=0 && %s(tr)  \n" '@' nbTrans curTrans transCond;
  fprintf !out_fmt "  %c                      && %s(tr)==st && \\old(%s[%s(tr)]!=0))) \n" '@' transStop curState transStart;
  fprintf !out_fmt "*/ \n" ;
  fprintf !out_fmt "void %s(int CurOp, int Case); \n" buch_sync;

  print_end_block "Function of synchronisation between C code and Buchi automata"







(*
let print_automata_specification (states_l,trans_l) operations_l main_op fichier =
  let cout = open_out fichier in
    out_fmt:=formatter_of_out_channel cout ;

    fprintf !out_fmt "#ifndef _BUCHI_AUTOMATA_H_\n";
    fprintf !out_fmt "#define _BUCHI_AUTOMATA_H_\n\n";
    print_operations_constants (states_l,trans_l);
    print_operations_list operations_l ;
    print_automata (states_l,trans_l) ;
    print_ghosts_declaration main_op states_l;
    print_macros (states_l,trans_l);
    print_buch_synch ();
    fprintf !out_fmt "\n#endif /*_BUCHI_AUTOMATA_H_*/\n";

    close_out cout;
    out_fmt:=formatter_of_out_channel stdout
*)










let dot_state out st =
  if st.init=Bool3.True && st.acceptation=Bool3.True then
    fprintf out "  \"%s\" [shape = doubleoctagon];\n" (string_of_state st)
  else if st.acceptation=Bool3.True then
    fprintf out "  \"%s\" [shape = octagon];\n" (string_of_state st)
  else if st.init=Bool3.True then
    fprintf out "  \"%s\" [shape = doublecircle];\n" (string_of_state st)
  else
    fprintf out "  \"%s\" [shape = circle];\n" (string_of_state st)



let dot_trans out tr =
  fprintf
    out
    "  \"%s\" -> \"%s\" [ label = \"%s\"];\n"
    (string_of_state tr.start)
    (string_of_state tr.stop)
    (if DotSeparatedLabels.get() then
       trans_label tr.numt
     else string_of_trans tr.numt tr.cross)

let dot_guards out tr =
  fprintf out "%s" (string_of_trans tr.numt tr.cross)

let output_dot_automata (states_l,trans_l) fichier =
  let cout = open_out fichier in
  out_fmt:=formatter_of_out_channel cout ;

  fprintf !out_fmt "/* File generated by Aorai LTL2ACSL Plug-in                      */\n";
  fprintf !out_fmt "/*                                                               */\n";
  fprintf !out_fmt "/* Usage of dot files '.dot' :                                   */\n";
  fprintf !out_fmt "/*    dot <MyFile.dot> -T<DesiredType> > <OutputFile>            */\n";
  fprintf !out_fmt "/*                                                               */\n";
  fprintf !out_fmt "/*    Allowed types : canon,dot,xdot,fig,gd,gd2,                 */\n";
  fprintf !out_fmt "/*      gif,hpgl,imap,cmap,ismap,jpg,jpeg,mif,mp,pcl,pic,plain,  */\n";
  fprintf !out_fmt "/*      plain-ext,png,ps,ps2,svg,svgz,vrml,vtx,wbmp              */\n";
  fprintf !out_fmt "/*                                                               */\n";
  fprintf !out_fmt "/* Example with postscript file :                                */\n";
  fprintf !out_fmt "/*    dot property.dot -Tps > property.ps                        */\n";
  fprintf !out_fmt "";
  fprintf !out_fmt "digraph %s {\n"
    (Filename.chop_extension (Filename.basename fichier));
  fprintf !out_fmt "\n";
  List.iter (dot_state !out_fmt) states_l;
  fprintf !out_fmt "\n";
  List.iter (dot_trans !out_fmt) trans_l;
  fprintf !out_fmt "\n";
  if DotSeparatedLabels.get () then begin
    fprintf !out_fmt "/* guards of transitions */@\ncomment=\"%a\";@\n"
      (Pretty_utils.pp_list ~sep:(format_of_string "\\n") dot_guards) trans_l
  end;
  fprintf !out_fmt "} /*End of graph*/\n";
  fprintf !out_fmt "\n";

  close_out cout;
  out_fmt:=formatter_of_out_channel stdout





(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
(**************************************************************************)

(* $Id: data_for_ltl.ml,v 1.4 2008/11/18 16:37:29 uid562 Exp $ *)

open Cil_types

(* ************************************************************************* *)
let ltl_exps = ref (Hashtbl.create 1)

let setLtl_expressions exps =
  ltl_exps:=exps

let ltl_expressions_iter func =
  Hashtbl.iter  func !ltl_exps

let get_exp_from_tmpident var =
  try
    let (exp,_,_) = (Hashtbl.find !ltl_exps var) in exp
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : TMP Variable (%s) not declared in hashtbl. \n" var; assert false

let get_str_exp_from_tmpident var =
  try
    let (_,str,_) = (Hashtbl.find !ltl_exps var) in
      "("^(str)^")"
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : TMP Variable (%s) not declared in hashtbl. \n" var; assert false

let get_pred_from_tmpident var =
  try
    let (_,_,pred) = (Hashtbl.find !ltl_exps var) in pred
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : TMP Variable '%s' not declared in hashtbl. \n" var; assert false



let declared_logics = Hashtbl.create 97

let add_logic name log_info =
  begin
    if not (Hashtbl.mem declared_logics name) then
      Hashtbl.remove declared_logics name;
    Hashtbl.add declared_logics name log_info
  end

let get_logic name =
  try
    Hashtbl.find declared_logics name
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Logic function '%s' not declared in hashtbl. \n" name; assert false



let declared_predicates = Hashtbl.create 97

let add_predicate name pred_info =
  begin
    if not (Hashtbl.mem declared_predicates name) then
      Hashtbl.remove declared_predicates name;
    Hashtbl.add declared_predicates name pred_info
  end

let get_predicate name =
  try
    Hashtbl.find declared_predicates name
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Predicate '%s' not declared in hashtbl. \n" name; assert false



(* ************************************************************************* *)
(* Some constant names used for generation *)
(* Logic variables *)
let transStart  = "buch_Trans_Start"                    (* OK *)
let transStop   = "buch_Trans_Stop"                     (* OK *)
let transCond   = "buch_Trans_Cond"                     (* OK *)
let transCondP  = "buch_Trans_Cond_param"               (* OK *)
let loopInit    = "buch_Loop_Init"                      (* OK *)

(* C variables *)
let curState    = "buch_CurStates"                      (* OK *)
let curStateOld = "buch_CurStates_old"                  (* OK *)
let curTrans    = "buch_CurTrans"                       (* OK *)
(*let curTransTmp = "buch_CurTrans_tmp"                   (* OK *)*)
let curOp       = "buch_CurOperation"                   (* OK *)
let curOpStatus = "buch_CurOpStatus"                    (* OK *)
let acceptSt    = "buch_AcceptStates"                   (* TODO *)

(* C constants #define *)
let nbOp        = "buch_NbOper"                         (* Deprecated ? *)
let nbStates    = "buch_NbStates"                       (* Deprecated ? *)
let nbAcceptSt  = "buch_NbAcceptStates"                 (* Deprecated ? *)
let nbTrans     = "buch_NbTrans"                        (* Deprecated ? *)

(* C Macros *)
let macro_ligth = "buch_Macro_Prop_St_Tr_Without_Conds" (* Deprecated ? *)
let macro_full  = "buch_Macro_Prop_St_Tr"               (* Deprecated ? *)
let macro_pure  = "buch_Macro_Op_without_sub_call"      (* Deprecated ? *)

(* C enumeration *)
let listOp      = "buch_ListOper"                       (* OK *)
let listStatus  = "Buch_OpStatusList"                   (* OK *)
let callStatus  = "buch_Called"                         (* OK *)
let termStatus  = "buch_Terminated"                     (* OK *)

(* C function *)
let buch_sync   = "Buch_Sync"                           (* Deprecated ? *)





(* ************************************************************************* *)
(* Buchi automata as stored after parsing *)
let automata = ref ([],[])

(* List of variables name observed in the promela file *)
let variables_from_auto = ref []
(* List of functions name observed in the promela file *)
let functions_from_auto = ref []

(* List of variables name observed in the C file *)
let variables_from_c = ref []
(* List of functions name observed in the C file *)
let functions_from_c = ref []




(* Return the buchi automata as stored after parsing *)
let getAutomata () =
  !automata

(* Stores the buchi automata and its variables and functions as such as it  is return by the parsing *)
let setAutomata auto vars funcs =
  automata:=auto;
  variables_from_auto:=Hashtbl.fold (fun k _ l -> k::l) vars [];
  functions_from_auto:=Hashtbl.fold (fun k _ l -> k::l) funcs []

(* Initializes some tables according to data from Cil AST. *)
let setCData () =
  functions_from_c:=
    Globals.Functions.fold (fun f lf -> (Kernel_function.get_name f)::lf) [];
  variables_from_c:=
    Globals.Vars.fold (fun v _ lv -> (Cil.fprintf_to_string "%a" Ast_info.pretty_vname v)::lv) []


(* Return the number of transitions of the automata *)
let getNumberOfTransitions () =
  List.length (snd !automata)

(* Return the number of states of the automata *)
let getNumberOfStates () =
  List.length (fst !automata)




(* Return the list of all function name observed in the promela file. *)
let getFunctions_from_auto () =
  (!functions_from_auto)

(* Return the list of all variables name observed in the promela file. *)
let getVariables_from_auto () =
  (!variables_from_auto)

(* Return the list of all function name observed in the C file. *)
let getFunctions_from_c () =
  (!functions_from_c)

(* Return the list of all variables name observed in the C file. *)
let getVariables_from_c () =
  (!variables_from_c)





(* Manage particular consistency verification between C file and promela specification.
    It returns true if and only if these checks are ok. *)
let check_consistency () =
  (* Checking consistency *)
  let included_funs =
    List.for_all
      (fun fl ->
	 let r=List.exists (fun fc -> fc=fl) !functions_from_c in
	 if not r then Format.printf "Error : function '%s' from LTL not found in C code.\n" fl;
	 r
      )
      (!functions_from_auto)
  in
  let included_vars =
    List.for_all
      (fun vl ->
	 let r=(List.exists (fun vc -> vc=vl) ("result"::!variables_from_c)) in
	 if not r then Format.printf "Error : variable '%s' from LTL not found in C code.\n" vl;
	 r
      ) (!variables_from_auto)
  in
  (included_funs && included_vars)



(* ************************************************************************* *)
(* Table giving the varinfo structure associated to a given variable name *)
(* In practice it contains all variables (from promela and globals from C file) and only variables *)
let varinfos = Hashtbl.create 97

(* Add a new variable into the association table name -> varinfo *)
let set_varinfo name vi =
  Hashtbl.add varinfos name vi


(* Given a variable name, it returns its associated varinfo.
    If the variable is not found then an error message is print and an assert false is raised. *)
let get_varinfo name =
  try
    Hashtbl.find varinfos name
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Variable not declared (%s). \n" name; assert false


(* Same as get_varinfo, but the result is an option.
   Hence, if the variable is not found then None is return. *)
let get_varinfo_option name =
  try
    Some(Hashtbl.find varinfos name)
  with
    | _ -> None






(* ************************************************************************* *)


(* Private data, for memorization of current specification of each function *)
let pre_status = Hashtbl.create 97
let post_status = Hashtbl.create 97
let post_status_bycase = Hashtbl.create 97



(* Returns the pre condition associated to the given C function *)
let get_func_pre func =
  try
    Hashtbl.find pre_status func
  with
    | _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Function pre-condition not found. \n"; assert false

(* Sets the pre condition of the given C function *)
let set_func_pre func status =
  Hashtbl.replace pre_status func status

(* Returns the post condition associated to the given C function *)
let get_func_post func =
  try
    Hashtbl.find post_status func
  with
    | _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Function prostcondition not found. \n"; assert false

(* Sets the pre condition of the given C function *)
let set_func_post func status =
  Hashtbl.replace post_status func status


(* Returns the post condition associated to the given C function *)
let get_func_post_bycase func =
  try
    Hashtbl.find post_status_bycase  func
  with
    | _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Function prostcondition not found. \n"; assert false

(* Sets the pre condition of the given C function *)
let set_func_post_bycase  func status =
  Hashtbl.replace post_status_bycase func status





(* Private data, for memorization of current specification of each loop *)
let loop_ext_pre  = Hashtbl.create 97
let loop_int_pre  = Hashtbl.create 97
let loop_ext_post = Hashtbl.create 97
let loop_int_post = Hashtbl.create 97


(* Returns the pre condition associated to the given C function *)
let get_loop_ext_pre stmt_ref =
  try Hashtbl.find loop_ext_pre stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop external precondition not found. \n"; assert false
let get_loop_int_pre stmt_ref =
  try Hashtbl.find loop_int_pre stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop internal precondition not found. \n"; assert false

(* Sets the external or the block pre condition of the given loop *)
let set_loop_ext_pre stmt_ref pre =
  Hashtbl.replace loop_ext_pre stmt_ref pre
let set_loop_int_pre stmt_ref pre =
  Hashtbl.replace loop_int_pre stmt_ref pre


(* Returns the post condition associated to the given C function *)
let get_loop_ext_post stmt_ref =
  try Hashtbl.find loop_ext_post stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop external postcondition not found. \n"; assert false
let get_loop_int_post stmt_ref =
  try Hashtbl.find loop_int_post stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop internal postcondition not found. \n"; assert false

(* Sets the external or the block post condition of the given loop *)
let set_loop_ext_post stmt_ref post =
  Hashtbl.replace loop_ext_post stmt_ref post
let set_loop_int_post stmt_ref post =
  Hashtbl.replace loop_int_post stmt_ref post








(* Private data, for memorization of current specification of each loop *)
let loop_ext_pre_bycase  = Hashtbl.create 97
let loop_int_pre_bycase  = Hashtbl.create 97
let loop_ext_post_bycase = Hashtbl.create 97
let loop_int_post_bycase = Hashtbl.create 97


(* Returns the pre condition associated to the given C function *)
let get_loop_ext_pre_bycase stmt_ref =
  try Hashtbl.find loop_ext_pre_bycase stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop external precondition not found. \n"; assert false
let get_loop_int_pre_bycase stmt_ref =
  try Hashtbl.find loop_int_pre_bycase stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop internal precondition not found. \n"; assert false

(* Sets the external or the block pre condition of the given loop *)
let set_loop_ext_pre_bycase stmt_ref pre =
  Hashtbl.replace loop_ext_pre_bycase stmt_ref pre
let set_loop_int_pre_bycase stmt_ref pre =
  Hashtbl.replace loop_int_pre_bycase stmt_ref pre


(* Returns the post condition associated to the given C function *)
let get_loop_ext_post_bycase stmt_ref =
  try Hashtbl.find loop_ext_post_bycase stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop external postcondition not found. \n"; assert false
let get_loop_int_post_bycase stmt_ref =
  try Hashtbl.find loop_int_post_bycase stmt_ref
  with _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Loop internal postcondition not found. \n"; assert false

(* Sets the external or the block post condition of the given loop *)
let set_loop_ext_post_bycase stmt_ref post =
  Hashtbl.replace loop_ext_post_bycase stmt_ref post
let set_loop_int_post_bycase stmt_ref post =
  Hashtbl.replace loop_int_post_bycase stmt_ref post












(* Given the name of a function, it return the name of the associated element in the operation list. *)
let func_to_op_func f =
  "op_"^f







let used_enuminfo = Hashtbl.create 2

let set_usedinfo name einfo =
  Hashtbl.add used_enuminfo name einfo

let get_usedinfo name =
  try
    Hashtbl.find used_enuminfo name
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Incomplete enum information.\n";
	assert false




let get_cenum_option name =
  let opnamed = func_to_op_func name in
    Hashtbl.fold
      (fun _ ei value ->
	 match value with
	   | Some(_) as r -> r (* Already found *)
	   | None ->
	       let rec search = function
		 | {einame = n} as ei ::_ when n=name -> Some(CEnum ei)
		 | {einame = n} as ei ::_ when n=opnamed -> Some(CEnum ei)
		 | _::l -> search l
		 | [] -> None
	       in
	       search ei.eitems
      )
      used_enuminfo
      None




let func_to_cenum func =
  try
    let ei = Hashtbl.find used_enuminfo listOp in
    let name = func_to_op_func func in
    let rec search = function
      | {einame = n} as ei ::_ when n=name -> CEnum ei
      | _::l -> search l
      | [] ->
	  Format.printf "Ltl_to_acsl plugin internal error. Status : Operation '%s' not found in operations enumeration. \n" name; assert false
    in
      search ei.eitems
      (*    CEnum(ex,s,ei)*)
  with
    | _ -> Format.printf "Ltl_to_acsl plugin internal error. Status : Operation not found. \n"; assert false



let op_status_to_cenum status =
  try
    let ei = Hashtbl.find used_enuminfo listStatus in
    let name = if status = Promelaast.Call then callStatus else termStatus in
    let rec search = function
      | {einame=n} as ei ::_ when n=name -> CEnum ei
      | _::l -> search l
      | [] ->
	  Format.printf "Ltl_to_acsl plugin internal error. Status : Status not found. \n";
	  assert false
    in
      search ei.eitems
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Status not found. \n";
	assert false


let rec is_in_vi_list name = function
  | [] -> false
  | vi :: _ when name=vi.vname-> true
  | _ :: l -> is_in_vi_list name l


let get_fresh_name vi_list name =
  if not (is_in_vi_list name vi_list) then name
  else
    let n = ref 0 in
    while is_in_vi_list (name^"_"^(string_of_int !n)) vi_list do
      n:=!n+1
    done;
    (name^"_"^(string_of_int !n))



let local_tmp_vars = Hashtbl.create 97
let make_local_tmp func_name =
  try
    Hashtbl.find local_tmp_vars func_name
  with
    | Not_found ->
	let typ = TInt (IInt,[]) in
	let name = "_buch_tmp" in
	let func = Kernel_function.get_definition (Globals.Functions.find_by_name func_name) in
	let fresh_name = get_fresh_name (func.sformals@func.slocals) name in
	let vi = Cil.makeLocalVar func fresh_name typ in
	Hashtbl.add local_tmp_vars func_name vi;
	vi


let get_local_tmp_name func_name =
  try
    (Hashtbl.find local_tmp_vars func_name).vname
  with
    | Not_found ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : This function seems to not have tmp var. \n";
	assert false



let local_iter_vars = Hashtbl.create 97
let make_local_iter func_name  =
  try
    Hashtbl.find local_iter_vars func_name
  with
    | Not_found ->
	let typ = TInt (IInt,[]) in
	let name = "_buch_iter" in
	let func = Kernel_function.get_definition (Globals.Functions.find_by_name func_name) in
	let fresh_name = get_fresh_name (func.sformals@func.slocals) name in
	let vi = Cil.makeLocalVar func fresh_name typ in
	Hashtbl.add local_iter_vars func_name vi;
	vi

let get_local_iter_name func_name =
  try
    (Hashtbl.find local_iter_vars func_name).vname
  with
    | Not_found ->
	Format.printf  "Ltl_to_acsl plugin internal error. Status : This function seems to not have iter var. \n";
	assert false









(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

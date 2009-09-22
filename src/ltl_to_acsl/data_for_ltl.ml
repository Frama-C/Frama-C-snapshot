(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: data_for_ltl.ml,v 1.6 2008-12-19 15:30:56 uid588 Exp $ *)

open Cil_types
open Spec_tools
open Promelaast

let raise_error msg = 
	Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. \nStatus : %s.\n" msg;;
(*  Format.printf "Ltl_to_acsl plugin internal error. \nStatus : %s.\n" msg; *)
(*  assert false                                                             *)



(* ************************************************************************* *)
let ltl_exps = ref (Hashtbl.create 1)

let setLtl_expressions exps =
(*  Ltl_logic.setLtl_expressions exps;*)
  ltl_exps:=exps

let ltl_expressions_iter func =
  Hashtbl.iter  func !ltl_exps

let get_exp_from_tmpident var =
  try let (exp,_,_) = (Hashtbl.find !ltl_exps var) in exp
  with _ -> raise_error ("TMP Variable ("^var^") not declared in hashtbl")

let get_str_exp_from_tmpident var =
  try let (_,str,_) = (Hashtbl.find !ltl_exps var) in "("^(str)^")"
  with _ -> raise_error ("TMP Variable ("^var^") not declared in hashtbl")

let get_pred_from_tmpident var =
  try let (_,_,pred) = (Hashtbl.find !ltl_exps var) in pred
  with _ -> raise_error ("TMP Variable ("^var^") not declared in hashtbl")

let debug_ltl_expressions () =
  Ltl_to_acsl_option.feedback "  Known ltl expressions: \n";
  Hashtbl.iter 
    (fun key (_,str,_) ->
       Ltl_to_acsl_option.feedback "  Var tmp : %s  ~~~> exp : %s\n" key str
    )
    !ltl_exps



let declared_logics = Hashtbl.create 97

let add_logic name log_info =
  begin
    if not (Hashtbl.mem declared_logics name) then
      Hashtbl.remove declared_logics name;
    Hashtbl.add declared_logics name log_info
  end

let get_logic name =
  try Hashtbl.find declared_logics name
  with _ -> raise_error ("Logic function '"^name^"' not declared in hashtbl")



let declared_predicates = Hashtbl.create 97

let add_predicate name pred_info =
  begin
    if not (Hashtbl.mem declared_predicates name) then
      Hashtbl.remove declared_predicates name;
    Hashtbl.add declared_predicates name pred_info
  end

let get_predicate name =
  try Hashtbl.find declared_predicates name
  with _ -> raise_error ("Predicate '"^name^"' not declared in hashtbl")



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
(* List of functions call observed in the C file without declaration *)
let ignored_functions = ref []



(** Return the buchi automata as stored after parsing *)
let getAutomata () =
  !automata


(** Return the number of transitions of the automata *)
let getNumberOfTransitions () =
  List.length (snd !automata)

(** Return the number of states of the automata *)
let getNumberOfStates () =
  List.length (fst !automata)


(** Stores the buchi automata and its variables and functions as such as it  is return by the parsing *)
let setAutomata auto vars funcs =
  variables_from_auto:=Hashtbl.fold (fun k _ l -> k::l) vars [];
  functions_from_auto:=Hashtbl.fold (fun k _ l -> k::l) funcs [];
  automata:=auto;
  setNumberOfStates  (getNumberOfStates ());
  setNumberOfTransitions (getNumberOfTransitions  ())


(** Initializes some tables according to data from Cil AST. *)
let setCData () =
  let (f_decl,f_def) =
    Globals.Functions.fold 
      (fun f (lf_decl,lf_def) -> 
	 let name = (Kernel_function.get_name f) in
	 match f.Db_types.fundec with 
	   | Db_types.Definition _ -> (lf_decl,name::lf_def)
	   | Db_types.Declaration _ -> (name::lf_decl,lf_def)) 
      ([],[])
  in
  functions_from_c:=f_def;
  ignored_functions:=f_decl;
  variables_from_c:=
    Globals.Vars.fold (fun v _ lv -> (Pretty_utils.sfprintf "%a" Ast_info.pretty_vname v)::lv) []

(** Return the list of all function name observed in the promela file. *)
let getFunctions_from_auto () =
  (!functions_from_auto)

(** Return the list of all variables name observed in the promela file. *)
let getVariables_from_auto () =
  (!variables_from_auto)

(** Return the list of all function name observed in the C file, except ignored functions. *)
let getFunctions_from_c () =
  (!functions_from_c)

(** Return the list of all variables name observed in the C file. *)
let getVariables_from_c () =
  (!variables_from_c)

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
let getIgnoredFunctions () =
  (!ignored_functions)

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
let addIgnoredFunction fname =
  ignored_functions:=fname::(!ignored_functions)

(** Return true if and only if the given string fname denotes an ignored function. *)
let isIgnoredFunction fname =
  List.exists
    (fun s -> (String.compare fname s)=0)
    (!ignored_functions)


(* Manage particular consistency verification between C file and automata specification.
    It returns true if and only if these checks are ok. *)
let check_consistency () =
  (* Checking consistency *)
  let included_funs =
    List.for_all
      (fun fl ->
	 let r=List.exists (fun fc -> fc=fl) !functions_from_c in
	 if not r then Ltl_to_acsl_option.error "Error : function '%s' from LTL not found in C code.\n" fl;
	 r
      )
      (!functions_from_auto)
  in
  let included_vars =
    List.for_all
      (fun vl ->
	 let r=(List.exists (fun vc -> vc=vl) ("result"::!variables_from_c)) in
	 if not r then Ltl_to_acsl_option.error "Error : variable '%s' from LTL not found in C code.\n" vl;
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
  with _ -> raise_error ("Variable not declared ("^name^")")


(* Same as get_varinfo, but the result is an option.
   Hence, if the variable is not found then None is return. *)
let get_varinfo_option name =
  try
    Some(Hashtbl.find varinfos name)
  with
    | _ -> None






(* ************************************************************************* *)
(**{b Pre and post condition of C functions} In our point of view, the pre or 
   the post condition of a C function are defined by the set of states 
   authorized just before/after the call, as such as the set of crossable 
   transitions. The following functions manages these stored informations. 
   Usually, the first array is for the authorized states, while the second one 
   is for the crossable conditions *)

(* Private data, for memorization of current specification of each function *)
let pre_status = Hashtbl.create 97 (* bool array * bool array *)
let post_status = Hashtbl.create 97 (* bool array * bool array *)
let post_status_bycase = Hashtbl.create 97 (* bool array array * bool array array *)


  

(** Returns the pre condition associated to the given C function *)
let get_func_pre ?(securised=false) func =
  try Hashtbl.find pre_status func
  with _ -> 
    if securised  then mk_full_pre_or_post() 
    else raise_error "Function pre-condition not found"

(** Sets the pre condition of the given C function *)
let set_func_pre func status =
  Hashtbl.replace pre_status func status


(** Returns the post condition associated to the given C function *)
let get_func_post ?(securised=false) func =
  try Hashtbl.find post_status func
  with _ -> 
    if securised  then mk_full_pre_or_post() 
    else raise_error ("(data_for_ltl.get_func_post). Status : Function '"^func^"' postcondition not found")

(** Sets the pre condition of the given C function *)
let set_func_post func status =
  Hashtbl.replace post_status func status



(** Returns the post condition associated to the given C function *)
let get_func_post_bycase ?(securised=false) func =
  try Hashtbl.find post_status_bycase  func
  with _ -> 
    if securised  then mk_full_pre_or_post_bycase() 
    else raise_error ("(data_for_ltl.get_func_post_bycase). Status : Function '"^func^"' postcondition not found")

(** Sets the pre condition of the given C function *)
let set_func_post_bycase  func status =
  Hashtbl.replace post_status_bycase func status





(* Private data, for memorization of current specification of each function *)
let pre_call_status = Hashtbl.create 97 (* (String cur_op * int StmtId) -> bool array * bool array *)
(*let pre_call_status_bc = Hashtbl.create 97 (* (String cur_op * int StmtId) -> bool array * bool array *)*)


(** Gives the specification of the call stmt in the given C function at the given StmtId. 
    if the key (caller,sid) is not in table, then a full spec is returned.
*)
let get_func_pre_call caller sid = 
  try
    Hashtbl.find pre_call_status (caller,sid)
  with
    | _ -> (Array.make (!numberOfStates) true,
	    Array.make (!numberOfTransitions) true)
	(*Format.printf "Ltl_to_acsl plugin internal error. Status : Function pre-condition not found. \n"; assert false*)

(** Sets the specification of the call stmt in the given C function at the given StmtId. *)
let set_func_pre_call caller sid status =
  Hashtbl.replace pre_call_status (caller,sid) status


(** Sets the specification of the call stmt in the given C function at the given StmtId. *)
let set_func_pre_call_bycase caller sid status =
  Hashtbl.replace pre_call_status (caller,sid) (Spec_tools.pre_flattening status)





(* ************************************************************************* *)
(**{b Pre and post condition of loops} In our point of view, the pre or 
   the post condition are defined by the set of states authorized just 
   before/after the loop (external pre/post), and by the set of states 
   authorized just before/after the execution of the internal block of the 
   loop (internal pre/post).
   The following functions manages these stored informations. 
   Usually, the first array is for the authorized states, while the second one 
   is for the crossable conditions. *)

(* Private data, for memorization of current specification of each loop *)
let loop_ext_pre  = Hashtbl.create 97
let loop_int_pre  = Hashtbl.create 97
let loop_ext_post = Hashtbl.create 97
let loop_int_post = Hashtbl.create 97


(* Returns the pre condition associated to the given C function *)
let get_loop_ext_pre stmt_ref = try Hashtbl.find loop_ext_pre stmt_ref with _ -> mk_full_pre_or_post()
let get_loop_int_pre stmt_ref = try Hashtbl.find loop_int_pre stmt_ref with _ -> mk_full_pre_or_post()

(* Sets the external or the block pre condition of the given loop *)
let set_loop_ext_pre stmt_ref pre = Hashtbl.replace loop_ext_pre stmt_ref pre
let set_loop_int_pre stmt_ref pre = Hashtbl.replace loop_int_pre stmt_ref pre


(* Returns the post condition associated to the given C function *)
let get_loop_ext_post stmt_ref = try Hashtbl.find loop_ext_post stmt_ref with _ -> mk_full_pre_or_post()
let get_loop_int_post stmt_ref = try Hashtbl.find loop_int_post stmt_ref with _ -> mk_full_pre_or_post()

(* Sets the external or the block post condition of the given loop *)
let set_loop_ext_post stmt_ref post = Hashtbl.replace loop_ext_post stmt_ref post
let set_loop_int_post stmt_ref post = Hashtbl.replace loop_int_post stmt_ref post








(* Private data, for memorization of current specification of each loop *)
let loop_ext_pre_bycase  = Hashtbl.create 97
let loop_int_pre_bycase  = Hashtbl.create 97
let loop_ext_post_bycase = Hashtbl.create 97
let loop_int_post_bycase = Hashtbl.create 97


(* Returns the pre condition associated to the given C function *)
let get_loop_ext_pre_bycase stmt_ref = try Hashtbl.find loop_ext_pre_bycase stmt_ref with _ -> mk_full_pre_or_post_bycase()
let get_loop_int_pre_bycase stmt_ref = try Hashtbl.find loop_int_pre_bycase stmt_ref with _ -> mk_full_pre_or_post_bycase()

(* Sets the external or the block pre condition of the given loop *)
let set_loop_ext_pre_bycase stmt_ref pre = Hashtbl.replace loop_ext_pre_bycase stmt_ref pre
let set_loop_int_pre_bycase stmt_ref pre = Hashtbl.replace loop_int_pre_bycase stmt_ref pre

(* Returns the post condition associated to the given C function *)
let get_loop_ext_post_bycase stmt_r = try Hashtbl.find loop_ext_post_bycase stmt_r with _ -> mk_full_pre_or_post_bycase()
let get_loop_int_post_bycase stmt_r = try Hashtbl.find loop_int_post_bycase stmt_r with _ -> mk_full_pre_or_post_bycase()

(* Sets the external or the block post condition of the given loop *)
let set_loop_ext_post_bycase stmt_ref post = Hashtbl.replace loop_ext_post_bycase stmt_ref post
let set_loop_int_post_bycase stmt_ref post = Hashtbl.replace loop_int_post_bycase stmt_ref post


(** Returns a stmt_ref list. It is the set of all registered loop in loop_specs hashtables *)
let get_loops_index () = 
  Hashtbl.fold (fun key _ lk-> key::lk ) loop_int_pre_bycase []
    
  




(* ************************************************************************* *)


let removeUnusedTransitionsAndStates () = 
(* Step 1 : computation of reached states and crossed transitions *)
  let crossedTransitions = ref (Array.make(getNumberOfTransitions()) false) in
  let reachedStates = ref (Array.make(getNumberOfStates()) false) in
  let addHash htbl = 
    Hashtbl.iter 
      (fun _ (_,tr) -> 
	 crossedTransitions:= bool_array_or !crossedTransitions tr;
      )
      htbl
  in

  let addHash_bycase htbl_bc = 
    let htbl = Hashtbl.create (Hashtbl.length htbl_bc) in
    Hashtbl.iter 
      (fun key dbaa -> 
	 Hashtbl.add htbl key (Spec_tools.pre_flattening dbaa)
      )
      htbl_bc;
    addHash htbl
  in

  addHash pre_status;
(*   addHash post_status; *)
  addHash_bycase post_status_bycase;

  addHash_bycase loop_ext_pre_bycase;
  addHash_bycase loop_int_pre_bycase;
(*   addHash_bycase loop_ext_post_bycase;  Always empty*) 
 addHash_bycase loop_int_post_bycase;

  List.iter
    (fun tr -> 
       if !crossedTransitions.(tr.numt) then
	 begin
	   !reachedStates.(tr.start.nums)<-true;
	   !reachedStates.(tr.stop.nums)<-true
	 end
    )
    (snd !automata);

(* Verification pass because of a plugin limitation *)
(*  List.iter
    (fun tr ->  
       if not !reachedStates.(tr.start.nums) && !crossedTransitions.(tr.numt) then 
	 begin 
	   Format.printf "Ltl_to_acsl plugin internal error. Status : States/Transitions simplification try to remove an unreachable state wich is the starting point for a crossable transition. \n";
	   assert false
	 end 
    )
    (snd !automata);
*)


(* DEBUG : Displaying information *)
(*  Format.printf "\n\n Used states and transitions:\n";
  debug_display_stmt_all_pre (!reachedStates,!crossedTransitions) ;
  Format.printf "\n\n" ;*)

(* Step 2 : computation of translation tables *)
  let newNbTrans = Array.fold_left (fun nb tr -> if tr then nb+1 else nb ) 0 !crossedTransitions in
  let newNbStates = Array.fold_left (fun nb st -> if st then nb+1 else nb ) 0 !reachedStates in

  let replaceTransitions = ref (Array.make(getNumberOfTransitions()) 0) in  
  let replaceStates = ref (Array.make(getNumberOfStates()) 0) in
  let nextTr = ref 0 in
  let nextSt = ref 0 in
  Array.iteri 
    (fun i _ -> if !crossedTransitions.(i) then 
       begin
	 !replaceTransitions.(i) <- !nextTr;
	 nextTr:=!nextTr+1
       end
     else
       !replaceTransitions.(i) <- (-1)
    )
    !replaceTransitions;
  Array.iteri 
    (fun i _ -> if !reachedStates.(i) then 
       begin
	 !replaceStates.(i) <- !nextSt;
	 nextSt:=!nextSt+1
       end
     else
       !replaceStates.(i) <- (-1)
    )
    !replaceStates ;


(* DEBUG : Displaying information *)
(*   Format.printf "\n\n New nb trans :%d \n New nb states :%d  \n" newNbTrans newNbStates; *)
(*   Format.printf "\n\n Transitions replacement:\n"; *)
(*   Array.iteri  *)
(*     (fun i v -> *)
(*        Format.printf "   tr%s ~> tr%s\n" (string_of_int i) (string_of_int v)  *)
(*     )  *)
(*     !replaceTransitions; *)
(*   Format.printf "\n\n States replacement:\n";  *)
(*   Array.iteri *)
(*     (fun i v ->  *)
(*        Format.printf "   st%s ~> st%s\n" (string_of_int i) (string_of_int v) *)
(*     )  *)
(*     !replaceStates;  *)
(*   Format.printf "\n\n"; *)
    

(* Step 3 : rewritting stored information *)
  (* Rewritting automata *)
  let sts = List.rev (List.fold_left 
			(fun newl st -> 
			   let newn= !replaceStates.(st.nums) in 
			   if newn= -1 then newl 
			   else begin st.nums<-newn;st::newl end
			) [] (fst !automata)) 
  in
  let trs = List.rev (List.fold_left 
			(fun newl tr -> 
			   let newn= !replaceTransitions.(tr.numt) in 
			   if newn= -1 then newl 
			   else begin tr.numt<-newn;tr::newl end
			)
			[] (snd !automata)) in
  automata:=(sts,trs);
  
  (* Rewritting size of automata cached in Spec_tools *)
  setNumberOfStates newNbStates;
  setNumberOfTransitions newNbTrans;
 

  (* Rewritting pre/post conditions and loops specification *)
  let rewriteHashtblSpec htbl =
    Hashtbl.iter 
      (fun key (ost,otr) -> 
	 let nst,ntr=mk_empty_pre_or_post () in
	 Array.iteri (fun i b -> if b then nst.(!replaceStates.(i))<-true) ost;
	 Array.iteri (fun i b -> if b then ntr.(!replaceTransitions.(i))<-true) otr;
	 Hashtbl.replace htbl key (nst,ntr)
      )
      htbl
  in
  let rewriteHashtblSpec_bycase htbl =
    Hashtbl.iter 
      (fun key (ost,otr) -> 
	 let nst,ntr=mk_empty_pre_or_post_bycase () in
	 Array.iteri 
	   (fun iniSt sp ->
	      let newIniSt = !replaceStates.(iniSt) in
	      if newIniSt <> -1 then 
		Array.iteri 
		  (fun i b -> if b then nst.(newIniSt).(!replaceStates.(i))<-true) 
		  sp
	   )
	   ost;
	 Array.iteri 
	   (fun iniSt sp ->
	      let newIniSt = !replaceStates.(iniSt) in
	      if newIniSt <> -1 then 
		Array.iteri 
		  (fun i b -> if b then ntr.(newIniSt).(!replaceTransitions.(i))<-true) 
		  sp
	   )
	   otr;
	 Hashtbl.replace htbl key (nst,ntr)
      )
      htbl
  in

  rewriteHashtblSpec pre_status;
(*   rewriteHashtblSpec post_status; *)
  rewriteHashtblSpec_bycase post_status_bycase;

(*   rewriteHashtblSpec loop_ext_pre; *)
(*   rewriteHashtblSpec loop_int_pre; *)
(*   rewriteHashtblSpec loop_ext_post; *)
(*   rewriteHashtblSpec loop_int_post; *)

  rewriteHashtblSpec_bycase loop_ext_pre_bycase;
  rewriteHashtblSpec_bycase loop_int_pre_bycase;
(*   rewriteHashtblSpec_bycase loop_ext_post_bycase; *)
  rewriteHashtblSpec_bycase loop_int_post_bycase






(* ************************************************************************* *)


(* Given the name of a function, it return the name of the associated element in the operation list. *)
let func_to_op_func f =
  "op_"^f







let used_enuminfo = Hashtbl.create 2 

let set_usedinfo name einfo = 
  Hashtbl.add used_enuminfo name einfo

let get_usedinfo name =
  try Hashtbl.find used_enuminfo name
  with _ -> raise_error ("Incomplete enum information.")




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
      | [] -> raise_error ("Operation '"^name^"' not found in operations enumeration")
    in
      search ei.eitems 
      (*    CEnum(ex,s,ei)*)
  with _ -> raise_error ("Operation not found")



let op_status_to_cenum status =
  try
    let ei = Hashtbl.find used_enuminfo listStatus in
    let name = if status = Promelaast.Call then callStatus else termStatus in
    let rec search = function
      | {einame=n} as ei ::_ when n=name -> CEnum ei
      | _::l -> search l
      | [] -> raise_error ("Status not found")
    in
      search ei.eitems
  with _ -> raise_error ("Status not found")


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
  try (Hashtbl.find local_tmp_vars func_name).vname
  with Not_found -> raise_error "This function seems to not have tmp var"



let local_iter_vars = Hashtbl.create 97
let make_local_iter func_name  =
  try Hashtbl.find local_iter_vars func_name
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
  try (Hashtbl.find local_iter_vars func_name).vname
  with Not_found -> raise_error "This function seems to not have iter var"









(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

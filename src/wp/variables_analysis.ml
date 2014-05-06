(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

(*
   This analysis performs a classification of the variables of the input
   program. The aim of this classification is to optimize the translation
   of variables by WP: 
    1) optimization of the by-reference call and 
    2) functional variables. 
*)

open Cil_types
open Cil 

let dkey = Wp_parameters.register_category "var_analysis" (* debugging key*)

let debug = Wp_parameters.debug ~dkey 

let dkey = Wp_parameters.register_category "var_kind"

let oracle = Wp_parameters.debug ~dkey
	  
(* -------------------------------------------------------------------------- *)
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)


(*
   At the end, the analysis associates an [var_kind] information to each 
   variables: 
     1) [Fvar] functional variable, variable such as its address is never
        taken,
 
     2) [PRarg] by_pointer_reference argument, variable such as its 
        address is only taken in by reference calls (one or more),

     3) [ARarg] by_array_reference argument, variable such as its 
        address is only taken in by array reference calls (one or more), 

     4) [PRpar n] by_pointer_reference parameter of arity , 
        variable which is a formal parameter use for a by reference call
        and can be invoked in a chain of by reference call such as their
        arity are less or equal than n, 

     5) [ARpar n] by_array_reference parameter of arity , 
        variable which is a formal parameter use for a by array reference
        call and can be invoked in a chain of by array reference call
        such as their arity are less or equal than n, 
     
     6) [Cvar] other variable.
 
*)

type var_kind = 
    Fvar | Cvar | PRarg | ARarg | PRpar of int | ARpar of int 


(**********************************************************************)
(*** I - By reference call optimisation.                           ****)
(**********************************************************************)

(*
  A by pointer reference call is characterized by 2 facts : 
   1p) the formal parameters [p] is of pointer type [*..<n>..*t], 
     p always occurs with <n>*:[<n>* p] except in call. 
     As call argument, 
     p can occurs with less than <n>*: [<k>*p], k<=n in case of
     by pointer reference call (ie. if p has the characteristic of a 
     by pointer reference argument.) 

   2p) the by pointer reference argument [x] is a variable which 
      is not a formal parameter and which appears as argument
      to the match place of a by pointer reference parameter, in one or 
      more of those patterns [by_pref_pattern]: 
        - &x+offset
        - x+i with x of pointer type and + as +PI.
        - *<n>x, n <= stars(typ(x)) 

 A by array reference call is characterized by 2 facts : 
   1a) the formal parameters [p] is of pointer type [*..<n>..*t], 
     p always occurs with <n> indexes:[p<n>[i]] except in call. 
     As call argument, 
     p can occurs with less than <n> indexes: [p<k>[i]], k<=n in case of
     by array reference call (ie. if p has the characteristic of a 
     by array reference argument.) 

   2a) the by array reference argument [x] is a variable which 
      is not a formal parameter and which appears as argument
      to the match place of a by pointer reference parameter, in one or 
      more of those patterns [by_array_reference_pattern]: 
        - x+offset (StarOf)
        - x<k>[], k <= bracket(typ(x)) .
        - &(x+i) + ==+PI 
*)



(**********************************************************************)
(*** Helper section with some smart constructors for                ***)
(*** patterns identifications                                       ***)
(**********************************************************************)

(* [stars_typ typ] accounts the number of * if typ is a pointer type.*)
let rec stars_typ typ = 
  match Cil.unrollType typ with 
    | TPtr (typ,_) -> 1+ stars_typ (Cil.unrollType typ) 
    | TInt(_,_) | TFloat(_,_) | TFun _ | TEnum (_,_) | TComp (_,_,_)
    | TArray (_,_,_,_) | TBuiltin_va_list _ | TVoid _ | TNamed _ -> 0

(* [bracket_typ typ] accounts the number of [dim] if typ is an array 
   type. *)
let rec brackets_typ typ = 
  match Cil.unrollType typ with 
    | TArray (typ,_,_,_) -> 1+ brackets_typ (Cil.unrollType typ) 
    | TInt(_,_) | TFloat(_,_) | TFun _ | TEnum (_,_) | TComp (_,_,_)
    | TPtr (_,_) | TBuiltin_va_list _ | TVoid _ | TNamed _ -> 0


(* [bracket_and_stars_typ typ] accounts the number of [dim] and the number of 
   pointer if typ is a pointer on array  type. *)
let brackets_and_stars_typ typ =
  let rec stars_and_elt typ = 
     match Cil.unrollType typ with 
    | TPtr (typ,_) -> 
	let (n,t) = stars_and_elt (Cil.unrollType typ) in
	(n+1),t
    | TInt(_,_) | TFloat(_,_) | TFun _ | TEnum (_,_) | TComp (_,_,_)
    | TArray (_,_,_,_) 
    | TBuiltin_va_list _ | TVoid _ | TNamed _ as t-> (0,t)
  in
  let (n,t) = stars_and_elt typ in n+brackets_typ t

(* [stars_lv_typ] same as stars_typ on logic_type*)
let stars_lv_typ = function | Ctype t -> stars_typ t | _ -> 0

(* [brackets_lv_typ] same as brackets_typ on logic_type*)
let brackets_lv_typ = function | Ctype t -> brackets_typ t | _ -> 0

let brackets_and_stars_lv_typ = 
  function | Ctype t -> brackets_and_stars_typ t | _ -> 0



(* [stars_exp e] returns Some (x,ty,n) if e == <n>* x and ty is the type of the 
   entire inner lval else returns none. *)
let rec stars_exp = function
  | Lval (Var x,off ) -> Some(x,Cil.typeOfLval (Var x,off),0)
  | Lval (Mem e, _) -> 
      (match stars_exp (Cil.stripInfo e).enode with 
	 | None -> None 
	 | Some (x,ty,n) -> Some (x, ty ,n+1))
  | _ -> None
     

(* [stars_term t] returns Some (x,ty,n) if t == <n>* x and ty is the 
   type of the entire inner lval else returns none. *)
let rec stars_term = function
  | TLval (TVar lvar,off ) 
  | Tat ({term_node =
	     TLval (TVar lvar,off )}, _ )  -> 
      Some(lvar,(Cil.typeOfTermLval(TVar lvar,off ) ),0)
  | TLval (TMem t,_ ) 
  | Tat  ({term_node =
	      TLval (TMem t,_)}, _ ) -> 
      (match stars_term t.term_node with 
	 | None -> None 
	 | Some (x,ty,n) -> Some (x,ty,n+1))
   | _ -> None
       
(* [brackets_off off] returns Some n if off == <n>[] else returns none. *)
let rec brackets_off = function 
   | Index (_,off) -> 
       (match brackets_off off with 
	  | Some n ->Some (1+n)
	  | None -> None )
   | NoOffset -> Some 0
   | _ -> None 

(* [brackets_toff off] returns Some n if off == <n>[] else returns none. *)
let rec brackets_toff = function 
  | TIndex(_,toff) -> 
      (match brackets_toff toff with 
	  | Some n ->Some (1+n)
	  | None -> None )
  | TNoOffset -> Some 0
  | _ -> None

(* [bracket_exp e] returns Some(x,n) if e == x<n>[] else returns none*)
let bracket_exp = function
  | Lval (Var x,off) -> 
      (match brackets_off off with 
	 | Some n -> Some(x,n)
	 | None -> None)
  | _ -> None

(* [bracket_term t] returns Some(x,n) if t == x<n>[] else returns none*)
let bracket_term = function 
  |  TLval (TVar x,off) 
  | Tat ({term_node =
	     TLval (TVar x,off)}, _ ) ->  
      (match brackets_toff off with 
	 | Some n -> Some(x,n)
	 | None -> None)
  | _ -> None
      
(* [delta_ptr e] returns Some x if e == x+i and x has pointer type 
                 returns None *)
let delta_ptr  = function 
  | BinOp ((PlusPI|MinusPI),
	   {enode = Lval (Var x,off)},_ , _ )  -> 
      Some (x, stars_typ (Cil.typeOfLval (Var x,off))) 
  | _ -> None
      
(* variante of delta_ptr on term; takes care of labelled term *)
let delta_ptr_term = function
  | TBinOp((PlusPI|MinusPI),{term_node = TLval (TVar lvar,off)},_) 
  | Tat
      ({term_node = 
	   TBinOp((PlusPI|MinusPI),{term_node = TLval (TVar lvar,off)},_)
       },_)
  | TLval (TMem 
	     {term_node = 
		 Tat
		   ({term_node = 
			TBinOp((PlusPI|MinusPI),
			       {term_node = TLval (TVar lvar,off)},_)
		    },_)},_)
  | TLval (TMem 
	     {term_node = 
		 TBinOp((PlusPI|MinusPI),
               {term_node = Tat({term_node = TLval (TVar lvar,off)},_)},_)},_)
  |  TLval (TMem
	      {term_node =
		  TBinOp((PlusPI|MinusPI),
			 {term_node = TLval (TVar lvar,off)},_)},_)
  | TBinOp((PlusPI|MinusPI),
	   {term_node = 
	       TLval (TMem 
			{term_node =
			    Tat({term_node = TLval (TVar lvar,off)},_)},_)},_) 
    
    -> Some (lvar, stars_lv_typ (Cil.typeOfTermLval (TVar lvar,off)))
  | _ -> None
	 
(* [delta_array e] returns Some x if e == x[i] and x has pointer type 
                 else returns None *)
let delta_array = function
  | BinOp (IndexPI,{enode = Lval (Var x,off)}, _ ,_) 
    -> Some (x, stars_typ (Cil.typeOfLval (Var x,off)))  
  | e -> debug "[delta_array] calls delta_ptr"; delta_ptr e

let delta_array_term = function 
  | TBinOp(IndexPI,{term_node = TLval (TVar lvar,off)},_) 
  | Tat
      ({term_node = TBinOp(IndexPI,{term_node = TLval (TVar lvar,off)},_) 
       },_)
  | TLval (TMem 
	     {term_node =
		 TBinOp(IndexPI,{term_node = TLval (TVar lvar,off)},_)}
	     , _)
   | TLval (TMem 
	     {term_node = Tat
      ({term_node = TBinOp(IndexPI,{term_node = TLval (TVar lvar,off)},_) 
       },_)},_)   
   | TBinOp(IndexPI,
	    {term_node = 
		Tat({term_node = TLval (TVar lvar,off)},_)},_)
     -> Some (lvar, stars_lv_typ (Cil.typeOfTermLval (TVar lvar,off)))
   | t -> 
       debug "[delta_array_term] calls delta_ptr_term"; delta_ptr_term t

(**********************************************************************)
(*** A - Identification of by reference formal parameters usage and ***)
(*** Identification of reference argument by usage.                 ***)
(*** and accounting of addresss taken of each variable              ***)
(*** We also protect the translation of pure logic variables bound  ***)
(*** by pforall and pexists.                                        ***)
(**********************************************************************)
(* Table of logic parameters, parameters of logic functions and predicate.
   The associated boolean is used to tagged the user parameters as 
   an argument of a ACSL builtin predicates or functions which 
   parameters are by reference: \valid and family, \block_length, 
   \separated, \initialized*)
module LogicParam =
  State_builder.Hashtbl
    (Cil_datatype.Logic_var.Hashtbl)
    (Datatype.Bool)
     (struct let name = "WP : logic parameters"
	    let dependencies = [Ast.self]
	    let size = 40 
     end)

let logic_param_memory_info x = 
  debug "[LogicParam] %a" Printer.pp_logic_var x;
 if LogicParam.mem x then 
  (debug "[LogicParam] %a in " Printer.pp_logic_var x;
    LogicParam.replace x true)
 else 
   (debug "[LogicParam] %a out"Printer.pp_logic_var x;())

(* Type of ACSL Variable, C-variable or Logic Variable or Formal parameters 
   of builtin predicates/functions.*)
type var_type = 
  | Cv of varinfo   (*C formal parameter*) 
  | Lv of logic_var (*Logic formal parameter*)
  | Prop (*Parameter of valid or separated or initialized,
	   ie builtin predicate*)

(* tests if a logic variable is a formal parameter.*)
let is_lformal = function 
  | {lv_origin = Some x} -> x.vformal
  | lv -> LogicParam.mem lv

(* according to a logicvar returns the more specified var_type*)
let var_type_of_lvar = function 
  |{lv_origin = Some x} -> Cv x 
  | l -> Lv l 

let pp_var_type fmt = function 
  | Cv x -> Printer.pp_varinfo fmt x
  | Lv p -> Printer.pp_logic_var fmt p 
  | Prop -> Format.pp_print_string fmt "Prop"

let _brackets_var_type_typ = function 
  | Cv x -> brackets_typ x.vtype
  | Lv lv ->  brackets_lv_typ lv.lv_type 
  | Prop -> 0

let brackets_and_stars_var_type_typ =  function 
  | Cv x -> brackets_and_stars_typ x.vtype
  | Lv lv ->  brackets_and_stars_lv_typ lv.lv_type 
  | Prop -> 0

let stars_var_type_typ = function 
  | Cv x -> stars_typ x.vtype
  | Lv lv ->  stars_lv_typ lv.lv_type 
  | Prop -> 0

let isVarTypePointerType =function 
  | Cv x -> Cil.isPointerType x.vtype
  | Lv lv -> Logic_utils.isLogicPointerType lv.lv_type 
  | Prop -> false 

let is_formal_var_type = function 
  | Cv x -> x.vformal
  | Lv lv -> LogicParam.mem lv
  | Prop -> false 
      


module VarType = 
   (Datatype.Make_with_collections
	  (struct 
	     include Datatype.Serializable_undefined
	     let name = "WpVarType"
	     type t = var_type
	     let reprs = 
	       let cp_repr = List.hd Cil_datatype.Varinfo.reprs in 
	       let lp_repr = List.hd Cil_datatype.Logic_var.reprs in 
	       [Cv cp_repr ; Lv lp_repr ; Prop]
	     let equal a b = 
	       match a,b with 
		 | Cv a, Cv b -> Cil_datatype.Varinfo.equal a b 
		 | Lv a, Lv b -> Cil_datatype.Logic_var.equal a b 
		 | Prop, Prop -> true 
		 | _ , _ -> false

	     let compare a b = 
	       match a,b with 
		 | Cv a, Cv b -> Cil_datatype.Varinfo.compare a b 
		 | Cv _ , _ -> (-1)
		 | _ , Cv _ -> (1)
		 | Lv a, Lv b -> Cil_datatype.Logic_var.compare a b
		 | Prop , Prop -> 0
		 | Lv _ , _ -> (-1) 
		 | _ , Lv _ -> (1) 

	       let hash = function 
		 | Cv v -> (Cil_datatype.Varinfo.hash v)*121
		 | Lv p -> (Cil_datatype.Logic_var.hash p)*147
		 | Prop -> 147
	   end))

(*Table of other kind of variables *)
module AnyVar = 
  State_builder.Hashtbl
    (VarType.Hashtbl)
    (Datatype.Unit)
    (struct let name = "WP: argument multi pattern"
            let dependencies = [Ast.self]
            let size = 47
     end)

(* only used to records universally and existentially bound variables as
   value variables. (ie. do not have to be optimized) *)
let add_logics_value l = 
  List.iter (fun lv -> AnyVar.replace (Lv lv) ()) l




(* Table of variables which addresses are taken. 
   Each variable [x] is associated to a pair of integer (plus,minus) such as
   [plus] is the total occurences of address taken of [x] and 
   [minus] is the number of occurences of address taken of [x] in
           a by reference pattern. *)
module AddrTaken =
  State_builder.Hashtbl
   (VarType.Hashtbl)
    (Datatype.Pair (Datatype.Int) (Datatype.Int))
    (struct let name = "WP: addr_taken"
            let dependencies = [Ast.self]
            let size = 47
     end)


let string_addr b = if b then "address taken" else "not address taken"

(*[incr_addr_taken var] adds [1] to the total occurences of address taken 
  of [var]*)
let incr_addr_taken var = 
  debug "[incr_addr] %a" pp_var_type var;
  oracle "[incr_addr] %a" pp_var_type var; 
  let (n,r) = try AddrTaken.find var with Not_found -> (0,0) in 
  AddrTaken.replace var (n+1,r)

(*[decr_addr_taken var] adds [1] to the numbre of occurences of 
  address taken of [var] in by reference pattern*)
let decr_addr_taken var = 
  debug "[decr_addr] %a" pp_var_type var;
  oracle "[decr_addr] %a" pp_var_type var;
  let (n,r) = try AddrTaken.find var with Not_found -> (0,0) in 
  AddrTaken.replace var (n,r+1) 

(* variant occurs only if [b] is true else do nothing *)
let decr_addr_taken_bool var b = if b then decr_addr_taken var



(* Visitor which : 
   -  collects the totale occurences of address taken ; 
   -  collects all logic parameters ; 
   -  collects all existentially and universally bound variables, as 
      variables which have not to be optimized. *)
class logic_parameters_and_addr_taken_collection : 
  Visitor.frama_c_visitor = object
    inherit Visitor.frama_c_inplace
      
    method! vexpr e =
      match (Cil.stripInfo e).enode with
	|  StartOf (Var vinfo,_)
	| AddrOf (Var vinfo,_) -> incr_addr_taken (Cv vinfo); DoChildren
	| _ -> DoChildren
	    
    method! vterm t = 
      match t.term_node with
	| TAddrOf(TVar lv,_)
	| TStartOf(TVar lv,_) ->
	    incr_addr_taken (var_type_of_lvar lv); DoChildren
	| _ -> DoChildren
	    
    method! vpredicate = function
      | Pforall (xl,_) | Pexists (xl,_) ->
	  add_logics_value xl ; DoChildren
      | _ -> DoChildren


    method! vannotation = function
      | Dfun_or_pred (linfo,_) ->
	  List.iter (fun lv -> 
		       oracle "[logicParam] %a" Printer.pp_logic_var lv;
		       LogicParam.replace lv false) linfo.l_profile;
	  DoChildren
      | _ ->DoChildren

  end

let compute_logic_params () = 
  debug "[LP+AT] logic parameters and address taken computation"; 
  if not (LogicParam.is_computed()) || not (AddrTaken.is_computed()) then
      ( Visitor.visitFramacFile 
	  (new logic_parameters_and_addr_taken_collection)(Ast.get());
	LogicParam.mark_as_computed();AddrTaken.mark_as_computed())



(**********************************************************************)
(*** Parameters Tables                                              ***)
(**********************************************************************)

(* A [call] represents the binding at call time of an effective argument 
   to a formal parameter. A [call] is then a triplet : 
     - an arity using in the effective argument;
     - a test of address taken in the effective argument; 
     - a vartype represented the the formal parameter. 
*)
(* A [ChainCalls] is a list of [call]s. 
   For a vartype [x], a [ChainCalls] the list of all call binding 
   when [x] is (the root of) the effective argument.*)
module ChainCalls = 
 (Datatype.List  (Datatype.Pair (Datatype.Int) 
		    (Datatype.Pair (Datatype.Bool)(VarType)))) 
     

let pp_call fmt (n,(b,p)) =
  Format.fprintf fmt "%a of arity:%d with %s " pp_var_type p n (string_addr b)

let pp_chaincall l = (Pretty_utils.pp_list ~sep:";@, " pp_call) l  

(* Table of the parameters of by pointer reference passing call *)
module ByPReference = 
  State_builder.Hashtbl 
    (VarType.Hashtbl)
    (Datatype.Pair (Datatype.Int) (ChainCalls))
    (struct let name = "WP: by pointer reference parameters"
            let dependencies = [Ast.self]
            let size = 47
     end)

(* Table of the parameters of by array reference passing call *)
module ByAReference = 
  State_builder.Hashtbl 
    (VarType.Hashtbl)
     (Datatype.Pair (Datatype.Int) (ChainCalls))
    (struct let name = "WP: by array reference parameters"
            let dependencies = [Ast.self]
            let size = 47
     end)

(* Table of the parameter of by value passing call *)
module ByValue = 
  State_builder.Hashtbl 
    (VarType.Hashtbl)
    (Datatype.Unit)
    (struct let name = "WP: by value parameters"
            let dependencies = [Ast.self]
            let size = 47
     end)

let is_pure_logic = function 
  | Lv lv -> (LogicParam.mem lv) && (lv.lv_origin = None) 
  | _ -> false

(*[add_ptr_reference_param x n] tries to add the paramtype [x] with 
  an arity of [n] in the table of by pointer reference parameters.
   - If [x] is already in the table 2 case : 
         * [n] does not the recorded arity the [x] is removed from 
            this table and added to the by value table.
         * Else nothing has to be done
   - If [x] is not in the table : 
         * [x] is in Byvalue table, nothing has to be done
         * [x] is in the table of by array reference parameter, [x]
            is removed from this table and puts in ByValue table.
         * [x] is not already registered in any tables, then 
           [x] is registered in the by pointer reference parameter
            with arity [n] and an empty chaincalls.
*)
let add_ptr_reference_param x n = 
  oracle "[ByPRef] first step + (%a,%d)" pp_var_type x n;
  if n = 0 && (is_pure_logic x) then 
    (ByPReference.remove x ; ByValue.replace x ())
   else (
     try 
       if not (fst(ByPReference.find x) = n) then 
	 (oracle "[ByPRef] remove %a: ko arity -> + ByValue" pp_var_type x;
	  ByPReference.remove x ; ByValue.replace x ())
       else (oracle "[ByPRef] (%a,%d) already" pp_var_type x n;()) 
     with Not_found ->
       oracle "[ByPRef] %a not yet"pp_var_type x;
       if ByValue.mem x then 
	 (oracle "[ByPRef] not add %a : byValue" pp_var_type x; ()) 
       else 
	 (if ByAReference.mem x then 
	    (oracle "[ByPRef] %a in byARef : remove -> add in ByValue" 
	       pp_var_type x;
	     ByAReference.remove x;ByValue.replace x())
	  else 
	    (oracle "[ByPRef] add (%a,%d)" pp_var_type x n;
	     ByPReference.replace x (n,[]))) )


(*[remove_ptr_reference_param x] tries to removed [x] from the 
  table of by pointer reference parameters. 
*)
let remove_ptr_reference_param x =
  oracle "[ByPRef] remove %a" pp_var_type x;
  if ByPReference.mem x then
    ( oracle "[ByPRef] remove %a of ByPref" pp_var_type x;
      ByPReference.remove x) ; 
  oracle "[ByPRef] add in ByValue %a"pp_var_type x;
  ByValue.replace x ()
    

(*[add_array_reference_param x n] tries to register [x] with arity [n] 
  in the table of by array reference parameters. 
  - If [x] already in this table : 
      *[n] is not the correct arity : [x] is removed from this table
       and add to the by value parameters table. 
      * Else nothing has to be done 
  - If [x] is not yet in this table : 
      *[x] is a by value parameter, nothing has to be done
      *[x] is a by pointer reference parameter then [x] is removed 
       from the table of by pointer reference parameter and adds to 
       the by array reference parameter. 
      *[x] is not in any table, [x] is registered in the by array 
       reference parameters with arity [n] and the empty chaincall. 

NB : As the behavior of a  by pointer reference parameter is included 
     in the behavior of a by array reference parameter; a vartype [x] 
     in ByPReference has to be "promoted" to the ByAReference table 
     in this function.

*)
let add_array_reference_param x n = 
  oracle "[ByARef] first step + (%a,%d)" pp_var_type x n;
  try 
    if not (fst (ByAReference.find x) = n) then 
      (oracle "[ByARef] remove %a: ko arity" pp_var_type x;
       ByAReference.remove x ; ByValue.replace x ())
    else (oracle "[ByARef] (%a,%d) already" pp_var_type x n;()) 
  with Not_found ->
    oracle "[ByARef] %a not yet"pp_var_type x;
    if ByValue.mem x then 
      (oracle "[ByARef] not add %a : byValue" pp_var_type x; ())
    else
      begin 
	try let (_,calls) = ByPReference.find x in 
	oracle "[ByARef] %a in byPRef : promote to byAref"
	  pp_var_type x;
	ByAReference.replace x (n,calls); ByPReference.remove x 
	with Not_found -> 
	  (oracle "[ByARef] add (%a,%d)" pp_var_type x n;
	   ByAReference.replace x (n,[]))
      end

let remove_array_reference_param x = 
  oracle "[ByARef] remove %a" pp_var_type x;
  if ByAReference.mem x then 
    (oracle "[ByARef] remove %a of ByAref" pp_var_type x;
     ByAReference.remove x) ; 
  oracle "[ByARef] add in ByValue %a"pp_var_type x;
  ByValue.replace x ()



(*************************************************************************)
(*** Usage of formal parameter as by reference parameter out of call   ***)
(*************************************************************************)	
type 'a usage = Ok of 'a  | Ko of 'a  | Any

(* Invariant : 
   by_pointerXXX must always been called before by_arrayXXX 
   then by_pointer can never returns KO*)

(* [by_pointer_reference_usage e] implemants 1p *)
let by_pointer_reference_usage e = 
  match stars_exp e with 
    | None -> Any 
    | Some (x,ty,n) -> 
	if x.vformal then 
	  (if (stars_typ ty = n) then Ok (x,n) else Any) 
	else Any

let by_pointer_reference_usage_term e = 
  match stars_term e with 
    | None -> Any 
    | Some (x,ty,n) ->
	if (is_lformal x) then 
	  (if (stars_lv_typ ty = n) then Ok (x,n) else Any) 
	else Any

(* [by_array_reference_usage e] implements 1a*)
let by_array_reference_usage e =
  let s = "[by_array_ref_usage]" in 
  debug "%s" s; 
  match delta_array e with 
    | None ->
	(match bracket_exp e with 
	   | None -> 
	       debug "%s not a bracket pattern" s;
	       Any
	   | Some (x,n) ->
	       debug "%s %a[]<%d>" s Printer.pp_varinfo x n;
	       if x.vformal then 
		 (debug "%s %a is a formal" s Printer.pp_varinfo x;
		  let arr = brackets_and_stars_typ x.vtype in 
		  if (arr >= n) then 
		    (debug "%s %a has dim %d ok!" s Printer.pp_varinfo x arr;
		     Ok (x,arr)) else 
		      (debug "%s %a has dim %d when need %d ko!"
			 s Printer.pp_varinfo x arr n;
		       Ko(x,arr))
		 )
	       else 
		 ( debug "%s %a is not a formal" s Printer.pp_varinfo x; 
		   Any) )
    | Some (x,n) -> 
	 debug "%s %a[]" s Printer.pp_varinfo x ;
	       if x.vformal then Ok (x,n) else Any
		

let by_array_reference_usage_term e = 
  let s = "[by_array_ref_usage_term]" in
   debug "%s" s; 
  match delta_array_term e with 
    | None ->
	(match bracket_term e with 
	   | None -> 
	       debug "%s not a bracket pattern" s; Any
	   | Some (x,n) ->
	       begin
		 debug "%s %a[]<%d>" s Printer.pp_logic_var x n;
		 if (is_lformal x) then 
		   (  debug "%s %a is a formal" s Printer.pp_logic_var x;
		      let arr = brackets_and_stars_lv_typ x.lv_type in
		      if (arr >= n) then 
			(debug "%s %a has dim %d ok!" s 
			   Printer.pp_logic_var x arr ;Ok (x,arr)) 
		      else 
			(debug "%s %a has dim %d when need %d ko!"
			   s Printer.pp_logic_var x arr n  
			;Ko (x,arr))) 
		 else
		   ( debug "%s %a is not a formal" 
		       s Printer.pp_logic_var x;Any)
	       end)

    |Some (x,n) ->
       debug "%s %a[]" s Printer.pp_logic_var x ;
	if is_lformal x then Ok (x,n) else Any
	  
(*[reference_parameter_usage e] implements the recognition of the patterns 
  of by reference parameters *)
let reference_parameter_usage e = 
   debug "[reference_parameter_usage]" ;
  match by_pointer_reference_usage e with 
    | Ok(x,n) -> 
	debug "   %a used as ptr reference param of arity %d"
	  Printer.pp_varinfo x n ;
	add_ptr_reference_param (Cv x) n; true 
    | Ko(x,_) -> 
	debug "   %a BADLY used as ptr reference param"
	  Printer.pp_varinfo x  ;
	remove_ptr_reference_param (Cv x); true 
    | Any -> 
	(match by_array_reference_usage e with
	   | Ok(x,n) -> 
	       debug "   %a used as array reference param of arity %d"
		 Printer.pp_varinfo x n ;
	       add_array_reference_param (Cv x) n ; true
	   | Ko(x,_) -> 
	       debug "   %a BADLY used as array reference param"
		 Printer.pp_varinfo x  ;
	       remove_array_reference_param (Cv x);true
	   | Any -> (); false)

let reference_parameter_usage_lval lv = reference_parameter_usage (Lval lv) 

let reference_parameter_usage_term e = 
  debug "[reference_parameter_usage_term]" ;
  match by_pointer_reference_usage_term e with 
    | Ok(x,n) -> 
	debug "   %a used as ptr reference param of arity %d"
	  Printer.pp_logic_var x n ;
	add_ptr_reference_param (var_type_of_lvar x) n ; true
    | Ko(x,_) ->
	debug "   %a BADLY used as ptr reference param"
	  Printer.pp_logic_var x  ;
	remove_ptr_reference_param (var_type_of_lvar x) ; true
    | Any -> 
	(match by_array_reference_usage_term e with 
	   | Ok(x,n) ->
	       debug "   %a used as array reference param of arity %d"
		 Printer.pp_logic_var x n ;
	       add_array_reference_param (var_type_of_lvar x) n ; true
	   | Ko(x,_) -> 
	       debug "   %a BADLY used as array reference param"
		 Printer.pp_logic_var x  ;
	       remove_array_reference_param (var_type_of_lvar x) ; true
	   | Any -> (); false)


(**********************************************************************)
(*** Parameters identification without call                         ***)
(**********************************************************************)

(* This visitor dispatches all formal parameters according to their 
   usage in terms and expressions without inpecting the calls,
   applications and application in ACSL builtin predicates and functions.
*)
class parameters_call_kind_analysis : Visitor.frama_c_visitor = object
  inherit Visitor.frama_c_inplace

  method! vinst = function 
    | Call (_ ,{enode =Lval(Var _,NoOffset)} , _,_) -> SkipChildren
    | Set (lv,_,_) ->
	if reference_parameter_usage_lval lv then SkipChildren else DoChildren 
    | _ -> DoChildren

  method! vexpr e = 
    if reference_parameter_usage  (Cil.stripInfo e).enode then SkipChildren 
    else DoChildren
	    
  method! vterm t = 
    match t.term_node with 
      | Tapp (_,_ , _) -> SkipChildren 
      | Tblock_length (_,_)
      | Toffset (_,_) -> SkipChildren 
      | t1 -> 
	  if reference_parameter_usage_term t1 then SkipChildren else DoChildren
	 
  method! vpredicate = function
    | Papp (_, _, _) -> SkipChildren
    | Pvalid _ 
    | Pvalid_read _ 
    | Pinitialized _ | Pfresh _ | Pseparated _  ->  SkipChildren
    | _ -> DoChildren

end

let compute_parameters_usage () =
  debug 
    "[Parameters Usage] logic parameters usage computation"; 
  debug 
    "[Parameters Usage] computing address taken and logic parameters first"; 
  compute_logic_params ();
  if not (ByValue.is_computed()) || 
    not (ByPReference.is_computed() || not(ByAReference.is_computed()))
  then 
    ( Visitor.visitFramacFile 
	(new parameters_call_kind_analysis)(Ast.get());
      ByPReference.mark_as_computed();ByAReference.mark_as_computed();
      ByValue.mark_as_computed())
      

(*************************************************************************)
(*** Usage of effective parameter in by reference call                 ***)
(*************************************************************************)	


(* [by_pointer_reference_pattern e] returns [Ok(x,b,n)] if [x] appears as 
   a root in [e] with arity [n] and a test of address taken [b].

   A by pointer reference pattern is one a the following :
   - &x+offset --> (x,true,arity of typ(x)) ;
   - x+i with x of pointer type and + as +PI ----> (x,false, arity of typ(x+i))
   - *<n>x, n < stars(typ(x)) ----> (x,false,n).

   else returns:
    - [Any] when the pattern is not significant ; 
    - [Ko] when the pattern is clearly uncompatible with a 
   by pointer reference pattern.
*)
let by_pointer_reference_pattern = function
  | Lval (Var x,off) ->
      let t = (Cil.typeOfLval (Var x,off)) in 
      if  Cil.isPointerType t then 
	Ok (x,false, stars_typ t)
      else Any
  | AddrOf (Var x, off) -> Ok (x,true, stars_typ (Cil.typeOfLval (Var x,off))) 
  | e -> 
      begin 
	match delta_ptr e with 
	   | None ->
	       (match stars_exp e with 
		  | None -> Any 
		  | Some (x,ty,n) -> 
		      let stars = stars_typ ty in
		      if n < stars
		      then Ok (x,false,n) else 
			(if stars = n then Any else Ko (x,false,n))) 
	   | Some (x,n) -> Ok (x,false,n)
      end
	
let by_pointer_reference_pattern_term = function
  | TLval(TVar lvar, off)
  | Tat ({term_node = TLval(TVar lvar, off) },_)->
      let t = Cil.typeOfTermLval (TVar lvar,off) in 
      if Logic_utils.isLogicPointerType t then
	Ok (lvar,false,stars_lv_typ t) 
      else Any
  | TAddrOf(TVar lvar, off)
  | Tat ({term_node = TAddrOf(TVar lvar, off) },_)->
      Ok (lvar,true, stars_lv_typ (Cil.typeOfTermLval (TVar lvar,off)))
  | Tat({term_node = t},_) | t -> 
      begin
	match delta_ptr_term t with 
	  | None -> 
	      (match stars_term t with 
		 | None -> Any 
		 | Some (x,ty,n) -> 
		     let stars = stars_lv_typ ty in
		     if n < stars
		     then Ok (x,false,n) else 
		       (if n = stars then Any else Ko (x,false,n)))
	  | Some (x,n) -> Ok (x,false,n) 
      end



(* help called in [by_array_reference_pattern e]*)
let help_by_array_reference_pattern e =
    match delta_array e with 
      | None ->
	  (match bracket_exp e with 
	     | None -> Any
	     | Some (x,n) ->
		 if x.vformal then
		   begin 
		     let dim = brackets_typ x.vtype in 
                     if n < dim
                     then (Ok (x,false,n)) else 
                       (if n = dim then Any else Ko (x,false,n))
		   end 
		 else Ok(x,false,n))
      | Some (x,n) -> Ok (x,false,n)



(*[by_array_reference_pattern e] returns [Ok (x,b,n)] 
  if [x] appears as a root in [e] with test of address taken (b] and arity [n]. 
  
  A by array reference pattern is one of the following :
        - x+offset (StarOf) ----> Ok(x,true,arity_of (typ(x)));
        - x<k>[], k < bracket(typ(x)) ----->Ok(x,false,k);
        - &(x+i) + ==+PI ----> Ok(x,true,n)

  else returns:
    - [Any] when the pattern is not significant ; 
    - [Ko] when the pattern is clearly uncompatible with a 
   by array reference pattern.
*)


let by_array_reference_pattern = function
  | StartOf (Var x,off) -> Ok (x,true,brackets_typ (Cil.typeOfLval (Var x,off)))
  | CastE(ty,{enode = StartOf (Var x,off)}) when Cil.isPointerType ty -> 
      Ok (x,true,brackets_typ (Cil.typeOfLval (Var x,off)))
  | AddrOf (Mem e, _) -> 
      (match delta_ptr (Cil.stripInfo e).enode with 
	 | None -> Any
	 | Some (x,n) -> Ok (x,true,n)) 
  | CastE (t,e) -> 
      debug "[by_array_reference_pattern] cast case";
     if Cil.isPointerType t then 
       ( debug "is a pointer type";
       help_by_array_reference_pattern (Cil.stripInfo e).enode)
     else 
       (debug "is NOT a pointer type " ;Any )
  | e -> help_by_array_reference_pattern e


let help_array_reference_pattern_term s t = 
  match delta_array_term t with 
    | None ->
	(match bracket_term t with 
	   | None -> Any
	   | Some (x,n) ->
	       if is_lformal x then
		 begin
	           debug "%s %a[]<%d>" s Printer.pp_logic_var x n;
		   let dim = brackets_lv_typ x.lv_type in
		   if n < dim 
		   then 
		     (debug "%s %a has dimension %d ok!"
			s Printer.pp_logic_var x n;
		      Ok (x,false,n)) 
		   else 
		     ( if dim = n then Any else 
			 (debug "%s %a has dimension %d when need %d!"
                            s Printer.pp_logic_var x dim n;
			  Ko (x,false,n)))
		 end
	       else Ok(x,false,n)
	)
    | Some (x,n) -> 
	debug "%s %a in delta_array term" s Printer.pp_logic_var x;
	Ok (x,false,n) 
	  
let by_array_reference_pattern_term t =
  let s = "[by_array_reference_pattern_term]" in
  match t with 
  | TStartOf (TVar lvar,off) 
  | Tat ({term_node = TStartOf (TVar lvar,off) },_)-> 
    debug "%s %a " s Printer.pp_logic_var lvar; 
      Ok(lvar,true,brackets_lv_typ (Cil.typeOfTermLval (TVar lvar,off)))
     
  |TCastE(ty,{term_node = ( TStartOf (TVar lvar,off) 
	      | Tat ({term_node = TStartOf (TVar lvar,off) },_))}) when 
      Cil.isPointerType ty ->
    debug "%s %a " s Printer.pp_logic_var lvar; 
	Ok (lvar,true,brackets_lv_typ(Cil.typeOfTermLval (TVar lvar,off)))

  | TAddrOf (TMem t, _) 
  | Tat ({term_node = TAddrOf (TMem t, _) },_) -> 
      (match delta_ptr_term t.term_node with 
	 | None -> Any
	 | Some (x,n) -> 
	     debug "%s %a in delta_ptr term" s Printer.pp_logic_var x;
	     Ok (x,true,n)) 
  | Tat({term_node = t},_)-> help_array_reference_pattern_term s t
  | TCastE(ty,{term_node = t}) when (Cil.isPointerType ty)->
      help_array_reference_pattern_term s t
  | t ->help_array_reference_pattern_term s t
	
	



(**********************************************************************)
(*** Collection of potential Chain of by reference calls            ***)
(**********************************************************************)

(* [collect_calls_occurences (eargs,sgn)] visits a list of arguments and 
   a signature and collects each call of thus cases: 
    - [x<n>*] in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a by pointer reference parameter.
      Then, the [chain_call] of [x] is updated in the [ByPReference] table
      with the call site [(n,p)].

   - [x<n>[]]  in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a by array reference parameter. 
      Then, the [chain_call] of [x] is updated in the [ByAReference] table
      with the call site [(n,p)].

   - [x<n>*] in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a none formal.
      Then, the [chain_call] of [x] is updated in the [ArgPReference] table
      with the call site [(n,p)].

   - [x<n>[]]  in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a none formal. 
      Then, the [chain_call] of [x] is updated in the [ByAReference] table
      with the call site [(n,p)].

   - in all other case, nothing is done and the collection progress 
   in the tail of both lists.
*)

(* Table of by pointer reference argument *)
module ArgPReference = 
  State_builder.Hashtbl
    (VarType.Hashtbl)
     (Datatype.Pair (Datatype.Int) (ChainCalls))
    (struct let name = "WP: argument by pointer reference not formal"
            let dependencies = [Ast.self]
            let size = 47
     end)

(*Table of by array reference argument *)
module ArgAReference = 
  State_builder.Hashtbl
    (VarType.Hashtbl)
     (Datatype.Pair (Datatype.Int) (ChainCalls))
    (struct let name = "WP: argument by array reference not formal"
            let dependencies = [Ast.self]
            let size = 47
     end)


    
(* [add_ptr_reference_arg x n] tries to adds [x] of arity [n] in the 
   table of by pointer reference argument.
 
    -If [x] is in AnyVar table, then [x] can't been added to this table.

    -If [x] is already registered in by pointer reference argument, the 
     already recorded arity has to be [n] else [x] is removed from this 
     table and adds to the AnyVar table. 
     
   - If [x] is not registered in the by pointer reference table: 
        *[x] is in the by array reference argument then [x] is removed 
        from this table and adds to the any var table. 
        *else [x] is registered in the by pointer reference argument with the
        arity [n] and the empty chain call.
*)    
let add_ptr_reference_arg x n = 
  oracle "[ArgPRef] try + %a" pp_var_type x;
  if AnyVar.mem x then 
    (oracle "[ArgPRef] %a AnyVar"pp_var_type x;())
  else
    begin
      try 
	if not (fst (ArgPReference.find x) = n) then
	  (oracle "[ArgPRef] remove %a : ko arity ->+AnyVar"
	     pp_var_type x;
	   ArgPReference.remove x; AnyVar.replace x ())
	else
	  (oracle "[ArgPRef] %a already" pp_var_type x;())
      with Not_found ->
	(if ArgAReference.mem x then 
	   (oracle "[ArgPRef] %a ArgARef : remove -> + AnyVar" 
	      pp_var_type x;
	    ArgAReference.remove x; AnyVar.add x ())
	 else 
	   (oracle "[ArgPRef] + %a"pp_var_type x;
	    ArgPReference.add x (n,[])))
    end
      
let remove_ptr_reference_arg x =
  oracle "[ArgPRef] remove %a" pp_var_type x;
  if ArgPReference.mem x then
    (oracle "[ArgPRef] remove %a of ArgPRef" pp_var_type x;
     ArgPReference.remove x);
  oracle "[ArgPRef] + %a AnyVar" pp_var_type x ;
  AnyVar.replace x ()

(* [add_array_reference_arg x n] tries to add [x] with arity [n] in the table 
  of by array reference arguments. 
   - If [x] is in any var table, [x] can't been added to this table.

   - If [x] already registered in the by array reference argument; then 
     the already recorded arity has to been [n] otherwise
         *[n] is not the correct arity, [x] is removed from this table 
         and adds to the any var table
         *[n] is the correct arity, nothing has to be done
  - If [x] is not yet in the table of by array reference argument:
         *[x] is in the table of by pointer reference argument. [x] is removed
         form this table and adds to the any var table 
         *[x] has not yet been registered, [x] is registered with the 
         arity [n] and the empty chaincalls in the table of by array reference
         argument.
*)
let add_array_reference_arg x n = 
  oracle "[ArgARef] try + %a" pp_var_type x;
  if AnyVar.mem x then 
    (oracle "[ArgARef] %a AnyVar"pp_var_type x;())
  else
    begin
      try 
	if not (fst (ArgAReference.find x) = n) then
	  (oracle "[ArgARef] remove %a : ko arity ->+AnyVar"
	     pp_var_type x;
	   ArgAReference.remove x; AnyVar.replace x ())
	else
	  (oracle "[ArgARef] %a already" pp_var_type x;())
      with Not_found ->
	(if ArgPReference.mem x then 
	   (oracle "[ArgARef] %a ArgPRef : remove -> + AnyVar" 
	      pp_var_type x;
	    ArgPReference.remove x; AnyVar.add x ())
	 else 
	   (oracle "[ArgARef] + %a"pp_var_type x;
	    ArgAReference.add x (n,[])))
    end
      
let remove_array_reference_arg x =
  oracle "[ArgARef] remove %a" pp_var_type x;
  if ArgAReference.mem x then
    (oracle "[ArgARef] remove %a of ArgARef" pp_var_type x;
     ArgAReference.remove x);
  oracle "[ArgARef] + %a AnyVar" pp_var_type x ;
  AnyVar.replace x ()


(* [collect_calls_rec (eargs,sgn)] visits a list of arguments and 
   a signature and collects each call of thus cases: 
    - [x<n>*] in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a by pointer reference parameter.
      Then, the [chain_call] of [x] is updated in the [ByPReference] table
      with the call site [(n,p)].
   - [x<n>[]]  in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a by array reference parameter. 
      Then, the [chain_call] of [x] is updated in the [ByAReference] table
      with the call site [(n,p)].
   - [x<n>*] in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a none formal.
      Then, the [chain_call] of [x] is updated in the [ArgPReference] table
      with the call site [(n,p)].
 - [x<n>[]]  in [eargs] associates to the parameter [p] 
      into the signature [sgn] when [x] is a none formal. 
      Then, the [chain_call] of [x] is updated in the [ByAReference] table
      with the call site [(n,p)].
   - in all other case, nothing is done and the collection progress 
   in the tail of both lists.

   Implements 2p) 2a) and computation of other kind of variables passed
   by reference. 
*)	

(*[collect_formal_array_call s x n b p] tries to collect in bellow function
  characterized by [s] for debugging  the by array reference call [(n,(b,p))] 
  in the chaincall of the by array reference parameter [x] with:
  arity [n] with test of address taken [b] on prameter type [p] with 
  the effective argument containing the variable [x].
   -If [x] already in the table of by array reference parameter:
      * [n] is convenient with the registered arity of [x] then 
        adds the call to the chain call of [x]
      * else [x] is removed from the table of by array reference parameters
   -If [x] is not yet registered in the table of by array reference parameters, 
    tries to add [x] in this table: 
      * if ok then computes the arity of [x], [arr]. 
           a) if [n] is convenient for [arr] then add [x] to the table of 
           by array reference parameter with arity [arr] and the call. 
           b) else nothing has to be done      
 *)
let collect_formal_array_call s x n b p =
  try 
    let (arr,calls) = ByAReference.find x in 
    oracle "%s %a ByARef" s pp_var_type x;
    if n <= arr then 
      ( oracle "%s %a + call(%a,%d,%s)" s pp_var_type x
	  pp_var_type p n (string_addr b);
	ByAReference.replace x (arr,((n,(b, p))::calls)))
    else 
      ( oracle "%s %a remove %d used %d" s pp_var_type x arr n;
	remove_array_reference_param x) 
  with Not_found ->
    oracle "%s %a not yet ByARef" s pp_var_type x;
    let arr = brackets_and_stars_var_type_typ x in
    add_array_reference_param x arr;
    try let (_,calls) = ByAReference.find x in  
    ByAReference.replace x (arr,(n,(b,p))::calls)
    with Not_found -> ()

      
(* as collect_arg_array_call for by pointer reference call of argument.*)
let collect_arg_ptr_call s x n b p =
  if AnyVar.mem x then 
    (oracle "%s %a AnyVar" s pp_var_type x ;())
  else
    try 
      let (arr,calls) = ArgPReference.find x in
      oracle "%s %a ArfPRef" s pp_var_type x;
      if n <= arr then
	(oracle "%s %a + call(%a,%d,%s)" s pp_var_type x
	   pp_var_type p n (string_addr b);
	 ArgPReference.replace x (arr,((n,(b,p))::calls)))
      else 
	(oracle "%s %a remove %d used %d" s pp_var_type x arr n;
	 remove_ptr_reference_arg x)
    with Not_found ->
      oracle "%s %a not yet in ArgPref" s pp_var_type x;
      let arr = stars_var_type_typ x in add_ptr_reference_arg x arr;
      if ArgPReference.mem x then 
	(if n <= arr then
	   (oracle"%s %a + call(%a,%d,%s)" 
	      s pp_var_type x pp_var_type p n (string_addr b); 
	    ArgPReference.replace x (arr,[n,(b,p)]))
	 else ())


(* [collect_arg_array_call s x n b p] tries to collect, in the bellow function 
   characterized by [s] for debugging, the calls [(n,(b,p))] in the chain call
   of the by array reference argument [x].
    -If [x] is any var, nothing has to be done. 
    -If [x] is already registered in the table of by array reference argument,
     according to the convenient of [n] to the registered arity of [x], 
     the calls is added to the chaincall of (x] or [x] is removed from this 
     table. 
    -If (x] is not yet registered, the arity of [x] is computed [arr] and
     and tries to add [(x,arr)] in the table of by array reference argument.  
     If the add succeed adds the call else nothing has to be done       
*)
let collect_arg_array_call s x n b p =
  if AnyVar.mem x then 
    (oracle "%s %a AnyVar" s pp_var_type x ;())
  else
    try 
      let (arr,calls) = ArgAReference.find x in
      oracle "%s %a ArfARef" s pp_var_type x;
      if n <= arr then
	(oracle "%s %a + call(%a,%d,%s)" s pp_var_type x
	   pp_var_type p n (string_addr b);
	 ArgAReference.replace x (arr,((n,(b,p))::calls)))
      else 
	(oracle "%s %a remove %d used %d" s pp_var_type x arr n;
	 remove_array_reference_arg x)
  with Not_found ->
    oracle "%s %a not yet in ArgAref" s pp_var_type x;
    oracle "%s %a try to collect with %d" 
      s pp_var_type x n;
    if isVarTypePointerType x then collect_arg_ptr_call s x n b p
    else
    (if n <> 0 then
       (oracle"%s %a + call(%a,%d,%s)" 
	  s pp_var_type x pp_var_type p n (string_addr b); 
	add_array_reference_arg x n;
	try let (n,calls) = ArgAReference.find x in  
	  ArgAReference.replace x (n,(n,(b,p))::calls)
	with Not_found -> ())
    else ())

(* as collect_formal_array_call for by pointer reference parameters. 
   Note that is [px] not yet in the table of by pointer reference parameters 
   then if [px] is in the table of by array reference parameters then 
   tries to register this call as a formal array call -> 
   [collect_formal_array_call] In fact, the patterns of by array reference calls
   contains the patterns of by pointer reference calls.
*)
let collect_formal_ptr_call s px n b p =
  try 
    let (arr,calls) = ByPReference.find px in 
    oracle "%s %a ByPRef" s pp_var_type px;
    if n <= arr then 
      ( oracle "%s %a + call(%a,%d,%s)" s pp_var_type px
	  pp_var_type p n (string_addr b);
	ByPReference.replace px (arr,((n,(b,p))::calls)))
    else 
      ( oracle "%s %a remove %d used %d" s pp_var_type px arr n;
	remove_ptr_reference_param px) 
  with Not_found ->
    oracle "%s %a not yet ByPRef" s pp_var_type px;
    if ByAReference.mem px then collect_formal_array_call s px n b p
    else 
      begin
	let arr = stars_var_type_typ px in add_ptr_reference_param px arr;
	if ByPReference.mem px then
	  (if n <= arr then 
	     ( oracle "%s %a + call(%a,%d,%s)" 
		 s pp_var_type px pp_var_type p n (string_addr b);
	       ByPReference.replace px (arr,[n,(b,p)])))
	else () 
      end




(*[collect_calls_rec (eargs,fmls)] collects, in a C call assigning the 
  effective arguments [eargs] to the formal parameter [fmls], 
  the calls, using preview functions according to the identified argument
  patterns for each pair of effective argument [e] and formal parameter 
  [p].
 *)
let rec collect_calls_rec (eargs,fmls) =
  let s = "[collect_calls]" in 
  match eargs,fmls with 
    | [],[] -> () 
    | [], _ | _, [] -> () (*TODO: check for variadyc functions *)
    | e::args, p::fmls -> 
	debug "%s no empty list" s; 
	let e1 = (Cil.stripInfo e).enode in 
	(match by_array_reference_pattern e1 with 
	   | Ok (x,b,n) ->
	       let sb =string_addr b in
	       debug "%s array pattern of %a with %s" s 
		 Printer.pp_varinfo x sb;
	       let x = Cv x and p = Cv p in 
	       if is_formal_var_type x then
		 collect_formal_array_call s x n b p 
	       else 
		 collect_arg_array_call s x n b p      
		   
	   | Ko (x,_,_) ->
	       debug "%s not array pattern" s;
	       if x.vformal then
		 remove_array_reference_param (Cv x)
	       else ArgAReference.remove (Cv x)
	   | Any ->
	       ( match by_pointer_reference_pattern e1 with 
		   | Ok (x,b,n) ->
		       let sb = string_addr b in
		       debug "%s ptr pattern of %a with %s and %d" 
			 s Printer.pp_varinfo x sb n;
		       let x = Cv x and p = Cv p in 
		       if is_formal_var_type x then  
			 collect_formal_ptr_call s x n b p 
		       else collect_arg_ptr_call s x n b p
			 
		   | Ko (x,_,_) ->
		       debug "%s not ptr pattern" s; 		     
		       if x.vformal then remove_ptr_reference_param (Cv x)
		       else ArgPReference.remove (Cv x)
			 
		   | Any ->()
	       )
	); collect_calls_rec (args,fmls)

	  
let collect_calls f el = 
  let kf = Globals.Functions.get f in 
  let fmls =  Kernel_function.get_formals kf in 
  debug "[collect_calls]";
  collect_calls_rec (el,fmls)
    
    
let ok_array_term_formal s x n b p = 
  collect_formal_array_call s x n b p
				     
let ok_array_term_arg s x n b p = 
  collect_arg_array_call s x n b p
    				 
let ok_array_term s x n b p = 
  if is_formal_var_type x then ok_array_term_formal s x n b p
  else ok_array_term_arg s x n b p
    
let ok_ptr_term_formal s x n b p = 
  collect_formal_ptr_call s x n b p
				     
let ok_ptr_term_arg s x n b p = 
  collect_arg_ptr_call s x n b p
				 
let ok_pointer_term s x n b p =
  if is_formal_var_type x then ok_ptr_term_formal s x n b p
  else ok_ptr_term_arg s x n b p
    
(* as collect_calls_rec on logic application*)
let rec collect_apps_rec = function 
  | [],[] -> () 
  | [], _ | _, [] -> () (*TODO: check correctness for variadyc functions *)
  | t::args, p::fmls -> 
      let s = "collect_app" in
      (match by_array_reference_pattern_term t.term_node with 
	 | Ok (x,b,n) -> 
	     debug "(%a,%b,%d) by_array in apps_rec" 
	       Printer.pp_logic_var x b n;
	     ok_array_term s (var_type_of_lvar x) n b (var_type_of_lvar p )
	 | Ko (x,_,_) -> 
	     let x = var_type_of_lvar x in
	     if is_formal_var_type x 
	     then  remove_array_reference_param x
	     else ArgAReference.remove x
	       
	 | Any -> 
	     ( match by_pointer_reference_pattern_term t.term_node with 
		 | Ok (x,b,n) -> 
		     let p = var_type_of_lvar p in
		     let x = var_type_of_lvar x in 
		     ok_pointer_term s x n b p
		 | Ko (x,_,_) ->
		     let x = var_type_of_lvar x in
		     if is_formal_var_type x 
		     then remove_ptr_reference_param x
		     else ArgPReference.remove x    
		 | Any ->()
	     )
      ); collect_apps_rec (args,fmls)

let collect_apps lf tl =  collect_apps_rec (tl,lf.l_profile)

  

(* as collect_apps_rec on logic builtin application 
   if the argument is a userdef parameter, its information in 
   LogicParam is updated by the test of addresse taken *)
let rec collect_apps_builtin targs = 
  let s = "[BuiltinCall]" in
  match targs with 
    | [] -> () 
    | t::args -> 
	(match by_array_reference_pattern_term t.term_node with 
	   | Ok (x,b,n) ->
	       debug "%s %a in array ref position with %s with dim = %d" 
		 s Printer.pp_logic_var x (string_addr b) n;
	       logic_param_memory_info x; 
	       ok_array_term s (var_type_of_lvar x) n b Prop
	   | Ko (x,_,_) -> 
	       debug "%s %a is not in a array ref position"
		 s Printer.pp_logic_var x ;
	       let x = var_type_of_lvar x in 
	       if is_formal_var_type x 
	       then remove_array_reference_param x
	       else ArgAReference.remove x
		 
	   | Any -> 
	       ( match by_pointer_reference_pattern_term t.term_node with 
		   | Ok (x,b,n) ->  
		       debug "%s %a in ptr ref position with %s with %d *" 
			 s Printer.pp_logic_var x (string_addr b) n;
		       logic_param_memory_info x;
		       ok_pointer_term s (var_type_of_lvar x) n b Prop
		   | Ko (x,_,_) ->
		       debug "%s %a is not in a ptr ref position" 
			 s Printer.pp_logic_var x ;
		       let x = var_type_of_lvar x in
		       if is_formal_var_type x 
		       then remove_ptr_reference_param x
		       else ArgPReference.remove x
		   | Any -> ()
	       )
	); collect_apps_builtin args

	  
(**********************************************************************)
(*** Chain of calls collections                                     ***)
(**********************************************************************)

let calls_collection_computed = ref false

(* This visitor inpects all calls,applications and ACSL builtin applications
   and then : 
    - collects [call]s and builds [chaincalls] of each kind of variable; 
    - redefines the kind of a variable if needed. 
   Typically when patterns of a same variable are of different kinds 
   or for a formal when the usage (found in the last visitor) is not 
   compatible with a pttern (found in this visitor). 

NB: The resolution of an entire [ChainCall] can't been done here because 
    all [call] has to been inspected before. 

*)

class calls_collection : Visitor.frama_c_visitor = object
  inherit Visitor.frama_c_inplace

  method! vinst = function 
    | Call (_ ,{enode =Lval(Var f,NoOffset)} , el,_) as e->
	debug "[Calls_collection] call %a" Printer.pp_instr e;
	collect_calls f el ; SkipChildren
    | _ -> DoChildren
	    
  method! vterm t = 
    match t.term_node with 
      | Tapp (lf,_ , targs) -> 
	  debug "[Calls_collection] app %a" Printer.pp_term t;
	  collect_apps lf targs ; SkipChildren
      | Tblock_length (_label,ta) -> (* [PB] TODO label added *)
	  debug "[Calls_collection] block_length %a" Printer.pp_term t;
	 collect_apps_builtin [ta] ; SkipChildren
      | _ -> DoChildren
	  
  method! vpredicate = function
    | Papp (lf, _, targs) -> collect_apps lf targs ; SkipChildren
    | Pfresh (_todo_label1,_todo_label2,t,n) -> (* [PB] TODO: labels and size added *)
	debug "[Calls_collection] predicate app on %a, %a" 
	  Printer.pp_term t  Printer.pp_term n ;
	collect_apps_builtin [t;n] ; SkipChildren
    | Pallocable (_todo_label,t) (* [PB] TODO: construct added *)
    | Pfreeable (_todo_label,t)  (* [PB] TODO: construct added *)
    | Pvalid_read (_todo_label,t)(* [PB] TODO: construct added *)
    | Pvalid (_todo_label,t)     (* [PB] TODO: label added *)
    | Pinitialized (_todo_label,t) -> (* [PB] TODO: label added *)
	debug "[Calls_collection] predicate app on %a" Printer.pp_term t;
	collect_apps_builtin [t] ; SkipChildren
    | Pseparated lt  ->	collect_apps_builtin lt ; SkipChildren
    | _ -> DoChildren

end

let compute_calls_collection () =
  debug 
    "[Calls Collection] collectinfg potential by reference calls"; 
  debug 
    "[Calls Collection] computing parameters usage first"; 
  compute_parameters_usage ();
  if not !calls_collection_computed then 
    (Visitor.visitFramacFile (new calls_collection)(Ast.get());
     calls_collection_computed := true)
      



(**********************************************************************)
(*** Chain of calls Resolution                                      ***)
(**********************************************************************)


(*
 B - Chain of Calls Resolutions                                 
 The second part of the by-reference-parameters                 
 identification,  is the verification of the [ChainCalls]         
 for each formal parameter  occured in ByPReference             
 or ByAReference table.                                         
 Concerning the argument by-reference, chain of calls has to be 
 resolved too. During this resolution, the addrtaken table is   
 updated : address taken in real by-reference call are          
 subtracted.                                                     

  The resolution of the [ChainCall] of formal parameters have to occur
  before de resolution the [ChainCall]of other variable. 

*)


(* Chain of call resolution of the table of by pointer reference parameters. 
   A convenient [call] for a by pointer reference parameter [x] with 
   arity [n] is : 
     -a builtin application [(k,(b,Prop))], k <= [n] 
     -an application or call [(k,(b,p))], k <= n and [p] is a
      by pointer reference parameter.
   
   For each convenient [call], if the test of address taken is true, then 
   the [minus] information of [x] in the address taken table is incremented.

   A [ChainCalls] is resolved when all is [call]s has been inspected.
   If all [call]s of the [ChainCalls] [calls] are convenient,
   [x] stays in by pointer reference parameter with arity [n].
   Otherwise, [x] is moved from the by pointer reference parameter
   table to the by value parameter table.
   
   NB: For a call [(k,(b,p))], [p] can not yet occur in 
   the by pointer reference paramter table, then [p] has first to be 
   add in this table and its [ChainCalls] has to been resolved before 
   the resolution of this call.

*)
let rec by_ptr_reference x n calls =
  let s = "[by_ptr_reference]" in 
  debug "%s %a of arity %d" s pp_var_type x n; 
  match calls with
    | [] -> 
	debug "%s %a: ok " s pp_var_type x; 
	oracle "%s %a ByPref" s pp_var_type x;
	ByPReference.replace x (n,[])
    | (k,(b,Prop))::m ->
	let sb = string_addr b in 
	debug "%s %a: (builtin,%d,%s)" s pp_var_type x k sb;
	if k <= n then 
	  (debug "%s arity of call ok" s; decr_addr_taken_bool x b;
	   by_ptr_reference x n m)
	else 
	  (debug "%s arity of call too big" s; remove_ptr_reference_param x)
    | (k,(b,p))::m ->
	let bv =  ByValue.mem p in let ba = ByAReference.mem p in
	let c = k > n in let sb = string_addr b in
	if c || bv || ba then 
	  (debug "%s: KO %a ByValue:%b ; Aref : %b; call arity:%b" 
	     s pp_var_type p bv bv c ; remove_ptr_reference_param x)
	else
	  (debug "%s: OK %a ByValue:%b ; Aref : %b; call ari:%b; with %s" 
	     s pp_var_type p bv ba c sb ;
	   try (match ByPReference.find p with 
		  | (i,[]) -> 
		      debug "%s %a already resolved ; arity :%d"
			s pp_var_type p i; 
		      if k <= i then 
			(debug"%s arity OK" s; decr_addr_taken_bool x b; 
			 by_ptr_reference x n m) 
		      else 
			(debug "%s arity KO %a with %d and %a with %d used %d"
			   s pp_var_type x n pp_var_type p i k;
			 remove_ptr_reference_param x )
		  | (i,lp) ->
		      debug "%s %a has to be resolved; with %d used %d"
			s pp_var_type p i k;
		      if k <= i then 
			(by_ptr_reference p i lp; 
			 by_ptr_reference x n ((k,(b,p))::m))
		      else remove_ptr_reference_param x
	       )
	   with Not_found -> 
	     debug "%s %a NOT in PRef param" s pp_var_type p;
	     let i = stars_var_type_typ p in  
	     add_ptr_reference_param p i;
	     if not (ByPReference.mem p) || ByValue.mem p
	     then remove_ptr_reference_param x
	     else by_ptr_reference x n ((k,(b,p))::m))
	    
	    
(* Chain of call resolution of the table of by array reference parameters*)
let rec by_array_reference x n l =  
  let s = "[by_array_reference]" in 
  debug "%s %a of arity %d" s pp_var_type x n; 
  match l with 
  | [] ->  
      oracle "%s %a ByAref" s pp_var_type x;
      ByAReference.replace x (n,[])
  | (k,(b,Prop))::m -> 
      if k <= n then 
	(decr_addr_taken_bool x b; by_array_reference x n m)
      else remove_array_reference_param x
  | (k,(b,p))::m ->
      if k < n || ByValue.mem p || ByPReference.mem p then 
	remove_array_reference_param x
      else
	try (match ByAReference.find p with 
	       | (i,[]) -> 
		   if i <= k then 
		     (decr_addr_taken_bool x b; by_array_reference x n m) 
		   else remove_array_reference_param x 
	       | (i,lp) ->
		   if i <= k then 
		     (by_array_reference p i lp; 
		      by_array_reference x n ((k,(b,p))::m))
		   else remove_array_reference_param x
	    )
	with Not_found -> 
	  debug "%s %a NOT in ARef param" s pp_var_type p;
	  let i = brackets_and_stars_var_type_typ p in  
	  add_array_reference_param p i;
	  if not (ByAReference.mem p) || ByValue.mem p 
	  then remove_array_reference_param x
	  else by_array_reference x n ((k,(b,p))::m)

(* resolution of chain of call of formal parameters.*)	   
let resolved_call_chain_param () = 
  ByAReference.iter_sorted 
    (fun var (n,l) -> 
       debug "[resolve chaincall of param] array -> %a:%a"
	 pp_var_type var pp_chaincall l;
       by_array_reference var n l) ; 
  ByPReference.iter_sorted
    (fun var (n,l) -> 
       debug "[resolve chaincall of param] ptr -> %a:%a"
	 pp_var_type var pp_chaincall l;
       by_ptr_reference var n l) 
    
(* Chain of call resolution of the table of by pointer reference argument*)
let rec ptr_reference x n calls = 
  let s = "[ptr_reference arg]" in 
  match calls with 
    | [] ->  
	debug "%s %a: arity %d ok" s pp_var_type x n;
	oracle "%s %a ArgPref"  s pp_var_type x;
	ArgPReference.replace x (n,[])
    | (k,(b,Prop))::m -> 
	let sb = string_addr b in 
	debug "%s (%a,%d) used builtin %d and %s" s pp_var_type x n k sb;
	if k <= n then 
	  ( debug "%s builtin arity OK" s; decr_addr_taken_bool x b ;
	    ptr_reference x n m)
	else 
	  (debug "%s builtin arity KO" s; remove_ptr_reference_arg x)
    | (k,(b,p))::m ->
	let sb = string_addr b in 
	debug "%s (%a %d) ; used as (%a,%d) and %s"
	  s pp_var_type x n  pp_var_type p k sb;
	if k > n then 
	  (debug "%s %a:arity KO " s pp_var_type p;
	   remove_ptr_reference_arg x) 
	else
	  try (match ByPReference.find p with 
		 | (i,[]) ->
		     debug "%s %a is byPref resolved" s pp_var_type p;
		     if k <= i then 
		       ( debug "%s arity OK" s; decr_addr_taken_bool x b;
			 ptr_reference x n m) 
		     else (debug "%s arity KO" s; remove_ptr_reference_arg x) 
		 | (i,lp) -> (* can't happen *) 
		     debug "%s %a is byPref NOT resolved"s pp_var_type p;
		     if k <= i then 
		       ( debug "%s arity OK"s ;by_ptr_reference p i lp; 
			 debug "%s resolution of %a" s pp_var_type p;
			 ptr_reference x n ((k,(b,p))::m))
		     else 
		       (debug "%s arity KO" s; remove_ptr_reference_arg x )
	    )
	with Not_found -> (* can't happen *) 
	  debug "%s %a NOT ByPRef" s pp_var_type p;
	  let i = stars_var_type_typ p in  
	  add_ptr_reference_param p i;
	  if not (ByPReference.mem p) || ByValue.mem p
	  then remove_ptr_reference_arg x
	  else ptr_reference x n ((k,(b,p))::m)
	  
	    
(* Chain of call resolution of the table of by array reference argument*)
let rec array_reference x n calls =
  let s = "[array_reference arg]" in 
  match calls with 
    | [] ->  
	debug "%s %a: arity %d ok" s pp_var_type x n;
	oracle "%s %a ArgAref"  s pp_var_type x;
	ArgAReference.replace x (n,[])
    | (k,(b,Prop))::m -> 
	if k <= n then 
	  (decr_addr_taken_bool x b; array_reference x n m)
	else remove_array_reference_arg x 
    | (k,(b,p))::m ->
	if k > n then ArgAReference.remove x 
	else
	  (
	    if ByPReference.mem p then 
	      begin
		try (match ByPReference.find p with 
		       | (i,[]) ->
			   debug "%s %a is byPref resolved" s pp_var_type p;
			   if k <= i then 
			     ( debug "%s arity OK" s; decr_addr_taken_bool x b;
			       array_reference x n m) 
			   else 
			     (debug "%s arity KO" s;
			      remove_array_reference_arg x) 
		       | (i,lp) -> (* can't happen *) 
			   debug "%s %a is byPref NOT resolved"s pp_var_type p;
			   if k <= i then 
			     ( debug "%s arity OK"s ;by_ptr_reference p i lp; 
			       debug "%s resolution of %a" s pp_var_type p;
			       array_reference x n ((k,(b,p))::m))
			   else 
			     (debug "%s arity KO" s; 
			      remove_ptr_reference_arg x )
		    )
		with Not_found -> remove_array_reference_arg x
	      end
	    else 
	      begin
		try (match ByAReference.find p with 
		       | (_,[]) -> 
			   decr_addr_taken_bool x b; array_reference x n m 
		       | (i,lp) -> (* can't happen *)
			   by_array_reference p i lp; 
			   array_reference x n ((k,(b,p))::m)
		    )
		with Not_found -> (* can't happen *)
		  debug "%s %a NOT ByARef" s pp_var_type p;
		  let i = brackets_and_stars_var_type_typ p in  
		  add_array_reference_param p i;
		  if not (ByAReference.mem p) || ByValue.mem p
		  then remove_array_reference_arg x
		  else array_reference x n ((k,(b,p))::m)
	      end
	  )
	       

(* resolution of chain of call of arguments.*)
let resolved_call_chain_arg () = 
  ArgAReference.iter_sorted
    (fun var (n,l) -> 
       debug "[resolve chaincall of arg] array -> %a:%a"
	 pp_var_type var pp_chaincall l;
       array_reference var n l) ; 
  ArgPReference.iter_sorted
    (fun var (n,l) -> 
       debug "[resolve chaincall of arg] ptr -> %a:%a"
	 pp_var_type var pp_chaincall l;
       ptr_reference var n l)
  

(**********************************************************************)
(*** Address Taken Resolution                                       ***)
(**********************************************************************)

(* [resolve_addr_taken ()] iterates on Address Taken table. 
   For each variable [var] :
    - if the occurences of address taken out of by reference call [m] 
   is strictly more than the occurences in by reference call [r] then 
   [var] stays in the address taken table and it is removes from the 
   by reference table.
   - if [var] address taken occurs more or as much in by reference calls
   [r] than in other case [m] then [var] is remove from the address taken 
   table.*)
  
  let resolve_addr_taken () =
    let remove_from_refs var =
      if is_formal_var_type var then 
	(remove_ptr_reference_param var; 
	 remove_array_reference_param var)
      else 
      (ArgPReference.remove var; ArgAReference.remove var)
    in
    let s = "[resolves addr taken]" in
    AddrTaken.iter_sorted
      (fun var (m,r) -> 
	 debug "%s %a +:%d -:%d" s pp_var_type var m r ; 
	 if m > r then 
	   (debug "%s %a: addr taken %d et %d" s pp_var_type var m r;
	    oracle"%s %a: stays addrtaken"s pp_var_type var;
	   remove_from_refs var)
	 else 
	   (debug "%s %a: not addr taken %d et %d" s pp_var_type var m r;
	    oracle"%s %a: remove addrtaken"s pp_var_type var;
	   AddrTaken.remove var))
      



(**********************************************************************)
(*** Adding Separated hypothesis                                    ***)
(**********************************************************************)

(* The optimization of by reference calls supposing a quiet important
   number of hypothesis about separation between variables. 
   One of this kind of separation hypothesis concerns 
   the separation between by pointer reference parameters of a same 
   signature and all their dereferenced pointers. In this case , 
   we add the pre-condition to the contract of the function. 
   Concerning other kind of separation hypothesis, we emit a warning.*)


  (* Creates the l-value *lv *)
  let deref loc (t:term) : term =
    let typ = match t.term_type with
      | Ctype (TPtr (typ,_)) -> Ctype typ
      | _ -> Wp_parameters.fatal "[deref] on a pure logic type" 
    in
    Logic_const.term ~loc (TLval (TMem t,TNoOffset)) typ

  type formal_kind =
    | Formal_Value
    | Formal_Ref of int
    | Formal_Array of int

  let kind_of_formal x =
    try 
      let (n,_calls) = ByPReference.find (Cv x) in
      if Cil.isPointerType x.vtype then 
	Formal_Ref n
      else
	Formal_Value
    with Not_found ->
      try 
	let (n,_calls) = ByAReference.find (Cv x) in
	Formal_Array n
      with Not_found ->
	Formal_Value
	  
  let rec collect_sepstars loc n (t:term) (sep_terms:term list) =
    let sep_terms = t :: sep_terms in
    if n=1 then sep_terms else
      let tstar = deref loc t in
      collect_sepstars loc (pred n) tstar sep_terms


  let pp_formals fmt = function
    | [] -> ()
    | x::xs -> 
	Format.fprintf fmt "'%s'" x.vname (* user info *) ;
	List.iter (fun x -> Format.fprintf fmt ",@ '%s'" x.vname) xs
        
  let rec collect_refparams kf loc arr_vars ref_vars sep_terms = function
    | x::xs ->
	begin
	  match kind_of_formal x with
	    | Formal_Value -> 
		collect_refparams kf loc arr_vars ref_vars sep_terms xs
	    | Formal_Array _ -> 
		collect_refparams kf loc (x::arr_vars) ref_vars sep_terms xs
	    | Formal_Ref n ->
		let t = Logic_const.tvar ~loc (Cil.cvar_to_lvar x) in
		let sep_terms = collect_sepstars loc n t sep_terms in
		collect_refparams kf loc arr_vars (x::ref_vars) sep_terms xs
	end
    | [] ->
	begin
	  match List.rev arr_vars , List.rev ref_vars with
	    | [] , _ -> ()
	    | [_] , [] -> ()
	    | xs , [] ->
		Wp_parameters.warning 
		  "For function %s,@ array reference parameters %a@ must be disjoint at call site"
		  (Kernel_function.get_name kf) pp_formals xs
	    | xs , ys ->
		Wp_parameters.warning
		  "For function %s, reference parameters@ %a and %a@ must be disjoint at call site"
		  (Kernel_function.get_name kf) pp_formals xs pp_formals ys
	end ;
	match sep_terms with
	  | [] | [_] -> None
	  | ts -> Some(Logic_const.new_predicate (Logic_const.pseparated ts))

  let emitter = 
    Emitter.create
      "Wp variable analysis"
      [ Emitter.Funspec ]
      ~correctness:[]
      ~tuning:[]
    
  let add_requires hyp kf = 
    (*[LC+JS]: This function does nothing if there is no default bhv (!) *)
    let spec = Annotations.funspec kf in
    Extlib.may
      (fun b -> Annotations.add_requires emitter kf b.b_name [ hyp ])
      (Cil.find_default_behavior spec)

  let kernel_functions_separation_hyps () =
    debug "[kf separation hyps]";
    Globals.Functions.iter 
      (fun kf ->
	 debug "[kf separation hyps] %s" (Kernel_function.get_name kf); 
	 let formals = Kernel_function.get_formals kf in 
	 let loc = Kernel_function.get_location kf in
	 match collect_refparams kf loc [] [] [] formals with
	 | Some hyp ->
	   debug "[kf separation hyps] case hyp:%a" 
	     Printer.pp_identified_predicate hyp;
	   add_requires hyp kf;
	 | None -> 
	   debug "[kf separation hyps] case None")
      
(**********************************************************************)
(*** Variable Anaylisis Computation                                 ***)
(**********************************************************************)

(* 
   If both optimization are required :

   Computation of the variable analysis; calls all visitors and resolution 
   in the good order, which is the order of their definitions in this file: 
   - Computation of address taken, collection of logic formal parameters, 
   preserved universally and existentially bound variables from the 
   optimization ; 
   - Identification of usages of formal parameters without 
   inspecting calls, application and ACSL builtin application 
   to dispatch them into formal parameters tables. 
   - Collection of calls according to the pattern of the effective 
   arguments and, then, updating the [ChainCalls] of the variables tables. 
   Collecting the occurences of address taken into a by reference pattern
   and updating the AddressTaken table.
   - Resolution of [ChainCalls], first in formal parameters tables, 
   secondly in other kind of variables tables. 
   - Resolution of address taken table.

   If only logicVar is required : 
   - Computation of addresse taken table.

*)


type case = 
  | All (* both optimizations are required*) 
  | Nothing (* none of the optimization are required *)
  | Half (* only logic var is required*)

(* Discrimination of the case of the analysis *)
(* [LC] Discrimination is now performed in Factory *)
let case_of_optimization ~logicvar ~refvar =
  if not logicvar then (if refvar then All else Nothing)
  else  (if refvar then All else Half)
  
let not_half_computed () = 
  not (AddrTaken.is_computed()) || not (LogicParam.is_computed())

let not_param_computed () = 
  not (ByValue.is_computed()) || 
    not (ByPReference.is_computed()) ||
    not (ByAReference.is_computed())

let not_arg_computed() = 
  not (ArgPReference.is_computed()) || not (ArgAReference.is_computed())

let not_computed () = 
  not_half_computed () && not_param_computed () && not_arg_computed ()
    

let compute () =
  match case_of_optimization ~logicvar:true ~refvar:false with
    | Nothing -> ()
    | Half ->
	if not_half_computed() then 
	  (debug
	     "[COMPUTE] DO address taken table computing";
	   compute_logic_params ())
	else ()
    | All ->
	if not_computed () then 
	  begin
	    debug "[COMPUTE] DO all table computation";
	    compute_calls_collection (); 
	    debug "[COMPUTE] DONE all table computation";
	    debug "[COMPUTE] DO resolution of formals calls";
	    resolved_call_chain_param (); 
	    debug 
	      "[COMPUTE] DONE resolution of formals calls";
	    debug 
	      "[COMPUTE] DO resolution of arguments chain calls";
	    resolved_call_chain_arg (); 
	    debug 
	      "[COMPUTE] DONE resolution of arguments chain calls";
	    debug 
	      "[COMPUTE] resolved address taken equation"; 
	    resolve_addr_taken ()
	  end	    
	else ()
	  
let dispatch_var var =   
  match case_of_optimization ~logicvar:true ~refvar:false with
    | Nothing -> Cvar 
    | Half -> 
	compute();
	if AddrTaken.mem var then Cvar else Fvar	   
    | All ->
	compute();
	if is_formal_var_type var then 
	  begin
	    if ByValue.mem var then
	      if AddrTaken.mem var then Cvar else Fvar
	    else
	      ( try let (n,_) = ByPReference.find var in PRpar n 
		with Not_found ->
		  (try let (n,_) = ByAReference.find var in ARpar n
		   with Not_found -> (* impossible case *) Cvar ))
	  end
	else
	  begin 
	    if AddrTaken.mem var then Cvar
	    else 
	      (if ArgAReference.mem var then ARarg 
	       else (if ArgPReference.mem var 
		     then PRarg else Fvar))
	  end
	    
let dispatch_cvar vinfo = dispatch_var (Cv vinfo)
let dispatch_lvar lv    = dispatch_var (Lv lv)

let is_user_formal_in_builtin lv = 
  try LogicParam.find lv with Not_found -> false 

let is_memvar case vinfo = 
  match case with
      | Nothing -> true
      | Half | All -> compute(); AddrTaken.mem (Cv vinfo)
   
let is_ref case vinfo = 
  match case with
    | Nothing -> false
    | Half  -> false 
    | All -> 
	compute();
	let cv = Cv vinfo in
	if vinfo.vformal then
	  (try fst (ByPReference.find cv) = 0 with Not_found -> false)
	else 
	  (try fst (ArgPReference.find cv) = 0 with Not_found -> false)
	    
let is_to_scope vinfo = 
  let case =  case_of_optimization ~logicvar:true ~refvar:false in
  is_ref case vinfo || is_memvar case vinfo
    
let precondition_compute () = 
  if (* Wp_parameters.RefVar.get () *) false then 
    begin
      compute ();
      kernel_functions_separation_hyps ()
    end
  else ()

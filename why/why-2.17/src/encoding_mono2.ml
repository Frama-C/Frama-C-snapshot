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

(*i $Id: encoding_mono2.ml,v 1.6 2008/11/05 14:03:17 filliatr Exp $ i*)

(** 
    Such encoding aims at simulating polymorphism in 
    multi-sorted logic. 
    There are basically three cases 
    1- For a term that is not of sort INT and 
    which is not used in a INT context, the translation 
    of the outermost symbol is the same as in strat
    2- For a term of sort INT and which is used in an INT 
    context, the outermost term is not modified 
    3- For a term of sort INT in a polymorphic context and 
    a term of an instantied sort which is used in an INT context 
    a special treatment is provided.    
*)






open Cc
open Logic
open Logic_decl
open Util 

let loc = Loc.dummy_floc

let prefix = "c_"
let suffix = "_c"
let var = "x"
let tvar = "t"
let cpt = ref 0
let axiomA c = (c^suffix)^ "_to_" ^ c
let axiomR c = (c^suffix)^ "_to_" ^ (c^suffix)
let def c = "def_"^c

let int2U = "int2U"
let ss2Int = "ss2Int"
let real2U = "real2U"
let ss2Real = "ss2Real" 
let bool2U = "bool2U"
let ss2Bool = "ss2Bool"
let unit2U = "unit2U"
let ss2Unit = "ss2Unit"
let arities = ref []




let prefixForTypeIdent ident = 
  "type_"^ident


let int = Tapp (Ident.create (prefix^"int"), [], []) 
let real = Tapp (Ident.create (prefix^"real"), [], [])
let bool = Tapp (Ident.create (prefix^"bool"), [], []) 
let unit = Tapp (Ident.create (prefix^"unit"), [], [])


(* Ground instanciation of an arity (to be plunged under) *)
let instantiate_arity id inst =
  let arity = 
    try List.assoc (Ident.string id) !arities
    with e -> (print_endline ("unknown arity :"^(Ident.string id))); raise e in
  let (vs, log_type) = Env.specialize_logic_type arity in
  match log_type with 
      Function (ptl, rt) ->
	ignore (Env.Vmap.fold 
		  (fun _ v l ->
		     (match l with [] -> [] 
			| _ -> (v.type_val <- Some (List.hd l); (List.tl l))))
		  vs (List.rev inst));
	rt
    | _ -> assert false
	

(**
   @return the list containing the type of the function/predicate parameters.
   This fucntion is called when we want to reduce the encoding of the function/predicate 
   parameter according to the type of these ones.
   @param id is the function/predicate name for which we look for the parameters 
**)
let getParamTypeList id =
  let arity = 
    try List.assoc (Ident.string id) !arities
    with e -> (print_endline ("unknown arity :"^(Ident.string id))); raise e in
  let (vs, log_type) = Env.specialize_logic_type arity in
  match log_type with 
      Function (ptl, _) -> ptl
    | Predicate ptl -> ptl


(**
   The unique type for all terms.
   this function is used everywhere we use a generic sort for the term
**) 
let ut = PTexternal ([], Ident.create (prefix^"unique"))


(**
   @param pt is the type we are going to translate.
   @return either the type if this one if a built in type (like int, float)
   or the type  ssorted
**)
let ssortedButBuiltIn  pt = match pt with 
    PTint as k -> k 
  | PTreal as k -> k
  | PTbool as k -> k
  | PTunit as k -> k
  | _ ->       PTexternal ([], Ident.create (prefix^"ssorted"))


(**
   @param pt is the type we are going to translate.
   @return either the type if this one if a built in type (like int, float)
   or the unique type define just above
**)
let utButBuiltIn  pt = match pt with 
    PTint as k -> k 
  | PTreal as k -> k
  | PTbool as k -> k
  | PTunit as k -> k
  | _ -> ut


(**
   @param pt is the type we are going to translate.
   @return either the type if this one if a built in type (like int, float)
   or the unique type define in the ut function. It is a shortcut since it allows 
   to create quickly a PureType. 
**)
let ssortedButBuiltInString  pt = match pt with 
    "PTint"  -> PTint 
  | "PTreal" -> PTreal
  | "PTbool" -> PTbool
  | "PTunit" -> PTunit
  | "type" -> PTexternal ([], Ident.create (prefix^"type"))
  | "ssorted" -> PTexternal ([], Ident.create (prefix^"ssorted"))
  | _ -> 
      (*Printf.printf " type : %s \n" pt ;  *)
      PTexternal ([], Ident.create (prefix^"type"))


(**
   @param ptl is the list of pure type 
   @return a list with the same size as ptl and whose only contains the 
   the type unique
**)
let unify ptl = List.map (fun _ -> ut) ptl

(**
   @param ptl is the list of pure type
   @return a list with the same size as ptl and whose only contains the 
   the type unique, but for built in type that are preserved
**)
let unifyPureType ptl = 
  List.map (fun pt -> ssortedButBuiltIn pt) ptl

(**
   @param ptl is the list of pure type, described as string  
   @return a list with the same size as ptl and whose only contains the 
   the type unique, but for built in type that are preserved
**)
let unifyString ptl = List.map (fun pt -> ssortedButBuiltInString pt) ptl

  

(**
   @return a forall predicate without trigger 
**)
let newForallTriggerFree is_wp x t p = 
  let n = Ident.bound x in 
  let s = Misc.subst_onev x n in
  let p = Misc.subst_in_predicate s p in
  Forall (is_wp, x, n, t, [], p) 


(**
   @return a forall predicate with triggers
   aiming at helping the prover
**)
let newForall is_wp x ty tr p = 
  let n = Ident.bound x  in
  let sp = Misc.subst_onev x n in
  let pp = Misc.subst_in_predicate sp p in
  let tt = Misc.subst_in_triggers sp tr in
  Forall (is_wp, x , n, ty, tt, pp) 


(**
   Axiomatizes the theory used in this encoding.
**) 
let prelude =
  (* The unique sort for not built-in types*)
  (Dtype (loc, [], prefix^"unique"))::
  (Dtype (loc, [], prefix^"ssorted"))::	
    (Dtype (loc, [], prefix^"type"))::
    (Dtype (loc, [], prefix^"Boolean"))::
    (Dtype (loc, [], prefix^"Unit"))::
    (Dlogic (loc, prefix^"sort", 
	     Env.empty_scheme (Function 
				 ([ssortedButBuiltInString "type" ; ut], 
				  ssortedButBuiltInString "ssorted")))):: 
    (Dlogic (loc, prefix^"Boolean_true",
	     Env.empty_scheme (Function ([], ssortedButBuiltIn PTbool)))):: 
    (Dlogic (loc, prefix^"Boolean_false",
	     Env.empty_scheme (Function ([], ssortedButBuiltIn PTbool)))):: 
    (** \forall b : bool b= true \lor b = false **)
    (Daxiom (loc, axiomR "either true_or_false",
	     let b = Ident.create "b" in 
	     let boolTrue =  Tapp (Ident.create (prefix^"Boolean_true"), [], []) in
	     let boolFalse = Tapp (Ident.create (prefix^"Boolean_false"), [], []) in 
	     let eqTrue  = Papp (Ident.t_eq, [boolTrue ; Tvar b], []) in 
	     let eqFalse  = Papp (Ident.t_eq, [boolFalse ; Tvar b], []) in 
	     let eq = Por (eqTrue, eqFalse) in 	     
	     Env.empty_scheme (newForallTriggerFree false  b  PTbool eq )))::
    (** true \neq false **)
    (Daxiom (loc, axiomR "true_neq_false",
	     let boolTrue =  Tapp (Ident.create (prefix^"Boolean_true"), [], []) in
	     let boolFalse = Tapp (Ident.create (prefix^"Boolean_false"), [], []) in 
    	     let neq = Papp (Ident.t_neq,[boolTrue;boolFalse], []) in 	     
	     Env.empty_scheme neq ))::
    
    (Dlogic (loc, int2U, 
	     Env.empty_scheme (Function ([ssortedButBuiltIn PTint], ut)))):: 
    (Dlogic (loc, ss2Int, 
	     Env.empty_scheme (Function ([ssortedButBuiltInString "ssorted"], 
					 ssortedButBuiltIn PTint))))::
    (Dlogic (loc, real2U, 
	     Env.empty_scheme (Function ([ssortedButBuiltIn PTreal], ut)))):: 
    (Dlogic (loc, ss2Real, 
	     Env.empty_scheme (Function ([ssortedButBuiltInString "ssorted"], 
					 ssortedButBuiltIn PTreal)))):: 
    (Dlogic (loc, bool2U, 
	     Env.empty_scheme (Function ([ssortedButBuiltIn PTbool], ut))))::
    (Dlogic (loc, ss2Bool, 
	     Env.empty_scheme (Function ([ssortedButBuiltInString "ssorted"], 
					 ssortedButBuiltIn PTbool)))):: 
    (Dlogic (loc, unit2U, 
	     Env.empty_scheme (Function ([ssortedButBuiltIn PTunit], ut))))::
    (Dlogic (loc, ss2Unit, 
	     Env.empty_scheme (Function ([ssortedButBuiltInString "ssorted"], 
					 ssortedButBuiltIn PTunit)))):: 
    (Dlogic (loc, prefix^"int",
	     Env.empty_scheme (Function ([], ssortedButBuiltInString "type")))):: 
    (Dlogic (loc, prefix^"bool", 
	     Env.empty_scheme (Function ([], ssortedButBuiltInString "type"))))::
    (Dlogic (loc, prefix^"real", 
	     Env.empty_scheme (Function ([], ssortedButBuiltInString "type"))))::
    (Dlogic (loc, prefix^"unit", 
	     Env.empty_scheme (Function ([], ssortedButBuiltInString "type"))))::
    (Dlogic (loc, prefix^"ref", 
	     Env.empty_scheme (Function ([ut], ut))))::
    (** \forall x,y: U, t : Typ .  sort(t,x) = sort(t,y) => x = y   **)
    (Daxiom (loc, axiomR prefix^"sort_is_injective",
	     let t = Ident.create "t" in 
 	     let x = Ident.create "x" in 
 	     let y = Ident.create "y" in 
	     let sort_t_x_ = Tapp (Ident.create (prefix^"sort"),
				  [Tvar t;Tvar x], []) in 
	     let sort_t_y_ = Tapp (Ident.create (prefix^"sort"),
				  [Tvar t;Tvar y], []) in 
	     let sort_t_x_Eq_sort_t_y = Papp (Ident.t_eq, [sort_t_x_; sort_t_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false, sort_t_x_Eq_sort_t_y, x_Eq_y) in 
	     let innerForally =  newForallTriggerFree false  y  ut  implication in 
	     let innerForallx =  newForallTriggerFree false  x  ut  innerForally in 
	     let outerForall =  newForallTriggerFree false  t  (ssortedButBuiltInString "type") innerForallx in 
             Env.empty_scheme outerForall))::
    (** \forall x: Int .  ss2Int(sort(int, int2U(x))) = x  (a) **)
    (Daxiom (loc, axiomR "eq_"^ss2Int^"_"^int2U,
 	     let x = Ident.create "x" in 
	     let int2u_x_ = Tapp (Ident.create int2U,
				  [Tvar x], []) in 
	     let sort_int_int2U_x_ = Tapp (Ident.create (prefix^"sort"),
					   [int; int2u_x_], []) in 
	     let ss2Intsort_int_int2U_x_ = Tapp (Ident.create ss2Int,
						[sort_int_int2U_x_], []) in
	     let peq = Papp (Ident.t_eq,[ss2Intsort_int_int2U_x_;Tvar x], []) in 
             Env.empty_scheme (newForall false  x  PTint [[TPat sort_int_int2U_x_]]peq )))::
    (** \forall x,y: Int .  int2U(x) = int2U(y) => x = y   **)
    (Daxiom (loc, axiomR int2U^"_is_injective",
 	     let x = Ident.create "x" in 
	     let int2u_x_ = Tapp (Ident.create int2U,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let int2u_y_ = Tapp (Ident.create int2U,
				  [Tvar y], []) in 
	     let int2u_x_Eq_int2u_y_ = Papp (Ident.t_eq, [int2u_x_; int2u_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   int2u_x_Eq_int2u_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  PTint  implication in 
	     let outerForall =  newForallTriggerFree false  x  PTint  innerForall in 
             Env.empty_scheme outerForall))::
(** \forall x,y: Real .  real2U(x) = real2U(y) => x = y   **)
    (Daxiom (loc, axiomR real2U^"_is_injective",
 	     let x = Ident.create "x" in 
	     let real2u_x_ = Tapp (Ident.create real2U,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let real2u_y_ = Tapp (Ident.create real2U,
				  [Tvar y], []) in 
	     let real2u_x_Eq_real2u_y_ = Papp (Ident.t_eq, [real2u_x_; real2u_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   real2u_x_Eq_real2u_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  PTreal  implication in 
	     let outerForall =  newForallTriggerFree false  x  PTreal  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x,y: Bool .  bool2U(x) = bool2U(y) => x = y   **)
    (Daxiom (loc, axiomR bool2U^"_is_injective",
 	     let x = Ident.create "x" in 
	     let bool2u_x_ = Tapp (Ident.create bool2U,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let bool2u_y_ = Tapp (Ident.create bool2U,
				  [Tvar y], []) in 
	     let bool2u_x_Eq_bool2u_y_ = Papp (Ident.t_eq, [bool2u_x_; bool2u_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   bool2u_x_Eq_bool2u_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  PTbool  implication in 
	     let outerForall =  newForallTriggerFree false  x  PTbool  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x,y: ssorted .  ss2Int(x) = ss2Int(y) => x = y   **)
    (Daxiom (loc, axiomR ss2Int^"_is_injective",
 	     let x = Ident.create "x" in 
	     let ss2int_x_ = Tapp (Ident.create ss2Int,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let ss2int_y_ = Tapp (Ident.create ss2Int,
				  [Tvar y], []) in 
	     let ss2int_x_Eq_ss2int_y_ = Papp (Ident.t_eq, [ss2int_x_; ss2int_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   ss2int_x_Eq_ss2int_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  (ssortedButBuiltInString "ssorted")  implication in 
	     let outerForall =  newForallTriggerFree false  x  (ssortedButBuiltInString "ssorted")  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x,y: U .  ss2Real(x) = ss2Real(y) => x = y   **)
    (Daxiom (loc, axiomR ss2Real^"_is_injective",
 	     let x = Ident.create "x" in 
	     let ss2real_x_ = Tapp (Ident.create ss2Real,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let ss2real_y_ = Tapp (Ident.create ss2Real,
				  [Tvar y], []) in 
	     let ss2real_x_Eq_ss2real_y_ = Papp (Ident.t_eq, [ss2real_x_; ss2real_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   ss2real_x_Eq_ss2real_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  (ssortedButBuiltInString "ssorted")  implication in 
	     let outerForall =  newForallTriggerFree false  x  (ssortedButBuiltInString "ssorted")  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x,y: U .  ss2Bool(x) = ss2Bool(y) => x = y   **)
    (Daxiom (loc, axiomR ss2Bool^"_is_injective",
 	     let x = Ident.create "x" in 
	     let ss2bool_x_ = Tapp (Ident.create ss2Bool,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let ss2bool_y_ = Tapp (Ident.create ss2Bool,
				  [Tvar y], []) in 
	     let ss2bool_x_Eq_ss2bool_y_ = Papp (Ident.t_eq, [ss2bool_x_; ss2bool_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   ss2bool_x_Eq_ss2bool_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  (ssortedButBuiltInString "ssorted")  implication in 
	     let outerForall =  newForallTriggerFree false  x  (ssortedButBuiltInString "ssorted")  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x,y: U .  ss2Unit(x) = ss2Unit(y) => x = y   **)
    (Daxiom (loc, axiomR ss2Unit^"_is_injective",
 	     let x = Ident.create "x" in 
	     let ss2unit_x_ = Tapp (Ident.create ss2Unit,
				  [Tvar x], []) in 
 	     let y = Ident.create "y" in 
	     let ss2unit_y_ = Tapp (Ident.create ss2Unit,
				  [Tvar y], []) in 
	     let ss2unit_x_Eq_ss2unit_y_ = Papp (Ident.t_eq, [ss2unit_x_; ss2unit_y_], []) in
	     let x_Eq_y = Papp (Ident.t_eq, [Tvar x; Tvar y], []) in
	     let implication = Pimplies (false,   ss2unit_x_Eq_ss2unit_y_, x_Eq_y) in 
	     let innerForall =  newForallTriggerFree false  y  (ssortedButBuiltInString "ssorted")  implication in 
	     let outerForall =  newForallTriggerFree false  x  (ssortedButBuiltInString "ssorted")  innerForall in 
             Env.empty_scheme outerForall))::
    (** \forall x:Real .  ss2Real(sort(real, real2U(x))) = x  (b) **)
    (Daxiom (loc, axiomR "eq_"^ss2Real^"_"^real2U,
	     let x = Ident.create "x" in 
	     let real2u_x_ = Tapp (Ident.create real2U,
				   [Tvar x], []) in 
	     let sort_real_real2U_x_ = Tapp (Ident.create (prefix^"sort"),
					     [real; real2u_x_], []) in 
	     let ss2Realsort_real_real2U_x_ = Tapp (Ident.create ss2Real,
						   [sort_real_real2U_x_], []) in
	     let peq = Papp (Ident.t_eq,[ss2Realsort_real_real2U_x_;Tvar x], []) in 
             Env.empty_scheme (newForall false  x  PTreal [[TPat sort_real_real2U_x_]]peq )))::
    (** \forall x:Bool .  ss2Bool(sort(bool, bool2U(x))) = x  **)
    (Daxiom (loc, axiomR "eq_"^ss2Bool^"_"^bool2U,
	     let x = Ident.create "x" in 
	     let bool2u_x_ = Tapp (Ident.create bool2U,
				   [Tvar x], []) in 
	     let sort_bool_bool2U_x_ = Tapp (Ident.create (prefix^"sort"),
					     [bool; bool2u_x_], []) in 
	     let ss2Boolsort_bool_bool2U_x_ = Tapp (Ident.create ss2Bool,
						   [sort_bool_bool2U_x_], []) in
	     let peq = Papp (Ident.t_eq,[ss2Boolsort_bool_bool2U_x_;Tvar x], []) in 
             Env.empty_scheme (newForall false  x  PTbool [[TPat sort_bool_bool2U_x_]]peq )))::
    (** \forall x:Unit .  ss2Unit(sort(unit, unit2U(x))) = x  **)
    (Daxiom (loc, axiomR "eq_"^ss2Unit^"_"^unit2U,
	     let x = Ident.create "x" in 
	     let unit2u_x_ = Tapp (Ident.create unit2U,
				   [Tvar x], []) in 
	     let sort_unit_unit2U_x_ = Tapp (Ident.create (prefix^"sort"),
					     [unit; unit2u_x_], []) in 
	     let ss2Unitsort_unit_unit2U_x_ = Tapp (Ident.create ss2Unit,
						   [sort_unit_unit2U_x_], []) in
	     let peq = Papp (Ident.t_eq,[ss2Unitsort_unit_unit2U_x_;Tvar x], []) in 
             Env.empty_scheme (newForall false  x  PTunit [[TPat sort_unit_unit2U_x_]]peq )))::
    (** \forall x. U  int2U(ss2Int(sort(int,x))) = x **)
    (Daxiom (loc, axiomR "eq_"^int2U^"_"^ss2Int,
	     let x = Ident.create "x" in 
	     let sort_int_x_ = Tapp (Ident.create (prefix^"sort"),
				     [int; Tvar x], []) in
	     let ss2Int_sort_int_x_ = Tapp (Ident.create ss2Int,
					   [sort_int_x_], []) in 
	     let int2U_ss2Int_sort_int_x_ = Tapp (Ident.create int2U,
						 [ss2Int_sort_int_x_], []) in
	     let peq = Papp (Ident.t_eq,
			     [int2U_ss2Int_sort_int_x_;Tvar x], [])
	     in
	     Env.empty_scheme (newForall false  x ut [[TPat ss2Int_sort_int_x_]]peq )))::
    (** \forall x. U  real2U(ss2Real(sort(real,x))) = x **)
    (Daxiom (loc, axiomR "eq_"^real2U^"_"^ss2Real,
	     let x = Ident.create "x" in 
	     let sort_real_x_ = Tapp (Ident.create (prefix^"sort"),
				      [real; Tvar x], []) in
	     let ss2Real_sort_real_x_ = Tapp (Ident.create ss2Real,
					     [sort_real_x_], []) in 
	     let real2U_ss2Real_sort_real_x_ = Tapp (Ident.create real2U,
						    [ss2Real_sort_real_x_], []) in
	     let peq = Papp (Ident.t_eq,
			     [real2U_ss2Real_sort_real_x_;Tvar x], [])
	     in
	     Env.empty_scheme (newForall false  x ut [[TPat ss2Real_sort_real_x_]]peq )))::
    (** \forall x. U  bool2U(ss2Bool(sort(bool,x))) = sort(bool,x) **)
    (Daxiom (loc, axiomR "eq_"^bool2U^"_"^ss2Bool,
	     let x = Ident.create "x" in 
	     let sort_bool_x_ = Tapp (Ident.create (prefix^"sort"),
				      [bool; Tvar x], []) in
	     let ss2Bool_sort_bool_x_ = Tapp (Ident.create ss2Bool,
					     [sort_bool_x_], []) in 
	     let bool2U_ss2Bool_sort_bool_x_ = Tapp (Ident.create bool2U,
						    [ss2Bool_sort_bool_x_], []) in
	     let peq = Papp (Ident.t_eq,
			     [bool2U_ss2Bool_sort_bool_x_;Tvar x], [])
	     in
	     Env.empty_scheme (newForall false  x ut [[TPat bool2U_ss2Bool_sort_bool_x_]]peq )))::
    (** \forall x. U  unit2U(ss2Unit(sort(unit,x))) = sort(unit,x) **)
    (Daxiom (loc, axiomR "eq_"^unit2U^"_"^ss2Unit,
	     let x = Ident.create "x" in 
	     let sort_unit_x_ = Tapp (Ident.create (prefix^"sort"),
				      [unit; Tvar x], []) in
	     let ss2Unit_sort_unit_x_ = Tapp (Ident.create ss2Unit,
					     [sort_unit_x_], []) in 
	     let unit2U_ss2Unit_sort_unit_x_ = Tapp (Ident.create unit2U,
						    [ss2Unit_sort_unit_x_], []) in
	     let peq = Papp (Ident.t_eq,
			     [unit2U_ss2Unit_sort_unit_x_;Tvar x], [])
	     in
	     Env.empty_scheme (newForall false  x ut [[TPat unit2U_ss2Unit_sort_unit_x_]]peq )))::
    []



(** internal function used in  plunge and typedPlunge.
    it recursively translates  the type given in parameter 
    if it is a built-in type, a constant corresponding to this type is generated
    if it is a type variable, a variable corresponding to this type is generated
    @pt is the type
    @fv is list of (tag,typeVar) of the problem
**)
let rec leftt pt fv=
  match pt with
      PTint -> Tapp (Ident.create (prefix^"int"), [], [])
    | PTbool -> Tapp (Ident.create (prefix^"bool"), [], [])
    | PTreal -> Tapp (Ident.create (prefix^"real"), [], [])
    | PTunit -> Tapp (Ident.create (prefix^"unit"), [], [])
    | PTvar ({type_val = None} as var) -> 
	(*let s = string_of_int var.tag in *)
	let t = try (List.assoc var.tag fv)
	with _ ->
	  let s = string_of_int var.tag in
	  (print_endline ("unknown vartype : "^s); Ident.create "dummy") in
	(*print_endline ("Vartype : "^s^" :"^(Ident.string t)); *)
	Tvar t
    | PTvar {type_val = Some pt} -> leftt pt fv 
    | PTexternal (ptl, id) -> Tapp ( Ident.create (prefixForTypeIdent (Ident.string id)), List.map (fun pt -> leftt pt fv) ptl, [])


(**@return a Tapp that encapsulates a term t with sort(..., t) 
   by calling to lefft
   @param fv is the list of type variable of the problem 
   @param term is the term we want to encapsulate
   @param pt is the type of the term
**)
let plunge fv term pt =
  Tapp (Ident.create (prefix^"sort"),
	[leftt pt fv; term],
	[])
    
(**@return a Tapp that encapsulates a term t with sort(...,f(t))
   where f is a casting function from built-in type to the unique sort
   sinc the second parameter of sort is of type unique.
   @param fv is the list of type variable of the problem 
   @param term is the term we want to encapsulate
   @param pt is the type of the term
**)
let typedPlunge fv term pt =
  (** internal function that accomplish the casting task **)
  let cast pt term = 
    match pt with
	PTint -> Tapp (Ident.create int2U,  [term], [])
      | PTbool -> Tapp (Ident.create bool2U, [term], [])
      | PTreal -> Tapp (Ident.create real2U, [term], [])
      | PTunit -> Tapp (Ident.create unit2U, [term], [])
      | _ -> term 
  in
  let l =  Tapp (Ident.create (prefix^"sort"),
	[leftt pt fv; cast pt term],
	[]) in 
  l

    



(**
   @return the type of a term given in parameter
   @param term is the term we want the type
   @param lv is the list of (var,type) of the problem 
**)
let rec getTermType term lv = 
  (*Format.eprintf 
    "getTermType  : %a \n List " 
    Util.print_term term ;
  List.iter (fun (id,ty) -> 
	       Format.eprintf 
		 "(%s ,%a) " 
    (Ident.string id)
		 Util.print_pure_type ty
    ) lv; *)
  match term with
    | Tvar id -> 
	(* let l = List.map (fun (id,x) -> Ident.string id,x) lv in
	let associatedType = 
	  (try List.assoc (Ident.string id) l
	   with e ->  
	     Format.eprintf 
	       "Exception 2 Caught In getTermType  : %s" 
	       (Ident.string id);   
	     raise e) in*)
	let associatedType = 
	(try List.assoc id lv 
	 with e ->  
	   Format.eprintf 
	     "Exception Caught In getTermType  : %s" 
	     (Ident.string id);   
	   raise e) in
      associatedType
  | Tapp (id, l, inst) when (Ident.is_int_arith id) ->
      PTint
  | Tapp (id, l, inst) when (Ident.is_real_arith id) ->
      PTreal
  | Tapp (id, l, inst)  ->  instantiate_arity id inst
  | Tconst (ConstInt _)  ->  PTint
  | Tconst (ConstBool _)  -> PTbool
  | Tconst (ConstUnit)  ->  PTunit
  | Tconst (ConstFloat f) ->  PTreal
  | Tderef _ -> assert false
  | Tnamed(_,t) ->  getTermType t lv 

let isBuiltInType t= match t with 
    PTint | PTreal | PTbool  | PTunit -> true
  | _ -> false




(** Translate  of a term 
    @param fv is the list of type variables of the problem 
    @param lv is the list of free  variables of the term
    @param term is the term we want to encapsulate
    @param pt is the type of the term
    
**)
let rec translate_term fv lv term doTheCast= 
  let rec translate fv lv term = match term with 
    | Tnamed(_,t) -> translate fv lv t
    | Tvar id as t -> 
        let t = typedPlunge fv  (Tvar id) (getTermType t lv) in 
	t
    | Tapp (id, tl, inst) when Ident.is_simplify_arith id ->
	(** This function yields a numeric parameter 
	    we have to cast the result into the u type **)
 	let outermostCast = if Ident.is_real_arith id then  real2U 
	else int2U in
	let inner = Tapp(Ident.create outermostCast, 
			 [Tapp(id, List.map 
				 (** each parameter of this function is a built-in
				     We cast it to respect this constraint **)
				 (fun t -> translateParam  fv lv t true) tl, [])], 
			 []) in 
	let innerCast = if ( outermostCast= real2U) then real else int in 
	Tapp(Ident.create (prefix^"sort"), [innerCast; inner],[])
    | Tapp (id, tl, inst) ->
	(** it is not a arithmetic function **)
	(** retrieves the  function signature **)
	let paramList = ref (getParamTypeList id) in
	(** translates the parameter wrt the function signature **)
	let translateParameter t  = match !paramList with 
	    pt::tl ->
	      paramList := tl ;
	      translateParam fv lv t (isBuiltInType pt) 
	  | [] -> assert false
	in
	(** eventually encapsulates the function **)
	let t = typedPlunge fv 
	  (Tapp (id, 
		 List.map 
		   (fun t -> translateParameter t) tl, 
		 []))
	  (instantiate_arity id inst) in 
	(*Format.printf "term: %a\n" Util.print_term ter ; *)
	t
    | Tconst (ConstInt _) as t -> typedPlunge fv  t PTint
    | Tconst (ConstBool _) as t -> typedPlunge fv t PTbool
    | Tconst (ConstUnit) as t -> typedPlunge [] t PTunit
    | Tconst (ConstFloat f) as t -> typedPlunge fv t PTreal
    | Tderef id as t -> print_endline ("id in Tderef : "^(Ident.string id)); t 
  in
  if doTheCast then   
    match (getTermType term lv) with 
	PTint  ->
 	  Tapp(Ident.create ss2Int, [translate fv lv term],[])
      | PTreal  ->
 	  Tapp(Ident.create ss2Real, [translate fv lv term],[])
      | PTbool -> 
	  Tapp(Ident.create ss2Bool, [translate fv lv term],[])
      | PTunit -> 
	  Tapp(Ident.create ss2Unit, [translate fv lv term],[])
      | _   ->
	  (** if the type is not pure, i.e. it is deduced syntactically **)
	  let term'= translate fv lv term  in 
	    match term' with 
		Tapp (id, [sortTerm;_], _) when 
		  Ident.string id = prefix^"sort" -> 
		    begin
		      match sortTerm with       
			  Tapp (k, [], [])  when 
			    Ident.string k = prefix^"int" ->  
			      Tapp(Ident.create ss2Int, [term'],[])
		      	| Tapp (k, [], [])  when 
			    Ident.string k = prefix^"real" -> 
			    Tapp(Ident.create ss2Real, [term'],[])
			| Tapp (k, [], [])  when 
			    Ident.string k = prefix^"bool"->  
			    Tapp(Ident.create ss2Bool, [term'],[])
			| _ (* Tapp (k, [], [])  when  
			    Ident.string k = prefix^"unit" *)->  
			    Tapp(Ident.create ss2Unit, [term'],[])
(* 			| _ as t -> *)
(* 			    Format.printf ("tt :  %a \n ") print_term t ;  *)
(* 			    assert false *)
		    end
	      | _ -> assert false
  else
    translate fv lv term 




(**
   @return a term generated by a classical translation  
   composed with a reduction that  directly applies the axioms 
   (a) and (b).
   @param fv is the list of type variables of the problem 
   @param lv is the list of variables of the term
   @param t is the term we have to translate
   @param isArith the a boolean flag used in the clasical translate_term 
   function 
**)
and translateParam fv lv t doTheCast =
  let tp = translate_term fv lv t doTheCast in
  match tp with 
      Tapp(id1, [ti],_) when 
	Ident.string id1 = ss2Int ->
	  begin 
	    match ti with       
		Tapp(id2, [_;tk],_) when 
		  Ident.string id2 = prefix^"sort" -> 
		    begin
		      match tk with       
			  Tapp(id3, [tl],_) when 
			    Ident.string id3 = int2U -> 
			      tl
			| _ -> tp
		    end
	      | _ -> tp 
	  end
    | Tapp(id1, [ti],_) when 
	Ident.string id1 = ss2Real ->
	begin 
	  match ti with       
	      Tapp(id2, [_;tk],_) when 
		Ident.string id2 = prefix^"sort" -> 
		  begin
		    match tk with       
			Tapp(id3, [tl],_) when 
			  Ident.string id3 = real2U -> 
			    tl
		      | _ -> tp
		  end
	    | _ -> tp 
	end
    | Tapp(id1, [ti],_) when 
	Ident.string id1 = ss2Bool ->
	begin 
	  match ti with       
	      Tapp(id2, [_;tk],_) when 
		Ident.string id2 = prefix^"sort" -> 
		  begin
		    match tk with       
			Tapp(id3, [tl],_) when 
			  Ident.string id3 = bool2U -> 
			    tl
		      | _ -> tp
		  end
	    | _ -> tp 
	end
    | _ -> tp


(**
   @param fv is the list of type variables of the problem 
   @param lv is the list of variables of the term
   @param t is the term we have to translate

**)
and literalParam fv lv t =
  let tp = translate_term fv lv t false in
  match tp with 
      Tapp(id, [_;tk],_) when 
	Ident.string id = prefix^"sort" ->  tk
    | _ -> tp 


let rec reduceEquality t1 t2 =
 match (t1,t2) with 
     (Tapp(id1, [tk1],_),Tapp(id2, [tk2],_)) when 
       (Ident.string id1 = Ident.string id2 &&
	   (id1 = Ident.create ("int2U") ||
	       id1 = Ident.create ("ss2Int") ||
	       id1 = Ident.create ("bool2U") ||
	       id1 = Ident.create ("ss2Bool") ||
	       id1 = Ident.create ("real2U") ||
	       id1 = Ident.create ("ss2Real") 
	   ))-> reduceEquality tk1 tk2
   | (Tapp(id1, [_;tk1],_),Tapp(id2, [_;tk2],_)) when 
       (id1 = Ident.create (prefix^"sort") && Ident.string id1 = Ident.string id2) 
       -> reduceEquality tk1 tk2
   | _-> t1::t2::[]


(**
   @return a predicate that is universally quantified 
   with the top most variable present in the list l. 
   These varaible are of sort type. 
   When it is the last variable into the list, it adds trigger
   When the type variable list is empty, it returns only the predicate  
   It is called when we build an axiom at the outermost position
   @param l is a list of type variables 
   @param p is the predicate we want to add quantifiers 
   @param t is the triggers 
**)
let rec lifted  l p t =  
  match l with [] -> p
    | (_, s)::[] ->
	newForall false s (ssortedButBuiltInString "type") t p
	  
    | (_, s)::q -> 
	newForallTriggerFree false s (ssortedButBuiltInString "type") (lifted q p t)
	  
(**
   @return a predicate that is universally quantified 
   with the top most variable present in the list l.
   When it is the last variable into the list, it adds trigger
   When the variable list is empty, it returns only the predicate  
   It is called when we build an axiom to quantify the free variables 
   @param l is a list of variables (a,t) where 
   a is the variable identifier and t is its type. 
   @param p is the predicate we want to add quantifiers 
   @param t is the triggers 
**)
let rec lifted_t l p tr =
  match l with [] -> p
    | (a,t)::[] -> 
	newForall false a t tr p    
    | (a,t)::q -> 
	newForallTriggerFree false  a t  (lifted_t q p tr)

let lifted_ctxt l cel =
  (List.map (fun (pt,s) -> Svar(s, PTexternal ([], Ident.create (prefix^"type")))) l)@cel






(** 
    @return true if all the element of tl are of sort Int or Real
    (and not sort (int,...) i.e. of sort u)
    @param tl is the list of terms we analyse
    @param lv is the list of free varaiables in the terms list
**)
let rec getEqualityType tl lv =
  let ret = ref PTbool in 
  (fun tl -> match tl with 
       [] -> !ret 
     | hd::l -> 
	 begin 
	   match hd with 
	       Tapp (id, _, _) when Ident.is_int_arith id -> 
		 ret:= PTint ; 
		 getEqualityType l lv 
	     | Tapp (id, _, _) when Ident.is_real_arith id -> 
		 ret:= PTint ; 
		 getEqualityType l lv 
	     | Tapp (id, _, inst) ->
		 begin
		   let pt = (instantiate_arity id inst) in
		   match pt  with 
		       PTint  ->
			 ret:= PTint ;
			 getEqualityType l lv 
		     | PTreal -> 
			 ret:= PTreal ; 
			 getEqualityType l lv 
		     | PTbool -> 
			 ret:= PTbool ; 
			 getEqualityType l lv
		     | PTunit ->
			 ret := PTunit ;
			 getEqualityType l lv
		     | _ ->  pt
		 end
	     | Tconst (ConstInt _) ->
		 ret:= PTint ; 
		 getEqualityType l lv 
             | Tconst (ConstFloat _) -> 
		 ret:= PTreal ; 
		 getEqualityType l lv 
	     | Tconst (ConstBool _) ->
		 ret:= PTbool ; 
		 getEqualityType l lv 
	     | Tconst ConstUnit ->
		 ret:= PTunit ; 
		 getEqualityType l lv
	     | Tvar id as t-> 
		 begin 
		   let pt = (try List.assoc id lv
			     with e ->  
			       Format.eprintf 
				 "Exception Caught In getEqualityType  : %a" 
				 Util.print_term t ; 
			       raise e) in 
		   match pt  with 
		       PTint  ->
			 ret:= PTint ; 
			 getEqualityType l lv 
		     | PTreal -> 
			 ret:= PTreal ; 
			 getEqualityType l lv 
		     | PTbool -> 
			 ret:= PTbool ; 
			 getEqualityType l lv 
		     | PTunit ->
			 ret:= PTunit ; 
			 getEqualityType l lv 
		     | _ ->  pt
			 
		 end
	     | _ -> assert false 
	 end) tl  
    

(** 
    @param fv is the list of type variables of the problem 
    @param p is the predicate we have to translate
    @param lv is the list of free variables in the predicate 
**)
let rec translate_pred fv lv p  = match p with
  | Papp (id, tl, inst) when  
      (Ident.is_eq id && ( Ident.is_int_comparison id ||
	  Ident.is_real_comparison id ))->
      let lt = List.map (fun t-> translateParam fv lv t true) tl in 
      begin
	match lt with 
	    [t1;t2] -> Papp (id, reduceEquality t1 t2, [])   
	  | _ -> 	  Papp (id,lt, [])   
      end
  | Papp (id, tl, inst) when  
      ( Ident.is_int_comparison id ||
	  Ident.is_real_comparison id )->
      Papp (id,List.map (fun t-> translateParam fv lv t true) tl, [])   
  | Papp (id, tl, inst) when  
      (Ident.is_eq id || Ident.is_neq id) ->
      let lt = List.map (fun t-> literalParam fv lv t) tl in 
      begin
	match lt with 
	    [t1;t2] -> Papp (id, reduceEquality t1 t2, [])   
	  | _ -> 	  Papp (id,lt, [])   
      end
  | Papp (id, [a;b], _) when id==Ident.t_zwf_zero ->  
       (* 0 <= a *)
      let aLeftBound = (Papp (Ident.t_le_int,
			      [Tconst(ConstInt("0"));
			       (translateParam fv lv a true)],[PTint;PTint])) in 
      (* a < b *)
      let aRightBound = Papp (Ident.t_lt_int,
			      [(translateParam fv lv a true);
			       (translateParam fv lv b true)],[PTint;PTint]) in  
      (* 0 <= a  &  a < b *)
      Pand(false, false, aLeftBound,aRightBound)
  | Papp (id, tl, inst) ->
      (** retrieves the predicate signature **) 
      let paramList = ref (getParamTypeList id) in
      let translateParameter t  = match !paramList with 
	  pType::tl  ->
	    paramList := tl ;
	    translateParam fv lv t (isBuiltInType pType) 
	| [] -> assert false
      in	
      Papp (id, 
	    List.map 
	      (fun t -> translateParameter t) tl, 
	    [])
  | Pimplies (iswp, p1, p2) ->
      Pimplies (iswp, translate_pred fv lv p1, translate_pred fv lv p2)
  | Pif (t, p1, p2) ->
      Pif (translate_term fv lv t true, translate_pred fv lv p1, translate_pred fv lv p2)
  | Pand (iswp, issym, p1, p2) ->
      Pand (iswp, issym, translate_pred fv lv p1, translate_pred fv lv p2)
  | Por (p1, p2) ->
      Por (translate_pred fv lv p1, translate_pred fv lv p2)
  | Piff (p1, p2) ->
      Piff (translate_pred fv lv p1, translate_pred fv lv p2)
  | Pnot p ->
      Pnot (translate_pred fv lv p)
  | Forall (iswp, id, n, pt, tl, p) ->
      let lv' = (n,pt)::lv in
      let tl' = List.map (List.map (translate_pattern fv lv')) tl in
      Forall (iswp, id, n, utButBuiltIn pt, tl', translate_pred fv lv' p)
  | Forallb (iswp, p1, p2) ->
      Forallb (iswp, translate_pred fv lv p1, translate_pred fv lv p2)
  | Exists (id, n, pt, p) ->
      Exists (id, n, utButBuiltIn pt, translate_pred fv ((n,pt)::lv) p)
  | Pnamed (s, p) ->
      Pnamed (s, translate_pred fv lv p)
  | _ as d -> d 

and translate_pattern fv lv = function
  | TPat t -> TPat (translate_term fv lv t false)
  | PPat p -> PPat (translate_pred fv lv p)


(* The core *)
let queue = Queue.create ()



let bound_variable =
  let count = ref 0 in
  function n ->  
    count := !count+1 ;
    Ident.create (n^"_"^ (string_of_int !count))


let rec push d = 
  try (match d with
	   (* A type declaration is translated as new logical function, the arity of *)
	   (* which depends on the number of type variables in the declaration *)
	 | Dtype (loc, vars, ident) ->
	     Queue.add (Dlogic (loc, (prefixForTypeIdent ident), 
				Env.empty_scheme (Function (unifyString vars, ssortedButBuiltInString "type")))) queue
	       (* For arithmetic symbols, another encoding is used (see encoding_rec.ml) *)
	 | Dlogic (loc, ident, arity) when Ident.is_simplify_arith (Ident.create ident) ->
	     arities := (ident, arity)::!arities;
	     (* with type u, and its complete arity is stored for the encoding *)
	 | Dlogic (loc, ident, arity) -> 
	     arities := (ident, arity)::!arities;
	     let newarity = match arity.Env.scheme_type with
		 Predicate ptl -> Predicate (unifyPureType ptl)
	       | Function (ptl, rt) -> Function (unifyPureType ptl, utButBuiltIn rt) in
	     
	     Queue.add (Dlogic (loc, ident,
				Env.empty_scheme newarity)) queue
	       (* A predicate definition can be handled as a predicate logic definition + an axiom *)
	 | Dpredicate_def (loc, ident, pred_def_sch) ->
	     let (argl, pred) = pred_def_sch.Env.scheme_type in
	     let rootexp = (Papp (Ident.create ident, List.map (fun (i,_) -> Tvar i) argl, [])) in
	     push (Dlogic (loc, ident, (Env.generalize_logic_type (Predicate (snd (List.split argl))))));
	     let pred_scheme = (lifted_t argl (Piff (rootexp, pred)) [[PPat rootexp]]) in
	     push (Daxiom (loc, def ident, 
			   (Env.generalize_predicate 
			      pred_scheme)))
	       (* A function definition can be handled as a function logic definition + an axiom *)
	 | Dfunction_def (loc, ident, fun_def_sch) ->
	     let _ = print_endline ident in
	     let (argl, rt, term) = fun_def_sch.Env.scheme_type in
	     let rootexp = (Tapp (Ident.create ident, List.map (fun (i,_) -> Tvar i) argl, [])) in
	     push (Dlogic (loc, ident, (Env.generalize_logic_type (Function (snd (List.split argl), rt)))));
	     let pred_scheme = (Env.generalize_predicate
				  (lifted_t argl 
				     (Papp (Ident.t_eq, [rootexp; term], [])) 
				     [[TPat rootexp]])) in 
	     push (Daxiom (loc, def ident,pred_scheme))
	       (* Axiom definitions *)
	 | Daxiom (loc, ident, pred_sch) ->
	     let fv = Env.Vset.fold
	       (fun tv acc -> 
		  (tv.tag, (bound_variable tvar) )::acc)
	       (pred_sch.Env.scheme_vars) [] in
	     let new_pred = (translate_pred fv [] pred_sch.Env.scheme_type) in 
	     let pred' = lifted fv new_pred [] in 
	     let new_axiom =
	       Env.empty_scheme (pred') in
	     Queue.add (Daxiom (loc, ident, new_axiom)) queue
	       (* A goal is a sequent : a context and a predicate and both have to be translated *)
	 | Dgoal (loc, expl, ident, s_sch) ->
	     let fv = Env.Vset.fold
	       (fun tv acc -> 
		  (tv.tag,  (bound_variable tvar))::acc)
	       (s_sch.Env.scheme_vars) [] in
	     (* function for the following fold_left *)
	     let f  (acc_c, acc_new_cel) s = match s with
		 Spred (id, p) -> (acc_c, 
				   (Spred (id, translate_pred fv acc_c p))::acc_new_cel)
	       | Svar (id, t)  -> 
		   ((id,t)::acc_c, (Svar (id, utButBuiltIn t))::acc_new_cel) in
	     (* list of hyps for the following fold_left *)
	     let l = fst s_sch.Env.scheme_type in
	     
	     let (context, new_cel) =  List.fold_left 	f ([], []) l in
	     let ctxt = lifted_ctxt fv (List.rev new_cel) in 
	     let pred' =  translate_pred fv context (snd (s_sch.Env.scheme_type)) in 
	     (*Format.printf "%a" Util.print_predicate pred' ; *)
	     let new_sequent =
	       Env.empty_scheme
		 (ctxt,pred'
		  ) in
	     let temp = Dgoal (loc, expl, ident, new_sequent) in
	     Queue.add temp queue 
      )
  with
      Not_found -> 
	Format.eprintf "Exception Caught In Push : %a\n" Util.print_decl d;
	raise Not_found

let iter f =
  (* first the prelude *)
  List.iter f prelude;
  (* then the queue *)
  Queue.iter f queue

let reset () = 
  arities := [];
  Queue.clear queue;
  (*List.iter push arith_kernel*)
  


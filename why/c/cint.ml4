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

(* $Id: cint.ml4,v 1.24 2008/02/05 12:10:47 marche Exp $ *)

(* TO DO:

   - document that inequalities like [x > 1 => x > 2] are not representable
   with constrained octogons

   - treat test [p != 0] as constraint on [arrlen(p)]
   
   - take into account remaining pointer arithmetic to invalidate arrlen

   - transfer for backward propagation should do nothing for Context part

   - replace complex type contraints in module by adding Map argument
   to functors

   - treat allocations in transfer function

   - improve on [guarantee_test] for Make_PointWiseFromAtomic
     allow backward prop for pointwise lattice

   - replace [with module VV = V] with [with module V = V] does not work !

   - implement a real variable packing.

   - make octogon analysis use result of interval analysis, printing
   result only if more precise than previously known result.
   (use "diff" printing)

   - see if adding +/- infty make things simpler
  
   - [transform] introduces blocks. remove them.

   - identify how true and false are encoded: 1 == 1 ? 0 == 1 ?
   This could improve on the analysis.

   - define the conditions under which a logical variable like [arrlen(p)] or
   [strlen(p)] is valid and has the expected meaning w.r.t. the annotations
   from the user.

   - take into account possible non-initialization

   - take into account possible overflow

*)

open Info
open Clogic
open Cast
open Cutil
open Cabsint
open Csymbol
open Pp

let debug = Coptions.debug
let debug_more = false


(*****************************************************************************
 *                                                                           *
 * 		Signatures for integer lattices                              *
 *                                                                           *
 *****************************************************************************)

(* elements of signature to add to [SEMI_LATTICE] and [LATTICE] to make them
   integer semi-lattice and lattice signatures *)

module type INT_DELTA = sig
  (* type of variable associated with a value in this lattice. Used during
     the generation of an equivalent predicate (see [to_predicate]). *)
  module V : PVARIABLE
  (* underlying integer type *)
  module I : INT_VALUE
end

(* general interface of an integer semi-lattice *)

module type INT_SEMI_LATTICE = sig
  include SEMI_LATTICE
  include INT_DELTA
end

(* general interface of an integer lattice *)

module type INT_LATTICE = sig
  include LATTICE      
  include INT_DELTA
end

(* elements of signature to add to [INT_SEMI_LATTICE] and [INT_LATTICE] 
   to make them atomic integer semi-lattice and lattice signatures.
   Any integer lattice should be an atomic lattice or a cluster lattice.
   In an atomic lattice, the abstract value associated to a variable [v] 
   depends only on that variable, whereas in a cluster lattice, this value 
   depends on a cluster of variables to which [v] belongs. *)

module type ATOMIC_DELTA = sig
      (* repeat the basic types *)
  type tt 
  type int_t
  type var_t
  type ipred_t

      (* arithmetic operations *)
  val add : tt -> tt -> tt
  val iadd : tt -> int_t -> tt
  val sub : tt -> tt -> tt
  val isub : tt -> int_t -> tt
  val neg : tt -> tt
  val mul : tt -> tt -> tt
  val imul : tt -> int_t -> tt
  val div : tt -> tt -> tt
  val idiv : tt -> int_t -> tt  

      (* specialized query functions *)
  val get_bounds : tt -> int_t option * int_t option

      (* specialized constructors *)
  val make_singleton : int_t -> tt
  val make_from_bounds : int_t option -> int_t option -> tt

      (* specialized operations *)
  val lower_bound : tt -> int_t option -> tt
  val upper_bound : tt -> int_t option -> tt

      (* conversion to an equivalent (or closer under-approximation) 
	 predicate *)
  val to_pred : var_t -> tt -> ipred_t option
end

(* general interface of an atomic integer semi-lattice *)

module type ATOMIC_SEMI_LATTICE = sig
  include INT_SEMI_LATTICE with type dim_t = unit
  include ATOMIC_DELTA with type tt = t and type int_t = I.t 
  and type var_t = V.t and type ipred_t = V.P.t
end

(* general interface of an atomic integer lattice *)

module type ATOMIC_LATTICE = sig
  include INT_LATTICE with type dim_t = unit
  include ATOMIC_DELTA with type tt = t and type int_t = I.t
  and type var_t = V.t and type ipred_t = V.P.t
end

(* type of dimension used for a cluster lattice. 
   The integer [n] is the number of variables in the cluster, and the map
   is a correspondance between indices in [0..n-1] and variables. *)

type 'v cluster_dim_t = int * 'v Int31Map.t

module type CLUSTER_LATTICE_NODIM = sig
  include INT_LATTICE

  (* transfer functions *)

      (* transfer function of assignment *)
  val eval_assign : backward:bool -> V.t -> V.T.t -> t -> t
      (* transfer function of test *)
  val eval_test : backward:bool -> V.P.t -> t -> t
      (* returns [true] if the abstract value passed as argument guarantees
	 the success of the test *)
  val guarantee_test : V.P.t -> t -> bool
      (* remove the variable passed as argument from the abstract value *)
  val remove_variable : V.t -> t -> t

  (* formatting functions *)

      (* returns the normal form of the abstract value *)
  val normalize : t -> t
      (* returns a possibly more aggressive normalization than [normalize] *)
  val finalize : t -> t
      (* remove in the 1st abstract value the constraints already present in 
	 the 2nd abstract value *)
  val subtract : t -> t -> t
      
  (* interfacing and queries *)
    
      (* conversion to an equivalent (or closer under-approximation) 
	 predicate *)
  val to_pred : t -> V.P.t option
      (* variables whose domain is restrained by the abstract value *)
  val restrained_variables : t -> V.t list
      (* abstract value represents no concrete individual element *)
  val is_empty : t -> bool
      (* abstract value represents all individual elements *)
  val is_full : t -> bool
end

(* general interface of a cluster integer semi-lattice *)

module type CLUSTER_LATTICE = sig
  module VV : PVARIABLE
  include CLUSTER_LATTICE_NODIM 
  with module V = VV and type dim_t = VV.t cluster_dim_t
end

(* elements of signature to add to some cluster lattice to make it
   a multi-cluster lattice, i.e. so that different clusters of variables can
   be followed at the same time. *)

module type PACKED_DELTA = sig
      (* repeat basic type *)
  type var_t
      (* pack the variables passed as argument. Each list of variables in
	 the main list groups the variables in the same pack. *)
  val pack_variables : var_t list list -> unit
      (* is this variable packed ? (taken into account by the analysis) *)
  val is_packed_variable : var_t -> bool
end

module type PACKED_CLUSTER_LATTICE = sig
  include CLUSTER_LATTICE_NODIM with type dim_t = unit
  include PACKED_DELTA with type var_t = V.t
end

(* interface of a constrained integer semi-lattice. 
   This allows us to tag parts of an abstract value as constrained,
   and follow these constraints as they propagate through the code. *)

module type CONSTRAINED_LATTICE_NODIM = sig
  include CLUSTER_LATTICE_NODIM
      
      (* eliminate the variables in the list according to some heuristics.
	 Used to infer invariants that -should- guarantee good behavior.
	 Cannot be simply using some "forget" operator. *)
  val eliminate : V.t list -> t -> t      
      (* returns a new abstract value based on the abstract value passed
	 as argument, with the additional constraint.
	 Similar to [eval_test] except that here the constraint is tagged 
	 so that we can follow it. *)
  val eval_constraint : V.P.t -> t -> t
      (* is this a constrained abstract value (with tags for constraints) ? *)
  val is_constrained : t -> bool
      (* get the unconstrained part of the abstract value. 
	 If the abstract value is morally equivalent to [A -> B], this returns
	 the [A] part only. *)
  val get_unconstrained : t -> t
      (* get the constrained part of the abstract value. 
	 If the abstract value is morally equivalent to [A -> B], this returns
	 the [B] part only. *)
  val get_constrained : t -> t
      (* get the constraints in an abstract value *)
  val make_unconstrained : t -> t
      (* variables whose domain is restrained by the abstract value on
	 the left-hand side of the implication (they may be restrained on
	 the right-hand side too) *)
  val unconstrained_variables : t -> V.t list
end

module type CONSTRAINED_LATTICE = sig
  module VV : PVARIABLE
  include CONSTRAINED_LATTICE_NODIM 
  with module V = VV and type dim_t = VV.t cluster_dim_t
end

module type PACKED_CONSTRAINED_LATTICE = sig
  include CONSTRAINED_LATTICE_NODIM with type dim_t = unit
  include PACKED_DELTA with type var_t = V.t
end
      
(* interface of a bridge used to communicate information between 
   the main context and the conditional parts of a contextual lattice *)

module type CONTEXTUAL_BRIDGE = sig
  type ipredicate

  module Contxt : PACKED_CLUSTER_LATTICE
  module Constr : PACKED_CONSTRAINED_LATTICE 

  val get_unconstrained : Constr.t -> Contxt.t
  val get_constrained : Constr.t -> Constr.t
  val make_unconstrained : Constr.t -> Contxt.t
  val subtract : Constr.t -> Contxt.t -> Constr.t
  val join : ?backward:bool -> Constr.t -> Contxt.t -> Constr.t
  val meet : Constr.t -> Contxt.t -> Constr.t
  val eval_constraint : ipredicate -> Contxt.t -> Constr.t
end

(* interface of a contextual integer semi-lattice. 
   It encapsulates a context part and a constraint part, with the implicit 
   meaning that the context is always true, while the constraint has an hidden
   implication [constrained parts -> unconstrained parts]. *)

module type PACKED_CONTEXTUAL_LATTICE = sig
  include CLUSTER_LATTICE_NODIM with type dim_t = unit
  include PACKED_DELTA with type var_t = V.t

  module Contxt : PACKED_CLUSTER_LATTICE
  module Constr : PACKED_CONSTRAINED_LATTICE 
  module Bridge : CONTEXTUAL_BRIDGE 
  with module Contxt = Contxt and module Constr = Constr

    (* echoes the elimination on the constrained parts *)
  val eliminate : V.t list -> t -> t
    (* remove the variables not satisfying the filter condition *)
  val filter_variables : remove:(V.t -> bool) -> t -> t
    (* returns the main context *)
  val get_context : t -> Contxt.t
    (* updates the main context *)
  val set_context : t -> Contxt.t -> t
    (* keep only main context *)
  val eliminate_conditionals : t -> t
    (* are there conditional informations ? *)
  val has_conditionals : t -> bool
    (* similar to join, expects it adds conditionals *)
  val join_context : t -> t -> t
    (* similar to remove_variable, expects it only does so on conditionals
       for which join was not performed ("joined" field) *)
  val remove_variable_conditionals : V.t -> t -> t
    (* subtract main context from unique conditional. The integer is a unique
     identifier for the conditional being returned. *)
  val format_singleton : t -> int * bool * Constr.t
    (* precise the information in some context
       takes as parameters the value to precise and the contextual value.
       returns the original value with more information in some context. *)
  val add_conditional : t -> do_join:bool -> int * Constr.t -> t
  val add_new_conditional : t -> do_join:bool -> Constr.t -> t
    (* allow uniform transformation to be applied *)
  val transform : (Contxt.t -> Contxt.t) -> (Constr.t -> Constr.t) -> t -> t
    (* [fold] on context/constraint parts *)
  val fold : (Contxt.t -> 'a -> 'a) -> (Constr.t -> 'a -> 'a) -> t -> 'a -> 'a
end

(* interface of a lattice for discribing variable separation *)

module type SEPARATION_LATTICE = sig
  include PACKED_CLUSTER_LATTICE
  val add_separated_pair : V.t -> V.t -> t -> t
  val get_separated_pairs : t -> (V.t * V.t) list
  val fully_separated : V.t -> V.t -> t -> bool
  val from_pred : V.P.t -> t
end

module type READ_WRITE_LATTICE = sig
  include PACKED_CLUSTER_LATTICE
  val eval_read : V.t -> t -> t
  val eval_write : V.t -> t -> t
  val eval_precondition : V.P.t -> t -> t
end


(*****************************************************************************
 *                                                                           *
 * 		Integer lattices used for value analysis                     *
 *                                                                           *
 *****************************************************************************)

module Make_IntervalLattice (V : PVARIABLE) (I : INT_VALUE) 
    : ATOMIC_LATTICE with module V = V and module I = I =
struct
  module V = V     module I = I      type int_t = I.t     type var_t = V.t

  type ipred_t = V.P.t
  type t = 
        (* full range *)
    | IKfull
	(* greater-than interval *)
    | IKleft_bounded of I.t
	(* less-than interval *)
    | IKright_bounded of I.t
	(* regular interval *)
    | IKbounded of I.t * I.t
	(* empty interval *)
    | IKempty
  type tt = t

  type dim_t = unit
  let top () = IKfull
  let bottom () = IKempty
  let init () = IKempty

  let has_bounds i = match i with
    | IKfull | IKempty -> false
    | IKleft_bounded _ | IKright_bounded _ | IKbounded _ -> true

  let get_bounds i = match i with
    | IKfull | IKempty -> None,None
    | IKleft_bounded lb -> Some lb,None
    | IKright_bounded rb -> None,Some rb
    | IKbounded (lb,rb) -> Some lb,Some rb

  let make_from_bounds lb rb = match lb,rb with
    | None,None -> IKfull
    | None,Some rb -> IKright_bounded rb
    | Some lb,None -> IKleft_bounded lb
    | Some lb,Some rb -> 
	if I.le lb rb then
	  IKbounded (lb,rb)
	else 
	  IKempty

  let lower_bound i b =
    let lb,rb = get_bounds i in
    let new_lb = match Option.some lb b with
      | Some nb -> Some nb
      | None -> Option.binapp I.max lb b
    in
    make_from_bounds new_lb rb

  let upper_bound i b =
    let lb,rb = get_bounds i in
    let new_rb = match Option.some rb b with
      | Some nb -> Some nb
      | None -> Option.binapp I.min rb b
    in
    make_from_bounds lb new_rb

  let is_singleton i = match i with
    | IKfull | IKempty | IKleft_bounded _ | IKright_bounded _ -> false
    | IKbounded (lb,rb) -> I.eq lb rb
	
  let get_singleton i = match i with
    | IKfull | IKempty | IKleft_bounded _ | IKright_bounded _ -> assert false
    | IKbounded (lb,rb) -> assert (I.eq lb rb); lb

  let make_singleton c = IKbounded (c,c)

  let equal i1 i2 = i1 = i2

  let pretty fmt i = match i with
    | IKfull -> Format.fprintf fmt "IKfull"
    | IKleft_bounded lb -> 
	Format.fprintf fmt "IKleft_bounded(%a)" I.pretty lb
    | IKright_bounded rb -> 
	Format.fprintf fmt "IKright_bounded(%a)" I.pretty rb
    | IKbounded (lb,rb) -> 
	Format.fprintf fmt "IKbounded(%a,%a)" I.pretty lb I.pretty rb
    | IKempty -> Format.fprintf fmt "IKempty"

  let to_pred v i = match i with
    | IKfull | IKempty -> None
    | IKright_bounded rb -> 
	Some (IPrel (ITvar v, Le, ITconstant (IntConstant (I.to_string rb))))
    | IKleft_bounded lb -> 
	Some (IPrel (ITvar v, Ge, ITconstant (IntConstant (I.to_string lb))))
    | IKbounded (lb,rb) -> 
	if I.eq lb rb then
	  Some (IPrel (ITvar v, Eq, ITconstant (IntConstant (I.to_string rb))))
	else
	  Some (
	    IPand (
	      IPrel (ITvar v, Le, ITconstant (IntConstant (I.to_string rb))),
	      IPrel (ITvar v, Ge, ITconstant (IntConstant (I.to_string lb)))))

  let join ?(backward=false) i1 i2 =
    if has_bounds i1 && (has_bounds i2) then
      let lb1,rb1 = get_bounds i1 in
      let lb2,rb2 = get_bounds i2 in
      let lb = Option.binapp I.min lb1 lb2 in
      let rb = Option.binapp I.max rb1 rb2 in
      make_from_bounds lb rb
    else
      match i1,i2 with
	| IKfull,_ | _,IKfull -> IKfull
	| IKempty,i | i,IKempty -> i
	| _ -> assert false
      
  let meet i1 i2 =
    if has_bounds i1 && (has_bounds i2) then
      let lb1,rb1 = get_bounds i1 in
      let lb2,rb2 = get_bounds i2 in
      let lb = Option.binapp I.max lb1 lb2 in
      let rb = Option.binapp I.min rb1 rb2 in
      make_from_bounds lb rb
    else
      match i1,i2 with
	| IKfull,i | i,IKfull -> i
	| IKempty,_ | _,IKempty -> IKempty
	| _ -> assert false

  let add i1 i2 =
    if has_bounds i1 && (has_bounds i2) then
      let lb1,rb1 = get_bounds i1 in
      let lb2,rb2 = get_bounds i2 in
      let lb = Option.binapp I.add lb1 lb2 in
      let rb = Option.binapp I.add rb1 rb2 in
      make_from_bounds lb rb
    else
      match i1,i2 with
	| IKempty,_ | _,IKempty -> IKempty
	| IKfull,_ | _,IKfull -> IKfull
	| _ -> assert false

  let iadd i c = 
    if has_bounds i then
      let lb,rb = get_bounds i in
      let lb = Option.app (I.add c) lb in
      let rb = Option.app (I.add c) rb in
      make_from_bounds lb rb
    else i

  let neg i =
    if has_bounds i then
      let lb,rb = get_bounds i in
      let lb,rb = Option.app I.neg rb,Option.app I.neg lb in
      make_from_bounds lb rb
    else i

  let sub i1 i2 = add i1 (neg i2)
    
  let isub i c = 
    if has_bounds i then
      let lb,rb = get_bounds i in
      let lb = Option.app (fun lb -> I.sub lb c) lb in
      let rb = Option.app (fun rb -> I.sub rb c) rb in
      make_from_bounds lb rb
    else i

  let mul i1 i2 =
    let is_zero b = match b with
      | None -> false
      | Some v -> I.is_zero v in
    let zero_or f b1 b2 = 
      if Pair.any is_zero b1 b2 then
	Some I.zero
      else
	f b1 b2 
    in
    if has_bounds i1 && (has_bounds i2) then
      let lb1,rb1 = get_bounds i1 in
      let lb2,rb2 = get_bounds i2 in
      let b1 = zero_or (Option.binapp I.mul) lb1 lb2 in
      let b2 = zero_or (Option.binapp I.mul) lb1 rb2 in
      let b3 = zero_or (Option.binapp I.mul) rb1 lb2 in
      let b4 = zero_or (Option.binapp I.mul) rb1 rb2 in
      let lb = List.fold_left (Option.binapp I.min) b1 [b2;b3] in
      let rb = List.fold_left (Option.binapp I.max) b2 [b3;b4] in
      make_from_bounds lb rb
    else 
      let is_mul_zero = 
	if has_bounds i1 then
	  let lb1,rb1 = get_bounds i1 in
	  Pair.both is_zero lb1 rb1
	else if has_bounds i2 then
	  let lb2,rb2 = get_bounds i2 in
	  Pair.both is_zero lb2 rb2
	else false
      in
      if is_mul_zero then
	match i1,i2 with
	  | IKempty,_ | _,IKempty -> IKempty
	  | IKfull,izero | izero,IKfull -> izero
	  | _ -> assert false
      else
	match i1,i2 with
	  | IKempty,_ | _,IKempty -> IKempty
	  | IKfull,_ | _,IKfull -> IKfull
	  | _ -> assert false
	      
  let imul i c =
    if has_bounds i then
      let lb,rb = get_bounds i in
      let lb = if I.is_zero c then Some c else Option.app (I.mul c) lb in
      let rb = if I.is_zero c then Some c else Option.app (I.mul c) rb in
      make_from_bounds lb rb
    else i

  let idiv i c =
    if has_bounds i then
      let lb,rb = get_bounds i in
      let rev_div c v = I.div v c in
      let lb = if I.is_zero c then None else Option.app (rev_div c) lb in
      let rb = if I.is_zero c then None else Option.app (rev_div c) rb in
      make_from_bounds lb rb
    else i

  (* rough approximation. Only the division by a constant is precisely
     computed. Could be improved on. *)
  let div i1 i2 =
    if is_singleton i2 then
      idiv i1 (get_singleton i2)
    else
      match i1 with
	| IKempty -> IKempty
	| _ -> IKfull

  let widening ws ~old_value ~new_value =
    if has_bounds old_value && (has_bounds new_value) then
      let lb1,rb1 = get_bounds old_value in
      let lb2,rb2 = get_bounds new_value in
      let rec first_match comp b l = match l with
        | [] -> None
	| thres :: r ->
	    if comp b thres then 
	      Some thres 
	    else 
	      first_match comp b r
      in
      let list_threshold = match ws with
        | WidenFast -> []
	| WidenZero -> [I.zero]
	| WidenUnit -> [I.neg I.one; I.zero; I.one]
	| WidenSteps il -> List.map I.of_int il
      in
      let rb =
	match Option.binapp I.le rb2 rb1 with
	  | None -> None
	  | Some true -> rb2
	  | Some false ->
	      let b = match rb2 with Some b -> b | None -> assert false in
	      first_match I.le b list_threshold
      in
      let lb =
	match Option.binapp I.ge lb2 lb1 with
	  | None -> None
	  | Some true -> lb2
	  | Some false ->
	      let b = match lb2 with Some b -> b | None -> assert false in
	      first_match I.ge b (List.rev list_threshold)
      in
      make_from_bounds lb rb
    else
      match old_value,new_value with
        | IKempty,new_value -> new_value
	| _,IKfull -> IKfull
	| _ ->
	    (* the stored value [old_value] is less precise than the new 
	       computed value [new_value], which should not be the case *)
	    assert false
end

module Make_PointWiseFromAtomic (L : ATOMIC_LATTICE)
    : PACKED_CLUSTER_LATTICE with module V = L.V and module I = L.I =
struct 
  module V = L.V    module I = L.I     type int_t = I.t

  include Make_PointWiseLattice(V)(L)

  module VMap = Map.Make (V)

  (* A pointwise lattice is not a relational lattice, therefore the
     evaluation of an assignment is straightforward. We just need to evaluate
     the right-hand side of the assignment and update the map with this new
     abstract value for the assigned variable. *)

  let rec eval_term term pw = match term with
    | ITconstant (IntConstant s) ->
	begin 
	  try L.make_singleton (I.of_string s)
	  with _ -> L.top ()
	end
    | ITconstant (RealConstant _) -> L.top ()
    | ITvar var ->
	find var pw
    | ITunop (op,t1) ->
	let v1 = eval_term t1 pw in
	begin match op with
	  | Clogic.Uminus -> 
	      L.neg v1
	  | Clogic.Uplus | Clogic.Uint_conversion ->
	      v1
	  | Clogic.Utilde | Clogic.Ustar | Clogic.Uamp | Clogic.Uexact 
	  | Clogic.Umodel | Clogic.Uabs_real | Clogic.Usqrt_real 
	  | Clogic.Uround_error | Clogic.Utotal_error | Clogic.Ufloat_of_int
	  | Clogic.Uint_of_float | Clogic.Ufloat_conversion | Clogic.Unot ->
	      L.top ()
	end
    | ITbinop (t1,op,t2) ->
	let v1 = eval_term t1 pw in
	let v2 = eval_term t2 pw in
	begin match op with
	  | Clogic.Badd -> L.add v1 v2
	  | Clogic.Bsub -> L.sub v1 v2
	  | Clogic.Bmul -> L.mul v1 v2
	  | Clogic.Bdiv -> L.div v1 v2
	  | Clogic.Bmod | Clogic.Bpow_real | Clogic.Bbw_and | Clogic.Bbw_xor 
	  | Clogic.Bbw_or | Clogic.Bshift_left | Clogic.Bshift_right 
	      -> L.top ()
	end
    | ITmin _ | ITmax _ -> L.top ()
    | ITany -> L.top ()

  (* not used here: the pointwise lattice is not a relational one *)
  let pack_variables _ = ()
  let is_packed_variable _ = false
  let guarantee_test _ _ = false

  let eval_assign ~backward var term pw =
    assert (not backward);
    let new_val = eval_term term pw in
    replace var new_val pw
    
  (* A test is built from:
     - conjunction, disjunction: these can be translated into [meet] or [join]
     over the abstract domain
     - negation: since the negation of an abstract value is not an abstract
     value in general, we need to push the negation inside the sub-expression.
     This is taken care of by a call to [explicit_pred].
     - relations: every variable involved in the (dis-,in-)equality may have
     its domain reduced by the test. Isolate each variable on the right-hand 
     side or left-hand side to compute the reduced domain.
   *)

  let rec brute_eval_relation p pw = match p with
    | IPrel (ITvar var,op,t1) ->
	let v1 = eval_term t1 pw in
	let vold = find var pw in
	let cstr_val = match op with
	  | Lt -> 
	      let v2 = L.isub v1 (I.of_int 1) in
	      let _,b = L.get_bounds v2 in
	      L.upper_bound vold b
	  | Gt ->
	      let v2 = L.iadd v1 (I.of_int 1) in
	      let b,_ = L.get_bounds v2 in
	      L.lower_bound vold b
	  | Le ->
	      let _,b = L.get_bounds v1 in
	      L.upper_bound vold b
	  | Ge ->
	      let b,_ = L.get_bounds v1 in
	      L.lower_bound vold b
	  | Eq ->
	      L.meet v1 vold
	  | Neq ->
	      (* either a less-than ... *)
	      let v2 = L.isub v1 (I.of_int 1) in
	      let _,b = L.get_bounds v2 in
	      let vlt = L.upper_bound vold b in
	      (* ... or a greater-than ... *)
	      let v2 = L.iadd v1 (I.of_int 1) in
	      let b,_ = L.get_bounds v2 in
	      let vgt = L.lower_bound vold b in
	      (* ... that we combine *)
	      L.join vlt vgt
	in
	replace var cstr_val pw
    | IPrel (t1,op,ITvar var) ->
	let new_p = match op with 
	  | Lt -> IPrel (ITvar var,Gt,t1)
	  | Gt -> IPrel (ITvar var,Lt,t1)
	  | Le -> IPrel (ITvar var,Ge,t1)
	  | Ge -> IPrel (ITvar var,Le,t1)
	  | Eq -> IPrel (ITvar var,Eq,t1)
	  | Neq -> IPrel (ITvar var,Neq,t1)
	in
	brute_eval_relation new_p pw
    | IPrel _ ->
	(* does not deal with more complex relations *)
	pw
    | IPfalse | IPtrue | IPand _ | IPor _ 
    | IPimplies _ | IPiff _ | IPnot _ | IPany | IPfull_separated _ 
    | IPnull_pointer _
    | IPnot_null_pointer _ | IPnull_char_pointed _ | IPnot_null_char_pointed _
	(* should be called only on relations *)
	-> assert false

  let rec eval_test ~backward pred pw = 
    let pred = V.translate_predicate [] (V.P.explicit_pred pred) in
    match pred with
      | IPfalse -> 
	  bottom ()
      | IPtrue | IPany | IPfull_separated _ -> 
	  pw
      | IPand (p1,p2) ->
          let v1 = eval_test ~backward p1 pw in
          let v2 = eval_test ~backward p2 pw in
          meet v1 v2
      | IPor (p1,p2) ->
          let v1 = eval_test ~backward p1 pw in
          let v2 = eval_test ~backward p2 pw in
          join v1 v2
      | IPimplies _ | IPiff _ | IPnot _ ->
	  (* these constructs should have been removed by the call to
	     [explicit_pred] *)
          assert false
      | IPrel (t1,op,t2) ->
          let vars = (V.T.collect_term_vars t1) @ (V.T.collect_term_vars t2) in
	  let init_occur_map =
	    List.fold_left (fun m v -> VMap.add v 1 m) VMap.empty vars 
	  in
	  let preds,_ = 
	    List.fold_left 
	      (fun (pl,m) v -> 
		let noccur = VMap.find v m in
		let vpred = V.P.rewrite_pred_wrt_var pred v noccur in
		let updm = VMap.add v (noccur + 1) m in
		vpred :: pl,updm
	      ) ([],init_occur_map) vars
	  in
	  List.fold_right brute_eval_relation preds pw
      | IPnull_pointer _ | IPnot_null_pointer _ | IPnull_char_pointed _ 
      | IPnot_null_char_pointed _ ->
	  (* these constructs should have been removed by the call to
	     [translate_predicate] *)
          assert false

  let to_pred pw =
    fold (fun v i p_opt ->
      let vp_opt = L.to_pred v i in
      match Option.some p_opt vp_opt with
      | Some p -> Some p
      | None -> Option.binapp (fun p1 p2 -> IPand (p1,p2)) p_opt vp_opt)
      pw None

  let restrained_variables pw =
    fold (fun v i vlist ->
      if L.equal i (L.bottom ()) then
	vlist
      else v :: vlist) pw []

  let is_empty pw = pw = bottom ()
  let is_full pw = pw = top () (* necessary to compare pointwise ? *)
    
  let remove_variable var pw =
    replace var (L.bottom ()) pw

  let normalize pw = pw (* implement here switch to PWAll and PWEmpty *)
  let finalize = normalize

  let subtract pw1 pw2 = pw1 (* minimal implementation *)
end

(* module created does not have a signature. To be used internally, to share
   code between different functors. *)

module Make_InternalPackedFromCluster (V : PVARIABLE) 
  (L : CLUSTER_LATTICE with module VV = V)
  (* : PACKED_CLUSTER_LATTICE with module V = L.V and module I = L.I *) = 
struct
  (* leave this type here so that the OCaml compiler can infer type equality *)
  (* each packed variable has a corresponding abstract value.
     Only the representative variables have an entry in this map. *)
  type t = L.t Map.Make(V).t

  module VMap = Map.Make (V)
  module VSet = Set.Make (V)

  module V = L.V         module I = L.I          type var_t = V.t

    (* Each pack of variables is represented by one of them, chosen as
       representative. For all variables in the pack, this relation is stored
       in [variable_to_rep]. *)
  let variable_to_rep = Hashtbl.create 0
    (* Each representative variable corresponds to a dimension (number of
       variables in the pack) and a pack (correspondance from indices 
       to variables). *)
  let rep_to_dim_and_pack = Hashtbl.create 0
    (* packed variables *)
  let variables = ref VSet.empty
      
  type dim_t = unit

  let bottom () =
    Hashtbl.fold (fun v (dim,pack as d) m -> VMap.add v (L.bottom d) m) 
      rep_to_dim_and_pack VMap.empty

  let top () =
    Hashtbl.fold (fun v (dim,pack as d) m -> VMap.add v (L.top d) m) 
      rep_to_dim_and_pack VMap.empty

  let init = top

  (* functions for packing *)

  let pack_variables vll = 
    let pack_one = function
      | [] -> ()
      | rep_var :: _ as var_list ->
	  let dim,var_map = 
	    List.fold_left 
	      (fun (c,m) v -> 
		 variables := VSet.add v (!variables);
		 Hashtbl.replace variable_to_rep v rep_var;
		 c+1,Int31Map.add c v m
	      ) (0,Int31Map.empty) var_list
	  in
	  Hashtbl.replace rep_to_dim_and_pack rep_var (dim,var_map)
    in
    Hashtbl.clear rep_to_dim_and_pack;
    List.iter pack_one vll

  let is_packed_variable var = VSet.mem var (!variables)

  (* lattice operations *)

  let equal pack1 pack2 =
    Hashtbl.fold 
      (fun v _ is_eq -> is_eq && 
	let elt1 = VMap.find v pack1 and elt2 = VMap.find v pack2 in
	L.equal elt1 elt2
      ) rep_to_dim_and_pack true

  let pretty fmt = VMap.iter (fun _ elt -> L.pretty fmt elt)

  let join ?(backward=false) pack1 pack2 =
    Hashtbl.fold
      (fun v _ m ->
	let elt1 = VMap.find v pack1 and elt2 = VMap.find v pack2 in
	let elt = L.join ~backward elt1 elt2 in
	VMap.add v elt m
      ) rep_to_dim_and_pack VMap.empty

  let meet pack1 pack2 =
    Hashtbl.fold
      (fun v _ m ->
	let elt1 = VMap.find v pack1 and elt2 = VMap.find v pack2 in
	let elt = L.meet elt1 elt2 in
	VMap.add v elt m
      ) rep_to_dim_and_pack VMap.empty

  let widening ws ~old_value ~new_value =
    Hashtbl.fold
      (fun v _ m ->
	let elt1 = VMap.find v old_value and elt2 = VMap.find v new_value in
	let elt = L.widening ws ~old_value:elt1 ~new_value:elt2 in
	VMap.add v elt m
      ) rep_to_dim_and_pack VMap.empty

  let to_pred pack =
    VMap.fold 
      (fun _ elt p_opt ->
	match p_opt,L.to_pred elt with
	| None,None -> None
	| None,Some p | Some p,None -> Some p
	| Some p1,Some p2 -> Some (IPand (p1,p2))
      ) pack None

  let eval_assign ~backward var term pack =
    if is_packed_variable var then
      let rep_var = Hashtbl.find variable_to_rep var in
      let elt = VMap.find rep_var pack in
      let new_elt = L.eval_assign ~backward var term elt in
      VMap.add rep_var new_elt pack
    else pack

  let eval_test ~backward pred pack =
    match pred with
      | IPfalse -> bottom ()
      | _ -> 
	  let test_vars = V.P.collect_predicate_vars pred in
	  let packed_vars = List.filter is_packed_variable test_vars in
	  let rep_vars = List.map (Hashtbl.find variable_to_rep) packed_vars in
	  let rep_set = List.fold_right VSet.add rep_vars VSet.empty in
	  VMap.mapi
	    (fun rep_var elt ->
	      if VSet.mem rep_var rep_set then
		(* some variables in this pack occur in the test *)
		L.eval_test ~backward pred elt
	      else elt
	    ) pack

  let guarantee_test pred pack =
    match pred with
      | IPtrue -> true
      | _ -> 
	  let test_vars = V.P.collect_predicate_vars pred in
	  let packed_vars = List.filter is_packed_variable test_vars in
	  let rep_vars = List.map (Hashtbl.find variable_to_rep) packed_vars in
	  let rep_set = List.fold_right VSet.add rep_vars VSet.empty in
	  VMap.fold
	    (fun rep_var elt do_guarantee ->
	      do_guarantee ||
	      if VSet.mem rep_var rep_set then
		(* some variables in this pack occur in the test *)
		L.guarantee_test pred elt
	      else false
	    ) pack false

  let restrained_variables pack =
    VMap.fold (fun _ elt varl -> (L.restrained_variables elt) @ varl) pack []

  let is_empty pack = 
    VMap.fold (fun _ elt empty -> empty && L.is_empty elt) pack true

  let is_full pack = 
    VMap.fold (fun _ elt full -> full && L.is_full elt) pack true

  let remove_variable var pack =
    if is_packed_variable var then
      let rep_var = Hashtbl.find variable_to_rep var in
      let elt = VMap.find rep_var pack in
      let new_elt = L.remove_variable var elt in
      VMap.add rep_var new_elt pack
    else pack
    
  let normalize pack = VMap.mapi (fun _ elt -> L.normalize elt) pack
  let finalize pack = VMap.mapi (fun _ elt -> L.finalize elt) pack

  let subtract pack1 pack2 =
    Hashtbl.fold
      (fun v _ m ->
	 let elt1 = VMap.find v pack1 and elt2 = VMap.find v pack2 in
	 let elt = L.subtract elt1 elt2 in
	 VMap.add v elt m
      ) rep_to_dim_and_pack VMap.empty
end

module Make_PackedFromCluster (V : PVARIABLE) 
    (L : CLUSTER_LATTICE with module VV = V)
    : PACKED_CLUSTER_LATTICE with module V = L.V and module I = L.I 
      and type t = L.t Map.Make(V).t = 
struct
  include Make_InternalPackedFromCluster (V) (L)
end

module Make_PackedFromConstrained (V : PVARIABLE) 
    (L : CONSTRAINED_LATTICE with module VV = V)
    : PACKED_CONSTRAINED_LATTICE with module V = L.V and module I = L.I
      and type t = L.t Map.Make(V).t = 
struct
  include Make_InternalPackedFromCluster (V) (L)

  let eval_constraint pred pack =
    match pred with
      | IPfalse -> bottom ()
      | _ -> 
	  let test_vars = V.P.collect_predicate_vars pred in
	  let packed_vars = List.filter is_packed_variable test_vars in
	  let rep_vars = List.map (Hashtbl.find variable_to_rep) packed_vars in
	  let rep_set = List.fold_right VSet.add rep_vars VSet.empty in
	  VMap.mapi
	    (fun rep_var elt ->
	      if VSet.mem rep_var rep_set then
		(* some variables in this pack occur in the test *)
		L.eval_constraint pred elt
	      else elt
	    ) pack

  let is_constrained pack =
    VMap.fold (fun _ elt is_cstr -> is_cstr || L.is_constrained elt) pack false

  let get_unconstrained pack = VMap.map L.get_unconstrained pack

  let get_constrained pack = VMap.map L.get_constrained pack

  let make_unconstrained pack = VMap.map L.make_unconstrained pack

  let unconstrained_variables pack = 
    VMap.fold (fun _ elt vlist -> L.unconstrained_variables elt @ vlist)
      pack []

  let subtract pack1 pack2 =
    Hashtbl.fold
      (fun v _ m ->
	let elt1 = VMap.find v pack1 and elt2 = VMap.find v pack2 in
	let elt = L.subtract elt1 elt2 in
	VMap.add v elt m
      ) rep_to_dim_and_pack VMap.empty

  let eliminate var_list pack =
    if debug_more then Coptions.lprintf 
	"[eliminate] list of written variables %a@."
	(print_list comma V.pretty) var_list;
    VMap.mapi (fun rep_var elt -> L.eliminate var_list elt) pack
end

(* a simple octogon with its dimension and the variables it represents *)
type ('v,'map) oct_t =
    { 
      dimension : int;
      variables : 'v Int31Map.t; (* map from indices to variables *)
      indices   : 'map; (* reverse map from variables to indices *)
      octogon   : Oct.oct 
    }

(* module created does not have a signature. To be used internally, to share
   code between different functors. *)

module Make_InternalOctogonLattice (V : PVARIABLE) (I : INT_VALUE)
    (* : CLUSTER_LATTICE with module VV = V and module I = I *) =
struct
  (* leave this type here so that the OCaml compiler can infer type equality *)
  type t = (V.t,int Map.Make(V).t) oct_t
  type dim_t = int * V.t Int31Map.t

  module VMap = Map.Make (V)
  module VSet = Set.Make (V)

  module V = V     module VV = V     module I = I     type var_t = V.t

  (* lattice values *)

    (* exploit the fact [bottom] and [top] were made functions to build
       the correct bottom and top elements from the information on 
       packed variables *)
  let bottom (dim,vars) = 
    { dimension = dim; variables = vars; octogon = Oct.empty dim;
      indices = Int31Map.fold (fun i v m -> VMap.add v i m) vars VMap.empty }

  let top (dim,vars) =
    { dimension = dim; variables = vars; octogon = Oct.universe dim;
      indices = Int31Map.fold (fun i v m -> VMap.add v i m) vars VMap.empty }

  let init = top

  let is_empty oct = Oct.is_empty oct.octogon
  let is_full oct = Oct.is_universe oct.octogon

  (* lattice operations *)

  let equal oct1 oct2 = Oct.is_equal oct1.octogon oct2.octogon

  let pretty fmt oct =
    let var_name i = V.to_string (Int31Map.find i oct.variables) in
    Oct.foctprinter var_name fmt oct.octogon

  let get_widening_strategy ws = match ws with
    | WidenFast -> Oct.WidenFast
    | WidenZero -> Oct.WidenZero
    | WidenUnit -> Oct.WidenUnit
    | WidenSteps il ->
	let vl = List.map float_of_int il in
	let va = Array.of_list vl in
	Oct.WidenSteps (Oct.vnum_of_float va)

  (* the stored value is [oct1] and the new computed value is [oct2] *)
  let widening ws ~old_value ~new_value =
    let ws = get_widening_strategy ws in
    { old_value with octogon = 
	Oct.widening old_value.octogon new_value.octogon ws }

  (* query functions *)

  let is_targetted_variable oct v = 
    try ignore(VMap.find v oct.indices); true with Not_found -> false

  let followed_indices ?(tagged=false) ?(untagged=false) oct =
    if debug_more then Coptions.lprintf 
      "[followed_indices] tagged ? %B untagged ? %B on %a@."
      tagged untagged pretty oct;
    if is_empty oct then 
      Int31Set.empty 
    else
      let classify_vars = 
	if tagged then
	  Oct.get_tagged_vars oct.octogon
	else if untagged then
	  Oct.get_untagged_vars oct.octogon
	else
	  Oct.get_restrained_vars oct.octogon
      in
      let classify_vars = Oct.int_of_vnum classify_vars in
      let _,idx_set =
	Array.fold_left
	  (fun (idx,iset) tag_opt -> match tag_opt with
	  | None -> 
	      (* [Oct.int_of_num] failed on num = 0 or 1. Impossible. *)
	      assert false
	  | Some is_cstr ->
	      if is_cstr <> 0 then
		(* [idx] is the index of a restrained/constrained variable *)
		idx + 1,Int31Set.add idx iset
	      else 
		idx + 1,iset
	  ) (0,Int31Set.empty) classify_vars
      in
      idx_set

  let followed_variables ?(tagged=false) ?(untagged=false) (oct : t) =
    let idx_set = followed_indices ~tagged ~untagged oct in
    Int31Set.fold (fun i varl -> (Int31Map.find i oct.variables) :: varl)
      idx_set []
      
  let restrained_variables oct = followed_variables oct

  let remove_variable var oct = 
    let idx = VMap.find var oct.indices in
    let new_octogon = Oct.forget oct.octogon idx in
    { oct with octogon = new_octogon }

  (* interfacing *)

  let internal_to_pred minimize oct = 
    (* take care of special empty case separately *)
    if is_empty oct || Oct.is_universe oct.octogon then
      None
    else
      let v_name i = "v" ^ (string_of_int i) in
      let v_num s = int_of_string (String.sub s 1 (String.length s - 1)) in
      let lex = 
	Genlex.make_lexer 
	  ["+"; "-"; "<"; "<="; ">"; ">="; "="; ","; "{"; "}";"=>"] in
      let rec parse_atom = parser
	| [< 'Genlex.Int n >] -> 
	    ITconstant (IntConstant (string_of_int n))
	| [< 'Genlex.Ident id >] -> 
	    ITvar (Int31Map.find (v_num id) oct.variables)
	| [<>] -> failwith "[to_pred] atom parsing error"
      and parse_term = parser
	| [< t1 = parse_atom; t2 = parse_term_rest t1 >] -> t2
	| [<>] -> failwith "[to_pred] term parsing error"
      and parse_term_rest t1 = parser
	| [< 'Genlex.Kwd "+"; t2 = parse_atom >] ->
	    ITbinop (t1,Clogic.Badd,t2)
	| [< 'Genlex.Kwd "-"; t2 = parse_atom >] ->
	    ITbinop (t1,Clogic.Bsub,t2)
	| [<>] -> t1
      and parse_relation = parser
	| [< t1 = parse_term; p = parse_relation_rest t1 >] -> p
	| [<>] -> failwith "[to_pred] relation parsing error"
      and parse_relation_rest t1 = parser
	| [< 'Genlex.Kwd "<"; p = parse_term_or_relation t1 Clogic.Lt >]
	  -> p
	| [< 'Genlex.Kwd "<="; p = parse_term_or_relation t1 Clogic.Le >]
	  -> p
	| [< 'Genlex.Kwd ">"; p = parse_term_or_relation t1 Clogic.Gt >]
	  -> p
	| [< 'Genlex.Kwd ">="; p = parse_term_or_relation t1 Clogic.Ge >]
	  -> p
	| [< 'Genlex.Kwd "="; p = parse_term_or_relation t1 Clogic.Eq >] 
	  -> p
      and parse_term_or_relation t1 op = parser
	| [< t2 = parse_term; 
	     p = parse_term_or_relation_rest (IPrel (t1,op,t2)) t2 >] -> p
	| [<>] -> failwith "[to_pred] term or relation parsing error"
      and parse_term_or_relation_rest p1 t2 = parser
	| [< 'Genlex.Kwd "<"; t3 = parse_term >] ->
	    IPand (p1, IPrel (t2,Clogic.Lt,t3))
	| [< 'Genlex.Kwd "<="; t3 = parse_term >] ->
	    IPand (p1, IPrel (t2,Clogic.Le,t3))
	| [< 'Genlex.Kwd ">"; t3 = parse_term >] ->
	    IPand (p1, IPrel (t2,Clogic.Gt,t3))
	| [< 'Genlex.Kwd ">="; t3 = parse_term >] ->
	    IPand (p1, IPrel (t2,Clogic.Ge,t3))
	| [< 'Genlex.Kwd "="; t3 = parse_term >] ->
	    IPand (p1, IPrel (t2,Clogic.Eq,t3))
	| [<>] -> p1
      and parse_relation_list = parser
	| [< p1 = parse_relation; p2 = parse_relation_list_rest p1 >] -> p2
	| [<>] -> failwith "[to_pred] list of relations parsing error"
      and parse_relation_list_rest p1 = parser
	| [< 'Genlex.Kwd ","; p2 = parse_relation_list >] ->
	    IPand (p1,p2)
	| [<>] -> p1
      and parse_predicate = parser
	| [< 'Genlex.Kwd "{"; p = parse_predicate_rest; >] -> p
	| [<>] -> failwith "[to_pred] predicate parsing error"
      and parse_predicate_rest = parser
	| [< 'Genlex.Kwd "}" >] -> 
	    (* allow here an empty left-hand side of an implication.
	       It is useful if the context is not only determined by
	       the octogon but also by another lattice.
	    *)
	    IPtrue
	| [< p = parse_relation_list; 'Genlex.Kwd "}" >] -> p
	| [<>] -> failwith "[to_pred] rest of predicate parsing error"
      and parse_octogon = parser
	| [< p1 = parse_predicate; p2 = parse_octogon_rest p1 >] -> p2
	| [<>] -> failwith "[to_pred] octogon parsing error"
      and parse_octogon_rest p1 = parser
	| [< 'Genlex.Kwd "=>"; p2 = parse_predicate >] ->
	    IPimplies (p1,p2)
	| [<>] -> p1
      in
      let buf = Buffer.create 100 in
      let fmt = Format.formatter_of_buffer buf in
      let moct = minimize oct in
      Oct.foctprinter v_name fmt moct.octogon;
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buf in
      if debug_more then Coptions.lprintf 
	  "[to_pred] %a vs %s@." (Oct.foctprinter v_name) moct.octogon s;
      Some (parse_octogon (lex (Stream.of_string s)))

  (* flat types and functions taken from Miné's example analysis *)

  type flat_term =
    | FIrand                          (* random expression *)
    | FIlinear of float array         (* linear expression *)

  type flat_predicate =
    | FBrand                          (* random expression *)
    | FBand of flat_predicate list    (* and *)
    | FBor of flat_predicate list     (* or *)
    | FBtest of float array           (* linear test *)
    | FBtrue                          (* true *)
    | FBfalse                         (* false *)

  let rec flatify_term t oct =
    let n = oct.dimension in
    let rec randup = function
      | ITconstant (IntConstant s) | ITconstant (RealConstant s) -> 
	  begin 
	    try 
	      let a = Array.make (n+1) 0. in 
	      a.(n) <- float_of_string s;
	      FIlinear a
	    with Failure "float_of_string" -> FIrand
	  end
      | ITvar s ->
	  begin try 
	    let idx = VMap.find s oct.indices in
	    let s2 = Int31Map.find idx oct.variables in
	    if V.equal s s2 then
	      let a = Array.make (n+1) 0. in 
	      a.(idx) <- 1.; 
	      FIlinear a
	    else
	      FIrand
	  with Not_found -> FIrand end
      | ITunop (op,t1) -> 
	  begin match op with
	    | Clogic.Uminus -> 
		begin match randup t1 with
		  | FIrand -> FIrand
		  | FIlinear a1 ->
		      let a = Array.make (n+1) 0. in
		      for i=0 to n do a.(i) <- (-. a1.(i)) done;
		      FIlinear a
		end
	    | Clogic.Uplus | Clogic.Uint_conversion ->
		randup t1
	    | Clogic.Utilde | Clogic.Ustar | Clogic.Uamp | Clogic.Uexact 
	    | Clogic.Umodel | Clogic.Uabs_real | Clogic.Usqrt_real 
	    | Clogic.Uround_error | Clogic.Utotal_error | Clogic.Ufloat_of_int
	    | Clogic.Uint_of_float | Clogic.Ufloat_conversion | Clogic.Unot ->
		FIrand
	  end
      | ITbinop (t1,op,t2) ->
	  begin match op with
	    | Clogic.Badd -> 
		begin match randup t1,randup t2 with
		  | FIrand,_ | _,FIrand -> FIrand
		  | FIlinear a1,FIlinear a2 ->
		      let a = Array.make (n+1) 0. in
		      for i=0 to n do a.(i) <- a1.(i) +. a2.(i) done;
		      FIlinear a
		end   
	    | Clogic.Bsub -> 
		begin match randup t1,randup t2 with
		  | FIrand,_ | _,FIrand -> FIrand
		  | FIlinear a1,FIlinear a2 ->
		      let a = Array.make (n+1) 0. in
		      for i=0 to n do a.(i) <- a1.(i) -. a2.(i) done;
		      FIlinear a
		end   
	    | Clogic.Bmul ->
		begin match randup t1,randup t2 with
		  | FIrand,_ | _,FIrand -> FIrand
		  | FIlinear a1,FIlinear a2 ->
		      begin try
			for i=0 to n-1 do 
			  if a1.(i)<>0. then raise Not_found done;
			let a = Array.make (n+1) 0. in
			for i=0 to n do a.(i) <- a1.(n) *. a2.(i) done;
			FIlinear a
		      with Not_found ->
			begin try
			  for i=0 to n-1 do 
			    if a2.(i)<>0. then raise Not_found done;
			  let a = Array.make (n+1) 0. in
			  for i=0 to n do a.(i) <- a2.(n) *. a1.(i) done;
			  FIlinear a
			with Not_found -> FIrand
			end
		      end
		end
	    | Clogic.Bdiv | Clogic.Bmod | Clogic.Bpow_real
	    | Clogic.Bbw_and | Clogic.Bbw_xor 
	    | Clogic.Bbw_or | Clogic.Bshift_left | Clogic.Bshift_right 
		-> FIrand
	  end
      | ITmax tlist | ITmin tlist as minmax_t ->
	    (* 
	    let v_of_t t = match t with
	      | ITvar s ->
		  let idx = VMap.find s oct.indices in
		  let s2 = Int31Map.find idx oct.variables in
		  if V.equal s s2 then
		    (* variable is followed by octagon *)
		    s
		  else
		    raise Not_found
	      | _ -> raise Not_found
	    in
	    let _ = List.map v_of_t tlist in 
	    *)
	    let vminmax = V.generate_variable minmax_t in
	    let idx = 
	      try
		VMap.find vminmax oct.indices
	      with Not_found -> 
		raise (V.Introduce_variable vminmax)
	    in
	    let s2 = Int31Map.find idx oct.variables in
	    if V.equal vminmax s2 then
	      (* variable is followed by octagon *)
	      let a = Array.make (n+1) 0. in 
	      a.(idx) <- 1.; 
	      FIlinear a
	    else
	      raise (V.Introduce_variable vminmax)
      | ITany -> FIrand
    in randup t

  let flatify_predicate ?(guarantee=false) p oct =
    let n = oct.dimension in
    let restr_vars = restrained_variables oct in
    if debug_more then Coptions.lprintf 
      "[flatify_predicate] restr_vars %a@." 
      (print_list comma V.pretty) restr_vars;
    let p = V.translate_predicate restr_vars (V.P.explicit_pred p) in
    let rec simpl = function
      | IPfalse -> FBfalse
      | IPtrue -> FBtrue
      | IPand (p1,p2) -> 
	  FBand [simpl p1; simpl p2]
      | IPor (p1,p2) ->
	  FBor [simpl p1; simpl p2]
      | IPimplies _ | IPiff _ | IPnot _ ->
	  (* these constructs should have been removed by the call to
	     [explicit_pred] *)
          assert false
      | IPany -> FBrand
      | IPfull_separated _ -> FBrand (* or FBtrue ? *)
      | IPrel (t1,Clogic.Eq,t2) ->
	  FBand [simpl (IPrel (t1,Clogic.Le,t2)); 
		 simpl (IPrel (t1,Clogic.Ge,t2))]
      | IPrel (t1,Clogic.Neq,t2) ->
	  FBor [simpl (IPrel (t1,Clogic.Lt,t2)); 
		simpl (IPrel (t1,Clogic.Gt,t2))]
      | IPrel (t1,Clogic.Gt,t2) -> 
	  simpl (IPrel (t2,Clogic.Lt,t1))
      | IPrel (t1,Clogic.Ge,t2) -> 
	  simpl (IPrel (t2,Clogic.Le,t1))
      | IPrel (t1,Clogic.Lt,t2) ->
	  simpl (IPrel(t1,Clogic.Le,
		       ITbinop (t2,Clogic.Bsub,ITconstant (IntConstant "1"))))
      | IPrel (t1,Clogic.Le,t2) ->
	  begin match flatify_term t1 oct,flatify_term t2 oct with
	    | FIrand,_ | _,FIrand -> FBrand
	    | FIlinear a1,FIlinear a2 ->
		let a = Array.make (n+1) 0. in
		for i=0 to n do a.(i) <- a2.(i) -. a1.(i) done;
		FBtest a
	  end
      | IPnull_pointer _ | IPnot_null_pointer _ | IPnull_char_pointed _ 
      | IPnot_null_char_pointed _ ->
	  (* these constructs should have been removed by the call to
	     [translate_predicate] *)
          assert false
    in simpl p

  (* transfer functions *)

  let eval_assign ~backward var term oct =
    let idx = VMap.find var oct.indices in
    let new_octogon = match flatify_term term oct with
      | FIrand     -> 
	  (* same effect on forward and backward propagation *)
	  Oct.forget oct.octogon idx
      | FIlinear a -> 
	  if backward then
	    Oct.substitute_var oct.octogon idx (Oct.vnum_of_float a)
	  else
	    Oct.assign_var oct.octogon idx (Oct.vnum_of_float a)
    in
    { oct with octogon = new_octogon }

  (* exception raised by [eval_flat] when in guarantee mode to signal that 
     the test cannot be guaranteed due to some random part (or non-linear part
     that would have been translated to a random part).
     
     Non-convex parts (i.e. a disjunction) are treated differently,
     e.g. the test 
          [a != b] 
     which would be translated in 
          [a <= b-1 || a >= b+1]
     does not raise the exception [No_guarantee], although an octogon cannot
     in general guarantee such a non-convex test.
     See below.
   *)

  exception No_guarantee

  type or_collect_t =
    | Join of (t -> t -> t)
    | Meet of (t -> t -> t)

  let eval_flat ?(guarantee=false) ?(tagging=false) ~or_collect orig_oct pred =
    if debug_more then Coptions.lprintf 
      "[eval_flat] orig_oct %a@." pretty orig_oct;
    let rec eval oct = function
      | FBrand              -> 
	  if guarantee then
	    (* cannot guarantee anything if the predicate contains 
	       random parts *)
	    raise No_guarantee
	  else oct
      | FBtrue              -> oct
      | FBfalse             -> bottom (oct.dimension,oct.variables)
      | FBand l             -> List.fold_left eval oct l
      | FBtest f            ->
	  let num_var = ref 0 in
	  for i=0 to oct.dimension-1 do if f.(i) <> 0. then incr num_var done;
	  if guarantee && !num_var > 2 then 
	    (* The octagon domain cannot take into account exactly tests
	       with more than 2 variable involved, so we conservatively
	       consider the test cannot be guaranteed. *)
	    raise No_guarantee;
	  let new_octogon =
	    if tagging then
	      (* evaluating backward a constraint means tagging it to allow
		 following its transformation throughout the code *)
	      Oct.add_tagged_constraint oct.octogon (Oct.vnum_of_float f)
	    else 
	      Oct.add_constraint oct.octogon (Oct.vnum_of_float f)
	  in
	  { oct with octogon = new_octogon }
      | FBor l              ->
	  if guarantee then
	    (* cannot guarantee anything if the predicate contains 
	       non-convex parts. We should therefore raise the exception 
	       [No_guarantee]. 

	       Here we can do better since the result of this call will only
	       be compared to the initial octogon [orig_oct]. 
	       We can return any sub-result that is equal to [orig_oct],
	       and we can ignore those sub-results that raise [No_guarantee].
	       If all sub-results raise this exception, it is equivalent to 
	       return the bottom element.

	       It should also be noted that in the guarantee mode, since 
	       [eval_flat] does not perform any [join] operation, it returns
	       an octogon in normalized form if its argument was in normalized
	       form.
	     *)
	    let _,bottom_or_orig =
	      List.fold_left 
		(fun (found_orig,acc_oct as acc) e -> 
		  if found_orig then
		    acc
		  else
		    try 
		      let sub_oct = eval oct e in
		      if equal sub_oct orig_oct then
			true,orig_oct
		      else
			false,acc_oct
		    with No_guarantee -> false,acc_oct
		) (false,bottom (oct.dimension,oct.variables)) l
	    in bottom_or_orig
	  else
	    match or_collect with
	      | Join join ->
		  List.fold_left (fun acc e -> join (eval oct e) acc) 
		    (bottom (oct.dimension,oct.variables)) l
	      | Meet meet ->
		  let list_oct = List.map (eval oct) l in
		  let acc_oct = match list_oct with
		    | [] -> assert false (* not expected in code around *)
		    | fst_oct :: rest_oct ->
			List.fold_right meet rest_oct fst_oct
		  in
		  if is_empty acc_oct then 
		    (* if all octogons in [l] are empty, return empty, 
		       otherwise conflict may arise from operating the [meet]
		       over the or-ed conditions. In that case, return 
		       the current [oct]. *)
		    if List.length (List.filter is_empty list_oct) 
		      = List.length list_oct
		    then acc_oct else oct
		  else acc_oct
    in
    let res = eval orig_oct pred in
    if debug_more then Coptions.lprintf 
      "[eval_flat] result %a@." pretty res;
    res

  let eval_test_or_constraint ~tagging ~or_collect pred oct =
    let pred = flatify_predicate pred oct in
    eval_flat ~tagging ~or_collect oct pred

  let internal_guarantee_test ~or_collect pred oct =
    try
      let pred = flatify_predicate ~guarantee:true pred oct in
      let test_oct = eval_flat ~guarantee:true ~or_collect oct pred
	(* no need to normalize [test_oct], it should already be *)
      in
      (* if the predicate does not contain random parts and no more than
	 2 variables involved in every test, it has been taken into account
	 exactly by the octogon domain.
	 In that case, the test is guaranteed by the original [close_oct]
	 iff the new octogon [test_oct] is equal to it. *)
      equal oct test_oct
    with No_guarantee ->
      (* cannot guarantee anything if the predicate contains random parts 
	 or more than 2 variables in some test *)
      false

end

module Make_OctogonLattice (V : PVARIABLE) (I : INT_VALUE)
    : CLUSTER_LATTICE with module VV = V and module I = I 
      and type t = (V.t,int Map.Make(V).t) oct_t =
struct
  include Make_InternalOctogonLattice (V) (I)

  let join ?(backward=false) oct1 oct2 =
    { oct1 with octogon = Oct.union oct1.octogon oct2.octogon }

  let meet oct1 oct2 =
    { oct1 with octogon = Oct.inter oct1.octogon oct2.octogon }

  (* same transfer function for forward/backward test *)
  let eval_test ~backward = 
    eval_test_or_constraint ~tagging:false ~or_collect:(Join join)

  (* normalized form of an octogon: closed octogon *)
  let rec normalize oct = { oct with octogon = Oct.close oct.octogon }
  let finalize = normalize

  (* minimal form of an octogon *)
  let minimize oct = 
    { oct with octogon = Oct.m_to_oct (Oct.m_from_oct oct.octogon) }

  let subtract oct1 oct2 = 
    let oct1 = minimize oct1 in
    let oct2 = normalize oct2 in
    if debug_more then Coptions.lprintf 
      "[subtract] oct1 is %a@.oct2 is %a@." pretty oct1 pretty oct2;
    let new_octogon = Oct.subtract oct1.octogon oct2.octogon in
    { oct1 with octogon = new_octogon }

  let to_pred oct = internal_to_pred minimize (normalize oct)

  let guarantee_test pred oct =
    internal_guarantee_test ~or_collect:(Join join) pred (normalize oct)
end

module Make_ConstrainedOctogonLattice (V : PVARIABLE) (I : INT_VALUE)
    (NO : CLUSTER_LATTICE with module VV = V and module I = I 
			  and type t = (V.t,int Map.Make(V).t) oct_t)
    : CONSTRAINED_LATTICE with module VV = V and module I = I 
      and type t = (V.t,int Map.Make(V).t) oct_t =
struct
  include Make_InternalOctogonLattice (V) (I)

  module VarElim = VarElimination (V)

  let is_constrained oct =
    assert (Oct.hastags oct.octogon = Oct.hastags2 oct.octogon);
    Oct.hastags oct.octogon

  let get_unconstrained oct =
    let new_octogon = Oct.remove_tagged_constraints oct.octogon in
    { oct with octogon = new_octogon }

  let get_constrained oct =
    let new_octogon = Oct.remove_untagged_constraints oct.octogon in
    { oct with octogon = new_octogon }

  let make_unconstrained oct =
    let new_octogon = Oct.remove_tags oct.octogon in
    { oct with octogon = new_octogon }

  let make_constrained oct =
    let new_octogon = Oct.makeall_tags oct.octogon in
    { oct with octogon = new_octogon }

  let join_meet ~join loper roper oct1 oct2 =
    if is_constrained oct1 && is_constrained oct2 then
      let loct1 = get_unconstrained oct1 and roct1 = get_constrained oct1 in
      if debug_more then Coptions.lprintf 
	"[join/meet] loct1 is %a@.roct1 is %a@." pretty loct1 pretty roct1;
      let loct2 = get_unconstrained oct2 and roct2 = get_constrained oct2 in
      if debug_more then Coptions.lprintf 
	"[join/meet] loct2 is %a@.roct2 is %a@." pretty loct2 pretty roct2;
      let loct = Oct.close (loper loct1.octogon loct2.octogon) in
      if debug_more then Coptions.lprintf 
	"[join/meet] loct is %a@." pretty { oct1 with octogon = loct };
      let roct = roper roct1.octogon roct2.octogon in
      if debug_more then Coptions.lprintf 
	"[join/meet] roct is %a@." pretty { oct1 with octogon = roct };
      (*
	now if [loct] contains some inequality, e.g. x <= c or x - y <= c,
	then the corresponding inequality in [roct] should be ignored, if any.
	Indeed, constrained octogons cannot represent formulas of the form
            x > 1 => x > 2
	therefore the safe approximation in that case is to choose
            x > 1 => {}
	rather than 
            {}    => x > 2
      *)
      let res = { oct1 with octogon = Oct.complete loct roct } in
      if debug_more then Coptions.lprintf 
	"[join/meet] result is %a@." pretty res;
      res
    else if join && (is_constrained oct1 || is_constrained oct2) then
      (* one is not constrained *)
      let constr_oct,oct = 
	if is_constrained oct1 then oct1,oct2 else oct2,oct1
      in
      (* [constr_oct] contains the full context *)
      let unconstr_oct = get_unconstrained constr_oct in
      let merge_oct = make_unconstrained constr_oct in
      let join_oct = NO.join merge_oct oct in
      (* [sub_oct] is new rhs from join *)
      let sub_oct = NO.subtract join_oct unconstr_oct in
      (* AJOUTER CHAMP POUR SIGNIFIER JOIN FAIT, APRES TEST SUR sub_oct *)
      let rhs_oct = make_constrained sub_oct in
      let new_constr_oct = NO.meet unconstr_oct rhs_oct in
      new_constr_oct
    else 
      (* both unconstrained *)
      bottom (oct1.dimension,oct1.variables)

  let double_join oct1 oct2 =
    let loct1 = get_unconstrained oct1 and roct1 = get_constrained oct1 in
    let loct2 = get_unconstrained oct2 and roct2 = get_constrained oct2 in
    let loct = Oct.close (Oct.union loct1.octogon loct2.octogon) in
    let roct = Oct.union roct1.octogon roct2.octogon in
    { oct1 with octogon = Oct.complete loct roct }

  let join ?(backward=false) = 
    if backward then 
      double_join
    else
      join_meet ~join:true Oct.inter Oct.union

  let meet oct1 oct2 = (*join_meet ~join:false Oct.inter Oct.inter*)
    if is_constrained oct1 && is_constrained oct2 then
      join oct1 oct2
    else (* at least one is not constrained *)
      NO.meet oct1 oct2

  (* equivalent to the normal octogon [meet], used internally to put together
     some unconstrained part and some constrained part *)
  let inter oct1 oct2 =
    { oct1 with octogon = Oct.inter oct1.octogon oct2.octogon }

  (* normalized form of a constrained octogon *)
  let internal_normalize ~remove_left_full oct = 
    if is_constrained oct then
      let oct =
	if remove_left_full then
	  (* transform an octogon representing an implication with an empty
	     left part to an equivalent octogon with no implication *)
	  let left_oct = get_unconstrained oct in
	  let left_oct =
	    { left_oct with octogon = Oct.close left_oct.octogon } in
	  if debug_more then Coptions.lprintf 
	      "[normalize] normalized from %a@." pretty oct;
	  if is_full left_oct then make_unconstrained oct else oct
	else oct
      in
      if debug_more then Coptions.lprintf 
	  "[normalize] normalized to %a@." pretty oct;
      (* close the resulting octogon *)
      { oct with octogon = Oct.close oct.octogon }
    else
      (* constrained octogon without constraint is equivalent to empty one *)
      bottom (oct.dimension,oct.variables)

  (* external normalization function *)
  let normalize = internal_normalize ~remove_left_full:false
  let finalize = normalize

  (* function used only once on octogon, because it may remove the constraint
     left part if it is full. If reapplied, it would return the empty octogon.
     Used for printing or other queries not returning the resulting octogon.
   *)
(* CHANGE DUE TO ADDED CONTEXT OUTSIDE OF OCTOGON *)
  let normalize_only_once = internal_normalize ~remove_left_full:false

  (* minimal form of an octogon *)
  let minimize oct = 
    let unconstr_oct = get_unconstrained oct in
    let constr_oct = get_constrained oct in
    (* minimization by switching to hollow form of octogon and back only works
       on untagged octogons (so that closed octogon has appropriate 
       properties). This makes it necessary here to separate the tagged and
       untagged parts of the initial octogon to minimize the first one only. *)
    let unconstr_moct = 
      { oct with octogon = Oct.m_to_oct (Oct.m_from_oct unconstr_oct.octogon) }
    in
    inter unconstr_moct constr_oct

  let to_pred oct = internal_to_pred minimize (normalize_only_once oct)

  let guarantee_test pred oct = 
    internal_guarantee_test ~or_collect:(Join join) pred
      (get_unconstrained (normalize_only_once oct))

  (* noop in forward mode, normal test in backward mode *)
  let eval_test ~backward pred oct = 
    if backward then
      eval_test_or_constraint ~tagging:false ~or_collect:(Join double_join) pred oct
    else oct

  let eval_constraint pred oct =
    (* current use expects unconstrained or constrained [oct] here *)
    eval_test_or_constraint ~tagging:true ~or_collect:(Join join) pred oct
    
  let subtract oct1 oct2 =
    if equal oct1 oct2 then
      top (oct1.dimension,oct1.variables)
    else
      (* only unconstrained parts can be safely subtracted. Otherwise we would
	 have to tell whether a particular inequality in the octogon is 
	 contrained or not. It could be added if necessary. *)
      let oct1 = minimize oct1 in
      let oct2 = get_unconstrained oct2 in
      let new_octogon = Oct.subtract oct1.octogon oct2.octogon in
      { oct1 with octogon = new_octogon }
	
  let constrained_variables oct = followed_variables ~tagged:true oct

  let unconstrained_variables oct = followed_variables ~untagged:true oct

  let eliminate remove_vars oct =
    if debug then Coptions.lprintf 
      "[eliminate] initial oct %a@." pretty oct;
    (* get those variables from [remove_vars] that are in the current pack *)
    let remove_vars =
      List.filter (fun v -> is_targetted_variable oct v) remove_vars
    in
    if debug then Coptions.lprintf 
      "[eliminate] list of written variables %a@."
      (print_list comma V.pretty) remove_vars;
    let remove_vars =
      List.fold_right (fun v s -> VSet.add v s) remove_vars VSet.empty
    in

    let rec elim oct =
      (* keep only minimal relations to increase the chance of finding 
	 the adequate necessary inequality with Fourier-Motzkin *)
      let full_oct = normalize oct in
      let oct = minimize full_oct in
      if debug_more then Coptions.lprintf 
	"[eliminate] minimal oct %a@." pretty oct;
      let cstr_vars = constrained_variables oct in
      if debug_more then Coptions.lprintf 
	"[eliminate] list of constrained variables %a@."
	(print_list comma V.pretty) cstr_vars;
      (* deal with constrained variables only *)
      let _,new_oct = List.fold_left (fun (cur_full_oct,cur_oct) cstr_var ->
        if VSet.mem cstr_var remove_vars then
(*	  let idx = VMap.find cstr_var cur_oct.indices in*)
	  (* perform Fourier-Motzkin elimination instead of forget
	     operation on constrained variables *)
(*
	  let new_octogon = Oct.fourier_motzkin cur_oct.octogon idx in
	  let new_oct = { cur_oct with octogon = new_octogon } in
	  if is_constrained new_oct then
	    (* Fourier-Motzkin captured the constraint, continue. *)
	    new_oct
	  else
*)
	    (* Fourier-Motzkin with constants *)
	    let p_full_oct = match to_pred cur_full_oct with
		(* constrained octogon must have an associate predicate *)
	      | Some p -> p | None -> assert false
	    in
	    let p_oct = match to_pred cur_oct with
		(* constrained octogon must have an associate predicate *)
	      | Some p -> p | None -> assert false
	    in
	    let p_fm = 
	      VarElim.fourier_motzkin cstr_var ~full:p_full_oct p_oct in
	    if debug then Coptions.lprintf
	      "[eliminate] Result of Fourier-Motzkin is %a@." 
	      V.P.pretty p_fm;
	    let new_oct = 
	      remove_variable cstr_var (eval_constraint p_fm cur_oct)
	    in
	    if debug_more then Coptions.lprintf 
	      "[eliminate] fourier-motzkin on %a@." V.pretty cstr_var;
	    if is_constrained new_oct then
	      (* Fourier-Motzkin captured the constraint, continue. *)
	      normalize new_oct,new_oct
	    else
	      (* try transitivity instead of Fourier-Motzkin *)
	      let p_trans = VarElim.transitivity cstr_var p_oct in
	      let new_oct =
		remove_variable cstr_var (eval_constraint p_trans cur_oct)
	      in
	      normalize new_oct,new_oct
	else cur_full_oct,cur_oct
      ) (full_oct,oct) cstr_vars 
      in
      let new_cstr_vars = constrained_variables new_oct in
      let new_cstr_vars = 
	List.filter (fun v -> not (List.mem v cstr_vars)) new_cstr_vars
      in
      if List.length new_cstr_vars = 0 then
	(* no new constrained variables, elimination by Fourier-Motzkin is 
	   finished *)
	new_oct
      else
	elim new_oct
    in

    let new_oct =
      if is_constrained oct then
	(* octogon is constrained. Treat specially constrained variables. *)
	elim oct
      else oct
    in
(*      let new_oct =
	List.fold_left 
	  (fun cur_oct v ->
	     let cstr_vars = constrained_variables cur_oct in
	     if debug_more then Coptions.lprintf 
	       "[eliminate] list of constrained variables %a@."
	       (print_list comma V.pretty) cstr_vars;
	     let cstr_vars =
	       List.fold_right (fun v s -> VSet.add v s) cstr_vars VSet.empty
	     in
	     let idx = VMap.find v cur_oct.indices in
	     let new_octogon =
	       if VSet.mem v cstr_vars then
		 (* perform Fourier-Motzkin elimination instead of forget
		    operation on constrained variables *)
		 begin
		   if debug_more then Coptions.lprintf 
		     "[eliminate] fourier-motzkin on %a@."
		     V.pretty v;
		   Oct.fourier_motzkin cur_oct.octogon idx
		 end
	       else
		 Oct.forget cur_oct.octogon idx
	     in
	     { cur_oct with octogon = new_octogon }
	  ) oct vl
      in
      if debug_more then Coptions.lprintf 
	  "[eliminate] new octogon %a@." pretty new_oct;
      new_oct
    else
*)
    (* normal octogon or constrained octogon after Fourier-Motzkin elimination.
       Forget variables. *)
    let new_octogon =
      VSet.fold (fun v cur_oct -> 
	let idx = VMap.find v oct.indices in Oct.forget cur_oct idx
      ) remove_vars new_oct.octogon
    in
    { oct with octogon = new_octogon }
end

module Make_ContextualLattice (V : PVARIABLE) (I : INT_VALUE) 
    (Ctxt : PACKED_CLUSTER_LATTICE with module V = V)
    (Cstr : PACKED_CONSTRAINED_LATTICE with module V = V)
    (Brdg : CONTEXTUAL_BRIDGE 
    with module Contxt = Ctxt and module Constr = Cstr)
    : PACKED_CONTEXTUAL_LATTICE 
    with module V = V and module I = I and module Contxt = Ctxt 
    and module Constr = Cstr and module Bridge = Brdg =
struct
  module V = V         module I = I         type var_t = V.t

  module VSet = Set.Make (V)

  module Contxt = Ctxt
  module Constr = Cstr
  module Bridge = Brdg

  (* global counter that uniquely identifies conditional abstract values *)
  let next_id = ref 0

  type cond_t = { joined : bool; do_join : bool; cond : Constr.t }

  type t = 
     {
       main_context : Contxt.t;
       (* conditionals are identified by a unique integer.
	  The conditional may be [Constr.bottom], which means that in 
	  this branch this conditional cannot give information. 
	  This is useful when joining conditionals (e.g. after an [if]) to
	  propagate the fact the conditional cannot be used anymore. 
       *)
       conditionals : cond_t Int31Map.t;
     }

  type dim_t = unit

  let bottom () = { main_context = Contxt.bottom (); 
		    conditionals = Int31Map.empty; }
  let top () = { main_context = Contxt.top (); 
		 conditionals = Int31Map.empty; }

  let init = top

  let mapcond f cond =
    { joined = cond.joined; do_join = cond.do_join; cond = f cond.cond; }
  let opcond f cond = f cond.cond
  let binopcond f cond1 cond2 = f cond1.cond cond2.cond
  let crcond ~joined ~do_join cond = 
    { joined = joined; do_join = do_join; cond = cond; }
  let nwcond ~do_join cond = 
    { joined = false; do_join = do_join; cond = cond; }
  let jncond cond = { joined = true; do_join = true; cond = cond; }

  (* functions for packing *)

  let pack_variables var_llist =
    Contxt.pack_variables var_llist; Constr.pack_variables var_llist

  let is_packed_variable var = 
    Contxt.is_packed_variable var || Constr.is_packed_variable var

  (* lattice operations *)

  let eliminate_conditionals ctxt = { ctxt with conditionals = Int31Map.empty }

  let normalize_separately ctxt =
    let new_main = Contxt.normalize ctxt.main_context in
    let new_cond = Int31Map.map (mapcond Constr.normalize) ctxt.conditionals in
    let new_cond = Int31Map.fold (fun cid cond m ->
      (* remove unconstrained conditionals *)
      if Constr.is_constrained cond.cond then
	Int31Map.add cid cond m
      else m) new_cond Int31Map.empty
    in { main_context = new_main; conditionals = new_cond; }

  let normalize ctxt =
    let ctxt = normalize_separately ctxt in
    if Contxt.is_empty ctxt.main_context then
      (* when the main context becomes empty, remove all conditional
	 information, so that future joins do no take this into account *)
      eliminate_conditionals ctxt
    else
      (* if the left part of a conditional is implied by the main context,
	 add its right part to the main context *)
      let new_main =
	Int31Map.fold (fun cid { cond=cond } cur_main ->
	  if Constr.is_constrained cond then
	    let left_cond = Bridge.get_unconstrained cond in
	    let cur_main = Contxt.normalize cur_main in
	    if debug_more then Coptions.lprintf
		"[eval_test] cur_main %a@." Contxt.pretty cur_main;
	    let cur_test = Contxt.normalize (Contxt.meet cur_main left_cond) in
	    if debug_more then Coptions.lprintf
		"[eval_test] cur_test %a@." Contxt.pretty cur_test;
	    if Contxt.equal cur_main cur_test then
	      (* left part of the conditional is implied by current context *)
	      let unconstr_cond = Bridge.make_unconstrained cond in
	      if debug_more then Coptions.lprintf
		  "[eval_test] add unconstr_cond %a@." 
		  Contxt.pretty unconstr_cond;
	      (* remove conditional, incorporated to main context *)
	      Contxt.meet cur_main unconstr_cond
	    else cur_main
	  else 
	    (* should be forbidden by normalization performed before *)
	    assert false)
	  ctxt.conditionals ctxt.main_context
      in 
      (* in any case, keep the conditional, for correction of future joins
	 with information from other paths *)
      { ctxt with main_context = new_main }

  let finalize ctxt =
    let ctxt = normalize ctxt in
    (* further reduce the context by removing conditionals
       - whose left part is implied by the main context
       (their right part being pushed into this main context by the call to
       [normalize] above)
       - whose right part is implied by the main context, which makes them
       uninformative
    *)
    let cur_main = ctxt.main_context in
    let new_cond = 
      Int31Map.fold (fun cid cond cur_cond ->
	if Constr.is_constrained cond.cond then
	  let joined = cond.joined and do_join = cond.do_join 
				   and cond = cond.cond in
	  let left_cond = Bridge.get_unconstrained cond in
	  let cur_test = Contxt.normalize (Contxt.meet cur_main left_cond) in
	  if Contxt.equal cur_main cur_test then
	    (* left part of the conditional is implied by current context *)
	    cur_cond
	  else 
	    let right_cond = 
	      Bridge.make_unconstrained (Bridge.get_constrained cond) in
	    let cur_test = Contxt.normalize (Contxt.meet cur_main right_cond)
	    in
	    if Contxt.equal cur_main cur_test then
	      (* right part of the conditional is implied by current context *)
	      cur_cond
	    else 
	      (* remove the already known main context from the conditional
		 currently examined, in order to minimize it *)
	      let new_cond = Bridge.subtract cond ctxt.main_context in
	      Int31Map.add cid (crcond ~joined ~do_join new_cond) cur_cond
	else
	  (* should be forbidden by normalization performed before *)
	  assert false)
	ctxt.conditionals Int31Map.empty
    in 
    (* keep the main context computed by [normalize] *)
    { ctxt with conditionals = new_cond; }

  let equal ctxt1 ctxt2 =
    Contxt.equal ctxt1.main_context ctxt2.main_context
    && Int31Map.equal (binopcond Constr.equal)
      ctxt1.conditionals ctxt2.conditionals
      
  let pretty fmt ctxt =
    let cond_list = Int31Map.fold (fun _ cond cl ->
      if Constr.is_empty cond.cond then
	cl
      else cond :: cl) ctxt.conditionals []
    in
    Format.fprintf fmt "main: %a@\ncond: %a" Contxt.pretty ctxt.main_context
      (print_list comma (fun fmt cond -> Constr.pretty fmt cond.cond)) 
      cond_list

  let join ?(backward=false) ctxt1 ctxt2 =
    let new_main = 
      Contxt.join ~backward ctxt1.main_context ctxt2.main_context 
    in
    (* join corresponding conditionals in [ctxt1] and [ctxt2].
       remove simply conditionals that do not have a counterpart. *)
    let new_cond = 
      Int31Map.fold (fun cid cond1 m ->
	try
	  let do_join = cond1.do_join in
	  let cond2 = Int31Map.find cid ctxt2.conditionals in
	  let cond = Constr.join ~backward cond1.cond cond2.cond in
	  Int31Map.add cid (nwcond ~do_join cond) m
  	with Not_found -> m)
	ctxt1.conditionals Int31Map.empty
    in
    { main_context = new_main; conditionals = new_cond; }

  let join_context ctxt1 ctxt2 =
    let new_main = Contxt.join ctxt1.main_context ctxt2.main_context in
    (* join corresponding conditionals in [ctxt1] and [ctxt2].
       join conditionals that do not have a counterpart with the main
       context from the opposite abstract value. *)
    let new_cond = 
      Int31Map.fold (fun cid cond1 m ->
(*	try
	  let cond2 = Int31Map.find cid ctxt2.conditionals in
	  let cond = Constr.join cond1.cond cond2.cond in
	  Int31Map.add cid (jncond cond) m
  	with Not_found -> 
*)
          if not cond1.do_join then Int31Map.add cid cond1 m else
	  let lhs_cond1 = Bridge.get_unconstrained cond1.cond in
	  let add_cond1 = Bridge.meet cond1.cond ctxt1.main_context in
	  if debug_more then Coptions.lprintf
	    "[join_context] add_cond1 %a and ctxt2.main_context %a@." 
	    Constr.pretty add_cond1 Contxt.pretty ctxt2.main_context;
	  let cond = Bridge.join add_cond1 ctxt2.main_context in
	  let rhs_cond = Bridge.get_constrained cond in
	  if debug_more then Coptions.lprintf
	    "[join_context] rhs_cond %a and lhs_cond1 %a@." 
	    Constr.pretty rhs_cond Contxt.pretty lhs_cond1;
	  let cond = Bridge.meet rhs_cond lhs_cond1 in
	  let cond = 
	    if Constr.is_constrained cond then jncond cond
	    else cond1
	  in
	  if debug then Coptions.lprintf
	    "[join_context] Transform condition %a@.[join_context] into %a@."
	    Constr.pretty cond1.cond Constr.pretty cond.cond;
	  Int31Map.add cid cond m)
	ctxt1.conditionals Int31Map.empty
    in
    let new_cond =
      Int31Map.fold (fun cid cond2 m ->
	try
	  let _ = Int31Map.find cid ctxt1.conditionals in
	  m (* already taken care of above *)
  	with Not_found -> 
          if not cond2.do_join then Int31Map.add cid cond2 m else
	  let lhs_cond2 = Bridge.get_unconstrained cond2.cond in
	  let add_cond2 = Bridge.meet cond2.cond ctxt2.main_context in
	  if debug_more then Coptions.lprintf
	    "[join_context] add_cond2 %a and ctxt1.main_context %a@." 
	    Constr.pretty add_cond2 Contxt.pretty ctxt1.main_context;
	  let cond = Bridge.join add_cond2 ctxt1.main_context in
	  let rhs_cond = Bridge.get_constrained cond in
	  if debug_more then Coptions.lprintf
	    "[join_context] rhs_cond %a and lhs_cond2 %a@." 
	    Constr.pretty rhs_cond Contxt.pretty lhs_cond2;
	  let cond = Bridge.meet rhs_cond lhs_cond2 in
	  let cond = 
	    if Constr.is_constrained cond then jncond cond
	    else cond2
	  in
	  if debug then Coptions.lprintf
	    "[join_context] Transform condition %a@.[join_context] into %a@."
	    Constr.pretty cond2.cond Constr.pretty cond.cond;
	  Int31Map.add cid cond m)
	ctxt2.conditionals new_cond
    in
    let new_oct = { main_context = new_main; conditionals = new_cond; } in
    if debug then Coptions.lprintf
      "[join_context] Resulting in context@.[join_context] %a@."
      pretty new_oct;
    new_oct

  let meet ctxt1 ctxt2 =
    let new_main = Contxt.meet ctxt1.main_context ctxt2.main_context in
    (* meet corresponding conditionals in [ctxt1] and [ctxt2].
       add simply conditionals that do not have a counterpart. *)
    let new_cond = 
      Int31Map.fold (fun cid cond1 m ->
	try
	  let do_join = cond1.do_join in
	  let cond2 = Int31Map.find cid ctxt2.conditionals in
	  let cond = Constr.meet cond1.cond cond2.cond in
	  Int31Map.add cid (nwcond ~do_join cond) m
	with Not_found -> Int31Map.add cid cond1 m)
	ctxt1.conditionals ctxt2.conditionals
    in
    { main_context = new_main; conditionals = new_cond; }

  let widening ws ~old_value ~new_value = 
    (* widening has a meaning only for main context *)
    assert (Int31Map.is_empty old_value.conditionals);
    assert (Int31Map.is_empty new_value.conditionals);
    let new_main = 
      Contxt.widening ws old_value.main_context new_value.main_context in
    { old_value with main_context = new_main }

  let to_pred ctxt =
    let pred_main = Contxt.to_pred ctxt.main_context in
    let pred_cond = Int31Map.map (opcond Constr.to_pred) ctxt.conditionals in
    Int31Map.fold (fun _ p1_opt p2_opt -> match p1_opt,p2_opt with
    | None,None -> None
    | None,Some p | Some p,None -> Some p
    | Some p1,Some p2 -> Some (IPand (p1,p2)))
      pred_cond pred_main 

  let restrained_variables ctxt =
    let set_of_list l = List.fold_right VSet.add l VSet.empty in
    let vset = Int31Map.fold 
	(fun _ cond s ->
	  VSet.fold VSet.add 
	    (set_of_list (Constr.restrained_variables cond.cond)) s
	) ctxt.conditionals 
	(set_of_list (Contxt.restrained_variables ctxt.main_context))
    in
    VSet.fold (fun x l -> x :: l) vset []

  let remove_variable var ctxt =
    let new_main = Contxt.remove_variable var ctxt.main_context in
    let new_cond = 
      Int31Map.map (mapcond (Constr.remove_variable var)) ctxt.conditionals 
    in { main_context = new_main; conditionals = new_cond; }

  let remove_variable_conditionals var ctxt =
    let new_cond = 
      Int31Map.map (fun cond ->
		      if cond.joined then cond 
		      else (mapcond (Constr.remove_variable var)) cond
		   ) ctxt.conditionals 
    in { main_context = ctxt.main_context; conditionals = new_cond; }

  let filter_variables ~remove ctxt =
    let restr_vars = restrained_variables ctxt in
    let remove_vars = List.filter remove restr_vars in
    List.fold_right remove_variable remove_vars ctxt

  let eval_assign ~backward var term ctxt =
    let new_main = 
      Contxt.eval_assign ~backward var term ctxt.main_context 
    in
    if Contxt.is_empty new_main then
      (* when the main context becomes empty, remove all conditional
	 information, so that future joins do no take this into account *)
      { main_context = new_main; conditionals = Int31Map.empty; }
    else
      let new_cond = Int31Map.map 
	(mapcond (Constr.eval_assign ~backward var term)) ctxt.conditionals
      in
      { main_context = new_main; conditionals = new_cond; }

  let eval_test ~backward pred ctxt =
    (* keep only constrained conditionals *)
    let ctxt = normalize_separately ctxt in
    let new_main = Contxt.eval_test ~backward pred ctxt.main_context 
    in
    if backward then
      (* during backward propagation, only dispatch test on conditionals *)
      let new_cond = Int31Map.map
	  (mapcond (Constr.eval_test ~backward pred)) ctxt.conditionals in
      { main_context = new_main; conditionals = new_cond; }
    else if Contxt.is_empty new_main then
      (* when the main context becomes empty, remove all conditional
	 information, so that future joins do no take this into account *)
      { main_context = new_main; conditionals = Int31Map.empty; }
    else
      let new_cond = Int31Map.map
	  (mapcond (Constr.eval_test ~backward pred)) ctxt.conditionals in
      (* if the left part of a conditional is implied by the main context,
	 add its right part to the main context.
	 Do this repeatedly until no more conditional is implied. *)
      let rec add_implied_cond cur_main cur_cond =
	let change = ref false in
	let new_main,new_cond =
	  Int31Map.fold (fun cid cond (cur_main,cur_cond) ->
	    if Constr.is_constrained cond.cond then
	      let left_cond = Bridge.get_unconstrained cond.cond in
	      let cur_main = Contxt.normalize cur_main in
	      if debug_more then Coptions.lprintf
		"[eval_test] cur_main %a@." Contxt.pretty cur_main;
	      let cur_test = Contxt.normalize (Contxt.meet cur_main left_cond)
	      in
	      if debug_more then Coptions.lprintf
		"[eval_test] cur_test %a@." Contxt.pretty cur_test;
	      if Contxt.equal cur_main cur_test then
		(* left part of the conditional is implied by current context *)
		let unconstr_cond = Bridge.make_unconstrained cond.cond in
		if debug_more then Coptions.lprintf
		  "[eval_test] add unconstr_cond %a@." 
		  Contxt.pretty unconstr_cond;
		(* remove conditional, incorporated to main context *)
		change := true;
		Contxt.meet cur_main unconstr_cond, cur_cond
	      else 
		(* keep conditional *)
		cur_main, Int31Map.add cid cond cur_cond
	    else 
	      (* should be forbidden by normalization performed before *)
	      assert false)
	    cur_cond (cur_main,Int31Map.empty)
	in
	if !change then add_implied_cond new_main new_cond
	else new_main,new_cond
      in
      let new_main,new_cond = add_implied_cond new_main new_cond
      in { main_context = new_main; conditionals = new_cond; }

  let guarantee_test pred ctxt = Contxt.guarantee_test pred ctxt.main_context

  let eliminate var_list ctxt =
    let new_main = 
      List.fold_right Contxt.remove_variable var_list ctxt.main_context in
    let new_cond = 
      Int31Map.map (mapcond (Constr.eliminate var_list)) ctxt.conditionals 
    in { main_context = new_main; conditionals = new_cond; }

  let get_context ctxt = ctxt.main_context

  let set_context ctxt main = { ctxt with main_context = main; }

  let has_conditionals ctxt = not (Int31Map.is_empty ctxt.conditionals)

  let format_singleton ctxt =
    let cond_list =
      Int31Map.fold (fun cid cond cl -> (cid,cond) :: cl) ctxt.conditionals []
    in
    match cond_list with
    | [cid,cond] ->
	if Constr.is_constrained cond.cond then
	  (* remove the already known main context from the conditional
	     currently examined, in order to minimize it *)
	  cid, cond.do_join, Bridge.subtract cond.cond ctxt.main_context
	else
	  (* return the initial constraint, so that future calls to 
	     [Constr.is_constrained] return [false] *)
	  cid, cond.do_join, cond.cond
    | _ -> failwith ("[format_singleton] should be passed only"
		       ^ " unique conditionals")

  let is_empty ctxt =
    let ctxt = normalize ctxt in Contxt.is_empty ctxt.main_context

  let is_full ctxt = 
    let ctxt = normalize ctxt in Contxt.is_full ctxt.main_context

  let add_conditional ctxt ~do_join (cid,cond) =
    let new_cond = Int31Map.add cid (nwcond ~do_join cond) ctxt.conditionals in
    { ctxt with conditionals = new_cond; }

  let add_new_conditional ctxt ~do_join cond =
    let new_cid = !next_id in incr next_id; 
    add_conditional ctxt ~do_join (new_cid,cond)

  let transform f g ctxt =
    let new_main = f ctxt.main_context in
    let new_cond = Int31Map.map (mapcond g) ctxt.conditionals in
    { main_context = new_main; conditionals = new_cond; }    

  let fold f g ctxt init =
    Int31Map.fold (fun _ cond acc -> g cond.cond acc) ctxt.conditionals 
      (f ctxt.main_context init)

  let subtract ctxt1 ctxt2 =
    let new_main = Contxt.subtract ctxt1.main_context ctxt2.main_context in
    let new_cond =
      Int31Map.fold (fun cid cond1 m ->
	try
	  let do_join = cond1.do_join in
	  let cond2 = Int31Map.find cid ctxt2.conditionals in
	  let new_cond = Constr.subtract cond1.cond cond2.cond in
	  if Constr.is_constrained new_cond then
	    Int31Map.add cid (nwcond ~do_join new_cond) m
	  else m
  	with Not_found -> Int31Map.add cid cond1 m)
	ctxt1.conditionals Int31Map.empty
    in
    { main_context = new_main; conditionals = new_cond; }
      
end

module Make_SeparationLattice (V : PVARIABLE) (I : INT_VALUE) 
    : SEPARATION_LATTICE with module V = V and module I = I =
struct
  module V = V         module I = I         type var_t = V.t

  module VSet = Set.Make (V)
  module VPair = Pair.Make (V) (V)
  module VPSet = Set.Make (VPair)

  type t = VPSet.t

  type dim_t = unit

  let top () = VPSet.empty
  let bottom () = VPSet.empty
  let init = bottom

  let equal = VPSet.equal

  let pretty fmt seps =
    let sepl = VPSet.fold (fun sep sl -> sep :: sl) seps [] in
    print_list comma (fun fmt sep -> Format.fprintf fmt "full_separated(%a,%a)"
	V.pretty (fst sep) V.pretty (snd sep)) fmt sepl
    
  let join ?(backward=false) = VPSet.inter

  let meet = VPSet.union

  let widening ws ~old_value ~new_value = join old_value new_value

  (* not used here *)
  let pack_variables _ = ()
  let is_packed_variable _ = false
  let guarantee_test _ _ = false

  let add_separated_pair v1 v2 seps = 
    if V.equal v1 v2 then seps else VPSet.add (v1,v2) seps

  let get_separated_pairs seps =
    VPSet.fold (fun sep sl -> sep :: sl) seps []

  let fully_separated v1 v2 seps =
    VPSet.exists (fun sep -> (V.equal (fst sep) v1 && V.equal (snd sep) v2)
    || (V.equal (fst sep) v2 && V.equal (snd sep) v1)) seps

  let eval_test ~backward pred seps = 
    let preds = V.P.get_conjuncts pred in
    (* allow pointer arithmetic *)
    let rec get_pair t1 t2 = match t1,t2 with
      | ITvar v1,ITvar v2 -> Some (v1,v2)
      | ITbinop (t1,_,_),t2 -> get_pair t1 t2
      | t1,ITbinop (t2,_,_) -> get_pair t1 t2
      | _ -> None
    in
    List.fold_left 
      (fun seps p -> match p with
	 | IPfull_separated (t1,t2) -> 
	     begin match get_pair t1 t2 with
	       | None -> seps
	       | Some (v1,v2) ->
		   add_separated_pair v1 v2 seps
	     end
	 | _ -> seps) seps preds
      
  let sep_to_pred sep = 
    Some (IPfull_separated (ITvar (fst sep),ITvar (snd sep)))

  let from_pred pred = eval_test ~backward:false pred (bottom ())

  let to_pred seps =
    VPSet.fold (fun sep p_opt ->
      match p_opt,sep_to_pred sep with
      | None,None -> None
      | None,Some p | Some p,None -> Some p
      | Some p1,Some p2 -> Some (IPand (p1,p2))) seps None

  let restrained_variables seps =
    let vset = VPSet.fold (fun sep s ->
      VSet.add (fst sep) (VSet.add (snd sep) s)) seps VSet.empty
    in
    VSet.fold (fun x l -> x :: l) vset []

  let remove_variable var seps =
    VPSet.filter 
      (fun sep -> not (V.equal var (fst sep) || V.equal var (snd sep))) seps

  let normalize seps = seps
  let finalize = normalize

  let subtract = VPSet.diff

  let is_empty = VPSet.is_empty
  let is_full _ = false

  let eval_assign ~backward var term seps = remove_variable var seps

end

module Make_ReadWriteLattice (V : PVARIABLE) (I : INT_VALUE)
    (S : SEPARATION_LATTICE with module V = V and module I = I)
    : READ_WRITE_LATTICE with module V = V and module I = I =
struct
  module V = V         module I = I         type var_t = V.t

  type access_t =
    | Read of V.t
    | Write of V.t
    | Param of V.t

  module AccessNode =
  struct
    type t = access_t
    let equal = ( = )
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let get_variable = function
      | Read v -> v
      | Write v -> v
      | Param v -> v
  end

  module Self = Graph.Persistent.Graph.Concrete (AccessNode)

  type t = Self.t
  type dim_t = unit

  let top () = Self.empty
  let bottom () = Self.empty
  let init = bottom

  let equal g1 g2 = (g1 == g2)

  let join ?(backward=false) g1 g2 =
    let g = Self.fold_vertex (fun v g -> Self.add_vertex g v) g1 g2 in
    Self.fold_edges (fun v1 v2 g -> Self.add_edge g v1 v2) g1 g

  let meet _ _ = failwith "Not implemented"

  let widening ws ~old_value ~new_value = join old_value new_value

  (* not used here *)
  let pack_variables _ = ()
  let is_packed_variable _ = false
  let guarantee_test _ _ = false

  let eval_test ~backward pred g = g
    
  let edge_to_pred v1 v2 = 
    let v1 = AccessNode.get_variable v1 in
    let v2 = AccessNode.get_variable v2 in
    Some (IPfull_separated (ITvar v1,ITvar v2))

  let to_pred g =
    Self.fold_edges (fun v1 v2 p_opt ->
      match p_opt,edge_to_pred v1 v2 with
      | None,None -> None
      | None,Some p | Some p,None -> Some p
      | Some p1,Some p2 -> Some (IPand (p1,p2))) g None

  let pretty fmt g = match to_pred g with
    | None -> Format.fprintf fmt "empty graph"
    | Some p -> V.P.pretty fmt p

  let restrained_variables _ = failwith "Not implemented"

  let remove_variable var g = 
    let new_g = Self.remove_vertex g (Read var) in
    let new_g = Self.remove_vertex new_g (Write var) in
    Self.remove_vertex new_g (Param var)
    
  let normalize g = g
  let finalize = normalize

  let subtract g1 g2 =
    Self.fold_edges (fun v1 v2 g -> Self.remove_edge g v1 v2) g2 g1

  let is_empty = Self.is_empty
  let is_full _ = false

  let eval_assign ~backward var term g = remove_variable var g

  let eval_read v g =
    if debug_more then Coptions.lprintf
      "[eval_read] read %a, current %a@." V.pretty v pretty g;
    Self.add_vertex g (Read v)

  let eval_write v g =
    if debug_more then Coptions.lprintf
      "[eval_write] write %a, current %a@." V.pretty v pretty g;
    let new_g = Self.add_vertex g (Write v) in
    Self.fold_vertex 
      (fun w g -> match w with
	 | Write _ | Param _ -> g
	 | Read _ -> Self.add_edge g w (Write v)) g new_g

  let eval_precondition p g =
    if debug_more then Coptions.lprintf
      "[eval_separation] sep %a, current %a@." 
      V.P.pretty p pretty g;
    let sepl = S.get_separated_pairs (S.from_pred p) in
    List.fold_left 
      (fun new_g (v1,v2) ->
	 let new_g = Self.add_vertex new_g (Param v1) in
	 let new_g = Self.add_vertex new_g (Param v2) in
	 Self.add_edge new_g (Param v1) (Param v2)
      ) g sepl
end

module Make_PredicateLattice (V : PVARIABLE) (I : INT_VALUE) 
  : PACKED_CLUSTER_LATTICE with module V = V and module I = I =
struct
  module V = V         module I = I         type var_t = V.t
  
  type state_t = {
    (* abstract state captured by the current predicate abstraction *)
    state : bool V.PM.t;
    (* dependencies between variables and the predicates they appear in *)
    deps : V.PS.t V.M.t;
  }

  type t = NORMAL of state_t | EMPTY

  type dim_t = unit

  let bottom () = EMPTY
  let top () = NORMAL { state = V.PM.empty; deps = V.M.empty; }
  let init = top

  let is_full = function
    | EMPTY -> false
    | NORMAL ps -> V.PM.is_empty ps.state

  let is_empty = function
    | EMPTY -> true
    | NORMAL _ -> false

  (* [state] field captures all relevant information for 
     an external description *)
  let equal ps1 ps2 = match ps1,ps2 with
    | EMPTY,EMPTY -> true
    | EMPTY,_ | _,EMPTY -> false
    | NORMAL ps1,NORMAL ps2 -> V.PM.equal ( = ) ps1.state ps2.state

  let rec norm_term t = match t with
    | ITconstant _ | ITvar _ | ITmax _ | ITmin _ -> t
    | ITunop (op,t1) -> ITunop (op,norm_term t1)
    | ITbinop (t1,op,t2) -> ITbinop (norm_term t1,op,norm_term t2)
    | ITany -> failwith "term ITany"

  let norm_predicate p = 
    try match p with
      | IPrel (t1,Eq,t2) -> Some (IPrel (norm_term t1,Eq,norm_term t2),true)
      | IPrel (t1,Neq,t2) -> Some (IPrel (norm_term t1,Eq,norm_term t2),false)
      | IPnull_pointer t1 -> Some (IPnull_pointer (norm_term t1),true)
      | IPnot_null_pointer t1 -> Some (IPnull_pointer (norm_term t1),false)
      | IPnull_char_pointed (t1,t2) -> 
	  Some (IPnull_char_pointed (norm_term t1,norm_term t2),true)
      | IPnot_null_char_pointed (t1,t2) -> 
	  Some (IPnull_char_pointed (norm_term t1,norm_term t2),false)
      | IPimplies _ | IPiff _ | IPnot _ ->
	  (* these constructs should have been removed by the call to
	     [explicit_pred] *)
          assert false
      | IPand _ ->
	  (* these constructs should have been removed by the call to
	     [get_conjuncts] *)
	  assert false
      | IPfalse | IPtrue | IPrel _ | IPor _ | IPfull_separated _ | IPany -> 
	  None
    with Failure "term ITany" -> None

  let denorm_predicate p = function
    | true -> p
    | false -> IPnot p

  let pretty fmt = function
    | EMPTY -> Format.fprintf fmt "EMPTY"
    | NORMAL ps ->
	let pl = 
	  V.PM.fold (fun p b acc_l -> denorm_predicate p b :: acc_l) ps.state []
	in
	Format.fprintf fmt "NORMAL(%a)"
	  (print_list (fun fmt () -> Format.fprintf fmt " && ") V.P.pretty) pl

  let join ?(backward=false) ps1 ps2 = match ps1,ps2 with
    | EMPTY,ps | ps,EMPTY -> ps
    | NORMAL ps1,NORMAL ps2 ->
	let new_state = 
	  V.PM.fold (fun p b1 acc_m ->
		       try
			 let b2 = V.PM.find p ps2.state in
			 if b1 = b2 then
			   (* [ps1] and [ps2] agree on the predicate [p] *)
			   V.PM.add p b1 acc_m
			 else acc_m
		       with Not_found -> acc_m
  		    ) ps1.state V.PM.empty
	in
	(* do not recompute exact dependencies, rather compute a superset of
	   the current dependencies *)
	let new_deps = 
	  V.M.fold (fun v ps1 acc_m ->
		      try 
			let ps2 = V.M.find v ps2.deps in
			V.M.add v (V.PS.union ps1 ps2) acc_m
		      with Not_found ->
			V.M.add v ps1 acc_m
		   ) ps1.deps ps2.deps
	in
	NORMAL { state = new_state; deps = new_deps; }
	  
  let meet ps1 ps2 = match ps1,ps2 with
    | EMPTY,ps | ps,EMPTY -> EMPTY
    | NORMAL ps1,NORMAL ps2 ->
	try
	  let new_state = 
	    V.PM.fold (fun p b1 acc_m ->
			 try
			   let b2 = V.PM.find p ps2.state in
			   if b1 = b2 then
			     (* [ps1] and [ps2] agree on the predicate [p] *)
			     V.PM.add p b1 acc_m
			   else 
			     failwith "No agreement"
			 with Not_found -> V.PM.add p b1 acc_m
  		      ) ps1.state ps2.state
	  in
	  (* exact dependencies are here the sum of [ps1] and [ps2] current
	     dependencies *)
	  let new_deps = 
	    V.M.fold (fun v ps1 acc_m ->
			try 
			  let ps2 = V.M.find v ps2.deps in
			  V.M.add v (V.PS.union ps1 ps2) acc_m
			with Not_found ->
			  V.M.add v ps1 acc_m
		     ) ps1.deps ps2.deps
	  in
	  NORMAL { state = new_state; deps = new_deps; }

	with Failure "No agreement" -> EMPTY

  let widening ws ~old_value ~new_value = old_value

  let is_packed_variable v = true
  let pack_variables _ = ()

  let normalize = function
    | EMPTY -> EMPTY
    | NORMAL ps ->
	(* compute the most accurate dependencies *)
	let new_deps =
	  V.PM.fold (fun p _ acc_m ->
		       let p_vars = V.P.collect_predicate_vars p in
		       List.fold_left (fun m v ->
					 try
					   let pset = V.M.find v m in
					   let new_pset = V.PS.add p pset in
					   V.M.add v new_pset m
					 with Not_found ->
					   let new_pset = V.PS.singleton p in
					   V.M.add v new_pset m
				      ) acc_m p_vars
		    ) ps.state V.M.empty
	in
	NORMAL { state = ps.state; deps = new_deps; }
	
  let finalize = normalize

  let restrained_variables ps = match normalize ps with
    | EMPTY -> []
    | NORMAL ps ->
	V.M.fold (fun v _ acc_l -> v :: acc_l) ps.deps []
	  
  let to_pred = function
    | EMPTY -> None
    | NORMAL ps ->
	let plist = 
	  V.PM.fold (fun p b acc_l -> denorm_predicate p b :: acc_l) ps.state []
	in
	match V.P.make_conjunct plist with
	  | IPtrue -> None
	  | p -> Some p

  let subtract ps1 ps2 = match ps1,ps2 with
    | EMPTY,_ -> EMPTY
    | ps1,EMPTY -> ps1
    | NORMAL ps1,NORMAL ps2 ->
	let new_state = 
	  V.PM.fold (fun p b1 acc_m ->
		       try
			 let b2 = V.PM.find p ps2.state in
			 if b1 = b2 then
			   (* [ps1] and [ps2] agree on the predicate [p] *)
			   acc_m
			 else 
			   V.PM.add p b1 acc_m
		       with Not_found -> V.PM.add p b1 acc_m
  		    ) ps1.state V.PM.empty
	in
	NORMAL { state = new_state; deps = ps1.deps; }	

  let remove_variable var = function
    | EMPTY -> EMPTY
    | NORMAL ps ->
	begin try 
	  let pset = V.M.find var ps.deps in
	  let new_state = 
	    V.PM.fold (fun p b acc_m ->
			 if V.PS.mem p pset then acc_m
			 else V.PM.add p b acc_m
		      ) ps.state V.PM.empty
	  in
	  (* do not recompute exact dependencies, rather compute 
	     a superset of the current dependencies *)
	  let new_deps = V.M.remove var ps.deps in
	  NORMAL { state = new_state; deps = new_deps; }
	with Not_found ->
	  NORMAL ps
	end

  let guarantee_test _ _ = false

  let singleton p = 
    let p = V.translate_predicate [] (V.P.explicit_pred p) in
    if debug_more then Coptions.lprintf
      "[singleton] translated predicate %a@." V.P.pretty p;
    match norm_predicate p with
      | None -> top ()
      | Some (p,b) -> 
	  let p_vars = V.P.collect_predicate_vars p in
	  let singl_state = V.PM.add p b V.PM.empty in
	  let p_singl = V.PS.singleton p in
	  let singl_deps =
	    List.fold_left (fun acc_m v -> V.M.add v p_singl acc_m)
	      V.M.empty p_vars
	  in
	  if debug_more then Coptions.lprintf
	    "[singleton] norm predicate (%a,%B)@." V.P.pretty p b;
	  NORMAL { state = singl_state; deps = singl_deps; }

  let eval_test ~backward pred = function _ -> top()
(* Bug with predicate and strcpy
    | EMPTY -> EMPTY
    | NORMAL _ as ps -> 
	let plist = V.P.get_conjuncts pred in
	let pslist = List.map singleton plist in
	List.fold_left meet ps pslist
*)

  let eval_assign ~backward var term ps = remove_variable var ps

end

module Make_InternalPairLattice (V : PVARIABLE) (I : INT_VALUE) 
  (L1 : PACKED_CLUSTER_LATTICE with module V = V and module I = I)
  (L2 : PACKED_CLUSTER_LATTICE with module V = V and module I = I) 
(*  : PACKED_CLUSTER_LATTICE with module V = V and module I = I 
			   and type t = L1.t * L2.t *) =
struct
  module V = V         module I = I         type var_t = V.t

  include Make_PairLattice(L1)(L2)

  let eval_assign ~backward var term (v1,v2) =
    (L1.eval_assign ~backward var term v1, L2.eval_assign ~backward var term v2)

  let eval_test ~backward pred (v1,v2) =
    (L1.eval_test ~backward pred v1, L2.eval_test ~backward pred v2)

  let remove_variable var (v1,v2) =
    (L1.remove_variable var v1, L2.remove_variable var v2)

  let normalize (v1,v2) = (L1.normalize v1, L2.normalize v2)

  let finalize (v1,v2) = (L1.finalize v1, L2.finalize v2)

  let subtract (v1,v2) (v3,v4) = (L1.subtract v1 v3, L2.subtract v2 v4)

  let restrained_variables (v1,v2) =
    L1.restrained_variables v1 @ (L2.restrained_variables v2)

  let is_empty (v1,v2) = L1.is_empty v1 || (L2.is_empty v2)

  let is_full (v1,v2) = L1.is_full v1 || (L2.is_full v2)
    
  let pack_variables vllist = L1.pack_variables vllist; L2.pack_variables vllist

  let is_packed_variable var =
    L1.is_packed_variable var || L2.is_packed_variable var

end

module Make_ClusterPairLattice (V : PVARIABLE) (I : INT_VALUE) 
  (L1 : PACKED_CLUSTER_LATTICE with module V = V and module I = I)
  (L2 : PACKED_CLUSTER_LATTICE with module V = V and module I = I) 
  : PACKED_CLUSTER_LATTICE with module V = V and module I = I 
			   and type t = L1.t * L2.t =
struct
  
  include Make_InternalPairLattice(V)(I)(L1)(L2)

  let guarantee_test pred (v1,v2) =
    L1.guarantee_test pred v1 || (L2.guarantee_test pred v2)

  let to_pred (v1,v2) = 
    match L1.to_pred v1,L2.to_pred v2 with
      | None,p | p,None -> p
      | Some p1,Some p2 -> Some (V.P.make_conjunct [p1;p2])

end

module Make_ConstrainedPairLattice (V : PVARIABLE) (I : INT_VALUE) 
  (L1 : PACKED_CLUSTER_LATTICE with module V = V and module I = I)
  (L2 : PACKED_CONSTRAINED_LATTICE with module V = V and module I = I) 
  : PACKED_CONSTRAINED_LATTICE with module V = V and module I = I 
			       and type t = L1.t * L2.t =
struct
  
  include Make_InternalPairLattice(V)(I)(L1)(L2)

  let guarantee_test pred (v1,v2) =
    L1.guarantee_test pred v1 || 
      (L1.is_full v1 && L2.guarantee_test pred v2)

  let to_pred (v1,v2) =
    match L1.to_pred v1,L2.to_pred v2 with
      | None,p | p,None -> p
      | Some p1,Some p2 -> 
	  let lhs_p2,rhs_p2 = V.P.get_implicants p2 in
	  let new_lhs = V.P.make_conjunct [p1;lhs_p2] in
	  Some (V.P.make_implication new_lhs rhs_p2)
    
  let eliminate vlist (v1,v2) = (v1,L2.eliminate vlist v2)

  let eval_constraint pred (v1,v2) = (v1,L2.eval_constraint pred v2)

  let is_constrained (v1,v2) = L2.is_constrained v2

  let get_unconstrained (v1,v2) = (v1,L2.get_unconstrained v2)

  let get_constrained (v1,v2) = (L1.top(),L2.get_constrained v2)

  let make_unconstrained (v1,v2) = (v1,L2.make_unconstrained v2)

  let unconstrained_variables (v1,v2) = 
    L1.restrained_variables v1 @ (L2.unconstrained_variables v2)

end


(*****************************************************************************
 *                                                                           *
 * 		Concrete modules for integer value analysis		     *
 *                                                                           *
 *****************************************************************************)

(* type used for generated terms *)
let int_term_type = Ctypes.c_int

(* type of a variable for integer analysis:
   - [Vvar v] describes a normal variable [v]
   - [Varrlen v] describes a logical variable, the application of the logical
   function [arrlen] to the normal variable [v]. The intended meaning is 
   the length of the typed array of elements pointed-to by [v] (possibly 1).
   - [Vstrlen v] describes a logical variable, the application of the logical
   function [strlen] to the normal variable [v]. The intended meaning is 
   the length of the string pointed-to by [v].
   - [Vterm t] represents the term [t]
*)
module Var : sig
  type var_t = 
    | Vvar of var_info
    | Varrlen of var_info
    | Vstrlen of var_info
    | Vterm of var_t int_term
  include PVARIABLE with type t = var_t
  val get_variable : t -> var_info
  val is_strlen : t -> bool
  val is_arrlen : t -> bool
  val safe_access_predicate : 
    ?read_string:bool -> ?after_read_string:bool 
    -> var_info -> t int_term -> t int_predicate
  val string_predicate : var_info -> t int_predicate
  val pointer_predicate : ?non_null:bool -> var_info -> t int_predicate
  val arrlen_term : T.t -> T.t
  val strlen_term : T.t -> T.t
end = struct

  type var_t = 
    | Vvar of var_info
    | Varrlen of var_info
    | Vstrlen of var_info
    | Vterm of var_t int_term

  module Self : ELEMENT_OF_CONTAINER with type t = var_t = 
  struct
    type t = var_t
    let compare = Pervasives.compare
    let equal = ( = )
    let hash = Hashtbl.hash
    let rec pretty fmt v = match v with
      | Vvar v -> Format.fprintf fmt "%s" v.var_name
      | Varrlen v -> Format.fprintf fmt "\\arrlen(%s)" v.var_name
      | Vstrlen v -> Format.fprintf fmt "\\strlen(%s)" v.var_name
      | Vterm _ -> Format.fprintf fmt "(term)" 
  end

  include Self

  exception Introduce_variable of t

  module S = Set.Make (Self)
  module M = Map.Make (Self)
  module H = Hashtbl.Make (Self)

  type iterm = t int_term
  type ipredicate = t int_predicate

  module T = TermOfVariable (Self)
  module P = PredicateOfVariable (Self) (T)

  module TS = Set.Make (T)
  module TM = Map.Make (T)
  module TH = Hashtbl.Make (T)

  module PS = Set.Make (P)
  module PM = Map.Make (P)
  module PH = Hashtbl.Make (P)

  let rec to_string v = match v with
    | Vvar v -> v.var_name
    | Varrlen v -> "\\arrlen(" ^ v.var_name ^ ")"
    | Vstrlen v -> "\\strlen(" ^ v.var_name ^ ")"
    | Vterm t -> "(term)"

  let get_variable = function
    | Vvar v | Varrlen v | Vstrlen v -> v
    | Vterm _ ->
	failwith "No single variable for term meta-variables"
  let is_strlen = function
    | Vvar _ | Varrlen _ | Vterm _ -> false
    | Vstrlen _ -> true
  let is_arrlen = function
    | Vvar _ | Vstrlen _ | Vterm _ -> false
    | Varrlen _ -> true

  let safe_access_predicate 
      ?(read_string=false) ?(after_read_string=false) v t_off =
    (* build the safe access predicate *)
    let t_upbound,op_upbound = 
      if read_string then ITvar (Vstrlen v),Clogic.Le
      else if after_read_string then ITvar (Vstrlen v),Clogic.Lt
      else ITvar (Varrlen v),Clogic.Lt
    in
    (* after string read ? offset < strlen(v) 
       string read ?       offset <= strlen(v) 
       other ?             offset < arrlen(v)
    *)
    let p_upsafe = IPrel (t_off,op_upbound,t_upbound) in
    let t_downbound = ITconstant (IntConstant "0") in
    (* offset >= 0 *)
    let p_downsafe = IPrel (t_off,Clogic.Ge,t_downbound) in
    IPand (p_upsafe,p_downsafe) 

  let string_predicate v =
    (* build the predicate that [v] is a string *)
    let strlen = ITvar (Vstrlen v) in
(*    let arrlen = ITvar (Varrlen v) in*)
    let zero = ITconstant (IntConstant "0") in
    (* 0 <= strlen(v) *)
    let p_lower = IPrel (zero,Clogic.Le,strlen) in
(*    (* strlen(v) < arrlen(v) *)
    let p_upper = IPrel (strlen,Clogic.Lt,arrlen) in
    IPand (p_lower,p_upper)
*)
    p_lower

  let pointer_predicate ?(non_null=false) v =
    (* build the predicate that [v] is a "valid" pointer: either null or
       pointing to some valid memory block *)
    let arrlen = ITvar (Varrlen v) in
    let zero = ITconstant (IntConstant "0") in
    (* non_null ? arrlen(v) > 0 
       other ?    arrlen(v) >= 0
    *)
    let op = if non_null then Clogic.Lt else Clogic.Le in
    IPrel (zero,op,arrlen)

  let translate_predicate restrained_variables p = 
    let rec trans = function
      | IPnull_pointer (ITvar (Vvar v)) -> IPany
      | IPnull_pointer _ -> IPany
      | IPnot_null_pointer (ITvar (Vvar v)) -> IPany
      | IPnot_null_pointer _ -> IPany
      | IPnull_char_pointed (ITvar (Vvar v), off) -> 
	  let strlen_var = Vstrlen v in
	  if not Coptions.absint_as_proof 
	    || List.mem strlen_var restrained_variables then
	    (* [p[offset] == 0] translated by [strlen(p) == offset] 
	       for strings *)
	    IPrel (ITvar strlen_var, Eq, off)
	  else
	    IPany
      | IPnull_char_pointed _ -> IPany
      | IPnot_null_char_pointed (ITvar (Vvar v), off) -> 
	  let strlen_var = Vstrlen v in
	  if not Coptions.absint_as_proof 
	    || List.mem strlen_var restrained_variables then
	    (* [p[offset] != 0] translated by [strlen(p) > offset] 
	       for strings *)
	    IPrel (ITvar (Vstrlen v), Gt, off)
	  else
	    IPany
      | IPnot_null_char_pointed _ -> IPany
      | IPfalse | IPtrue | IPany | IPrel _ | IPfull_separated _ as p -> p
      | IPand (p1,p2) -> IPand (trans p1, trans p2)
      | IPor (p1,p2) -> IPor (trans p1, trans p2)
      | IPimplies (p1,p2) -> IPimplies (trans p1, trans p2)
      | IPiff (p1,p2) -> IPiff (trans p1, trans p2)
      | IPnot p1 -> IPnot (trans p1)
    in
    trans p

  let generate_variable t = Vterm t

  let strlen_term t = match t with
  | ITvar (Vvar v) -> ITvar (Vstrlen v)
  | _ -> ITany

  let rec arrlen_term t = match t with
  | ITvar (Vvar v) -> ITvar (Varrlen v)
  | ITbinop (t1,op,t2) -> 
      begin match arrlen_term t1 with
      | ITany -> ITany
      | arrt1 -> ITbinop (arrt1,op,t2)
      end
  | _ -> ITany

end

module VarAsPVARIABLE : PVARIABLE 
  with type t = Var.t and type S.t = Var.S.t and type 'a M.t = 'a Var.M.t
  and type 'a H.t = 'a Var.H.t 

  and type PS.t = Var.PS.t and type 'a PM.t = 'a Var.PM.t
  and type 'a PH.t = 'a Var.PH.t

  and type TS.t = Var.TS.t and type 'a TM.t = 'a Var.TM.t
  and type 'a TH.t = 'a Var.TH.t
= Var

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)

(* basic integer module based on Int32 *)
module Int : INT_VALUE with type t = int32 =
struct
  include Int32
  let length a b = add (sub b a) 1l
  let is_one a = a = one
  let is_zero a = a = zero
  let eq a b = a = b
  let lt a b = (compare a b < 0)
  let le a b = (compare a b <= 0)
  let gt a b = (compare a b > 0)
  let ge a b = (compare a b >= 0)
  let min a b = if le a b then a else b
  let max a b = if gt a b then a else b
  let pretty fmt a = Format.fprintf fmt "%ld" a
end

(* specialized intermediate language for integer analysis, built upon 
   CFGLangFromNormalized *)
module IntLangFromNormalized : sig

  include CFG_LANG_EXTERNAL

  (* query functions *)

    (* is this expression a binary arithmetic operation ? *)
  val expr_is_int_arith_binop : Node.t -> bool
    (* is this expression a binary logical operation ? *)
  val expr_is_logic_binop : Node.t -> bool
    (* is this expression a binary comparison operation ? *)
  val expr_is_compar_binop : Node.t -> bool
    (* get the operands of this binary operation *)
  val binop_get_operands : Node.t -> Node.t * Node.t
    (* is this expression a unary arithmetic operation ? *)
  val expr_is_int_arith_unop : Node.t -> bool
    (* is this expression a unary logical operation ? *)
  val expr_is_logic_unop : Node.t -> bool
    (* get the operand of this unary arithmetic operation *)
  val unop_get_operand : Node.t -> Node.t
    (* is this expression an addition ? *)
  val expr_is_add : Node.t -> bool
    (* is this expression a substraction ? *)
  val expr_is_sub : Node.t -> bool
    (* is this expression a multiplication ? *)
  val expr_is_mul : Node.t -> bool
    (* is this expression a division ? *)
  val expr_is_div : Node.t -> bool
    (* is this expression a negation ? *)
  val expr_is_neg : Node.t -> bool
    (* is this expression an identity ? Used for [Uplus] and [Uint_conversion]
       unary operators. *)
  val expr_is_id : Node.t -> bool
    (* is this expression a logical and ? *)
  val expr_is_and : Node.t -> bool
    (* is this expression a logical or ? *)
  val expr_is_or : Node.t -> bool
    (* is this expression a logical not ? *)
  val expr_is_not : Node.t -> bool
    (* is this expression a less than comparison ? *)
  val expr_is_less_than : Node.t -> bool
    (* is this expression a greater than comparison ? *)
  val expr_is_greater_than : Node.t -> bool
    (* is this expression a less or equal comparison ? *)
  val expr_is_less_or_equal : Node.t -> bool
    (* is this expression a greater or equal comparison ? *)
  val expr_is_greater_or_equal : Node.t -> bool
    (* is this expression an equality test ? *)
  val expr_is_equal : Node.t -> bool
    (* is this expression a disequality test ? *)
  val expr_is_not_equal : Node.t -> bool

  (* transformations *)

    (* push logical not inside as far as possible *)
  val push_not_inside : Node.t -> Node.t
    (* expand equal as 2 inequalities *)
  val expand_equal : Node.t -> Node.t
    (* takes as input a node and a term corresponding to the same assignment.
       In case it is an increment/decrement, patch it to add/subtract 1. *)
  val patch_term_for_incr_decr : Node.t -> Var.T.t -> Var.T.t
    (* translate an expression into a side-effect free term *)
  val from_expr : Node.t -> Var.T.t
    (* translate an test into a side-effect free predicate *)
  val from_test : Node.t -> Var.P.t
    (* translate a normalized predicate into an integer predicate *)
  val from_pred : Node.t -> Var.P.t

  (* constructors *)

    (* create a new assume statement *)
  val change_in_assume_stat : Node.t -> Var.P.t -> Node.t
    (* modify an existing assume/assert predicate. The optional node gives
       already known information. *)
  val grow_predicate : Node.t -> Node.t option -> Var.P.t -> Node.t
    (* create a predicate that expresses a safe access, if given a node
       targetted by the analysis. The first argument tells whether a given
       variable is to be considered or not. *)
  val memory_access_safe_predicate : 
    (Var.t -> bool) -> Var.t list -> Node.t -> Var.P.t option
    (* return a predicate that expresses call precondition, if any *)
  val call_precondition : Node.t -> Var.P.t option
    (* create a predicate that expresses a variable is a string, when 
       accessing a string in test (against zero) *)
  val string_access_predicate : Node.t -> Var.P.t option
    (* change type of dereference expression to make it a safe access *)
  val make_safe_access : Node.t -> Node.t
    (* derefence is safe by construction (e.g. array initialization) *)
  val is_safe_access : Node.t -> bool

end = struct
  
  include CFGLangFromNormalized

  let expr_is_selected_binop select node = match get_expr node with
    | NEbinary (_,op,_) -> select op
    | _ -> false

  let expr_is_int_arith_binop node = 
    let is_arith op = match op with
      | Badd_int _ | Bsub_int _ | Bmul_int _ | Bdiv_int _ -> true
      | _ -> false
    in
    expr_is_selected_binop is_arith node

  let expr_is_logic_binop node = 
    let is_logic op = match op with
      | Band | Bor -> true
      | _ -> false
    in
    expr_is_selected_binop is_logic node

  let expr_is_compar_binop node =
    let is_compar op = match op with
      | Blt | Bgt | Ble | Bge | Beq | Bneq -> true
      | _ -> false
    in
    expr_is_selected_binop is_compar node

  let binop_get_operands node = match get_expr node with
    | NEbinary (e1,_,e2) -> 
	create_tmp_node (Nexpr e1),create_tmp_node (Nexpr e2)
    | _ -> assert false

  let expr_is_selected_unop select node = match get_expr node with
    | NEunary (op,_) -> select op
    | _ -> false

  let expr_is_int_arith_unop node = 
    let is_arith op = match op with
      | Uplus | Uminus | Uint_conversion -> true
      | _ -> false
    in
    expr_is_selected_unop is_arith node

  let expr_is_logic_unop node =
    let is_logic op = match op with
      | Unot -> true
      | _ -> false
    in
    expr_is_selected_unop is_logic node

  let unop_get_operand node = match get_expr node with
    | NEunary (_,e1) -> 
	create_tmp_node (Nexpr e1)
    | _ -> assert false

  (* arithmetic operations *)

  let expr_is_add node = match get_expr node with
    | NEbinary (_,Badd_int _,_) -> true
    | _ -> false

  let expr_is_sub node = match get_expr node with
    | NEbinary (_,Bsub_int _,_) -> true
    | _ -> false

  let expr_is_mul node = match get_expr node with
    | NEbinary (_,Bmul_int _,_) -> true
    | _ -> false

  let expr_is_div node = match get_expr node with
    | NEbinary (_,Bdiv_int _,_) -> true
    | _ -> false

  let expr_is_neg node = match get_expr node with
    | NEunary (Uminus,e1) -> true
    | _ -> false

  let expr_is_id node = match get_expr node with
    | NEunary ((Uplus | Uint_conversion),e1) -> true
    | _ -> false

  (* logical operations *)

  let expr_is_and node = match get_expr node with
    | NEbinary (_,Band,e1) -> true
    | _ -> false

  let expr_is_or node = match get_expr node with
    | NEbinary (_,Bor,e1) -> true
    | _ -> false

  let expr_is_not node = match get_expr node with
    | NEunary (Unot,e1) -> true
    | _ -> false

  (* comparison operators *)
	
  let expr_is_less_than node = match get_expr node with
    | NEbinary (_,Blt,_) -> true
    | _ -> false

  let expr_is_greater_than node = match get_expr node with
    | NEbinary (_,Bgt,_) -> true
    | _ -> false

  let expr_is_less_or_equal node = match get_expr node with
    | NEbinary (_,Ble,_) -> true
    | _ -> false

  let expr_is_greater_or_equal node = match get_expr node with
    | NEbinary (_,Bge,_) -> true
    | _ -> false

  let expr_is_equal node = match get_expr node with
    | NEbinary (_,Beq,_) -> true
    | _ -> false

  let expr_is_not_equal node = match get_expr node with
    | NEbinary (_,Bneq,_) -> true
    | _ -> false

  let push_not_inside node =
    let rec push_not e = match e.nexpr_node with
      | NEbinary (e1,Band,e2) ->
	  let new_e1 = push_not e1 in
	  let new_e2 = push_not e2 in
	  { e with nexpr_node = NEbinary (new_e1,Bor,new_e2) }
      | NEbinary (e1,Bor,e2) ->
	  let new_e1 = push_not e1 in
	  let new_e2 = push_not e2 in
	  { e with nexpr_node = NEbinary (new_e1,Band,new_e2) }
      | NEunary (Unot,e1) ->
	  e1
      | NEbinary (e1,Blt,e2) ->
	  { e with nexpr_node = NEbinary (e1,Bge,e2) }
      | NEbinary (e1,Ble,e2) ->
	  { e with nexpr_node = NEbinary (e1,Bgt,e2) }
      | NEbinary (e1,Bgt,e2) ->
	  { e with nexpr_node = NEbinary (e1,Ble,e2) }
      | NEbinary (e1,Bge,e2) ->
	  { e with nexpr_node = NEbinary (e1,Blt,e2) }
      | NEbinary (e1,Beq,e2) ->
	  { e with nexpr_node = NEbinary (e1,Bneq,e2) }
      | NEbinary (e1,Bneq,e2) ->
	  { e with nexpr_node = NEbinary (e1,Beq,e2) }
      | _ -> e
    in
    let sub_node = unop_get_operand node in
    let new_e = push_not (get_e sub_node) in
    create_tmp_node (Nexpr new_e)

  let expand_equal node =
    let e = get_e node in
    let new_e = match e.nexpr_node with
      | NEbinary (e1,Beq,e2) -> 
	  let le = { e with nexpr_node = NEbinary (e1,Ble,e2) } in
	  let ge = { e with nexpr_node = NEbinary (e1,Bge,e2) } in
	  { e with nexpr_node = NEbinary (le,Band,ge) }
      | NEbinary (e1,Bneq,e2) -> 
	  let le = { e with nexpr_node = NEbinary (e1,Blt,e2) } in
	  let ge = { e with nexpr_node = NEbinary (e1,Bgt,e2) } in
	  { e with nexpr_node = NEbinary (le,Bor,ge) }
      | _ -> assert false
    in
    create_tmp_node (Nexpr new_e)

  let patch_term_for_incr_decr node t =
    match get_expr node with
      | NEincr (op,_) ->
	  begin match op with
	    | Uprefix_inc | Upostfix_inc ->
		ITbinop (t,Clogic.Badd,ITconstant (IntConstant "1"))
	    | Uprefix_dec | Upostfix_dec ->
		ITbinop (t,Clogic.Bsub,ITconstant (IntConstant "1"))
	  end
      | _ -> t

  (* takes as input an unary operator of type Cast.unary_operator.
     returns the corresponding operator in Clogic.term_unop, if any.
     It uses locally [Clogic.Uexact] to denote an operator with no effect on
     the value of its operand, to be removed by its caller. *)
  let from_unop op = match op with
    | Uplus | Uint_conversion -> 
	Some Clogic.Uexact
    | Uminus -> 
	Some Clogic.Uminus
    | Unot | Ustar | Uamp | Utilde 
    | Ufloat_of_int | Uint_of_float | Ufloat_conversion ->
	None

  type binop_relation =
    | Binop of term_binop
    | Relation of relation

  (* takes as input an binary operator of type Cast.binary_operator.
     returns either the corresponding operator in Clogic.term_binop 
     or the corresponding relation in Clogic.relation, if any. *)
  let from_binop op = match op with
    | Badd | Badd_int _ | Badd_float _ | Badd_pointer_int -> 
	Some (Binop Clogic.Badd)
    | Bsub | Bsub_int _ | Bsub_float _ | Bsub_pointer ->
	Some (Binop Clogic.Bsub)
    | Bmul | Bmul_int _ | Bmul_float _ ->
	Some (Binop Clogic.Bmul)
    | Bdiv | Bdiv_int _ | Bdiv_float _ ->
	Some (Binop Clogic.Bdiv)
    | Bmod | Bmod_int _ ->
	Some (Binop Clogic.Bmod)
    | Blt | Blt_int | Blt_float _ | Blt_pointer ->
	Some (Relation Clogic.Lt)
    | Bgt | Bgt_int | Bgt_float _ | Bgt_pointer ->
	Some (Relation Clogic.Gt)
    | Ble | Ble_int | Ble_float _ | Ble_pointer ->
	Some (Relation Clogic.Le)
    | Bge | Bge_int | Bge_float _ | Bge_pointer ->
	Some (Relation Clogic.Ge)
    | Beq| Beq_int | Beq_float _ | Beq_pointer ->
	Some (Relation Clogic.Eq)
    | Bneq | Bneq_int | Bneq_float _ | Bneq_pointer ->
	Some (Relation Clogic.Neq)
    | Bbw_and | Bbw_xor | Bbw_or | Band | Bor | Bshift_left | Bshift_right ->
	None

  type term_predicate = 
    | Term of Var.t int_term
    | Predicate of Var.t int_predicate

  (* translates the expression [e] into the closest term (for an expression)
     or the closest predicate (for a test), forgetting any side-effect 
     during the evaluation of [e] *)
  let rec from_expr_or_test ?(test=false) e = match e.nexpr_node with
    | NEnop -> 
	(* not denoting any value. This should not occur. *)
	assert false
    | NEconstant c ->
	Term (ITconstant c)
    | NEvar (Var_info v) ->
	if test && var_is_pointer v then 
	  Predicate (IPnot_null_pointer (ITvar (Var.Vvar v)))
	else
	  Term (ITvar (Var.Vvar v))
    | NEunary (op,e1) ->
	begin match from_unop op with
	  | None -> 
	      begin match op with
	        | Unot -> 
		    let p1 = from_test e1 in
		    Predicate (IPnot p1)
		| _ -> Term ITany
	      end
	  | Some op ->
	      let pt = from_expr_or_test ~test e1 in
	      begin match op with
	        | Uexact ->
		    (* [Uexact] used here to mean the operation is useless *)
		    pt
		| _ ->
		    begin match pt with
		      | Term t1 -> Term (ITunop (op,t1))
		      | Predicate _ -> Term ITany
		    end
 	      end
	end
    | NEbinary (e1,op,e2) ->
	begin match from_binop op with
	  | None -> 
	      begin match op with
	        | Band -> 
		    let p1 = from_test e1 in
		    let p2 = from_test e2 in
		    Predicate (IPand (p1,p2))
	        | Bor -> 
		    let p1 = from_test e1 in
		    let p2 = from_test e2 in
		    Predicate (IPor (p1,p2))
		| _ -> Term ITany
	      end
	  | Some (Binop op) ->
	      let t1 = from_expr e1 in
	      let t2 = from_expr e2 in
	      Term (ITbinop (t1,op,t2))
	  | Some (Relation op) ->
	      let is_zero e = match e.nexpr_node with
		| NEconstant (IntConstant "0") -> true
		| _ -> false
	      in
	      let destroy_null_test =
		if test then
		  match op with 
		    | Eq ->
			if is_zero (sub_skip_casts e1) then
			  Some (true,false,e2)
			else if is_zero (sub_skip_casts e2) then
			  Some (true,false,e1)
			else None
		    | Neq ->
			if is_zero (sub_skip_casts e1) then
			  Some (false,true,e2)
			else if is_zero (sub_skip_casts e2) then
			  Some (false,true,e1)
			else None
		    | _ -> None
		else None
	      in
	      match destroy_null_test with
		| None ->
		    let t1 = from_expr e1 in
		    let t2 = from_expr e2 in
		    Predicate (IPrel (t1,op,t2))
		| Some (test_null,test_not_null,etest) ->
		    if test_not_null then
		      Predicate (from_test etest)
		    else if test_null then
		      Predicate (IPnot (from_test etest))
		    else assert false
	end
    | NEincr (op,e1) ->
	let t1 = from_expr e1 in
	begin match op with
	  | Uprefix_inc | Uprefix_dec ->
	      (* since we ignore side-effects here, pre-increment and
		 pre-decrement have no effect. In fact their effect is already
		 taken care of. *)
	      Term t1
	  | Upostfix_inc ->
	      (* reverse the effect of the increment *)
	      Term (ITbinop (t1,Clogic.Bsub,ITconstant (IntConstant "1")))
	  | Upostfix_dec ->
	      (* reverse the effect of the decrement *)
	      Term (ITbinop (t1,Clogic.Badd,ITconstant (IntConstant "1")))
	end
    | NEseq (_,e2) ->
	(* since we ignore side-effects here, the first expression in 
	   the sequence has no effect *)
	Term (from_expr e2)
    | NEassign (e1,e2) ->
	(* since we ignore side-effects here, the assignment is equivalent
	   to its left-hand side, unless it is a post-increment/decrement,
	   in which case we must reverse the corresponding operation.
	   To increase precision, we could add the fact it should be equal to 
	   the right-hand side too, under some conditions that guarantee
	   the assignment did not change the rhs value. *)
	if test then
	  (* take into account the common case of testing a string end by 
	     simultaneously copying its value *)
	  let e2node = create_tmp_node (Nexpr (sub_skip_casts e2)) in
	  if expr_is_deref e2node then
	    match deref_get_variable_and_offset e2node with
	      | None -> Term (from_expr e1)
	      | Some (v,off_opt) ->
		  if expr_type_is_char e2node then
		    let t_off = match off_opt with
		      | None -> ITconstant (IntConstant "0")
		      | Some off -> from_expr (get_e off)
		    in
		    (* check that no variable apearing in [t_off] are assigned
		       in the operation *)
		    let t1 = from_expr e1 in
		    let v_off = Var.T.collect_term_vars t_off in
		    let v1 = Var.T.collect_term_vars t1 in
		    (* on terms, not original expressions *)
		    if List.exists (fun v -> List.mem v v1) v_off then
		      Term (from_expr e1)
		    else
		      Predicate
			(IPnot_null_char_pointed (ITvar (Var.Vvar v),t_off))
		  else Term (from_expr e1)
	  else Term (from_expr e1)
	else Term (from_expr e1)
    | NEassign_op (e1,_,_) ->
	Term (from_expr e1)
    | NEarrow (e1,zone,var) ->
	if test then
	  let enode = create_tmp_node (Nexpr e) in
	  match deref_get_variable_and_offset enode with
	    | None -> Term ITany
	    | Some (v,off_opt) ->
		if expr_type_is_char enode then
		  let t_off = match off_opt with
		    | None -> ITconstant (IntConstant "0")
		    | Some off -> from_expr (get_e off)
		  in
		  Predicate (IPnot_null_char_pointed (ITvar (Var.Vvar v),t_off))
		else Term ITany
	else Term ITany
    | NEcast (_,e1) -> from_expr_or_test ~test e1
    | NEvar (Fun_info _) | NEstring_literal _ 
    | NEmalloc _ | NEcall _ | NEcond _ ->
	Term ITany

  and from_expr e =
    match from_expr_or_test e with
      | Term t -> t
      | Predicate _ -> 
	  (* it could be the case e.g. 
	         a = (b < c);
	   *)
	  ITany

  and from_test e =
    match from_expr_or_test ~test:true e with
      | Predicate p -> p
      | Term t -> IPrel(t,Clogic.Neq,ITconstant (IntConstant "0"))

  let rec from_term t = match t.nterm_node with
    | NTconstant c -> ITconstant c
    | NTvar v -> ITvar (Var.Vvar v)
    | NTunop (op,t1) -> ITunop (op,from_term t1)
    | NTbinop (t1,op,t2) -> ITbinop (from_term t1,op,from_term t2)
    | NTarrlen t1 -> 
	begin match from_term t1 with
	  | ITvar (Var.Vvar v) -> ITvar (Var.Varrlen v)
	  | _ -> ITany
	end
    | NTstrlen (t1,_,_) ->
	begin match from_term t1 with
	  | ITvar (Var.Vvar v) -> ITvar (Var.Vstrlen v)
	  | _ -> ITany
	end
    | NTmin _ | NTmax _
    | NTapp _ | NTarrow _ | NTif _ | NTold _ | NTat _ | NTbase_addr _
    | NToffset _ | NTblock_length _ | NTcast _ | NTrange _ 
    | NTmaxint _ | NTminint _ -> ITany

  let rec from_pred p = match p.npred_node with
    | NPfalse -> IPfalse
    | NPtrue -> IPtrue
    | NPrel (t1,rel,t2) -> IPrel (from_term t1,rel,from_term t2)
    | NPand (p1,p2) -> IPand (from_pred p1,from_pred p2)
    | NPor (p1,p2) -> IPor (from_pred p1,from_pred p2)
    | NPimplies (p1,p2) -> IPimplies (from_pred p1,from_pred p2)
    | NPiff (p1,p2) -> IPiff (from_pred p1,from_pred p2)
    | NPnot p1 -> IPnot (from_pred p1)
    | NPfull_separated (t1,t2) -> IPfull_separated (from_term t1,from_term t2)
    | NPseparated _ | NPbound_separated _
    | NPapp _ | NPif _ | NPforall _ | NPexists _ | NPold _ | NPat _ | NPvalid _
    | NPvalid_index _ | NPvalid_range _ | NPfresh _ | NPnamed _  -> IPany

  (* give the correct interface *)
  let from_expr node = from_expr (get_e node)
  let from_test node = from_test (get_e node)
  let from_pred node = from_pred (get_p node)

  let rec to_term t loc =
    let tnode = match t with
      | ITconstant c -> 
	  NTconstant c
      | ITvar (Var.Vvar v) ->
	  NTvar v
      | ITvar (Var.Varrlen v) ->
	  let vt = to_term (ITvar (Var.Vvar v)) loc in
	  NTarrlen vt
      | ITvar (Var.Vstrlen v) ->
	  let vt = to_term (ITvar (Var.Vvar v)) loc in
	  (* [strlen(p)] depends on the value pointed to by [p].
	     Add fields to describe this dependency. *)
	  Cnorm.make_nstrlen_node_from_nterm vt
      | ITvar (Var.Vterm t) -> (to_term t loc).nterm_node
      | ITunop (op,t1) -> 
	  let nt1 = to_term t1 loc in
	  NTunop (op,nt1)
      | ITbinop (t1,op,t2) ->
	  let nt1 = to_term t1 loc in
	  let nt2 = to_term t2 loc in
	  NTbinop (nt1,op,nt2)
      | ITmin tl ->
	  let ntl = List.map (fun t -> to_term t loc) tl in
	  let rec norm = function
	    | [] -> assert false
	    | [a] -> a
	    | a :: r -> 
		let subnode = NTmin (a,norm r) in
		{ nterm_node = subnode; nterm_loc = loc; 
		  nterm_type = int_term_type }
	  in (norm ntl).nterm_node
      | ITmax tl ->
	  let ntl = List.map (fun t -> to_term t loc) tl in
	  let rec norm = function
	    | [] -> assert false
	    | [a] -> a
	    | a :: r -> 
		let subnode = NTmax (a,norm r) in
		{ nterm_node = subnode; nterm_loc = loc; 
		  nterm_type = int_term_type }
	  in (norm ntl).nterm_node
      | ITany -> 
	  (* no such undefined term should be produced as 
	     the result of the analysis *)
	  assert false
    in
    let ttype = match t with
      | ITvar (Var.Vvar v) ->
	  (* pointer variables should get a pointer type *)
	  v.var_type
      | ITconstant _ | ITvar _ | ITunop _ | ITbinop _
      | ITmin _ | ITmax _ | ITany ->
	  int_term_type
    in
    { nterm_node = tnode; nterm_loc = loc; nterm_type = ttype }

  let rec to_pred p loc = 
    let pnode = match p with
      | IPfalse -> NPfalse
      | IPtrue -> NPtrue
      | IPrel (t1,rel,t2) -> 
	  let nt1 = to_term t1 loc in
	  let nt2 = to_term t2 loc in
	  NPrel (nt1,rel,nt2)
      | IPand (p1,p2) -> 
	  let np1 = to_pred p1 loc in
	  let np2 = to_pred p2 loc in
	  NPand (np1,np2)
      | IPor (p1,p2) -> 
	  let np1 = to_pred p1 loc in
	  let np2 = to_pred p2 loc in
	  NPor (np1,np2)
      | IPimplies (p1,p2) -> 
	  let np1 = to_pred p1 loc in
	  let np2 = to_pred p2 loc in
	  NPimplies (np1,np2)
      | IPiff (p1,p2) -> 
	  let np1 = to_pred p1 loc in
	  let np2 = to_pred p2 loc in
	  NPiff (np1,np2)
      | IPnot p1 -> 
	  let np1 = to_pred p1 loc in
	  NPnot (np1)
      | IPfull_separated (t1,t2) ->
	  let nt1 = to_term t1 loc in
	  let nt2 = to_term t2 loc in
	  NPfull_separated (nt1,nt2)
      | IPany | IPnull_pointer _ | IPnot_null_pointer _ | IPnull_char_pointed _ 
      | IPnot_null_char_pointed _ ->
	  (* no such predicates should be produced as 
	     the result of the analysis *)
	  assert false
    in
    { npred_node = pnode; npred_loc = loc } 

  (* create an assume statement from a predicate [p] and a statement [node] 
     (that may not be an assume statement) *)
  let change_in_assume_stat node p =
    let s = get_s node in
    let np = to_pred p s.nst_loc in
    let new_s = NSassume np in
    let new_s = { s with nst_node = new_s } in
    create_tmp_node (Nstat new_s)

  (* add a predicate [p] to the assume predicate [node] *)
  let grow_predicate node assume_node_opt p =
    let old_p = get_p node in
    let np = to_pred p old_p.npred_loc in
    let assume_p = Option.app get_p assume_node_opt in
    let np = Option.fold (fun p1 p2 -> NPredicate.subtract p2 p1) assume_p np in
    let new_p = NPredicate.make_conjunct [old_p;np] in
    create_tmp_node (Npred new_p)

  let internal_access ?string_write ?string_read ?pointer_access
      is_packed_var restr_vars node =
    if debug_more then Coptions.lprintf
      "[internal_access] %a@." Node.pretty node;
    match get_node_kind node with
    | NKexpr | NKtest | NKlvalue ->
	if expr_is_deref node then
	  match deref_get_variable_and_offset node with
	  | None -> 
	      (* dereference form not recognized *)
	      None
	  | Some (v,off) ->
	      if is_packed_var (Var.Vvar v) then
		(* get equivalent term for offset *)
		let t_off = match off with
		| None -> ITconstant (IntConstant "0")
		| Some e -> from_expr e
		in
		(* build the safe access predicate *)
		if expr_type_is_char node 
		  && (List.mem (Var.Vstrlen v) restr_vars) then
		  match get_node_kind node with
		    | NKlvalue ->  
			if debug_more then Coptions.lprintf
			  "[internal_access] string write access@.";
			(* write access to a string *)
			begin match string_write with
			  | Some string_write -> Some (string_write v t_off)
			  | None -> None
			end
		    | NKexpr | NKtest ->
			if debug_more then Coptions.lprintf
			  "[internal_access] string read access@.";
			(* read access to a string *)
			begin match string_read with
			  | Some string_read -> Some (string_read v t_off)
			  | None -> None
			end
		    | _ -> assert false
		else
		  (* not a string access *)
		  begin match pointer_access with
		    | Some pointer_access -> Some (pointer_access v t_off)
		    | None -> None
		  end
	      else
		(* variable is not packed *)
		None
	else
	  (* expression is not a dereference *)
	  None
    | NKassume | NKassert | NKnone | NKdecl | NKstat
    | NKpred | NKterm | NKannot | NKspec -> 
	None

  (* create a predicate that expresses a safe access, if [node] targetted by
     the analysis. [is_packed_var] tells whether a given variable is to be
     considered or not. *)
  let memory_access_safe_predicate =
    internal_access ~string_write:Var.safe_access_predicate 
      ~string_read:(Var.safe_access_predicate ~read_string:true)
      ~pointer_access:Var.safe_access_predicate

  let call_precondition node =
    match get_node_kind node with
    | NKexpr | NKtest | NKlvalue ->
	if expr_is_call node then
	  match call_get_function node with
	  | None -> None
	  | Some func ->
	      (* get function precondition *)
	      begin match function_get_precondition func with
	      | None -> None
	      | Some p ->
		  let pred = from_pred p in
		  let vars = function_get_params func in
		  let params = List.map (fun v -> ITvar (Var.Vvar v)) vars in
		  let arrparams = 
		    List.map (fun v -> ITvar (Var.Varrlen v)) vars in
		  let strparams = 
		    List.map (fun v -> ITvar (Var.Vstrlen v)) vars in
		  let params = params @ arrparams @ strparams in
		  let args = call_get_args node in
		  let args = List.map from_expr args in
		  let arrargs = List.map Var.arrlen_term args in
		  let strargs = List.map Var.strlen_term args in
		  let args = args @ arrargs @ strargs in
		  let trans = List.map2 (fun param arg -> (param,arg))
		      params args in
		  let pred = Var.P.translate trans pred in
		  Some pred
	      end
	else None
    | NKassume | NKassert | NKnone | NKdecl | NKstat
    | NKpred | NKterm | NKannot | NKspec -> 
	None

  let string_access_predicate node =
    match get_node_kind node with
      | NKtest -> 
	  let p = Var.P.explicit_pred (from_test node) in
	  begin match p with
	    | IPnull_char_pointed (ITvar (Var.Vvar v), _) 
	    | IPnot_null_char_pointed (ITvar (Var.Vvar v), _) ->
		Some (Var.string_predicate v)
	    | _ -> None
	  end
      | _ -> None

  let is_safe_access node =
    let e = get_e node in
    match e.nexpr_node with
      | NEarrow (e1,zone,field) ->
	  let typ = e1.nexpr_type in
	  begin match typ.Ctypes.ctype_node with
	    | Ctypes.Tpointer (Ctypes.Valid _,_)
	    | Ctypes.Tarray (Ctypes.Valid _,_,_) -> true
	    | _ -> false
	  end
      | _ ->
	  (* should be called only on dereference *)
	  assert false

  (* change type of dereference expression to make it a safe access *)
  let make_safe_access node =
    let e = get_e node in
    match e.nexpr_node with
      | NEarrow (e1,zone,field) ->
	  let typ = e1.nexpr_type in
	  let new_typ = match typ.Ctypes.ctype_node with
	  | Ctypes.Tpointer (valid,t) -> 
	      Ctypes.Tpointer (Ctypes.Valid (Int64.zero,Int64.one) ,t)
	  | Ctypes.Tarray (valid,t,s) -> 
	      Ctypes.Tarray (Ctypes.Valid (Int64.zero,Int64.one) ,t,s)
	  | _ ->
	      (* should be called only on pointer or array access *)
	      assert false
	  in
	  let new_typ = { typ with Ctypes.ctype_node = new_typ } in
	  let new_e1 = { e1 with nexpr_type = new_typ } in
	  let new_e = { e with nexpr_node = NEarrow (new_e1,zone,field) } in
	  create_tmp_node (Nexpr new_e)
      | _ ->
	  (* should be called only on dereference *)
	  assert false
end

type sep_transform_tt = SepTrans

module Make_ConnectCFGtoSep 
  (RWL : READ_WRITE_LATTICE with module V = VarAsPVARIABLE)
  : CONNECTION with type node_t = IntLangFromNormalized.Node.t 
	       and type 'a node_hash_t = 'a IntLangFromNormalized.NodeHash.t 
	       and type absval_t = RWL.t
	       and type transform_t = sep_transform_tt =
struct
 
  open IntLangFromNormalized

  type node_t = Node.t
  type 'a node_hash_t = 'a NodeHash.t
  type absval_t = RWL.t
  type 'a analysis_t = 'a pair_t node_hash_t
  type absint_analysis_t = absval_t analysis_t
  type transform_t = sep_transform_tt

  let widening_threshold = None
  let widening_strategy = WidenFast
 
  let transfer ?(backward=false) ?(with_assert=false) ?(one_pass=false) 
      ?previous_value node cur_val =

    if debug_more then Coptions.lprintf 
      "[transfer] %a@." Node.pretty node;

    (* this transfer function is only meant to be used in one-pass backward
       propagation, to discover necessary separation conditions *)
    assert (backward && one_pass);

    match get_node_kind node with
      | NKlvalue -> 
	  (* voluntarily excluded from treatment below to treat differently
	     read and write accesses *)
	  cur_val

      | NKstat ->
	  if backward && stat_is_decl node then 
	    (* ignore information on variable before its declaration *)
	    let var = decl_stat_get_var node in
	    let new_val = 
	      RWL.remove_variable (Var.Vvar var) cur_val in
	    if debug_more then Coptions.lprintf
	      "[transfer] removing info on %a@." Var.pretty (Var.Vvar var);
	    new_val
	  else cur_val

      | NKexpr | NKtest ->
	  if expr_is_int_assign node then
	    let lhs_node = assign_get_lhs_operand node in
	    if expr_is_deref lhs_node then
	      match deref_get_local_var lhs_node with
		| None -> cur_val
		| Some lhs_var -> 
		    (* writing under pointer [lhs_var] *)
		    RWL.eval_write (Var.Vvar lhs_var) cur_val
	    else cur_val
	  else if expr_is_deref node then
	    match deref_get_local_var node with
	      | None -> cur_val
	      | Some rhs_var ->
		  (* reading under pointer [rhs_var] *)
		  RWL.eval_read (Var.Vvar rhs_var) cur_val
	  else if expr_is_call node then
	    match call_get_function node with
	      | None -> cur_val
	      | Some func ->
		  (* get function precondition *)
		  begin match function_get_precondition func with
		    | None -> cur_val
		    | Some p ->
			let pred = from_pred p in
			let params = function_get_params func in
			let params = 
			  List.map (fun v -> ITvar (Var.Vvar v)) params in
			let args = call_get_args node in
			let args = List.map from_expr args in
			let trans = List.map2 (fun param arg -> (param,arg))
			  params args in
			let pred = Var.P.translate trans pred in
			RWL.eval_precondition pred cur_val
		  end
	  else cur_val
	    
      | NKassert ->
	  if is_invariant_node node then
	    let read_under_pointers = get_loop_read_under_pointers node in
	    let write_under_pointers = get_loop_write_under_pointers node in
	    let read_under_pointers = 
	      List.map (fun v -> Var.Vvar v) read_under_pointers in
	    let write_under_pointers = 
	      List.map (fun v -> Var.Vvar v) write_under_pointers in
	    let new_val =
	      List.fold_right RWL.eval_read read_under_pointers cur_val
	    in
	    List.fold_right RWL.eval_write write_under_pointers new_val
	  else cur_val

      | NKassume | NKdecl
      | NKspec | NKannot | NKterm | NKpred
      | NKnone -> cur_val

  (* exception used to share the default treatment in [sub_transform] *)
  exception Rec_transform
	  
  let rec sub_transform analysis trans_params node =
    let sub_nodes = code_children node @ (logic_children node) in
    let new_sub_nodes = 
      List.map (sub_transform analysis trans_params) sub_nodes in
    let new_node = change_sub_components node new_sub_nodes in

    try 
      (* transformation is possible only if analysis provides 
	 some information. Otherwise raise Not_found. *)
      let post_val = match NodeHash.find_post analysis node with
	| None -> raise Rec_transform
	| Some v -> v
      in
      (* match [node] here, not [new_node], as the additional information of
	 [NKtest, NKassume, NKassert] will be lost on [new_node].
	 No special problem here since the modified node is still of the same
	 kind as the original one, except the special ones mentioned above. *)
      match get_node_kind node with
	| NKassume ->
	    if is_function_precondition_node node then
	      begin 
		let p_assume = RWL.to_pred post_val in
		match p_assume with
	        | None ->
		    (* no useful information here *)
		    raise Rec_transform
		| Some pred ->
		    grow_predicate new_node None pred
	      end
	    else raise Rec_transform
	      
	| NKexpr | NKtest | NKlvalue
	| NKnone | NKdecl | NKstat | NKassert
	| NKpred | NKterm | NKannot | NKspec -> 
	    raise Rec_transform

    with Rec_transform -> new_node

  let transform analysis trans_params decls =
    List.map (sub_transform analysis trans_params) decls
	    
end

type int_transform_tt =
   {
     safe_access_nodes : IntLangFromNormalized.NodeSet.t;
   }

(* string and integer analysis *)
module Make_ConnectCFGtoInt 
    (CL : PACKED_CONTEXTUAL_LATTICE with module V = VarAsPVARIABLE)
    (SL : SEPARATION_LATTICE with module V = VarAsPVARIABLE)
    : CONNECTION 
      with type node_t = IntLangFromNormalized.Node.t 
      and type 'a node_hash_t = 'a IntLangFromNormalized.NodeHash.t 
      and type absval_t = CL.t * SL.t
      and type transform_t = int_transform_tt =
struct

  open IntLangFromNormalized

  type node_t = Node.t
  type 'a node_hash_t = 'a NodeHash.t
  type absval_t = CL.t * SL.t
  type 'a analysis_t = 'a pair_t node_hash_t
  type absint_analysis_t = absval_t analysis_t
  type transform_t = int_transform_tt

  (* default value of 5 taken from Miné's example analysis *)
  let widening_threshold = Some 1 (* for debug, use 1 *)
  let widening_strategy = WidenFast

  (* Various constructs change the abstract information:
     - assignments change the information for the variable assigned
     - tests constrain the information for various variables involved 
     in the test
     - logic information (assertions, loop annotations, etc) constrain
     the information for possibly many variables

     Furthermore, even assignments must sometimes be treated globally, 
     if the lattice considered is a relational lattice. Consider the following
     assignment:
         a = b + 1;
     Computing the domain of [b+1] is sufficient for an interval integer 
     analysis, but not for an analysis based on octogons. Here the interesting
     fact is the bound on [a-b].
     Therefore we translate assignments and tests in terms understood by
     the integer analysis, and we leave it to the analysis to compute 
     the transfer function for such terms.
   *)

  let term_reps = Hashtbl.create 0
  let pred_reps = Hashtbl.create 0

  let get_term_rep node =
    try
      Hashtbl.find term_reps node
    with Not_found ->
      let t_rep = from_expr node in
      Hashtbl.replace term_reps node t_rep;
      t_rep

  let get_pred_rep node =
    try
      Hashtbl.find pred_reps node
    with Not_found ->
      let p_rep = match get_node_kind node with
        | NKtest -> from_test node
	| NKassume | NKassert -> from_pred node
	| NKnone | NKstat | NKdecl | NKexpr | NKlvalue
	| NKspec | NKannot | NKterm | NKpred -> 
	    (* [get_pred_rep] should only be called on test/assume/assert *)
	    assert false
      in
      Hashtbl.replace pred_reps node p_rep;
      p_rep

  let keep_invariant_value node previous_value cur_val =
    let cur_ctxt_val,cur_sep_val = cur_val in

    (* ignore variables written in loop, only on conditionals *)
    let write_vars = get_loop_write_vars node in
    let write_vars = List.map (fun v -> Var.Vvar v) write_vars in
    let fwd_ctxt_val = 
      List.fold_right CL.remove_variable_conditionals write_vars cur_ctxt_val
    in

    let fwd_sep_val = cur_sep_val in
    if debug_more then Coptions.lprintf 
      "[transfer] (assume) invariant current value %a@."
      CL.pretty fwd_ctxt_val;
    match previous_value with
      | None -> fwd_ctxt_val,fwd_sep_val
      | Some (prev_ctxt_val,prev_sep_val) ->
	  if debug_more then Coptions.lprintf 
	    "[transfer] (assume) invariant previous value %a@."
	    CL.pretty prev_ctxt_val;
	  if debug_more then Coptions.lprintf 
	    "[transfer] (assume) invariant current value %a@."
	    CL.pretty fwd_ctxt_val;
	  (* [meet] justified here because used between
	     - [prev_ctxt_val] previous value of assumed invariant,
	     - [fwd_val], result of current propagation, from which
	     variables that are assigned in the loop are removed
	  *)
	  let res1 = CL.normalize (CL.meet prev_ctxt_val fwd_ctxt_val) in
	  let res2 = SL.normalize (SL.meet prev_sep_val fwd_sep_val) in
	  if debug_more then Coptions.lprintf 
	    "[transfer] (assume) invariant result value %a@."
	    CL.pretty res1;
	  res1,res2

  let transfer ?(backward=false) ?(with_assert=false) ?(one_pass=false) 
      ?previous_value node cur_val =

    if debug_more then Coptions.lprintf 
	"[transfer] %a@." Node.pretty node;
    begin match previous_value with
      | None -> ()
      | Some (prev_ctxt_val,prev_sep_val) -> 
	  if debug_more then Coptions.lprintf 
	    "[transfer] with previous value %a %a@." CL.pretty prev_ctxt_val
	    SL.pretty prev_sep_val
    end;

    let cur_ctxt_val,cur_sep_val = cur_val in
    let forward = not backward in
    match get_node_kind node with
      | NKnone -> cur_val

      | NKstat ->
	  if backward && stat_is_decl node then 
	    (* ignore information on variable before its declaration *)
	    let var = decl_stat_get_var node in
	    let new_ctxt_val = 
	      CL.remove_variable (Var.Vvar var) cur_ctxt_val in
	    let new_ctxt_val = 
	      CL.remove_variable (Var.Varrlen var) new_ctxt_val in
	    let new_ctxt_val = 
	      CL.remove_variable (Var.Vstrlen var) new_ctxt_val in
	    if debug_more then Coptions.lprintf
	      "[transfer] removing info on %a@." Var.pretty (Var.Vvar var);
	    new_ctxt_val,cur_sep_val
	  else cur_val

      | NKdecl ->
	  if backward then
	    cur_val
	  else
	    begin
	      (* originally reset the tables for representatives *)
	      Hashtbl.clear term_reps;
	      Hashtbl.clear pred_reps;
	      CL.init (),SL.init ()
	    end

      | NKexpr | NKtest | NKlvalue | NKassume ->
	  (* test is both expression and assume, which leads to treating 
	     those cases simultaneously *)
	  let expr_val = match get_node_kind node with
	  | NKexpr | NKtest | NKlvalue ->

	      if expr_is_int_assign node then
		match assign_get_local_lhs_var node with
		| None -> 
		    let lhs_node = assign_get_lhs_operand node in
		    (* in the forward case, remove those [strlen] variables
		       that are invalidated by an assignment under 
		       some pointer *)
		    if forward && expr_is_deref lhs_node then
		      let new_ctxt_val =
			match deref_get_local_var lhs_node with
			| None ->
			    (* no way to keep talking about [strlen] 
			       variables *)
			    CL.filter_variables 
			      ~remove:Var.is_strlen cur_ctxt_val
			| Some lhs_var ->
			    (* remove information on [strlen(p)] if [p] not 
			       separated from the pointer being assigned.
			       This includes [strlen(lhs_var)]. *)
			    let not_separated = function
			      | Var.Vstrlen v ->
				  not (SL.fully_separated (Var.Vvar lhs_var) 
					 (Var.Vvar v) cur_sep_val)
			      | Var.Vvar _ | Var.Varrlen _ 
			      | Var.Vterm _ -> false
			    in
			    CL.filter_variables 
			      ~remove:not_separated cur_ctxt_val
		      in new_ctxt_val,cur_sep_val
		    else cur_val
		| Some lhs_var ->
		    (* compute new value for [lhs_var] *)
		    let rhs_node = assign_get_rhs_operand node in
		    let rhs_rep = get_term_rep rhs_node in
		    let rhs_rep = patch_term_for_incr_decr node rhs_rep in
		    let forget_val = CL.remove_variable
			(Var.Varrlen lhs_var) cur_ctxt_val in
		    let forget_val = CL.remove_variable
			(Var.Vstrlen lhs_var) forget_val in
		    let new_ctxt_val =
		      CL.eval_assign ~backward 
			(Var.Vvar lhs_var) rhs_rep forget_val
		    in new_ctxt_val,cur_sep_val

	      else if expr_is_ptr_assign node then
		match assign_get_local_lhs_var node with
		| None -> cur_val
		| Some lhs_var ->
		    (* TODO: treat allocations *)
		    let new_sep_val = 
		      SL.remove_variable (Var.Vvar lhs_var) cur_sep_val in
		    cur_ctxt_val,new_sep_val
	      else cur_val
	  | _ -> cur_val
	  in

	  let expr_ctxt_val,expr_sep_val = expr_val in
	  let expr_ctxt_val =
	    if with_assert then
	      (* consider memory accesses as asserts *)
	      match memory_access_safe_predicate CL.is_packed_variable 
		(CL.restrained_variables expr_ctxt_val) node 
	      with
		| None -> expr_ctxt_val
		| Some p_safe ->
		    if debug_more then Coptions.lprintf
		      "[transfer] adding assertion %a@." 
		      Var.P.pretty p_safe;
		    (* access is guaranteed to be safe for the following *)
		    let res = 
		      CL.eval_test ~backward p_safe expr_ctxt_val in
		    if debug_more then Coptions.lprintf 
		      "[transfer] resulting value with assertion %a@."
		      CL.pretty res;
		    res
	    else expr_ctxt_val
	  in
	  
	  let expr_ctxt_val,expr_sep_val =
	    if forward && one_pass 
	      && (is_assume_invariant_node node 
		  || is_function_precondition_node node) then
		(* keep last value computed (either forward or backward) *)
		if is_assume_invariant_node node then
		  begin
		    if debug then Coptions.lprintf
		      "[transfer] Assume invariant presented with@.%a@."
		      CL.pretty expr_ctxt_val;
		    let res1,res2 = keep_invariant_value node previous_value 
		      (expr_ctxt_val,expr_sep_val) in
		    if debug then Coptions.lprintf
		      "[transfer] Assume invariant result@.%a@."
		      CL.pretty res1;
		    res1,res2
		  end
		else if is_function_precondition_node node then
		  match previous_value with
		    | None -> expr_ctxt_val,expr_sep_val
		    | Some prev_val -> prev_val
		else assert false
	    else
	      expr_ctxt_val,expr_sep_val
	  in
	  begin match get_node_kind node with
	  | NKtest | NKassume ->
	      (* same transfer for forward and backward propagation *)
	      let node_rep = get_pred_rep node in
	      if debug then Coptions.lprintf 
		"[transfer] Before test %a value is@.[transfer] %a@."
		Var.P.pretty node_rep CL.pretty expr_ctxt_val;
	      let new_ctxt_val = 
		CL.eval_test ~backward node_rep expr_ctxt_val in
	      if debug then Coptions.lprintf 
		"[transfer] After test %a value is@.[transfer] %a@."
		Var.P.pretty node_rep CL.pretty new_ctxt_val;
	      let new_sep_val = 
		SL.eval_test ~backward node_rep expr_sep_val in
	      new_ctxt_val,new_sep_val
	  | _ -> expr_ctxt_val,expr_sep_val
	  end

      | NKassert ->
	  if is_invariant_node node then
	    if backward then
	      (* eliminate variables written in the loop *)
	      let write_vars = get_loop_write_vars node in
	      let write_vars = List.map (fun v -> Var.Vvar v) write_vars in
	      if debug_more then Coptions.lprintf 
		"[transfer] invariant with written vars %a@."
		(print_list comma Var.pretty) write_vars;
	      let new_ctxt_val = CL.eliminate write_vars cur_ctxt_val in
	      new_ctxt_val,cur_sep_val

	    else if forward && one_pass then
	      let new_ctxt_val,new_sep_val =
		keep_invariant_value node previous_value cur_val
	      in
	      (* only the proper separation conditions
		 should allow proper use of [strlen] variables *)
	      let write_under_pointers = 
		get_loop_write_under_pointers node in
	      let write_under_pointers = 
		List.map (fun v -> Var.Vvar v) write_under_pointers in
	      let restr_vars = 
		CL.restrained_variables new_ctxt_val in
	      let strlen_vars = List.filter Var.is_strlen restr_vars in
	      if debug_more then Coptions.lprintf 
		"[transfer] fwd invariant with write under pointers %a@."
		(print_list comma Var.pretty) write_under_pointers;
	      if debug_more then Coptions.lprintf 
		"[transfer] fwd invariant with restrained variables %a@."
		(print_list comma Var.pretty) restr_vars;
	      if debug_more then Coptions.lprintf 
		"[transfer] fwd invariant with strlen variables %a@."
		(print_list comma Var.pretty) strlen_vars;
	      let not_written_under var =
		List.fold_left (fun acc_b v -> acc_b 
				  && SL.fully_separated var v new_sep_val
			       ) true write_under_pointers
	      in
	      let new_ctxt_val =
		List.fold_left 
		  (fun ctxt_val strlen_var ->
		     let v = Var.get_variable strlen_var in
		     if not_written_under (Var.Vvar v) then
		       ctxt_val
		     else
		       CL.remove_variable strlen_var ctxt_val
		  ) new_ctxt_val strlen_vars
	      in
	      new_ctxt_val,new_sep_val

	    else (* forward && not one_pass *)
	      cur_val

	  else
	    begin
	      if debug_more then Coptions.lprintf 
		  "[transfer] assert normal treatment@.";
	      (* same transfer for forward and backward propagation *)
	      if with_assert then 
		let node_rep = get_pred_rep node in
		let new_ctxt_val = 
		  CL.eval_test ~backward node_rep cur_ctxt_val in
		new_ctxt_val,cur_sep_val
	      else cur_val
	    end

      | NKspec | NKannot | NKterm | NKpred -> cur_val

  (* exception used to share the default treatment in [sub_transform] *)
  exception Rec_transform

  let rec sub_transform analysis trans_params node =
    let sub_nodes = code_children node @ (logic_children node) in
    let new_sub_nodes = 
      List.map (sub_transform analysis trans_params) sub_nodes in
    let new_node = change_sub_components node new_sub_nodes in

    try 
      (* transformation is possible only if analysis provides 
	 some information. Otherwise raise Not_found. *)
      let post_val = match NodeHash.find_post analysis node with
	| None -> raise Rec_transform
	| Some v -> v
      in
      let post_ctxt_val,post_sep_val = post_val 
      in
      if debug_more then Coptions.lprintf 
	  "[sub_transform] %a %a@." 
	  CL.pretty post_ctxt_val SL.pretty post_sep_val;
	    
      (* match [node] here, not [new_node], as the additional information of
	 [NKtest, NKassume, NKassert] will be lost on [new_node].
	 No special problem here since the modified node is still of the same
	 kind as the original one, except the special ones mentioned above. *)
      match get_node_kind node with
        | NKstat ->
	    if debug_more then Coptions.lprintf 
		"[sub_transform] old node %a@." Node.pretty node;
	    if debug_more then Coptions.lprintf 
		"[sub_transform] new node %a@." Node.pretty new_node;
	    
	    if stat_is_jump new_node then
	      (* next statement will not be executable. Do not add it. *)
	      raise Rec_transform
	    else
	      (* get the result of the analysis as a predicate and append
		 an assume statement of this predicate after the current 
		 statement *)
	      let p_assume = Option.transform (fun p1 p2 -> IPand (p1,p2))
		(CL.to_pred post_ctxt_val) (SL.to_pred post_sep_val) 
	      in
	      begin match p_assume with
	        | None ->
		    if debug_more then Coptions.lprintf 
			"[sub_transform] no predicate found with %a %a@."
			CL.pretty post_ctxt_val SL.pretty post_sep_val;
		    
		    (* no useful information here *)
		    raise Rec_transform
		| Some pred ->
		    if debug_more then Coptions.lprintf 
			"[sub_transform] predicate found with %a %a@."
			CL.pretty post_ctxt_val SL.pretty post_sep_val;
		    
		    let assume_s = change_in_assume_stat new_node pred in
		    make_seq_stat new_node assume_s
	      end
		
	| NKassume ->
	    if is_function_precondition_node node then
	      begin 
		if debug_more then Coptions.lprintf 
		    "[sub_transform] precondition contextual value %a %a@."
		    CL.pretty post_ctxt_val SL.pretty post_sep_val;
		let p_assume = Option.transform (fun p1 p2 -> IPand (p1,p2))
		  (CL.to_pred post_ctxt_val) (SL.to_pred post_sep_val) 
		in
		match p_assume with
	        | None ->
		    (* no useful information here *)
		    raise Rec_transform
		| Some pred ->
		    grow_predicate new_node None pred
	      end
	    else
	      let p_assume = Option.transform (fun p1 p2 -> IPand (p1,p2))
		(CL.to_pred post_ctxt_val) (SL.to_pred post_sep_val) 
	      in
	      begin match p_assume with
	        | None ->
		    (* no useful information here *)
		    raise Rec_transform
		| Some pred ->
		    grow_predicate new_node None pred
	      end
	      
	| NKassert ->
	    if is_invariant_node node then
	      begin
		if debug_more then Coptions.lprintf 
		    "[sub_transform] precondition contextual value %a %a@."
		    CL.pretty post_ctxt_val SL.pretty post_sep_val;
		let p_assert = Option.transform (fun p1 p2 -> IPand (p1,p2))
		  (CL.to_pred post_ctxt_val) (SL.to_pred post_sep_val) 
		in
		match p_assert with
	        | None ->
		    (* no useful information here *)
		    raise Rec_transform
		| Some pred ->
		    (* do not add as invariant some already known 
		       assumed invariant *)
		    grow_predicate new_node (logic_invariant node) pred
	      end
	    else
	      raise Rec_transform
	    
	| NKexpr | NKtest | NKlvalue ->
	    if Coptions.absint_as_proof && 
	      NodeSet.mem node trans_params.safe_access_nodes then
	      (* change type of dereference expression to make it a 
		 safe access *)
	      begin
		if debug_more then Coptions.lprintf 
		    "[sub_transform] safe access %a@." 
		    Node.pretty new_node;
		make_safe_access new_node
	      end
	    else
	      (* not a dereference, or safe access cannot be guaranteed *)
	      raise Rec_transform

	| NKnone | NKdecl
	| NKpred | NKterm | NKannot | NKspec -> 
	    raise Rec_transform

    with Rec_transform -> new_node

  let transform analysis trans_params decls =
    List.map (sub_transform analysis trans_params) decls
end

module IdentTypeBridge (Ctxt : PACKED_CLUSTER_LATTICE) 
    (Cstr : PACKED_CONSTRAINED_LATTICE with type t = Ctxt.t) 
    : CONTEXTUAL_BRIDGE with module Contxt = Ctxt and module Constr = Cstr
    and type ipredicate = Cstr.V.P.t =
struct
  module Contxt = Ctxt
  module Constr = Cstr

  type ipredicate = Constr.V.P.t

  let get_unconstrained = Constr.get_unconstrained
  let get_constrained = Constr.get_constrained
  let make_unconstrained = Constr.make_unconstrained
  let subtract = Constr.subtract
  let meet = Constr.meet
  let join = Constr.join
  let eval_constraint = Constr.eval_constraint
end

(* modules for interval analysis *)

module IntervLattice = Make_IntervalLattice(Var)(Int)

module PointWiseIntervLattice = Make_PointWiseFromAtomic(IntervLattice)

(* modules for separation analysis *)

module SepLattice = Make_SeparationLattice(Var)(Int) 

module ReadWriteLattice = Make_ReadWriteLattice(Var)(Int)(SepLattice)

(* module for octogon analysis *)

module UnpackOctLattice = Make_OctogonLattice(Var)(Int)

module OctLattice = 
  Make_PackedFromCluster(Var)(UnpackOctLattice)

module ConstrOctLattice = 
  Make_PackedFromConstrained(Var)
    (Make_ConstrainedOctogonLattice(Var)(Int)(UnpackOctLattice))

(* module for predicate abstraction *)

module PredLattice = Make_PredicateLattice(Var)(Int)

(* modules for the analysis *)

module OctPredLattice = 
  Make_ClusterPairLattice(Var)(Int)(PredLattice)(OctLattice)

module ConstrOctPredLattice = 
  Make_ConstrainedPairLattice(Var)(Int)(PredLattice)(ConstrOctLattice)

module ContextLattice = 
  Make_ContextualLattice
    (Var)(Int)(OctPredLattice)(ConstrOctPredLattice)
    (IdentTypeBridge(OctPredLattice)(ConstrOctPredLattice))

module ConnectCFGtoSep = Make_ConnectCFGtoSep(ReadWriteLattice)

module LocalSeparationAnalysis =
  Make_DataFlowAnalysis(Var)(IntLangFromNormalized)
    (ReadWriteLattice)(ConnectCFGtoSep)

module ConnectCFGtoOct = Make_ConnectCFGtoInt(ContextLattice)(SepLattice)

module ContextSepLattice = Make_PairLattice(ContextLattice)(SepLattice)

module LocalMemoryAnalysis :
sig
  open IntLangFromNormalized

  include DATA_FLOW_ANALYSIS 
    with type node_t = Node.t
    and type 'a node_hash_t = 'a NodeHash.t
    and type 'a analysis_t = 'a ConnectCFGtoOct.analysis_t
    and type absint_analysis_t = ConnectCFGtoOct.absint_analysis_t

  val string_bnf_params : compute_bnf_t
  val compute_bnf_params : compute_bnf_t
 
    (* takes the result of the abstract interpretation.
       returns a formatted analysis easily exploited by 
       [ConnectCFGtoOct.transform]. *)
  val format : 
    absint_analysis_t -> node_t list -> absint_analysis_t * NodeSet.t
end = 
struct

  include Make_DataFlowAnalysis(Var)(IntLangFromNormalized)
      (ContextSepLattice)(ConnectCFGtoOct)

  open IntLangFromNormalized

  (* select memory accesses that need to be considered in the analysis.
     The memory accesses selected are those for which we want to express
     a safety property. This excludes:
     - memory accesses already analyzed as safe by the forward analysis
     - memory accesses not of the form that can be analyzed
     - memory accesses safe by construction (e.g. array initialization)
   *)
  let memory_access_select node pre_val =
    if debug_more then Coptions.lprintf
	"[memory_access_select] %a with value %a@." Node.pretty node
	ContextSepLattice.pretty pre_val;
    let pre_ctxt_val,_ = pre_val in
    match memory_access_safe_predicate ContextLattice.is_packed_variable 
	(ContextLattice.restrained_variables pre_ctxt_val) node 
    with
    | None ->
	begin match call_precondition node with
	| None -> false
	| Some p_precond ->
	    (* is the precondition already guaranteed to be safe ? *)
	    not (ContextLattice.guarantee_test p_precond pre_ctxt_val)
	end
    | Some p_safe ->
	(* is the access already guaranteed to be safe ? *)
	not (is_safe_access node
	   || ContextLattice.guarantee_test p_safe pre_ctxt_val)
	    
  let string_test_select node pre_val =
    if debug_more then Coptions.lprintf
      "[string_test_select] %a with value %a@." Node.pretty node
      ContextSepLattice.pretty pre_val;
    let pre_ctxt_val,_ = pre_val in
    match string_access_predicate node with
      | None -> false
      | Some p_string ->
	  (* is the pointer already guaranteed to be a string ? *)
	  not (ContextLattice.guarantee_test p_string pre_ctxt_val)

  let build_safe_memory_access node pre_val =
    if debug then Coptions.lprintf
	"[build_safe_memory_access] %a with value %a@." Node.pretty node
	ContextSepLattice.pretty pre_val;
    let pre_ctxt_val,pre_sep_val = pre_val in
    match memory_access_safe_predicate ContextLattice.is_packed_variable 
	(ContextLattice.restrained_variables pre_ctxt_val) node
    with
    | None ->
	begin match call_precondition node with
	| None -> assert false (* [node] should have been selected first *)
	| Some p_precond ->	
	    if debug then Coptions.lprintf
		"[build_safe_memory_access] Trying to prove precondition %a@." 
		Var.P.pretty p_precond;
	    (* [arrlen] variables should not be used in left parts of conditionals,
	       since their value cannot be tested by the programmer *)
	    let pre_ctxt_val = 
	      ContextLattice.filter_variables ~remove:Var.is_arrlen pre_ctxt_val in
	    let ctxt_val = ContextLattice.get_context pre_ctxt_val in
	    let cstr_val = ContextLattice.Bridge.eval_constraint p_precond ctxt_val in
	    let init_val = ContextLattice.eliminate_conditionals pre_ctxt_val in
	    let init_val = 
	      ContextLattice.add_new_conditional init_val ~do_join:false cstr_val 
	    in
	    if debug_more then Coptions.lprintf
		"[build_safe_memory_access] initial value for precondition %a@."
		ContextLattice.pretty init_val;
	    init_val,pre_sep_val
	end
    | Some p_safe ->
	if debug then Coptions.lprintf
	    "[build_safe_memory_access] Trying to prove %a@." 
	    Var.P.pretty p_safe;
	(* [arrlen] variables should not be used in left parts of conditionals,
	   since their value cannot be tested by the programmer *)
	let pre_ctxt_val = 
	  ContextLattice.filter_variables ~remove:Var.is_arrlen pre_ctxt_val in
	let ctxt_val = ContextLattice.get_context pre_ctxt_val in
	let cstr_val = ContextLattice.Bridge.eval_constraint p_safe ctxt_val in
	let init_val = ContextLattice.eliminate_conditionals pre_ctxt_val in
	let init_val = 
	  ContextLattice.add_new_conditional init_val ~do_join:false cstr_val 
	in
	if debug_more then Coptions.lprintf
	    "[build_safe_memory_access] initial value %a@."
	    ContextLattice.pretty init_val;
	init_val,pre_sep_val

  let build_string_context node pre_val =
    if debug_more then Coptions.lprintf
      "[build_string_context] %a with value %a@." Node.pretty node
      ContextSepLattice.pretty pre_val;
    match string_access_predicate node with
      | Some p_string ->
	  let pre_ctxt_val,pre_sep_val = pre_val in
	  if debug then Coptions.lprintf
	    "[build_string_context] Trying to prove %a@."
	    Var.P.pretty p_string;
	  (* [arrlen] variables should not be used in left parts of 
	     conditionals, since their value cannot be tested by 
	     the programmer *)
	  let pre_ctxt_val = 
	    ContextLattice.filter_variables ~remove:Var.is_arrlen pre_ctxt_val
	  in
	  let ctxt_val = ContextLattice.get_context pre_ctxt_val in
	  let cstr_val = ContextLattice.Bridge.eval_constraint p_string ctxt_val
	  in
	  let init_val = ContextLattice.eliminate_conditionals pre_ctxt_val in
	  let init_val = 
	    ContextLattice.add_new_conditional init_val ~do_join:true cstr_val 
	  in
	  if debug_more then Coptions.lprintf
	    "[build_string_context] initial value %a@."
	    ContextLattice.pretty init_val;
	  init_val,pre_sep_val
      | None -> assert false

  let merge_node_select node =
    if debug_more then Coptions.lprintf
      "[merge_node_select] %a@." Node.pretty node;
    let res = is_invariant_node node || is_function_precondition_node node in
    if debug_more then Coptions.lprintf
      "[merge_node_select] selected ? %b@." res;
    res
      
  let keep_node_select node =
    merge_node_select node || is_assume_invariant_node node
    || is_loop_backward_source_node node 

  (* [cur_val] is the current contextual abstract value, obtained by repeated
     forward/backward propagation.
     [new_val] is the conditional information obtained through a unique 
     backward propagation. It should contain only one conditional at most.
   *)
  let store_context_info cur_val new_val =
    if debug_more then Coptions.lprintf
      "[store_context_info] from %a to %a@."
      ContextSepLattice.pretty cur_val ContextSepLattice.pretty new_val;
    let cur_ctxt_val,cur_sep_val = cur_val in
    let new_ctxt_val,_ = new_val in
    let new_ctxt_val = 
      if ContextLattice.has_conditionals new_ctxt_val then
	(* set as main context the context obtained by forward propagation *)
	let new_ctxt_val = ContextLattice.set_context new_ctxt_val 
	  (ContextLattice.get_context cur_ctxt_val) in
	(* subtract the main context from the conditional information *)
	let new_cid,new_do_join,new_cond = 
	  ContextLattice.format_singleton new_ctxt_val in
	(* add this minimal conditional information to the current context *)
	ContextLattice.add_conditional 
	  cur_ctxt_val new_do_join (new_cid,new_cond)
      else cur_ctxt_val
    in
    (* add invariants on strings and pointers *)
    (* let new_ctxt_val = normalize_info new_ctxt_val in *)
    (* renormalize resulting contextual value *)
    let new_ctxt_val = ContextLattice.normalize new_ctxt_val in
    if debug_more then Coptions.lprintf
      "[store_context_info] result %a@."
      ContextLattice.pretty new_ctxt_val;
    new_ctxt_val,cur_sep_val

  let join_context (ctxt1,sep1) (ctxt2,sep2) =
    ContextLattice.join_context ctxt1 ctxt2, SepLattice.join sep1 sep2

  let string_bnf_params =
    {
      compute         = compute_with_assert keep_node_select;
      join_context    = join_context;
      backward_select = string_test_select;
      backward_modify = build_string_context;
      merge_select    = merge_node_select;
      keep_select     = keep_node_select;
      merge_analyses  = store_context_info;
    }

  let compute_bnf_params =
    {
      compute         = compute_back_and_forth string_bnf_params;
      join_context    = join_context;
      backward_select = memory_access_select;
      backward_modify = build_safe_memory_access;
      merge_select    = merge_node_select;
      keep_select     = keep_node_select;
      merge_analyses  = store_context_info;
    }

  exception Rec_format

  type format_t =
     {
      analysis : absint_analysis_t;
      safe_access_nodes : NodeSet.t ref
     }

  let rec sub_format format_params node =
    let sub_nodes = code_children node @ (logic_children node) in
    List.iter (sub_format format_params) sub_nodes;
    try 
      (* transformation is possible only if analysis provides 
	 some information. Otherwise raise Not_found. *)
      let pre_val = match NodeHash.find_pre format_params.analysis node with
	| None -> raise Rec_format
	| Some v -> v
      in
      let pre_ctxt_val,pre_sep_val = pre_val in
      if debug_more then Coptions.lprintf 
	"[sub_format] %a %a@." 
	Node.pretty node ContextLattice.pretty pre_ctxt_val;
      match memory_access_safe_predicate ContextLattice.is_packed_variable 
	(ContextLattice.restrained_variables pre_ctxt_val) node
      with
      | None -> ()
      | Some p_safe ->
	  (* is the access guaranteed to be safe ? *)
	  if ContextLattice.guarantee_test p_safe pre_ctxt_val then
	    (* change type of dereference expression to make it a 
	       safe access *)
	    begin
	      if debug_more then Coptions.lprintf 
		  "[sub_format] safe access %a@." Node.pretty node;
	      format_params.safe_access_nodes := 
		NodeSet.add node (!(format_params.safe_access_nodes))
	    end
	  else
	    (* safe access cannot be guaranteed. Verification condition will
	       be generated for some prover to prove it. *)
	    ()
    with Rec_format -> ()

  (* remove abstract information on statements that do not change it.
     The analysis returned is only valid for post-analysis. *)
  let format analysis decls =

    (* modify [analysis] to take into account constraints, and return
       safe access nodes *)
    let format_params = 
      {
       analysis = analysis;
       safe_access_nodes = ref NodeSet.empty;
      }
    in
    List.iter (sub_format format_params) decls;

    let inv_analysis = NodeHash.create (NodeHash.length analysis) in
    NodeHash.iter_both (fun node pre_val post_val ->
      if NodeSet.mem node !(format_params.safe_access_nodes) then
	(* keep analysis value for nodes to transform. This is what
	   [transform] expects. *)
	let pre_val = match pre_val with 
	  | Some v -> v | None -> assert false in
	let post_val = match post_val with
	  | Some v -> v | None -> assert false in
	NodeHash.replace_both inv_analysis node pre_val post_val
      else if is_function_precondition_node node
	|| is_assume_invariant_node node
	|| is_invariant_node node
      then 
	let pre_ctxt_val,pre_sep_val = match pre_val with 
	  | Some v -> v | None -> assert false in
	let post_ctxt_val,post_sep_val = match post_val with
	  | Some v -> v | None -> assert false in
	(* finalize contextual value *)
	let pre_ctxt_val = 
	  ContextLattice.finalize pre_ctxt_val in
	let post_ctxt_val = 
	  ContextLattice.finalize post_ctxt_val in
	(* subtract assume invariant from invariant *)
	let pre_ctxt_val,pre_sep_val =
	  if is_invariant_node node then
	    match logic_invariant node with
	      | None -> pre_ctxt_val,pre_sep_val
	      | Some assinv ->
		  begin match NodeHash.find_pre analysis assinv with
		    | Some (assinv_val,assinv_sep) ->
		      	ContextLattice.subtract pre_ctxt_val assinv_val,
			SepLattice.subtract pre_sep_val assinv_sep
		    | None -> pre_ctxt_val,pre_sep_val
		  end
	  else pre_ctxt_val,pre_sep_val
	in
	let post_ctxt_val,post_sep_val =
	  if is_invariant_node node then
	    match logic_invariant node with
	      | None -> post_ctxt_val,post_sep_val
	      | Some assinv ->
		  begin match NodeHash.find_post analysis assinv with
		    | Some (assinv_val,assinv_sep) ->
		      	ContextLattice.subtract post_ctxt_val assinv_val,
			SepLattice.subtract post_sep_val assinv_sep
		    | None -> post_ctxt_val,post_sep_val
		  end
	  else post_ctxt_val,post_sep_val
	in
	(* rebuild complete abstract value *)
	let pre_val = pre_ctxt_val,pre_sep_val in
	let post_val = post_ctxt_val,post_sep_val in
	begin
	  if debug_more then Coptions.lprintf 
	    "[format] %a %a@." 
	    Node.pretty node ContextSepLattice.pretty pre_val;
	  NodeHash.replace_both inv_analysis node pre_val post_val
	end
      ) analysis;
    inv_analysis,!(format_params.safe_access_nodes)

end


(*****************************************************************************
 *                                                                           *
 * 		External interface for integer value analysis		     *
 *                                                                           *
 *****************************************************************************)

let local_int_analysis fundecl =

  if debug then Coptions.lprintf 
    "[local_int_analysis] treating function %s@." fundecl.f.fun_name; 

  (* build control-flow graph *)
  let _ = IntLangFromNormalized.from_file [fundecl] in

  (* collect the local variables used/declared *)
  let used_vars,decl_vars = IntLangFromNormalized.collect_vars () in
  (* pack all local integer variables together *)
  let il_pack_vars = 
    ILVarSet.elements (ILVarSet.fold ILVarSet.add used_vars decl_vars) in
  (* very rough packing that slows down the analysis. Should be improved on. *)
  let int_vars =
    List.filter IntLangFromNormalized.var_is_integer il_pack_vars in
  let ptr_vars = 
    List.filter IntLangFromNormalized.var_is_pointer il_pack_vars in
  let arrlen_vars = List.map (fun v -> Var.Varrlen v) ptr_vars in
  let strlen_vars = List.map (fun v -> Var.Vstrlen v) ptr_vars in
  let normal_vars = List.map (fun v -> Var.Vvar v) int_vars in
  let pack_vars = arrlen_vars @ strlen_vars @ normal_vars in

  let compute_new_decls pack_vars =
    (* only one pack for now *)
    ContextLattice.pack_variables [pack_vars];

    (* rebuild control-flow graph, needed because problem with 
       imperative Graph and exception *)
    let decls = IntLangFromNormalized.from_file [fundecl] in

    if Coptions.abstract_interp then

      (* build control-flow graph *)
      let end_decls = List.map snd decls in
      let decls = List.map fst decls in
      (* perform local separation analysis *)
      let analysis = LocalSeparationAnalysis.compute_back end_decls in
      let decls = ConnectCFGtoSep.transform analysis SepTrans decls in
      (* return the new program *)
      let decls = IntLangFromNormalized.to_file decls in

(* TO PERFORM STRING ANALYSIS IN ISOLATION
      (* re-build control-flow graph *)
      let decls = IntLangFromNormalized.from_file decls in
      let decls = List.map fst decls in
      (* perform local string analysis *)
      let comp_params = 
	LocalMemoryAnalysis.string_bnf_params in
      let raw_analysis = 
	LocalMemoryAnalysis.compute_back_and_forth comp_params decls in
      (* detect the statements where introducing an assume is useful *)
      let analysis,_ = 
	LocalMemoryAnalysis.format raw_analysis decls in
      (* transform the program using the analysis *)
      let trans_params = 
	{ safe_access_nodes = IntLangFromNormalized.NodeSet.empty } in
      let decls = ConnectCFGtoOct.transform analysis trans_params decls in
      (* return the new program *)
      let decls = IntLangFromNormalized.to_file decls in
*)
      (* re-build control-flow graph *)
      let decls = IntLangFromNormalized.from_file decls in
      let decls = List.map fst decls in
      (* perform local memory analysis *)
      let comp_params = 
	LocalMemoryAnalysis.compute_bnf_params in
      let raw_analysis = 
	LocalMemoryAnalysis.compute_back_and_forth comp_params decls in
      (* detect the statements where introducing an assume is useful *)
      let analysis,safe_access_nodes = 
	LocalMemoryAnalysis.format raw_analysis decls in
      (* transform the program using the analysis *)
      let trans_params = 
	{ safe_access_nodes = safe_access_nodes } in
      ConnectCFGtoOct.transform analysis trans_params decls

    else if Coptions.gen_invariant then

      let decls = List.map fst decls in
      (* perform local memory analysis *)
      let raw_analysis = LocalMemoryAnalysis.compute decls in
      (* detect the statements where introducing an assume is useful *)
      let analysis,_ = 
	LocalMemoryAnalysis.format raw_analysis decls in
      (* transform the program using the analysis *)
      let trans_params = 
	{ safe_access_nodes = IntLangFromNormalized.NodeSet.empty } in
      ConnectCFGtoOct.transform analysis trans_params decls      

    else assert false
  in
  
  (* add variables to pack until no more needed *)
  let rec compute_while_new_vars pack_vars =
    try 
      let decls = compute_new_decls pack_vars in
      (* return the new program *)
      IntLangFromNormalized.to_file decls

    with Var.Introduce_variable v ->
      begin
	if debug then Coptions.lprintf
	  "[compute_while_new_vars] adding variable %a@." Var.pretty v;
	compute_while_new_vars (v :: pack_vars)
      end
  in
  compute_while_new_vars pack_vars

let local_int_analysis_attach funcs =

  (* necessary prefix to translate the hash-table of functions in 
     the normalized code into a list of function representatives,
     as defined by type [func_t] in [Cabsint] *)
  let file = 
    List.fold_left 
      (fun acc func ->
	 try
	   let name = func.fun_name in
	   let (spec,typ,f,s,loc) = Hashtbl.find Cenv.c_functions name in
	   { name = name; spec = spec; typ = typ; f = f; s = s; loc = loc } 
	   :: acc
	 with Not_found -> acc
      ) [] funcs
  in

  if debug_more then Coptions.lprintf 
    "[local_int_analysis_attach] %i functions to treat@." (List.length file); 

  (* update each function information as soon as treated, so that called
     function information is up-to-date when caller treated *)
  List.iter 
    (fun fundecl -> 
       (* do not analyze functions that already have a precondition *)
       match fundecl.spec.requires with
	 | Some _ -> ()
	 | None ->
	     let newfundecl = local_int_analysis fundecl in
	     (* necessary suffix to translate the list of function
		representatives to the hash-table format *)
	     List.iter (fun { name = name; spec = spec; typ = typ; 
			      f = f; s = s; loc = loc } ->
			  Cenv.add_c_fun name (spec,typ,f,s,loc)) newfundecl
    ) file;
  
  if debug_more then Coptions.lprintf 
    "[local_int_analysis_attach] %i functions treated@." (List.length file);

(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)

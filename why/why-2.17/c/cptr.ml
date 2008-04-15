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

(* $Id: cptr.ml,v 1.27 2008/11/05 14:03:14 filliatr Exp $ *)

(* TO DO:

   - treat correctly variables whose address is taken (probably do not treat
   them at all). Problem even without option on code:
   void f(int* p) {
     int** q = &p;
     ( *q )++;
     *p = 0;
   }
   Definition of [q] printed in cnorm:
     int** q = p;

   - change the naming scheme for offset variables.
   Currently, offset variable for pointer [p] is either "p_offset" or
   "p_self_offset". Neither one is satisfactory, since a variable could
   already be called like this. The solution should use "__" in front of
   the name, to make it a reserved name by the compiler, e.g.
   "__offset_p" and "__self_offset_p". We must only check that such names
   are correctly understood by the provers, or translated appropriately.

   - add same transformation for integers, e.g. on [size] in
         while (size --) *p++ = *q++;
   --> DONE. document it.
   
   - take into account possible non-initialization

*)

open Info
open Clogic
open Cast
open Cutil
open Cabsint

let debug = Coptions.debug
let debug_more = false


(*****************************************************************************
 *                                                                           *
 * 		Pointer lattice used for local aliasing analysis             *
 *                                                                           *
 *****************************************************************************)

module type POINTER_SEMI_LATTICE = sig
  include SEMI_LATTICE
  type var_t
    (* in the following, an [alias] is the same as an [index] of 0.
       It is provided for the sake of simplicity. *)
    (* constructors *)
  val make_alias : var_t -> t
  val make_index : var_t -> int -> t
  val make_offset : var_t -> var_t option -> t
  val make_defined : var_t -> var_t option -> t
    (* query functions *)
  val is_alias : t -> bool
  val is_index : t -> bool
  val is_self_index : var_t -> t -> bool
  val is_offset : t -> bool
  val is_self_offset : var_t -> t -> bool
  val is_defined : t -> bool
  val is_top : t -> bool
    (* destructors *)
  val get_alias : t -> var_t
  val get_index : t -> var_t * int
  val get_offset_opt : t -> var_t * var_t option
  val get_offset : t -> var_t * var_t
  val get_defined_opt : t -> var_t * var_t option
  val has_variable : t -> bool
  val get_variable : t -> var_t
end

module Make_PtrSemiLattice(V : VARIABLE)
    : POINTER_SEMI_LATTICE with type var_t = V.t and type dim_t = unit =
struct

  type var_t = V.t

  (* different kinds of pointers, depending on their local aliasing,
     obtained through assignments. 
     Only relevant for local variables, i.e. parameters and locals, 
     because globals could be modified inside a function call. *)
  type t =
        (* result of join between incompatible pointers on different paths *)
    | PKcomplex
	(* not known offset from local/global variable or parameter.
	   The optional variable will be set during the analysis to be 
	   an integer variable containing the actual offset. *)
    | PKoffset of V.t * V.t option
	(* known index from local/global variable or parameter.
	   In the particular case of a 0 index, it represents an alias of 
	   a local/global variable or parameter. *)
    | PKindex of V.t * int 
	(* defined pointer, but definition was too complex to be classified
	   as an offset or an index pointer. 
	   The 1st variable allows for a common treatment with other kinds.
	   The optional variable will be possibly set during the analysis 
	   to be an integer variable containing an offset to reset to 0
	   when assigning the pointer variable.
	   Conceptually, [PKdefined] is not far from [PKindex] on the same 
	   variable with index 0. *)
    | PKdefined of V.t * V.t option
	(* undefined pointer *)
    | PKundefined

  type dim_t = unit
  let top () = PKcomplex
  let bottom () = PKundefined
  let init = bottom

  (* get underlying variable if any *)
  let get_var_opt p1 = match p1 with
    | PKundefined | PKcomplex -> None
    | PKindex (v,_) | PKoffset (v,_) | PKdefined (v,_) -> Some v

  let same_var p1 p2 = match get_var_opt p1,get_var_opt p2 with
    | Some v1,Some v2 -> V.compare v1 v2 = 0
    | _ -> false

  let is_not_self_index_offset p = match p with
    | PKindex _ | PKoffset _ -> true
    | _ -> false

  (* constructors *)
  let make_alias v = PKindex (v,0)
  let make_index v i = PKindex (v,i)
  let make_offset v vo = PKoffset (v,vo)
  let make_defined v vo = PKdefined (v,vo)

  (* query functions *)
  let is_alias t = match t with PKindex (_,0) -> true | _ -> false
  let is_index t = match t with PKindex _ -> true | _ -> false
  let is_self_index var t = 
    match t with PKindex (v,_) -> V.equal var v | _ -> false
  let is_offset t = match t with PKoffset _ -> true | _ -> false
  let is_self_offset var t = 
    match t with PKoffset (v,_) -> V.equal var v | _ -> false
  let is_defined t = match t with PKdefined _ -> true | _ -> false
  let is_top t = match t with PKcomplex -> true | _ -> false

  (* destructors *)
  let get_alias t = match t with 
    | PKindex (v,0) -> v 
    | _ -> assert false
  let get_index t = match t with 
    | PKindex (v,i) -> v,i 
    | _ -> assert false
  let get_offset_opt t = match t with 
    | PKoffset (v,vo) -> v,vo 
    | _ -> assert false
  let get_offset t = match t with 
    | PKoffset (v,o) ->
	begin match o with
	  | Some off -> v,off
	  | _ -> 
	      (* when querying an [offset] pointer kind, we expect 
		 the offset variable to be set *)
	      assert false
	end
    | _ -> assert false
  let get_defined_opt t = match t with 
    | PKdefined (v,vo) -> v,vo 
    | _ -> assert false
  let has_variable t = match t with
    | PKindex _ | PKoffset _ | PKdefined _ -> true
    | _ -> false
  let get_variable t = match t with
    | PKindex _ -> fst (get_index t)
    | PKoffset _ -> fst (get_offset_opt t)
    | PKdefined _ -> fst (get_defined_opt t)
    | _ -> assert false

  let equal p1 p2 = match p1,p2 with
    | PKundefined,PKundefined -> true
    | PKindex (v1,o1),PKindex (v2,o2) ->
	V.equal v1 v2 && o1 = o2
    | PKoffset (v1,o1),PKoffset (v2,o2)
    | PKdefined (v1,o1),PKdefined (v2,o2) ->
	V.equal v1 v2 && (Option.equal V.equal o1 o2)
    | PKcomplex,PKcomplex -> true
    | _ -> false

  let pretty fmt t = match t with
    | PKundefined -> Format.fprintf fmt "PKundefined"
    | PKindex (v,i) -> Format.fprintf fmt "PKindex(%a,%d)" V.pretty v i
    | PKoffset (v,o) -> 
	begin
	  match o with
	    | None -> Format.fprintf fmt "PKoffset(%a)" V.pretty v
	    | Some vo -> 
		Format.fprintf fmt "PKoffset(%a,%a)" V.pretty v V.pretty vo
	end
    | PKdefined (v,o) ->
	begin
	  match o with
	    | None -> Format.fprintf fmt "PKdefined(%a)" V.pretty v
	    | Some vo -> Format.fprintf 
		fmt "PKdefined(%a,%a)" V.pretty v V.pretty vo
	end
    | PKcomplex -> Format.fprintf fmt "PKcomplex"

  (* lattice associated to var [v] has the following form:

                              PKcomplex
                                   |
                              PKoffset(v,o)
                                   |
                              PKindex(v,i)
                                   |
                              PKdefined(v,o)
                                   |
                              PKundefined

     2 different lattices for [u] and [v] only connect at top and bottom.
  *)

  (* 2 pointer kinds are comparable only if they have the same underlying 
     variable, if any. The opposite is true too, except for pairs of the same
     pointer kind with different second argument. 
     A subtlety is that a PKdefined pointer is comparable to a PKindex one
     only if the index is 0. *)
  let comparable p1 p2 = match get_var_opt p1,get_var_opt p2 with
    | Some v1,Some v2 -> 
	let second_arg_same = 
	  if is_offset p1 && (is_offset p2) then
	    let o1 = snd (get_offset_opt p1) in
	    let o2 = snd (get_offset_opt p2) in
	    Option.equal V.equal o1 o2
	  else if is_index p1 && (is_index p2) then
	    let o1 = snd (get_index p1) in
	    let o2 = snd (get_index p2) in
	    o1 = o2
	  else if is_defined p1 && (is_defined p2) then
	    let o1 = snd (get_defined_opt p1) in
	    let o2 = snd (get_defined_opt p2) in
	    Option.equal V.equal o1 o2
	  else true
	in
	let defined_with_index0 =
	  if is_defined p1 || (is_defined p2) then
	    if is_index p1 then snd (get_index p1) = 0
	    else if is_index p2 then snd (get_index p2) = 0
	    else true
	  else true
	in
	V.compare v1 v2 = 0 && second_arg_same && defined_with_index0
    | _ -> true

  (* p1 <= p2 *)
  let inf p1 p2 = 
    comparable p1 p2 &&
      match p1,p2 with
            (* PKundefined is the bottom element *)
	| PKundefined,_ -> true
	| _,PKundefined -> false
	    (* PKdefined is the next lowest element *)
	| PKdefined _,_ -> true
	| _,PKdefined _ -> false
	    (* PKindex is the next lowest element *)
	| PKindex _,_ -> true
	| _,PKindex _ -> false
	    (* PKoffset is the next lowest element *)
	| PKoffset _,_ -> true
	| _,PKoffset _ -> false
	    (* PKcomplex is the top element *)
	| PKcomplex,PKcomplex -> true

  (* union *)
  let rec join ?(backward=false) p1 p2 = 
    if not (comparable p1 p2) then 
      if same_var p1 p2 then
	(* case of interest here is 2 PKindex with different indices,
	   or equivalently a PKdefined with a PKindex of index different 
	   from 0 *)
	let v = get_variable p1 in
	PKoffset (v,None)
      else
	PKcomplex
    else if inf p2 p1 && not (equal p2 p1) then join p2 p1 else
      (* only treat here the case where p1 <= p2 *)
      match p1,p2 with
          (* PKundefined is the bottom element *)
	| PKundefined,p -> p
	    (* PKdefined is the next lowest element *)
	| PKdefined _,p -> p
	    (* PKindex is the next lowest element *)
	| PKindex _,p -> p
	    (* PKoffset is the next lowest element *)
	| PKoffset _,p -> p
	    (* PKcomplex is the top element *)
	| PKcomplex,_ -> PKcomplex

    (* widening should not be used on this finite lattice *)
    let widening _ = assert false
end


(*****************************************************************************
 *                                                                           *
 * 		Concrete modules for local pointer analysis		     *
 *                                                                           *
 *****************************************************************************)

(* We make the following choices in the following:
   - variables are represented by an entity of type [Info.var_info]
   - the intermediate language is the normalized AST as described by
   Cast.ndecl/nstatement/nexpr
   
   Other choices are perfectly possible, e.g. the pre-normalized AST
   as described by Cast.tdecl/tstatement/texpr as intermediate language.
*)

(* it happens to be the same as ILVar *)
module Var : VARIABLE with type t = var_info = struct
  type t = var_info
  let pretty fmt v = Format.fprintf fmt "%s" v.var_name
  let to_string v = v.var_name
  let compare v1 v2 = Pervasives.compare v1.var_uniq_tag v2.var_uniq_tag
  let equal v1 v2 = compare v1 v2 = 0
  let hash v = v.var_uniq_tag
end

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)

module PtrLattice 
    : POINTER_SEMI_LATTICE with type var_t = Var.t and type dim_t = unit
  = Make_PtrSemiLattice(Var)

module PointWisePtrLattice = 
  Make_PointWiseSemiLattice(Var)(PtrLattice)

(* specialized intermediate language for pointer analysis *)

module PtrLangFromNormalized : sig
  include CFG_LANG_EXTERNAL
  
  (* query functions *)
    (* if the right-hand side of this assignment is a variable, 
       return it, with an optional constant offset if known.
       Only when [assign_get_local_lhs_var] has been called before
       with success. *)
  val assign_get_rhs_var : Node.t -> ilvar_t option * int option
    (* takes a variable expression or a pointer/integer addition 
       of an expression and an offset.
       returns as 1st item the possible variable in the addition.
       returns as 2nd item the possible offset expression in the addition. *)
  val get_intptr_add_on_var_opt : Node.t -> ilvar_t option * Node.t option

  (* constructors.
     - [make_] functions operate directly on their arguments.
     - [change_] functions take a first node as context, and operate on 
     other arguments.

     Most of those functions are valid both for pointer and integer variables.
  *)
    (* create a new node integer/pointer expression + constant *)
  val make_intptr_expr_add_cst : Node.t -> int -> Node.t
    (* create a new node integer/pointer expression + expression *)
  val make_intptr_expr_add_expr : Node.t -> Node.t -> Node.t
    (* make this node be a term/expression over an integer/pointer variable *)
  val change_in_intptr_var : Node.t -> ilvar_t -> Node.t
    (* make this node be a term/expression of a sum variable + constant *)
  val change_in_intptr_var_add_cst : Node.t -> ilvar_t -> int -> Node.t
    (* make this node be a term/expression of a sum variable + variable *)
  val change_in_intptr_var_add_var : Node.t -> ilvar_t -> ilvar_t -> Node.t
    (* make this node be an increment/decrement of the 2nd node,
       materialized as a pointer addition.
       The boolean is [true] if the function is called on the result of
       the assignment, to get back a pointer expression for the evaluation
       of the increment/decrement expression.
       The boolean is [false] if the function is called before the assignment,
       to get the result of the increment/decrement evaluation. *)
  val change_in_intptr_incr : Node.t -> Node.t -> bool -> Node.t

    (* make this node be an increment/decrement of the variable,
       materialized as an integer addition, that is assigned to
       the 1st node *)
  val change_in_int_incr_assign : Node.t -> ilvar_t -> Node.t
    (* change the structural sub-components of this node.
       Supersede the same function from CFGLangFromNormalized. *)
  val change_sub_components : Node.t -> Node.t list -> Node.t 
    (* add new local variables to this declaration node.
       The boolean is [true] if the variables are zero-initialized. *)
  val introduce_new_vars : Node.t -> ilvar_t list -> bool -> Node.t

end = struct
  
  include CFGLangFromNormalized

  (* more elaborate query functions related to pointer usage *)

  let assign_get_rhs_var node =
    let rec get_rhs_var e = match e.nexpr_node with
      | NEvar (Var_info var) -> 
	  (* direct alias *)
	  Some var,Some 0
      | NEincr _ | NEassign _ | NEassign_op _ ->
	  (* right-hand side is itself an assignment *)
	  begin match sub_assign_get_lhs_var e with
	    | None -> None,None (* rhs variable not identified *)
	    | Some var ->
		(* rhs variable has been identified *)
		(* post- and pre- increment/decrement not treated uniformly
		   since we are interested in the -value- resulting from
		   this operation *)
		begin match e.nexpr_node with
		  | NEincr (Upostfix_inc,_) ->
		      Some var,Some (-1)
		  | NEincr (Upostfix_dec,_) ->
		      Some var,Some 1
		  | _ -> Some var,Some 0
		end
	  end
      | NEbinary (e1,(Badd_pointer_int | Badd_int _ | Bsub_int _ as op),e2) ->
	  (* right-hand side is an offset from a pointer/integer expression *)
	  begin match get_rhs_var e1 with
	    | Some var,Some off ->
		(* rhs is an known offset from some variable *)
		begin match e2.nexpr_node with
		  | NEconstant (IntConstant s) ->
		      (* rhs is constant offset from variable *)
		      begin
			try 
			  let new_off = match op with
			    | Badd_pointer_int | Badd_int _ -> 
				off + (int_of_string s)
			    | Bsub_int _ ->
				off - (int_of_string s)
			    | _ -> assert false
			  in
			  (* constant offset is representable *)
			  Some var,Some new_off
			with Failure "int_of_string" ->
			  (* constant offset not representable *)
			  Some var,None
		      end
		  | _ -> Some var,None (* offset not known *)
		end
	    | _ -> None,None (* rhs not offset from variable *)
	  end
      | _ -> None,None (* rhs not recognized *)
    in
    match get_expr node with
      | NEincr (op,e) ->
	  begin match e.nexpr_node with
	    | NEvar (Var_info var) ->
		(* post- and pre- increment/decrement treated uniformy,
		   since we are interested here in the -assignment- resulting
		   from this operation *)
		begin match op with
		  | Upostfix_inc | Uprefix_inc ->
		      Some var,Some 1
		  | Upostfix_dec | Uprefix_dec ->
		      Some var,Some (-1)
		end
	    | _ -> None,None (* rhs not recognized *)
	  end
      | NEassign_op (e1,(Badd_pointer_int | Badd_int _ | Bsub_int _ as op),
		     e2) ->
	  begin match e1.nexpr_node with
	    | NEvar (Var_info var) ->
		begin match e2.nexpr_node with
		  | NEconstant (IntConstant s) ->
		      (* rhs is constant offset from variable *)
		      begin
			try 
			  let new_off = match op with
			    | Badd_pointer_int | Badd_int _ -> 
			        (int_of_string s)
			    | Bsub_int _ ->
			        - (int_of_string s)
			    | _ -> assert false
			  in
			  (* constant offset is representable *)
			  Some var,Some new_off
			with Failure "int_of_string" ->
			  (* constant offset not representable *)
			  Some var,None
		      end
		  | _ -> Some var,None (* offset not known *)
		end
	    | _ -> None,None (* rhs not recognized *)
	  end
      | NEassign (_,e) ->
	  get_rhs_var e
      | _ -> failwith ("[assign_get_rhs_var] should be called on assignment")

  let rec get_intptr_add_on_var_opt node =
    if debug_more then Coptions.lprintf 
      "[get_intptr_add_on_var_opt] %a@." Node.pretty node;
    (* a cast may have been introduced for arrays used as pointers *)
    let node = skip_casts node in
    let e = get_e node in
    match e.nexpr_node with
      | NEvar _ -> None,None
      | NEbinary (e1,(Badd_pointer_int | Badd_int _ | Bsub_int _ as op),e2) ->
	  let e2 = match op with
	    | Bsub_int _ -> { e2 with nexpr_node = NEunary (Uminus,e2) }
	    | _ -> e2
	  in
	  let add_opt = Some (create_tmp_node (Nexpr e2)) in
	  let var_opt = match e1.nexpr_node with
	    | NEvar (Var_info v) -> Some v
	    | _ -> None
	  in
	  var_opt,add_opt
      | _ -> assert false

  (* constructors *)

  (* expr + neg_cst is coded as expr + (-abs(neg_cst))
     This format is expected by [Cconst] module. *)
  let make_intptr_expr_add_cst node cst =
    let e = get_e node in
    let cst_e = NEconstant (IntConstant (string_of_int (abs cst))) in
    let cst_e = { e with nexpr_node = cst_e; nexpr_type = int_offset_type } in
    let cst_e = if cst >= 0 then cst_e else
      { cst_e with nexpr_node = NEunary (Uminus,cst_e) } in
    let addop = 
      if expr_type_is_ptr node then Badd_pointer_int
      else if expr_type_is_int node then int_offset_addop
      else assert false in
    let new_e = NEbinary (e,addop,cst_e) in
    let new_e = { e with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  let make_intptr_expr_add_expr node1 node2 =
    let e1 = get_e node1 in
    let e2 = get_e node2 in
    let addop = 
      if expr_type_is_ptr node1 then Badd_pointer_int
      else if expr_type_is_int node1 then int_offset_addop
      else assert false in
    let new_e = NEbinary (e1,addop,e2) in
    let new_e = { e1 with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  (* deals with array address used as pointer *)
  let change_in_intptr_var node var =
    match get_node_kind node with
      | NKexpr | NKtest | NKlvalue -> 
	  let e = get_e node in
	  let var_e = NEvar (Var_info var) in
	  let var_e = { e with nexpr_node = var_e } in
	  let new_e =
	    match var.var_type.Ctypes.ctype_node with 
	      | Ctypes.Tarray (valid,ty,_) -> 
		  let cast_e = NEcast (Ctypes.c_pointer valid ty,var_e) in
		  { e with nexpr_node = cast_e }
	      | _ -> var_e
	  in
	  create_tmp_node (Nexpr new_e) 
      | NKterm ->
	  let t = get_t node in
	  let var_t = NTvar var in
	  let var_t = { t with nterm_node = var_t } in
	  let new_t = 
	    match var.var_type.Ctypes.ctype_node with 
	      | Ctypes.Tarray (valid,ty,_) -> 
		  let cast_t = NTcast (Ctypes.c_pointer valid ty,var_t) in
		  { t with nterm_node = cast_t }
	      | _ -> var_t
	  in
	  create_tmp_node (Nterm new_t)
      | _ -> assert false

  (* var + neg_cst is coded as var + (-abs(neg_cst))
     This format is expected by [Cconst] module. *)
  let change_in_intptr_var_add_cst node var offset =
    let var_te = change_in_intptr_var node var in
    match get_node_kind node with
      | NKexpr | NKtest | NKlvalue -> 
	  let e = get_e node in
	  let cst_e = NEconstant (IntConstant (string_of_int (abs offset))) in
	  let cst_e = { e with nexpr_node = cst_e; 
			  nexpr_type = int_offset_type } in
	  let cst_e = if offset >= 0 then cst_e else
	    { cst_e with nexpr_node = NEunary (Uminus,cst_e) } in
	  let addop = 
	    if var_is_pointer var then Badd_pointer_int
	    else if var_is_integer var then int_offset_addop
	    else assert false in
	  let new_e = NEbinary (get_e var_te, addop, cst_e) in
	  let new_e = { e with nexpr_node = new_e } in
	  create_tmp_node (Nexpr new_e)
      | NKterm ->
	  let t = get_t node in
	  let cst_t = NTconstant (IntConstant (string_of_int (abs offset))) in
	  let cst_t = { t with nterm_node = cst_t; 
			  nterm_type = int_offset_type } in
	  let cst_t = if offset >= 0 then cst_t else
	    { cst_t with nterm_node = NTunop (Clogic.Uminus,cst_t) } in
	  let new_t = NTbinop (get_t var_te, Clogic.Badd, cst_t) in
	  let new_t = { t with nterm_node = new_t } in
	  create_tmp_node (Nterm new_t)
      | _ -> assert false

  let change_in_intptr_var_add_var node var offset_var =
    let var_te = change_in_intptr_var node var in
    match get_node_kind node with
      | NKexpr | NKtest | NKlvalue -> 
	  let e = get_e node in
	  let cst_e = NEvar (Var_info offset_var) in
	  let cst_e = { e with nexpr_node = cst_e; 
			  nexpr_type = int_offset_type } in
	  let addop = 
	    if var_is_pointer var then Badd_pointer_int
	    else if var_is_integer var then int_offset_addop
	    else assert false in
	  let new_e = NEbinary (get_e var_te, addop, cst_e) in
	  let new_e = { e with nexpr_node = new_e } in
	  create_tmp_node (Nexpr new_e)
      | NKterm ->
	  let t = get_t node in
	  let cst_t = NTvar offset_var in
	  let cst_t = { t with nterm_node = cst_t; 
			  nterm_type = int_offset_type } in
	  let new_t = NTbinop (get_t var_te, Clogic.Badd, cst_t) in
	  let new_t = { t with nterm_node = new_t } in
	  create_tmp_node (Nterm new_t)
      | _ -> assert false

  let change_in_intptr_incr node new_rhs_node is_after_assign =
    let e = get_e node in
    let op = match e.nexpr_node with NEincr (op,_) -> op | _ -> assert false in
    let is_inop_op op =
      if is_after_assign then
	(* prefix operations must have occurred after assignment *)
	op = Uprefix_inc || op = Uprefix_dec
      else
	(* postfix operations must not have occurred before assignment *)
	op = Upostfix_inc || op = Upostfix_dec
    in
    let is_add1_op op =
      if is_after_assign then
	(* postfix decrement should be reversed after asssignment *)
	op = Upostfix_dec
      else
	(* prefix increment must be coded before assignment *)
	op = Uprefix_inc
    in
    let is_sub1_op op =
      if is_after_assign then
	(* postfix increment should be reversed after asssignment *)
	op = Upostfix_inc
      else
	(* prefix decrement must be coded before assignment *)
	op = Uprefix_dec
    in
    if is_inop_op op then
      new_rhs_node
    else if is_add1_op op then
      make_intptr_expr_add_cst new_rhs_node 1
    else if is_sub1_op op then
      make_intptr_expr_add_cst new_rhs_node (-1)
    else
      assert false

  let change_in_int_incr_assign node offset_var =
    let e = get_e node in
    let var_e = NEvar (Var_info offset_var) in
    let var_e = { e with nexpr_node = var_e; nexpr_type = int_offset_type } in
    let new_e =
      match e.nexpr_node with
	| NEincr (op,_) -> 
	    (* keep the original increment/decrement operator for ergonomy
	       purposes. Postfix/prefix choice should have no effect here
	       since this node is not supposed to be used as 
	       a sub-expression. *)
	    NEincr (op,var_e)
	| _ -> assert false
    in
    let new_e = { e with nexpr_node = new_e; nexpr_type = int_offset_type } in
    create_tmp_node (Nexpr new_e)

  (* exception used to report unexpected encoding in [change_sub_components]
     or its sub-functions. *)
  exception Bad_encoding

  (* exception used to report CFGLangFromNormalized treatment should be 
     applied, instead of a special treatment for pointer analysis *)
  exception Use_inherited

  let change_sub_components_in_stat node sub_nodes =
    let s = get_s node in
    (* in case an assignment was transformed into its right-hand side,
       do not keep the resulting expression if useless.
       In general this could be seen as an optimization.
       Currently it is necessary because [Cinterp] module might fail
       on such code that it considers as a "statement expression". *)
    let rec useless_expr e = match e.nexpr_node with
	(* only possible "statement expression" cases returned 
	   by the transformation *)
      | NEconstant _ | NEvar _ -> true
      | NEbinary (e1,_,e2) -> (useless_expr e1) && (useless_expr e2)
      | NEunary (_,e1) | NEcast (_,e1) -> useless_expr e1
      | _ -> false
    in
    let simplify_expr e = match e.nexpr_node with
      | NEseq (e1,e2) ->
	  (* [e2] may be the pointer evaluation, useless here *)
	  if useless_expr e2 then e1 else e
      | _ -> e
    in
    let simplify_expr_under_stat e = 
      let e = simplify_expr e in
      match e.nexpr_node with
	| NEbinary (e1,_,e2) ->
	    (* [e1] may be the base pointer evaluation, useless here *)
	    if useless_expr e1 then e2 else e
	| _ -> e
    in
    let statement_expr_or_nop e = 
      let e = simplify_expr_under_stat e in
      if useless_expr e then NSnop else NSexpr e
    in
    let filter_nop sl =
      List.filter (fun s -> not (s.nst_node = NSnop)) sl
    in
    try let new_s = match s.nst_node with
      | NSexpr _e -> 
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  statement_expr_or_nop new_e
      | NSif (_e,_s1,_s2) ->
	  assert (List.length sub_nodes = 3);
	  let new_e,new_s1,new_s2 = list3 sub_nodes in
	  let new_e,new_s1,new_s2 
	    = get_e new_e,get_s new_s1,get_s new_s2 in
	  let new_e = simplify_expr new_e in
	  NSif (new_e,new_s1,new_s2)
      | NSwhile (_annot,_e,_s1) ->
	  assert (List.length sub_nodes = 3);
	  let new_e,new_s1,new_a = list3 sub_nodes in
	  let new_e,new_s1,new_a = get_e new_e,get_s new_s1,get_annot new_a in
	  let new_e = simplify_expr new_e in
	  NSwhile (new_a,new_e,new_s1)
      | NSdowhile (_annot,_s1,_e) ->
	  assert (List.length sub_nodes = 3);
	  let new_s1,new_e,new_a = list3 sub_nodes in
	  let new_s1,new_e,new_a = get_s new_s1,get_e new_e,get_annot new_a in
	  let new_e = simplify_expr new_e in
	  NSdowhile (new_a,new_s1,new_e)
      | NSfor (_annot,_einit,_etest,_eincr,_s1) ->
	  assert (List.length sub_nodes = 5);
	  let new_einit,new_etest,new_eincr,new_s1,new_a = list5 sub_nodes in
	  let new_einit,new_etest,new_eincr,new_s1,new_a
	    = get_e new_einit,get_e new_etest,
	    get_e new_eincr,get_s new_s1,get_annot new_a in
	  let new_einit = simplify_expr_under_stat new_einit in
	  let new_etest = simplify_expr new_etest in
	  let new_eincr = simplify_expr_under_stat new_eincr in
	  NSfor (new_a,new_einit,new_etest,new_eincr,new_s1)
      | NSblock _sl ->
	  let new_sl = filter_nop (List.map get_s sub_nodes) in
	  NSblock new_sl
      | NSreturn (Some _e) -> 
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  let new_e = simplify_expr new_e in
	  NSreturn (Some new_e)
      | NSdecl (typ,var,Some cinit,_s1) ->
	  assert (List.length sub_nodes = 2);
	  let new_e,new_s1 = list2 sub_nodes in
	  let new_e = get_e new_e in
	  let new_e = simplify_expr new_e in
	  let new_s1 = get_s new_s1 in
	  begin try
	    let lhs_expr,rhs_expr = match new_e.nexpr_node with 
	      | NEassign (lhs_expr,rhs_expr) -> lhs_expr,rhs_expr
	      | _ -> raise Bad_encoding in
	    let new_var = match lhs_expr.nexpr_node with
	      | NEvar (Var_info new_var) -> new_var
	      | _ -> raise Bad_encoding
	    in
	    if ILVar.equal var new_var then
	      (* not an offset assignment *)
	      let new_cinit = decode_decl_list rhs_expr in
	      NSdecl (typ,var,Some new_cinit,new_s1)
	    else if var_is_pointer new_var then
	      (* neither the original variable nor an offset 
		 assignment. It must be of pointer type. *)
	      raise Bad_encoding
	    else
	      begin
		(* offset assignment.
		   Can only occur for [Iexpr] initializer, because 
		   [Ilist] initializer is encoded into a call, which
		   can only lead to a complex pointer kind for [var].
		   The variable [var] is not used anymore in the new
		   program. We can safely eliminate it. *)
		match cinit with
		  | Iexpr _e ->
		      (* keep the offset initialization *)
		      let new_typ = new_var.var_type in
		      let new_cinit = decode_decl_list rhs_expr in
		      let offset_stat = 
			NSdecl (new_typ,new_var,Some new_cinit,new_s1) in
		      let offset_stat = { s with nst_node = offset_stat } in
		      (* always keep the pointer declaration *)
		      NSdecl (typ,var,None,offset_stat)
		  | _ -> assert false
	      end
	  with Bad_encoding ->
	    (* exception [Bad_encoding] was raised if the encoded
	       assignment was neither maintained nor transformed 
	       into an offset assignment. This can happen if
	       the assignment was detected as useless, and therefore
	       only the right-hand side was returned. Keep it. *)
	    let new_s = statement_expr_or_nop new_e in
	    match new_s with
	      | NSnop ->
		  (* always keep the pointer declaration *)
		  NSdecl (typ,var,None,new_s1)
	      | _ ->
		  let new_stat = { s with nst_node = new_s } in
		  let block_stat = NSblock [new_stat;new_s1] in
		  let block_stat = { s with nst_node = block_stat }
		  in
		  (* always keep the pointer declaration *)
		  NSdecl (typ,var,None,block_stat)
	  end
      | NSswitch (_e,c,cases) -> 
	  let new_e = List.hd sub_nodes in
	  let new_cases = List.tl sub_nodes in
	  let new_e = get_e new_e in
	  let new_e = simplify_expr new_e in
	  (* remove [Nfwd] node introduced for each [case] *)
	  let new_cases = 
	    List.map (fun n -> code_children n) new_cases in
	  let new_cases = List.map (List.map get_s) new_cases in
	  let new_cases = 
	    List.map2 (fun (cmap,_) sl -> (cmap,sl)) cases new_cases in
	  NSswitch (new_e,c,new_cases)
      | NSnop | NSlogic_label _ | NSassert _ | NSassume _ | NSreturn None 
      | NSbreak | NScontinue | NSgoto _ | NSlabel _ | NSspec _
      | NSdecl (_,_,None,_) ->
	  raise Use_inherited
    in
    let new_s = { s with nst_node = new_s } in
    create_tmp_node (Nstat new_s)
    with Use_inherited ->
      CFGLangFromNormalized.change_sub_components_in_stat node sub_nodes

  let change_sub_components_in_expr node sub_nodes =
    let e = get_e node in
    try let new_e = match e.nexpr_node with
      | NEbinary (_e1,op,_e2) ->
	  assert (List.length sub_nodes = 2);
	  let new_e1,new_e2 = list2 sub_nodes in
	  let new_e1,new_e2 = get_e new_e1,get_e new_e2 in
	  begin match op with
	    | Bsub_pointer | Blt_pointer | Bgt_pointer | Ble_pointer 
	    | Bge_pointer | Beq_pointer | Bneq_pointer ->
		(* binary operation on pointers. If both arguments are
		   indices/offsets from the same pointer, translate
		   the pointer operation into an equivalent integer 
		   operation. *)
		let rec destr_ptr_off e = match e.nexpr_node with
		  | NEvar (Var_info v) -> Some (v,None)
		  | NEcast (_,e3) ->
		      (* deals with array address used as pointer *)
		      begin match e3.nexpr_node with
			| NEvar (Var_info v) -> Some (v,None)
			| _ -> None
		      end
		  | NEbinary(e1,Badd_pointer_int,e2) ->
		      (* recursive call *)
		      begin match destr_ptr_off e1 with
			| Some (v,None) ->
			    Some (v,Some e2)
			| Some (v,Some e3) ->
			    let e2 = create_tmp_node (Nexpr e2) in
			    let e3 = create_tmp_node (Nexpr e3) in
			    let e4 = make_int_termexpr_add_termexpr e2 e3
			    in
			    Some (v,Some (get_e e4))
			| None -> None
		      end
		  | _ -> None
		in
		let pointer_op_to_int_op op = match op with
		  | Bsub_pointer -> Bsub_int int_offset_kind
		  | Blt_pointer -> Blt_int
		  | Bgt_pointer -> Bgt_int
		  | Ble_pointer -> Ble_int
		  | Bge_pointer -> Bge_int
		  | Beq_pointer -> Beq_int
		  | Bneq_pointer -> Bneq_int
		  | _ -> assert false
		in
		begin match destr_ptr_off new_e1,destr_ptr_off new_e2 with
		  | Some (v1,e3),Some (v2,e4) ->
		      if ILVar.equal v1 v2 then 
			let op = pointer_op_to_int_op op in
			match e3,e4 with
			  | None,None -> 
			      let e3 = 
				NEconstant (IntConstant (string_of_int 0)) in
			      let e3 = { new_e1 with nexpr_node = e3;
					   nexpr_type = int_offset_type } in
			      let e4 = 
				NEconstant (IntConstant (string_of_int 0)) in
			      let e4 = { new_e2 with nexpr_node = e4;
					   nexpr_type = int_offset_type } in
			      NEbinary (e3,op,e4)
			  | None,Some e4 ->
			      let e3 = 
				NEconstant (IntConstant (string_of_int 0)) in
			      let e3 = { new_e1 with nexpr_node = e3;
					   nexpr_type = int_offset_type } in
			      NEbinary (e3,op,e4)
			  | Some e3,None ->
			      let e4 = 
				NEconstant (IntConstant (string_of_int 0)) in
			      let e4 = { new_e2 with nexpr_node = e4;
					   nexpr_type = int_offset_type } in
			      NEbinary (e3,op,e4)
			  | Some e3,Some e4 ->
			      NEbinary (e3,op,e4)
		      else NEbinary (new_e1,op,new_e2)
		  | _ -> NEbinary (new_e1,op,new_e2)
		end
	    | _ -> NEbinary (new_e1,op,new_e2)
	  end
      | NEnop | NEconstant _ | NEstring_literal _ | NEvar _ | NEarrow _ 
      | NEunary _ | NEincr _ | NEcast _ | NEmalloc _ | NEseq _ | NEassign _
      | NEassign_op _ | NEcall _ | NEcond _ ->
	  raise Use_inherited
    in		
    let new_e = { e with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)
    with Use_inherited ->
      CFGLangFromNormalized.change_sub_components_in_expr node sub_nodes

  (* recognize offset from pointer *)
  let rec term_destr_ptr_off t = match t.nterm_node with
    | NTvar v -> Some (v,None)
    | NTcast (_,t3) -> (* deals with array address used as pointer *)
	begin match t3.nterm_node with
	  | NTvar v -> Some (v,None)
	  | _ -> None
	end
    | NTbinop(t1,Clogic.Badd,t2) ->
	(* recursive call *)
	begin match term_destr_ptr_off t1 with
	  | Some (v,None) ->
	      Some (v,Some t2)
	  | Some (v,Some t3) ->
	      let t2 = create_tmp_node (Nterm t2) in
	      let t3 = create_tmp_node (Nterm t3) in
	      let t4 = make_int_termexpr_add_termexpr t2 t3
	      in
	      Some (v,Some (get_t t4))
	  | None -> None
	end
    | _ -> None

  let change_sub_components_in_term node sub_nodes =
    let t = get_t node in
    try let new_t = match t.nterm_node with
      | NTbinop (_t1,op,_t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  begin match op with
	    | Clogic.Bsub ->
		(* could be a binary operation on pointers. 
		   If both arguments are indices/offsets from the same pointer,
		   translate the pointer operation into an equivalent integer 
		   operation. *)
		begin match term_destr_ptr_off new_t1,
		  term_destr_ptr_off new_t2 with
		    | Some (v1,t3),Some (v2,t4) ->
			if ILVar.equal v1 v2 then 
			  match t3,t4 with
			    | None,None -> 
				NTconstant (IntConstant (string_of_int 0))
			    | Some t,None ->
				t.nterm_node
			    | None,Some t ->
				NTunop (Clogic.Uminus,t)
			    | Some t3,Some t4 ->
				NTbinop (t3,op,t4)
			else NTbinop (new_t1,op,new_t2)
		    | _ -> NTbinop (new_t1,op,new_t2)
		end
	    | _ -> NTbinop (new_t1,op,new_t2)
	  end
      | NTrange (_t1,_t2opt,_t3opt,zone,info) ->
	  assert (List.length sub_nodes = 3);
	  let new_t1,new_t2,new_t3 = list3 sub_nodes in
	  let new_t1 = get_t new_t1 in
	  let new_t2 = match logic_children new_t2 with
	    | [new_t2] -> Some (get_t new_t2)
	    | [] -> None
	    | _ -> assert false (* bad encoding *)
	  in
	  let new_t3 = match logic_children new_t3 with
	    | [new_t3] -> Some (get_t new_t3)
	    | [] -> None
	    | _ -> assert false (* bad encoding *)
	  in
	  (* [new_t1] could be an offset from some pointer *)
	  begin match term_destr_ptr_off new_t1 with
	    | Some (v,Some t4) ->
		let new_t2 = match new_t2 with
		  | Some new_t2 -> 
		      let t2 = create_tmp_node (Nterm new_t2) in
		      let t4 = create_tmp_node (Nterm t4) in
		      Some (get_t (make_int_termexpr_add_termexpr t2 t4))
		  | None -> None
		in
		let new_t3 = match new_t3 with
		  | Some new_t3 -> 
		      let t3 = create_tmp_node (Nterm new_t3) in
		      let t4 = create_tmp_node (Nterm t4) in
		      Some (get_t (make_int_termexpr_add_termexpr t3 t4))
		  | None -> None
		in
		let new_t1 = { new_t1 with nterm_node = NTvar v } in
		NTrange (new_t1,new_t2,new_t3,zone,info)
	    | _ ->
		NTrange (new_t1,new_t2,new_t3,zone,info)
	  end
      | NTconstant _ | NTstring_literal _
      | NTvar _ | NTapp _ | NTunop _ | NTarrow _ | NTold _
      | NTat _ | NTbase_addr _ | NToffset _ | NTblock_length _ | NTarrlen _
      | NTstrlen _ | NTcast _ | NTif _ | NTmin _ | NTmax _ 
      | NTminint _ | NTmaxint _
	  -> raise Use_inherited
    in		
    let new_t = { t with nterm_node = new_t } in
    create_tmp_node (Nterm new_t)
    with Use_inherited ->
      CFGLangFromNormalized.change_sub_components_in_term node sub_nodes

  let change_sub_components_in_pred node sub_nodes =
    let p = get_p node in
    try let new_p = match p.npred_node with
      | NPrel (_t1,rel,_t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  (* could be a binary operation on pointers. 
	     If both arguments are indices/offsets from the same pointer,
	     translate the pointer operation into an equivalent integer 
	     operation. *)
	  begin match term_destr_ptr_off new_t1,term_destr_ptr_off new_t2 with
	    | Some (v1,t3),Some (v2,t4) ->
		if ILVar.equal v1 v2 then 
		  match t3,t4 with
		    | None,None -> 
			begin match rel with
			  | Le | Ge | Eq -> NPtrue
			  | Lt | Gt | Neq -> NPfalse
			end
		    | None,Some t4 ->
			let t3 = NTconstant (IntConstant (string_of_int 0)) in
			let t3 = { new_t1 with nterm_node = t3;
				     nterm_type = t4.nterm_type } in
			NPrel (t3,rel,t4)
		    | Some t3,None ->
			let t4 = NTconstant (IntConstant (string_of_int 0)) in
			let t4 = { new_t2 with nterm_node = t4;
				     nterm_type = t3.nterm_type } in
			NPrel (t3,rel,t4)
		    | Some t3,Some t4 ->
			NPrel (t3,rel,t4)
		else NPrel (new_t1,rel,new_t2)
	    | _ -> NPrel (new_t1,rel,new_t2)
	  end
      | NPfalse | NPtrue | NPapp _ | NPvalid_index _ | NPand _ | NPor _
      | NPimplies _ | NPiff _ | NPnot _ | NPforall _ | NPexists _ | NPold _
      | NPat _ | NPnamed _ | NPif _ | NPvalid _ | NPfresh _ 
      | NPvalid_range _ | NPseparated _ | NPfull_separated _ 
      | NPbound_separated _ ->
	  raise Use_inherited
    in		
    let new_p = { p with npred_node = new_p } in
    create_tmp_node (Npred new_p)
    with Use_inherited ->
      CFGLangFromNormalized.change_sub_components_in_pred node sub_nodes

  let change_sub_components node sub_nodes =
    match get_node_kind node with
      | NKstat ->
	  change_sub_components_in_stat node sub_nodes
	    
      | NKexpr | NKtest | NKlvalue ->
	  change_sub_components_in_expr node sub_nodes

      | NKterm ->
	  change_sub_components_in_term node sub_nodes
	    
      | NKpred | NKassume | NKassert -> 
	  change_sub_components_in_pred node sub_nodes
	    
      | NKnone | NKdecl | NKannot | NKspec ->
	  CFGLangFromNormalized.change_sub_components node sub_nodes

  let introduce_new_vars node var_list zero_init =
    let d = get_decl node in
    let new_d = match d.s with
      | Some s ->
	  let new_s = 
	    List.fold_left 
	      (fun next_s var -> 
		 let init = 
		   if zero_init then
		     let zero_cst = 
		       NEconstant (IntConstant (string_of_int 0)) in
		     let zero_expr = { nexpr_node = zero_cst; 
				       nexpr_type = int_offset_type;
				       nexpr_loc = Loc.dummy_position }
		     in
		     Some (Iexpr zero_expr)
		   else None
		 in
		 let decl_stat = 
		   NSdecl (var.var_type,var,init,next_s) in
		 { next_s with nst_node = decl_stat }
	      ) s var_list
	  in
	  { d with s = Some new_s }
      | None -> 
	  begin match var_list with
	    | [] -> d
	    | _ -> failwith ("[introduce_new_vars] should be called with"
			     ^ " empty [var_list] in this case")
	  end
    in
    create_tmp_node (Ndecl new_d)

end

module ConnectCFGtoPtr : sig
  include CONNECTION with type node_t = PtrLangFromNormalized.Node.t 
                and type 'a node_hash_t = 'a PtrLangFromNormalized.NodeHash.t 
		and type absval_t = PointWisePtrLattice.t
		and type transform_t = ILVar.t ILVarMap.t

    (* type of a map from variables to their offset variable *)
  type map_t = ILVar.t ILVarMap.t

    (* takes the result of the abstract interpretation.
       returns a formatted analysis easily exploited by [transform].
       The map returned is the map of all offset variables. *)
  val format : absint_analysis_t -> absint_analysis_t * map_t
    (* cleanup variable declarations and declare offset variables *)
  val cleanup : ILVarSet.t -> ILVarSet.t -> map_t -> node_t list -> node_t list
end = struct

  open PtrLangFromNormalized

  type node_t = Node.t
  type 'a node_hash_t = 'a NodeHash.t
  type absval_t = PointWisePtrLattice.t
  type 'a analysis_t = 'a pair_t node_hash_t
  type absint_analysis_t = absval_t analysis_t
  type map_t = ILVar.t ILVarMap.t
  type transform_t = map_t

  (* type used in [transfer] to compute the representant of a pointer *)
  type transfer_rep_t = 
      {
	orig_lhs_var : ILVar.t;
	orig_rhs_var : ILVar.t;
	is_index : bool;
	sum_index : int;
	is_offset : bool
      }

  (* no need for widening here as we are operating on a finite lattice *)
  let widening_threshold = None
  let widening_strategy = WidenFast (* any value would fit *)

  (* exception used in [representative] to end the search *)
  exception End_representative of int

  (* transfer function.
     Only interesting case is the assignment to a pointer variable,
     in which case we discriminate on the form of the right-hand side.
  *)
  let transfer ?(backward=false) ?(with_assert=false) ?(one_pass=false) 
      ?previous_value node cur_val =

    if debug_more then Coptions.lprintf 
      "[transfer] %a@." Node.pretty node;

    (* returns the representative integer/pointer kind for offset/index
       on [var], as described by [rep], with the invariant that [cur_val] 
       does not contain a loop between variables, except possible self-loops.
       [rep] is an accumulator that tells if on the current path of 
       representative integers/pointers, we found index or offset pointer
       kinds. [rep.orig_lhs_var] is the original variable for which we compute 
       a representant.
    *)
    let rec representative rep cur_val var =
      try
	let pval = PointWisePtrLattice.find var cur_val in
	(* [var] has itself a representant *)
	if PtrLattice.is_index pval then
	  let new_var,index = PtrLattice.get_index pval in
	  if ILVar.equal var new_var then
	    (* [var] is a self-index *)
	    raise (End_representative index)
	  else
	    let new_rep = { rep with is_index = true; 
			      sum_index = rep.sum_index + index } in
	    representative new_rep cur_val new_var
	else if PtrLattice.is_offset pval then
	  let new_var,_ = PtrLattice.get_offset_opt pval in
	  if ILVar.equal var new_var && rep.is_offset then
	    (* [var] is a self-offset *)
	    raise (End_representative 0)
	  else
	    let new_rep = { rep with is_offset = true } in
	    representative new_rep cur_val new_var
	else 
	  (* [var] has no representant *)
	  raise (End_representative 0)
      with End_representative last_index ->
	if rep.is_offset then
	  begin
	    if debug_more then Coptions.lprintf 
	      "[transfer] %a is represented by an offset of %a@."
	      ILVar.pretty rep.orig_lhs_var ILVar.pretty var;
	    PtrLattice.make_offset var None
	  end
	else if rep.is_index then
	  begin
	    (* In an assignment
	           q = p;
	       if [p] is a self-index, then count this self-index for [q].
	       But if [p] is an index on [r], which is itself a self-index, 
	       then only count [p]'s index for [q]. *)
	    let idx = 
	      if ILVar.equal rep.orig_rhs_var var then
		rep.sum_index + last_index
	      else
		rep.sum_index 
	    in
	    if debug_more then Coptions.lprintf 
	      "[transfer] %a is represented by an index of %i from %a@."
	      ILVar.pretty rep.orig_lhs_var idx ILVar.pretty var;
	    PtrLattice.make_index var idx
	  end
	else assert false
    in
    match get_node_kind node with
      | NKnone | NKstat | NKassume | NKassert -> cur_val

      | NKdecl ->
	  (* initially define an abstract value for parameters *)
	  let param_list = decl_get_params node in
	  List.fold_left 
	    (fun pw param ->
	       let init_val = PtrLattice.make_defined param None in
	       PointWisePtrLattice.replace param init_val pw
	    ) cur_val param_list

      | NKexpr | NKtest | NKlvalue ->
	  if expr_is_ptr_assign node || expr_is_int_assign node then
	    match assign_get_local_lhs_var node with
	      | None -> cur_val
	      | Some lhs_var ->
		  begin
		    (* compute new value for [lhs_var] *)
		    let rhs_val =
		      match assign_get_rhs_var node with
			| None,None -> 
			    if expr_is_int_assign node then
			      let rhs_node = 
				skip_casts (assign_get_rhs_operand node) 
			      in
			      if expr_is_int_constant rhs_node then
				(* avoid creating a self-offset variable for
				   an integer variable that is initialized
				   with some integer constant *)
				PtrLattice.top ()
			      else 
				PtrLattice.make_defined lhs_var None
			    else
			      PtrLattice.make_defined lhs_var None
			| Some rhs_var,None -> 
			    let rep = { orig_lhs_var = lhs_var; 
					orig_rhs_var = rhs_var; 
					is_index = false; 
					sum_index = 0; is_offset = true } in
			    representative rep cur_val rhs_var
			| Some rhs_var,Some index ->
			    let rep = { orig_lhs_var = lhs_var; 
					orig_rhs_var = rhs_var; 
					is_index = true;
					sum_index = index; is_offset = false }
			    in
			    representative rep cur_val rhs_var
			| _ -> assert false
		    in
		    let representative_or_var var rep_val = 
		      if PtrLattice.has_variable rep_val then
			PtrLattice.get_variable rep_val 
		      else var
		    in
		    let remove_ref_to_var rem_var var abs_val =
		      let rep_var = representative_or_var var abs_val in
		      if ILVar.equal rem_var rep_var then
			PtrLattice.top ()
		      else
			abs_val
		    in
		    (* remove information on variables previously represented
		       by [lhs_var] if this last variable representant 
		       changes *)
		    let old_val = PointWisePtrLattice.find lhs_var cur_val in
		    let old_rep_var = representative_or_var lhs_var old_val in
		    let new_rep_var = representative_or_var lhs_var rhs_val in
		    let cur_val =
		      if ILVar.equal old_rep_var new_rep_var
			&& not (PtrLattice.is_defined rhs_val) then
			cur_val
		      else
			PointWisePtrLattice.mapi 
			  (remove_ref_to_var lhs_var) cur_val
		    in
		    PointWisePtrLattice.replace lhs_var rhs_val cur_val
		  end
	  else cur_val
	    
      | NKspec | NKannot | NKterm | NKpred -> cur_val

  (* format function.
     Only the post-part of the analysis is relevant here.
     - If a variable is always referenced as an (un)defined or an index 
     pointer, leave it as such.
     - If a variable is always references as an (un)defined, index or offset
     pointer, make it always an offset pointer, with a fixed offset variable.
     - If a variable is sometimes references as a complex pointer, make it
     a complex pointer everywhere.

     Self-index/offset pointers are those that only reference themselves.

     This allows to exploit the pointer kind information locally to transform 
     the program.
  *)
  let format analysis =

    (* sets of variables of interest *)
    let index_vars = ref VarSet.empty in
    let self_index_vars = ref VarSet.empty in
    let offset_vars = ref VarSet.empty in
    let self_offset_vars = ref VarSet.empty in
    let defined_vars = ref VarSet.empty in
    let complex_vars = ref VarSet.empty in

    (* correspondance between representative variable and the variables 
       it represents at some point *)
    let rep_to_based = Hashtbl.create 0 in

    (* basic operations on sets of variables *)
    let add_to_set var set =
      set := VarSet.add var (!set) in
    let print_set set msg =
      Coptions.lprintf 
	"[format] detected %i %s integer/pointer(s)@."
	(VarSet.cardinal set) msg;
      if not (VarSet.is_empty set) then 
	Coptions.lprintf "[format] %a@." 
	  (fun _fmt -> (VarSet.iter (Coptions.lprintf "%a " ILVar.pretty))) set
    in
    let add_to_table rep_var var =
      let var_set =
	try 
	  VarSet.add var (Hashtbl.find rep_to_based rep_var)
	with Not_found ->
	  VarSet.add var VarSet.empty 
      in
      Hashtbl.replace rep_to_based rep_var var_set
    in

    (* only keep variables where read/written *)
    let analysis = 
      NodeHash.fold_post
	(fun node pwval new_analysis ->
	   match get_node_kind node with
	     | NKexpr | NKtest | NKlvalue | NKterm -> 
		 let new_pwval =
		   if termexpr_is_local_var node then
		     let var = termexpr_var_get node in
		     let pval = PointWisePtrLattice.find var pwval in 
		     PointWisePtrLattice.singleton var pval
		   else if (expr_is_ptr_assign node 
			    || expr_is_int_assign node) then
		     match assign_get_local_lhs_var node with
		       | None -> PointWisePtrLattice.bottom ()
		       | Some lhs_var ->
			   let pval = PointWisePtrLattice.find lhs_var pwval in
			   PointWisePtrLattice.singleton lhs_var pval
		   else PointWisePtrLattice.bottom ()
		 in
		 (* also store abstract value for rhs variable in assignment *)
		 let new_pwval =
		   match get_node_kind node with
		     | NKexpr | NKtest | NKlvalue ->
			 if expr_is_assign node then
			   match assign_get_rhs_var node with
			     | Some rhs_var,_ -> 
				 let pval = 
				   PointWisePtrLattice.find rhs_var pwval in 
				 PointWisePtrLattice.replace 
				   rhs_var pval new_pwval
			     | _ -> new_pwval
			 else new_pwval
		     | _ -> new_pwval
		 in
		 NodeHash.replace_post new_analysis node new_pwval;
		 new_analysis
	     | NKstat | NKpred | NKassume | NKassert
	     | NKnone | NKdecl | NKannot | NKspec ->
		 if is_logic_scope node then
		   NodeHash.replace_post new_analysis node pwval
		 else 
                   (* do not include abstract values for these constructs *)
		   (); 
		 new_analysis
	) analysis (NodeHash.create 0)
    in

    (* collect variables in the sets they match *)
    NodeHash.iter_post
	(fun _ pwval ->
	   PointWisePtrLattice.iter
	     (fun var pval ->
		(* if [pval] uses a representative variable, 
		   store this correspondance *)
		if PtrLattice.has_variable pval then
		  begin
		    let rep_var = PtrLattice.get_variable pval in
		    add_to_table rep_var var
		  end;
		(* dispatch [var] in sets according to [pval] *)
		if PtrLattice.is_index pval then
		  if PtrLattice.is_self_index var pval then
		    add_to_set var self_index_vars
		  else
		    add_to_set var index_vars
		else if PtrLattice.is_offset pval then
		  if PtrLattice.is_self_offset var pval then
		    add_to_set var self_offset_vars
		  else
		    add_to_set var offset_vars
		else if PtrLattice.is_defined pval then
		  add_to_set var defined_vars
		else if PtrLattice.is_top pval then
		  add_to_set var complex_vars
		else (* undefined integer/pointer value *)
		  ()
	     ) pwval
	) analysis;

    (* remove references *)
    let index_vars = !index_vars in
    let self_index_vars = !self_index_vars in
    let offset_vars = !offset_vars in
    let self_offset_vars = !self_offset_vars in
    let defined_vars = !defined_vars in
    let complex_vars = !complex_vars in
    if debug_more then print_set index_vars "basic index";
    if debug_more then print_set self_index_vars "basic self-index";
    if debug_more then print_set offset_vars "basic offset";
    if debug_more then print_set self_offset_vars "basic self-offset";
    if debug_more then print_set defined_vars "basic defined";
    if debug_more then print_set complex_vars "basic complex";

    (* all variables represented by a complex one are complex *)
    let rec close_set corresp set =
      let new_set =
	VarSet.fold (fun v vs -> 
		       try
			 let ns = Hashtbl.find corresp v in
			 VarSet.fold VarSet.add ns vs
		       with Not_found -> vs
		    ) set set
      in
      if VarSet.equal new_set set then
	set
      else
	close_set corresp new_set
    in
    let complex_vars = close_set rep_to_based complex_vars in

    (* compute cross-referencing integers/pointers *)
    let cross_vars = VarSet.union offset_vars index_vars in
    (* cross-referencing integers/pointers (index/offset) cannot be self-one *)
    let cross_rest = VarSet.inter self_offset_vars cross_vars in
    let self_offset_vars = VarSet.diff self_offset_vars cross_rest in
    let offset_vars = VarSet.union offset_vars cross_rest in
    (* offset integers/pointers must never be complex *)
    let offset_vars = VarSet.diff offset_vars complex_vars in
    if debug_more then print_set offset_vars "offset"; 
    (* self-offset integers/pointers must never be complex *)
    let self_offset_vars = VarSet.diff self_offset_vars complex_vars in
    if debug_more then print_set self_offset_vars "self-offset";
    (* compute sum of offset and self-offset integers/pointers *)
    let all_offset_vars = VarSet.union offset_vars self_offset_vars in

    (* index integers/pointers must never be offset *)
    let index_vars = VarSet.diff index_vars all_offset_vars in
    (* self-index integers/pointers must never be offset *)
    let self_index_vars = VarSet.diff self_index_vars all_offset_vars in
    (* index integers/pointers must never be complex *)
    let index_vars = VarSet.diff index_vars complex_vars in
    if debug_more then print_set index_vars "index";
    (* self_index integers/pointers must never be complex *)
    let self_index_vars = VarSet.diff self_index_vars complex_vars in
    if debug_more then print_set self_index_vars "self-index";

    (* defined integers/pointers must never be complex *)
    let defined_vars = VarSet.diff defined_vars complex_vars in

    (* associate a unique offset variable to every offset integer/pointer *)
    let offset_map =
      VarSet.fold (fun var m ->
		     let offset_var = 
		       Info.default_var_info (var.var_unique_name ^ "_offset")
		     in
		     Info.set_assigned offset_var;
		     Cenv.set_var_type 
		       (Var_info offset_var) int_offset_type false;
		     ILVarMap.add var offset_var m
		  ) offset_vars ILVarMap.empty 
    in
    let offset_map =
      VarSet.fold (fun var m ->
		     let offset_var = 
		       Info.default_var_info 
			 (var.var_unique_name ^ "_self_offset") in
		     Info.set_assigned offset_var;
		     Cenv.set_var_type 
		       (Var_info offset_var) int_offset_type false;
		     ILVarMap.add var offset_var m
		  ) self_offset_vars offset_map
    in

    (* compute the formatted analysis from the information just gathered *)
    let formatted_analysis = 
      NodeHash.fold_post
	(fun node pwval new_analysis ->
	   let new_pwval =
	     PointWisePtrLattice.mapi
	       (fun var pval ->
		  if VarSet.mem var index_vars then
		    (* pval = undefined/self-index/index/defined *)
		    pval 
		  else if VarSet.mem var self_index_vars then
		    (* pval = undefined/self-index/defined *)
		    pval
		  else if VarSet.mem var offset_vars then
		    (* pval = all except complex *)
		    if PtrLattice.is_defined pval then
		      (* explicit the offset variable for initialization *)
		      let offset_var = ILVarMap.find var offset_map in
		      PtrLattice.make_defined var (Some offset_var)
		    else if PtrLattice.is_index pval 
		      || PtrLattice.is_offset pval then
			(* explicit the offset variable *)
			let avar = PtrLattice.get_variable pval in
			let offset_var = ILVarMap.find var offset_map in
			PtrLattice.make_offset avar (Some offset_var)
		    else (* undefined *)
		      pval
		  else if VarSet.mem var self_offset_vars then
		    (* pval = undefined/self-index/self-offset/defined *)
		    if PtrLattice.is_defined pval then
		      (* explicit the offset variable for initialization *)
		      let offset_var = ILVarMap.find var offset_map in
		      PtrLattice.make_defined var (Some offset_var)
		    else if PtrLattice.is_index pval 
		      || PtrLattice.is_offset pval then
			(* explicit the offset variable *)
			let avar = PtrLattice.get_variable pval in
			assert (ILVar.equal avar var);
			let offset_var = ILVarMap.find var offset_map in
			PtrLattice.make_offset var (Some offset_var)
		    else (* undefined *)
		      pval
		  else if VarSet.mem var defined_vars then
		    (* pval = all except complex *)
		    PtrLattice.make_defined var None
		  else if VarSet.mem var complex_vars then
		    (* pval = any one is possible
		       Destroy any information associated to [var]. *)
		    PtrLattice.top ()
		  else
		    (* pval = undefined *)
		    pval
	       ) pwval
	   in
	   NodeHash.replace_post new_analysis node new_pwval;
	   new_analysis
	) analysis (NodeHash.create 0)
    in
    formatted_analysis,offset_map

  (* exception used to share the default treatment in [sub_transform] *)
  exception Rec_transform

  (* type used to propagate information in [sub_transform] and
     its sub-functions *)
  type inter_transform_t =
      {
        offset_nodes : NodeSet.t ref;
	offset_vars : VarSet.t ref;
	offset_map : ILVar.t ILVarMap.t;
	label_corresp : Node.t StringMap.t;
	block_begin : Node.t;
	block_end : Node.t;
	has_old : bool;
	has_at : String.t option
      }

  (* types used to pass information from [sub_transform] to 
     its sub-functions *)
  type addon_t = 
      {
	is_replacement : bool;
	addon_node : Node.t
      }
  type local_nodes_t =
      {
	node : Node.t;
	sub_nodes : Node.t list;
	new_sub_nodes : (Node.t * addon_t option) list
      }

  (* transformation on an individual node.
     takes as input a node [node] in the graph(s) previously created.
     returns a pair consisting of:
     - a main part, i.e. a potentially new node that is not connected 
     to the graph(s), that corresponds to the transformed node.
     - an addon part, i.e. a potentially new node that corresponds to
     the resulting pointer.

     On an assignment 
          [p = q], 
     the main part could be e.g. 
          [p_offset = q_offset] 
     and the addon part could be 
          [r + p_offset].
     The statement 
          [p = q;] 
     would be transformed into 
          [p_offset = q_offset;]
     and the statement 
          [return (p = q);] 
     into the more complex:
          [return (p_offset = q_offset,r + p_offset);]

     The results of the data-flow analysis [analysis] contain only 
     the relevant information for code transformation. Other intermediate
     results of the analysis have been erased after the analysis phase.
     Only the post-part of the analysis is relevant here.
  *)

  (* 4 sub-functions that share code between cases below *)

  (* every offset variable that is used in the code must be stored in 
     the repository [offset_vars], to be declared later on by a call to
     [introduce_new_vars]. *)
  let store_offset_var params offset_var =
    params.offset_vars := VarSet.add offset_var (!(params.offset_vars))

  (* returns an integer/pointer read that corresponds to the abstract value
     [aval], with other elements taken in [node] 
  *)
  let make_integer_pointer_read params node pval =
    if PtrLattice.is_alias pval then
      (* rewrite alias of v as v *)
      let new_var = PtrLattice.get_alias pval in
      change_in_intptr_var node new_var
    else if PtrLattice.is_index pval then
      (* rewrite constant offset i of v as v+i *)
      let new_var,index = PtrLattice.get_index pval in
      change_in_intptr_var_add_cst node new_var index
    else if PtrLattice.is_offset pval then
      (* rewrite offset o of v as v+o *)
      let new_var,offset_var = PtrLattice.get_offset pval in
      store_offset_var params offset_var;
      change_in_intptr_var_add_var node new_var offset_var
    else if PtrLattice.is_defined pval then
      (* rewrite possible assignment to v as v *)
      let new_var,_ = PtrLattice.get_defined_opt pval in
      change_in_intptr_var node new_var
    else node

  (* returns an integer/pointer read that corresponds to the base 
     integer/pointer for the abstract value [aval], with other elements
     taken in [node] 
  *)
  let make_base_integer_pointer_read node pval =
    if PtrLattice.is_alias pval then
      (* rewrite alias of v as v *)
      let new_var = PtrLattice.get_alias pval in
      change_in_intptr_var node new_var
    else if PtrLattice.is_index pval then
      (* rewrite constant offset i of v as v *)
      let new_var,_ = PtrLattice.get_index pval in
      change_in_intptr_var node new_var
    else if PtrLattice.is_offset pval then
      (* rewrite offset o of v as v *)
      let new_var,_ = PtrLattice.get_offset pval in
      change_in_intptr_var node new_var
    else if PtrLattice.is_defined pval then
      (* rewrite possible assignment to v as v *)
      let new_var,_ = PtrLattice.get_defined_opt pval in
      change_in_intptr_var node new_var
    else node

  (* returns an offset assignment from the abstract value [rhs_val]
     to the variable [offset_lhs_var], with the possible addition
     of an expression [add_offset_opt]. Other elements are taken
     in [node]. 
  *)
  let make_offset_assign params 
      node offset_lhs_var rhs_val add_offset_opt is_incr_decr =
    if PtrLattice.is_index rhs_val then
      begin
	(* it cannot be an increment/decrement *)
	assert (not is_incr_decr);
	(* [offset_lhs_var] is assigned a constant *)
	let (_,index) = PtrLattice.get_index rhs_val in
	match add_offset_opt with
	  | None -> 
	      change_in_int_var_assign_cst node offset_lhs_var index
	  | Some offset_expr ->
	      let add_expr = make_int_expr_add_cst offset_expr index
	      in
	      change_in_int_var_assign_expr node offset_lhs_var add_expr
      end
    else if PtrLattice.is_offset rhs_val then
      (* [offset_lhs_var] is assigned another offset *)
      let (_,offset_rhs_var) = 
	PtrLattice.get_offset rhs_val in
      store_offset_var params offset_rhs_var;
      match add_offset_opt with
	| None -> 
	    (* only possible case for an increment/decrement *)
	    if is_incr_decr then
	      change_in_int_incr_assign node offset_lhs_var
	    else
	      change_in_int_var_assign_var 
		node offset_lhs_var offset_rhs_var
	| Some offset_expr ->
	    begin
	      (* it cannot be an increment/decrement *)
	      assert (not is_incr_decr);
	      let add_expr = 
		make_int_expr_add_var offset_expr offset_rhs_var in
	      change_in_int_var_assign_expr node offset_lhs_var add_expr
	    end
    else
      begin
	(* it cannot be an increment/decrement *)
	assert (not is_incr_decr);
	match add_offset_opt with
	  | None -> (* [offset_lhs_var] is reset *)
	      change_in_int_var_assign_cst node offset_lhs_var 0
	  | Some offset_expr ->
	      change_in_int_var_assign_expr 
		node offset_lhs_var offset_expr
      end

  (* returns an integer/pointer assignment from [new_rhs_node] to [lhs_node],
     with [new_rhs_not_offset] telling if the new right-hand side
     is the translation for the original expression, or an offset expression.
     Other elements are taken in [node]. 
  *)
  let keep_integer_pointer_assignment params 
      node aval new_rhs_not_offset lhs_node new_rhs_node is_incr_decr =
    (* integer/pointer assignment must be kept.
       The only possible problem is that the right-hand side might be 
       an integer/pointer assignment too, that could have been transformed
       into an offset assignment. *)
    if new_rhs_not_offset then
      if is_incr_decr then
	(* nothing to change *)
	node
      else
	(* keep old lhs and new rhs *)
	change_sub_components node [lhs_node;new_rhs_node]
    else
      begin
	(* it cannot be an increment/decrement *)
	assert (not is_incr_decr);
	(* The rhs is now an offset assignment.
	   In all cases, the analysis must have computed 
	   an index or an offset for the right-hand side.
	   Create an equivalent sequence of assignments. *)
	let rhs_val =
	  match assign_get_rhs_var node with
	    | Some rhs_var,_ -> 
		PointWisePtrLattice.find rhs_var aval
	    | _ -> assert false
	in
	let ptr_node = make_integer_pointer_read params node rhs_val in
	let assign_node = 
	  change_sub_components node [lhs_node;ptr_node]
	in
	make_seq_expr new_rhs_node assign_node
      end

  (* part of the transformation that deals with expressions.
     Can raise exception Rec_transform for a common treatment with
     [sub_transform]. *)
  let sub_transform_on_expr analysis params local_nodes =
    let node = local_nodes.node in
    let sub_nodes = local_nodes.sub_nodes in
    let new_sub_nodes = local_nodes.new_sub_nodes in

    if debug_more then Coptions.lprintf
      "[sub_transform_on_expr] node %a@." Node.pretty node;

    (* transformation is possible only if analysis provides some information. 
       Otherwise raise Not_found. *)
    let aval = match NodeHash.find_post analysis node with
      | None -> raise Rec_transform
      | Some v -> v
    in

    (* beginning of transformation for expressions *)
    
    (* reading some integer/pointer variable.
       This is called also on integer/pointer write, but the result
       of this rewrite is ignored by the calling [sub_transform]
       on integer/pointer assignment. *)
    if termexpr_is_local_var node then
      let var = termexpr_var_get node in
      if var_is_pointer var || var_is_integer var then
	let pval = PointWisePtrLattice.find var aval in 
	(make_integer_pointer_read params node pval,
	 None) (* nothing to add to make it a pointer *)
      else
	raise Rec_transform

    (* writing some integer/pointer variable *)
    else if (expr_is_ptr_assign node || expr_is_int_assign node)
	     && (match assign_get_local_lhs_var node with
	           | None -> false | Some _ -> true)
    then
      (* 2 possibilities: it may be a real assignment or 
	 an increment/decrement operation *)
      let is_incr_decr = List.length sub_nodes = 1 in
      assert (List.length sub_nodes = List.length new_sub_nodes);
      let lhs_node,new_rhs_node =
	if is_incr_decr then
	  (* increment/decrement *)
	  list1 sub_nodes,list1 new_sub_nodes
	else
	  (* assignment *)
	  fst (list2 sub_nodes),snd (list2 new_sub_nodes)
      in
      (* separate main part from addon part *)
      let new_rhs_node,new_rhs_node_addon = new_rhs_node in
      let new_rhs_is_assign = expr_is_assign new_rhs_node in
      let new_rhs_not_offset =
	not (NodeSet.mem new_rhs_node !(params.offset_nodes))
      in
      match assign_get_local_lhs_var node with
	| None -> 
	    (keep_integer_pointer_assignment params node aval
	       new_rhs_not_offset lhs_node new_rhs_node is_incr_decr,
	     None) (* nothing to add to make it an integer/pointer *)
	      
	| Some lhs_var ->
	    let lhs_val = PointWisePtrLattice.find lhs_var aval in 

	    (* share addon part used in offset/defined cases *)
	    let wrap_addon offset_node = 
	      let addon_part = 
		if is_incr_decr then
		  (* [new_rhs_node] must be a variable here, 
		     in the offset/defined cases *)
		  (* the original treatment here was 
		         { is_replacement = false;
		           addon_node = 
		             change_in_intptr_incr node new_rhs_node true }
		     but this led to some problems with Simplify trying
		     to prove the subsequent goal.
		     Therefore it was change to the following, which gives
		     Simplify an easier goal.
		     e.g. the C code
		         *p++ = 0;
		     was changed to
		         *(p_offset++, q+p_offset-1) = 0;
		     and is now translated into
		         *(q+(p_offset++)) = 0;
		  *)
		  let base_node = make_base_integer_pointer_read node lhs_val 
		  in
		  { is_replacement = true;
		    addon_node = 
		    make_intptr_expr_add_expr base_node offset_node }
		else
		  { is_replacement = false;
		    addon_node = 
		    make_integer_pointer_read params node lhs_val }
	      in
	      (* store the information that this node is an offset node *)
	      params.offset_nodes := 
		NodeSet.add offset_node !(params.offset_nodes);
	      offset_node,Some addon_part in

	    if PtrLattice.is_index lhs_val then
	      (* assignment to [lhs_var] not useful, since reading
		 [lhs_var] will be replaced by reading its alias
		 or its constant offset from some variable.
		 Contains the test [PtrLattice.is_alias lhs_val]. *)
	      if is_incr_decr then
		(change_in_intptr_incr node new_rhs_node false,
		 None) (* nothing to add to make it a pointer *)
	      else
		(* just propagate the right-hand side *)
		(new_rhs_node,new_rhs_node_addon)

	    else if PtrLattice.is_offset lhs_val then
	      let (_new_lhs_var,offset_lhs_var) =
		PtrLattice.get_offset lhs_val in
	      store_offset_var params offset_lhs_var;
	      (* right-hand side can only be another variable or
		 another integer/pointer assignment.
		 In either case, the analysis must have computed 
		 an index or an offset for the right-hand side. *)
	      let rhs_val =
		match assign_get_rhs_var node with
		  | Some rhs_var,_ -> 
		      PointWisePtrLattice.find rhs_var aval
		  | _ -> assert false
	      in
	      (* The integer/pointer assignment must be changed into
		 an offset assignment. 4 cases are possible depending
		 on the new right-hand side computed :
		 - the rhs is still an integer/pointer assignment, e.g. in
		 p = q = malloc(...);
		 Create an equivalent sequence of assignments, e.g.:
		 q = malloc(...), p_offset = 0;
		 - the rhs is itself an offset assignment. Use it.
		 - the rhs is the sum of a integer/pointer variable 
		 and an integer expression. Ignore the integer/pointer 
		 and keep the expression.
		 - the rhs is another integer/pointer expression. Ignore it. *)
	      if new_rhs_is_assign then
		begin
		  (* it cannot be an increment/decrement *)
		  assert (not is_incr_decr);
		  if new_rhs_not_offset then	
		    (* The rhs is still an integer/pointer assignment.
		       Create an equivalent sequence of assignments. *)
		    let offset_node = 
		      make_offset_assign params 
			node offset_lhs_var rhs_val None false
		    in
		    wrap_addon (make_seq_expr new_rhs_node offset_node)
		  else
		    (* The rhs is itself an offset assignment. Use it. *)
		    wrap_addon (change_in_int_var_assign_expr 
				  node offset_lhs_var new_rhs_node)
		end
	      else
		(* The rhs can be:
		   - the sum of an integer/pointer variable 
		   and an integer expression
		   - or any other integer/pointer expression.
		   The former case can be the original source code or 
		   due to the transformation of an offset pointer 
		   or an index pointer.
		*)
		if is_incr_decr then
		  (* do not compute [off_opt] here, it would be equal
		     to the offset variable, because the rhs has been
		     translated to the sum var + offset_var *)
		  wrap_addon (make_offset_assign params 
				node offset_lhs_var rhs_val None true)
		else
		  let var_opt,off_opt = 
		    get_intptr_add_on_var_opt new_rhs_node in
		  let off_opt = match off_opt with
		    | None -> None
		    | Some off_node ->
			if expr_is_local_var off_node then
			  (* rule out transformation of offset *)
			  let off_var = termexpr_var_get off_node in
			  if VarSet.mem off_var (!(params.offset_vars)) then
			    (* here, offset is only the transformation
			       on the sub-node. Same as the increment/
			       decrement case. Ignore it. *)
			    None
			  else off_opt
			else 
			  match var_opt with
			    | Some var ->
				(* rule out transformation of index *)
				let var_val = PointWisePtrLattice.find 
				  var aval in 
				if PtrLattice.is_index var_val
				  && not (PtrLattice.is_alias var_val)
				then None
				else off_opt
			    | None -> off_opt
		  in    
		  wrap_addon 
		    (make_offset_assign params 
		       node offset_lhs_var rhs_val off_opt false)

	    else if PtrLattice.is_defined lhs_val then
	      let assign_node = 
		keep_integer_pointer_assignment params node aval 
		  new_rhs_not_offset lhs_node new_rhs_node is_incr_decr
	      in
	      match PtrLattice.get_defined_opt lhs_val with
		| _,Some offset_var ->
		    store_offset_var params offset_var;
		    let reset_node = 
		      change_in_int_var_assign_cst node offset_var 0 in
		    (* sequence in this order due to possible effects
		       in [assign_node]. Use [wrap_addon] to return 
		       pointer. *) 
		    wrap_addon (make_seq_expr assign_node reset_node)
		| _,None -> (* like in the default assignment case *)
		    (assign_node,
		     None) (* nothing to add to make it a pointer *)

	    else (* default: not any of index/offset/defined pointer *)
	      (keep_integer_pointer_assignment params node aval 
		 new_rhs_not_offset lhs_node new_rhs_node is_incr_decr,
	       None) (* nothing to add to make it a pointer *)

    else raise Rec_transform

  let sub_transform_on_term analysis params local_nodes =
    let node = local_nodes.node in

    if termexpr_is_local_var node then
      let var = termexpr_var_get node in
      if var_is_pointer var || var_is_integer var then
	let pval = 
	  if params.has_old then
	    begin
	      assert (params.has_at = None);
	      if debug then Coptions.lprintf
		  "[sub_transform_on_term] has old@.";
	      let begin_val = 
		match NodeHash.find_post analysis params.block_begin with
		  | None -> PointWisePtrLattice.bottom ()
		  | Some v -> v
	      in
	      PointWisePtrLattice.find var begin_val
	    end
	  else match params.has_at with
	  | Some lab ->
	      begin
		assert (not params.has_old);
		if debug then Coptions.lprintf
		    "[sub_transform_on_term] has at@.";
		let at_node = StringMap.find lab params.label_corresp in
		let at_val = match NodeHash.find_post analysis at_node with
		  | None -> PointWisePtrLattice.bottom ()
		  | Some v -> v
		in
		PointWisePtrLattice.find var at_val
	      end
	  | None ->
	      let end_val = 
		match NodeHash.find_post analysis params.block_end with
		  | None -> PointWisePtrLattice.bottom ()
		  | Some v -> v
	      in
	      PointWisePtrLattice.find var end_val
	in
	let new_node = make_integer_pointer_read params node pval in
	if debug then Coptions.lprintf
	    "[sub_transform_on_term] term %a@.into %a@."
	    Node.pretty node Node.pretty new_node;
	(new_node,
	 None) (* addon part useless here *)
      else raise Rec_transform
    else raise Rec_transform

  let rec sub_transform analysis params node =

    let params = match get_node_kind node with
      | NKstat ->
	  if stat_is_assert node then
	    let beg_node = logic_begin node in
	    let end_node = logic_end node in
	    { params with block_begin = beg_node; block_end = end_node }
	  else if stat_is_label node then (* both kinds of labels ? *)
	    let lab = stat_get_label node in
	    { params with label_corresp = 
		StringMap.add lab node params.label_corresp }
	  else params 
      | NKpred | NKassume | NKassert | NKterm ->
	  if termpred_is_old node then
	    { params with has_old = true }
	  else if termpred_is_at node then
	    let lab = termpred_get_label node in
	    { params with has_at = Some lab }
	  else params
      | _ -> params
    in

    (* apply [sub_transform] recursively on sub-nodes *)
    let sub_nodes = (code_children node) @ (logic_children node) in
    let new_sub_nodes = match get_node_kind node with
      | NKannot ->
	  let beg_function = logic_begin node and beg_loop = logic_end node in
	  let inv_params = 
	    { params with block_begin = beg_function; block_end = beg_loop } in
	  let assinv_params = inv_params in
	  let ass_params = inv_params in
	  let var_params = inv_params in
	  let params = [inv_params; assinv_params; ass_params; var_params] in
	  List.map2 (sub_transform analysis) params sub_nodes
      | NKspec ->
	  let beg_block = logic_begin node and end_block = logic_end node in
	  let req_params = 
	    { params with block_begin = beg_block; block_end = beg_block } in
	  let ass_params = req_params in
	  let ens_params = 
	    { params with block_begin = beg_block; block_end = end_block } in
	  let dec_params = req_params in
	  let params = [req_params; ass_params; ens_params; dec_params] in
	  List.map2 (sub_transform analysis) params sub_nodes	  
      | _ ->
	  List.map (sub_transform analysis params) sub_nodes
    in
    let local_nodes =
      {
	node = node;
	sub_nodes = sub_nodes;
	new_sub_nodes = new_sub_nodes
      } 
    in

    (* treat declaration/statement/expression separately *)
    try match get_node_kind node with
      | NKnone | NKstat | NKpred 
      | NKassume | NKassert | NKannot | NKspec -> 
	  raise Rec_transform

      | NKdecl -> 
	  (* no type change needed here, keep only main part *)
	  let new_sub_nodes = List.map fst new_sub_nodes in
	  let new_node = change_sub_components node new_sub_nodes in
	  let param_list = decl_get_params node in
	  let param_offset_vars =
	    List.fold_left 
	      (fun set param ->
		 try 
		   let offset_var = ILVarMap.find param params.offset_map in
		   if VarSet.mem offset_var (!(params.offset_vars)) then
		     VarSet.add offset_var set
		   else set
		 with Not_found -> set
	      ) VarSet.empty param_list
	  in
	  (* only introduce offset variables for parameters here.
	     Other offset variables are already declared locally, or will
	     be declared in [cleanup]. *)
	  let new_node = introduce_new_vars new_node 
	    (VarSet.elements param_offset_vars) (* zero_init= *)true
	  in
	  (new_node,None) (* addon part has no meaning here *)

      | NKexpr | NKtest | NKlvalue ->
	  sub_transform_on_expr analysis params local_nodes

      | NKterm ->
	  sub_transform_on_term analysis params local_nodes
	  

    with Rec_transform -> 
      (* return same expression on new sub-nodes.
	 Only subtlety is to use the addon part of an expression sub-node when
	 the new sub-node is an offset node (which the original sub-node
	 obviously was not). *)
      let new_sub_nodes =
	List.map2 (fun sub_node (new_main,new_addon) ->
		     begin match get_node_kind sub_node with
		       | NKexpr | NKtest | NKlvalue ->
			   if NodeSet.mem new_main !(params.offset_nodes) then
			       match new_addon with
				 | Some new_addon ->
				     let addon_node = new_addon.addon_node in
				     if new_addon.is_replacement then
				       addon_node
				     else
				       make_seq_expr new_main addon_node
				 | None -> assert false
			   else
			     new_main
		       | _ -> new_main
		     end
		  ) sub_nodes new_sub_nodes
      in
      (change_sub_components node new_sub_nodes,
       None) (* addon part empty here *)

  let transform analysis offset_map decls =
    List.map (fun decl ->
		let params = 
		  { 
		    offset_nodes = ref NodeSet.empty;
		    offset_vars = ref VarSet.empty;
		    offset_map = offset_map;
		    label_corresp = StringMap.empty;
		    block_begin = decl; (* dummy value *)
		    block_end = decl; (* dummy value *)
		    has_old = false;
		    has_at = None
		  } 
		in
		let new_decl,_ = 
		  sub_transform analysis params decl in
		new_decl
	     ) decls

  let cleanup used_vars decl_vars offset_map decls =
    
    let rec sub_cleanup node = 

      let sub_nodes = (code_children node) @ (logic_children node) in
      let new_sub_nodes = List.map sub_cleanup sub_nodes in
      let node = change_sub_components node new_sub_nodes in

      match get_node_kind node with
	| NKstat ->
	    if stat_is_decl node then
	      let var = decl_stat_get_var node in
	      if debug_more then Coptions.lprintf
		"[sub_cleanup] declaration of variable %s used ? %B@."
		var.var_name (ILVarSet.mem var used_vars);
	      let node =
		if ILVarSet.mem var used_vars then 
		  node 
		else 
		  decl_stat_get_next node 
	      in
	      try
		let offset_var = ILVarMap.find var offset_map in
		if ILVarSet.mem offset_var decl_vars then
		  node
		else
		  make_var_decl node offset_var
	      with Not_found -> node
	    else node
	| _ -> node
    in
    List.map sub_cleanup decls
end

module LocalPtrAnalysis = Make_DataFlowAnalysis(Var)(PtrLangFromNormalized)
    (Make_LatticeFromSemiLattice(PointWisePtrLattice))(ConnectCFGtoPtr)


(*****************************************************************************
 *                                                                           *
 * 		External interface for local pointer analysis		     *
 *                                                                           *
 *****************************************************************************)

let local_aliasing fundecl =

  if debug_more then Coptions.lprintf 
    "[local_aliasing] treating function %s@." fundecl.f.fun_name; 

  (* build control-flow graph *)
  let decls = PtrLangFromNormalized.from_file [fundecl] in
  let decls = List.map fst decls in
  (* perform local pointer analysis *)
  let raw_analysis = LocalPtrAnalysis.compute decls in
  (* format results of the analysis *)
  let analysis,offset_map = ConnectCFGtoPtr.format raw_analysis in
  (* transform the program using the analysis *)
  let decls = ConnectCFGtoPtr.transform analysis offset_map decls in
  (* return the new program *)
  let file = PtrLangFromNormalized.to_file decls in

  (* rebuild control-flow graph *)
  let decls = PtrLangFromNormalized.from_file file in
  let decls = List.map fst decls in
  (* collect the local variables used/declared *)
  let used_vars,decl_vars = PtrLangFromNormalized.collect_vars () in
  if debug_more then Coptions.lprintf
    "[local_aliasing_transform] %i local variables used@." 
    (ILVarSet.cardinal used_vars);
  if debug_more && not (ILVarSet.is_empty used_vars) then 
    Coptions.lprintf "[local_aliasing_transform] %a@." 
      (fun _ -> (ILVarSet.iter (Coptions.lprintf "%a " ILVar.pretty))) 
      used_vars;
  (* add the necessary declarations *)
  let decls = ConnectCFGtoPtr.cleanup used_vars decl_vars offset_map decls in
  (* return the new program *)
  PtrLangFromNormalized.to_file decls

let local_aliasing_transform () =
  (* necessary prefix to translate the hash-table of functions in 
     the normalized code into a list of function representatives,
     as defined by type [func_t] in [Cabsint] *)
  let file = Hashtbl.fold 
    (fun name (spec,typ,f,s,loc) funcs ->
       { name = name; spec = spec; typ = typ; f = f; s = s; loc = loc } 
       :: funcs
    ) Cenv.c_functions []
  in

  if debug_more then Coptions.lprintf 
    "[local_aliasing_transform] %i functions to treat@." (List.length file); 

  let file = List.fold_right
    (fun fundecl acc -> (local_aliasing fundecl) @ acc) file [] in

  if debug_more then Coptions.lprintf 
    "[local_aliasing_transform] %i functions treated@." (List.length file);

  (* necessary suffix to translate the list of function representatives
     to the hash-table format *)
  List.iter (fun { name = name; spec = spec; typ = typ; 
		   f = f; s = s; loc = loc } ->
	       Cenv.add_c_fun name (spec,typ,f,s,loc)) file

(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)

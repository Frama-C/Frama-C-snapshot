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

(*i $Id: output.mli,v 1.27 2008/11/05 14:03:16 filliatr Exp $ i*)

type constant =
  | Prim_void
  | Prim_int of string
  | Prim_real of string
  | Prim_bool of bool
(*
  | Prim_string of string
*)

type logic_type = 
    { logic_type_name : string;
      logic_type_args : logic_type list;
    }

val fprintf_logic_type : Format.formatter -> logic_type -> unit


type term = 
  | LConst of constant
  | LApp of string * term list
  | LVar of string
  | LVarAtLabel of string * string     (*r x@L *)
  | Tnamed of string * term
  | TIf of term * term * term

val fprintf_term : Format.formatter -> term -> unit

type assertion = 
  | LTrue | LFalse
  | LAnd of assertion * assertion
  | LOr of assertion * assertion
  | LIff of assertion * assertion
  | LNot of assertion
  | LImpl of assertion * assertion
  | LIf of term * assertion * assertion
  | LLet of string * term * assertion
  | LForall of string * logic_type * assertion
      (*r forall x:t.a *)
  | LExists of string * logic_type * assertion
      (*r exists x:t.a *)
  | LPred of string * term list
  | LNamed of string * assertion
;;

val make_or : assertion -> assertion -> assertion
val make_and : assertion -> assertion -> assertion
val make_or_list : assertion list -> assertion
val make_and_list : assertion list -> assertion
val make_impl : assertion -> assertion -> assertion
val make_equiv : assertion -> assertion -> assertion

val fprintf_assertion : Format.formatter -> assertion -> unit

type why_type = 
  | Prod_type of string * why_type * why_type (*r (x:t1)->t2 *)
  | Base_type of logic_type
  | Ref_type of why_type
  | Annot_type of 
      assertion * why_type * 
      string list * string list * assertion * ((string * assertion) list)
	(*r { P } t reads r writes w raises E { Q | E => R } *)
;;

val int_type : why_type
val bool_type : why_type
val unit_type : why_type
val base_type : string -> why_type

type variant = term * string option

type opaque = bool

type expr =
  | Cte of constant
  | Var of string
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Void
  | Deref of string
  | If of expr * expr * expr
  | While of 
      expr (* loop condition *)
      * assertion (* invariant *) 
      * variant option (* variant *) 
      * expr list (* loop body *)
  | Block of expr list
  | Assign of string * expr
  | Let of string * expr * expr
  | Let_ref of string * expr * expr
  | App of expr * expr
  | Raise of string * expr option
  | Try of expr * string * string option * expr
  | Fun of (string * why_type) list * 
      assertion * expr * assertion * ((string * assertion) list)
  | Triple of opaque * 
      assertion * expr * assertion * ((string * assertion) list)
  | Assert of assertion * expr
  | Label of string * expr
  | BlackBox of why_type
  | Absurd 
  | Loc of Lexing.position * expr
;;

val fprintf_expr : Format.formatter -> expr -> unit

val make_or_expr : expr -> expr -> expr
val make_and_expr : expr -> expr -> expr


(*

  [make_app id [e1;..;en])] builds
  App(...(App(Var(id),e1),...,en)

*)

val make_app : string -> expr list -> expr
val make_logic_app : string -> expr list -> expr
val make_app_e : expr -> expr list -> expr

(*

  [make_label l e] builds

    begin label l; e end

  applying simplification if [e] is already a block

    

*)

val make_label : string -> expr -> expr;;

(*

  [make_while cond inv dec e] builds

  while cond do { invariant inv variant dec } e

  applying simplifications if possible

*)
val make_while : expr -> assertion -> variant option -> expr -> expr;;

val make_pre : assertion -> expr -> expr;;

val append : expr -> expr -> expr

type why_decl =
  | Param of bool * string * why_type         (*r parameter in why *)
  | Def of string * expr               (*r global let in why *)
  | Logic of bool * string * (string * logic_type) list * logic_type    (*r logic decl in why *)
  | Predicate of bool * string * (string * logic_type) list * assertion  
  | Inductive of bool * string * (string * logic_type) list *  
      (string * assertion) list (*r inductive definition *)
  | Axiom of string * assertion         (*r Axiom *)
  | Goal of string * assertion         (*r Goal *)
  | Function of bool * string * (string * logic_type) list * logic_type * term
  | Type of string * string list
  | Exception of string * logic_type option

val fprintf_why_decl : Format.formatter -> why_decl -> unit;;

val fprintf_why_decls : Format.formatter -> why_decl list -> unit

type kind =
  | ArithOverflow
  | DownCast
  | IndexBounds
  | PointerDeref
  | UserCall
  | DivByZero
  | AllocSize
  | Pack
  | Unpack

val pos_table : 
    (string, (kind option * string option * string option * Loc.position)) 
    Hashtbl.t 

val reg_pos : string -> ?id:string -> ?kind:kind -> ?name:string
  -> ?formula:string -> Loc.position -> string

val print_pos : Format.formatter -> unit


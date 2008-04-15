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

(* $Id: jc_ast.mli,v 1.163 2008/12/09 09:14:18 marche Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_region

class type positioned =
object
  method pos: Loc.position
end

class type typed =
object
  method typ: jc_type
end

class type labeled =
object
  method label: label option
  method set_label: label option -> unit
end

class type marked =
object
  method mark: string
end

class type regioned =
object
  method region: region
  method set_region: region -> unit
end

type const =
  | JCCvoid
  | JCCnull
  | JCCboolean of bool
  | JCCinteger of string
  | JCCreal of string 
  | JCCstring of string

class type identifier = 
object
  inherit positioned
  method name: string
end

class type ['a] node_positioned = 
object
  inherit positioned
  method node: 'a
end

(***************)
(* parse trees *)
(***************)

type ptype_node = 
  | JCPTnative of native_type
  | JCPTidentifier of string
  | JCPTpointer of string * ptype list * Num.num option * Num.num option

and ptype = ptype_node node_positioned

type comparison_op = [ `Blt | `Bgt | `Ble | `Bge | `Beq | `Bneq ]
type arithmetic_op = [ `Badd | `Bsub | `Bmul | `Bdiv | `Bmod ]
type logical_op = [ `Bland | `Blor | `Bimplies | `Biff ]
type bitwise_op = 
    [ `Bbw_and | `Bbw_or | `Bbw_xor 
    | `Bshift_left | `Blogical_shift_right | `Barith_shift_right ]

type basic_op = [ comparison_op | arithmetic_op ]
type operational_op = [ basic_op | bitwise_op | `Bconcat | `Bland | `Blor ]
type bin_op = [ operational_op | logical_op ]
      
type pre_unary_op = [ `Uprefix_inc | `Uprefix_dec ]
type post_unary_op = [ `Upostfix_inc | `Upostfix_dec ]
type pm_unary_op = [ pre_unary_op | post_unary_op | `Uplus ]
type unary_op = [ `Uminus | `Unot | `Ubw_not ]

type pexpr_unary_op = [ pm_unary_op | unary_op ]

type float_operator_type = [`Double | `Float ]
type native_operator_type = [ `Unit | `Boolean | `Integer | `Real | float_operator_type ]
type operator_type = [ native_operator_type | `Pointer | `Logic ]

type pred_bin_op = [comparison_op | logical_op] * operator_type
type expr_unary_op = unary_op * native_operator_type
type term_unary_op = expr_unary_op
type expr_bin_op = operational_op * operator_type
type term_bin_op = bin_op * operator_type
type pred_rel_op = comparison_op * operator_type

type offset_kind = Offset_max | Offset_min

type address_kind = Addr_absolute | Addr_pointer

type quantifier = Forall | Exists

type asrt_kind = 
  | Aassert (* Assertion to prove *)
  | Ahint   (* Assertion to help in proofs, 
	       can be either discarded or both used and proved *)
  | Aassume (* Assertion that can be relied on without proof *)

type rounding_mode =
  | Round_nearest_even | Round_to_zero | Round_up | Round_down 
  | Round_nearest_away

type float_format = 
  | FormatFloat | FormatDouble

type real_conversion = 
  | Integer_to_real | Real_to_integer 
  | Double_to_real
  | Float_to_real
  | Round_double of rounding_mode
  | Round_float of rounding_mode

type ppattern_node =
  | JCPPstruct of identifier * (identifier * ppattern) list
  | JCPPvar of identifier
  | JCPPor of ppattern * ppattern
  | JCPPas of ppattern * identifier
  | JCPPany
  | JCPPconst of const

and ppattern = ppattern_node node_positioned

type 'expr pbehavior = 
    Loc.position * string * identifier option * 'expr option 
    * 'expr option * (Loc.position * 'expr list) option * 'expr
      (*r loc, name, throws, assumes,requires,assigns,ensures *)
 
and pexpr_node =
  | JCPEconst of const
  | JCPElabel of string * pexpr
  | JCPEvar of string
  | JCPEderef of pexpr * string
  | JCPEbinary of pexpr * bin_op * pexpr
  | JCPEunary of pexpr_unary_op * pexpr
  | JCPEapp of string * label list * pexpr list
  | JCPEassign of pexpr * pexpr
  | JCPEassign_op of pexpr * bin_op * pexpr
  | JCPEinstanceof of pexpr * string
  | JCPEcast of pexpr * ptype
  | JCPEquantifier of quantifier * ptype * string list * pexpr
  | JCPEold of pexpr
  | JCPEat of pexpr * label
  | JCPEoffset of offset_kind * pexpr 
  | JCPEaddress of address_kind * pexpr 
      (* expression is of integer type for an absolute address, and of
	 pointer type for a pointer address *)
  | JCPEbase_block of pexpr 
  | JCPEif of pexpr * pexpr * pexpr
  | JCPElet of ptype option * string * pexpr option * pexpr
  | JCPEdecl of ptype * string * pexpr option
  | JCPErange of pexpr option * pexpr option
  | JCPEalloc of pexpr * string
  | JCPEfree of pexpr
  | JCPEmutable of pexpr * pexpr ptag
  | JCPEeqtype of pexpr ptag * pexpr ptag
  | JCPEsubtype of pexpr ptag * pexpr ptag
  | JCPEmatch of pexpr * (ppattern * pexpr) list
(*  | JCPSskip *) (* -> JCPEconst JCCvoid *)
  | JCPEblock of pexpr list
  | JCPEassert of string list * asrt_kind * pexpr
  | JCPEcontract of 
      pexpr option * pexpr option * pexpr pbehavior list * pexpr 
	(* requires, decreases, behaviors, expression *)
  | JCPEwhile of 
      pexpr * (string list * pexpr) list * pexpr option * pexpr
	(*r condition, invariant, variant, body *)
  | JCPEfor of 
      pexpr list * pexpr * pexpr list * (string list * pexpr) list * pexpr option * pexpr
	(*r inits, condition, updates, invariant, variant, body *)
  | JCPEreturn of pexpr
  | JCPEbreak of string
  | JCPEcontinue of string
  | JCPEgoto of string
  | JCPEtry of pexpr * (identifier * string * pexpr) list * pexpr
  | JCPEthrow of identifier * pexpr
  | JCPEpack of pexpr * identifier option
  | JCPEunpack of pexpr * identifier option
  | JCPEswitch of pexpr * (pexpr option list * pexpr) list

and pexpr = pexpr_node node_positioned

and 'a ptag_node =
  | JCPTtag of identifier
  | JCPTbottom
  | JCPTtypeof of 'a

and 'a ptag = 'a ptag_node node_positioned

type 'expr clause =
  | JCCrequires of 'expr
  | JCCbehavior of 'expr pbehavior

type 'expr reads_or_expr =
  | JCreads of 'expr list
  | JCexpr of 'expr
  | JCinductive of (identifier * label list * 'expr) list

type 'expr decl_node =
  | JCDvar of ptype * string * 'expr option
  | JCDfun of ptype * identifier * (ptype * string) list * 'expr clause list
      * 'expr option
  | JCDtag of
      string (* name of the tag *)
      * string list (* type parameters *)
      * (string * ptype list) option (* parent tag, applied type parameters *)
      * (bool * ptype * string * int option) list (* fields *)
      * (identifier * string * 'expr) list (* invariants *)
  | JCDvariant_type of string * identifier list
  | JCDunion_type of string * bool * identifier list
      (* name, discriminated, structure names *)
  | JCDenum_type of string * Num.num * Num.num
  | JCDlogic_type of string 
  | JCDlemma of string * bool * label list * 'expr
      (* 2nd arg is true if it is an axiom *)
  | JCDexception of string * ptype option
  (* logic functions and predicates (return type: None if predicate) *)
  | JCDlogic of ptype option * string * label list * (ptype * string) list 
      * 'expr reads_or_expr
  | JCDlogic_var of ptype * string * 'expr option
  (* global invariant *)
  | JCDglobal_inv of string * 'expr
  (* "pragma" options and policies *)
  | JCDinvariant_policy of Jc_env.inv_sem
  | JCDseparation_policy of Jc_env.separation_sem
  | JCDannotation_policy of Jc_env.annotation_sem
  | JCDabstract_domain of Jc_env.abstract_domain 
  | JCDint_model of Jc_env.int_model
  | JCDaxiomatic of string * 'expr decl list

and 'expr decl = 'expr decl_node node_positioned

type pdecl = pexpr decl

class type ['expr_node] c_nexpr =
object
  inherit labeled
  inherit ['expr_node] node_positioned
end

(** Normalized expressions. Not typed yet, but without gotos. *)
type nexpr_node =
  | JCNEconst of const
  | JCNElabel of string * nexpr
  | JCNEvar of string
  | JCNEderef of nexpr * string
  | JCNEbinary of nexpr * bin_op * nexpr
  | JCNEunary of unary_op * nexpr
  | JCNEapp of string * label list * nexpr list
  | JCNEassign of nexpr * nexpr
  | JCNEinstanceof of nexpr * string
  | JCNEcast of nexpr * ptype
  | JCNEif of nexpr * nexpr * nexpr
  | JCNEoffset of offset_kind * nexpr 
  | JCNEaddress of address_kind * nexpr 
  | JCNEbase_block of nexpr 
  | JCNEalloc of nexpr * string
  | JCNEfree of nexpr
  | JCNElet of ptype option * string * nexpr option * nexpr
  | JCNEassert of string list * asrt_kind * nexpr
  | JCNEcontract of 
      nexpr option * nexpr option * nexpr pbehavior list * nexpr 
	(* requires, decreases, behaviors, expression *)
  | JCNEblock of nexpr list
  | JCNEloop of (string list * nexpr) list * nexpr option * nexpr
      (*r invariant, variant, body *)
  | JCNEreturn of nexpr option
  | JCNEtry of nexpr * (identifier * string * nexpr) list * nexpr
  | JCNEthrow of identifier * nexpr option
  | JCNEpack of nexpr * identifier option
  | JCNEunpack of nexpr * identifier option
  | JCNEmatch of nexpr * (ppattern * nexpr) list
  (* Assertions only *)
  | JCNEquantifier of quantifier * ptype * string list * nexpr
  | JCNEold of nexpr
  | JCNEat of nexpr * label
  | JCNEmutable of nexpr * nexpr ptag
  | JCNEeqtype of nexpr ptag * nexpr ptag
  | JCNEsubtype of nexpr ptag * nexpr ptag
  (* Locations only *)
  | JCNErange of nexpr option * nexpr option

and nexpr = nexpr_node c_nexpr

     
(*************)
(* typed ast *)
(*************)

class type ['pattern_node] c_pattern =
object
  inherit typed
  inherit ['pattern_node] node_positioned
end

type pattern_node =
  | JCPstruct of struct_info * (field_info * pattern) list
  | JCPvar of var_info
  | JCPor of pattern * pattern
  | JCPas of pattern * var_info
  | JCPany
  | JCPconst of const

and pattern = pattern_node c_pattern

class type ['node] c_term =
object
  inherit typed
  inherit regioned
  inherit marked
  inherit labeled
  inherit ['node] node_positioned
end

type 'li app = 
    {
      jc_app_fun : 'li;
      jc_app_args : 'li term list;
      mutable jc_app_region_assoc : (region * region) list;
      jc_app_label_assoc : (label * label) list;
    }

and 'li term_node =
  | JCTconst of const
  | JCTvar of var_info
  | JCTshift of 'li term * 'li term
  | JCTderef of 'li term * label * field_info
  | JCTbinary of 'li term * term_bin_op * 'li term
  | JCTunary of term_unary_op * 'li term
  | JCTapp of 'li app
  | JCTold of 'li term
  | JCTat of 'li term * label
  | JCToffset of offset_kind * 'li term * struct_info 
  | JCTaddress of address_kind * 'li term
  | JCTbase_block of 'li term
  | JCTinstanceof of 'li term * label * struct_info
  | JCTcast of 'li term * label * struct_info
  | JCTbitwise_cast of 'li term * label * struct_info
  | JCTrange_cast of 'li term * enum_info
  | JCTreal_cast of 'li term * real_conversion
  | JCTif of 'li term * 'li term * 'li term
  | JCTrange of 'li term option * 'li term option
  | JCTmatch of 'li term * (pattern * 'li term) list

and 'li term = 'li term_node c_term

type 'li tag = 'li tag_node node_positioned

and 'li tag_node =
  | JCTtag of struct_info
  | JCTbottom
  | JCTtypeof of 'li term * struct_info

class type ['node] c_location =
object
  inherit regioned
  inherit labeled
  inherit ['node] node_positioned
end

type 'li location_set_node = 
  | JCLSvar of var_info
  | JCLSderef of 'li location_set * label * field_info * region
(* TODO ?
  | JCLSshift of location_set * term 
*)
  | JCLSrange of 'li location_set * 'li term option * 'li term option
  | JCLSrange_term of 'li term * 'li term option * 'li term option

and 'li location_node =
  | JCLvar of var_info
  | JCLderef of 'li location_set * label * field_info * region
  | JCLderef_term of 'li term * field_info
  | JCLat of 'li location * label

and 'li location_set = 'li location_set_node c_location

and 'li location = 'li location_node c_location

class type ['assertion_node] c_assertion =
object
  inherit marked
  inherit labeled
  inherit ['assertion_node] node_positioned
end

type 'li assertion_node =
  | JCAtrue
  | JCAfalse
  | JCArelation of 'li term * pred_rel_op * 'li term
  | JCAand of 'li assertion list
  | JCAor of 'li assertion list
  | JCAimplies of 'li assertion * 'li assertion
  | JCAiff of 'li assertion * 'li assertion
  | JCAnot of 'li assertion
  | JCAapp of 'li app
  | JCAquantifier of quantifier * var_info * 'li assertion
  | JCAold of 'li assertion
  | JCAat of 'li assertion * label
  | JCAinstanceof of 'li term * label * struct_info
  | JCAbool_term of 'li term
  | JCAif of 'li term * 'li assertion * 'li assertion
  | JCAmutable of 'li term * struct_info * 'li tag
  | JCAeqtype of 'li tag * 'li tag * struct_info option
  | JCAsubtype of 'li tag * 'li tag * struct_info option
  | JCAmatch of 'li term * (pattern * 'li assertion) list

and 'li assertion = 'li assertion_node c_assertion

type 'li term_or_assertion =
  | JCAssertion of 'li assertion
  | JCTerm of 'li term
  | JCReads of 'li location list
  | JCInductive of (identifier * label list * 'li assertion) list

type 'li loop_annot =
    {
      jc_loop_tag : int;
      mutable jc_loop_invariant : (string list * 'li assertion) list;
      mutable jc_free_loop_invariant : 'li assertion;
      jc_loop_variant : 'li term option;
    }


type 'li behavior =
    { 
      jc_behavior_throws : exception_info option ;
      jc_behavior_assumes : 'li assertion option ;
      jc_behavior_assigns : (Loc.position * 'li location list) option ;
      mutable jc_behavior_ensures : 'li assertion;
      (* "free" postcondition, proved by static analysis. It can be used
	 as the postcondition of a call without being checked in the function
	 body, if static analysis is trusted (option [-trust-ai]) *)
      mutable jc_behavior_free_ensures : 'li assertion;
    }

type 'li fun_spec =
    {
      mutable jc_fun_requires : 'li assertion;
      (* "free" precondition, proved by static analysis. It can be used
	 to prove the function correctness without being checked at 
	 calls, if static analysis is trusted (option [-trust-ai]) *)
      mutable jc_fun_free_requires : 'li assertion; 
      (* special behavior without [assumes] clause, on which all annotations
	 not specifically attached to a behavior are checked *)
      mutable jc_fun_default_behavior : Loc.position * string * 'li behavior;
      mutable jc_fun_behavior : (Loc.position * string * 'li behavior) list;
    }


(******************)
(*    typed ast   *)
(******************)

class type ['node] c_expr =
object
  inherit typed
  inherit regioned
  inherit marked
  inherit ['node] node_positioned
  method original_type: jc_type
end

(* application, increment and assignment are statements.
   special assignment with operation disappears.
 *)
type ('li,'fi) expr_node =
  | JCEconst of const
  | JCEvar of var_info
  | JCEderef of ('li,'fi) expr * field_info
  | JCEbinary of ('li,'fi) expr * expr_bin_op * ('li,'fi) expr
  | JCEunary of expr_unary_op * ('li,'fi) expr
  | JCEapp of ('li,'fi) call
  | JCEassign_var of var_info * ('li,'fi) expr
  | JCEassign_heap of ('li,'fi) expr * field_info * ('li,'fi) expr
  | JCEinstanceof of ('li,'fi) expr * struct_info
  | JCEcast of ('li,'fi) expr * struct_info
  | JCEbitwise_cast of ('li,'fi) expr * struct_info
  | JCErange_cast of ('li,'fi) expr * enum_info
  | JCEreal_cast of ('li,'fi) expr * real_conversion
  | JCEif of ('li,'fi) expr * ('li,'fi) expr * ('li,'fi) expr
  | JCEoffset of offset_kind * ('li,'fi) expr * struct_info
  | JCEaddress of address_kind * ('li,'fi) expr
  | JCEbase_block of ('li,'fi) expr
  | JCEalloc of ('li,'fi) expr * struct_info
  | JCEfree of ('li,'fi) expr
  | JCElet of var_info * ('li,'fi) expr option * ('li,'fi) expr
  | JCEassert of string list * asrt_kind * 'li assertion
  | JCEcontract of 'li assertion option * 'li term option * var_info * 
      (Loc.position * string * 'li behavior) list * ('li,'fi) expr
  | JCEblock of ('li,'fi) expr list
  | JCEloop of 'li loop_annot * ('li,'fi) expr
  | JCEreturn_void 
  | JCEreturn of jc_type * ('li,'fi) expr (*r expected return type *) 
  | JCEtry of ('li,'fi) expr 
      * (exception_info * var_info option * ('li,'fi) expr) list * ('li,'fi) expr
  | JCEthrow of exception_info * ('li,'fi) expr option
  | JCEpack of struct_info * ('li,'fi) expr * struct_info
  | JCEunpack of struct_info * ('li,'fi) expr * struct_info
  | JCEmatch of ('li,'fi) expr * (pattern * ('li,'fi) expr) list
  | JCEshift of ('li,'fi) expr * ('li,'fi) expr
      
and ('li,'fi) expr = ('li,'fi) expr_node c_expr

and ('li,'fi) callee = 
    JClogic_fun of 'li | JCfun of 'fi

and ('li,'fi) call = 
    {
      jc_call_fun : ('li,'fi) callee;
      jc_call_args : ('li,'fi) expr list;
      mutable jc_call_region_assoc : (region * region) list;
      jc_call_label_assoc : (label * label) list;
    }

(*
type loop_annot =
    {
      jc_loop_invariant : assertion;
      jc_loop_variant : term;
    }
*)

(*type incr_op = Stat_inc | Stat_dec*)

(* application, increment and assignment are exprs. 
   expressions (without any of the above) are not exprs anymore.
   break, continue, goto are translated with exceptions.
*)


(*
type behavior =
    {  
      jc_behavior_throws : exception_info option ;
      jc_behavior_assumes : assertion option ;
(*
      jc_behavior_requires : assertion option ;
*)
      jc_behavior_assigns : location list option ;
      jc_behavior_ensures : assertion;
    }
*)

(*
type fun_spec =
    {
      jc_fun_requires : assertion;
      jc_fun_behavior : (string * behavior) list;
    }
*)


    
(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)

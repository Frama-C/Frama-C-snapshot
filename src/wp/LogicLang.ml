(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open LogicTau
open LogicRaw

(* -------------------------------------------------------------------------- *)
(* --- Primitives                                                         --- *)
(* -------------------------------------------------------------------------- *)

type integer_op = Iadd | Isub | Imul | Idiv | Imod
type real_op = Radd | Rsub | Rmul | Rdiv
type cmp_op = Ceq | Cneq | Clt | Cleq

(* -------------------------------------------------------------------------- *)
(* --- Primitives                                                         --- *)
(* -------------------------------------------------------------------------- *)

type term = TERM.t
type pred = PRED.t

let e_call f ts = TERM.e_call f ts
let p_call f xs = PRED.p_call f xs

(* -------------------------------------------------------------------------- *)
(* --- Arithmetics                                                        --- *)
(* -------------------------------------------------------------------------- *)

let e_true  = TERM.Ttrue
let e_false = TERM.Tfalse
let e_zero  = TERM.e_zero
let e_int   = TERM.e_int
let e_float k = TERM.Treal(string_of_float k)
let e_bigint z = TERM.Tint(My_bigint.to_string z)
let e_icst z = TERM.Tint z
let e_rcst z =  TERM.Treal z

let unop f a = TERM.e_prim f [a]
let binop f a b = TERM.e_prim f [a;b]
let predop f a b = PRED.p_prim f [a;b]

let i_pred = function
  | Ceq  -> PRED.L_eq
  | Cneq -> PRED.L_neq
  | Clt  -> PRED.I_lt
  | Cleq -> PRED.I_leq

let i_bool = function
  | Ceq  -> TERM.L_eq
  | Cneq -> TERM.L_neq
  | Clt  -> TERM.I_lt
  | Cleq -> TERM.I_leq

let i_op = function
  | Iadd -> TERM.I_add
  | Isub -> TERM.I_sub
  | Imul -> TERM.I_mul
  | Idiv -> TERM.I_div
  | Imod -> TERM.I_mod

let r_pred = function
  | Ceq  -> PRED.L_eq
  | Cneq -> PRED.L_neq
  | Clt  -> PRED.R_lt
  | Cleq -> PRED.R_leq

let r_bool = function
  | Ceq  -> TERM.L_eq
  | Cneq -> TERM.L_neq
  | Clt  -> TERM.R_lt
  | Cleq -> TERM.R_leq

let r_op = function
  | Radd -> TERM.R_add
  | Rsub -> TERM.R_sub
  | Rmul -> TERM.R_mul
  | Rdiv -> TERM.R_div

let e_ineg = unop TERM.I_opp
let e_rneg = unop TERM.R_opp
  
let e_icmp op = binop (i_bool op)
let p_icmp op = predop (i_pred op)
  
let e_rcmp op = binop (r_bool op)
let p_rcmp op = predop (r_pred op)

let p_equal = predop PRED.L_eq
let p_neq = predop PRED.L_neq  

let e_iop op = binop (i_op op)
let e_rop op = binop (r_op op)
  
let e_real_of_int = unop TERM.R_of_I
let e_int_of_real = unop TERM.I_of_R
  
let a_true = e_int 1
let a_false = e_int 0
  
let e_bool c = TERM.e_cond c a_true a_false
let e_cond c a b = TERM.e_cond c a b
  
let e_not = TERM.e_not
let e_and = TERM.e_and
let e_or  = TERM.e_or
  
let e_bnot   = unop TERM.I_bnot
let e_band   = binop TERM.I_band
let e_bor    = binop TERM.I_bor
let e_bxor   = binop TERM.I_bxor
let e_lshift = binop TERM.I_lsl
let e_rshift = binop TERM.I_lsr
  
let e_getfield = TERM.e_getfield
let e_setfield = TERM.e_setfield  
let e_access = TERM.e_access
let e_update = TERM.e_update

(* -------------------------------------------------------------------------- *)
(* --- Predicates                                                         --- *)
(* -------------------------------------------------------------------------- *)
 	  
let p_true = PRED.Ptrue
let p_false = PRED.Pfalse
let p_bool = PRED.p_bool
let p_not = PRED.p_not
let p_implies = PRED.p_implies
let p_and = PRED.p_and
let p_or  = PRED.p_or
let p_xor = PRED.p_xor
let p_iff = PRED.p_iff
let p_cond = PRED.p_cond

let p_named label p = PRED.Pnamed(label,p)
let p_hide p = p

let rec p_conj = function
  | [] -> PRED.Ptrue | [p] -> p
  | p::ps -> PRED.p_and p (p_conj ps)
      
let rec p_disj = function
  | [] -> PRED.Pfalse | [p] -> p
  | p::ps -> PRED.p_or p (p_disj ps)

let rec p_goal hs p = match hs with
  | [] -> p
  | h::hs -> PRED.p_implies h (p_goal hs p)

(* -------------------------------------------------------------------------- *)
(* --- Variables                                                          --- *)
(* -------------------------------------------------------------------------- *)

type var = VAR.t
type pool = VAR.pool

let tau_of_var = VAR.tau_of_var
let pool = VAR.pool
let fresh = VAR.fresh
let e_var x = TERM.Tvar x
let e_let = SUBST.e_let
let p_let = SUBST.p_let
let p_forall = PRED.p_forall
let p_exists = PRED.p_exists
let is_atomic = SUBST.is_atomic

module Vmap = VMAP
module Vset = VSET

(* -------------------------------------------------------------------------- *)
(* --- PRETTY PRINTING                                                    --- *)
(* -------------------------------------------------------------------------- *)

let space = LogicId.space ()
let () = LogicId.reserved space [
  "if" ; "then" ; "let" ; "in" ;
  "pointer" ;
  "and" ; "or" ; "not" ; "forall" ; "exists" ;
]

let pretty = new LogicPretty.engine space
let pp_tau = pretty#pp_tau
let pp_term = pretty#alpha pretty#pp_term
let pp_pred = pretty#alpha pretty#pp_pred

(* -------------------------------------------------------------------------- *)
(* --- DEPENDENCIES                                                       --- *)
(* -------------------------------------------------------------------------- *)

let add_depend_tau = LogicTau.depend
let add_depend_term = TERM.depend
let add_depend_pred = PRED.depend

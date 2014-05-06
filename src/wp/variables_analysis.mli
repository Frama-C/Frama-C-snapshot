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

(**
   This analysis performs a classification of the variables of the input
   program. The aim of this classification is to optimize the translation
   of variables by WP: 
    1) optimization of the by-reference call and 
    2) functional variables. 
**)

(**
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
 
**)

type var_kind = 
    Fvar | Cvar | PRarg | ARarg | PRpar of int | ARpar of int 


(** [dispatch_cvar v] returns the var_kind associated to the C variable [v]
    according the current optimisations activated.*)
val dispatch_cvar: Cil_types.varinfo -> var_kind

(** [dispatch_lvar v] returns the var_kind associated to the logic variable [v]
    according the current optimisations activated.*)
val dispatch_lvar: Cil_types.logic_var -> var_kind

(** [is_to_scope v] returns true if [v] has to been scoped into the inner 
    memory model : cvar of ref*)
val is_to_scope : Cil_types.varinfo -> bool

(** [precondition_compute ()] adds warnings and precondition suitable 
    to the current optimisations which are activated *)
val precondition_compute : unit -> unit

(** [brackets_typ typ] returns the numbre of brackets of the type [typ].*)
val brackets_typ : Cil_types.typ -> int 

(** [is_user_formal_in_builtins lv] tests if the address 
    of the by-reference formal [lv] of user definition is an argument 
    of (one or more) ACSL builtin predicate(s) or function : 
      valid and family, separated, block_length, initialized*)
val is_user_formal_in_builtin : Cil_types.logic_var -> bool

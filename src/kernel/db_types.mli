(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: db_types.mli,v 1.39 2008/04/01 09:25:21 uid568 Exp $ *)

(** Type definitions for [Db] module. Each plugin may add its additional
    types. *)

open Cil_types
open Cil

type from_model = Lmap_bitwise.From_Model.t

(** Internal representation of decorated C functions *)
type cil_function =
  | Definition of (fundec * location) (** defined function *)
  | Declaration of (funspec * varinfo * varinfo list option * location)
      (** Declaration(spec,f,args,loc) represents a leaf function [f] with
          specification [spec] and arguments [args], at location [loc]. As
          with the [TFun] constructor of {!Cil_types.typ}, the arg list is
          optional, to distinguish [void f()] ([None]) from
          [void f(void)] ([Some []]).
       *)

type rooted_code_annotation =
  | User of code_annotation
  | AI of Alarms.t*code_annotation
  | WP of Fol.decl list * int (** formula * unique id *)

type 'a before_after = Before of 'a | After of 'a

type stmts_graph = Graph.Imperative.Digraph.Concrete(Cilutil.StmtComparable).t

(** Except field [fundec], do not used the other fields directly.
    Prefer to use {!Kernel_function.find_return}, {!Kernel_function.get_spec} 
    and {!Stmts_graph.stmt_can_reach}. *)
type kernel_function = {
  fundec : cil_function;
  mutable return_stmt : stmt option;
  mutable spec : funspec;
  mutable stmts_graph : stmts_graph option;
}

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

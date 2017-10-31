(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Simplified interfaces for abstract domains. Complete abstract domains can be
    built from these interfaces through the functors in {!Domain_builder}.  More
    documentation can be found on the complete interface of abstract domains,
    in {!Abstract_domain}. *)

open Cil_types
open Eval

(** Both the formal argument of a called function and the concrete argument at a
    call site. *)
type simple_argument = {
  formal: varinfo;
  concrete: exp;
}

(** Simple information about a function call. *)
type simple_call = {
  kf: kernel_function;                (* The called function. *)
  arguments: simple_argument list;    (* The list of arguments of the call. *)
  rest: exp list;                     (* Extra arguments. *)
  return: varinfo option;             (* Fake varinfo where the result of the
                                         call is stored. *)
  recursive: bool;                    (* Is the call recursive? *)
}

(** Simplest interface for an abstract domain. No exchange of information with
    the other abstractions of Eva. *)
module type Minimal = sig
  type t
  val name: string
  val compare: t -> t -> int
  val hash: t -> int

  (** Lattice structure. *)

  val top: t
  val is_included: t -> t -> bool
  val join: t -> t -> t
  val widen: kernel_function -> stmt -> t -> t -> t

  (** Transfer functions. *)

  val assign: kinstr -> lval -> exp -> t -> t or_bottom
  val assume: stmt -> exp -> bool -> t -> t or_bottom
  val start_call: stmt -> simple_call -> t -> t
  val finalize_call: stmt -> simple_call -> pre:t -> post:t -> t or_bottom
  val approximate_call: stmt -> simple_call -> t -> t list or_bottom

  (** Initialization of variables. *)

  val empty: unit -> t
  val introduce_globals: varinfo list -> t -> t
  val initialize_variable:
    lval -> initialized:bool -> Abstract_domain.init_value -> t -> t

  val enter_scope: kernel_function -> varinfo list -> t -> t
  val leave_scope: kernel_function -> varinfo list -> t -> t

  (** Pretty printers. *)

  val pretty: Format.formatter -> t -> unit
  val show_expr: t -> Format.formatter -> exp -> unit
end

(** The simplest interface of domains, equipped with a frama-c datatype. *)
module type Minimal_with_datatype = sig
  include Minimal
  include Datatype.S with type t := t
end


(** A simpler functional interface for valuations. *)
type cvalue_valuation = {
  find: exp -> Cvalue.V.t flagged_value or_top;
  find_loc: lval -> Precise_locs.precise_location or_top
}

(** A simple interface allowing the abstract domain to use the value and
    location abstractions computed by the other domains. Only the {!Cvalue.V}
    and the the {!Precise_locs} abstractions are available in this interface, on
    the transfer functions for assignment, assumption and at the call sites. On
    the other hand, the abstract domain cannot assist the computation of these
    value and location abstractions. The communication is thus unidirectional,
    from other domains to these simpler domains. *)
module type Simple_Cvalue = sig
  include Datatype.S

  (** Lattice structure. *)

  val top: t
  val is_included: t -> t -> bool
  val join: t -> t -> t
  val widen: kernel_function -> stmt -> t -> t -> t

  (** Query functions. *)

  val extract_expr: t -> exp -> Cvalue.V.t or_bottom
  val extract_lval:
    t -> lval -> typ -> Precise_locs.precise_location -> Cvalue.V.t or_bottom

  (** Transfer functions. *)

  val assign:
    kinstr -> Precise_locs.precise_location left_value -> exp ->
    Cvalue.V.t assigned -> cvalue_valuation -> t -> t or_bottom

  val assume: stmt -> exp -> bool -> cvalue_valuation -> t -> t or_bottom

  val start_call: stmt -> Cvalue.V.t call -> cvalue_valuation -> t -> t

  val finalize_call: stmt -> Cvalue.V.t call ->  pre:t -> post:t -> t or_bottom
  val approximate_call: stmt -> Cvalue.V.t call -> t -> t list or_bottom

  (** Initialization of variables. *)

  val empty: unit -> t
  val introduce_globals: varinfo list -> t -> t
  val initialize_variable:
    lval -> initialized:bool -> Abstract_domain.init_value -> t -> t

  val enter_scope: kernel_function -> varinfo list -> t -> t
  val leave_scope: kernel_function -> varinfo list -> t -> t

  (** Pretty printer. *)
  val show_expr: t -> Format.formatter -> exp -> unit
end

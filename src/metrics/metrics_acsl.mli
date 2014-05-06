(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Visitor to compute various metrics about annotations *)

type acsl_stats =
  { mutable f_requires: int; (** number of requires in function contracts *)
    mutable s_requires: int; (** number of requires in statement contracts *)
    mutable f_ensures: int;
    mutable s_ensures: int;
    mutable f_behaviors: int;
    mutable s_behaviors: int;
    mutable f_assumes: int;
    mutable s_assumes: int;
    mutable f_assigns: int;
    mutable s_assigns: int; (** does not include loop assigns. *)
    mutable f_froms: int;
    mutable s_froms: int; (** does not include loop dependencies. *)
    mutable invariants: int;
    mutable loop_assigns: int;
    mutable loop_froms: int;
    mutable variants: int;
    mutable asserts: int;
  }

val empty_acsl_stat: unit -> acsl_stats

val incr_f_requires: acsl_stats -> unit
val incr_s_requires: acsl_stats -> unit
val incr_f_ensures: acsl_stats -> unit
val incr_s_ensures: acsl_stats -> unit
val incr_f_behaviors: acsl_stats -> unit
val incr_s_behaviors: acsl_stats -> unit
val incr_f_assumes: acsl_stats -> unit
val incr_s_assumes: acsl_stats -> unit
val incr_f_assigns: acsl_stats -> unit
val incr_s_assigns: acsl_stats -> unit
val incr_f_froms: acsl_stats -> unit
val incr_s_froms: acsl_stats -> unit
val incr_invariants: acsl_stats -> unit
val incr_loop_assigns: acsl_stats -> unit
val incr_loop_froms: acsl_stats -> unit
val incr_variants: acsl_stats -> unit
val incr_asserts: acsl_stats -> unit

val pretty_acsl_stats: Format.formatter -> acsl_stats -> unit
val pretty_acsl_stats_html: Format.formatter -> acsl_stats -> unit

val get_global_stats: unit -> acsl_stats
val get_kf_stats: Kernel_function.t -> acsl_stats

val dump_acsl_stats: Format.formatter -> unit
val dump_acsl_stats_html: Format.formatter -> unit

val dump: unit -> unit

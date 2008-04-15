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

(*i $Id: coptions.mli,v 1.39 2008/11/05 14:03:13 filliatr Exp $ i*)

(*s environment variables *)

val libdir : string
val whylib : string
val libfile : string (* depends on the command-line option --arith-mem *)

(*s command-line options *)

val zones : bool
val show_time : bool
val no_zone_type : bool
val parse_only : bool
val type_only : bool
val print_norm : bool
val print_graph : bool
val debug : bool
val verbose : bool
val werror : bool
val with_cpp : bool
val cpp_command : string
val cpp_dump : bool
val why_opt : unit -> string
val coq_tactic : string
val separate : bool
val closed_program : bool
val local_aliasing : bool
val no_alloc_table : bool
val arith_memory_model : bool
val abstract_interp : bool
val gen_invariant : bool
val absint_as_proof : bool

val use_floats : bool ref
val floats : bool
type fp_rounding_mode = 
  | RM_nearest_even | RM_to_zero | RM_up | RM_down | RM_nearest_away 
  | RM_dynamic
val fp_rounding_mode : fp_rounding_mode ref
val dft_fp_rounding_mode : fp_rounding_mode
val fp_overflow_check : bool

type int_model = IMexact | IMbounded | IMmodulo
val int_model : int_model
val machine_ints : bool

val enum_check : bool

val char_size : int
val short_size : int
val int_size : int
val long_size : int
val long_long_size : int

val files : unit -> string list 

val verify : string -> bool

(*s The log file *)

val log : Format.formatter
val lprintf : ('a, Format.formatter, unit) format -> 'a
val close_log : unit -> unit

type evaluation_order_t =
    { binary_left_to_right : bool;
      assign_left_to_right : bool;
      call_left_to_right : bool }

val evaluation_order : evaluation_order_t

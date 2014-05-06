(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)


open Cabs
open Format

val pp_cabsloc : formatter -> cabsloc -> unit
val pp_storage : formatter -> storage -> unit
val pp_fun_spec : formatter -> funspec -> unit
val pp_cvspec : formatter -> cvspec -> unit
val pp_const : formatter -> constant -> unit
val pp_labels : formatter -> string list -> unit
val pp_typeSpecifier : formatter -> typeSpecifier -> unit
val pp_spec_elem : formatter -> spec_elem -> unit
val pp_spec : formatter -> specifier -> unit
val pp_decl_type : formatter -> decl_type -> unit
val pp_name_group : formatter -> name_group -> unit
val pp_field_group : formatter -> field_group -> unit
val pp_field_groups : formatter -> field_group list -> unit
val pp_init_name_group : formatter -> init_name_group -> unit
val pp_name : formatter -> name -> unit
val pp_init_name : formatter -> init_name -> unit
val pp_single_name : formatter -> single_name -> unit
val pp_enum_item : formatter -> enum_item -> unit
val pp_def : formatter -> definition -> unit
val pp_block : formatter -> block -> unit
val pp_raw_stmt : formatter -> raw_statement -> unit
val pp_stmt : formatter -> statement -> unit
val pp_for_clause : formatter -> for_clause -> unit
val pp_bin_op : formatter -> binary_operator -> unit
val pp_un_op : formatter -> unary_operator -> unit
val pp_exp : formatter -> expression -> unit
val pp_exp_node : formatter -> cabsexp -> unit
val pp_init_exp : formatter -> init_expression -> unit
val pp_initwhat : formatter -> initwhat -> unit
val pp_attr : formatter -> attribute -> unit
val pp_attrs : formatter -> attribute list -> unit
val pp_file : formatter -> file -> unit

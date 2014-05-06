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

(** Printers for the Cabs AST *)
val version : string

val msvcMode : bool ref
val printLn : bool ref
val printLnComment : bool ref
val printCounters : bool ref
val printComments : bool ref

val get_operator : Cabs.expression -> (string * int)
  
val print_specifiers : Format.formatter -> Cabs.specifier -> unit
val print_type_spec : Format.formatter -> Cabs.typeSpecifier -> unit
val print_struct_name_attr :
  string -> Format.formatter -> (string * Cabs.attribute list) -> unit
val print_decl : string -> Format.formatter -> Cabs.decl_type -> unit
val print_fields : Format.formatter -> Cabs.field_group list -> unit
val print_enum_items : Format.formatter -> Cabs.enum_item list -> unit
val print_onlytype : Format.formatter -> Cabs.specifier * Cabs.decl_type -> unit
val print_name : Format.formatter -> Cabs.name -> unit
val print_init_name : Format.formatter -> Cabs.init_name -> unit
val print_name_group : Format.formatter -> Cabs.name_group -> unit
val print_field_group : Format.formatter -> Cabs.field_group -> unit
val print_field : Format.formatter -> Cabs.name * Cabs.expression option -> unit
val print_init_name_group : Format.formatter -> Cabs.init_name_group -> unit
val print_single_name : Format.formatter -> Cabs.single_name -> unit
val print_params : Format.formatter -> (Cabs.single_name list * bool) -> unit
val print_init_expression : Format.formatter -> Cabs.init_expression -> unit
val print_expression : Format.formatter -> Cabs.expression -> unit
val print_expression_level : int -> Format.formatter -> Cabs.expression -> unit
val print_statement : Format.formatter -> Cabs.statement -> unit
val print_block : Format.formatter -> Cabs.block -> unit
val print_attribute : Format.formatter -> Cabs.attribute -> unit
val print_attributes : Format.formatter -> Cabs.attribute list -> unit
val print_defs : Format.formatter -> (bool*Cabs.definition) list -> unit
val print_def : Format.formatter -> Cabs.definition -> unit

val printFile : Format.formatter -> Cabs.file -> unit

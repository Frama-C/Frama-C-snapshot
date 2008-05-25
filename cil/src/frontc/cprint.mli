(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

val version : string
type loc = { line : int; file : string; }
val lu : loc
val cabslu : Cabs.cabsloc
val curLoc : Cabs.cabsloc ref
val msvcMode : bool ref
val printLn : bool ref
val printLnComment : bool ref
val printCounters : bool ref
val printComments : bool ref
val out : out_channel ref
val width : int ref
val tab : int ref
val max_indent : int ref
val line : string ref
val line_len : int ref
val current : string ref
val current_len : int ref
val spaces : int ref
val follow : int ref
val roll : int ref
val new_line : unit -> unit
val space : unit -> unit
val indent : unit -> unit
val unindent : unit -> unit
val force_new_line : unit -> unit
val flush : unit -> unit
val commit : unit -> unit
val print_unescaped_string : string -> unit
val print_list : (unit -> unit) -> ('a -> 'b) -> 'a list -> unit
val print_commas : bool -> ('a -> 'b) -> 'a list -> unit
val print_string : string -> unit
val print_wstring : Escape.wstring -> unit
val print_specifiers : Cabs.specifier -> unit
val print_type_spec : Cabs.typeSpecifier -> unit
val print_struct_name_attr : string -> string -> Cabs.attribute list -> unit
val print_decl : string -> Cabs.decl_type -> unit
val print_fields : Cabs.field_group list -> unit
val print_enum_items : Cabs.enum_item list -> unit
val print_onlytype : Cabs.specifier * Cabs.decl_type -> unit
val print_name : Cabs.name -> unit
val print_init_name : Cabs.init_name -> unit
val print_name_group : Cabs.name_group -> unit
val print_field_group : Cabs.field_group -> unit
val print_field : Cabs.name * Cabs.expression option -> unit
val print_init_name_group : Cabs.init_name_group -> unit
val print_single_name : Cabs.single_name -> unit
val print_params : Cabs.single_name list -> bool -> unit
val print_old_params : string list -> bool -> unit
val get_operator : Cabs.expression -> string * int
val print_comma_exps : Cabs.expression list -> unit
val print_init_expression : Cabs.init_expression -> unit
val print_expression : Cabs.expression -> unit
val print_expression_level : int -> Cabs.expression -> unit
val print_statement : Cabs.statement -> unit
val print_block : Cabs.block -> unit
val print_substatement : Cabs.statement -> unit
val print_attribute : Cabs.attribute -> unit
val print_attributes : Cabs.attribute list -> unit
val print_defs : (bool*Cabs.definition) list -> unit
val print_def : Cabs.definition -> unit
val comprint : string -> unit
val comstring : string -> string
val printFile : out_channel -> Cabs.file -> unit
val set_tab : int -> unit
val set_width : int -> unit

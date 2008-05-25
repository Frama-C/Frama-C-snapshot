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

open Cil_types

type location = { filename : string; lineno : int }

val push : location -> unit
val pop : location -> unit

val add_var : location -> string -> varinfo -> unit
val add_type : location -> string -> typ -> unit
val add_label : location -> string -> string -> unit
val add_enum_id : location -> string -> exp * typ -> unit

val find_var : location -> string -> varinfo
val find_enum_id : location -> string -> exp * typ

val find_glob_var : location -> string -> varinfo

val find_typedef : location -> string -> typ
val find_struct : location -> string -> typ
val find_union : location -> string -> typ
val find_enum : location -> string -> typ

val mem_label : location -> string -> bool

val find_function_after : location -> varinfo
  (* [find_function_after loc] returns the first function defined or declared
     (strictly) after location [loc] *)

val find_function_before : location -> varinfo
  (* [find_function_after loc] returns the first function defined or declared
     before or at location [loc] *)

val find_block_after : location -> int * int

val dump : unit -> unit

val replace : 
  (varinfo -> varinfo) ->
  (typ -> typ) ->
  (exp * typ -> exp * typ) -> unit

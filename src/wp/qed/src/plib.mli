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

(* -------------------------------------------------------------------------- *)
(**    Pretty Printing Utilities.                                             *)
(* -------------------------------------------------------------------------- *)

open Format

(** Message Formatters *)

val kprintf : (string -> 'b) -> ('a,Format.formatter,unit,'b) format4 -> 'a
val sprintf : ('a,Format.formatter,unit,string) format4 -> 'a
val failure : ('a,Format.formatter,unit,'b) format4 -> 'a
val to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

(** Printy printers *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a printer2 = Format.formatter -> 'a -> 'a -> unit

(** Function calls *)

val pp_call_var   : f:string -> 'a printer -> 'a list printer
val pp_call_void  : f:string -> 'a printer -> 'a list printer
val pp_call_apply : f:string -> 'a printer -> 'a list printer

(** Operators *)

val pp_assoc : ?e:string -> op:string -> 'a printer -> 'a list printer
val pp_binop : op:string -> 'a printer -> 'a printer2
val pp_fold_binop : ?e:string -> op:string -> 'a printer -> 'a list printer
val pp_fold_call  : ?e:string -> f:string -> 'a printer -> 'a list printer
val pp_fold_apply : ?e:string -> f:string -> 'a printer -> 'a list printer
val pp_fold_call_rev  : ?e:string -> f:string -> 'a printer -> 'a list printer
val pp_fold_apply_rev : ?e:string -> f:string -> 'a printer -> 'a list printer

(** Iterations *)

type index = Isingle | Ifirst | Ilast | Imiddle
val iteri : (index -> 'a -> unit) -> 'a list -> unit
val iterk : (int -> 'a -> unit) -> 'a list -> unit
val mapk : (int -> 'a -> 'b) -> 'a list -> 'b list

val pp_listcompact : sep:string -> 'a printer -> 'a list printer
val pp_listsep : sep:string -> 'a printer -> 'a list printer

(** string substitution *)
val global_substitute_fmt :
  Str.regexp -> string printer -> Format.formatter -> string -> unit
(** substitute the result of the given printer for each non-overlapping part
    of the given string that match the regexp *)

val iter_group : Str.regexp -> (string -> unit) -> string -> unit
(** call the given function for each non-overlapping part of the given string
    that match the regexp *)

val substitute_list  : 'a printer -> string -> 'a list printer
(** [substitute_list templ print_arg fmt l] prints in the formatter [fmt]
     the list [l] using the template [templ] and the printer [print_arg].
    The template use %[0-9]+ hole.
*)


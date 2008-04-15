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

(* $Id: funTbl.mli,v 1.7 2008/11/20 10:21:37 uid568 Exp $ *)

(** Generic tables of functions indexed by values of type {!Type.t}. *)

type t

(** [create n] create  a table of functions. [n] is the initial size. *)
val create: int -> t
  
(** May be raised by {!register}
    @plugin development guide *)
exception AlreadyExists of string

(** [register tbl name type function] registered the [function] in [tbl] with
    the given [name] and the given [type].
    @raise AlreadyExists if the given name is already registered in [tbl] *)
val register: t -> string -> 'a Type.t -> 'a -> unit 

(** May be raised by {!apply}.
    @plugin development guide *)
exception Not_Registered of string
  
(** May be raised by {!apply}.
    @plugin development guide *)
exception Incompatible_Type of string

(** [appply tbl name type] return the function registered in [tbl] with the
    given [name] and the given [type].
    @raise Incompatible_Type if the given [type] is not the same as previously
    registered by [register]
    @raise Not_Registered if the given name is not registered in [tbl] *) 
val apply : t -> string -> 'a Type.t -> 'a

(**/**)
(** Same as apply but this function is very unsafe. 
    This function is not for the casual user.
    @raise Not_Registered if the given name is not registered in [tbl] *)
val unsafe_apply : t -> string -> 'a
(**/**)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

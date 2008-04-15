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

(** Type variables: unification, generalization, ... *)

(** The type of type variables. *)
type t

(** Obtain a fresh type variable. *)
val fresh: string -> t
(** The argument is the name of the variable.
It can be anything, it shall only be used for pretty-printing purposes. *)

(** Find the value associated to a type variable. *)
val find: t -> t list -> 'a list -> 'a
(** Useful for type parameters.

[find v params param_values]: if [List.nth params i = v], return
[List.nth param_values i]. *)

(** Unique ID of a variable, different for each variable, even if it is
quantified. *)
val uid: t -> int

(** The name of a variable, which should only be used for
pretty-printing purposes. *)
val name: t -> string

(** The unique name of a variable, which is composed of its name and its UID. *)
val uname: t -> string

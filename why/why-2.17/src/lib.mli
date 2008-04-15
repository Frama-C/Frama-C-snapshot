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

(*i $Id: lib.mli,v 1.13 2008/11/05 14:03:17 filliatr Exp $ i*)

module Sset : Set.S with type elt = string

val mkdir_p : string -> unit

(* [file dir file] returns "dir/basename" if [file] is "dirname/basename", 
   creating [dir] if necessary. *)
val file : dir:string -> file:string -> string

(* [file_subdir dir file] returns "dirname/dir/basename" if [file] is "dirname/basename", 
   creating [dir] if necessary. *)
val file_subdir : dir:string -> file:string -> string

(* [file_copy_if_different f1 f2] copies [f1] into name [f2], unless
   [f2] already exists and is identical to [f1] (thus keeping the same
   modification date) *)
val file_copy_if_different : string -> string -> unit



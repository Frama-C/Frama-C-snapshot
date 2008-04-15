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

(*i $Id: pretty.mli,v 1.15 2008/11/10 16:23:41 moy Exp $ i*)

(* Why pretty-printer *)

val push_decl : ?ergo:bool -> Logic_decl.t -> unit

(* [push_or_output_decl d] either pushes the goal in a queue like [push_decl]
   for declarations other than goals, and produces a file for goal 
   declarations much as what [output_files] does. *)
val push_or_output_decl : Logic_decl.t -> unit

val reset : unit -> unit

val output_file : string -> unit

(* [output_files f] produces the context in file [f_ctx.why]
   and each goal in a seaparate file [f_po<i>.why] for i=1,2,... *)
val output_files : string -> unit

(* [output_project f] produces a whole project description, in a file
[f.wpr], together with other needed files [f_ctx.why], [f_lemmas.why],
and each goal in a separate file [f_po<i>.why] for i=1,2,... *)
val output_project : string -> Project.t

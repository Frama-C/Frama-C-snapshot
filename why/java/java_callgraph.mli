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

val compute_logic_calls : Java_env.java_logic_info -> [< Java_typing.logic_decl_body] -> unit

val compute_calls : Java_env.method_info -> 'a -> Java_tast.statement list -> unit

val compute_constr_calls : Java_env.constructor_info -> 'a -> Java_tast.statement list -> unit

val compute_logic_components : 
  (int, (Java_env.java_logic_info * Java_typing.logic_def_body)) Hashtbl.t -> 
  Java_env.java_logic_info list array

val compute_components : 
  (int, Java_typing.method_table_info) Hashtbl.t -> 
  (int, Java_typing.constructor_table_info) Hashtbl.t -> 
  Java_env.method_or_constructor_info list array


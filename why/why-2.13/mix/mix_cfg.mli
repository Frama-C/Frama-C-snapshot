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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* From assembly to a set of purely sequential programs *)

module type INPUT = sig
  
  module Label : sig
    type t
    val create : unit -> t
    val equal : t -> t -> bool
    val hash : t -> int
    val to_string : t -> string
  end

  type predicate
    
  val ptrue : predicate

  val string_of_predicate : predicate -> string

  type statement
    
  val void_stmt : statement

  val append_stmt : statement -> statement -> statement

  val assert_stmt : predicate -> statement

  val string_of_stmt : statement -> string
    
end


module Make(X : INPUT) : sig

  type asm =
    | Ainvariant of X.predicate
    | Ajump of X.Label.t
    | Acond of X.Label.t * X.statement * X.statement
    | Ahalt
    | Aother of X.statement

  type asm_code = (X.Label.t * asm) list
  
  type seq_code = {
    seq_pre : X.predicate option;
    seq_code : X.statement;
  }

  val transform : show_graph:bool -> asm_code -> X.Label.t -> seq_code list

end



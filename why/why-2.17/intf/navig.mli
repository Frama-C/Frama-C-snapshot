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

(*i $Id: navig.mli,v 1.12 2008/11/05 14:03:14 filliatr Exp $ i*)

(*s trees *)

module type Tree = sig

  type t
  val children : t -> t list

  type info
  val info : t -> info
  val show_info : info -> unit

end

(*s trees equipped with navigation functions *)

module type NavTree = sig

  type tree (* type of trees *)
  type t    (* type of navigable trees *)

  val create : tree list -> t

  (* functions to navigate in the tree; 
     must raise [NoMove] when the move is not possible *)
  exception NoMove
  val down : t -> t
  val up : t -> t
  val left : t -> t
  val right : t -> t

  type info
  val info : t -> info
  val show_info : info -> unit

end

(*s functor to add navigation fuctions to a tree *)

module MakeNavTree (T : Tree) : 
  NavTree with type tree = T.t and type info = T.info 

(*s functor to build a navigator *)

module MakeNavigator (T : NavTree) : sig

  val set : T.t -> unit

  val down : unit -> unit
  val up : unit -> unit
  val left : unit -> unit
  val right : unit -> unit

  (* depth-first traversal *)
  val next : unit -> unit

end

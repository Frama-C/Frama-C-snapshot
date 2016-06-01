(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(** The Frama-C source viewer.
    That is the buffer where Frama-C puts its pretty-printed AST. *)

val make : ?name:string -> packing:(GObj.widget -> unit) -> unit ->
  GSourceView2.source_view
(** Build a new source viewer. *)

val buffer : unit -> GSourceView2.source_buffer
(** @return the buffer displaying the pretty-printed AST. *)

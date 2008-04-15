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

(** Interpretation of patterns. *)

(** Translate an OCaml pattern to a Jessie pattern. Return the environment
extended with the variables bound by the pattern. *)
val pattern: Ml_env.t -> Ml_ocaml.Typedtree.pattern -> Ml_env.t * Jc_ast.pattern

(** Translate an OCaml pattern to a Jessie pattern-matching term with
a single case.
The second argument is used to get the body of the pattern; its argument is
the environment in which the body should be typed (with the variables bound
by the pattern).
Return value is [vi, t] where [vi] is a Jc_env.var_info which should be bound
to the argument of the pattern-matching. *)
val pattern_term: Ml_env.t -> (Ml_env.t -> Jc_ast.term) ->
  Ml_ocaml.Typedtree.pattern -> Jc_env.var_info * Jc_ast.term

(** Same as [pattern_term] but with a list of arguments instead. *)
val pattern_list_term: Ml_env.t -> (Ml_env.t -> Jc_ast.term) ->
  Ml_ocaml.Typedtree.pattern list -> Jc_env.var_info list * Jc_ast.term

(** Same as [pattern_term] but for assertions. *)
val pattern_assertion: Ml_env.t -> (Ml_env.t -> Jc_ast.assertion) ->
  Ml_ocaml.Typedtree.pattern -> Jc_env.var_info * Jc_ast.assertion

(** Same as [pattern_list_term] but for assertions. *)
val pattern_list_assertion: Ml_env.t -> (Ml_env.t -> Jc_ast.assertion) ->
  Ml_ocaml.Typedtree.pattern list -> Jc_env.var_info list * Jc_ast.assertion

(*
Local Variables: 
compile-command: "unset LANG; make -j -C .. bin/jessica.opt"
End: 
*)

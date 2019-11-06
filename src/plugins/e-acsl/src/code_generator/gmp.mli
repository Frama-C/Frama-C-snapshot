(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Calls to the GMP's API. *)

open Cil_types

val init: loc:location -> exp -> stmt
(** build stmt [mpz_init(v)] or [mpq_init(v)] depending on typ of [v] *)

val init_set: loc:location -> lval -> exp -> exp -> stmt
(** [init_set x_as_lv x_as_exp e] builds stmt [x = e] or [mpz_init_set*(v, e)]
    or [mpq_init_set*(v, e)] with the good function 'set'
    according to the type of [e] *)

val clear: loc:location -> exp -> stmt
(** build stmt [mpz_clear(v)] or [mpq_clear(v)] depending on typ of [v] *)

val affect: loc:location -> lval -> exp -> exp -> stmt
(** [affect x_as_lv x_as_exp e] builds stmt [x = e] or [mpz_set*(e)]
    or [mpq_set*(e)] with the good function 'set'
    according to the type of [e] *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

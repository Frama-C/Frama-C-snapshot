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

(** GMP Values. *)

open Cil_types

(**************************************************************************)
(***************************** GMP types***********************************)
(**************************************************************************)

let mk_dummy_type_info_ref () =
  ref
    { torig_name = "";
      tname = "";
      ttype = TVoid [];
      treferenced = false }

module type S = sig
  val t: unit -> typ
  val t_as_ptr: unit -> typ
  val is_now_referenced: unit -> unit
  val is_t: typ -> bool
end

module Make(X: sig end) = struct

  let t_torig_ref = mk_dummy_type_info_ref ()
  let t_struct_torig_ref = mk_dummy_type_info_ref ()

  let set_t ty = t_torig_ref := ty
  let set_t_struct ty = t_struct_torig_ref := ty

  let is_now_referenced () = !t_torig_ref.treferenced <- true

  let t () = TNamed(!t_torig_ref, [])

  (* create a unique shared representation in order to use [==] in [is_t] *)
  let t_as_ptr_info =
    lazy
      {
        torig_name = "";
        tname = !t_struct_torig_ref.tname ^ " *";
        ttype = TArray(
            TNamed(!t_struct_torig_ref, []),
            Some (Cil.one ~loc:Cil_datatype.Location.unknown),
            {scache = Not_Computed},
            []);
        treferenced = true;
      }

  let t_as_ptr () = TNamed (Lazy.force t_as_ptr_info, [])

  let is_t ty = match ty with
    | TNamed(tinfo, []) ->
      tinfo == !t_torig_ref || tinfo == Lazy.force t_as_ptr_info
    | _ -> false

end

module Z = Make(struct end)
module Q = Make(struct end)

(**************************************************************************)
(******************* Initialization of mpz and mpq types ******************)
(**************************************************************************)

let init () =
  Options.feedback ~level:2 "initializing GMP types.";
  let set_mp_t = object (self)
    inherit Cil.nopCilVisitor

    (* exit after having initialized the 4 values (for Z.t and Q.t) *)
    val mutable visited = 0
    method private set f info =
      f info;
      if visited = 3 then
        raise Exit
      else begin
        visited <- visited + 1;
        Cil.SkipChildren
      end

    method !vglob = function
      | GType({ torig_name = name } as info, _) ->
        if name = "__e_acsl_mpz_t" then self#set Z.set_t info
        else if name = "__e_acsl_mpz_struct" then self#set Z.set_t_struct info
        else if name = "__e_acsl_mpq_t" then self#set Q.set_t info
        else if name = "__e_acsl_mpq_struct" then self#set Q.set_t_struct info
        else Cil.SkipChildren
      | _ ->
        Cil.SkipChildren

  end in
  try Cil.visitCilFileSameGlobals set_mp_t (Ast.get ()) with Exit -> ()

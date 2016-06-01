(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Cil_types

module Exp = Cil_datatype.ExpStructEq
module Lval = Cil_datatype.LvalStructEq

type atom =
  | Exp of Exp.t
  | Lvalue of Lval.t
  | Old of atom
  | Fix of atom

let of_exp exp = match exp.enode with
  | Lval v -> Lvalue v
  | _ -> Exp exp

module type Atom = sig
  include Datatype.S_with_collections

  val make : atom -> t
  val get : t -> atom

  (* Constructors *)
  val of_exp : exp -> t
  val of_lval : lval -> t
  val old : t -> t
  val reborn : t -> t
  val fix : t -> t
  val release : t -> t

  (** Utilitaries *)
  val is_alive : t -> bool
  val is_old : t -> bool
  val is_fix : t -> bool

  val id : t -> int
  val self : State.t

  val debug : t -> string
  val pretty_debug : t Pretty_utils.formatter

  module Hptset : Hptset.S with type elt = t
  module Lattice_Set : Lattice_type.Lattice_Hashconsed_Set with module O = Hptset
  module Lmap_Bitwise: Lmap_bitwise.Location_map_bitwise with type v = Lattice_Set.t
end

(* -------------------------------------------------------------------------
                             Unhashconsed Atoms
   -------------------------------------------------------------------------- *)

module Unhashconsed_Atom = struct

  type tt = atom

  let make t = t
  let get t = t

  let of_exp exp = of_exp exp
  let of_lval lval = Lvalue lval

  let old atom = Old atom
  let fix atom = Fix atom

  let reborn = function
    | Old t -> t
    | x -> x

  let release = function
    | Fix t -> t
    | x -> x

  let is_alive = function
    | Lvalue _ | Exp _ -> true
    | _ -> false

  let is_old = function
    | Old _ -> true
    | _ -> false

  let is_fix = function
    | Old _ -> true
    | _ -> false


  let rec exp_id e = match e.enode with
    | Lval lval -> lval_id lval
    | _ -> "e-" ^ (string_of_int e.eid)

  and lval_id lval = match fst lval with
    | Var varinfo -> "v-" ^ varinfo.vname
    | Mem e -> "p-" ^ (string_of_int e.eid)

  let rec debug = function
    | Exp e -> exp_id e
    | Lvalue lval -> lval_id lval
    | Old t -> "old[" ^ (debug t) ^ "]"
    | Fix t -> "fix[" ^ (debug t) ^ "]"

  include Datatype.Make_with_collections (struct
      include Datatype.Serializable_undefined

      type t = tt
      let name = "equality_term"
      let reprs = [ Exp Cil_datatype.Exp.dummy ]

      let structural_descr =
        let r = Structural_descr.Recursive.create () in
        let d = Structural_descr.t_sum
            [| [| Exp.packed_descr |] ;
               [| Lval.packed_descr |] ;
               [| Structural_descr.recursive_pack r ; Cil_datatype.Stmt.packed_descr |];
               [| Structural_descr.recursive_pack r |] ;
               [| Structural_descr.recursive_pack r |]
            |]
        in
        Structural_descr.Recursive.update r d;
        d

      let rec equal a b = match a, b with
        | Exp e, Exp f           -> Exp.equal e f
        | Lvalue v, Lvalue w     -> Lval.equal v w
        | Old t, Old s
        | Fix t, Fix s           -> equal t s
        | _, _                   -> false

      let rec compare a b = match a, b with
        | Exp e, Exp f -> Exp.compare e f
        | Lvalue v, Lvalue w -> Lval.compare v w
        | Old v, Old w -> compare v w
        | Fix s, Fix t -> compare s t
        | Lvalue _, _  -> -1
        | _, Lvalue _  -> 1
        | Exp _, _     -> -1
        | _, Exp _     -> 1
        | Old _, _     -> -1
        | _, Old _     -> 1

      let rec pretty fmt = function
        | Exp e      -> Exp.pretty fmt e
        | Lvalue v   -> Lval.pretty fmt v
        | Old t      -> Format.fprintf fmt "(Old %a)" pretty t
        | Fix t      -> Format.fprintf fmt "(Fix %a)" pretty t

      let rec hash = function
        | Exp e      -> Exp.hash e
        | Lvalue v   -> Lval.hash v
        | Old t
        | Fix t      -> hash t

      let copy c = c
    end)
end


(* --------------------------------------------------------------------------
                               Hashconsed atom
   -------------------------------------------------------------------------- *)

module Hashconsed_Atom = struct

  type tt =
    { hash : int;
      value : Unhashconsed_Atom.t;
      tag : int; }

  let rehash_ref = ref (fun _ -> assert false)

  module D = Datatype.Make_with_collections (struct
      include Datatype.Serializable_undefined
      type t = tt
      let name = "hashconsed_" ^ Unhashconsed_Atom.name

      let reprs =
        let atom = Exp Cil_datatype.Exp.dummy in
        [ { hash = Unhashconsed_Atom.hash atom ;
            value = atom ;
            tag = 0 } ]

      let structural_descr =
        Structural_descr.t_record
          [| Structural_descr.p_int;
             Unhashconsed_Atom.packed_descr;
             Structural_descr.p_int |]

      let equal = ( == )
      let compare { tag = t1 } { tag = t2 } = Datatype.Int.compare t1 t2
      let pretty fmt { value = atom } = Unhashconsed_Atom.pretty fmt atom
      let hash { hash = h } = h
      let copy c = c

      let rehash x = !rehash_ref x

    end)

  include D

  let equal_internal a b =
    if not (Datatype.Int.equal a.hash b.hash) then false
    else Unhashconsed_Atom.equal a.value b.value

  let hash_internal a = a.hash

  module Unhashconsed_AtomHashConsTbl =
    State_builder.Hashconsing_tbl
      (struct
        include D
        let hash_internal = hash_internal
        let equal_internal = equal_internal
        let initial_values = []
      end)
      (struct
        let name = "hashconstbl_" ^ Unhashconsed_Atom.name
        let dependencies = [ Ast.self ]
        let size = 137
      end)

  let self = Unhashconsed_AtomHashConsTbl.self

  let counter = ref 0

  let wrap atom =
    let tag = !counter in
    let hashed_atom =
      { hash = Unhashconsed_Atom.hash atom ;
        value = atom ;
        tag = tag }
    in
    let hashconsed_atom = Unhashconsed_AtomHashConsTbl.merge hashed_atom in
    if hashconsed_atom.tag = !counter
    then counter := succ !counter;
    hashconsed_atom

  let () = rehash_ref := fun x -> wrap x.value


  let make t = wrap (Unhashconsed_Atom.make t)
  let get { value = t } = Unhashconsed_Atom.get t

  let of_exp exp = wrap (Unhashconsed_Atom.of_exp exp)
  let of_lval lval = wrap (Unhashconsed_Atom.of_lval lval)

  let old { value = atom } = wrap (Unhashconsed_Atom.old atom)
  let fix { value = atom } = wrap (Unhashconsed_Atom.fix atom)

  let reborn { value = atom } = wrap (Unhashconsed_Atom.reborn atom)
  let release { value = atom } = wrap (Unhashconsed_Atom.release atom)

  let is_alive { value = atom } = Unhashconsed_Atom.is_alive atom
  let is_old { value = atom } = Unhashconsed_Atom.is_old atom
  let is_fix { value = atom } = Unhashconsed_Atom.is_fix atom

  let id { tag } = tag
  let debug { value = atom } = Unhashconsed_Atom.debug atom
  let pretty_debug = pretty

end


(* --------------------------------------------------------------------------
                                   Set
   -------------------------------------------------------------------------- *)

module Hptset =
  Hptset.Make
    (Hashconsed_Atom)
    (struct let v = [[]] end)
    (struct let l = [ Hashconsed_Atom.self ] end)

module Lattice_Set =
  Abstract_interp.Make_Hashconsed_Lattice_Set
    (Hashconsed_Atom)
    (Hptset)

module Lmap_Bitwise = struct
  include Lmap_bitwise.Make_bitwise
      (struct include Lattice_Set let default = bottom end)
  let () = imprecise_write_msg := "equalities locations"
end

module Atom = struct
  include Hashconsed_Atom
  module Hptset = Hptset
  module Lattice_Set = Lattice_Set
  module Lmap_Bitwise = Lmap_Bitwise
end

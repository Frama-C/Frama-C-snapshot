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
open Locations

module Make(V: sig
    include Abstract_value.Internal
    val track_variable: Cil_types.varinfo -> bool
    val name: string
    val pretty_debug: t Pretty_utils.formatter
  end) = struct

  let cache_name s = Hptmap_sig.PersistentCache ("Value." ^ V.name ^ "." ^ s)
  
  include Hptmap.Make(Base)(V)(Hptmap.Comp_unused)
      (struct let v = [] end)
      (struct let l = [Ast.self] end)

  let join =
    let cache = cache_name "join" in
    let decide _ v1 v2 =
      let r = V.join v1 v2 in
      if V.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let widen fwiden =
    let cache = Hptmap_sig.NoCache in
    let decide _ b1 b2 =
      let r = fwiden b1 b2 in
      if V.(equal top r) then None else Some r
    in      
    inter ~cache ~symmetric:false ~idempotent:true ~decide

  let is_included =
    let cache = cache_name "is_included" in
    let decide_fst _b _v1 = true (* v2 is top *) in
    let decide_snd _b _v2 = false (* v1 is top, v2 is not *) in
    let decide_both _ v1 v2 = V.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let top = empty

  (* Simultaneous computation of join and inclusion, for optimisation purposes.
     Unoptimized in our case *)
  let join_and_is_included smaller larger =
    let join = join smaller larger in
    join, equal join larger
  ;;

  type loc_for_base = Precise | Imprecise
  
  (* Checks whether the offset [o] and the size [size] corresponds to the
     tracked location for [b].
     The conditions are as follow:
     - the variable corresponding to [b] must be tracked
     - the location must assign the entire variable.
     - the type of the variable matches [typ]
  *)
  let covers_base b o size typ =
    match b with
    | Base.Var (vi, Base.Known (_, max)) -> (* "standard" varinfos only *)
      if V.track_variable vi &&
         Cil_datatype.Typ.equal typ vi.vtype &&
         Ival.is_zero o &&
         (match size with
          | Int_Base.Value size -> Integer.equal size (Integer.succ max)
          | Int_Base.Top -> false)
      then Precise
      else Imprecise
    | _ -> Imprecise


  let add loc typ v state =
    let loc = Precise_locs.imprecise_location loc in
    let size = loc.Locations.size in
    (* exact means that the location is precise and that we can perform
       a strong update. *)
    let exact = Locations.cardinal_zero_or_one loc in
    let aux_base b o state =
      match covers_base b o size typ with
      | Precise ->
        (* The location exactly matches [b], we may be able to store the
           result *)
        let v =
          if exact then
            v
          else
            (* must perform a weak update. We find the current value
               and join it with [v] *)
            try V.join v (find b state) with Not_found -> V.top
        in
        (* Store the new value if it is precise. Otherwise, drop it for
           canonicity. *)
        if V.(equal v top) then
          remove b state
        else
          add b v state
      | Imprecise -> remove b state
    in
    Locations.Location_Bits.fold_topset_ok aux_base loc.Locations.loc state

  let remove_variables vars state =
    let remove_variable state v =
      remove (Base.of_varinfo v) state
    in
    List.fold_left remove_variable state vars

  let remove loc state =
    (* We use [add] we a dummy type, which ensures that the locations
       will end up being be removed *)
    add loc Cil.voidType V.top state
  
  let find loc typ state =
    let loc = Precise_locs.imprecise_location loc in
    let size = loc.size in
    let aux_base b o r =
      (* We degenerate to Top as soon as we find an imprecise location,
         or a base which is not bound in the map. *)
      match covers_base b o size typ with
      | Precise -> begin
          try Bottom.join V.join r (`Value (find b state))
          with Not_found -> `Value V.top
        end
      | Imprecise -> `Value V.top
    in
    match Location_Bits.fold_topset_ok aux_base loc.loc `Bottom with
    | `Bottom -> V.top (* does not happen if the location is not empty *)
    | `Value v -> v
  
end

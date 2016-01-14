(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

open Locations

module Deps = 
struct

  type deps = {
    data: Zone.t;
    indirect: Zone.t;
  }

  let to_zone {data; indirect} = Zone.join data indirect

  module DatatypeFromDeps = Datatype.Make(struct
    type t = deps

    let name = "Function_Froms.Deps.from_deps"

    let hash fd =
      Zone.hash fd.data + 37 * Zone.hash fd.indirect

    let compare fd1 fd2 =
      let c = Zone.compare fd1.data fd2.data in
      if c <> 0 then c
      else Zone.compare fd1.indirect fd2.indirect

    let equal = Datatype.from_compare

    let pretty fmt d = Zone.pretty fmt (to_zone d)

    let reprs =
      List.map (fun z -> {data = z; indirect = z}) Zone.reprs

    let structural_descr =
      Structural_descr.t_record [| Zone.packed_descr; Zone.packed_descr; |]
    let rehash = Datatype.identity

    let mem_project = Datatype.never_any_project
    let varname _ = "da"

    let internal_pretty_code = Datatype.undefined
    let copy = Datatype.undefined
  end)

  include DatatypeFromDeps

  let pretty_precise fmt {data; indirect} =
    let bottom_data = Zone.is_bottom data in
    let bottom_indirect = Zone.is_bottom indirect in
    match bottom_indirect, bottom_data with
    | true, true ->
      Format.fprintf fmt "\\nothing"
    | true, false ->
      Format.fprintf fmt "direct: %a"
	Zone.pretty data
    | false, true ->
      Format.fprintf fmt "indirect: %a"
	Zone.pretty indirect
    | false, false ->
      Format.fprintf fmt "indirect: %a; direct: %a"
	Zone.pretty indirect
	Zone.pretty data

  let from_data_deps z = { data = z; indirect = Zone.bottom }
  let from_indirect_deps z = { data = Zone.bottom; indirect = z }

  let bottom = {
    data = Zone.bottom;
    indirect = Zone.bottom;
  }

  let top = {
    data = Zone.top;
    indirect = Zone.top;
  }

  let is_included fd1 fd2 =
    Zone.is_included fd1.data fd2.data &&
    Zone.is_included fd1.indirect fd2.indirect

  let join fd1 fd2 =
    if fd1 == bottom then fd2
    else if fd2 == bottom then fd1
    else {
      data = Zone.join fd1.data fd2.data;
      indirect = Zone.join fd1.indirect fd2.indirect
    }

  let _narrow fd1 fd2 = {
    data = Zone.narrow fd1.data fd2.data;
    indirect = Zone.narrow fd1.indirect fd2.indirect
  }

  let add_data_dep fd data =
    { fd with data = Zone.join fd.data data }

  let add_indirect_dep fd indirect =
    { fd with indirect = Zone.join fd.indirect indirect }

  let map f fd = {
    data = f fd.data;
    indirect = f fd.indirect;
  }

end

module DepsOrUnassigned = struct

  type deps_or_unassigned =
  | DepsBottom
  | Unassigned
  | AssignedFrom of Deps.t
  | MaybeAssignedFrom of Deps.t

  module DatatypeDeps = Datatype.Make(struct
    type t = deps_or_unassigned

    let name = "Function_Froms.Deps.deps"

    let pretty fmt = function
      | DepsBottom -> Format.pp_print_string fmt "DEPS_BOTTOM"
      | Unassigned -> Format.pp_print_string fmt "UNASSIGNED"
      | AssignedFrom fd -> Deps.pretty_precise fmt fd
      | MaybeAssignedFrom fd ->
        (* '(or UNASSIGNED)' would be a better pretty-printer, we use
           '(and SELF)' only for compatibility reasons *)
        Format.fprintf fmt "%a (and SELF)" Deps.pretty_precise fd

    let hash = function
      | DepsBottom -> 3
      | Unassigned -> 17
      | AssignedFrom fd -> 37 + 13 * Deps.hash fd
      | MaybeAssignedFrom fd -> 57 + 123 * Deps.hash fd

    let compare d1 d2 = match d1, d2 with
      | DepsBottom, DepsBottom
      | Unassigned, Unassigned -> 0
      | AssignedFrom fd1, AssignedFrom fd2
      | MaybeAssignedFrom fd1, MaybeAssignedFrom fd2 ->
        Deps.compare fd1 fd2
      | DepsBottom, (Unassigned | AssignedFrom _ | MaybeAssignedFrom _)
      | Unassigned, (AssignedFrom _ | MaybeAssignedFrom _)
      | AssignedFrom _, MaybeAssignedFrom _ ->
        -1
      | (Unassigned | AssignedFrom _ | MaybeAssignedFrom _), DepsBottom
      | (AssignedFrom _ | MaybeAssignedFrom _), Unassigned
      | MaybeAssignedFrom _, AssignedFrom _ ->
        1

    let equal = Datatype.from_compare

    let reprs = Unassigned :: List.map (fun r -> AssignedFrom r) Deps.reprs

    let structural_descr =
      let d = Deps.packed_descr in
      Structural_descr.t_sum [| [| d |]; [| d |] |]
    let rehash = Datatype.identity

    let mem_project = Datatype.never_any_project
    let varname _ = "d"

    let internal_pretty_code = Datatype.undefined
    let copy = Datatype.undefined

  end)

  let join d1 d2 = match d1, d2 with
    | DepsBottom, d | d, DepsBottom -> d
    | Unassigned, Unassigned -> Unassigned
    | Unassigned, AssignedFrom fd | AssignedFrom fd, Unassigned ->
      MaybeAssignedFrom fd
    | Unassigned, (MaybeAssignedFrom _ as d)
    | (MaybeAssignedFrom _ as d), Unassigned ->
      d
    | AssignedFrom fd1, AssignedFrom fd2 ->
      AssignedFrom (Deps.join fd1 fd2)
    | AssignedFrom fd1, MaybeAssignedFrom fd2
    | MaybeAssignedFrom fd1, AssignedFrom fd2
    | MaybeAssignedFrom fd1, MaybeAssignedFrom fd2 ->
      MaybeAssignedFrom (Deps.join fd1 fd2)

  let narrow _ _ = assert false (* not used yet *)

  let is_included d1 d2 = match d1, d2 with
    | DepsBottom, (DepsBottom | Unassigned | AssignedFrom _ |
                   MaybeAssignedFrom _)
    | Unassigned, (Unassigned | AssignedFrom _ | MaybeAssignedFrom _) ->
      true
    | MaybeAssignedFrom fd1, (AssignedFrom fd2 | MaybeAssignedFrom fd2)
    | AssignedFrom fd1, AssignedFrom fd2 ->
      Deps.is_included fd1 fd2
    | (Unassigned | AssignedFrom _ | MaybeAssignedFrom _), DepsBottom
    | (AssignedFrom _ | MaybeAssignedFrom _), Unassigned
    | AssignedFrom _, MaybeAssignedFrom _ ->
      false

  let bottom = DepsBottom
  let top = MaybeAssignedFrom Deps.top
  let default = Unassigned

  include DatatypeDeps

  let join_and_is_included d1 d2 =
    let d12 = join d1 d2 in
    (d12, equal d12 d2)

  let subst f d = match d with
    | DepsBottom -> DepsBottom
    | Unassigned -> Unassigned
    | AssignedFrom fd ->
      let fd' = f fd in
      if fd == fd' then d else AssignedFrom fd'
    | MaybeAssignedFrom fd ->
      let fd' = f fd in
      if fd == fd' then d else MaybeAssignedFrom fd'

  let pretty_precise = pretty

  let to_zone = function
    | DepsBottom | Unassigned -> Zone.bottom
    | AssignedFrom fd | MaybeAssignedFrom fd -> Deps.to_zone fd

  let to_deps = function
    | DepsBottom | Unassigned -> Deps.bottom
    | AssignedFrom fd | MaybeAssignedFrom fd -> fd

  let extract_data = function
    | DepsBottom | Unassigned -> Zone.bottom
    | AssignedFrom fd | MaybeAssignedFrom fd -> fd.Deps.data

  let extract_indirect = function
    | DepsBottom | Unassigned -> Zone.bottom
    | AssignedFrom fd | MaybeAssignedFrom fd -> fd.Deps.indirect

  let may_be_unassigned = function
    | DepsBottom | AssignedFrom _ -> false
    | Unassigned | MaybeAssignedFrom _ -> true

  let compose d1 d2 =
    match d1, d2 with
    | DepsBottom, _ | _, DepsBottom ->
      DepsBottom (* could indicate dead code. Not used in practice anyway *)
    | Unassigned, _ -> d2
    | AssignedFrom _, _ -> d1
    | MaybeAssignedFrom _, Unassigned -> d1
    | MaybeAssignedFrom d1, MaybeAssignedFrom d2 ->
      MaybeAssignedFrom (Deps.join d1 d2)
    | MaybeAssignedFrom d1, AssignedFrom d2 ->
      AssignedFrom (Deps.join d1 d2)

  (* for backwards compatibility *)
  let pretty fmt fd =
    match fd with
    | DepsBottom -> Format.pp_print_string fmt "DEPS_BOTTOM"
    | Unassigned -> Format.pp_print_string fmt "(SELF)"
    | AssignedFrom d -> Zone.pretty fmt (Deps.to_zone d)
    | MaybeAssignedFrom d ->
      Format.fprintf fmt "%a (and SELF)" Zone.pretty (Deps.to_zone d)
end

module Memory = struct 
  (** A From table is internally represented as a Lmap of [DepsOrUnassigned].
      However, the API mostly hides this fact, and exports access functions
      that take or return [Deps.t] values. This way, the user needs not
      understand the subtleties of DepsBottom/Unassigned/MaybeAssigned. *)

  include Lmap_bitwise.Make_bitwise(DepsOrUnassigned)

  let () = imprecise_write_msg := "dependencies to update"

  let pretty_skip = function
    | DepsOrUnassigned.DepsBottom -> true
    | DepsOrUnassigned.Unassigned -> true
    | DepsOrUnassigned.AssignedFrom _ -> false
    | DepsOrUnassigned.MaybeAssignedFrom _ -> false

  let pretty =
    pretty_generic_printer
      ~skip_v:pretty_skip ~pretty_v:DepsOrUnassigned.pretty ~sep:"FROM" ()

  let pretty_ind_data =
    pretty_generic_printer
      ~skip_v:pretty_skip ~pretty_v:DepsOrUnassigned.pretty_precise ~sep:"FROM"
      ()


  (** This is the auxiliary datastructure used to write the function [find].
      When we iterate over a offsetmap of value [DepsOrUnassigned], we obtain
      two things: (1) some dependencies; (2) some intervals that may have not
      been assigned, and that will appear as data dependencies (once we know
      the base we are iterating on). *)
  type find_offsm = {
    fo_itvs: Int_Intervals.t;
    fo_deps: Deps.t;
  }

  (** Once the base is known, we can obtain something of type [Deps.t] *)
  let convert_find_offsm base fp =
    let z = Zone.inject base fp.fo_itvs in
    Deps.add_data_dep fp.fo_deps z

  let empty_find_offsm = {
    fo_itvs = Int_Intervals.bottom;
    fo_deps = Deps.bottom;
  }

  let join_find_offsm fp1 fp2 =
    if fp1 == empty_find_offsm then fp2
    else if fp2 == empty_find_offsm then fp1
    else {
      fo_itvs = Int_Intervals.join fp1.fo_itvs fp2.fo_itvs;
      fo_deps = Deps.join fp1.fo_deps fp2.fo_deps;
    }

  (** Auxiliary function that collects the dependencies on some intervals of
      an offsetmap. *)
  let find_precise_offsetmap : Int_Intervals.t -> LOffset.t -> find_offsm =
    let cache = Hptmap_sig.PersistentCache "Function_Froms.find_precise" in
    let aux_find_offsm ib ie v =
      (* If the interval can be unassigned, we collect its bound. We also
         return the dependencies stored at this interval. *)
      let default, v = match v with
        | DepsOrUnassigned.DepsBottom -> false, Deps.bottom
        | DepsOrUnassigned.Unassigned -> true, Deps.bottom
        | DepsOrUnassigned.MaybeAssignedFrom v -> true, v
        | DepsOrUnassigned.AssignedFrom v -> false, v
      in
      { fo_itvs =
          if default
          then Int_Intervals.inject_bounds ib ie
          else Int_Intervals.bottom;
        fo_deps = v }
    in
    (* Partial application is important *)
    LOffset.fold_join_itvs
      ~cache aux_find_offsm join_find_offsm empty_find_offsm

  (** Collecting dependencies on a given zone. *)
  let find_precise : t -> Zone.t -> Deps.t =
    let both = find_precise_offsetmap in
    let conv = convert_find_offsm in
    (* We are querying a zone for which no dependency is stored. Hence, every
       base is implicitely bound to [Unassigned]. *)
    let empty_map z = Deps.from_data_deps z in
    let join = Deps.join in
    let empty = Deps.bottom in
    (* Partial application is important *)
    let f = fold_join_zone ~both ~conv ~empty_map ~join ~empty in
    fun m z ->
      match m with
      | Top -> Deps.top
      | Bottom -> Deps.bottom
      | Map m -> f z m

  let find z m =
    Deps.to_zone (find_precise z m)

  let add_binding_precise_loc ~exact m loc v =
    let aux_one_loc loc m =
      let loc = Locations.valid_part ~for_writing:true loc in
      add_binding_loc
        ~reducing:false ~exact m loc (DepsOrUnassigned.AssignedFrom v)
    in
    Precise_locs.fold aux_one_loc loc m

  let bind_var vi v m =
    let z = Locations.zone_of_varinfo vi in
    add_binding ~reducing:true ~exact:true m z (DepsOrUnassigned.AssignedFrom v)

  let unbind_var vi m =
    remove_base (Base.of_varinfo vi) m

  let add_binding ~exact m z v =
    add_binding ~reducing:false ~exact m z (DepsOrUnassigned.AssignedFrom v)

  let add_binding_loc ~exact m loc v =
    add_binding_loc
      ~reducing:false ~exact m loc (DepsOrUnassigned.AssignedFrom v)

  let is_unassigned m =
    let unassigned v = DepsOrUnassigned.(equal v Unassigned) in
    LOffset.is_single_interval ~f:unassigned m

  (* Unassigned is a neutral value for compose, on both sides *)
  let decide_compose m1 m2 =
    if m1 == m2 || is_unassigned m1 then LOffset.ReturnRight
    else if is_unassigned m2 then LOffset.ReturnLeft
    else LOffset.Recurse

  let compose_map =
    let cache = Hptmap_sig.PersistentCache "Function_Froms.Memory.compose" in
    (* Partial application is important because of the cache. Idempotent,
       because [compose x x] is always equal to [x]. *)
    map2 ~cache ~symmetric:false ~idempotent:true ~empty_neutral:true
      decide_compose DepsOrUnassigned.compose

  let compose m1 m2 = match m1, m2 with
    | Top, _ | _, Top -> Top
    | Map m1, Map m2 -> Map (compose_map m1 m2)
    | Bottom, (Map _ | Bottom) | Map _, Bottom -> Bottom

  (** Auxiliary function that substitutes the data right-hand part of a
      dependency by a pre-existing From state. The returned result is a Deps.t:
      the data part will be the data part of the complete result, the indirect
      part will be added to the indirect part of the final result. *)
  (* This function iterates simultaneously on a From memory, and on a zone.
     It is cached. The definitions below are used to call the function that
     does the recursive descent. *)
  let substitute_data_deps =
    (* Nothing left to substitute, return z unchanged *)
    let empty_right z = Deps.from_data_deps z in
    (* Zone to subtitute is empty *)
    let empty_left _ = Deps.bottom in
    (* [b] is in the zone and substituted. Rewrite appropriately *)
    let both b itvs offsm =
      let fp = find_precise_offsetmap itvs offsm in
      convert_find_offsm b fp
    in
    let join = Deps.join in
    let empty = Deps.bottom in
    let cache = Hptmap_sig.PersistentCache "From_compute.subst_data" in
    let f_map =
      Zone.fold2_join_heterogeneous
        ~cache ~empty_left ~empty_right ~both ~join ~empty
    in
    fun call_site_froms z ->
      match call_site_froms with
      | Bottom -> Deps.bottom
      | Top -> Deps.top
      | Map m ->
        try f_map z (shape m)
        with Zone.Error_Top -> Deps.top

  (** Auxiliary function that substitutes the indirect right-hand part of a
      dependency by a pre-existing From state. The returned result is a zone,
      which will be added to the indirect part of the final result. *)
  let substitute_indirect_deps =
    (* Nothing left to substitute, z is directly an indirect dependency *)
    let empty_right z = z in
    (* Zone to subtitute is empty *)
    let empty_left _ = Zone.bottom in
    let both b itvs offsm =
      (* Both the found data and indirect dependencies are computed for indirect
         dependencies: merge to a single zone *)
      let fp = find_precise_offsetmap itvs offsm in
      Deps.to_zone (convert_find_offsm b fp)
    in
    let join = Zone.join in
    let empty = Zone.bottom in
    let cache = Hptmap_sig.PersistentCache "From_compute.subst_indirect" in
    let f_map =
      Zone.fold2_join_heterogeneous
        ~cache ~empty_left ~empty_right ~both ~join ~empty
    in
    fun call_site_froms z ->
      match call_site_froms with
      | Bottom -> Zone.bottom
      | Top -> Zone.top
      | Map m ->
        try f_map z (shape m)
        with Zone.Error_Top -> Zone.top

  let substitute call_site_froms deps =
    let open Deps in
    let { data; indirect } = deps in
    (* depending directly on an indirect dependency -> indirect,
     depending indirectly on a direct dependency  -> indirect *)
    let dirdeps = substitute_data_deps call_site_froms data in
    let inddeps = substitute_indirect_deps call_site_froms indirect in
    let dir = dirdeps.data in
    let ind = Zone.(join dirdeps.indirect inddeps) in
    { data = dir; indirect = ind }


  type return = Deps.t

  let default_return = Deps.bottom

  let top_return = Deps.top

  let add_to_return ?start:(_start=0) ~size:_size ?(m=default_return) v =
    Deps.join m v
(*
    let start = Ival.of_int start in
    let itvs = Int_Intervals.from_ival_size start size in
    LOffset.add_iset ~exact:true itvs (DepsOrUnassigned.AssignedFrom v) m
*)

  let top_return_size size =
    add_to_return ~size Deps.top

  let join_return = Deps.join

  let collapse_return x = x

end

type froms =
    { deps_return : Memory.return;
      deps_table : Memory.t }

let top = {
  deps_return = Memory.top_return;
  deps_table = Memory.top;
}

let join x y =
  { deps_return = Memory.join_return x.deps_return y.deps_return ;
    deps_table = Memory.join x.deps_table y.deps_table }

let outputs { deps_table = t } =
  match t with
  | Memory.Top -> Locations.Zone.top
  | Memory.Bottom -> Locations.Zone.bottom
  | Memory.Map(m) ->
    Memory.fold
      (fun z v acc ->
	let open DepsOrUnassigned in
	match v with
	| DepsBottom | Unassigned -> acc
	| AssignedFrom _ | MaybeAssignedFrom _ -> Locations.Zone.join z acc)
      m Locations.Zone.bottom

let inputs ?(include_self=false) t =
  let aux b offm acc =
    Memory.LOffset.fold
      (fun itvs deps acc ->
        let z = DepsOrUnassigned.to_zone deps in
        let self = DepsOrUnassigned.may_be_unassigned deps in
        let acc = Zone.join z acc in
        match include_self, self, b with
          | true, true, Some b ->
            Zone.join acc (Zone.inject b itvs)
          | _ -> acc
      )
      offm
      acc
  in
  let return = Deps.to_zone t.deps_return in
  let aux_table b = aux (Some b) in
  match t.deps_table with
  | Memory.Top -> Zone.top
  | Memory.Bottom -> Zone.bottom
  | Memory.Map m -> Memory.fold_base aux_table m return


let pretty fmt { deps_return = r ; deps_table = t } =
  Format.fprintf fmt "%a@\n\\result FROM @[%a@]@\n"
    Memory.pretty t
    Deps.pretty r

(** same as pretty, but uses the type of the function to output more
    precise information.
    @raise Error if the given type is not a function type
 *)
let pretty_with_type ~indirect typ fmt { deps_return = r; deps_table = t } =
  let (rt_typ,_,_,_) = Cil.splitFunctionType typ in
  if Memory.is_bottom t
  then Format.fprintf fmt
    "@[NON TERMINATING - NO EFFECTS@]"
  else
    let map_pretty =
      if indirect 
      then Memory.pretty_ind_data
      else Memory.pretty 
    in
    if Cil.isVoidType rt_typ 
    then begin
      if Memory.is_empty t
      then Format.fprintf fmt "@[NO EFFECTS@]"
      else map_pretty fmt t
    end
    else
      let pp_space fmt =
        if not (Memory.is_empty t) then
          Format.fprintf fmt "@ "
      in
      Format.fprintf fmt "@[<v>%a%t@[\\result FROM @[%a@]@]@]"
        map_pretty t pp_space Deps.pretty r

let pretty_with_type_indirect = pretty_with_type ~indirect:true
let pretty_with_type = pretty_with_type ~indirect:false

let hash { deps_return = dr ; deps_table = dt } =
  Memory.hash dt + 197 * Deps.hash dr

let equal
    { deps_return = dr ; deps_table = dt }
    { deps_return = dr' ; deps_table = dt' } =
  Memory.equal dt dt'&& Deps.equal dr dr'

include Datatype.Make
    (struct
      type t = froms
      let reprs =
        List.fold_left
          (fun acc o ->
            List.fold_left
              (fun acc m -> { deps_return = o; deps_table = m } :: acc)
              acc
              Memory.reprs)
          []
          Deps.reprs
      let structural_descr =
        Structural_descr.t_record
          [| Deps.packed_descr;
             Memory.packed_descr |]
       let name = "Function_Froms"
       let hash = hash
       let compare = Datatype.undefined
       let equal = equal
       let pretty = pretty
       let internal_pretty_code = Datatype.undefined
       let rehash = Datatype.identity
       let copy = Datatype.undefined
       let varname = Datatype.undefined
       let mem_project = Datatype.never_any_project
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

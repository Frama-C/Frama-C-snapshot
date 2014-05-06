(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

type 'a t =
    { equal: 'a -> 'a -> bool;
      compare: 'a -> 'a -> int;
      hash: 'a -> int;
      copy: 'a -> 'a;
      internal_pretty_code: Type.precedence -> Format.formatter -> 'a -> unit;
      pretty_code: Format.formatter -> 'a -> unit;
      pretty: Format.formatter -> 'a -> unit;
      varname: 'a -> string;
      mem_project: (Project_skeleton.t -> bool) -> 'a -> bool }

type 'a info = 'a t

module type Ty = sig
  type t
  val ty: t Type.t
end

module type S_no_copy = sig
  include Ty
  val name: string
  val descr: t Descr.t
  val packed_descr: Structural_descr.pack
  val reprs: t list
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val pretty_code: Format.formatter -> t -> unit
  val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
  val pretty: Format.formatter -> t -> unit
  val varname: t -> string
  val mem_project: (Project_skeleton.t -> bool) -> t -> bool
end

module type S = sig
  include S_no_copy
  val copy: t -> t
end

(* ********************************************************************** *)
(** {2 Getters from a type value} *)
(* ********************************************************************** *)

module Infos = Type.Ty_tbl(struct type 'a t = 'a info end)

let info_tbl = Infos.create 97

let internal_info s ty =
  try Infos.find info_tbl ty
  with Not_found ->
    Format.eprintf "Internal Datatype.info error: no %s for %S@."
      s (Type.name ty);
    assert false

let equal ty = (internal_info "equal" ty).equal
let compare ty = (internal_info "compare" ty).compare
let hash ty = (internal_info "hash" ty).hash
let copy ty = (internal_info "copy" ty).copy
let internal_pretty_code ty = 
  (internal_info "internal_pretty_code" ty).internal_pretty_code
let pretty_code ty = (internal_info "pretty_code" ty).pretty_code
let pretty ty = (internal_info "pretty" ty).pretty
let varname ty = (internal_info "varname" ty).varname
let mem_project ty = (internal_info "mem_project" ty).mem_project

let info ty = internal_info "info" ty

(* ********************************************************************** *)
(** {2 Easy builders} *)
(* ********************************************************************** *)

let undefined _ = assert false
let identity x = x
let never_any_project _ _ = false
let from_compare _ _ = assert false
let from_pretty_code _ _ = assert false
let pp_fail _ _ _ = assert false

module type Undefined = sig
  val structural_descr: Structural_descr.t
  val equal: 'a -> 'a -> bool
  val compare: 'a -> 'a -> int
  val hash: 'a -> int
  val rehash: 'a -> 'a
  val copy: 'a -> 'a
  val internal_pretty_code: Type.precedence -> Format.formatter -> 'a -> unit
  val pretty: Format.formatter -> 'a -> unit
  val varname: 'a -> string
  val mem_project: (Project_skeleton.t -> bool) -> 'a -> bool
end

module Partial_undefined = struct
  let equal = undefined
  let compare = undefined
  let hash = undefined
  let copy = undefined
  let internal_pretty_code = undefined
  let pretty = undefined
  let varname = undefined
  let mem_project = undefined
end

module Undefined = struct
  include Partial_undefined
  let structural_descr = Structural_descr.t_unknown
  let rehash = undefined
end

module Serializable_undefined = struct
  include Partial_undefined
  let structural_descr = Structural_descr.t_abstract
  let rehash = identity
  let mem_project = never_any_project
end

(* ********************************************************************** *)
(** {2 Generic builders} *)
(* ********************************************************************** *)

let valid_varname s =
  let r = Str.regexp "[^A-Za-z0-9_]+" in
  let s = Str.global_replace r "__" s in
  String.uncapitalize s

let check f fname tname fstr =
  assert
    (if f == undefined && Type.may_use_obj () then begin
      Format.printf "@[Preliminary datatype check failed.@\n\
Value `%s' of type %s is required for building %s.@]@."
        fname tname fstr;
      false
    end else
        true)

module Build
  (T: sig
    type t
    val ty: t Type.t
    val reprs: t list
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val hash: t -> int
    val rehash: t -> t
    val copy: t -> t
    val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
    val pretty: Format.formatter -> t -> unit
    val varname: t -> string
    val mem_project: (Project_skeleton.t -> bool) -> t -> bool
  end) =
struct

  let name = Type.name T.ty
(*  let () = Format.printf "datatype %S@." name*)

  let equal =
    if T.equal == from_compare then (fun x y -> T.compare x y = 0)
    else T.equal

  let compare = T.compare
  let hash = T.hash
  let rehash = T.rehash
  let copy = T.copy
  let internal_pretty_code = T.internal_pretty_code

  let pretty_code =
    if T.internal_pretty_code == undefined then undefined
    else if T.internal_pretty_code == pp_fail then pp_fail Type.NoPar
    else fun fmt x ->
      (*    Format.printf "pretty code %s@." name;*)
      let buf = Buffer.create 17 in
      let buffmt = Format.formatter_of_buffer buf in
      Format.fprintf buffmt "%a@?" (T.internal_pretty_code Type.NoPar) x;
      let f =
        Scanf.format_from_string (String.escaped (Buffer.contents buf)) ""
      in
      Format.fprintf fmt f

  let pretty =
    if T.pretty == from_pretty_code then pretty_code
    else T.pretty

  let varname =
    if T.varname == undefined then undefined
    else fun x -> valid_varname (T.varname x)

  let mem_project = T.mem_project

  let info =
    { equal = equal;
      compare = compare;
      hash = hash;
      copy = copy;
      internal_pretty_code = internal_pretty_code;
      pretty_code = pretty_code;
      pretty = pretty;
      varname = varname;
      mem_project = mem_project }

  let () = Infos.add info_tbl T.ty info

  let mk_full_descr d =
    let descr =
      if rehash == undefined then
        if Descr.is_unmarshable d then Descr.unmarshable
        else begin
          check rehash "rehash" name "descriptor";
          assert false
        end
      else
        if rehash == identity then d
        else
	  if Type.may_use_obj () then begin
            if Descr.is_unmarshable d then begin
              check undefined "structural_descr" name "descriptor";
              assert false
            end;
	    Descr.transform d rehash
	  end else
	    Descr.unmarshable
    in
    descr, Descr.pack descr

  let descr, packed_descr = mk_full_descr (Descr.of_type T.ty)
  let reprs = T.reprs (* [Type.reprs] is not usable in the "no-obj" mode *)

end

module type Make_input = sig
  type t
  val name: string
  val rehash: t -> t
  val structural_descr: Structural_descr.t
  val reprs: t list
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val copy: t -> t
  val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
  val pretty: Format.formatter -> t -> unit
  val varname: t -> string
  val mem_project: (Project_skeleton.t -> bool) -> t -> bool
end

let is_module_name s =
  let l = Str.split (Str.regexp "\\.") s in
  List.for_all(fun x -> String.length x > 0 && x.[0] = Char.uppercase x.[0]) l

module Make(X: Make_input) = struct

  module T = struct
    include X
    let name = if is_module_name X.name then X.name ^ ".t" else X.name
    let ml_name = if is_module_name X.name then Some (X.name ^ ".ty") else None
    let ty = Type.register ~name ~ml_name X.structural_descr X.reprs
  end

  include T
  include Build(T)

end

module type Set = sig
  include FCSet.S
  include S with type t := t
end

module type Map = sig
  include FCMap.S
  module Key: S with type t = key
  module Make(Data: S) : S with type t = Data.t t
end

module type Hashtbl_with_descr = sig
  include FCHashtbl.S
  val structural_descr: Structural_descr.t -> Structural_descr.t
end

module type Hashtbl = sig
  include Hashtbl_with_descr

  val make_type: 'a Type.t -> 'a t Type.t
  (** @since Fluorine-20130401 *)

  val memo: 'a t -> key -> (key -> 'a) -> 'a
  module Key: S with type t = key
  module Make(Data: S) : S with type t = Data.t t
end

module type S_with_collections = sig
  include S
  module Set: Set with type elt = t
  module Map: Map with type key = t
  module Hashtbl: Hashtbl with type key = t
end

(* ****************************************************************************)
(** {2 Polymorphic signature} *)
(* ****************************************************************************)

module type Polymorphic = sig
  include Type.Polymorphic
  module Make(T: S) : S with type t = T.t poly
end

(* local argument of below functors: not visible from outside *)
let poly_name_ref = ref "" 

(* ****************************************************************************)
(** {2 Polymorphic2 } *)
(* ****************************************************************************)

module type Polymorphic2 = sig
  include Type.Polymorphic2
  module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly
end

module type Polymorphic2_input = sig
  include Type.Polymorphic2_input
  val mk_equal:
    ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t ->
    bool
  val mk_compare:
    ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val mk_hash: ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
  val map: ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
  val mk_internal_pretty_code:
    (Type.precedence -> Format.formatter -> 'a -> unit) ->
    (Type.precedence -> Format.formatter -> 'b -> unit) ->
    Type.precedence -> Format.formatter -> ('a, 'b) t -> unit
  val mk_pretty:
    (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) t -> unit
  val mk_varname: ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
  val mk_mem_project:
    ((Project_skeleton.t -> bool) -> 'a -> bool) ->
    ((Project_skeleton.t -> bool) -> 'b -> bool) ->
    (Project_skeleton.t -> bool) -> ('a, 'b) t -> bool
end

module Polymorphic2(P: Polymorphic2_input) = struct

  include Type.Polymorphic2(P)

  (* cannot declare [name] locally in instantiate since it prevents OCaml
     generalization *)
  let name = !poly_name_ref 
  let instantiate ty1 ty2 =
    let res, first = instantiate ty1 ty2 in
    if first && name <> "" then begin
      let ml_name = 
	Type.sfprintf
	  "Datatype.%s %a %a" 
	  name
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty1
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty2
      in
      Type.set_ml_name res (Some ml_name)
    end;
    res, first

  let () = poly_name_ref := ""

  module Make(T1: S)(T2: S) = struct

    module T = struct
      type t = (T1.t, T2.t) P.t
      let ty, _is_new = instantiate T1.ty T2.ty
    end

    include T
    include Build
      (struct
        include T
	let reprs = if Type.may_use_obj () then Type.reprs ty else []
        let build mk f1 f2 =
          if mk == undefined || f1 == undefined || f2 == undefined then
            undefined
          else
            mk f1 f2
        let compare = build P.mk_compare T1.compare T2.compare
        let equal = build P.mk_equal T1.equal T2.equal
        let hash = build P.mk_hash T1.hash T2.hash
        let rehash = identity
        let copy =
          let mk f1 f2 =
            if P.map == undefined then undefined
            else
            (* [JS 2011/05/31] No optimisation for the special case of identity,
               since we really want to perform a DEEP copy. *)
            (*if f1 == identity && f2 == identity then identity
            else*) P.map f1 f2
          in
          build mk T1.copy T2.copy
        let internal_pretty_code =
          let mk f1 f2 =
            if f1 == pp_fail || f2 == pp_fail then pp_fail
            else fun p fmt x -> P.mk_internal_pretty_code f1 f2 p fmt x
          in
          build mk T1.internal_pretty_code T2.internal_pretty_code
        let pretty = build P.mk_pretty T1.pretty T2.pretty
        let varname = build P.mk_varname T1.varname T2.varname
        let mem_project =
          let mk f1 f2 =
            if P.mk_mem_project == undefined then undefined
            else if f1 == never_any_project && f2 == never_any_project then
              never_any_project
            else
              P.mk_mem_project f1 f2
          in
          build mk T1.mem_project T2.mem_project
       end)

    let descr, packed_descr =
      mk_full_descr
        (Descr.of_structural
           ty
           (P.structural_descr (Descr.str T1.descr) (Descr.str T2.descr)))

  end

end

(* ****************************************************************************)
(** {2 Polymorphic3 } *)
(* ****************************************************************************)

module type Polymorphic3 = sig
  include Type.Polymorphic3
  module Make(T1:S)(T2:S)(T3:S) : S with type t = (T1.t, T2.t, T3.t) poly
end

module Polymorphic3
  (P: sig
    include Type.Polymorphic3_input
    val mk_equal:
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('c -> 'c -> bool) ->
      ('a, 'b, 'c) t -> ('a, 'b, 'c) t ->
      bool
    val mk_compare:
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('c -> 'c -> int) ->
      ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
    val mk_hash: 
      ('a -> int) -> ('b -> int) -> ('c -> int) -> ('a, 'b, 'c) t -> int
    val map: 
      ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      (Type.precedence -> Format.formatter -> 'b -> unit) ->
      (Type.precedence -> Format.formatter -> 'c -> unit) ->
      Type.precedence -> Format.formatter -> ('a, 'b, 'c) t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> 
      (Format.formatter -> 'b -> unit) ->
      (Format.formatter -> 'c -> unit) ->
      Format.formatter -> ('a, 'b, 'c) t -> unit
    val mk_varname: 
      ('a -> string) -> ('b -> string) -> ('c -> string) -> 
      ('a, 'b, 'c) t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      ((Project_skeleton.t -> bool) -> 'b -> bool) ->
      ((Project_skeleton.t -> bool) -> 'c -> bool) ->
      (Project_skeleton.t -> bool) -> ('a, 'b, 'c) t -> bool
  end) =
struct

  include Type.Polymorphic3(P)

  (* cannot declare [name] locally in instantiate since it prevents OCaml
     generalization *)
  let name = !poly_name_ref 
  let instantiate ty1 ty2 ty3 =
    let res, first = instantiate ty1 ty2 ty3 in
    if first && name <> "" then begin
      let ml_name = 
	Type.sfprintf
	  "Datatype.%s %a %a %a" 
	  name
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty1
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty2
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty3
      in
      Type.set_ml_name res (Some ml_name)
    end;
    res, first

  let () = poly_name_ref := ""

  module Make(T1: S)(T2: S)(T3: S) = struct

    module T = struct
      type t = (T1.t, T2.t, T3.t) P.t
      let ty, _is_new = instantiate T1.ty T2.ty T3.ty
    end

    include T
    include Build
      (struct
        include T
	let reprs = if Type.may_use_obj () then Type.reprs ty else []
        let build mk f1 f2 f3 =
          if mk == undefined || f1 == undefined || f2 == undefined || 
	    f3 == undefined
	  then
            undefined
          else
            mk f1 f2 f3
        let compare = build P.mk_compare T1.compare T2.compare T3.compare
        let equal = build P.mk_equal T1.equal T2.equal T3.equal
        let hash = build P.mk_hash T1.hash T2.hash T3.hash
        let rehash = identity
        let copy =
          let mk f1 f2 f3 =
            if P.map == undefined then undefined
            else
            (* [JS 2011/05/31] No optimisation for the special case of identity,
               since we really want to perform a DEEP copy. *)
            (*if f1 == identity && f2 == identity then identity
              else*) P.map f1 f2 f3
          in
          build mk T1.copy T2.copy T3.copy
        let internal_pretty_code =
          let mk f1 f2 f3 =
            if f1 == pp_fail || f2 == pp_fail || f3 == pp_fail then pp_fail
            else fun p fmt x -> P.mk_internal_pretty_code f1 f2 f3 p fmt x
          in
          build mk 
	    T1.internal_pretty_code 
	    T2.internal_pretty_code
	    T3.internal_pretty_code
        let pretty = build P.mk_pretty T1.pretty T2.pretty T3.pretty
        let varname = build P.mk_varname T1.varname T2.varname T3.varname
        let mem_project =
          let mk f1 f2 f3 =
            if P.mk_mem_project == undefined then undefined
            else if f1 == never_any_project && f2 == never_any_project 
		&& f3 == never_any_project 
	    then
              never_any_project
            else
              P.mk_mem_project f1 f2 f3
          in
          build mk T1.mem_project T2.mem_project T3.mem_project
       end)

    let descr, packed_descr =
      mk_full_descr
        (Descr.of_structural
           ty
           (P.structural_descr
	      (Descr.str T1.descr) 
	      (Descr.str T2.descr)
	      (Descr.str T3.descr)))

  end

end

(* ****************************************************************************)
(** {2 Polymorphic4 } *)
(* ****************************************************************************)

module type Polymorphic4 = sig
  include Type.Polymorphic4
  module Make(T1:S)(T2:S)(T3:S)(T4:S) 
    : S with type t = (T1.t, T2.t, T3.t, T4.t) poly
end

module Polymorphic4
  (P: sig
    include Type.Polymorphic4_input
    val mk_equal:
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 
      ('c -> 'c -> bool) -> ('d -> 'd -> bool) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t ->
      bool
    val mk_compare:
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> 
      ('c -> 'c -> int) -> ('d -> 'd -> int) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> int
    val mk_hash: 
      ('a -> int) -> ('b -> int) -> ('c -> int) -> ('d -> int) -> 
      ('a, 'b, 'c, 'd) t -> int
    val map: 
      ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) -> ('d -> 'd) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      (Type.precedence -> Format.formatter -> 'b -> unit) ->
      (Type.precedence -> Format.formatter -> 'c -> unit) ->
      (Type.precedence -> Format.formatter -> 'd -> unit) ->
      Type.precedence -> Format.formatter -> ('a, 'b, 'c, 'd) t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> 
      (Format.formatter -> 'b -> unit) ->
      (Format.formatter -> 'c -> unit) ->
      (Format.formatter -> 'd -> unit) ->
      Format.formatter -> ('a, 'b, 'c, 'd) t -> unit
    val mk_varname: 
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> 
      ('a, 'b, 'c, 'd) t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      ((Project_skeleton.t -> bool) -> 'b -> bool) ->
      ((Project_skeleton.t -> bool) -> 'c -> bool) ->
      ((Project_skeleton.t -> bool) -> 'd -> bool) ->
      (Project_skeleton.t -> bool) -> ('a, 'b, 'c, 'd) t -> bool
  end) =
struct

  include Type.Polymorphic4(P)

  (* cannot declare [name] locally in instantiate since it prevents OCaml
     generalization *)
  let name = !poly_name_ref 
  let instantiate ty1 ty2 ty3 ty4 =
    let res, first = instantiate ty1 ty2 ty3 ty4 in
    if first && name <> "" then begin
      let ml_name = 
	Type.sfprintf
	  "Datatype.%s %a %a %a %a" 
	  name
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty1
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty2
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty3
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty4
      in
      Type.set_ml_name res (Some ml_name)
    end;
    res, first

  let () = poly_name_ref := ""

  module Make(T1: S)(T2: S)(T3: S)(T4: S) = struct

    module T = struct
      type t = (T1.t, T2.t, T3.t, T4.t) P.t
      let ty, _is_new = instantiate T1.ty T2.ty T3.ty T4.ty
    end

    include T
    include Build
      (struct
        include T
	let reprs = if Type.may_use_obj () then Type.reprs ty else []	    
        let build mk f1 f2 f3 f4 =
          if mk == undefined || f1 == undefined || f2 == undefined || 
	    f3 == undefined || f4 == undefined
	  then
            undefined
          else
            mk f1 f2 f3 f4
        let compare = 
	  build P.mk_compare T1.compare T2.compare T3.compare T4.compare
        let equal = build P.mk_equal T1.equal T2.equal T3.equal T4.equal
        let hash = build P.mk_hash T1.hash T2.hash T3.hash T4.hash
        let rehash = identity
        let copy =
          let mk f1 f2 f3 f4 =
            if P.map == undefined then undefined
            else
            (* [JS 2011/05/31] No optimisation for the special case of identity,
               since we really want to perform a DEEP copy. *)
            (*if f1 == identity && f2 == identity then identity
              else*) P.map f1 f2 f3 f4
          in
          build mk T1.copy T2.copy T3.copy T4.copy
        let internal_pretty_code =
          let mk f1 f2 f3 f4 =
            if f1 == pp_fail || f2 == pp_fail || f3 == pp_fail || f4 == pp_fail
	    then pp_fail
            else fun p fmt x -> P.mk_internal_pretty_code f1 f2 f3 f4 p fmt x
          in
          build mk 
	    T1.internal_pretty_code 
	    T2.internal_pretty_code
	    T3.internal_pretty_code
	    T4.internal_pretty_code
        let pretty = build P.mk_pretty T1.pretty T2.pretty T3.pretty T4.pretty
        let varname = 
	  build P.mk_varname T1.varname T2.varname T3.varname T4.varname
        let mem_project =
          let mk f1 f2 f3 f4 =
            if P.mk_mem_project == undefined then undefined
            else if f1 == never_any_project && f2 == never_any_project 
		&& f3 == never_any_project && f4 == never_any_project
	    then
              never_any_project
            else
              P.mk_mem_project f1 f2 f3 f4
          in
          build mk T1.mem_project T2.mem_project T3.mem_project T4.mem_project
       end)

    let descr, packed_descr =
      mk_full_descr
        (Descr.of_structural
           ty
           (P.structural_descr
	      (Descr.str T1.descr) 
	      (Descr.str T2.descr)
	      (Descr.str T3.descr)
	      (Descr.str T4.descr)))

  end

end

(* ****************************************************************************)
(** {3 Pair} *)
(* ****************************************************************************)

let () = poly_name_ref := "pair"

module Pair_arg = struct
  type ('a, 'b) t = 'a * 'b
  let module_name = "Datatype.Pair"
  let reprs a b = [ a, b ]
  let structural_descr d1 d2 =
    Structural_descr.t_tuple
      [| Structural_descr.pack d1; Structural_descr.pack d2 |]
  let mk_equal f1 f2 (x1,x2) (y1,y2) = f1 x1 y1 && f2 x2 y2
  let mk_compare f1 f2 (x1,x2 as x) (y1,y2 as y) =
    if x == y then 0 else let n = f1 x1 y1 in if n = 0 then f2 x2 y2 else n
  let mk_hash f1 f2 (x1,x2) = f1 x1 + 1351 * f2 x2
  let map f1 f2 (x1,x2) = f1 x1, f2 x2
  let mk_internal_pretty_code f1 f2 p fmt (x1, x2) =
    let pp fmt =
      Format.fprintf
        fmt "@[<hv 2>%a,@;%a@]" (f1 Type.Tuple) x1 (f2 Type.Tuple) x2
    in
    Type.par p Type.Tuple fmt pp
  let mk_pretty f1 f2 fmt p =
    Format.fprintf fmt "@[(%a)@]"
      (mk_internal_pretty_code (fun _ -> f1) (fun _ -> f2) Type.Basic) p
  let mk_varname = undefined
  let mk_mem_project mem1 mem2 f (x1, x2) = mem1 f x1 && mem2 f x2
end

module rec Pair_name: sig val name: 'a Type.t -> 'b Type.t -> string end =
struct
  let name ty1 ty2 =
    let arg ty =
      Type.par_ty_name
        (fun ty ->
          Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty)
        ty
    in
    arg ty1 ^ " * " ^ arg ty2
end

and Poly_pair : sig
  include Type.Polymorphic2 with type ('a,'b) poly = 'a * 'b
  module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly
end =
  struct
  (* Split the functor argument in 2 modules such that ocaml is able to safely
     evaluate the recursive modules *)
    include Polymorphic2(struct include Pair_arg include Pair_name end)
  end

module Pair = Poly_pair.Make

let pair (type typ1) (type typ2) (ty1: typ1 Type.t) (ty2: typ2 Type.t) =
  let module Make(X: sig type t val ty: t Type.t end) = struct
      type t = X.t
      let ty = X.ty
      let name = Type.name X.ty
      let descr = Descr.of_type X.ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs X.ty
      let equal = equal X.ty
      let compare = compare X.ty
      let hash = hash X.ty
      let copy = copy X.ty
      let internal_pretty_code = internal_pretty_code X.ty
      let pretty_code = pretty_code X.ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project X.ty
  end
  in
  let module L = Pair
	(Make(struct type t = typ1 let ty = ty1 end))
	(Make(struct type t = typ2 let ty = ty2 end))
  in
  L.ty

(* ****************************************************************************)
(** {3 Function} *)
(* ****************************************************************************)

module Function
  (T1: sig include Ty val label: (string * (unit -> t) option) option end)
  (T2: Ty) =
struct
  module T = struct
    type t = T1.t -> T2.t
    let ty, _is_new = Type.Function.instantiate ?label:T1.label T1.ty T2.ty
    let compare = undefined
    let equal = (==)
    let hash = undefined
    let rehash = undefined
    let copy = undefined
    let internal_pretty_code = undefined
    let pretty = undefined
    let varname _ = "f"
    let mem_project = never_any_project
    let reprs = 
      if Type.may_use_obj () then Type.reprs ty else [ fun _ -> assert false ]
  end
  include T
  include Build(T)
end

let func  (type typ1) (type typ2) ?label (ty1: typ1 Type.t) (ty2: typ2 Type.t) =
  let module L = Function
	(struct type t = typ1 let ty = ty1 let label = label end)
	(struct type t = typ2 let ty = ty2 end)
  in
  L.ty

let optlabel_func lab dft = func ~label:(lab, Some dft)

let func2 ?label1 ty1 ?label2 ty2 ty_ret =
  func ?label:label1 ty1 (func ?label:label2 ty2 ty_ret)

let func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 ty_ret =
  func2 ?label1 ty1 ?label2 ty2 (func ?label:label3 ty3 ty_ret)

let func4 ?label1 ty1 ?label2 ty2 ?label3 ty3 ?label4 ty4 ty_ret =
  func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 (func ?label:label4 ty4 ty_ret)

let is_function_or_pair ty =
  Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty

(* ****************************************************************************)
(** {2 Polymorphic generator} *)
(* ****************************************************************************)

module type Polymorphic_input = sig
  include Type.Polymorphic_input
  val mk_equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val mk_compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val mk_hash: ('a -> int) -> 'a t -> int
  val map: ('a -> 'a) -> 'a t -> 'a t
  val mk_internal_pretty_code:
    (Type.precedence -> Format.formatter -> 'a -> unit) ->
    Type.precedence -> Format.formatter -> 'a t -> unit
  val mk_pretty:
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val mk_varname: ('a -> string) -> 'a t -> string
  val mk_mem_project:
    ((Project_skeleton.t -> bool) -> 'a -> bool) ->
    (Project_skeleton.t -> bool) -> 'a t -> bool
end

module Polymorphic_gen(P: Polymorphic_input) = struct

  include Type.Polymorphic(P)

  (* cannot declare [name] locally in instantiate since it prevents OCaml
     generalization *)
  let name = !poly_name_ref 
  let instantiate ty =
    let res, first = instantiate ty in
    if first && name <> "" then begin
      let ml_name = 
	Type.sfprintf
	  "Datatype.%s %a" 
	  name
	  (fun fmt ty -> Type.pp_ml_name ty Type.Call fmt)
	  ty
      in
      Type.set_ml_name res (Some ml_name)
    end;
    res, first

  let () = poly_name_ref := ""

  module Make_gen(X: S)(R: sig val rehash: X.t poly -> X.t poly end) = struct

    module T = struct
      type t = X.t P.t
      let ty, _is_new = instantiate X.ty
    end

    include T
    include
      Build
      (struct
        include T
        let build mk f =
          if mk == undefined || f == undefined then undefined else mk f
        let compare = build P.mk_compare X.compare
        let equal = build P.mk_equal X.equal
        let hash = build P.mk_hash X.hash
        let copy =
          let mk f =
            if P.map == undefined then undefined
            else
              (* [JS 2011/05/31] No optimisation for the special case of
                 identity, since we really want to perform a DEEP copy. *)
              (*if f == identity then identity else*)
              fun x -> P.map f x
          in
          build mk X.copy
        let rehash = R.rehash

        let internal_pretty_code =
          let mk f =
            if f == pp_fail then pp_fail
            else fun p fmt x -> P.mk_internal_pretty_code f p fmt x
          in
          build mk X.internal_pretty_code
        let pretty = build P.mk_pretty X.pretty
        let varname = build P.mk_varname X.varname
        let mem_project =
          let mk f =
            if P.mk_mem_project == undefined then undefined
            else if f == never_any_project then never_any_project
            else fun p x -> P.mk_mem_project f p x
          in
          build mk X.mem_project
	let reprs = if Type.may_use_obj () then Type.reprs ty else []
       end)

    let descr, packed_descr =
      mk_full_descr
        (Descr.of_structural ty (P.structural_descr (Descr.str X.descr)))

  end

end

module Polymorphic(P: Polymorphic_input) = struct
  include Polymorphic_gen(P)
  module Make(X: S) = 
    Make_gen
      (X)
      (struct 
	let rehash = 
	  if Descr.is_unmarshable X.descr then undefined 
	  else identity
       end)
end

(* ****************************************************************************)
(** {3 Reference} *)
(* ****************************************************************************)

let () = poly_name_ref := "t_ref"
module Poly_ref =
  Polymorphic
    (struct
      type 'a t = 'a ref
      let name ty = Type.par_ty_name is_function_or_pair ty ^ " ref"
      let module_name = "Datatype.Ref"
      let reprs ty = [ ref ty ]
      let structural_descr = Structural_descr.t_ref
      let mk_equal f x y = f !x !y
      let mk_compare f x y = if x == y then 0 else f !x !y
      let mk_hash f x = f !x
      let map f x = ref (f !x)
      let mk_internal_pretty_code f p fmt x =
        let pp fmt = Format.fprintf fmt "@[<hv 2>ref@;%a@]" (f Type.Call) !x in
        Type.par p Type.Call fmt pp
      let mk_pretty f fmt x =
        mk_internal_pretty_code (fun _ -> f) Type.Basic fmt x
      let mk_varname = undefined
      let mk_mem_project mem f x = mem f !x
     end)

module Ref = Poly_ref.Make

let t_ref (type typ) (ty: typ Type.t) =
  let module L =
    Ref(struct
      type t = typ
      let ty = ty
      let name = Type.name ty
      let descr = Descr.of_type ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs ty
      let equal = equal ty
      let compare = compare ty
      let hash = hash ty
      let copy = copy ty
      let internal_pretty_code = internal_pretty_code ty
      let pretty_code = pretty_code ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project ty
    end)
  in
  L.ty

(* ****************************************************************************)
(** {3 Option} *)
(* ****************************************************************************)

let () = poly_name_ref := "option"
module Poly_option =
  Polymorphic
    (struct
      type 'a t = 'a option
      let name ty = Type.par_ty_name is_function_or_pair ty ^ " option"
      let module_name = "Type.Option"
      let reprs ty = [ Some ty ]
      let structural_descr = Structural_descr.t_option
      let mk_equal f x y = match x, y with
        | None, None -> true
        | None, Some _ | Some _, None -> false
        | Some x, Some y -> f x y
      let mk_compare f x y =
        if x == y then 0
        else match x, y with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some x, Some y -> f x y
      let mk_hash f = function None -> 0 | Some x -> f x
      let map f = function None -> None | Some x -> Some (f x)
      let mk_internal_pretty_code f p fmt = function
        | None -> Format.fprintf fmt "None"
        | Some x ->
          let pp fmt =
            Format.fprintf fmt "@[<hv 2>Some@;%a@]" (f Type.Call) x
          in
          Type.par p Type.Call fmt pp
      let mk_pretty f fmt x =
        mk_internal_pretty_code (fun _ -> f) Type.Basic fmt x
      let mk_varname = undefined
      let mk_mem_project mem f = function None -> false | Some x -> mem f x
     end)

module Option = Poly_option.Make


let option (type typ) (ty: typ Type.t) =
  let module L =
    Option(struct
      type t = typ
      let ty = ty
      let name = Type.name ty
      let descr = Descr.of_type ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs ty
      let equal = equal ty
      let compare = compare ty
      let hash = hash ty
      let copy = copy ty
      let internal_pretty_code = internal_pretty_code ty
      let pretty_code = pretty_code ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project ty
    end)
  in
  L.ty

(* ****************************************************************************)
(** {3 List} *)
(* ****************************************************************************)

let () = poly_name_ref := "list"
module Poly_list =
  Polymorphic
    (struct
      type 'a t = 'a list
      let name ty = Type.par_ty_name is_function_or_pair ty ^ " list"
      let module_name = "Datatype.List"
      let reprs ty = [ [ ty ] ]
      let structural_descr = Structural_descr.t_list
      let mk_equal f l1 l2 =
        try List.for_all2 f l1 l2 with Invalid_argument _ -> false
      let rec mk_compare f l1 l2 =
        if l1 == l2 then 0
        else match l1, l2 with
        | [], [] -> assert false
        | [], _ :: _ -> -1
        | _ :: _, [] -> 1
        | x1 :: q1, x2 :: q2 ->
          let n = f x1 x2 in
          if n = 0 then mk_compare f q1 q2 else n
      exception Too_long of int
      (* Do not spend too much time hashing long lists... *)
      let mk_hash f l =
        try
          snd (List.fold_left 
                 (fun (length,acc) d ->
                   if length > 15 then raise (Too_long acc);
                   length+1, 257 * acc + f d) 
                 (0,1) l)
        with Too_long n -> n
      let map = List.map
      let mk_internal_pretty_code f p fmt l =
        let pp fmt =
          Format.fprintf fmt "@[<hv 2>[ %t ]@]"
            (fun fmt ->
              let rec print fmt = function
                | [] -> ()
                | [ x ] -> Format.fprintf fmt "%a" (f Type.List) x
                | x :: l -> Format.fprintf fmt "%a;@;%a" (f Type.List) x print l
              in
              print fmt l)
        in
        Type.par p Type.Basic fmt pp (* Never enclose lists in parentheses *)
      let mk_pretty f fmt x =
        mk_internal_pretty_code (fun _ -> f) Type.Basic fmt x
      let mk_varname = undefined
      let mk_mem_project mem f = List.exists (mem f)
     end)

module Caml_list = List
module List = Poly_list.Make

let list (type typ) (ty: typ Type.t) =
  let module L =
    List(struct
      type t = typ
      let ty = ty
      let name = Type.name ty
      let descr = Descr.of_type ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs ty
      let equal = equal ty
      let compare = compare ty
      let hash = hash ty
      let copy = copy ty
      let internal_pretty_code = internal_pretty_code ty
      let pretty_code = pretty_code ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project ty
    end)
  in
  L.ty

(* ****************************************************************************)
(** {3 Arrays} *)
(* ****************************************************************************)

let () = poly_name_ref := "array"
module Poly_array =
  Polymorphic
    (struct
      type 'a t = 'a array
      let name ty = Type.par_ty_name is_function_or_pair ty ^ " array"
      let module_name = "Datatype.Array"
      let reprs ty = [ [| ty |] ]
      let structural_descr = Structural_descr.t_array
      exception Early_exit of int
      let mk_equal f a1 a2 =
	let size = Array.length a1 in
	if Array.length a2 != size then false
	else try
	       for i = 0 to size - 1 do
		 if not (f a1.(i) a2.(i)) then raise (Early_exit 0)
	       done;
	       true
	  with Early_exit _ -> false
      ;;
      let mk_compare f a1 a2 =
        if a1 == a2 then 0
	else let size1 = Array.length a1 and size2 = Array.length a2 in
	     if size1 < size2 then -1
	     else if size2 > size1 then 1
	     else try
		    for i = 0 to size1 do
		      let n = f a1.(i) a2.(i) in
		      if n != 0 then raise (Early_exit n)
		    done;
		    0
	       with Early_exit n -> n
      ;;
      (* Do not spend too much time hashing long arrays... *)
      let mk_hash f a =
	let max = max 15 ((Array.length a) - 1) in
	let acc = ref 1 in
	for i = 0 to max do acc := 257 * !acc + f a.(i) done;
	!acc
      ;;
      let map = Array.map
      let mk_internal_pretty_code f p fmt a =
        let pp fmt =
          Format.fprintf fmt "@[<hv 2>[| %t |]@]"
            (fun fmt ->
	      let length = Array.length a in
	      match length with
		| 0 -> ()
		| _ -> (Format.fprintf fmt "%a" (f Type.List) a.(0);
			for i = 1 to (length - 1) do
			  Format.fprintf fmt ";@;%a" (f Type.List) a.(i)
			done))
        in
        Type.par p Type.Basic fmt pp (* Never enclose arrays in parentheses *)
      let mk_pretty f fmt x =
        mk_internal_pretty_code (fun _ -> f) Type.Basic fmt x
      let mk_varname = undefined
      let mk_mem_project mem f a =
	try
	  for i = 0 to (Array.length a - 1) do
	    if mem f a.(i) then raise (Early_exit 0)
	  done;
	  false
	with Early_exit _ -> true
     end)

module Caml_array = Array
module Array = Poly_array.Make

let array (type typ) (ty: typ Type.t) =
  let module L =
    Array(struct
      type t = typ
      let ty = ty
      let name = Type.name ty
      let descr = Descr.of_type ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs ty
      let equal = equal ty
      let compare = compare ty
      let hash = hash ty
      let copy = copy ty
      let internal_pretty_code = internal_pretty_code ty
      let pretty_code = pretty_code ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project ty
    end)
  in
  L.ty


(* ****************************************************************************)
(** {3 Queue} *)
(* ****************************************************************************)

let () = poly_name_ref := "queue"
module Poly_queue =
  Polymorphic
    (struct
      type 'a t = 'a Queue.t
      let name ty = Type.par_ty_name is_function_or_pair ty ^ " Queue.t"
      let module_name = "Datatype.Queue"
      let reprs x =
        let q = Queue.create () in
        Queue.add x q;
        [ q ]
      let structural_descr = Structural_descr.t_queue
      let mk_equal = undefined
      let mk_compare = undefined
      let mk_hash = undefined
      let map = undefined
      let mk_internal_pretty_code = undefined
      let mk_pretty = undefined
      let mk_varname = undefined
      let mk_mem_project mem f q =
        try Queue.iter (fun x -> if mem f x then raise Exit) q; false
        with Exit -> true
     end)

module Queue = Poly_queue.Make

let queue (type typ) (ty: typ Type.t) =
  let module L =
    Queue(struct
      type t = typ
      let ty = ty
      let name = Type.name ty
      let descr = Descr.of_type ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs ty
      let equal = equal ty
      let compare = compare ty
      let hash = hash ty
      let copy = copy ty
      let internal_pretty_code = internal_pretty_code ty
      let pretty_code = pretty_code ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project ty
    end)
  in
  L.ty

(* ****************************************************************************)
(** {3 Set} *)
(* ****************************************************************************)

module type Functor_info = sig val module_name: string end

(* ocaml functors are generative *)
module Set
  (S: FCSet.S)(E: S with type t = S.elt)(Info: Functor_info) =
struct

  let () = check E.equal "equal" E.name Info.module_name
  let () = check E.compare "compare" E.name Info.module_name

  module P = Make
    (struct
      type t = S.t
      let name = Info.module_name ^ "(" ^ E.name ^ ")"
      let structural_descr =
        Structural_descr.t_set_unchanged_compares (Descr.str E.descr)
      open S
      let reprs = empty :: Caml_list.map (fun r -> singleton r) E.reprs
      let compare = S.compare
      let equal = S.equal
      let hash =
        if E.hash == undefined then undefined
        else (fun s -> S.fold (fun e h -> 67 * E.hash e + h) s 189)
      let rehash =
        if Descr.is_unmarshable E.descr then undefined
        else if Descr.is_abstract E.descr then identity
        else
          fun s -> (* The key changed, rebalance the tree *)
            S.fold S.add s S.empty
      let copy =
        (* [JS 2011/05/31] No optimisation for the special case of
           identity, since we really want to perform a DEEP copy. *)
(*      if E.copy == identity then identity
        else*) fun s -> S.fold (fun x -> S.add (E.copy x)) s S.empty

      let internal_pretty_code p_caller fmt s =
        if is_empty s then
          Format.fprintf fmt "%s.empty" Info.module_name
        else
          let pp fmt =
            if S.cardinal s = 1 then
              Format.fprintf fmt "@[<hv 2>%s.singleton@;%a@]"
                Info.module_name
                (E.internal_pretty_code Type.Call)
                (Caml_list.hd (S.elements s))
            else
              Format.fprintf fmt
                "@[<hv 2>List.fold_left@;\
(fun acc s -> %s.add s acc)@;%s.empty@;%a@]"
                Info.module_name
                Info.module_name
                (let module L = List(E) in L.internal_pretty_code Type.Call)
                (S.elements s)
          in
          Type.par p_caller Type.Call fmt pp

      let pretty fmt s = 
	Format.fprintf fmt "@[<hv 2>{@ %t}@]"
	  (fun fmt ->
	    S.iter (fun x -> Format.fprintf fmt "@[%a;@ @]" E.pretty x) s)
	  
      let varname = undefined
      let mem_project p s =
        try S.iter (fun x -> if E.mem_project p x then raise Exit) s; false
        with Exit -> true
     end)
  include S

  let () = Type.set_ml_name P.ty (Some (Info.module_name ^ ".ty"))

  let ty = P.ty
  let name = P.name
  let descr = P.descr
  let packed_descr = P.packed_descr
  let reprs = P.reprs
  let equal = P.equal
  let compare = P.compare
  let hash = P.hash
  let internal_pretty_code = P.internal_pretty_code
  let pretty_code = P.pretty_code
  let pretty = P.pretty
  let varname = P.varname
  let mem_project = P.mem_project
  let copy = P.copy

end

(* ****************************************************************************)
(** {3 Map} *)
(* ****************************************************************************)

module Map
  (M: FCMap.S)(Key: S with type t = M.key)(Info: Functor_info) = 
struct

  let () = check Key.equal "equal" Key.name Info.module_name
  let () = check Key.compare "compare" Key.name Info.module_name

  module P_gen = Polymorphic_gen
    (struct
      type 'a t = 'a M.t
      let name ty =
        Info.module_name ^ "(" ^ Key.name ^ ", " ^ Type.name ty ^ ")"
      let structural_descr d =
        Structural_descr.t_map_unchanged_compares (Descr.str Key.descr) d
      let module_name = Info.module_name
      open M
      let reprs r =
        [ Caml_list.fold_left (fun m k -> add k r m) empty Key.reprs ]
      let mk_compare = M.compare
      let mk_equal = M.equal
      let mk_hash = undefined
      let map = M.map
      let mk_internal_pretty_code = undefined
      (*f_value p_caller fmt map =
        (* [JS 2011/04/01] untested code! *)
        let pp_empty fmt = Format.fprintf fmt "%s.empty" Info.module_name in
        if M.is_empty map then
          Type.par p_caller Type.Basic fmt pp_empty
        else
          let pp fmt =
            Format.fprintf
              fmt "@[<hv 2>@[<hv 2>let map =@;%t@;<1 -2>in@]" pp_empty;
            M.iter
              (fun k v ->
                Format.fprintf
                  fmt
                  "@[<hv 2>let map =@;%s.add@;@[<hv 2>map@;%a@;%a@]@;<1 -2>in@]"
                  Info.module_name
                  (Key.internal_pretty_code Type.Call) k
                  (f_value Type.Call) v)
              map;
            Format.fprintf fmt "@[map@]@]"
          in
          Type.par p_caller Type.Call fmt pp*)
      let mk_pretty f_value fmt map =
        Format.fprintf fmt  "@[{{ ";
        M.iter
          (fun k v ->
            Format.fprintf fmt "@[@[%a@] -> @[%a@]@];@ "
              Key.pretty k
              f_value v)
          map;
        Format.fprintf fmt  " }}@]"
      let mk_varname _ =
        if Key.varname == undefined then undefined
        else fun _ -> Format.sprintf "%s_map" Key.name
      let mk_mem_project =
        if Key.mem_project == undefined then undefined
        else
          fun mem ->
            if mem == never_any_project && Key.mem_project == never_any_project
            then never_any_project
            else
              fun p m ->
                try
                  M.iter
                    (fun k v ->
                      if Key.mem_project p k || mem p v then raise Exit)
                    m;
                  false
                with Exit ->
                  true
     end)

  module P = struct
    include P_gen
    module Make(X:S) = 
      Make_gen
	(X)
	(struct
	  let rehash =
	    if Descr.is_unmarshable Key.descr
	      || Descr.is_unmarshable X.descr 
	    then undefined
	    else
	      if Descr.is_abstract Key.descr then identity
	      else (* the key changed: rebuild the map *)
		fun m ->
		  M.fold M.add m M.empty;
	 end)
  end


  include M
  module Key = Key
  module Make = P.Make

end

(* ****************************************************************************)
(** {3 Hashtbl} *)
(* ****************************************************************************)

(* ocaml functors are generative *)
module Hashtbl
  (H: Hashtbl_with_descr)(Key: S with type t = H.key)(Info : Functor_info) =
struct

  let () = check Key.equal "equal" Key.name Info.module_name
  let () = check Key.hash "hash" Key.name Info.module_name

  module P_gen = Polymorphic_gen
    (struct
      type 'a t = 'a H.t
      let name ty =
        Info.module_name ^ "(" ^ Key.name ^ ", " ^ Type.name ty ^ ")"
      let module_name = Info.module_name
      let structural_descr = H.structural_descr
      let reprs x =
        [ let h = H.create 7 in
          Caml_list.iter (fun k -> H.add h k x) Key.reprs; h ]
      let mk_compare = undefined
      let mk_equal = from_compare
      let mk_hash = undefined
      let map f_value tbl =
        (* first mapping which reverses the binding order *)
        let h = H.create (H.length tbl) (* may be very memory-consuming *) in
        H.iter (fun k v -> H.add h k (f_value v)) tbl;
        (* copy which reverses again the binding order: so we get the right
           order *)
        let h2 = H.create (H.length tbl) (* may be very memory-consuming *) in
        H.iter (fun k v -> H.add h2 k v) h;
        h2
      let mk_internal_pretty_code = undefined
      let mk_pretty = from_pretty_code
      let mk_varname = undefined
      let mk_mem_project =
        if Key.mem_project == undefined then undefined
        else
          fun mem ->
            if mem == never_any_project && Key.mem_project == never_any_project
            then never_any_project
            else
              fun p m ->
                try
                  H.iter
                    (fun k v ->
                      if Key.mem_project p k || mem p v then raise Exit)
                    m;
                  false
                with Exit ->
                  true
   end)

  module P = struct
    include P_gen
    module Make(X:S) = 
      Make_gen
	(X)
	(struct
	  let rehash =
	    if Descr.is_unmarshable Key.descr
	      || Descr.is_unmarshable X.descr 
	    then undefined
	    else
	      if Descr.is_abstract Key.descr then identity
	      else (* the key changed: rebuild the hashtbl *)
		fun h ->
		  let h' = H.create (H.length h) in
		  H.iter (H.add h') h;
		  h'
	 end)
	
  end

  include H

  let make_type (type typ) (ty: typ Type.t) =
    let module M =
          P.Make(struct
              type t = typ
              include Undefined
              let ty = ty
              let name = Type.name ty
              let descr = Descr.of_type ty
              let packed_descr = Descr.pack descr
              let reprs = Type.reprs ty
              let pretty_code = undefined
          end)
    in M.ty

  let memo tbl k f =
    try find tbl k
    with Not_found ->
      let v = f k in
      add tbl k v;
      v

  module Key = Key
  module Make = P.Make

end

(* ****************************************************************************)
(** {3 Weak hashtbl} *)
(* ****************************************************************************)

module type Sub_caml_weak_hashtbl = sig
  type data
  type t
  val create: int -> t
  val add: t -> data -> unit
end

module Initial_caml_weak = Weak

module Weak(W: Sub_caml_weak_hashtbl)(D: S with type t = W.data) = struct
  include Make
    (struct
      include Undefined
      type t = W.t
      let name = "Weak(" ^ D.name ^ ")"
      let reprs = let w = W.create 0 in Caml_list.iter (W.add w) D.reprs; [ w ]
     end)
  let () = Type.set_ml_name ty None;
end

module Caml_weak_hashtbl(D: S) = struct
  let () = check D.equal "equal" D.name "Caml_weak_hashtbl"
  let () = check D.compare "hash" D.name "Caml_weak_hashtbl"
  module W = Initial_caml_weak.Make(D)
  include W
  module Datatype = Weak(W)(D)
end

(* ****************************************************************************)
(** {2 Simple type values} *)
(* ****************************************************************************)

module With_collections(X: S)(Info: Functor_info) = struct

  module D = X
  include D

  module Set =
    Set
      (FCSet.Make(D))
      (D)
      (struct let module_name = Info.module_name ^ ".Set" end)

  module Map =
    Map
      (FCMap.Make(D))
      (D)
      (struct let module_name = Info.module_name ^ ".Map" end)

  module Hashtbl =
    Hashtbl
      (struct
	include FCHashtbl.Make(D)

        (* Override "sorted" iterators by using the datatype comparison
           function if it has been supplied *)
        let iter_sorted ?cmp = match cmp with
          | None ->
              if D.compare == undefined then iter_sorted ?cmp:None
              else iter_sorted ~cmp:D.compare
          | Some cmp -> iter_sorted ~cmp

        let fold_sorted ?cmp = match cmp with                                  
          | None ->
              if D.compare == undefined then fold_sorted ?cmp:None
              else fold_sorted ~cmp:D.compare
          | Some cmp -> fold_sorted ~cmp

        let structural_descr =
          Structural_descr.t_hashtbl_unchanged_hashs (Descr.str D.descr)
       end)
      (D)
      (struct let module_name = Info.module_name ^ ".Hashtbl" end)

end

module Make_with_collections(X: Make_input) =
  With_collections
    (Make(X))
    (struct let module_name = String.capitalize X.name end)

(* ****************************************************************************)
(** {2 Predefined datatype} *)
(* ****************************************************************************)

module Simple_type
  (X: sig
    type t
    val name: string
    val reprs: t list
    val pretty: Format.formatter -> t -> unit
    val copy: t -> t
    val varname: t -> string
    val compare: t -> t -> int
    val equal: t -> t -> bool
  end) =
struct

  let module_name = "Datatype." ^ String.capitalize X.name

  include With_collections
  (Make(struct
          type t = X.t
          let name = X.name
          let reprs = X.reprs
          let structural_descr = Structural_descr.t_abstract
          let equal = X.equal
          let compare = X.compare
          let hash = FCHashtbl.hash
          let rehash = identity
          let copy = X.copy
          let internal_pretty_code =
            if X.pretty == undefined then undefined else fun _ -> X.pretty
          let pretty = X.pretty
          let varname = X.varname
          let mem_project = never_any_project
        end))
  (struct let module_name = module_name end)

  let () = Type.set_ml_name ty (Some ("Datatype." ^ name))

end

module Unit =
  Simple_type
    (struct
      type t = unit
      let name = "unit"
      let reprs = [ () ]
      let copy = identity
      let compare () () = 0
      let equal () () = true
      let pretty fmt () = Format.fprintf fmt "()"
      let varname = undefined
     end)
let unit = Unit.ty

module Bool =
  Simple_type
    (struct
      type t = bool
      let name = "bool"
      let reprs = [ true ]
      let copy = identity
      let compare : bool -> bool -> int = Pervasives.compare
      let equal : bool -> bool -> bool = (=)
      let pretty fmt b = Format.fprintf fmt "%B" b
      let varname _ = "b"
     end)
let bool = Bool.ty

module Int = struct
  include Simple_type
    (struct
      type t = int
      let name = "int"
      let reprs = [ 2 ]
      let copy = identity
      let compare : int -> int -> int = Pervasives.compare
      let equal : int -> int -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%d" n
      let varname _ = "n"
     end)
  let compare : int -> int -> int = Pervasives.compare
end
let int = Int.ty

module Int32 =
  Simple_type
    (struct
      type t = int32
      let name = "int32"
      let reprs = [ Int32.zero ]
      let copy = identity
      let compare = Int32.compare
      let equal : int32 -> int32 -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%ld" n
      let varname _ = "n32"
     end)
let int32 = Int32.ty

module Int64 =
  Simple_type
    (struct
      type t = int64
      let name = "int64"
      let reprs = [ Int64.zero ]
      let copy = identity
      let compare = Int64.compare
      let equal : int64 -> int64 -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%Ld" n
      let varname _ = "n64"
     end)
let int64 = Int64.ty

module Nativeint =
  Simple_type
    (struct
      type t = nativeint
      let name = "nativeint"
      let reprs = [ Nativeint.zero ]
      let copy = identity
      let compare = Nativeint.compare
      let equal : nativeint -> nativeint -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%nd" n
      let varname _ = "native_n"
     end)
let nativeint = Nativeint.ty

module Float =
  Simple_type
    (struct
      type t = float
      let name = "float"
      let reprs = [ 0.1 ]
      let copy = identity
      let compare : float -> float -> int = Pervasives.compare
      let equal : float -> float -> bool = (=)
      let pretty fmt f = Format.fprintf fmt "%f" f
      let varname _ = "f"
     end)
let float = Float.ty

module Char =
  Simple_type
    (struct
      type t = char
      let name = "char"
      let reprs = [ ' ' ]
      let copy = identity
      let compare = Char.compare
      let equal : char -> char -> bool = (=)
      let pretty fmt c = Format.fprintf fmt "%c" c
      let varname _ = "c"
     end)
let char = Char.ty

module String =
  Simple_type
    (struct
      type t = string
      let name = "string"
      let reprs = [ "" ]
      let copy = String.copy
      let compare = String.compare
      let equal : string -> string -> bool = (=)
      let pretty fmt s = Format.fprintf fmt "%S" s
      let varname _ = "s"
     end)
let string = String.ty

module Formatter =
  Make
    (struct
      type t = Format.formatter
      let name = "Datatype.Formatter"
      let reprs = [ Format.std_formatter ]
      let structural_descr = Structural_descr.t_unknown
      let equal = undefined
      let compare = undefined
      let hash = undefined
      let rehash = undefined
      let copy = undefined
      let internal_pretty_code = undefined
      let pretty = undefined
      let varname _ = "fmt"
      let mem_project = never_any_project
     end)
let formatter = Formatter.ty

module Big_int =
  Make_with_collections
    (struct
      type t = Integer.t
      let name = "Datatype.Big_int"
      let reprs = [ Integer.zero ]
      let structural_descr = Structural_descr.t_abstract
      let equal = Integer.equal
      let compare = Integer.compare
      let hash = Integer.hash
      let rehash = identity
      let copy = identity
      let internal_pretty_code par fmt n =
        let pp fmt =
          Format.fprintf
            fmt
            "Big_int.big_int_of_string %S"
            (Integer.to_string n)
        in
        Type.par par Type.Call fmt pp
      let pretty = Integer.pretty ~hexa:false
      let varname _ = "big_n"
      let mem_project = never_any_project
     end)
let big_int = Big_int.ty

(* ****************************************************************************)
(** {3 Triple} *)
(* ****************************************************************************)

let () = poly_name_ref := "triple"

module Triple_arg = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  let module_name = "Datatype.Triple"
  let reprs a b c = [ a, b, c ]
  let structural_descr d1 d2 d3 =
    Structural_descr.t_tuple
      [| Structural_descr.pack d1; 
	 Structural_descr.pack d2;
	 Structural_descr.pack d3 |]
  let mk_equal f1 f2 f3 (x1,x2,x3) (y1,y2,y3) = f1 x1 y1 && f2 x2 y2 && f3 x3 y3
  let mk_compare f1 f2 f3 (x1,x2,x3 as x) (y1,y2,y3 as y) =
    if x == y then 0 
    else 
      let n = f1 x1 y1 in 
      if n = 0 then let n = f2 x2 y2 in if n = 0 then f3 x3 y3 else n
      else n
  let mk_hash f1 f2 f3 (x1,x2,x3) = f1 x1 + 1351 * f2 x2 + 257 * f3 x3
  let map f1 f2 f3 (x1,x2,x3) = f1 x1, f2 x2, f3 x3
  let mk_internal_pretty_code f1 f2 f3 p fmt (x1, x2, x3) =
    let pp fmt =
      Format.fprintf
        fmt "@[<hv 2>%a,@;%a,@;%a@]" 
	(f1 Type.Tuple) x1 
	(f2 Type.Tuple) x2
	(f3 Type.Tuple) x3
    in
    Type.par p Type.Tuple fmt pp
  let mk_pretty f1 f2 f3 fmt p =
    Format.fprintf fmt "@[(%a)@]"
      (mk_internal_pretty_code 
	 (fun _ -> f1) (fun _ -> f2) (fun _ -> f3) Type.Basic) 
      p
  let mk_varname = undefined
  let mk_mem_project mem1 mem2 mem3 f (x1, x2, x3) = 
    mem1 f x1 && mem2 f x2 && mem3 f x3
end

module rec Triple_name: sig 
  val name: 'a Type.t -> 'b Type.t -> 'c Type.t -> string 
end =
struct
  let name ty1 ty2 ty3 =
    let arg ty =
      Type.par_ty_name
        (fun ty ->
          Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty
	  || Poly_triple.is_instance_of ty)
        ty
    in
    arg ty1 ^ " * " ^ arg ty2 ^ " * " ^ arg ty3
end

and Poly_triple : sig
  include Type.Polymorphic3 with type ('a,'b,'c) poly = 'a * 'b * 'c
  module Make(T1: S)(T2: S)(T3:S) :  S with type t = (T1.t, T2.t, T3.t) poly
end =
  (* Split the functor argument in 2 modules such that ocaml is able to safely
     evaluate the recursive modules *)
  Polymorphic3(struct include Triple_arg include Triple_name end)

module Triple = Poly_triple.Make

let triple
    (type typ1) (type typ2) (type typ3)
    (ty1: typ1 Type.t) (ty2: typ2 Type.t) (ty3: typ3 Type.t) =
  let module Make(X: sig type t val ty: t Type.t end) = struct
      type t = X.t
      let ty = X.ty
      let name = Type.name X.ty
      let descr = Descr.of_type X.ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs X.ty
      let equal = equal X.ty
      let compare = compare X.ty
      let hash = hash X.ty
      let copy = copy X.ty
      let internal_pretty_code = internal_pretty_code X.ty
      let pretty_code = pretty_code X.ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project X.ty
  end
  in
  let module L = Triple
	(Make(struct type t = typ1 let ty = ty1 end))
	(Make(struct type t = typ2 let ty = ty2 end))
	(Make(struct type t = typ3 let ty = ty3 end))
  in
  L.ty

(* ****************************************************************************)
(** {3 Quadruple} *)
(* ****************************************************************************)

let () = poly_name_ref := "quadruple"

module Quadruple_arg = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd
  let module_name = "Datatype.Quadruple"
  let reprs a b c d = [ a, b, c, d ]
  let structural_descr d1 d2 d3 d4 =
    Structural_descr.t_tuple
      [| Structural_descr.pack d1; 
	 Structural_descr.pack d2;
	 Structural_descr.pack d3;
	 Structural_descr.pack d4 |]
  let mk_equal f1 f2 f3 f4 (x1,x2,x3,x4) (y1,y2,y3,y4) = 
    f1 x1 y1 && f2 x2 y2 && f3 x3 y3 && f4 x4 y4
  let mk_compare f1 f2 f3 f4 (x1,x2,x3,x4 as x) (y1,y2,y3,y4 as y) =
    if x == y then 0 
    else 
      let n = f1 x1 y1 in 
      if n = 0 then 
	let n = f2 x2 y2 in 
	if n = 0 then let n = f3 x3 y3 in if n = 0 then f4 x4 y4 else n
	else n
      else n
  let mk_hash f1 f2 f3 f4 (x1,x2,x3,x4) = 
    f1 x1 + 1351 * f2 x2 + 257 * f3 x3 + 997 * f4 x4
  let map f1 f2 f3 f4 (x1,x2,x3,x4) = f1 x1, f2 x2, f3 x3, f4 x4
  let mk_internal_pretty_code f1 f2 f3 f4 p fmt (x1, x2, x3, x4) =
    let pp fmt =
      Format.fprintf
        fmt "@[<hv 2>%a,@;%a,@;%a,@;%a@]" 
	(f1 Type.Tuple) x1 
	(f2 Type.Tuple) x2
	(f3 Type.Tuple) x3
	(f4 Type.Tuple) x4
    in
    Type.par p Type.Tuple fmt pp
  let mk_pretty f1 f2 f3 f4 fmt p =
    Format.fprintf fmt "@[(%a)@]"
      (mk_internal_pretty_code 
	 (fun _ -> f1) (fun _ -> f2) (fun _ -> f3) (fun _ -> f4) Type.Basic) 
      p
  let mk_varname = undefined
  let mk_mem_project mem1 mem2 mem3 mem4 f (x1, x2, x3, x4) = 
    mem1 f x1 && mem2 f x2 && mem3 f x3 && mem4 f x4
end

module rec Quadruple_name: sig 
  val name: 'a Type.t -> 'b Type.t -> 'c Type.t -> 'd Type.t -> string 
end =
struct
  let name ty1 ty2 ty3 ty4 =
    let arg ty =
      Type.par_ty_name
        (fun ty ->
          Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty
	  || Poly_triple.is_instance_of ty || Poly_quadruple.is_instance_of ty)
        ty
    in
    arg ty1 ^ " * " ^ arg ty2 ^ " * " ^ arg ty3 ^ " * " ^ arg ty4
end

and Poly_quadruple : sig
  include Type.Polymorphic4 with type ('a,'b,'c,'d) poly = 'a * 'b * 'c * 'd
  module Make(T1: S)(T2: S)(T3:S)(T4:S) :
    S with type t = (T1.t, T2.t, T3.t, T4.t) poly
end =
  struct
    (* Split the functor argument in 2 modules such that ocaml is able to safely
       evaluate the recursive modules *)
    include Polymorphic4
      (struct include Quadruple_arg include Quadruple_name end)
  end

module Quadruple = Poly_quadruple.Make

let quadruple
    (type typ1) (type typ2) (type typ3) (type typ4)
    (ty1: typ1 Type.t) (ty2: typ2 Type.t) (ty3: typ3 Type.t) (ty4: typ4 Type.t)
    =
  let module Make(X: sig type t val ty: t Type.t end) = struct
      type t = X.t
      let ty = X.ty
      let name = Type.name X.ty
      let descr = Descr.of_type X.ty
      let packed_descr = Descr.pack descr
      let reprs = Type.reprs X.ty
      let equal = equal X.ty
      let compare = compare X.ty
      let hash = hash X.ty
      let copy = copy X.ty
      let internal_pretty_code = internal_pretty_code X.ty
      let pretty_code = pretty_code X.ty
      let pretty = from_pretty_code
      let varname = varname ty
      let mem_project = mem_project X.ty
  end
  in
  let module L = Quadruple
	(Make(struct type t = typ1 let ty = ty1 end))
	(Make(struct type t = typ2 let ty = ty2 end))
	(Make(struct type t = typ3 let ty = ty3 end))
	(Make(struct type t = typ4 let ty = ty4 end))
  in
  L.ty

module Pair_with_collections(T1: S)(T2: S)(Info:Functor_info) =
  With_collections(Pair(T1)(T2))(Info)

module Triple_with_collections(T1: S)(T2: S)(T3: S)(Info:Functor_info) =
  With_collections(Triple(T1)(T2)(T3))(Info)

module Quadruple_with_collections(T1:S)(T2:S)(T3:S)(T4:S)(Info:Functor_info) =
  With_collections(Quadruple(T1)(T2)(T3)(T4))(Info)

module Option_with_collections(T:S)(Info:Functor_info) =
  With_collections (Option(T))(Info)

module List_with_collections(T:S)(Info:Functor_info) =
  With_collections (List(T))(Info)

module Array_with_collections(T:S)(Info:Functor_info) =
  With_collections (Array(T))(Info)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

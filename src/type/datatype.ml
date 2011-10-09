(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
  let structural_descr = Structural_descr.Unknown
  let rehash = undefined
end

module Serializable_undefined = struct
  include Partial_undefined
  let structural_descr = Structural_descr.Abstract
  let rehash = identity
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
  List.for_all (fun x -> x.[0] = (String.capitalize x).[0]) l

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
  include Set.S
  val ty: t Type.t
  val name: string
  val descr: t Descr.t
  val packed_descr: Structural_descr.pack
  val reprs: t list
  val hash: t -> int
  val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
  val pretty_code: Format.formatter -> t -> unit
  val pretty: Format.formatter -> t -> unit
  val varname: t -> string
  val mem_project: (Project_skeleton.t -> bool) -> t -> bool
  val copy: t -> t
end

module type Map = sig
  include Map_common_interface.S
  module Key: S with type t = key
  module Make(Data: S) : S with type t = Data.t t
end

module type Hashtbl = sig
  include Hashtbl.S
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

let get_info = info

module type Polymorphic = sig
  include Type.Polymorphic
  module Make(T: S) : S with type t = T.t poly
end

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

  module Make(T1: S)(T2: S) = struct

    module T = struct
      type t = (T1.t, T2.t) P.t
      let ty, is_new = instantiate T1.ty T2.ty
    end

    include T
    include Build
      (struct
        include T
	let reprs = 
	  if Type.may_use_obj () then Type.reprs ty 
	  else P.reprs (List.hd T1.reprs) (List.hd T2.reprs)
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
(** {3 Pair} *)
(* ****************************************************************************)

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
  module Make(T1: S)(T2: S) : sig
    (* include S with type t = (T1.t, T2.t) poly *)
    (* Copy of S to overcome 3.10.2 bug. Replace it with the line above
       when support of 3.10.2 is dropped... *)
    type t = (T1.t, T2.t) poly
    val ty: t Type.t
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
    val copy: t -> t
      (* End of copy of S *)
    val rehash: t -> t
  end
end =
  struct
  (* Split the functor argument in 2 modules such that ocaml is able to safely
     evaluate the recursive modules *)
    include Polymorphic2(struct include Pair_arg include Pair_name end)
  end

module Pair = Poly_pair.Make
let pair ty1 ty2 = fst (Poly_pair.instantiate ty1 ty2)

(* ****************************************************************************)
(** {3 Function} *)
(* ****************************************************************************)

module Function
  (T1: sig include Ty val label: (string * (unit -> t) option) option end)
  (T2: Ty) =
struct
  module T = struct
    type t = T1.t -> T2.t
    let ty, is_new = Type.Function.instantiate ?label:T1.label T1.ty T2.ty
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

let func ?label ty ty_ret = fst (Type.Function.instantiate ?label ty ty_ret)
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

module Polymorphic(P: Polymorphic_input) = struct

  include Type.Polymorphic(P)

  module Make(X: S) = struct

    module T = struct
      type t = X.t P.t
      let ty, is_new = instantiate X.ty
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
        let rehash = identity
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
	let reprs = 
	  if Type.may_use_obj () then Type.reprs ty 
	  else P.reprs (List.hd X.reprs)
       end)

    let descr, packed_descr =
      mk_full_descr
        (Descr.of_structural ty (P.structural_descr (Descr.str X.descr)))

  end

end

(* ****************************************************************************)
(** {3 Reference} *)
(* ****************************************************************************)

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
let t_ref ty = fst (Poly_ref.instantiate ty)

(* ****************************************************************************)
(** {3 Option} *)
(* ****************************************************************************)

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
let option ty = fst (Poly_option.instantiate ty)

(* ****************************************************************************)
(** {3 List} *)
(* ****************************************************************************)

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
      let mk_hash f = List.fold_left (fun acc d -> 257 * acc + f d) 1
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

(* [JS 2011/01/26] correct version of list, but require OCaml 3.12 *)

(* val list: 'a Type.t -> 'a Type.t *)
(*let list (type typ) (ty: typ Type.t) =
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
 *)
(* [JS 2011/01/26] this version is buggy if you use it on a specific datatype
   function (pretty-printers, descriptors, comparators, etc).
   There is the same issue with all the polymorphic type instantiators *)
let list ty = fst (Poly_list.instantiate ty)

(* ****************************************************************************)
(** {3 Queue} *)
(* ****************************************************************************)

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
let queue ty = fst (Poly_queue.instantiate ty)

(* ****************************************************************************)
(** {3 Set} *)
(* ****************************************************************************)

module type Functor_info = sig val module_name: string end

module Initial_caml_set = Set

(* ocaml functors are generative *)
module Set(S: Set.S)(E: S with type t = S.elt)(Info: Functor_info) = struct

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
      let hash = Hashtbl.hash (* Don't know how to do better *)
      let rehash = identity
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

      let pretty = from_pretty_code
      let varname = undefined
      let mem_project p s =
        try S.iter (fun x -> if E.mem_project p x then raise Exit) s; false
        with Exit -> true
     end)

  include S
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

module Map(M: Map_common_interface.S)
          (Key: S with type t = M.key)(Info: Functor_info) = struct

  let () = check Key.equal "equal" Key.name Info.module_name
  let () = check Key.compare "compare" Key.name Info.module_name

  module P = Polymorphic
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

  include M
  module Key = Key
  module Make = P.Make

end

(* ****************************************************************************)
(** {3 Hashtbl} *)
(* ****************************************************************************)

module Initial_caml_hashtbl = Hashtbl

(* ocaml functors are generative *)
module Hashtbl(H: Hashtbl.S)(Key: S with type t = H.key)(Info : Functor_info) =
struct

  let () = check Key.equal "equal" Key.name Info.module_name
  let () = check Key.hash "hash" Key.name Info.module_name

  module P = Polymorphic
    (struct
      type 'a t = 'a H.t
      let name ty =
        Info.module_name ^ "(" ^ Key.name ^ ", " ^ Type.name ty ^ ")"
      let module_name = Info.module_name
      let structural_descr d =
        Structural_descr.t_hashtbl_unchanged_hashs (Descr.str Key.descr) d
      open Hashtbl
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

  include H

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

module Generic_make_with_collections(X: S)(Info: Functor_info) = struct

  module D = X
  include D

  module Set =
    Set
      (Initial_caml_set.Make(D))
      (D)
      (struct let module_name = Info.module_name ^ ".Set" end)

  module Map =
    Map
      (Map_common_interface.Make(D))
      (D)
      (struct let module_name = Info.module_name ^ ".Map" end)

  module Hashtbl =
    Hashtbl
      (Initial_caml_hashtbl.Make(D))
      (D)
      (struct let module_name = Info.module_name ^ ".Hashtbl" end)

end

module Make_with_collections(X: Make_input) =
  Generic_make_with_collections
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

  include Generic_make_with_collections
  (Make(struct
          type t = X.t
          let name = X.name
          let reprs = X.reprs
          let structural_descr = Structural_descr.Abstract
          let equal = X.equal
          let compare = X.compare
          let hash = Initial_caml_hashtbl.hash
          let rehash = identity
          let copy = X.copy
          let internal_pretty_code =
            if X.pretty == undefined then undefined else fun _ -> X.pretty
          let pretty = X.pretty
          let varname = X.varname
          let mem_project = never_any_project
        end))
  (struct let module_name = module_name end)

  let () = Type.set_ml_name ty (Some (module_name ^ ".ty"))

end

module Unit =
  Simple_type
    (struct
      type t = unit
      let name = "unit"
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let is_marshalable = true
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
      let structural_descr = Structural_descr.Unknown
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
      type t = My_bigint.t
      let name = "Datatype.Big_int"
      let reprs = [ My_bigint.zero ]
      let structural_descr = Structural_descr.Abstract
      let equal = My_bigint.equal
      let compare = My_bigint.compare
      let hash = My_bigint.hash
      let rehash = identity
      let copy = identity
      let internal_pretty_code par fmt n =
        let pp fmt =
          Format.fprintf
            fmt
            "Big_int.big_int_of_string %S"
            (My_bigint.to_string n)
        in
        Type.par par Type.Call fmt pp
      let pretty = My_bigint.pretty ~hexa:false
      let varname _ = "big_n"
      let mem_project = never_any_project
     end)
let big_int = Big_int.ty

module Triple(T1: S)(T2: S)(T3: S) =
  Make
    (struct
      type t = T1.t * T2.t * T3.t
      let name = "(" ^ T1.name ^ ", " ^ T2.name ^ ", " ^ T3.name ^ ")"
      let reprs =
        Caml_list.fold_left
          (fun acc x1 ->
            Caml_list.fold_left
              (fun acc x2 ->
                Caml_list.fold_left
                  (fun acc x3 -> (x1, x2, x3) :: acc)
                  acc
                  T3.reprs)
              acc
              T2.reprs)
          []
          T1.reprs
      let structural_descr =
        Structural_descr.t_tuple
          [| T1.packed_descr; T2.packed_descr; T3.packed_descr |]
      let equal =
        if T1.equal == undefined
          || T2.equal == undefined
          || T3.equal == undefined
        then undefined
        else
          fun (x1, x2, x3) (y1, y2, y3) ->
            T1.equal x1 y1 && T2.equal x2 y2 && T3.equal x3 y3
      let compare =
        if T1.compare == undefined
          || T2.compare == undefined
          || T3.compare == undefined
        then undefined
        else
          fun (x1, x2, x3 as x) (y1, y2, y3 as y) ->
            if x == y then 0
            else
              let n = T1.compare x1 y1 in
              if n = 0 then
                let n = T2.compare x2 y2 in
                if n = 0 then T3.compare x3 y3 else n
              else n
      let hash =
        if T1.hash == undefined || T2.hash == undefined || T3.hash == undefined
        then undefined
        else
          fun (x1, x2, x3) ->
            Initial_caml_hashtbl.hash (T1.hash x1, T2.hash x2, T3.hash x3)
      let copy =
        if T1.copy == undefined || T2.copy == undefined || T3.copy == undefined
        then undefined
        else fun (x1, x2, x3) -> T1.copy x1, T2.copy x2, T3.copy x3
      let rehash = identity
      let varname = undefined
      let mem_project =
        if T1.mem_project == undefined
          || T2.mem_project == undefined
          || T3.mem_project == undefined
        then undefined
        else
          if T1.mem_project == never_any_project
            && T2.mem_project == never_any_project
            && T3.mem_project == never_any_project
          then never_any_project
          else
            fun f (x1, x2, x3) ->
              T1.mem_project f x1 && T2.mem_project f x2 && T3.mem_project f x3
      let pretty = from_pretty_code
      let internal_pretty_code =
        if T1.internal_pretty_code == undefined
          || T2.internal_pretty_code == undefined
          || T3.internal_pretty_code == undefined
        then undefined
        else
          if T1.internal_pretty_code == pp_fail
            || T2.internal_pretty_code == pp_fail
            || T3.internal_pretty_code == pp_fail
          then pp_fail
          else
            fun par fmt (x1, x2, x3) ->
              let pp fmt =
                Format.fprintf
                  fmt
                  "%a, %a, %a"
                  (T1.internal_pretty_code Type.Tuple) x1
                  (T2.internal_pretty_code Type.Tuple) x2
                  (T3.internal_pretty_code Type.Tuple) x3
              in
              Type.par par Type.Tuple fmt pp
     end)

module Quadruple(T1: S)(T2: S)(T3: S)(T4:S) =
  Make
    (struct
      type t = T1.t * T2.t * T3.t * T4.t
      let name = 
        Printf.sprintf "(%s, %s, %s, %s)"
          T1.name T2.name T3.name T4.name
      let reprs =
        Caml_list.fold_left
          (fun acc x1 ->
            Caml_list.fold_left
              (fun acc x2 ->
                Caml_list.fold_left
                  (fun acc x3 -> 
                    Caml_list.fold_left
                      (fun acc x4 ->
                        (x1, x2, x3, x4) :: acc)
                      acc
                      T4.reprs)
                  acc
                  T3.reprs)
              acc
              T2.reprs)
          []
          T1.reprs
      let structural_descr =
        Structural_descr.t_tuple
          [| T1.packed_descr; T2.packed_descr; T3.packed_descr; 
             T4.packed_descr |]
      let equal =
        if T1.equal == undefined
          || T2.equal == undefined
          || T3.equal == undefined
          || T4.equal == undefined
        then undefined
        else
          fun (x1, x2, x3, x4) (y1, y2, y3, y4) ->
            T1.equal x1 y1 && T2.equal x2 y2 && T3.equal x3 y3 && T4.equal x4 y4
      let compare =
        if T1.compare == undefined
          || T2.compare == undefined
          || T3.compare == undefined
          || T4.compare == undefined
        then undefined
        else
          fun (x1, x2, x3, x4 as x) (y1, y2, y3, y4 as y) ->
            if x == y then 0
            else
              let n = T1.compare x1 y1 in
              if n = 0 then
                let n = T2.compare x2 y2 in
                if n = 0 then 
                  let n = T3.compare x3 y3 in
                  if n = 0 then T4.compare x4 y4
                  else n
                else n
              else n
      let hash =
        if T1.hash == undefined 
          || T2.hash == undefined 
          || T3.hash == undefined
          || T4.hash == undefined
        then undefined
        else
          fun (x1, x2, x3, x4) ->
            Initial_caml_hashtbl.hash 
              (T1.hash x1, T2.hash x2, T3.hash x3, T4.hash x4)
      let copy =
        if T1.copy == undefined 
          || T2.copy == undefined 
          || T3.copy == undefined
          || T4.copy == undefined
        then undefined
        else fun (x1, x2, x3,x4) -> 
          T1.copy x1, T2.copy x2, T3.copy x3, T4.copy x4
      let rehash = identity
      let varname = undefined
      let mem_project =
        if T1.mem_project == undefined
          || T2.mem_project == undefined
          || T3.mem_project == undefined
          || T4.mem_project == undefined
        then undefined
        else
          if T1.mem_project == never_any_project
            && T2.mem_project == never_any_project
            && T3.mem_project == never_any_project
            && T4.mem_project == never_any_project
          then never_any_project
          else
            fun f (x1, x2, x3, x4) ->
              T1.mem_project f x1 
              && T2.mem_project f x2 
              && T3.mem_project f x3
              && T4.mem_project f x4
      let pretty = from_pretty_code
      let internal_pretty_code =
        if T1.internal_pretty_code == undefined
          || T2.internal_pretty_code == undefined
          || T3.internal_pretty_code == undefined
          || T4.internal_pretty_code == undefined
        then undefined
        else
          if T1.internal_pretty_code == pp_fail
            || T2.internal_pretty_code == pp_fail
            || T3.internal_pretty_code == pp_fail
            || T4.internal_pretty_code == pp_fail
          then pp_fail
          else
            fun par fmt (x1, x2, x3, x4) ->
              let pp fmt =
                Format.fprintf
                  fmt
                  "%a, %a, %a, %a"
                  (T1.internal_pretty_code Type.Tuple) x1
                  (T2.internal_pretty_code Type.Tuple) x2
                  (T3.internal_pretty_code Type.Tuple) x3
                  (T4.internal_pretty_code Type.Tuple) x4
              in
              Type.par par Type.Tuple fmt pp
     end)

module Pair_with_collections(T1: S)(T2: S)(Info:Functor_info) =
  Generic_make_with_collections
    (struct
       include Pair(T1)(T2)
       let structural_descr = Type.structural_descr ty
     end)
    (Info)

module Triple_with_collections(T1: S)(T2: S)(T3: S)(Info:Functor_info) =
  Generic_make_with_collections(Triple(T1)(T2)(T3))(Info)

module Quadruple_with_collections(T1:S)(T2:S)(T3:S)(T4:S)(Info:Functor_info) =
  Generic_make_with_collections(Quadruple(T1)(T2)(T3)(T4))(Info)

module Option_with_collections(T:S)(Info:Functor_info) =
  Generic_make_with_collections (Option(T))(Info)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

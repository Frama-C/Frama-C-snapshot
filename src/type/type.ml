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

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(* Disclaimer
   ----------
   This module uses very unsafe caml features (module Obj).
   Modify it at your own risk.
   Sometimes the caml type system does not help you here.
   Introducing a bug here may introduce some "segmentation faults" in Frama-C *)

let use_obj = ref true
let no_obj () = use_obj := false
let may_use_obj () = !use_obj

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(** Precedences used for generating the minimal number of parenthesis in
    combination with function {!par} below. *)
type precedence =
  | Basic
  | Call
  | Tuple
  | List
  | NoPar

(* p1 <= p2 *)
let lower_prec p1 p2 = match p1, p2 with
  | NoPar, _
  | _, Basic -> true
  | x, y when x = y -> true
  | List, (Tuple | Call) | Tuple, Call -> true
  | _, _ -> false

let par p_caller p_callee fmt pp =
  (* if p_callee <= p_caller then parenthesis else no parenthesis *)
  if lower_prec p_callee p_caller then Format.fprintf fmt "(%t)" pp
  else Format.fprintf fmt "%t" pp

type concrete_repr =
    { mutable name: string;
      digest: Digest.t;
      structural_descr: Structural_descr.t;
      mutable abstract: bool;
      mutable pp_ml_name: precedence -> Format.formatter -> unit }

(* phantom type *)
type 'a t = concrete_repr
type 'a ty = 'a t

(* non-phantom type: the type variable is used here *)
type 'a full_t = { ty: 'a t; reprs: 'a list }

(* ****************************************************************************)
(** {2 Type values are comparable} *)
(* ****************************************************************************)

module Comparable = struct
  let equal x y = x.digest = y.digest
  let compare x y = String.compare x.digest y.digest
  let hash x = Hashtbl.hash x.digest
end
include Comparable

module Tbl = Hashtbl.Make(struct type t = concrete_repr include Comparable end)

(* ****************************************************************************)
(** {2 Global useful values} *)
(* ****************************************************************************)

let types : (string (* name *), Obj.t full_t) Hashtbl.t = Hashtbl.create 97
let embedded_types: concrete_repr Tbl.t = Tbl.create 7

let dummy =
  { name = "";
    digest = "";
    structural_descr = Structural_descr.t_unknown;
    abstract = false;
    pp_ml_name = fun _ _ -> assert false }

(* ****************************************************************************)
(** {2 Main functions} *)
(* ****************************************************************************)

let mk_dyn_pp name = function
  | None ->
    let pp fmt = 
      let plugin_name = match Str.split (Str.regexp_string ".") name with
	| [] -> None
	| p :: _ -> Some p
      in
      match plugin_name with
      | None ->  
	Format.fprintf fmt "(failwith \"%s is not a printable type name\")" name
      | Some p ->
	Format.fprintf fmt "%s.ty" p
    in
    (fun p fmt -> par p Basic fmt pp)
  | Some s ->
    let prec =
      try
        ignore (Str.search_forward (Str.regexp " ") name 0);
        Call
      with Not_found ->
        Basic
    in
    fun p fmt -> par p prec fmt (fun fmt -> Format.fprintf fmt "%s" s)

exception AlreadyExists of string
let register ?(closure=false) ~name ~ml_name structural_descr reprs =
  let error () =
    invalid_arg ("Type.register: invalid reprs for type " ^ name)
  in
  (*  Format.printf "type %S@." name;*)
  match reprs with
  | [] -> error ()
  | r :: _ when Obj.tag (Obj.repr r) = Obj.closure_tag && not closure ->
    (* all the representants have the same types:
       thus that is correct to check only the first one *)
    error ()
  | _ ->
    if Hashtbl.mem types name then raise (AlreadyExists name);
    let pp_ml_name = mk_dyn_pp name ml_name in
    let digest = match structural_descr with
      | Structural_descr.Unknown ->
        (* unserializable type: weakest digest *)
        Digest.string name
      | _ ->
        let key = name, Structural_descr.cleanup structural_descr, reprs in
        Digest.string (Marshal.to_string key [])
    in
    let ty =
      { name = name;
        digest = digest;
        structural_descr = structural_descr;
	abstract = false;
        pp_ml_name = pp_ml_name }
    in
    let full_ty = { ty = ty; reprs = List.map Obj.repr reprs } in
    if !use_obj then Hashtbl.add types name full_ty;
    ty

let add_abstract_types = ref (fun _ _ -> ())

module Abstract(T: sig val name: string end) = struct
  type t
  let ty =
    if !use_obj then (Hashtbl.find types T.name).ty
    else failwith "Cannot call `Type.Abstract' in `no obj' mode"
  let () =
    let p = match Str.split (Str.regexp_string ".") T.name with
      | [] -> 
	failwith "name as argument of `Type.Abstract' must be a valid OCaml \
type name"
      | p :: _ -> p
    in
    !add_abstract_types p T.name
end
    
(* cannot use [Pretty_utils] here *)
let sfprintf fmt =
  let b = Buffer.create 20 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt

let name ty = ty.name
let structural_descr ty = ty.structural_descr
let digest ty = ty.digest
let pp_ml_name ty = ty.pp_ml_name
let ml_name ty = sfprintf "%t" (ty.pp_ml_name Basic)

let unsafe_reprs ty = (Hashtbl.find types ty.name).reprs
let reprs ty =
  if !use_obj then
    let l = try unsafe_reprs ty with Not_found -> assert false in
    List.map Obj.obj l
  else
    []

let set_ml_name ty ml_name =
  let pp = mk_dyn_pp ty.name ml_name in
  ty.pp_ml_name <- pp

let set_name ty name =
  let full_ty = try Hashtbl.find types ty.name with Not_found -> assert false in
  Hashtbl.remove types ty.name;
  ty.name <- name;
  Hashtbl.add types name full_ty

let rec get_embedded_type_names ty =
  let sub_ty = try Tbl.find_all embedded_types ty with Not_found -> [] in
  let sub_ty_names =
    List.fold_left (fun acc ty -> get_embedded_type_names ty @ acc) [] sub_ty
  in
  ty.name :: sub_ty_names

(* ****************************************************************************)
(** {2 Polymorphic type values} *)
(* ****************************************************************************)

module type Polymorphic_input = sig
  val name: 'a t -> string
  val module_name: string
  val structural_descr: Structural_descr.t -> Structural_descr.t
  type 'a t
  val reprs: 'a -> 'a t list
end

module type Polymorphic = sig
  type 'a poly
  val instantiate: 'a t -> 'a poly t * bool
  val is_instance_of: 'a t -> bool
  val get_instance: 'a poly t -> 'a t
end

module Polymorphic(T: Polymorphic_input) = struct

  module Tbl = struct
    let memo : concrete_repr Tbl.t = Tbl.create 17
    let instances: concrete_repr Tbl.t = Tbl.create 17

    let add instance ty =
      Tbl.add memo instance ty;
      Tbl.add instances ty instance;
      Tbl.add embedded_types ty instance

    let find = Tbl.find memo
    let find_instance = Tbl.find instances
    let mem_instance = Tbl.mem memo
  end

  type 'a poly = 'a T.t

  let ml_name from_ty = 
    sfprintf "%s.instantiate %t"
      T.module_name
      (from_ty.pp_ml_name Call)

  let instantiate (ty:'a t) =
    if !use_obj then
      try
        Tbl.find ty, false
      with Not_found ->
        let repr =
          register
            ~name:(T.name ty)
            ~ml_name:(Some (ml_name ty))
            (T.structural_descr ty.structural_descr)
            (List.fold_left
               (fun acc ty -> T.reprs ty @ acc) [] (unsafe_reprs ty))
        in
        Tbl.add ty repr;
        repr, true
    else
      dummy, false

  let is_instance_of = Tbl.mem_instance

  let get_instance (type a) (ty:a poly t) =
    try
      Tbl.find_instance ty
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

end

module type Polymorphic2_input = sig
  val name: 'a t -> 'b t -> string
  val module_name: string
  val structural_descr:
    Structural_descr.t -> Structural_descr.t -> Structural_descr.t
  type ('a, 'b) t
  val reprs: 'a -> 'b -> ('a, 'b) t list
end

module type Polymorphic2 = sig
  type ('a, 'b) poly
  val instantiate: 'a t -> 'b t -> ('a, 'b) poly t * bool
  val is_instance_of: 'a t -> bool
  val get_instance: ('a, 'b) poly t -> 'a t * 'b t
end

module Concrete_pair =
  Hashtbl.Make
    (struct
      type t = concrete_repr * concrete_repr
      let hash (x,y) = Hashtbl.hash (hash x, hash y)
      let equal (x1,y1) (x2,y2) = equal x1 x2 && equal y1 y2
     end)

module Polymorphic2(T: Polymorphic2_input) = struct

  type ('a, 'b) poly = ('a, 'b) T.t

  let memo_tbl : concrete_repr Concrete_pair.t = Concrete_pair.create 17
  let instances : (concrete_repr * concrete_repr) Tbl.t = Tbl.create 17

  let ml_name from_ty1 from_ty2 =
    sfprintf
      "%s.instantiate %t %t"
      T.module_name
      (from_ty1.pp_ml_name Call)
      (from_ty2.pp_ml_name Call)

  let instantiate a b =
    if !use_obj then
      let key = a, b in
      try
        Concrete_pair.find memo_tbl key, false
      with Not_found ->
        let reprs =
          List.fold_left
            (fun acc r1 ->
               List.fold_left
                 (fun acc r2 -> T.reprs r1 r2 @ acc)
                 acc
                 (unsafe_reprs b))
            []
            (unsafe_reprs a)
        in
        let ty =
          register
            ~name:(T.name a b)
            ~ml_name:(Some (ml_name a b))
            (T.structural_descr a.structural_descr b.structural_descr)
            reprs
        in
        Concrete_pair.add memo_tbl key ty;
        Tbl.add instances ty key;
	Tbl.add embedded_types ty a;
	Tbl.add embedded_types ty b;
        ty, true
    else
      dummy, false

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance (type a) (type b) (ty:(a, b) poly t) =
    try
      Tbl.find instances ty
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

end

(* ****************************************************************************)
(** {2 Functional types} *)
(* ****************************************************************************)

let par_ty_name test ty =
  if test ty then Format.sprintf "(%s)" ty.name
  else Format.sprintf "%s" ty.name

module Function = struct

  type ('a, 'b) poly = 'a -> 'b

  type instance =
      { arg: concrete_repr; ret: concrete_repr; label: string option }

  module Memo =
    Hashtbl.Make
      (struct
        type t = instance
        let hash x = Hashtbl.hash (hash x.arg, hash x.ret, x.label)
        let equal x y =
	  equal x.arg y.arg && equal x.ret y.ret && x.label = y.label
       end)
  let memo_tbl : concrete_repr Memo.t = Memo.create 17
  let instances
      : (instance * Obj.t (* default value of the optional label *) option)
      Tbl.t
      = Tbl.create 17

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance (type a) (type b) (ty:(a, b) poly t) =
    try
      let instance, _ = Tbl.find instances ty in
      instance.arg, instance.ret, instance.label
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

  let get_optional_argument (type a) (type b) (ty:(a, b) poly t) =
    if !use_obj then
      try
        match Tbl.find instances ty with
        | _, None -> None
        | _, Some o -> Some (Obj.obj o : unit -> 'b)
      with Not_found ->
        (* static typing ensures than [ty] has already been instantiated. *)
        assert false
    else
      invalid_arg "cannot call `Type.get_optional_argument in the 'no obj' mode"

  let name label ty1 ty2 =
    (match label with None -> "" | Some l -> "~" ^ l ^ ":")
    ^ par_ty_name is_instance_of ty1 ^ " -> " ^ name ty2

  let ml_name label ty1 ty2 =
    sfprintf
      "Datatype.func%s %t %t"
      (match label with None -> "" | Some l -> " ~label:(" ^ l ^ ", None)")
      (ty1.pp_ml_name Call) (ty2.pp_ml_name Call)

  let instantiate ?label (a:'a) (b:'b t): ('a, 'b) poly t * bool =
    if !use_obj then
      let l, o = match label with
        | None -> None, None
        | Some (l, None) -> Some l, None
        | Some (l, Some o) -> Some l , Some (Obj.repr o)
      in
      let key = { arg = a; ret = b; label = l } in
      try
        Memo.find memo_tbl key, false
      with Not_found ->
        let ty =
          (* Do not inline [Types.repr b] in the closure below because
             caml is not able to marshal the closure.
             Sadly don't know exactly why. Seem to have some value tagged as
             abstract in the closure environment. *)
          register
            ~closure:true
            ~name:(name l a b)
            ~ml_name:(Some (ml_name l a b))
            Structural_descr.t_unknown
            (List.map (fun r _ -> r) (unsafe_reprs b))
        in
        Memo.add memo_tbl key ty;
        Tbl.add instances ty (key, o);
	Tbl.add embedded_types ty a;
	Tbl.add embedded_types ty b;
        ty, true
    else
      dummy, false

end

(* ****************************************************************************)
(** {2 Polymorphic3} *)
(* ****************************************************************************)

module type Polymorphic3_input = sig
  val name: 'a t -> 'b t -> 'c t -> string
  val module_name: string
  val structural_descr:
    Structural_descr.t -> Structural_descr.t -> Structural_descr.t ->
    Structural_descr.t
  type ('a, 'b, 'c) t
  val reprs: 'a -> 'b -> 'c -> ('a, 'b, 'c) t list
end

module type Polymorphic3 = sig
  type ('a, 'b, 'c) poly
  val instantiate: 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) poly t * bool
  val is_instance_of: 'a t -> bool
  val get_instance: ('a, 'b, 'c) poly t -> 'a t * 'b t * 'c t
end

module Concrete_triple =
  Hashtbl.Make
    (struct
      type t = concrete_repr * concrete_repr * concrete_repr
      let hash (x,y,z) = Hashtbl.hash (hash x, hash y, hash z)
      let equal (x1,y1,z1) (x2,y2,z2) = 
	equal x1 x2 && equal y1 y2 && equal z1 z2
     end)

module Polymorphic3(T:Polymorphic3_input) = struct

  type ('a, 'b, 'c) poly = ('a, 'b, 'c) T.t

  let memo_tbl: concrete_repr Concrete_triple.t = Concrete_triple.create 17
  let instances
      : (concrete_repr * concrete_repr * concrete_repr) Tbl.t 
      = Tbl.create 17

  let ml_name from_ty1 from_ty2 from_ty3 =
    sfprintf
      "%s.instantiate %t %t %t"
      T.module_name
      (from_ty1.pp_ml_name Call)
      (from_ty2.pp_ml_name Call)
      (from_ty3.pp_ml_name Call)

  let instantiate a b c =
    if !use_obj then
      let key = a, b, c in
      try
        Concrete_triple.find memo_tbl key, false
      with Not_found ->
        let reprs =
          List.fold_left
            (fun acc r1 ->
               List.fold_left
                 (fun acc r2 -> 
		   List.fold_left
		     (fun acc r3 -> T.reprs r1 r2 r3 @ acc)
		     acc
		     (unsafe_reprs c))
                 acc
                 (unsafe_reprs b))
            []
            (unsafe_reprs a)
        in
        let ty =
          register
            ~name:(T.name a b c)
            ~ml_name:(Some (ml_name a b c))
            (T.structural_descr 
	       a.structural_descr 
	       b.structural_descr
	       c.structural_descr)
            reprs
        in
        Concrete_triple.add memo_tbl key ty;
        Tbl.add instances ty key;
	Tbl.add embedded_types ty a;
	Tbl.add embedded_types ty b;
	Tbl.add embedded_types ty c;
        ty, true
    else
      dummy, false

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance (type a) (type b) (type c) (ty:(a, b, c) poly t) =
    try
      Tbl.find instances ty
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

end

(* ****************************************************************************)
(** {2 Polymorphic4} *)
(* ****************************************************************************)

module type Polymorphic4_input = sig
  val name: 'a t -> 'b t -> 'c t -> 'd t -> string
  val module_name: string
  val structural_descr:
    Structural_descr.t -> Structural_descr.t -> Structural_descr.t ->
    Structural_descr.t -> Structural_descr.t
  type ('a, 'b, 'c, 'd) t
  val reprs: 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t list
end

module type Polymorphic4 = sig
  type ('a, 'b, 'c, 'd) poly
  val instantiate: 
    'a t -> 'b t -> 'c t -> 'd t -> ('a, 'b, 'c, 'd) poly t * bool
  val is_instance_of: 'a t -> bool
  val get_instance: ('a, 'b, 'c, 'd) poly t -> 'a t * 'b t * 'c t * 'd t
end

module Concrete_quadruple =
  Hashtbl.Make
    (struct
      type t = concrete_repr * concrete_repr * concrete_repr * concrete_repr
      let hash (x,y,z,t) = Hashtbl.hash (hash x, hash y, hash z, hash t)
      let equal (x1,y1,z1,t1) (x2,y2,z2,t2) = 
	equal x1 x2 && equal y1 y2 && equal z1 z2 && equal t1 t2
     end)

module Polymorphic4(T:Polymorphic4_input) = struct

  type ('a, 'b, 'c, 'd) poly = ('a, 'b, 'c, 'd) T.t

  let memo_tbl
      : concrete_repr Concrete_quadruple.t 
      = Concrete_quadruple.create 17

  let instances
      : (concrete_repr * concrete_repr * concrete_repr * concrete_repr) Tbl.t 
      = Tbl.create 17

  let ml_name from_ty1 from_ty2 from_ty3 from_ty4 =
    sfprintf
      "%s.instantiate %t %t %t %t"
      T.module_name
      (from_ty1.pp_ml_name Call)
      (from_ty2.pp_ml_name Call)
      (from_ty3.pp_ml_name Call)
      (from_ty4.pp_ml_name Call)

  let instantiate a b c d =
    if !use_obj then
      let key = a, b, c, d in
      try
        Concrete_quadruple.find memo_tbl key, false
      with Not_found ->
        let reprs =
          List.fold_left
            (fun acc r1 ->
               List.fold_left
                 (fun acc r2 -> 
		   List.fold_left
		     (fun acc r3 -> 
		       List.fold_left
			 (fun acc r4 -> T.reprs r1 r2 r3 r4 @ acc)
			 acc
			 (unsafe_reprs d))
		     acc
		     (unsafe_reprs c))
                 acc
                 (unsafe_reprs b))
            []
            (unsafe_reprs a)
        in
        let ty =
          register
            ~name:(T.name a b c d)
            ~ml_name:(Some (ml_name a b c d))
            (T.structural_descr 
	       a.structural_descr 
	       b.structural_descr
	       c.structural_descr
	       d.structural_descr)
            reprs
        in
        Concrete_quadruple.add memo_tbl key ty;
        Tbl.add instances ty key;
	Tbl.add embedded_types ty a;
	Tbl.add embedded_types ty b;
	Tbl.add embedded_types ty c;
	Tbl.add embedded_types ty d;
        ty, true
    else
      dummy, false

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance
      (type a) (type b) (type c) (type d) (ty:(a, b, c, d) poly t) =
    try
      Tbl.find instances ty
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

end

(* ****************************************************************************)
(** {2 Heterogeneous Tables} *)
(* ****************************************************************************)

module Ty_tbl(Info: sig type 'a t end) = struct
  type t = Obj.t Tbl.t
  let create x = Tbl.create x
  let add (type a) tbl (ty:a ty) (x:a Info.t) = Tbl.add tbl ty (Obj.repr x)
  let find (type a) tbl (ty:a ty) = (Obj.obj (Tbl.find tbl ty) : a Info.t)
end

module Obj_tbl: sig
  type 'a t
  val create: unit -> 'a t
  val add: 'a t -> 'b ty -> 'b -> 'a -> unit
  val find: 'a t -> 'b ty -> 'b -> 'a
  val mem: 'a t -> 'b ty -> 'b -> bool
  val iter: 'b t -> ('a ty -> 'a -> 'b -> unit) -> unit
end = struct

  module O =
    Hashtbl.Make(struct
      type t = Obj.t
      let equal = (==)
      let hash x =
        if !use_obj then
          (* 0 is correct; trying to do a bit better... *)
          let tag = Obj.tag x in
          if tag = 0 then
            0
          else if tag = Obj.closure_tag then
            (* Buggy code with OCaml 4.01, deactivated for now 
            (* assumes that the first word of a closure does not change in
               any way (even by Gc.compact invokation). *)
               Obj.magic (Obj.field x 0)*)
	       (* to be tested (suggested by Damien D.): add a 'xor 0' *)
(*	       Obj.magic (Obj.field x 0)*)
            0
          else
            Hashtbl.hash x
          else
            0
    end)

  type 'a t = 'a O.t Tbl.t

  let create () = Tbl.create 7

  let add tbl ty k v =
    if !use_obj then
      let tytbl =
        try Tbl.find tbl ty
        with Not_found ->
          let tytbl = O.create 7 in
          Tbl.add tbl ty tytbl;
          tytbl
      in
      O.replace tytbl (Obj.repr k) v

  let find tbl ty k =
    if !use_obj then O.find (Tbl.find tbl ty) (Obj.repr k)
    else invalid_arg "cannot call function 'find' in the 'no obj' mode"

  let mem tbl ty k =
    try
      let objs = Tbl.find tbl ty in
      assert !use_obj;
      O.mem objs (Obj.repr k)
    with Not_found ->
      false

  let iter tbl f = 
    Tbl.iter (fun ty objs -> O.iter (fun o v -> f ty (Obj.obj o) v) objs) tbl

end

module type Heterogeneous_table = sig
  type key
  type 'a info
  type t
  val create: int -> t
  val add: t -> key -> 'a ty -> 'a info -> unit
  exception Unbound_value of string
  exception Incompatible_type of string
  val find: t -> key -> 'a ty -> 'a info
  val iter: (key -> 'a ty -> 'a info -> unit) -> t -> unit
  val fold: (key -> 'a ty -> 'a info -> 'b -> 'b) -> t -> 'b -> 'b
end

module Make_tbl
  (Key: sig include Hashtbl.HashedType val to_string: t -> string end)
  (Info: sig type 'a t end) =
struct

  type key = Key.t
  type 'a info = 'a Info.t
  type data = { ty: concrete_repr; o: Obj.t }
  module H = Hashtbl.Make(Key)
  type t = data H.t

  exception Incompatible_type of string

  let create x = H.create x

  let add tbl s ty x =
    if !use_obj then begin
      let name = Key.to_string s in
      if H.mem tbl s then raise (AlreadyExists name);
      H.add tbl s { ty = ty; o = Obj.repr x }
    end

  exception Unbound_value of string
  let type_error s ty_name ty_name' =
    raise
      (Incompatible_type
         (Format.sprintf "%s has type %s but is used with type %s."
            s ty_name' ty_name))

  let find tbl s ty =
    if !use_obj then
      let name = Key.to_string s in
        let data = 
          try H.find tbl s with Not_found -> raise (Unbound_value name)
        in
        if ty.digest <> data.ty.digest then
          type_error name ty.name data.ty.name;
        Obj.obj data.o
    else
      invalid_arg "cannot call function 'find' in the 'no obj' mode"

  let iter f tbl =
    if !use_obj then H.iter (fun k v -> f k v.ty (Obj.obj v.o)) tbl
    else invalid_arg "cannot call function 'iter' in the 'no obj' mode"

  let fold f tbl acc =
    if !use_obj then H.fold (fun k v acc -> f k v.ty (Obj.obj v.o) acc) tbl acc
    else invalid_arg "cannot call function 'fold' in the 'no obj' mode"

end

module String_tbl =
  Make_tbl
    (struct
       type t = string
       let hash x = Hashtbl.hash x
       let equal : string -> string -> bool = (=)
       let to_string x = x
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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
    { name: string;
      digest: Digest.t;
      structural_descr: Structural_descr.t;
      mutable pp_ml_name: precedence -> Format.formatter -> unit }

(* phantom type *)
type 'a t = concrete_repr
type 'a ty = 'a t

(* non-phantom type: the type variable is used here *)
type 'a full_t = { ty: 'a t; reprs: 'a list }

let types : (string (* name *), Obj.t full_t) Hashtbl.t = Hashtbl.create 97

let dummy =
  { name = "";
    digest = "";
    structural_descr = Structural_descr.Unknown;
    pp_ml_name = fun _ _ -> assert false }

let mk_dyn_pp name = function
  | None ->
    let pp fmt = Format.fprintf fmt "Type.get %S" name in
    (fun p fmt -> par p Call fmt pp)
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
        pp_ml_name = pp_ml_name }
    in
    let full_ty = { ty = ty; reprs = List.map Obj.repr reprs } in
    if !use_obj then Hashtbl.add types name full_ty;
    ty

module Abstract(T: sig val name: string end) = struct
  type t
  let ty =
    if !use_obj then (Hashtbl.find types T.name).ty
    else failwith "Cannot call `Type.get' in `no obj' mode"
end

let name ty = ty.name
let structural_descr ty = ty.structural_descr
let digest ty = ty.digest
let pp_ml_name ty = ty.pp_ml_name
let ml_name ty =
  let b = Buffer.create 97 in
  Format.bprintf b "%t" (ty.pp_ml_name Basic);
  Buffer.contents b

let unsafe_reprs ty = (Hashtbl.find types ty.name).reprs
let reprs ty =
  let l = try unsafe_reprs ty with Not_found -> Format.printf "Type %s@." 
    ty.name ;assert false in
  List.map Obj.obj l

let set_ml_name ty ml_name =
  let pp = mk_dyn_pp ty.name ml_name in
  ty.pp_ml_name <- pp

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
      Tbl.add instances ty instance

    let find = Tbl.find memo
    let find_instance = Tbl.find instances
    let mem_instance = Tbl.mem memo
  end

  type 'a poly = 'a T.t

  let ml_name from_ty =
    let b = Buffer.create 31 in
    Format.bprintf b "%s.instantiate %t"
      T.module_name
      (from_ty.pp_ml_name Call);
    Buffer.contents b

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

  let get_instance (ty:'a poly t) =
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
    let b = Buffer.create 31 in
    Format.bprintf b "%s.instantiate %t %t"
        T.module_name
        (from_ty1.pp_ml_name Call)
        (from_ty2.pp_ml_name Call);
    Buffer.contents b

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
        ty, true
    else
      dummy, false

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance (ty:('a, 'b) poly t) =
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
        let hash x =
          Hashtbl.hash (hash x.arg, hash x.ret, x.label)
        let equal x y =
          equal x.arg y.arg && equal x.ret y.ret && x.label = y.label
       end)
  let memo_tbl : concrete_repr Memo.t = Memo.create 17
  let instances
      : (instance * Obj.t (* default value of the optional label *) option)
      Tbl.t
      = Tbl.create 17

  let repr (_:'a) (b:'b) : 'a -> 'b = fun _ -> b

  let is_instance_of ty = Tbl.mem instances ty

  let get_instance (ty:('a, 'b) poly t) =
    try
      let instance, _ = Tbl.find instances ty in
      instance.arg, instance.ret, instance.label
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

  let get_optional_argument (ty:('a, 'b) poly t) =
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
    let b = Buffer.create 97 in
    Format.bprintf
      b
      "Datatype.func%s %t %t"
      (match label with None -> "" | Some l -> " ~label:(" ^ l ^ ", None)")
      (ty1.pp_ml_name Call) (ty2.pp_ml_name Call);
    Buffer.contents b

  let instantiate ?label (a:'a) (b:'b t) =
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
            Structural_descr.Unknown
            (List.map (fun r _ -> r) (unsafe_reprs b))
        in
        Memo.add memo_tbl key ty;
        Tbl.add instances ty (key, o);
        ty, true
    else
      dummy, false

end

let func ?label x y = fst (Function.instantiate ?label x y)

let optlabel_func lab dft = func ~label:(lab, Some dft)

let func2 ?label1 ty1 ?label2 ty2 ty_ret =
  func ?label:label1 ty1 (func ?label:label2 ty2 ty_ret)

let func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 ty_ret =
  func2 ?label1 ty1 ?label2 ty2 (func ?label:label3 ty3 ty_ret)

let func4 ?label1 ty1 ?label2 ty2 ?label3 ty3 ?label4 ty4 ty_ret =
  func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 (func ?label:label4 ty4 ty_ret)

(* ****************************************************************************)
(** {2 Heterogeneous Tables} *)
(* ****************************************************************************)

module Ty_tbl(Info: sig type 'a t end) = struct
  type t = Obj.t Tbl.t
  let create x = Tbl.create x
  let add tbl (ty:'a ty) (x:'a Info.t) = Tbl.add tbl ty (Obj.repr x)
  let find tbl (ty:'a ty) = (Obj.obj (Tbl.find tbl ty) : 'a Info.t)
end

module Obj_tbl: sig
  type 'a t
  val create: unit -> 'a t
  val add: 'a t -> 'b ty -> 'b -> 'a -> unit
  val find: 'a t -> 'b ty -> 'b -> 'a
  val mem: 'a t -> 'b ty -> 'b -> bool
  val iter: 'a t -> ('a -> unit) -> unit
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
            (* assumes that the first word of a closure does not change in
               any way (even by Gc.compact invokation). *)
            Obj.magic (Obj.field x 0)
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

  let iter tbl f = Tbl.iter (fun _ objs -> O.iter (fun _ -> f) objs) tbl

end

module Obj_weak: sig
  type t
  val create: unit -> t
  val add: t -> 'b ty -> 'b -> unit
  val mem: t -> 'b ty -> 'b -> bool
end = struct

  module O =
    Weak.Make(struct
      (* we use the weak hash tbl as a weak list since we cannot use [(==)] in
         weak hash tables. See documentation of module Weak. *)
      type t = Obj.t
      let equal _ _ = false
      let hash _ = 0
    end)

  type t = O.t Tbl.t

  let create () = Tbl.create 7

  let add tbl ty k =
    if !use_obj then
      let tytbl =
        try Tbl.find tbl ty
        with Not_found ->
          let tytbl = O.create 7 in
          Tbl.add tbl ty tytbl;
          tytbl
      in
      O.add tytbl (Obj.repr k)

  (* linear in the number of values of type [ty] *)
  let mem tbl ty k =
    try
      let objs = Tbl.find tbl ty in
      assert !use_obj;
      try
        O.iter (fun x -> if x == Obj.repr k then raise Exit) objs;
        false
      with Exit ->
        true
    with Not_found ->
      false

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

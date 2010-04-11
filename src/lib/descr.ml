(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

type packed = Unmarshal.t

type 'a t = { descr: packed; ty: 'a Type.t }

let mk ty s = { descr = s; ty = ty }
let pack x = x.descr

let abstract ty = mk ty Unmarshal.Abstract
let structure ty a = mk ty (Unmarshal.Structure a)
let sum ty a = structure ty (Unmarshal.Sum a)
let array ty a = structure ty (Unmarshal.Array a)
let dependent_pair ty_b a f =
  structure
    (Type.couple a.ty ty_b)
    (Unmarshal.Dependent_pair (a.descr, (fun v -> (f (Obj.obj v)).descr)))

let transform x f =
  mk x.ty (Unmarshal.Transform(x.descr, (fun x -> Obj.repr (f (Obj.obj x)))))
let return x f = mk x.ty (Unmarshal.Return(x.descr, (fun x -> Obj.repr (f x))))
let dynamic ty f = mk ty (Unmarshal.Dynamic (fun () -> (f ()).descr))

let t_int = mk Type.int Unmarshal.t_int
let t_string = mk Type.string Unmarshal.t_string
let t_float = mk Type.float Unmarshal.t_float
let t_bool = mk Type.bool Unmarshal.t_bool
let t_int32 = mk Type.int32 Unmarshal.t_int32
let t_int64 = mk Type.int64 Unmarshal.t_int64
let t_nativeint = mk Type.nativeint Unmarshal.t_nativeint

let t_record ty a = mk ty (Unmarshal.t_record a)
let t_tuple ty a = mk ty (Unmarshal.t_tuple a)
let t_couple x y =
  mk (Type.couple x.ty y.ty) (Unmarshal.t_tuple [| x.descr; y.descr |])
let t_list x = mk (Type.list x.ty) (Unmarshal.t_list x.descr)
let t_ref x = mk (Type.t_ref x.ty) (Unmarshal.t_ref x.descr)
let t_option x = mk (Type.option x.ty) (Unmarshal.t_option x.descr)
let t_hashtbl_unchangedhashs k v =
  mk
    (Type.hashtbl k.ty v.ty)
    (Unmarshal.t_hashtbl_unchangedhashs k.descr v.descr)
let t_hashtbl_changedhashs create add k v =
  mk
    (Type.hashtbl k.ty v.ty)
    (Unmarshal.t_hashtbl_changedhashs create add k.descr v.descr)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

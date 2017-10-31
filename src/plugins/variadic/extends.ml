(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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



module Typ = struct
  let attributes_less_equal t1 t2 =
    let t1 = Cil.typeDeepDropAllAttributes t1 in
    let t2 = Cil.typeDeepDropAllAttributes t2 in
    Cil_datatype.Typ.equal t1 t2

  let params typ =
    match Cil.unrollType typ with
    | TFun (_,args,_,_) -> Cil.argsToList args
    | _ -> invalid_arg "params"

  let params_types typ =
    List.map (fun (_,typ,_) -> typ) (params typ)

  let params_count typ =
    List.length (params typ)

  let is_variadic typ =
    match Cil.unrollType typ with
    | TFun (_, _, b, _) -> b
    |  _ -> false
end

module Cil = struct
  include Cil

  let ptrType typ = TPtr (typ, [])
  let constPtrType typ = TPtr (typ, [Attr("const", [])])

  let shortType = TInt(IShort, [])
  let ushortType = TInt(IUShort, [])

  let shortPtrType = ptrType shortType
  let ushortPtrType = ptrType ushortType
  let longPtrType = ptrType longType
  let ulongPtrType = ptrType ulongType
  let longlongPtrType = ptrType longLongType
  let ulonglongPtrType = ptrType ulongLongType
  let doublePtrType = ptrType doubleType

  let signedIntegerTypes = [Cil.charType; shortType; Cil.intType;
			    Cil.longType; longLongType]
  let unsignedIntegerTypes = [ucharType; ushortType; Cil.uintType;
			      Cil.ulongType; Cil.ulongLongType]
  let signedIntegerPtrTypes = [Cil.charPtrType; shortPtrType; Cil.intPtrType;
			       longPtrType; longlongPtrType]
  let unsignedIntegerPtrTypes = [ucharPtrType; ushortPtrType; Cil.uintPtrType;
				 ulongPtrType; ulonglongPtrType]

  let signed_integers_ranking =
    Extlib.mapi (fun i t -> (t, i)) signedIntegerTypes
  let unsigned_integers_ranking =
    Extlib.mapi (fun i t -> (t, i)) unsignedIntegerTypes

  let is_signed_integer_type t = List.mem t signedIntegerTypes
  let is_unsigned_integer_type t = List.mem t unsignedIntegerTypes
  let is_integer_type t = is_signed_integer_type t || is_unsigned_integer_type t

  let is_signed_ptr_integer_type t = List.mem t signedIntegerPtrTypes
  let is_unsigned_ptr_integer_type t = List.mem t unsignedIntegerPtrTypes
  let is_integer_ptr_type t =
    is_signed_ptr_integer_type t || is_unsigned_ptr_integer_type t

  let integer_ranking_comp t1 t2 =
    let rt1, rt2 =
      if is_signed_integer_type t1 && is_signed_integer_type t2 then
	List.assoc t1 signed_integers_ranking,
	List.assoc t2 signed_integers_ranking
      else if is_unsigned_integer_type t1 && is_unsigned_integer_type t2 then
	List.assoc t1 unsigned_integers_ranking,
	List.assoc t2 unsigned_integers_ranking
      else
	raise (Invalid_argument "rank_comp") in
    rt1 - rt2

  let integer_promotion t1 t2 =
    try
      (integer_ranking_comp t1 t2) < 0
    with
      Invalid_argument _ -> false

  let is_folded_zero e = Cil.isZero (Cil.constFold false e)

  let is_function vi = match vi.vtype with
    | TFun _ -> true
    | _ -> false

  let is_variadic_function vi =
    Typ.is_variadic vi.vtype

  let get_fundec_return_type fd = match fd.svar.vtype with
    | TFun(rt, _, _, _) -> rt
    | _ -> Options.Self.fatal "Varinfo of fundec does not have function type."

  let get_kf_attributes kf = match kf.fundec with
    | Definition (fd, _) -> fd.svar.vattr
    | Declaration (_, vi, _, _) -> vi.vattr

  let get_inst_loc = Cil_datatype.Instr.loc

  let get_stmt_loc = Cil_datatype.Stmt.loc
end

module List = struct
  include List

  let rec make n a =
    if n <= 0 then []
    else a :: make (n - 1) a

  let to_scalar = function
  | [a] -> a
  | _ -> failwith "to_scalar"

  let of_opt = function
  | None -> []
  | Some x -> [x]

  let to_opt = function
  | [] -> None
  | [a] -> Some a
  | _ -> failwith "to_opt"

  let first = function
  | [] -> failwith "first"
  | a :: _ -> a

  exception EmptyList

  let rec last = function
  | [] -> raise EmptyList
  | [a] -> a
  | _ :: l -> last l

  let rec take n l =
    if n <= 0 then []
    else match l with
    | [] -> []
    | a :: l -> a :: take (n - 1) l

  let rec drop n l =
    if n <= 0 then l
    else match l with
    | [] -> []
    | _ :: l -> drop (n - 1) l

  let rec break n l =
    if n <= 0 then ([], l)
    else match l with
    | [] -> ([], [])
    | a :: l ->
        let l1, l2 = break (n - 1) l in
        (a :: l1, l2)

  let rec filter_map f = function
  | [] -> []
  | a :: l -> match f a with
    | Some r -> r :: filter_map f l
    | None -> filter_map f l

  let iteri f l =
    let i = ref 0 in
    iter (fun a -> f !i a; incr i) l

  let mapi f l =
    let i = ref 0 in
    map (fun a -> let r = f !i a in incr i; r) l

  let rev_mapi f l =
    let i = ref 0 in
    let rec aux acc = function
    | [] -> acc
    | a :: l -> let a' = f !i a in incr i; aux (a' :: acc) l
    in aux [] l

  let iteri2 f l1 l2 =
    let i = ref 0 in
    let rec aux l1 l2 = match l1, l2 with
    | [], [] -> ()
    | a1 :: l1, a2 :: l2 -> f !i a1 a2; incr i; aux l1 l2
    | _, _ -> invalid_arg "List.iteri2"
    in
    aux l1 l2

  let mapi2 f l1 l2 =
    let i = ref 0 in
    let rec aux l1 l2 = match l1, l2 with
    | [], [] -> []
    | a1 :: l1, a2 :: l2 -> let r = f !i a1 a2 in incr i; r :: aux l1 l2
    | _, _ -> invalid_arg "List.mapi2"
    in
    aux l1 l2

  let reduce_left f l =
    let rec aux acc = function
    | [] -> acc
    | a :: l -> aux (f acc a) l
    in match l with
    | [] -> failwith "reduce"
    | a :: l -> aux a l

  let rec reduce_right f = function
  | [] -> failwith "reduce"
  | [a] -> a
  | a :: l -> f a (reduce_right f l)

  let map_fold_left f acc l =
    let rec aux acc r = function
    | [] -> List.rev r, acc
    | a :: l ->
        let a, acc = f acc a in
        aux acc (a :: r) l
    in
    aux acc [] l


  let ifind f l =
    let i = ref 0 in
    let rec aux = function
      | [] -> raise Not_found
      | a :: l -> if not (f a) then (incr i; aux l)
    in aux l; !i


  let rec unique_sorted cmp = function
  | a1 :: a2 :: l when cmp a1 a2 = 0 -> unique_sorted cmp (a2 :: l)
  | [] -> []
  | a :: l -> a :: unique_sorted cmp l

  let sort_unique cmp l =
    unique_sorted cmp (sort cmp l)


  let replace i v =
    Extlib.mapi (fun i' v' -> if i = i' then v else v')
end

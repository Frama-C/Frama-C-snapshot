(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- C-Types                                                            --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

module WpLog = Wp_parameters

type c_int =
  | UInt8
  | SInt8
  | UInt16
  | SInt16
  | UInt32
  | SInt32
  | UInt64
  | SInt64

let compare_c_int : c_int -> c_int -> _ = Extlib.compare_basic

let signed  = function
  | UInt8 | UInt16 | UInt32 | UInt64 -> false
  | SInt8 | SInt16 | SInt32 | SInt64 -> true

let i_sizeof = function
  | UInt8  | SInt8  -> 8
  | UInt16 | SInt16 -> 16
  | UInt32 | SInt32 -> 32
  | UInt64 | SInt64 -> 64

let make_c_int signed = function
  | 1 -> if signed then SInt8 else UInt8
  | 2 -> if signed then SInt16 else UInt16
  | 4 -> if signed then SInt32 else UInt32
  | 8 -> if signed then SInt64 else UInt64
  | size -> WpLog.not_yet_implemented "%d-bits integers" size

let c_int ikind =
  let mach = Cil.theMachine.Cil.theMachine in
  match ikind with
    | IBool -> make_c_int false mach.sizeof_int
    | IChar -> if mach.char_is_unsigned then UInt8 else SInt8
    | ISChar -> SInt8
    | IUChar -> UInt8
    | IInt -> make_c_int true mach.sizeof_int
    | IUInt -> make_c_int false mach.sizeof_int
    | IShort -> make_c_int true mach.sizeof_short
    | IUShort -> make_c_int false mach.sizeof_short
    | ILong -> make_c_int true mach.sizeof_long
    | IULong -> make_c_int false mach.sizeof_long
    | ILongLong -> make_c_int true mach.sizeof_longlong
    | IULongLong -> make_c_int false mach.sizeof_longlong

(* Bounds of an integer according to c_int ti :
   An integer i : i \in [c_int_bounds ti] if
    [c_int_bounds ti] = (min,max) then min <=i<max.*)
let c_int_bounds = function
  | UInt8 -> (Big_int.zero_big_int,Big_int.big_int_of_string "256")
  | SInt8 -> (Big_int.big_int_of_string "-128",
              Big_int.big_int_of_string "128")
  | UInt16 -> (Big_int.zero_big_int,
               Big_int.big_int_of_string "65536")
  | SInt16 -> (Big_int.big_int_of_string "-32768",
               Big_int.big_int_of_string "32768")
  | UInt32 -> (Big_int.zero_big_int,
               Big_int.big_int_of_string "4294967296")
  | SInt32 -> (Big_int.big_int_of_string "-2147483648" ,
               Big_int.big_int_of_string "2147483648" )
  | UInt64 -> (Big_int.zero_big_int ,
               Big_int.big_int_of_string "18446744073709551616" )
  | SInt64 -> (Big_int.big_int_of_string  "-9223372036854775808" ,
               Big_int.big_int_of_string "9223372036854775808" )

let c_int_all =   
  [ UInt8 ; SInt8 ; UInt16 ; SInt16 ; UInt32 ; SInt32 ; UInt64 ; SInt64 ]

let c_bool () = c_int IInt
let c_char () = c_int IChar
let c_ptr () =
  make_c_int false Cil.theMachine.Cil.theMachine.sizeof_ptr

let sub_c_int t1 t2 =
  if (signed t1 = signed t2) then i_sizeof t1 <= i_sizeof t2
 else (not(signed t1) && (i_sizeof t1 < i_sizeof t2))


type c_float =
  | Float16
  | Float32
  | Float64
  | Float96
  | Float128

let compare_c_float : c_float -> c_float -> _ = Extlib.compare_basic

let f_sizeof = function
  | Float16 -> 16
  | Float32 -> 32
  | Float64 -> 64
  | Float96 -> 96
  | Float128 -> 128

let make_c_float = function
  | 2 -> Float16
  | 4 -> Float32
  | 8 -> Float64
  | 12 -> Float96
  | 16 -> Float128
  | size -> WpLog.not_yet_implemented "%d-bits floats" size

let c_float fkind =
  let mach = Cil.theMachine.Cil.theMachine in
  match fkind with
    | FFloat -> make_c_float mach.sizeof_float
    | FDouble -> make_c_float mach.sizeof_double
    | FLongDouble -> make_c_float mach.sizeof_longdouble

let sub_c_float f1 f2 = f_sizeof f1 <= f_sizeof f2

(* Array objects, with both the head view and the flatten view. *)

type arrayflat = {
  arr_size     : int64;  (* number of elements in the array *)
  arr_dim      : int ;   (* number of dimensions in the array *)
  arr_cell     : typ ;   (* type of elementary cells of the flatten array *)
  arr_cell_nbr : int64 ; (* number of elementary cells in the flatten array *)
}

type arrayinfo = {
  arr_element : typ ;    (* type of the elements of the array *)
  arr_flat : arrayflat option;
}

(* Type of variable, inits, field or assignable values. *)
type c_object =
  | C_int of c_int
  | C_float of c_float
  | C_pointer of typ
  | C_comp of compinfo
  | C_array of arrayinfo

(* -------------------------------------------------------------------------- *)
(* --- Memoization                                                        --- *)
(* -------------------------------------------------------------------------- *)

let idx = function
  | UInt8 -> 0
  | SInt8 -> 1
  | UInt16 -> 2
  | SInt16 -> 3
  | UInt32 -> 4
  | SInt32 -> 5
  | UInt64 -> 6
  | SInt64 -> 7

let imemo f =
  let m = Array.create 8 None in
  fun i ->
    let k = idx i in
    match m.(k) with
      | Some r -> r
      | None ->
          let r = f i in m.(k) <- Some r ; r

let fdx = function
  | Float16 -> 0
  | Float32 -> 1
  | Float64 -> 2
  | Float96 -> 3
  | Float128 -> 4

let fmemo f =
  let m = Array.create 8 None in
  fun z ->
    let k = fdx z in
    match m.(k) with
      | Some r -> r
      | None ->
          let r = f z in m.(k) <- Some r ; r

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_int fmt i = Format.fprintf fmt "%cint%d"
  (if signed i then 's' else 'u') (i_sizeof i)

let pp_float fmt f = Format.fprintf fmt "float%d" (f_sizeof f)

let pp_object fmt = function
  | C_int i -> pp_int fmt i
  | C_float f -> pp_float fmt f
  | C_pointer _ -> Format.pp_print_string fmt "obj-pointer"
  | C_comp _ -> Format.pp_print_string fmt "obj-struct/union"
  | C_array _ -> Format.pp_print_string fmt "obj-array"

(* -------------------------------------------------------------------------- *)
(* --- Array Info                                                         --- *)
(* -------------------------------------------------------------------------- *)

let char c =
  match Cil.charConstToInt c with
    | CInt64(k,_,_) -> My_bigint.to_int64 k
    | _ -> WpLog.fatal "char-const-to-int"

let constant e =
  match (Cil.constFold true e).enode with
    | Const(CInt64(k,_,_)) -> My_bigint.to_int64 k
    | Const(CChr c) -> char c
    | _ -> WpLog.fatal "Non-constant expression (%a)" !Ast_printer.d_exp e

let get_int e =
  match (Cil.constFold true e).enode with
    | Const(CInt64(k,_,_)) -> Some (My_bigint.to_int64 k)
    | Const(CChr c) -> Some (char c)
    | _ -> None

let dimension t =
  let rec flat k d = function
    | TNamed _ as t -> flat k d (Cil.unrollType t)
    | TArray(ty,Some e,_,_) ->
      flat (succ k) (Int64.mul d (constant e)) ty
    | te -> k , d , te
  in flat 1 Int64.one t

(* -------------------------------------------------------------------------- *)
(* --- Value State_builder.                                               --- *)
(* -------------------------------------------------------------------------- *)

let is_void typ =
  match Cil.unrollType typ with
    | TVoid _ -> true
    | _ -> false

let object_of  typ =
   match Cil.unrollType typ with
    | TInt(i,_) -> C_int (c_int i)
    | TFloat(f,_) -> C_float (c_float f)
    | TPtr(typ,_) ->
        begin
          match Cil.unrollType typ with
            | TVoid _ -> C_pointer (TInt (IChar,[]))
            | _ -> C_pointer typ
        end
    | TFun _ as t -> C_pointer t
    | TEnum ({ekind=i},_) -> C_int (c_int i)
    | TComp (comp,_,_) -> C_comp comp
    | TArray (typ_elt,e_opt,_,_) ->
        begin
          match e_opt with
            | None ->
                C_array {
                  arr_element = typ_elt;
                  arr_flat = None;
                }

            | Some e ->
                let dim,ncells,ty_cell = dimension typ in
                C_array {
                  arr_element = typ_elt ;
                  arr_flat = Some {
                    arr_size = constant e ;
                    arr_dim = dim ;
                    arr_cell = ty_cell ;
                    arr_cell_nbr = ncells ;
                  }
                }
        end
    | TBuiltin_va_list _ ->
        WpLog.not_yet_implemented "valiadyc type"
    | TVoid _ ->
        WpLog.fatal ~current:true "void object"
    | TNamed _  ->
        WpLog.fatal "non-unrolled named type (%a)" !Ast_printer.d_type typ

let object_of_pointed = function
    C_int _ | C_float _ | C_comp _ as o ->
      Wp_parameters.fatal
        "object_of_pointed called on non-pointer %a@." pp_object o
  | C_array info -> object_of info.arr_element
  | C_pointer typ -> object_of typ


let object_of_array_elem = function
  | C_array arr -> object_of arr.arr_element
  | o -> Wp_parameters.fatal 
       "object_of_array_elem called on non-array %a." pp_object o
       
let no_infinite_array = function 
    | C_array {arr_flat = None} -> false 
    | _ -> true

let array_dim arr =
  match arr.arr_flat with
    | Some f -> object_of f.arr_cell , f.arr_dim - 1
    | None ->
	let rec collect_dim arr n =
	  match object_of arr.arr_element with
	    | C_array arr -> collect_dim arr (succ n)
	    | te -> te,n
	in collect_dim arr 1

let int64_max a b =
  if Int64.compare a b < 0 then b else a

let rec sizeof_object = function
 | C_int i -> Int64.of_int (i_sizeof i)
 | C_float f -> Int64.of_int (f_sizeof f)
 | C_pointer _ty -> Int64.of_int (i_sizeof (c_ptr()))
 | C_comp cinfo ->
     let merge = if cinfo.cstruct then Int64.add else int64_max in
     List.fold_left
       (fun sz f -> merge sz (sizeof_object (object_of f.ftype)))
       Int64.zero cinfo.cfields
 | C_array ainfo ->
     begin
       match ainfo.arr_flat with
         | Some a -> Int64.mul
             (sizeof_object(object_of a.arr_cell))  a.arr_cell_nbr
         | None -> WpLog.not_yet_implemented "Sizeof unknown-size array"
     end

let field_offset f =
  let rec acc ofs f = function
    | [] -> Wp_parameters.fatal "[field_offset] not found field %s" f.fname ; 
    | fi::m ->
        if Cil_datatype.Fieldinfo.equal f fi then ofs else
          let sf = sizeof_object (object_of fi.ftype) in
          acc (Int64.add ofs sf) f m
  in acc Int64.zero f f.fcomp.cfields

(* Conforms to @ C-ISO § 6.3.1.8    *)
(* If same sign => greater rank.    *)
(* If different:                    *)
(* Case 1:                          *)
(*   rank(unsigned) >= rank(signed) *)
(*   then convert to unsigned       *)
(* Case 2:                          *)
(*   domain(unsigend) contains      *)
(*   domain(signed)                 *)
(*   then convert to signed         *)
(* Otherwise:                       *)
(*   both are converted to unsiged  *)
(*                                  *)
(* Case 2 is actually the negative  *)
(* of Case 1, and both simplifies   *)
(* into converting to the operand   *)
(* with greater rank, whatever      *)
(* their sign.                      *)

let i_convert t1 t2 = if i_sizeof t1 < i_sizeof t2 then t2 else t1
let f_convert t1 t2 = if f_sizeof t1 < f_sizeof t2 then t2 else t1

let promote a1 a2 =
  match a1 , a2 with
    | C_int i1 , C_int i2 -> C_int (i_convert i1 i2)
    | C_float f1 , C_float f2 -> C_float (f_convert f1 f2)
    | C_int _ , C_float _ -> a2
    | C_float _ , C_int _ -> a1
    | _ -> WpLog.not_yet_implemented
        "promotion between arithmetics and pointer types"

(* ------------------------------------------------------------------------ *)
(* --- Comparable                                                       --- *)
(* ------------------------------------------------------------------------ *)

module AinfoComparable = struct
  type t = arrayinfo
  let hash a = Typ.hash a.arr_element
  let equal a b =
    Typ.equal a.arr_element b.arr_element &&
      (match a.arr_flat , b.arr_flat with
          | Some a , Some b -> Int64.compare a.arr_size b.arr_size = 0
          | None , None -> true
          | _ -> false)
  let compare a b =
    let c = Typ.compare a.arr_element b.arr_element in
    if c <> 0 then c
    else match a.arr_flat , b.arr_flat with
        | Some a , Some b -> Int64.compare a.arr_size b.arr_size
        | None , Some _ -> (-1)
        | Some _ , None -> 1
        | None , None -> 0
end

let hash = function
  | C_int _ -> 3
  | C_float _ -> 5
  | C_pointer _ -> 7
  | C_comp c -> 11 * Compinfo.hash c
  | C_array a -> 13 * AinfoComparable.hash a

let equal a b =
  match a,b with
    | C_int i, C_int i' -> i=i'
    | C_float f , C_float f' -> f=f'
    | C_pointer te , C_pointer te' -> Typ.equal te te'
    | C_comp c , C_comp c' -> Compinfo.equal c c'
    | C_array a , C_array a' -> AinfoComparable.equal a a'
    | _ -> false

let basetype = function
  | C_int _ -> 1
  | C_float _ -> 2
  | C_pointer _ -> 3
  | C_comp c -> if c.cstruct then 4 else 5
  | C_array _ -> Wp_parameters.fatal "[basetype] of an array"

let compare a b =
  match a,b with
  | C_int i, C_int i' -> compare_c_int i i'
  | C_float f , C_float f' -> compare_c_float f f'
  | C_pointer te , C_pointer te' -> Typ.compare te te'
  | C_comp c , C_comp c' -> Compinfo.compare c c'
  | C_array a , C_array a' -> AinfoComparable.compare a a'
  | _ ->
    let k1 = basetype a in
    let k2 = basetype b in
    assert (k1<>k2) ;
    k1 - k2

let rec basename = function
  | C_int i -> Pretty_utils.sfprintf "%a" pp_int i
  | C_float f -> Pretty_utils.sfprintf "%a" pp_float f
  | C_pointer _ -> "pointer"
  | C_comp c -> c.cname
  | C_array a ->
      let te = basename (object_of a.arr_element) in
      match a.arr_flat with
        | None -> te ^ "_array"
        | Some f -> te ^ "_" ^ Int64.to_string f.arr_size

let rec pretty fmt = function
  | C_int i -> pp_int fmt i
  | C_float f -> pp_float fmt f
  | C_pointer _ -> Format.pp_print_string fmt "pointer"
  | C_comp c -> Format.pp_print_string fmt c.cname
  | C_array a ->
      let te = object_of a.arr_element in
      match a.arr_flat with
        | None -> Format.fprintf fmt "%a[]" pretty te
        | Some f -> Format.fprintf fmt "%a[%s]" pretty te
            (Int64.to_string f.arr_size)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

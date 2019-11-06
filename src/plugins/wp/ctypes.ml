(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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
  | CBool
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
  | CBool -> false
  | UInt8 | UInt16 | UInt32 | UInt64 -> false
  | SInt8 | SInt16 | SInt32 | SInt64 -> true

let i_bits = function
  | CBool -> 1
  | UInt8  | SInt8  -> 8
  | UInt16 | SInt16 -> 16
  | UInt32 | SInt32 -> 32
  | UInt64 | SInt64 -> 64

let i_bytes = function
  | CBool -> 1
  | UInt8  | SInt8  -> 1
  | UInt16 | SInt16 -> 2
  | UInt32 | SInt32 -> 4
  | UInt64 | SInt64 -> 8

let make_c_int signed = function
  | 1 -> if signed then SInt8 else UInt8
  | 2 -> if signed then SInt16 else UInt16
  | 4 -> if signed then SInt32 else UInt32
  | 8 -> if signed then SInt64 else UInt64
  | size -> WpLog.not_yet_implemented "%d-bytes integers" size

let is_char = function
  | UInt8 -> Cil.theMachine.Cil.theMachine.char_is_unsigned
  | SInt8 -> not Cil.theMachine.Cil.theMachine.char_is_unsigned
  | UInt16 | SInt16
  | UInt32 | SInt32
  | UInt64 | SInt64
  | CBool -> false

let c_int ikind =
  let mach = Cil.theMachine.Cil.theMachine in
  match ikind with
  | IBool -> CBool
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

let c_bool () = c_int IBool
let c_char () = c_int IChar

let p_bytes () = Cil.theMachine.Cil.theMachine.sizeof_ptr
let p_bits () = 8 * p_bytes ()

let c_ptr () = make_c_int false (p_bytes ())

let sub_c_int t1 t2 =
  if (signed t1 = signed t2) then i_bits t1 <= i_bits t2
  else (not(signed t1) && (i_bits t1 < i_bits t2))

type c_float =
  | Float32
  | Float64

let compare_c_float : c_float -> c_float -> _ = Extlib.compare_basic

let f_bytes = function
  | Float32 -> 4
  | Float64 -> 8

let f_bits = function
  | Float32 -> 32
  | Float64 -> 64

let make_c_float = function
  | 4 -> Float32
  | 8 -> Float64
  | size -> WpLog.not_yet_implemented "%d-bits floats" (8*size)

let c_float fkind =
  let mach = Cil.theMachine.Cil.theMachine in
  match fkind with
  | FFloat -> make_c_float mach.sizeof_float
  | FDouble -> make_c_float mach.sizeof_double
  | FLongDouble -> make_c_float mach.sizeof_longdouble

let equal_float f1 f2 = f_bits f1 = f_bits f2

(* Array objects, with both the head view and the flatten view. *)

type arrayflat = {
  arr_size     : int ;  (* number of elements in the array *)
  arr_dim      : int ;  (* number of dimensions in the array *)
  arr_cell     : typ ;  (* type of elementary cells of the flatten array *)
  arr_cell_nbr : int ;  (* number of elementary cells in the flatten array *)
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
  | CBool -> 8

let i_memo f =
  let m = Array.make 9 None in
  fun i ->
    let k = idx i in
    match m.(k) with
    | Some r -> r
    | None -> let r = f i in m.(k) <- Some r ; r

let fdx = function
  | Float32 -> 0
  | Float64 -> 1

let f_memo f =
  let m = Array.make 2 None in
  fun z ->
    let k = fdx z in
    match m.(k) with
    | Some r -> r
    | None -> let r = f z in m.(k) <- Some r ; r

let i_iter f =
  List.iter f [CBool;UInt8;SInt8;UInt16;SInt16;UInt32;SInt32;UInt64;SInt64]

let f_iter f =
  List.iter f [Float32;Float64]

(* -------------------------------------------------------------------------- *)
(* --- Bounds                                                             --- *)
(* -------------------------------------------------------------------------- *)

let i_bounds i =
  if signed i then
    let m = Integer.two_power_of_int (i_bits i - 1) in
    Integer.neg m , Integer.pred m
  else
    let m = Integer.two_power_of_int (i_bits i) in
    Integer.zero , Integer.pred m

let bounds i = i_memo i_bounds i

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_int fmt i =
  if i = CBool then Format.pp_print_string fmt "bool"
  else Format.fprintf fmt "%cint%d" (if signed i then 's' else 'u') (i_bits i)

let pp_float fmt f = Format.fprintf fmt "float%d" (f_bits f)

let pp_object fmt = function
  | C_int i -> pp_int fmt i
  | C_float f -> pp_float fmt f
  | C_pointer _ -> Format.pp_print_string fmt "obj-pointer"
  | C_comp _ -> Format.pp_print_string fmt "obj-struct/union"
  | C_array _ -> Format.pp_print_string fmt "obj-array"

(* -------------------------------------------------------------------------- *)
(* --- Array Info                                                         --- *)
(* -------------------------------------------------------------------------- *)

let char c = Integer.to_int64 (Cil.charConstToInt c)

let constant e =
  match (Cil.constFold true e).enode with
  | Const(CInt64(k,_,_)) -> Integer.to_int64 k
  | _ -> WpLog.fatal "Non-constant expression (%a)" Printer.pp_exp e

let get_int e =
  match (Cil.constFold true e).enode with
  | Const(CInt64(k,_,_)) -> Some (Integer.to_int k)
  | _ -> None

let get_int64 e =
  match (Cil.constFold true e).enode with
  | Const(CInt64(k,_,_)) -> Some (Integer.to_int64 k)
  | _ -> None

let dimension t =
  let rec flat k d = function
    | TNamed (r,_) -> flat k d r.ttype
    | TArray(ty,Some e,_,_) ->
        flat (succ k) (Int64.mul d (constant e)) ty
    | te -> k , d , te
  in flat 1 Int64.one t

(* -------------------------------------------------------------------------- *)
(* --- Value State_builder.                                               --- *)
(* -------------------------------------------------------------------------- *)

let is_pointer = function
  | C_pointer _ -> true
  | C_int _ | C_float _ | C_array _ | C_comp _ -> false

let rec object_of typ =
  match typ with
  | TInt(i,_) -> C_int (c_int i)
  | TFloat(f,_) -> C_float (c_float f)
  | TPtr(typ,_) -> C_pointer (if Cil.isVoidType typ then Cil.charType else typ)
  | TFun _ -> C_pointer Cil.voidType
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
                  arr_size = Int64.to_int (constant e) ;
                  arr_dim = dim ;
                  arr_cell = ty_cell ;
                  arr_cell_nbr = Int64.to_int (ncells) ;
                }
            }
      end
  | TBuiltin_va_list _ ->
      WpLog.warning ~current:true ~once:true "variadyc type (considered as void*)" ;
      C_pointer (TVoid [])
  | TVoid _ ->
      WpLog.warning ~current:true "void object" ;
      C_int (c_int IInt)
  | TNamed (r,_)  -> object_of r.ttype

(* ------------------------------------------------------------------------ *)
(* --- Comparable                                                       --- *)
(* ------------------------------------------------------------------------ *)

let hsh = ref (fun _ -> assert false) (* Recursive call to hash *)
let cmp = ref (fun _ _ -> assert false) (* Recursive call to compare *)

module AinfoComparable = struct
  type t = arrayinfo
  let hash a = !hsh (object_of a.arr_element)
  let equal a b =
    let obj_a = object_of a.arr_element in
    let obj_b = object_of b.arr_element in
    (!cmp obj_a obj_b = 0) &&
    (match a.arr_flat , b.arr_flat with
     | Some a , Some b -> a.arr_size = b.arr_size
     | None , None -> true
     | _ -> false)
  let compare a b =
    let obj_a = object_of a.arr_element in
    let obj_b = object_of b.arr_element in
    let c = !cmp obj_a obj_b in
    if c <> 0 then c
    else match a.arr_flat , b.arr_flat with
      | Some a , Some b -> Transitioning.Stdlib.compare a.arr_size b.arr_size
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

let compare a b =
  if a==b then 0 else
    match a,b with
    | C_int i, C_int i' -> compare_c_int i i'
    | C_int _ , _ -> (-1)
    | _ , C_int _ -> 1
    | C_float f , C_float f' -> compare_c_float f f'
    | C_float _ , _ -> (-1)
    | _ , C_float _ -> 1
    | C_pointer te , C_pointer te' -> Typ.compare te te'
    | C_pointer _ , _ -> (-1)
    | _ , C_pointer _ -> 1
    | C_comp c , C_comp c' -> Compinfo.compare c c'
    | C_comp _ , _ -> (-1)
    | _ , C_comp _ -> 1
    | C_array a , C_array a' -> AinfoComparable.compare a a'

let () =
  begin
    hsh := hash ;
    cmp := compare ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Accessor Utilities                                                 --- *)
(* -------------------------------------------------------------------------- *)

let object_of_pointed = function
    C_int _ | C_float _ | C_comp _ as o ->
      Wp_parameters.fatal
        "object_of_pointed called on non-pointer %a@." pp_object o
  | C_array info -> object_of info.arr_element
  | C_pointer typ -> object_of typ


let object_of_array_elem = function
  | C_array arr -> object_of arr.arr_element
  | o -> Wp_parameters.fatal ~current:true
           "object_of_array_elem called on non-array %a." pp_object o

let rec object_of_logic_type t =
  match Logic_utils.unroll_type ~unroll_typedef:false t with
  | Ctype ty -> object_of ty
  | Ltype({lt_name="set"},[t]) -> object_of_logic_type t
  | t -> Wp_parameters.fatal ~current:true
           "@[<hov 2>c-object of logic type@ (%a)@]"
           Printer.pp_logic_type t

let rec object_of_logic_pointed t =
  match Logic_utils.unroll_type ~unroll_typedef:false t with
  | Ctype ty -> object_of_pointed (object_of ty)
  | Ltype({lt_name="set"},[t]) -> object_of_logic_pointed t
  | t -> Wp_parameters.fatal ~current:true
           "@[<hov 2>pointed of logic type@ (%a)@]"
           Printer.pp_logic_type t

let rec array_dimensions a =
  let te = object_of a.arr_element in
  let d = match a.arr_flat with None -> None | Some f -> Some f.arr_size in
  match te with
  | C_array a -> let te,ds = array_dimensions a in te , d::ds
  | _ -> te , [d]

let dimension_of_object = function
  | C_int _ | C_float _ | C_pointer _ | C_comp _ | C_array { arr_flat=None } -> None
  | C_array { arr_flat=Some a } -> Some (a.arr_dim , a.arr_cell_nbr)

let no_infinite_array = function
  | C_array {arr_flat = None} -> false
  | _ -> true

let is_comp obj c = match obj with
  | C_comp c0 -> Compinfo.equal c c0
  | _ -> false

let is_array obj ~elt = match obj with
  | C_array { arr_element = e } -> equal (object_of e) elt
  | _ -> false

let array_size = function
  | { arr_flat = Some { arr_size=s } } -> Some s
  | { arr_flat = None } ->
      if Wp_parameters.ExternArrays.get () then Some max_int else None

let get_array_size = function
  | C_array a -> array_size a
  | _ -> None

let get_array_dim = function
  | C_array { arr_flat=Some a } -> a.arr_dim
  | C_array _ -> 1
  | _ -> 0

let get_array = function
  | C_array a -> Some( object_of a.arr_element, array_size a )
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Sizeof                                                             --- *)
(* -------------------------------------------------------------------------- *)

let sizeof_defined = function
  | C_array { arr_flat = None } -> false
  | _ -> true

let typ_comp cinfo = TComp(cinfo,Cil.empty_size_cache(),[])

let bits_sizeof_comp cinfo = Cil.bitsSizeOf (typ_comp cinfo)

let bits_sizeof_array ainfo =
  match ainfo.arr_flat with
  | Some a ->
      let csize = Cil.integer ~loc:Cil.builtinLoc a.arr_cell_nbr in
      let ctype = TArray(a.arr_cell,Some csize,Cil.empty_size_cache(),[]) in
      Cil.bitsSizeOf ctype
  | None ->
      if WpLog.ExternArrays.get () then
        max_int
      else
        WpLog.fatal ~current:true "Sizeof unknown-size array"


let sizeof_object = function
  | C_int i -> i_bytes i
  | C_float f -> f_bytes f
  | C_pointer _ty -> p_bytes ()
  | C_comp cinfo -> bits_sizeof_comp cinfo / 8
  | C_array ainfo -> bits_sizeof_array ainfo / 8

let bits_sizeof_object = function
  | C_int i -> i_bits i
  | C_float f -> f_bits f
  | C_pointer _ty -> p_bits ()
  | C_comp cinfo -> bits_sizeof_comp cinfo
  | C_array ainfo -> bits_sizeof_array ainfo

let field_offset fd =
  if fd.fcomp.cstruct then (* C struct *)
    let ctype = TComp(fd.fcomp,Cil.empty_size_cache(),[]) in
    let offset = Field(fd,NoOffset) in
    fst (Cil.bitsOffset ctype offset) / 8
  else (* CIL invariant: all C union fields start at offset 0 *)
    0

(* Conforms to C-ISO 6.3.1.8        *)
(* If same sign => greater rank.    *)
(* If different:                    *)
(* Case 1:                          *)
(*   rank(unsigned) >= rank(signed) *)
(*   then convert to unsigned       *)
(* Case 2:                          *)
(*   domain(unsigned) contains      *)
(*   domain(signed)                 *)
(*   then convert to signed         *)
(* Otherwise:                       *)
(*   both are converted to unsigned *)
(*                                  *)
(* Case 2 is actually the negative  *)
(* of Case 1, and both simplifies   *)
(* into converting to the operand   *)
(* with greater rank, whatever      *)
(* their sign.                      *)

let i_convert t1 t2 = if i_bits t1 < i_bits t2 then t2 else t1
let f_convert t1 t2 = if f_bits t1 < f_bits t2 then t2 else t1

let promote a1 a2 =
  match a1 , a2 with
  | C_int i1 , C_int i2 -> C_int (i_convert i1 i2)
  | C_float f1 , C_float f2 -> C_float (f_convert f1 f2)
  | C_int _ , C_float _ -> a2
  | C_float _ , C_int _ -> a1
  | _ -> WpLog.not_yet_implemented
           "promotion between arithmetics and pointer types"

let rec basename = function
  | C_int i -> Format.asprintf "%a" pp_int i
  | C_float f -> Format.asprintf "%a" pp_float f
  | C_pointer _ -> "pointer"
  | C_comp c -> c.cname
  | C_array a ->
      let te = basename (object_of a.arr_element) in
      match a.arr_flat with
      | None -> te ^ "_array"
      | Some f -> te ^ "_" ^ string_of_int f.arr_size

let is_atomic = function
  | TVoid _ | TInt _ | TFloat _ | TNamed _ -> true
  | _ -> false

let rec pretty fmt = function
  | C_int i -> pp_int fmt i
  | C_float f -> pp_float fmt f
  | C_comp c -> Format.pp_print_string fmt c.cname
  | C_pointer ty ->
      if is_atomic ty then
        Format.fprintf fmt "%a*" Printer.pp_typ ty
      else
        Format.fprintf fmt "(%a)*" Printer.pp_typ ty
  | C_array a ->
      let te = object_of a.arr_element in
      match a.arr_flat with
      | None -> Format.fprintf fmt "%a[]" pretty te
      | Some f -> Format.fprintf fmt "%a[%d]" pretty te f.arr_size

module C_object = Datatype.Make(struct
    type t = c_object
    let name = "Ctypes.C_object"

    let rehash = Datatype.Undefined.rehash
    let structural_descr = Datatype.Undefined.structural_descr

    let reprs = [C_int UInt8]

    let equal = equal
    let pretty = pretty
    let hash = hash
    let compare = compare

    let copy = Datatype.Undefined.copy

    let internal_pretty_code = Datatype.Undefined.internal_pretty_code
    let mem_project = Datatype.Undefined.mem_project

    let varname _ = "co"
  end)

let rec compare_ptr_conflated a b =
  if a==b then 0 else
    match a,b with
    | C_int i, C_int i' -> compare_c_int i i'
    | C_int _ , _ -> (-1)
    | _ , C_int _ -> 1
    | C_float f , C_float f' -> compare_c_float f f'
    | C_float _ , _ -> (-1)
    | _ , C_float _ -> 1
    | C_pointer _ , C_pointer _ -> 0
    | C_pointer _ , _ -> (-1)
    | _ , C_pointer _ -> 1
    | C_comp c , C_comp c' -> Compinfo.compare c c'
    | C_comp _ , _ -> (-1)
    | _ , C_comp _ -> 1
    | C_array a , C_array a' -> compare_array_ptr_conflated a a'

and compare_array_ptr_conflated a b =
  let obj_a = object_of a.arr_element in
  let obj_b = object_of b.arr_element in
  let c = compare_ptr_conflated obj_a obj_b in
  if c <> 0 then c
  else match a.arr_flat , b.arr_flat with
    | Some a , Some b -> Transitioning.Stdlib.compare a.arr_size b.arr_size
    | None , Some _ -> (-1)
    | Some _ , None -> 1
    | None , None -> 0

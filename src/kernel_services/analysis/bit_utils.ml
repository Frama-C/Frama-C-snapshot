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

(* $id$ *)

(** Some utilities *)

open Cil_types
open Cil

(** [sizeof(char)] in bits *)
let sizeofchar () = Integer.of_int (bitsSizeOf charType)

(** [sizeof(char* )] in bits *)
let sizeofpointer () =  bitsSizeOf theMachine.upointType

let max_bit_size () =
  Integer.mul
  (sizeofchar())
  (Integer.two_power_of_int (sizeofpointer()))

let max_bit_address () = Integer.pred (max_bit_size())

let warn_if_zero ty r =
  if r = 0 then
    Kernel.abort
      "size of '%a' is zero. Check target code or Frama-C -machdep option."
      Printer.pp_typ ty;
  r

(** [sizeof ty] is the size of [ty] in bits. This function may return
    [Int_Base.top]. *)
let sizeof ty =
  (match ty with
  | TVoid _ -> Kernel.warning ~current:true ~once:true "using size of 'void'"
  | _ -> ()) ;
  try Int_Base.inject (Integer.of_int (bitsSizeOf ty))
  with SizeOfError _ ->
    Int_Base.top

(** [osizeof ty] is the size of [ty] in bytes. This function may return
    [Int_Base.top]. *)
let osizeof ty =
  (match ty with
  | TVoid _ -> Kernel.warning ~once:true ~current:true "using size of 'void'"
  | _ -> ()) ;
  try
    Int_Base.inject (Integer.of_int (warn_if_zero ty (bitsSizeOf ty) / 8))
  with SizeOfError _ -> Int_Base.top

exception Neither_Int_Nor_Enum_Nor_Pointer

(** May raise [Neither_Int_Nor_Enum_Nor_Pointer] if the sign of the type is not
    meaningful. [true] means that the type is signed. *)
let is_signed_int_enum_pointer ty =
  match unrollType ty with
  | TInt (k,_) | TEnum ({ekind=k},_) -> Cil.isSigned k
  | TPtr _ -> false
  | TFloat _ | TFun _ | TBuiltin_va_list _
  | TVoid _ | TArray _ | TComp _
  | TNamed _  -> raise Neither_Int_Nor_Enum_Nor_Pointer

(** Returns the sign of type of the [lval]. [true] means that the type is
    signed. *)
let signof_typeof_lval lv =
  let typ = Cil.typeOfLval lv in
  is_signed_int_enum_pointer typ

(** Returns the size of a the type of the variable in bits. *)
let sizeof_vid v = sizeof v.vtype

(** Returns the size of a the type of the variable in bits. *)
let sizeof_lval lv =
  let typ = Cil.typeOfLval lv in
  let typ =  unrollType typ in
  if isIntegralType typ then (* We might be a bitfield *)
    let rec get_size off =
      match off with
      | NoOffset | Index (_,NoOffset) -> sizeof typ
      | Field (f,NoOffset) ->
          (match f.fbitfield with
           | None -> sizeof typ
           | Some i -> Int_Base.inject (Integer.of_int i))
      | Field (_,f) | Index(_,f) -> get_size f
    in get_size (snd lv)

  else sizeof typ

(** Returns the size of the type pointed by a pointer type in bits.
    Never call it on a non pointer type. *)
let sizeof_pointed typ =
  match unrollType typ with
    | TPtr (typ,_) -> sizeof typ
    | TArray(typ,_,_,_) -> sizeof typ
    | _ ->
        Kernel.abort "TYPE IS: %a (unrolled as %a)"
          Printer.pp_typ typ
          Printer.pp_typ (unrollType typ)

(** Returns the size of the type pointed by a pointer type in bytes.
    Never call it on a non pointer type. *)
let osizeof_pointed typ =
  match unrollType typ with
    | TPtr (typ,_) -> osizeof typ
    | TArray(typ,_,_,_) -> osizeof typ
    | _ ->
        assert false (*
        Format.printf "TYPE IS: %a\n" Printer.pp_typ typ;
        Int_Base.top*)

(** Returns the size of the type pointed by a pointer type of the [lval] in
    bits. Never call it on a non pointer type [lval]. *)
let sizeof_pointed_lval lv = sizeof_pointed (Cil.typeOfLval lv)



(* -------------------------------------------------------------------------- *)
(* --- Pretty Printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

type types =
  | NoneYet
  | SomeType of typ
  | Mixed

let update_types types t = match types with
  | NoneYet -> SomeType t
  | Mixed -> Mixed
  | SomeType t' -> if Cil_datatype.Typ.equal t t' then types else Mixed

type ppenv = {
  fmt : Format.formatter ;
  use_align : bool ;
  rh_size : Integer.t ;
  mutable misaligned : bool ;
  mutable types: types ;
}
type bfinfo = Other | Bitfield of int64
type fieldpart =
  | NamedField of string * bfinfo * typ * Integer.t * Integer.t * Integer.t
      (* name, parameters to pretty_bits_internal for the field *)
  | RawField of char * Integer.t * Integer.t
      (* parameters for raw_bits of the raw field *)

type arraypart =
  | ArrayPart of Integer.t * Integer.t * typ * Integer.t * Integer.t * Integer.t
      (* start index, stop index, typ of element , align , start, stop *)

let rec pretty_bits_internal env bfinfo typ ~align ~start ~stop =
  assert ( Integer.le Integer.zero align
           && Integer.lt align env.rh_size);
  assert (if (Integer.lt start Integer.zero
              || Integer.lt stop Integer.zero) then
            (Format.printf "start: %a stop: %a@\n"
               Abstract_interp.Int.pretty start
               Abstract_interp.Int.pretty stop;
             false) else true);
  let update_types typ = env.types <- update_types env.types typ in

  let req_size = Integer.length start stop in
  (*    Format.printf "align:%Ld size: %Ld start:%Ld stop:%Ld req_size:%Ld@\n"
        align size start stop req_size;*)
  let raw_bits c start stop =
    let cond =
      env.use_align
      && ((not (Integer.equal (Integer.pos_rem start env.rh_size) align))
          || (not (Integer.equal req_size env.rh_size)))
    in
    Format.fprintf env.fmt "[%s%t]%s"
      (if Kernel.debug_atleast 1 then String.make 1 c else "")
      (fun fmt ->
         if Integer.equal stop (max_bit_address ()) then
           Format.fprintf fmt "bits %a to .." Abstract_interp.Int.pretty start
         else
           Format.fprintf fmt "bits %a to %a"
             Abstract_interp.Int.pretty start
             Abstract_interp.Int.pretty stop
      )
      (if cond then (env.misaligned <- true ; "#") else "")
  in
  assert (if (Integer.le req_size Integer.zero
              || Integer.lt start Integer.zero
              || Integer.lt stop Integer.zero) then
            (Format.printf "req_s: %a start: %a stop: %a@\n"
               Abstract_interp.Int.pretty req_size
               Abstract_interp.Int.pretty start
               Abstract_interp.Int.pretty stop;
             false) else true);
  match (unrollType typ) with
    | TInt (_ , _) | TPtr (_, _) | TEnum (_, _)  | TFloat (_, _)
    | TVoid _ | TBuiltin_va_list _ | TNamed _ | TFun (_, _, _, _) as typ ->
        let size =
          match bfinfo with
            | Other -> Integer.of_int (bitsSizeOf typ)
            | Bitfield i -> Integer.of_int64 i
        in
        (if Integer.is_zero start
           && Integer.equal size req_size then
             (** pretty print a full offset *)
             (if not env.use_align ||
                (Integer.equal start align && Integer.equal env.rh_size size)
              then update_types typ
              else (env.types <- Mixed;
                    env.misaligned <- true ;
                    Format.pp_print_char env.fmt '#'))
         else (
           env.types <- Mixed;
           raw_bits 'b' start stop)
        )

    | TComp (compinfo, _, _) as typ ->
        let size = Integer.of_int (try bitsSizeOf typ
                               with SizeOfError _ -> 0)
        in
        if (not env.use_align) && Integer.compare req_size size = 0
        then
          update_types typ (* do not print sub-fields if the size is exactly
                            the right one and the alignement is not important *)
        else begin
        try
          let full_fields_to_print = List.fold_left
            (fun acc field ->
               let current_offset = Field (field,NoOffset) in
               let start_o,width_o = bitsOffset typ current_offset in
               let start_o,width_o =
                 Integer.of_int start_o, Integer.of_int width_o
               in

               let new_start =
                 if compinfo.cstruct then
                   Integer.max Integer.zero (Integer.sub start start_o)
                 else start
               in
               let new_stop =
                 if compinfo.cstruct then
                   Integer.min
                     (Integer.sub stop start_o)
                     (Integer.pred width_o)
                 else stop
               in
               if Integer.le new_start new_stop then
                 let new_bfinfo = match field.fbitfield with
                   | None -> Other
                   | Some i -> Bitfield (Integer.to_int64 (Integer.of_int i))
                 in
                 let new_align =
                   Integer.pos_rem (Integer.sub align start_o) env.rh_size
                 in
                 let name = Pretty_utils.sfprintf "%a" Printer.pp_field field in
                 NamedField( name ,
                             new_bfinfo , field.ftype ,
                             new_align , new_start , new_stop ) :: acc
               else
                 acc)
            []
            compinfo.cfields
          in
          (** find non covered intervals in structs *)
          let non_covered,succ_last =
            if compinfo.cstruct then
              List.fold_left
                (fun ((s,last_field_offset) as acc) field ->
                   let current_offset = Field (field,NoOffset) in
                   let start_o,width_o = bitsOffset typ current_offset in
                   let start_o,width_o =
                     Integer.of_int start_o, Integer.of_int width_o
                   in
                   let succ_stop_o = Integer.add start_o width_o in
                   if Integer.gt start_o stop then acc
                   else if Integer.le succ_stop_o start then acc
                   else if Integer.gt start_o last_field_offset then
                     (* found a hole *)
                     (RawField('c', last_field_offset,Integer.pred start_o)::s,
                      succ_stop_o)
                   else
                     (s,succ_stop_o)
                )
                (full_fields_to_print,start)
                compinfo.cfields
            else full_fields_to_print, Integer.zero
          in
          let overflowing =
            if compinfo.cstruct && Integer.le succ_last stop
            then RawField('o',Integer.max start succ_last,stop)::non_covered
            else non_covered
          in
          let pretty_one_field = function
            | NamedField(name,bf,ftyp,align,start,stop) ->
                Format.fprintf env.fmt ".%s" name ;
                pretty_bits_internal env bf ftyp ~align ~start ~stop
            | RawField(c,start,stop) ->
                env.types <- Mixed;
                Format.pp_print_char env.fmt '.' ;
                raw_bits c start stop
          in
          let rec pretty_all_fields = function
            | [] -> ()
            | [f] -> pretty_one_field f
            | f::fs ->
                pretty_all_fields fs ;
                Format.pp_print_string env.fmt "; ";
                pretty_one_field f ;
          in
          match overflowing with
            | [] -> Format.pp_print_string env.fmt "{}"
            | [f] -> pretty_one_field f
            | fs ->
                Format.pp_print_char env.fmt '{' ;
                pretty_all_fields fs ;
                Format.pp_print_char env.fmt '}'
        with Cil.SizeOfError _ ->
          raw_bits '?' start stop
        end

      | TArray (typ, _, _, _) ->
          let size =
            try Integer.of_int (bitsSizeOf typ)
            with Cil.SizeOfError _ -> Integer.zero
          in
          if Integer.is_zero size then
            raw_bits 'z' start stop
          else
          let start_case = Integer.pos_div start size in
          let stop_case =  Integer.pos_div stop size in
          let rem_start_size = Integer.pos_rem start size in
          let rem_stop_size = Integer.pos_rem stop size in
          if Integer.equal start_case stop_case then (** part of one element *)
            let new_align =
              Integer.pos_rem
                (Integer.sub align (Integer.mul start_case size))
                env.rh_size
            in
            Format.fprintf env.fmt "[%a]" Abstract_interp.Int.pretty start_case;
            pretty_bits_internal env Other typ
              ~align:new_align
              ~start:rem_start_size
              ~stop:rem_stop_size
          else if Integer.equal (Integer.rem start env.rh_size) align
              && (Integer.is_zero (Integer.rem size env.rh_size))
          then
                let pred_size = Integer.pred size in
                let start_full_case =
                  if Integer.is_zero rem_start_size then start_case
                  else Integer.succ start_case
                in
                let stop_full_case =
                  if Integer.equal rem_stop_size pred_size then stop_case
                  else Integer.pred stop_case
                in
                let first_part = if Integer.is_zero rem_start_size
                then []
                else [ArrayPart(start_case,start_case,
                                typ,align,rem_start_size,pred_size)]
                in
                let middle_part =
                  if Integer.lt stop_full_case start_full_case
                  then []
                  else [ArrayPart(start_full_case,stop_full_case,
                                  typ,align,Integer.zero,pred_size)]
                in
                let last_part =
                  if Integer.equal rem_stop_size pred_size
                  then []
                  else [ArrayPart(stop_case,stop_case,
                                  typ,align,Integer.zero,rem_stop_size)]
                in
                let do_part = function
                  | ArrayPart(start_index,stop_index,typ,align,start,stop) ->
                      if Integer.equal start_index stop_index then
                        Format.fprintf env.fmt "[%a]"
                          Abstract_interp.Int.pretty start_index
                      else
                        Format.fprintf env.fmt "[%a..%a]"
                          Abstract_interp.Int.pretty start_index
                          Abstract_interp.Int.pretty stop_index ;
                      pretty_bits_internal env Other typ ~align ~start ~stop
                in
                let rec do_all_parts = function
                  | [] -> ()
                  | [p] -> do_part p
                  | p::ps ->
                      do_part p ;
                      Format.pp_print_string env.fmt "; " ;
                      do_all_parts ps
                in
                match first_part @ middle_part @ last_part with
                  | [] -> Format.pp_print_string env.fmt "{}"
                  | [p] -> do_part p
                  | ps ->
                      Format.pp_print_char env.fmt '{' ;
                      do_all_parts ps ;
                      Format.pp_print_char env.fmt '}' ;
            else (env.types <- Mixed;
                  raw_bits 'a' start stop)


let pretty_bits typ ~use_align ~align ~rh_size ~start ~stop fmt =
  (* It is simpler to perform all computation using an absolute offset:
     Cil easily gives offset information in terms of offset since the start,
     but not easily the offset between two fields (with padding) *)
  let align =
    Integer.pos_rem (Abstract_interp.Rel.add_abs start align) rh_size
  in
  assert (Integer.le Integer.zero align
          && Integer.lt align rh_size);
  if Integer.lt start Integer.zero then
    (Format.fprintf fmt "[%sbits %a to %a]#(negative offsets)"
       (if Kernel.debug_atleast 1 then "?" else "")
       Abstract_interp.Int.pretty start Abstract_interp.Int.pretty stop;
     true, None)
  else
    let env = {
      fmt = fmt ;
      rh_size = rh_size ;
      use_align = use_align ;
      misaligned = false ;
      types = NoneYet ;
    } in
    pretty_bits_internal env Other typ ~align ~start ~stop ;
    env.misaligned,
    (match env.types with
      | Mixed | NoneYet -> None
      | SomeType t -> Some t)

(* -------------------------------------------------------------------------- *)
(* --- Mapping numeric offset -> symbolic one                             --- *)
(* -------------------------------------------------------------------------- *)

exception NoMatchingOffset

type offset_match =
| MatchType of typ
| MatchSize of Integer.t
| MatchFirst

(* Comparaison of the shape of two types.  Attributes are completely ignored. *)
let rec equal_type_no_attribute t1 t2 =
  match Cil.unrollType t1, Cil.unrollType t2 with
  | TVoid _, TVoid _ -> true
  | TInt (i1, _), TInt (i2, _) -> i1 = i2 
  | TFloat (f1, _), TFloat (f2, _) -> f1 = f2
  | TPtr (t1, _), TPtr (t2, _) -> equal_type_no_attribute t1 t2
  | TArray (t1', s1, _, _), TArray (t2', s2, _, _) ->
    equal_type_no_attribute t1' t2' &&
      (s1 == s2 || try Integer.equal (Cil.lenOfArray64 s1) (Cil.lenOfArray64 s2)
                   with Cil.LenOfArray -> false)
  | TFun (r1, a1, v1, _), TFun (r2, a2, v2, _) ->
    v1 = v2 && equal_type_no_attribute r1 r2 &&
    (match a1, a2 with
    | None, _ | _, None -> true
    | Some l1, Some l2 ->
      try
        List.for_all2
          (fun (_, t1, _) (_, t2, _) -> equal_type_no_attribute t1 t2) l1 l2
      with Invalid_argument _ -> false)
  | TNamed _, TNamed _ -> assert false
  | TComp (c1, _, _), TComp (c2, _, _) -> c1.ckey = c2.ckey
  | TEnum (e1, _), TEnum (e2, _) -> e1.ename = e2.ename
  | TBuiltin_va_list _, TBuiltin_va_list _ -> true
  | (TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _ | TNamed _ |
      TComp _ | TEnum _ | TBuiltin_va_list _), _ ->
    false

(* We have found a possible matching offset of type [typ] for [om], do we stop
   here? *)
let offset_matches om typ =
  match om with
  | MatchFirst -> true
  | MatchSize size -> Integer.equal size (Integer.of_int (Cil.bitsSizeOf typ))
  | MatchType typ' -> equal_type_no_attribute typ typ'

(* Can we match [om] inside a cell of an array whose elements have size
   [size_elt] *)
let offset_match_cell om size_elt =
  match om with
  | MatchFirst -> true
  | MatchSize size -> Integer.le size size_elt
  | MatchType typ' -> Integer.le (Integer.of_int (Cil.bitsSizeOf typ')) size_elt

let rec find_offset typ ~offset om =
  (* Format.printf "Searching offset %a in %a, size %a@."
     Abstract_interp.Int.pretty offset
     Printer.pp_typ typ
     Abstract_interp.Int.pretty size; *)
  let loc = Cil_datatype.Location.unknown in
  if Integer.is_zero offset && offset_matches om typ then
    NoOffset, typ
  else
    match Cil.unrollType typ with
    | TArray (typ_elt, _, _, _) ->
      let size_elt = Integer.of_int (Cil.bitsSizeOf typ_elt) in
      let start = Integer.pos_div offset size_elt in
      let exp_start = Cil.kinteger64 ~loc start in
      let rem = Integer.pos_rem offset size_elt in
      if offset_match_cell om size_elt then
        (* [size] covers at most one cell; we continue in the relevant one *)
        let off, typ = find_offset typ_elt rem om in
        Index (exp_start, off), typ
      else begin
        match om with
        | MatchFirst | MatchType _ -> raise NoMatchingOffset
        | MatchSize size ->
          if Integer.is_zero rem
            && Integer.is_zero (Integer.rem size size_elt)
          then
            (* We cover more than one cell, but we are aligned. *)
            let nb = Integer.div size size_elt in
            let exp_nb = Cil.kinteger64 ~loc nb in
            let typ =
              TArray (typ_elt, Some exp_nb, Cil.empty_size_cache (),[])
            in
            Index (exp_start, NoOffset), typ
          else (* We match different parts of multiple cells: too imprecise. *)
            raise NoMatchingOffset
      end

    | TComp (ci, _, _) ->
      let rec find_field = function
        | [] -> raise NoMatchingOffset
        | fi :: q ->
          try
            let off_fi, len_fi = Cil.bitsOffset typ (Field (fi, NoOffset)) in
            let off_fi, len_fi = Integer.of_int off_fi, Integer.of_int len_fi in
            if Integer.(ge offset (add off_fi len_fi)) then
              (* [offset] is not in the interval occupied by [fi]. Try the next
                 one (including for union: maybe the next fields are larger). *)
              find_field q
            else
              let off, typ =
                find_offset fi.ftype (Integer.sub offset off_fi) om
              in
              Field (fi, off), typ
          with NoMatchingOffset when not ci.cstruct ->
            (* Mismatch between [offset] and the structure of [fi.ftype]. In the
               union case, we try the other fields. In the struct case, the
               other fields are too far and we abort. *)
            find_field q
      in
      find_field ci.cfields

    | _ -> raise NoMatchingOffset

let find_offset typ ~offset om =
  try
    find_offset typ ~offset om
  with Cil.SizeOfError _ | Cil.Not_representable -> raise NoMatchingOffset

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)

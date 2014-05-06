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

(* $id$ *)

(** Some utilities *)

open Cil_types
open Cil
open Abstract_interp

(** [sizeof(char)] in bits *)
let sizeofchar () = Int.of_int (bitsSizeOf charType)

(** [sizeof(char* )] in bits *)
let sizeofpointer () =  bitsSizeOf theMachine.upointType

let max_bit_size () =
  Int.mul
  (sizeofchar())
  (Int.two_power_of_int (sizeofpointer()))

let max_bit_address () = Int.pred (max_bit_size())

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
  try Int_Base.inject (Int.of_int (bitsSizeOf ty))
  with SizeOfError _ ->
    Int_Base.top

(** [osizeof ty] is the size of [ty] in bytes. This function may return
    [Int_Base.top]. *)
let osizeof ty =
  (match ty with
  | TVoid _ -> Kernel.warning ~once:true ~current:true "using size of 'void'"
  | _ -> ()) ;
  try
    Int_Base.inject (Int.of_int (warn_if_zero ty (bitsSizeOf ty) / 8))
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
           | Some i -> Int_Base.inject (Int.of_int i))
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
  rh_size : Int.t ;
  mutable misaligned : bool ;
  mutable types: types ;
}
type bfinfo = Other | Bitfield of int64
type fieldpart =
  | NamedField of string * bfinfo * typ * Int.t * Int.t * Int.t
      (* name, parameters to pretty_bits_internal for the field *)
  | RawField of char * Int.t * Int.t
      (* parameters for raw_bits of the raw field *)

type arraypart =
  | ArrayPart of Int.t * Int.t * typ * Int.t * Int.t * Int.t
      (* start index, stop index, typ of element , align , start, stop *)

let rec pretty_bits_internal env bfinfo typ ~align ~start ~stop =
  assert ( Int.le Int.zero align
           && Int.lt align env.rh_size);
  assert (if (Int.lt start Int.zero
              || Int.lt stop Int.zero) then
            (Format.printf "start: %a stop: %a@\n"
               Int.pretty start
               Int.pretty stop;
             false) else true);
  let update_types typ = env.types <- update_types env.types typ in

  let req_size = Int.length start stop in
  (*    Format.printf "align:%Ld size: %Ld start:%Ld stop:%Ld req_size:%Ld@\n"
        align size start stop req_size;*)
  let raw_bits c start stop =
    let cond =
      env.use_align && ((not (Int.equal (Int.pos_rem start env.rh_size) align))
                         || (not (Int.equal req_size env.rh_size)))
    in
    Format.fprintf env.fmt "[%s%t]%s"
      (if Kernel.debug_atleast 1 then String.make 1 c else "")
      (fun fmt ->
         if Int.equal stop (max_bit_address ()) then
           if Int.equal start Int.zero then
             Format.pp_print_string fmt "..."
           else
             Format.fprintf fmt "bits %a to ..." Int.pretty start
         else
           Format.fprintf fmt "bits %a to %a" Int.pretty start Int.pretty stop
      )
      (if cond then (env.misaligned <- true ; "#") else "")
  in
  assert (if (Int.le req_size Int.zero
              || Int.lt start Int.zero
              || Int.lt stop Int.zero) then
            (Format.printf "req_s: %a start: %a stop: %a@\n"
               Int.pretty req_size
               Int.pretty start
               Int.pretty stop;
             false) else true);
  match (unrollType typ) with
    | TInt (_ , _) | TPtr (_, _) | TEnum (_, _)  | TFloat (_, _)
    | TVoid _ | TBuiltin_va_list _ | TNamed _ | TFun (_, _, _, _) as typ ->
        let size =
          match bfinfo with
            | Other -> Int.of_int (bitsSizeOf typ)
            | Bitfield i -> Int.of_int64 i
        in
        (if Int.is_zero start
           && Int.equal size req_size then
             (** pretty print a full offset *)
             (if not env.use_align ||
                (Int.equal start align && Int.equal env.rh_size size)
              then update_types typ
              else (env.types <- Mixed;
                    env.misaligned <- true ;
                    Format.pp_print_char env.fmt '#'))
         else (
           env.types <- Mixed;
           raw_bits 'b' start stop)
        )

    | TComp (compinfo, _, _) as typ ->
        let size = Int.of_int (try bitsSizeOf typ
                               with SizeOfError _ -> 0)
        in
        if (not env.use_align) && Int.compare req_size size = 0
        then
          update_types typ (* do not print sub-fields if the size is exactly
                            the right one and the alignement is not important *)
        else begin
        try
          let full_fields_to_print = List.fold_left
            (fun acc field ->
               let current_offset = Field (field,NoOffset) in
               let start_o,width_o = bitsOffset typ current_offset in
               let start_o,width_o = Int.of_int start_o, Int.of_int width_o in

               let new_start =
                 if compinfo.cstruct then
                   Int.max Int.zero (Int.sub start start_o)
                 else start
               in
               let new_stop =
                 if compinfo.cstruct then
                   Int.min
                     (Int.sub stop start_o)
                     (Int.pred width_o)
                 else stop
               in
               if Int.le new_start new_stop then
		 let new_bfinfo = match field.fbitfield with
                   | None -> Other
                   | Some i -> Bitfield (Int.to_int64 (Int.of_int i))
		 in
		 let new_align = Int.pos_rem (Int.sub align start_o) env.rh_size
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
                     Int.of_int start_o, Int.of_int width_o
                   in
                   let succ_stop_o = Int.add start_o width_o in
                   if Int.gt start_o stop then acc
                   else if Int.le succ_stop_o start then acc
                   else if Int.gt start_o last_field_offset then
                     (* found a hole *)
                     (RawField('c', last_field_offset,Int.pred start_o)::s,
                      succ_stop_o)
                   else
                     (s,succ_stop_o)
                )
                (full_fields_to_print,start)
                compinfo.cfields
            else full_fields_to_print, Int.zero
          in
          let overflowing =
            if compinfo.cstruct && Int.le succ_last stop
            then RawField('o',Int.max start succ_last,stop)::non_covered
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
            try Int.of_int (bitsSizeOf typ)
            with Cil.SizeOfError _ -> Int.zero
          in
          if Int.is_zero size then
            raw_bits 'z' start stop
          else
          let start_case = Int.pos_div start size in
          let stop_case =  Int.pos_div stop size in
          let rem_start_size = Int.pos_rem start size in
          let rem_stop_size = Int.pos_rem stop size in
          if Int.equal start_case stop_case then (** part of one element *)
            let new_align =
              Int.pos_rem
                (Int.sub align (Int.mul start_case size))
                env.rh_size
            in
            Format.fprintf env.fmt "[%a]" Int.pretty start_case ;
            pretty_bits_internal env Other typ
              ~align:new_align
              ~start:rem_start_size
              ~stop:rem_stop_size
          else if Int.equal (Int.rem start env.rh_size) align
              && (Int.is_zero (Int.rem size env.rh_size))
          then
                let pred_size = Int.pred size in
                let start_full_case =
                  if Int.is_zero rem_start_size then start_case
                  else Int.succ start_case
                in
                let stop_full_case =
                  if Int.equal rem_stop_size pred_size then stop_case
                  else Int.pred stop_case
                in
                let first_part = if Int.is_zero rem_start_size
                then []
                else [ArrayPart(start_case,start_case,
                                typ,align,rem_start_size,pred_size)]
                in
                let middle_part =
                  if Int.lt stop_full_case start_full_case
                  then []
                  else [ArrayPart(start_full_case,stop_full_case,
                                  typ,align,Int.zero,pred_size)]
                in
                let last_part =
                  if Int.equal rem_stop_size pred_size
                  then []
                  else [ArrayPart(stop_case,stop_case,
                                  typ,align,Int.zero,rem_stop_size)]
                in
                let do_part = function
                  | ArrayPart(start_index,stop_index,typ,align,start,stop) ->
                      if Int.equal start_index stop_index then
                        Format.fprintf env.fmt "[%a]"
                          Int.pretty start_index
                      else
                        Format.fprintf env.fmt "[%a..%a]"
                          Int.pretty start_index
                          Int.pretty stop_index ;
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


let rec pretty_offset_internal env typ ~start =

  let update_types typ = env.types <- update_types env.types typ in

  let raw_offset start =
    let cond = false in
    Format.fprintf env.fmt "[%t]%s"
      (fun fmt -> Format.fprintf fmt "bit %a" Int.pretty start)
      (if cond then (env.misaligned <- true ; "#") else "")
  in
  match (unrollType typ) with
    | TInt (_ , _) | TPtr (_, _) | TEnum (_, _)  | TFloat (_, _)
    | TVoid _ | TBuiltin_va_list _ | TNamed _ | TFun (_, _, _, _) as typ ->
        if Int.is_zero start then
             (** pretty print a full offset *)
             ( update_types typ)
        else begin 
	  env.types <- Mixed;
          raw_offset start
	end        

    | TComp (compinfo, _, _) as typ ->
        if (not env.use_align)
        then
          update_types typ (* do not print sub-fields if the size is exactly
                            the right one and the alignement is not important *)
        else begin
        try
          let full_fields_to_print = List.fold_left
            (fun acc field ->
               let current_offset = Field (field,NoOffset) in
               let start_o,width_o = bitsOffset typ current_offset in
               let start_o,width_o = Int.of_int start_o, Int.of_int width_o in
	       let diff = Int.sub start start_o in
               if Int.le start_o start && Int.le diff width_o then
		 let new_bfinfo = match field.fbitfield with
                   | None -> Other
                   | Some i -> Bitfield (Int.to_int64 (Int.of_int i))
		 in
		 let new_align = Int.zero
		 in
                 let name = Pretty_utils.sfprintf "%a" Printer.pp_field field in
                 NamedField( name ,
                             new_bfinfo , field.ftype ,
                             new_align , diff , diff ) :: acc
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
                     Int.of_int start_o, Int.of_int width_o
                   in
                   let succ_stop_o = Int.add start_o width_o in
                   if Int.gt start_o start then acc
                   else if Int.le succ_stop_o start then acc
                   else if Int.gt start_o last_field_offset then
                     (* found a hole *)
                     (RawField('c', last_field_offset,Int.pred start_o)::s,
                      succ_stop_o)
                   else
                     (s,succ_stop_o)
                )
                (full_fields_to_print,start)
                compinfo.cfields
            else full_fields_to_print, Int.zero
          in
          let overflowing =
            if compinfo.cstruct && Int.le succ_last start
            then RawField('o',Int.max start succ_last,start)::non_covered
            else non_covered
          in
          let pretty_one_field = function
            | NamedField(name,_bf,ftyp,_align,start,_stop) ->
                Format.fprintf env.fmt ".%s" name ;
                pretty_offset_internal env ftyp ~start
            | RawField(_c,start,_stop) ->
                env.types <- Mixed;
                Format.pp_print_char env.fmt '.' ;
                raw_offset start
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
          raw_offset start
        end

      | TArray (typ, _, _, _) ->
          let size =
            try Int.of_int (bitsSizeOf typ)
            with Cil.SizeOfError _ -> Int.zero
          in
          if Int.is_zero size then
            raw_offset start
          else
          let start_case = Int.pos_div start size in
          let rem_start_size = Int.pos_rem start size in
            Format.fprintf env.fmt "[%a]" Int.pretty start_case ;
            pretty_offset_internal env typ
              ~start:rem_start_size

let pretty_offset typ start fmt = 
    let env =
      {
	fmt = fmt ;
	rh_size = Int.zero ;
	use_align = true ;
	misaligned = false ;
	types = NoneYet ;}
    in
    pretty_offset_internal env typ start


let pretty_bits typ ~use_align ~align ~rh_size ~start ~stop fmt =
  (* It is simpler to perform all computation using an absolute offset:
     Cil easily gives offset information in terms of offset since the start,
     but not easily the offset between two fields (with padding) *)
  let align = Int.pos_rem (Rel.add_abs start align) rh_size in
  assert (Int.le Int.zero align
          && Int.lt align rh_size);
  if Int.lt start Int.zero then
    (Format.fprintf fmt "[%sbits %a to %a]#(negative offsets)"
       (if Kernel.debug_atleast 1 then "?" else "")
       Int.pretty start Int.pretty stop ; true, None)
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


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

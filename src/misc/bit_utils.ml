(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
let sizeofpointer () =  bitsSizeOf !upointType

let warn_if_zero ty r =
  if r = 0 then
    (ignore
       (Cil.error
          "size of '%a' is zero. Check your code or your configuration."
          !Ast_printer.d_type ty);
     exit 1;);
  r

(** [sizeof ty] is the size of [ty] in bits. This function may return
    [Int_Base.top]. *)
let sizeof ty =
  (match ty with
  | TVoid _ ->
      ignore(warn "using size of 'void'")
  | _ -> ()) ;
  try Int_Base.inject (Int.of_int (bitsSizeOf ty))
  with SizeOfError _ ->
    Int_Base.top

(** [osizeof ty] is the size of [ty] in bytes. This function may return
    [Int_Base.top]. *)
let osizeof ty =
  (match ty with
  | TVoid _ ->
      ignore(warn "using size of 'void'")
  | _ -> ()) ;
  try
    Int_Base.inject (Int.of_int (warn_if_zero ty (bitsSizeOf ty) / 8))
  with SizeOfError _ -> Int_Base.top

exception Neither_Int_Nor_Enum_Nor_Pointer

(** May raise [Neither_Int_Nor_Enum_Nor_Pointer] if the sign of the type is not
    meaningful. [true] means that the type is signed. *)
let is_signed_int_enum_pointer ty =
  match unrollType ty with
  | TInt (k,_) -> Cil.isSigned k
  | TPtr _ -> false
  | TEnum _ -> !enum_are_signed
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
    | TArray(typ,_,_) -> sizeof typ
    | _ ->
        assert false
      (*  ignore (Pretty.printf "TYPE IS: %a\n" !Ast_printer.d_type typ);
        Int_Base.top *)

(** Returns the size of the type pointed by a pointer type in bytes.
    Never call it on a non pointer type. *)
let osizeof_pointed typ =
  match unrollType typ with
    | TPtr (typ,_) -> osizeof typ
    | TArray(typ,_,_) -> osizeof typ
    | _ ->
        assert false (*
        Format.printf "TYPE IS: %a\n" !Ast_printer.d_type typ;
        Int_Base.top*)

(** Returns the size of the type pointed by a pointer type of the [lval] in
    bits. Never call it on a non pointer type [lval]. *)
let sizeof_pointed_lval lv = sizeof_pointed (Cil.typeOfLval lv)

(** Returns the type pointed by the given type. Asserts it is a pointer type *)
let typeOf_pointed typ =
  match unrollType typ with
  | TPtr (typ,_) -> typ
  | _ -> assert false

(** Set of integers *)
module IntSet = Set.Make(Int)

type bfinfo = Other | Bitfield of int64

(** Pretty prints a range of bits in a type for the user.
    Tries to find field names and array indexes, whenever possible. *)
let pretty_bits typ ~use_align ~align ~rh_size ~start ~stop =
  assert (Int.le Int.zero align
          && Int.lt align rh_size);
  if Int.lt start Int.zero then
    (Format.sprintf "[%sbits %a to %a]#(negative offsets)"
       (if Cmdline.Debug.get () > 0 then "?" else "")
       Int.pretty_s start
       Int.pretty_s stop), true
  else
    let has_misaligned_fields = ref false in
    let rec pretty_bits_internal bfinfo typ ~align ~start ~stop =
      assert ( Int.le Int.zero align
               && Int.lt align rh_size);
      assert (if (Int.lt start Int.zero
                  || Int.lt stop Int.zero) then
                (Format.printf "start: %a stop: %a@\n"
                   Int.pretty start
                   Int.pretty stop;
                 false) else true);

      let req_size = Int.length start stop in
      (*    Format.printf "align:%Ld size: %Ld start:%Ld stop:%Ld req_size:%Ld@\n"
            align size start stop req_size;*)
      let raw_bits c start stop =
        let cond =
          use_align && (Int.neq (Int.pos_rem start rh_size) align
                        || Int.neq req_size rh_size) in
        Format.sprintf "[%sbits %a to %a]%s"
	  (if Cmdline.Debug.get () > 0 then String.make 1 c else "")
          Int.pretty_s start
          Int.pretty_s stop
          (if cond
           then (has_misaligned_fields := true; "#")
           else "")
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
      | TVoid _ | TBuiltin_va_list _ | TNamed _ | TFun (_, _, _, _) ->
          let size =
            match bfinfo with
            | Other -> Int.of_int (bitsSizeOf typ)
            | Bitfield i -> Int.of_int64 i
          in
          (if Int.eq start Int.zero
             && Int.eq size req_size then
               (** pretty print a full offset *)
               (if (Int.eq start align
                    && Int.eq rh_size size) || (not use_align) then
                  ""
                else (has_misaligned_fields := true; "#"))
           else
             (** Not so readable after all...
                Format.sprintf " &0x%Lx"
                (Scanf.sscanf (Format.sprintf "0b%s"
                (Int.fold
                (fun i acc ->
                acc^(if i<start || i>stop then "0"
                else "1"))
                ~inf:0L
                ~sup:(Int.pred size)
                ~step:1L
                ""))
                "%Li" (fun x ->  x)) *)
             raw_bits 'b' start stop)

      | TComp (compinfo, _) -> begin
          let size = Int.of_int (try bitsSizeOf typ
                                 with SizeOfError _ -> 0)
          in
          if (not use_align) && Int.compare req_size size = 0
          then
            "" (* do not print sub-fields if the size is exactly the right one
                  and the alignement is not important *)
          else
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
               let new_bfinfo = match field.fbitfield with
               | None -> Other
               | Some i -> Bitfield (Int.to_int64 (Int.of_int i))
               in
               if Int.le new_start new_stop then
                 let s_for_o =
                   pretty_bits_internal new_bfinfo field.ftype
                     ~align:(Int.pos_rem (Int.sub align start_o) rh_size)
                     ~start:new_start
                     ~stop:new_stop
                 in (field.fname, s_for_o)::acc
               else acc)
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
                     ((raw_bits 'c' (last_field_offset) (Int.pred start_o)),"")
                     ::s,
                   succ_stop_o
                   else s,succ_stop_o)
                (full_fields_to_print,start)
                compinfo.cfields
            else full_fields_to_print, Int.zero
          in
          let overflowing =
            if compinfo.cstruct then
              if Int.le succ_last stop then
                (raw_bits 'o' (Int.max start succ_last) stop,"")::
                  non_covered
              else non_covered
            else non_covered
          in
          let pretty_one_field (name,s) =
            Pretty_utils.sfprintf ".%a%s" !Ast_printer.d_ident name s in
          match overflowing with
          | [] -> ""
          | [v] -> pretty_one_field v
          | _ ->
              Format.sprintf "{%s}"
                (List.fold_left
                   (fun acc v -> (pretty_one_field v)^"; "^ acc)
                   ""
                   overflowing)

        end
      | TArray (typ, _, _) ->
          let size = Int.of_int (bitsSizeOf typ) in
          if Int.eq size Int.zero then
            raw_bits 'z' start stop
          else
          let start_case = Int.pos_div start size in
          let stop_case =  Int.pos_div stop size in
          let rem_start_size = Int.pos_rem start size in
          let rem_stop_size = Int.pos_rem stop size in
          if Int.eq start_case stop_case then (** part of one element *)
            let new_align =
              Int.pos_rem
                (Int.sub align (Int.mul start_case size))
                rh_size
            in
            Format.sprintf
              "[%a]%s"
              Int.pretty_s start_case
              (pretty_bits_internal Other
                 typ
                 ~align:new_align
                 ~start:rem_start_size
                 ~stop:rem_stop_size)
          else
            if Int.eq (Int.rem start rh_size) align
              && (Int.eq (Int.rem size rh_size) Int.zero) then
                let pred_size = Int.pred size in
                let start_full_case =
                  if Int.eq rem_start_size Int.zero then start_case
                  else Int.succ start_case
                in
                let stop_full_case =
                  if Int.eq rem_stop_size pred_size then stop_case
                  else Int.pred stop_case
                in
                let first_part = if Int.eq rem_start_size Int.zero
                then None
                else
                  Some
                    (pretty_bits_internal Other typ ~align ~start:rem_start_size ~stop:pred_size)
                in
                let middle_part =
                  if Int.lt
                    (Int.sub stop_full_case start_full_case) Int.zero
                  then None
                  else
                    Some (pretty_bits_internal Other typ
                            ~align
                            ~start:Int.zero
                            ~stop:pred_size)
                in
                let last_part =
                  if Int.eq rem_stop_size pred_size then None
                  else Some (pretty_bits_internal Other typ
                               ~align
                               ~start:Int.zero
                               ~stop:rem_stop_size)
                in
                let at_least_two =
                  match first_part,middle_part,last_part with
                  | Some _,Some _,_|_,Some _,Some _ | Some _ ,_,Some _ -> true
                  | _ -> false
                in
                let semicol = if at_least_two then "; " else "" in
                let do_part p prefix =
                  (match p with
                   | None -> ""
                   | Some s -> prefix^s^semicol)
                in
                Format.sprintf "%s%s%s%s%s"
                  (if at_least_two then "{" else "")
                  (do_part first_part
                     (Format.sprintf "[%a]" Int.pretty_s start_case))
                  (do_part middle_part
                     (if Int.eq start_full_case stop_full_case then
                        Format.sprintf "[%a]" Int.pretty_s start_full_case
                      else Format.sprintf "[%a..%a]"
                        Int.pretty_s start_full_case
                        Int.pretty_s stop_full_case))
                  (do_part last_part
                     (Format.sprintf "[%a]" Int.pretty_s stop_case))
                  (if at_least_two then "}" else "")
            else raw_bits 'a' start stop

    in let r = pretty_bits_internal Other typ ~align ~start ~stop
    in r, !has_misaligned_fields

(** Returns [true] whenever the type contains only arithmetic types *)
let is_fully_arithmetic ty =
  not (existsType
         (fun typ -> match typ with
            | TNamed _
            | TComp _
            | TArray _ -> ExistsMaybe
            | TPtr _ | TBuiltin_va_list _ | TFun _ | TVoid _ -> ExistsTrue
            | TEnum _ |TFloat _ | TInt _ ->  ExistsFalse)
         ty)

let memory_size () =
  Int.pred
    (Int.shift_left
       Int.one
       (Int.of_int
          (8+sizeofpointer ())))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

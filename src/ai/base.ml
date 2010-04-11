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

open Cil_types
open Cil
open Abstract_interp
open Abstract_value

type cell_class_attributes =
    { cname : string ;
      cid : int ;
      cneverexact : bool ;
      ctyp : Cil_types.typ ;
      cvolatile : bool ;
    }

let name = "base"

type validity =
  | All
  | Unknown of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Known of Abstract_interp.Int.t*Abstract_interp.Int.t

type t =
  | Var of varinfo*validity
  | Initialized_Var of varinfo*validity
      (** base that is implicitely initialized. *)
  | Null (** base for adresses like [(int* )0x123] *)
  | String of int*string (** String constants *)
  | Cell_class of cell_class_attributes (** a class of memory cells *)

let invalid = Known(Int.one, Int.zero)

let id = function
  | Var (vi,_) | Initialized_Var (vi,_) -> vi.vid
  | Null -> 0
  | String (id,_) | Cell_class {cid = id} -> id

let hash = id

let null = Null

let is_null x = match x with Null -> true | _ -> false

let is_hidden_variable v =
  match v with
    Var (s,_) when s.vlogic -> true
  | _ -> false

let pretty_validity fmt v =
  match v with
  | All -> Format.fprintf fmt "All"
  | Unknown (b,e)  -> Format.fprintf fmt "Unknown %a-%a" Int.pretty b Int.pretty e
  | Known (b,e)  -> Format.fprintf fmt "Known %a-%a" Int.pretty b Int.pretty e

let pretty fmt t = Format.fprintf fmt "%s"
  (match t with
   | String (_,s) -> Format.sprintf "%S" s
   | Cell_class c -> Format.sprintf "%S" c.cname
   | Var (t,_) | Initialized_Var (t,_) ->
       Pretty_utils.sfprintf "@[%a@]" !Ast_printer.d_ident t.vname
   | Null -> "NULL")

(*
let pretty_caml fmt t =
  match t with
    String (_,s) -> Format.fprintf fmt "(Base.create_string %S)" s
  | Var (t,_) | Initialized_Var (t,_) ->
      Base.
*)

let compare v1 v2 = Pervasives.compare (id v1) (id v2)

let typeof v =
  match v with
  | String (_,_) -> Some charConstPtrType
  | Null -> None
  | Cell_class c ->
      Some c.ctyp
  | Var (v,_) | Initialized_Var (v,_) -> Some (unrollType v.vtype)

let bits_sizeof v =
  match v with
    | String (_,s) ->
        Int_Base.inject
          (Int.mul (Int.of_int 8) (Int.succ (Int.of_int (String.length s))))
    | Null -> Int_Base.top
    | Cell_class c ->
	Bit_utils.sizeof c.ctyp
    | Var (v,_) | Initialized_Var (v,_) ->
        Bit_utils.sizeof_vid v

(*        match findAttribute "original_type" (typeAttr typ) with
        | []  -> Bit_utils.sizeof_vid v
        | [ASizeOf (TArray (_, Some _,_) as pointed_typ)] ->
            bitsSizeOf pointed_typ
*)

(** All absolute address are invalid *)
module MinValidAbsoluteAddress =
  Computation.Ref
    (struct
       include Abstract_interp.Int.Datatype
       let default () = Abstract_interp.Int.zero
     end)
    (struct
       let name = "MinValidAbsoluteAddress"
       let dependencies = []
     end)

module MaxValidAbsoluteAddress =
  Computation.Ref
    (struct
       include Abstract_interp.Int.Datatype
       let default () = Abstract_interp.Int.minus_one
     end)
    (struct
       let name = "MaxValidAbsoluteAddress"
       let dependencies = []
     end)

let () =
  Parameters.AbsoluteValidRange.add_set_hook
    (fun _ x ->
       try Scanf.sscanf x "%Li-%Li"
	 (fun min max ->
	    let mul8 = Int64.mul 8L in
            MinValidAbsoluteAddress.set 
	      (Abstract_interp.Int.of_int64 (mul8 min));
            MaxValidAbsoluteAddress.set 
	      (Abstract_interp.Int.of_int64
		 (Int64.pred (mul8 (Int64.succ max)))))
       with End_of_file | Scanf.Scan_failure _ | Failure _ as e ->
	 Kernel.abort "Invalid -absolute-valid-range integer-integer: each integer may be in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and has to hold in 64 bits. A correct example is -absolute-valid-range 1-0xFFFFFF0.@\nError was %S@."
           (Printexc.to_string e))

let min_valid_absolute_address = MinValidAbsoluteAddress.get
let max_valid_absolute_address = MaxValidAbsoluteAddress.get

let validity v =
  match v with
  | Null -> Known (min_valid_absolute_address (), max_valid_absolute_address ())
  | Var (_,v) | Initialized_Var (_,v) -> v
  | String _ | Cell_class _ ->
      let max_valid = bits_sizeof v in
      match max_valid with
      | Int_Base.Bottom -> assert false
      | Int_Base.Top -> All
      | Int_Base.Value size ->
          (*Format.printf "Got %a for %a@\n" Int.pretty size pretty v;*)
          Known (Int.zero,Int.pred size)
            (* they all start to be valid at offset 0 *)

exception Not_valid_offset

let is_valid_offset size base offset =
  match validity base with
  | Known (min_valid,max_valid) ->
      let min = Ival.min_int offset in
      begin match min with
      | None -> raise Not_valid_offset
      | Some v -> if Int.lt v min_valid then raise Not_valid_offset
      end;
      let max = Ival.max_int offset in
      begin match max with
      | None -> raise Not_valid_offset
      | Some v ->
          if Int.gt (Int.pred (Int.add v size)) max_valid then
            raise Not_valid_offset
      end
  | Unknown _ -> raise Not_valid_offset
  | All -> ()

(*
  let is_volatile v =
  match v with
  | String _ | Null -> false
  | Cell_class c -> c.cvolatile
  | Var vv ->
  hasAttribute "volatile" (typeAttrs vv.vtype)
*)

let equal v w = (id v) = (id w)

let hash = id

let is_aligned_by b alignment =
  if Int.is_zero alignment
  then false
  else
    match b with
      Var (v,_) | Initialized_Var (v,_) ->
	Int.is_zero (Int.rem (Int.of_int (Cil.alignOf_int(v.vtype))) alignment)
    | Null -> true
    | String _ -> Int.is_one alignment
    | Cell_class _ -> assert false

let is_any_formal_or_local v =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> not v.vlogic && not v.vglob
  | Null | String _ | Cell_class _  -> false

let is_any_local v =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      not v.vlogic && not v.vglob && not v.vformal
  | Null | String _ | Cell_class _  -> false

let is_formal_or_local v fundec =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      Ast_info.Function.is_formal_or_local v fundec
  | Null | String _ | Cell_class _  -> false

let is_formal_of_prototype v vi =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      Ast_info.Function.is_formal_of_prototype v vi
  | Null | String _ | Cell_class _   -> false

let is_local v fundec =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> Ast_info.Function.is_local v fundec
  | Null | String _ | Cell_class _   -> false

let is_block_local v block =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> Ast_info.is_block_local v block
  | Null | String _ | Cell_class _   -> false

let validity_from_type v =
  if isFunctionType v.vtype then invalid
  else
  let max_valid = Bit_utils.sizeof_vid v in
  match max_valid with
  | Int_Base.Bottom -> assert false
  | Int_Base.Top ->
      (* TODO:
	 if (some configuration option)
	 then Unknown (Int.zero, Bit_utils.max_bit_address ())
	 else *)
      invalid
  | Int_Base.Value size when Int.gt size Int.zero ->
      (*Format.printf "Got %a for %s@\n" Int.pretty size v.vname;*)
      Known (Int.zero,Int.pred size)
  | Int_Base.Value size ->
      assert (Int.eq size Int.zero);
      Unknown (Int.zero, Bit_utils.max_bit_address ())

exception Not_a_variable

let get_varinfo t =
  match t with
  | Var (t,_) | Initialized_Var (t,_) -> t
  | _ -> raise Not_a_variable

let create_varinfo varinfo =
  assert (not varinfo.vlogic);
  Var (varinfo,validity_from_type varinfo)

let create_logic varinfo validity =
  assert varinfo.vlogic;
  Var (varinfo,validity)

let create_initialized varinfo validity =
  assert varinfo.vlogic;
  Initialized_Var (varinfo,validity)

type base = t

module LiteralStrings =
  Computation.Hashtbl
    (Datatype.String)
    ((* The function [copy] is used here but persistent strings are not
	required. *)
      Project.Datatype.Imperative
	(struct
	   type t = base
	   let copy = function
	     | String _ as b -> b
	     | _ -> assert false
	   let name = "LiteralStrings"
	 end))
    (struct
       let name = name
       let dependencies = [Ast.self]
       let size = 17
     end)

let create_string s =
  LiteralStrings.memo (fun _ -> String (Cil_const.new_raw_id (), s)) s

module Datatype =
  Project.Datatype.Imperative
    (struct
       type t = base
       let copy _ = assert false (* TODO if required *)
       let name = "base"
     end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

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

open Cil_types
open Cil
open Abstract_interp
open Abstract_value

let name = "base"

type validity =
  | All
  | Unknown of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Known of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Periodic of Abstract_interp.Int.t*Abstract_interp.Int.t*
      Abstract_interp.Int.t

type string_id = Cil_types.exp

type base =
  | Var of varinfo * validity
  | Initialized_Var of varinfo * validity
      (** base that is implicitly initialized. *)
  | Null (** base for addresses like [(int* )0x123] *)
  | String of int * string_id (** String constants *)

let invalid = Known(Int.one, Int.zero)

let id = function
  | Var (vi,_) | Initialized_Var (vi,_) -> vi.vid
  | Null -> 0
  | String (id,_) -> id

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
  | Periodic (b,e,p)  ->
      Format.fprintf fmt "Periodic %a-%a (%a)"
        Int.pretty b Int.pretty e
        Int.pretty p

type cstring = CSString of string | CSWstring of Escape.wstring

let get_string exp =
  match exp.enode with
    Const (CStr s) -> CSString s
  | Const (CWStr w) -> CSWstring w
  | _ -> assert false

let pretty fmt t = Format.fprintf fmt "%s"
  (match t with
   | String (_,{enode=Const (CStr s)}) -> 
       Format.sprintf "%S" s
   | String (_,{enode=Const (CWStr s)}) -> 
       Format.sprintf "L\"%s\"" (Escape.escape_wstring s)
   | String _ -> assert false
   | Var (t,_) | Initialized_Var (t,_) ->
       Pretty_utils.sfprintf "@[%a@]" !Ast_printer.d_ident t.vname
   | Null -> "NULL")

let compare v1 v2 = Datatype.Int.compare (id v1) (id v2)

let typeof v =
  match v with
  | String (_,_) -> Some charConstPtrType
  | Null -> None
  | Var (v,_) | Initialized_Var (v,_) -> Some (unrollType v.vtype)

let cstring_bitlength e = 
  let u, l = 
    match e with
      {enode=Const (CStr s)} ->
	8 (* FIXME: CHAR_BIT *), (String.length s)
    | {enode=Const (CWStr s)} ->
	bitsSizeOf theMachine.wcharType, (List.length s)
    | _ -> assert false
  in
  Int.of_int (u*(succ l))

let bits_sizeof v =
  match v with
    | String (_,e) ->
        Int_Base.inject (cstring_bitlength e)
    | Null -> Int_Base.top
    | Var (v,_) | Initialized_Var (v,_) ->
        Bit_utils.sizeof_vid v

module MinValidAbsoluteAddress =
  State_builder.Ref
    (Abstract_interp.Int)
    (struct
       let name = "MinValidAbsoluteAddress"
       let dependencies = []
       let kind = `Internal
       let default () = Abstract_interp.Int.zero
     end)

module MaxValidAbsoluteAddress =
  State_builder.Ref
    (Abstract_interp.Int)
    (struct
       let name = "MaxValidAbsoluteAddress"
       let dependencies = []
       let kind = `Internal
       let default () = Abstract_interp.Int.minus_one
     end)

let () =
  Kernel.AbsoluteValidRange.add_set_hook
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
  | String _ ->
      let max_valid = bits_sizeof v in
      match max_valid with
      | Int_Base.Bottom -> assert false
      | Int_Base.Top -> All
      | Int_Base.Value size ->
          (*Format.printf "Got %a for %a@\n" Int.pretty size pretty v;*)
          Known (Int.zero,Int.pred size)
            (* they all start to be valid at offset 0 *)

exception Not_valid_offset

let is_read_only base =
  match base with
    String _ -> true
  | _ -> false (* TODO: completely const types *)

let is_valid_offset ~for_writing size base offset =
  if for_writing && (is_read_only base)
  then raise Not_valid_offset;
  match validity base with
  | Known (min_valid,max_valid)
  | Periodic (min_valid, max_valid, _)->
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

let is_function base =
  match base with
    String _ | Null | Initialized_Var _ -> false
  | Var(v,_) ->
      isFunctionType v.vtype

(*
  let is_volatile v =
  match v with
  | String _ | Null -> false
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

let is_any_formal_or_local v =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> not v.vlogic && not v.vglob
  | Null | String _ -> false

let is_any_local v =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      not v.vlogic && not v.vglob && not v.vformal
  | Null | String _ -> false

let is_global v =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> v.vglob
  | Null | String _ -> true

let is_formal_or_local v fundec =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      Ast_info.Function.is_formal_or_local v fundec
  | Null | String _ -> false

let is_formal_of_prototype v vi =
  match v with
  | Var (v,_) | Initialized_Var (v,_) ->
      Ast_info.Function.is_formal_of_prototype v vi
  | Null | String _ -> false

let is_local v fundec =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> Ast_info.Function.is_local v fundec
  | Null | String _ -> false

let is_formal v fundec =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> Ast_info.Function.is_formal v fundec
  | Null | String _ -> false

let is_block_local v block =
  match v with
  | Var (v,_) | Initialized_Var (v,_) -> Ast_info.is_block_local v block
  | Null | String _ -> false

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
      assert (Int.equal size Int.zero);
      Unknown (Int.zero, Bit_utils.max_bit_address ())

exception Not_a_variable

module D = Datatype.Make_with_collections
  (struct
    type t = base
    let name = "Base"
    let structural_descr = Structural_descr.Abstract (* TODO better *)
    let reprs = [ Null ]
    let equal = equal
    let compare = compare
    let pretty = pretty
    let hash = hash
    let mem_project = Datatype.never_any_project
    let internal_pretty_code = Datatype.pp_fail
    let rehash = Datatype.identity
    let copy = Datatype.undefined
    let varname = Datatype.undefined
   end)

include D

module Hptset = Hptset.Make
  (struct include D let id = id end)
  (struct let v = [ [ ] ] end)
  (struct let l = [ Ast.self ] end)

module VarinfoLogic =
  Cil_state_builder.Varinfo_hashtbl
    (D)
    (struct
       let name = "Base.VarinfoLogic"
       let dependencies = [ Ast.self ]
       let size = 257
       let kind = `Internal
     end)

let get_varinfo t = match t with
  | Var (t,_) | Initialized_Var (t,_) -> t
  | _ -> raise Not_a_variable

let regexp = Str.regexp "Frama_C_periodic[^0-9]*\\([0-9]+\\)"

let create_varinfo varinfo =
  assert (not varinfo.vlogic);
  let validity = validity_from_type varinfo in
  let name = varinfo.vname in
  let validity =
    if Str.string_match regexp name 0 then
      let period = Str.matched_group 1 name in
      let period = int_of_string period in
      Kernel.warning ~current:true ~once:true
        "Periodic variable %s of period %d@."
        name
        period;
      match validity with
      | Known(mn, mx) ->
          assert (Int.is_zero mn);
          Periodic(mn, mx, Int.of_int period)
      | _ -> assert false
    else validity
  in
  Var (varinfo, validity)

let create_logic varinfo validity =
  assert (varinfo.vlogic && not (VarinfoLogic.mem varinfo));
  let base = Var (varinfo,validity) in
  VarinfoLogic.add varinfo base;
  base

let create_initialized varinfo validity =
  assert varinfo.vlogic;
  Initialized_Var (varinfo,validity)

let find varinfo =
  if varinfo.vlogic then VarinfoLogic.find varinfo
  else create_varinfo varinfo

module LiteralStrings =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (D)
    (struct
       let name = "litteral strings"
       let dependencies = [ Ast.self ]
       let size = 17
       let kind = `Internal
     end)

let create_string e =
  LiteralStrings.memo (fun _ -> String (Cil_const.new_raw_id (), e)) e.eid

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

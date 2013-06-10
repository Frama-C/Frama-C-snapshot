(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

type validity =
  | Known of Int.t * Int.t
  | Unknown of Int.t * Int.t option * Int.t (* begining, known end, unknown end *)
  | Periodic of Int.t * Int.t * Int.t
  | Invalid

let pretty_validity fmt v =
  match v with
  | Unknown (b,k,e)  -> 
      Format.fprintf fmt "Unknown %a/%a/%a"
	Int.pretty b (Pretty_utils.pp_opt Int.pretty) k Int.pretty e
  | Known (b,e)  -> Format.fprintf fmt "Known %a-%a" Int.pretty b Int.pretty e
  | Invalid -> Format.fprintf fmt "Invalid"
  | Periodic (b,e,p)  ->
      Format.fprintf fmt "Periodic %a-%a (%a)"
        Int.pretty b Int.pretty e
        Int.pretty p

module Validity = Datatype.Make
  (struct
    type t = validity
    let name = "Base.validity"
    let structural_descr = Structural_descr.Abstract
    let reprs = [ Known (Int.zero, Int.one) ]
    let equal = Datatype.undefined
    let compare = Datatype.undefined
    let hash = Datatype.undefined
    let pretty = pretty_validity
    let mem_project = Datatype.never_any_project
    let internal_pretty_code = Datatype.pp_fail
    let rehash = Datatype.identity
    let copy (x:t) = x
    let varname _ = "v"
   end)

type string_id = Cil_types.exp

type base =
  | Var of varinfo * validity
  | Initialized_Var of varinfo * validity
      (** base that is implicitly initialized. *)
  | Null (** base for addresses like [(int* )0x123] *)
  | String of int * string_id (** String constants *)

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

type cstring = CSString of string | CSWstring of Escape.wstring

let get_string exp =
  match exp.enode with
    Const (CStr s) -> CSString s
  | Const (CWStr w) -> CSWstring w
  | _ -> assert false

let pretty fmt t = 
  match t with
    | String (_,{enode=Const (CStr s)}) -> Format.fprintf fmt "%S" s
    | String (_,{enode=Const (CWStr s)}) -> 
        Format.fprintf fmt "L\"%s\"" (Escape.escape_wstring s)
    | String _ -> assert false
    | Var (t,_) | Initialized_Var (t,_) -> Printer.pp_varinfo fmt t
    | Null -> Format.pp_print_string fmt "NULL"

let pretty_addr fmt t =
  ( match t with
    Var _ | Initialized_Var _ ->
      Format.fprintf fmt "&"
  | String _ | Null -> ());
  pretty fmt t

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

let dep_absolute = [Kernel.AbsoluteValidRange.self]

module MinValidAbsoluteAddress =
  State_builder.Ref
    (Abstract_interp.Int)
    (struct
       let name = "MinValidAbsoluteAddress"
       let dependencies = dep_absolute
       let default () = Abstract_interp.Int.zero
     end)

module MaxValidAbsoluteAddress =
  State_builder.Ref
    (Abstract_interp.Int)
    (struct
       let name = "MaxValidAbsoluteAddress"
       let dependencies = dep_absolute
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
  | Null ->
      let mn = min_valid_absolute_address ()in
      let mx = max_valid_absolute_address () in
      if Integer.gt mx mn then
        Known (mn, mx)
      else
        Invalid
  | Var (_,v) | Initialized_Var (_,v) -> v
  | String _ ->
      let max_valid = bits_sizeof v in
      match max_valid with
      | Int_Base.Value size ->
          (* all start to be valid at offset 0 *)
          Known (Int.zero,Int.pred size)
      | Int_Base.Top ->
          Unknown (Int.zero, None, Bit_utils.max_bit_address ())

exception Not_valid_offset

let is_read_only base =
  match base with
    String _ -> true
  | _ -> false (* TODO: completely const types *)

let is_valid_offset ~for_writing size base offset =
  if for_writing && (is_read_only base)
  then raise Not_valid_offset;
  match validity base with
  | Invalid -> raise Not_valid_offset
  | Known (min_valid,max_valid)
  | Periodic (min_valid, max_valid, _)
  | Unknown (min_valid, Some max_valid, _) ->
      let min = Ival.min_int offset in
      begin match min with
      | None -> raise Not_valid_offset
      | Some min -> 
(*	  Format.printf "111 %a %a@." Int.pretty min_valid Int.pretty min; *)
	  if Int.lt min min_valid then raise Not_valid_offset
      end;
      let max = Ival.max_int offset in
      begin match max with
      | None -> raise Not_valid_offset
      | Some max ->
	  (*Format.printf "222 %a: mb %a, m %a, size %a@."
            pretty base Int.pretty  max_valid Int.pretty max Int.pretty size;*)
          if Int.gt (Int.pred (Int.add max size)) max_valid then
            raise Not_valid_offset
      end
  | Unknown (_, None, _) -> raise Not_valid_offset

let is_function base =
  match base with
    String _ | Null | Initialized_Var _ -> false
  | Var(v,_) ->
      isFunctionType v.vtype

let equal v w = (id v) = (id w)

let is_aligned_by b alignment =
  if Int.is_zero alignment
  then false
  else
    match b with
      Var (v,_) | Initialized_Var (v,_) ->
        Int.is_zero (Int.rem (Int.of_int (Cil.bytesAlignOf v.vtype)) alignment)
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
  if isFunctionType v.vtype then Invalid
  else
  let max_valid = Bit_utils.sizeof_vid v in
  match max_valid with
  | Int_Base.Top ->
      Unknown (Int.zero, None, Bit_utils.max_bit_address ())
  | Int_Base.Value size when Int.gt size Int.zero ->
      (*Format.printf "Got %a for %s@\n" Int.pretty size v.vname;*)
      Known (Int.zero,Int.pred size)
  | Int_Base.Value size ->
      assert (Int.equal size Int.zero);
      Unknown (Int.zero, None, Bit_utils.max_bit_address ())

exception Not_a_variable

module BaseDatatype = struct
  include Datatype.Make_with_collections
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
  let id = id
end

include BaseDatatype

module Hptset = Hptset.Make
  (BaseDatatype)
  (struct let v = [ [ ] ] end)
  (struct let l = [ Ast.self ] end)
let () = Ast.add_monotonic_state Hptset.self
let () = Ast.add_hook_on_update Hptset.clear_caches

module VarinfoLogic =
  Cil_state_builder.Varinfo_hashtbl
    (BaseDatatype)
    (struct
       let name = "Base.VarinfoLogic"
       let dependencies = [ Ast.self ]
       let size = 89
     end)
let () = Ast.add_monotonic_state VarinfoLogic.self

let get_varinfo t = match t with
  | Var (t,_) | Initialized_Var (t,_) -> t
  | _ -> raise Not_a_variable

let regexp = Str.regexp "Frama_C_periodic[^0-9]*\\([0-9]+\\)"

let create_varinfo varinfo =
  assert (not varinfo.vlogic);
  let validity = validity_from_type varinfo in
  let name = varinfo.vname in
  let periodic period =
    Kernel.feedback ~current:true ~once:true
      "Periodic variable %s of period %d@." name period;
    match validity with
    | Known(mn, mx) ->
        assert (Int.is_zero mn);
        Periodic(mn, mx, Int.of_int period)
    | _ -> assert false
  in
  let validity =
    if Str.string_match regexp name 0 then
      let period = Str.matched_group 1 name in
      let period = int_of_string period in
      periodic period
    else
      match Cil.unrollType varinfo.vtype with
        | TArray (typ, _, _, attrs) when
            Cil.hasAttribute "Frama_C_periodic" varinfo.vattr ||
            Cil.hasAttribute "Frama_C_periodic" attrs ->
          (try
             let size = Cil.bitsSizeOf typ in
             periodic size
           with Cil.SizeOfError _ -> validity)
        | _ -> validity
  in
  Var (varinfo, validity)

module Validities =
  Cil_state_builder.Varinfo_hashtbl
    (BaseDatatype)
    (struct
       let name = "Base.Validities"
       let dependencies = [ Ast.self ]
         (* No dependency on Kernel.AbsoluteValidRange.self needed:
            the null base is not present in this table (not a varinfo) *)
       let size = 117
     end)
let () = Ast.add_monotonic_state Validities.self

let create_varinfo = Validities.memo create_varinfo


let create_logic varinfo validity =
  assert (varinfo.vlogic && not (VarinfoLogic.mem varinfo));
  let base = Var (varinfo,validity) in
  VarinfoLogic.add varinfo base;
  base

let create_initialized varinfo validity =
  assert varinfo.vlogic;
  let base = Initialized_Var (varinfo,validity) in
  VarinfoLogic.add varinfo base;
  base

let find varinfo =
  if varinfo.vlogic then VarinfoLogic.find varinfo
  else create_varinfo varinfo

module LiteralStrings =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (BaseDatatype)
    (struct
       let name = "litteral strings"
       let dependencies = [ Ast.self ]
       let size = 17
     end)
let () = Ast.add_monotonic_state LiteralStrings.self

let create_string e =
  LiteralStrings.memo (fun _ -> String (Cil_const.new_raw_id (), e)) e.eid

module SetLattice = Make_Hashconsed_Lattice_Set(BaseDatatype)(Hptset)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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

open Cil_types
open Cil_datatype

type overflow_kind = Signed | Unsigned | Signed_downcast | Unsigned_downcast
type access_kind = For_reading | For_writing
type bound_kind = Lower_bound | Upper_bound

let string_of_overflow_kind = function
  | Signed -> "signed_overflow"
  | Unsigned -> "unsigned_overflow"
  | Signed_downcast -> "signed_downcast"
  | Unsigned_downcast -> "unsigned_downcast"

type alarm =
  | Division_by_zero of exp
  | Memory_access of lval * access_kind
  | Logic_memory_access (* temporary? *) of 
      term * access_kind
  | Index_out_of_bound of
      exp (* index *) 
    * exp option (* None = lower bound is zero; Some up = upper bound *) 
  | Invalid_shift of exp * int option (* strict upper bound, if any *)
  | Pointer_comparison of
      exp option (* [None] when implicit comparison to 0 *) 
    * exp
  | Differing_blocks of exp * exp
  | Overflow of
      overflow_kind
    * exp
    * Integer.t (* the bound *) 
    * bound_kind
  | Float_to_int of
      exp
    * Integer.t (* the bound *) 
    * bound_kind
  | Not_separated of lval * lval
  | Overlap of lval * lval
  | Uninitialized of lval
  | Is_nan_or_infinite of exp * fkind
  | Valid_string of exp

module D =
  Datatype.Make_with_collections
    (struct
      type t = alarm
      let name = "Alarms"
      let reprs = List.map (fun e -> Division_by_zero e) Exp.reprs

      let rank = function
	| Division_by_zero _ -> 0
	| Memory_access _ -> 1
	| Logic_memory_access _ -> 2
	| Index_out_of_bound _ -> 3
	| Invalid_shift _ -> 4
	| Pointer_comparison _ -> 5
	| Overflow _ -> 6
	| Not_separated _ -> 7
        | Overlap _ -> 8
	| Uninitialized _ -> 9
	| Is_nan_or_infinite _ -> 10
        | Float_to_int _ -> 11
        | Differing_blocks _ -> 12
        | Valid_string _ -> 13

      let compare a1 a2 = match a1, a2 with
	| Division_by_zero e1, Division_by_zero e2 -> Exp.compare e1 e2
	| Is_nan_or_infinite (e1, fk1), Is_nan_or_infinite (e2, fk2) ->
          let n = Exp.compare e1 e2 in
          if n = 0 then Extlib.compare_basic fk1 fk2 else n
	| Memory_access(lv1, access_kind1), Memory_access(lv2, access_kind2) ->
	  let n = Pervasives.compare access_kind1 access_kind2 in
	  if n = 0 then Lval.compare lv1 lv2 else n
	| Logic_memory_access(t1, b1), Logic_memory_access(t2, b2) ->
	  (* [TODO] pretty inefficient... *)
	  let n = Pervasives.compare (b1:access_kind) b2 in
	  if n = 0 then Term.compare t1 t2 else n
	| Index_out_of_bound(e11, e12), Index_out_of_bound(e21, e22) ->
	  let n = Exp.compare e11 e21 in
	  if n = 0 then Extlib.opt_compare Exp.compare e12 e22 else n
	| Invalid_shift(e1, n1), Invalid_shift(e2, n2) ->
	  let n = Exp.compare e1 e2 in
	  if n = 0 then Extlib.opt_compare Datatype.Int.compare n1 n2 else n
	| Pointer_comparison(e11, e12), Pointer_comparison(e21, e22) ->
	  let n = Extlib.opt_compare Exp.compare e11 e21 in
	  if n = 0 then Exp.compare e12 e22 else n
	| Overflow(s1, e1, n1, b1), Overflow(s2, e2, n2, b2) ->
	  let n = Pervasives.compare (s1:overflow_kind) s2 in
	  if n = 0 then
	    let n = Exp.compare e1 e2 in
	    if n = 0 then 
	      (* [TODO] pretty inefficient... *)
	      let n = Pervasives.compare (b1:bound_kind) b2 in
	      if n = 0 then Integer.compare n1 n2 else n
	    else
	      n
	  else
	    n
	| Float_to_int(e1, n1, b1), Float_to_int(e2, n2, b2) ->
	  let n = Exp.compare e1 e2 in
	  if n = 0 then 
	    (* [TODO] pretty inefficient... *)
	    let n = Pervasives.compare (b1:bound_kind) b2 in
	    if n = 0 then Integer.compare n1 n2 else n
	  else
	    n
	| Not_separated(lv11, lv12), Not_separated(lv21, lv22)
        | Overlap(lv11, lv12), Overlap(lv21, lv22)
            ->
	  let n = Lval.compare lv11 lv21 in
	  if n = 0 then Lval.compare lv12 lv22 else n
	| Uninitialized lv1, Uninitialized lv2 -> Lval.compare lv1 lv2
        | Differing_blocks (e11, e12), Differing_blocks (e21, e22) ->
          let n = Exp.compare e11 e21 in
	  if n = 0 then Exp.compare e12 e22 else n
        | Valid_string(e1), Valid_string(e2) ->
          Exp.compare e1 e2
	| _, (Division_by_zero _ | Memory_access _ | Logic_memory_access _  |
              Index_out_of_bound _ | Invalid_shift _ | Pointer_comparison _ |
              Overflow _ | Not_separated _ | Overlap _ | Uninitialized _ |
              Is_nan_or_infinite _ | Float_to_int _ | Differing_blocks _ |
              Valid_string _)
          ->
	  let n = rank a1 - rank a2 in
	  assert (n <> 0);
	  n

      let equal = Datatype.from_compare

      let hash a = match a with
	| Division_by_zero e ->
          Hashtbl.hash (rank a, Exp.hash e)
        | Is_nan_or_infinite (e, fk) -> 
          Hashtbl.hash (rank a, Exp.hash e, fk)
	| Memory_access(lv, b) -> Hashtbl.hash (rank a, Lval.hash lv, b)
	| Logic_memory_access(t, b) -> Hashtbl.hash (rank a, Term.hash t, b)
	| Index_out_of_bound(e1, e2) -> 
	  Hashtbl.hash
	    (rank a, 
	     Exp.hash e1, 
	     match e2 with None -> 0 | Some e -> 17 + Exp.hash e)
	| Invalid_shift(e, n) -> Hashtbl.hash (rank a, Exp.hash e, n)
	| Pointer_comparison(e1, e2) -> 
	  Hashtbl.hash 
	    (rank a, 
	     (match e1 with None -> 0 | Some e -> 17 + Exp.hash e), 
	     Exp.hash e2)
	| Differing_blocks (e1, e2) -> 
	  Hashtbl.hash (rank a, Exp.hash e1, Exp.hash e2)
	| Overflow(s, e, n, b) ->
	  Hashtbl.hash
	    (Hashtbl.hash (s:overflow_kind), 
	     rank a, 
	     Exp.hash e, 
	     Integer.hash n, 
	     b)
	| Float_to_int(e, n, b) ->
	  Hashtbl.hash
	    (rank a, 
	     Exp.hash e, 
	     Integer.hash n, 
	     b)
	| Not_separated(lv1, lv2) | Overlap(lv1, lv2) -> 
	  Hashtbl.hash (rank a, Lval.hash lv1, Lval.hash lv2)
	| Uninitialized lv -> Hashtbl.hash (rank a, Lval.hash lv)
        | Valid_string(e) -> Hashtbl.hash (rank a, Exp.hash e)

      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let varname = Datatype.undefined

      let pretty fmt = function
	| Division_by_zero e -> 
	  Format.fprintf fmt "Division_by_zero(@[%a@])" Printer.pp_exp e
	| Is_nan_or_infinite (e, fk) ->
	  Format.fprintf fmt "Is_nan_or_infinite(@[(%a)%a@])"
            Printer.pp_fkind fk Printer.pp_exp e
	| Memory_access(lv, read) ->
	  Format.fprintf fmt "Memory_access(@[%a@],@ %s)" 
	    Printer.pp_lval lv
	    (match read with For_reading -> "read" | For_writing -> "write")
	| Logic_memory_access(t, read) ->
	  Format.fprintf fmt "Logic_memory_access(@[%a@],@ %s)" 
	    Printer.pp_term t 
	    (match read with For_reading -> "read" | For_writing -> "write")
	| Index_out_of_bound(e1, e2) ->
	  Format.fprintf fmt "Index_out_of_bound(@[%a@]@ %s@ @[%a@])" 
	    Printer.pp_exp e1
	    (match e2 with None -> ">=" | Some _ -> "<")
	    Printer.pp_exp 
	    (match e2 with None -> Cil.zero e1.eloc | Some e -> e)
	| Invalid_shift(e, n) ->
	  Format.fprintf fmt "Invalid_shift(@[%a@]@ %s)"
	    Printer.pp_exp e
	    (match n with None -> "" | Some n -> "<= " ^ string_of_int n)
	| Pointer_comparison(e1, e2) ->
	  Format.fprintf fmt "Pointer_comparison(@[%a@],@ @[%a@])"
	    Printer.pp_exp 
	    (match e1 with None -> Cil.zero e2.eloc | Some e -> e)
	    Printer.pp_exp e2
        | Differing_blocks (e1, e2) ->
	  Format.fprintf fmt "Differing_blocks(@[%a@],@ @[%a@])"
	    Printer.pp_exp e1 Printer.pp_exp e2          
	| Overflow(s, e, n, b) ->
	  Format.fprintf fmt "%s(@[%a@]@ %s@ @[%a@])"
	    (String.capitalize (string_of_overflow_kind s))
	    Printer.pp_exp e
	    (match b with Lower_bound -> ">=" | Upper_bound -> "<=")
	    Datatype.Big_int.pretty n
	| Float_to_int(e, n, b) ->
	  Format.fprintf fmt "Float_to_int(@[%a@]@ %s@ @[%a@])"
	    Printer.pp_exp e
	    (match b with Lower_bound -> ">" | Upper_bound -> "<")
	    Datatype.Big_int.pretty
            ((match b with
	    | Lower_bound -> Integer.sub
	    | Upper_bound -> Integer.add) n Integer.one)
	| Not_separated(lv1, lv2) ->
	  Format.fprintf fmt "Not_separated(@[%a@],@ @[%a@])"
	    Lval.pretty lv1 Lval.pretty lv2
	| Overlap(lv1, lv2) ->
	  Format.fprintf fmt "Overlap(@[%a@],@ @[%a@])"
	    Lval.pretty lv1 Lval.pretty lv2
	| Uninitialized lv ->
	  Format.fprintf fmt "Uninitialized(@[%a@])" Lval.pretty lv
        | Valid_string e ->
          Format.fprintf fmt "Valid_string(@[%a@])" Exp.pretty e

      let internal_pretty_code = Datatype.undefined
      let copy = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)

include D

module Usable_emitter = struct
  include Emitter.Usable_emitter
  let local_clear _ h = Hashtbl.clear h
  let usable_get e = e
end

module Rank = State_builder.Counter(struct let name = "Alarms.Rank" end)

module State =
  Emitter.Make_table
    (Kinstr.Hashtbl)
    (Usable_emitter)
    (D.Hashtbl.Make
       (Datatype.Quadruple
	  (Code_annotation)(Kernel_function)(Stmt)(Datatype.Int)))
    (struct 
      let name = "Alarms.State" 
      let dependencies = [ Ast.self; Rank.self ] 
      let kinds = [ Emitter.Alarm ]
      let size = 97
     end)

let must_remove_annot = ref true

let () =
  State.add_hook_on_remove
    (fun e _ h ->
      if !must_remove_annot then
	D.Hashtbl.iter
	  (fun _ (a, kf, s, _) -> 
	    Annotations.remove_code_annot
	      (Emitter.Usable_emitter.get e) ~kf s a)
	  h)

module Alarm_of_annot =
  State_builder.Hashtbl
    (Code_annotation.Hashtbl)
    (D)
    (struct
      let name = "Alarms.Alarm_of_annot"
      let dependencies = [ Ast.self; Rank.self ]
      let size = 97
     end)

let self = State.self
let () = Ast.add_monotonic_state self

let emit_status emitter kf stmt annot status = 
  let p = Property.ip_of_code_annot_single kf stmt annot in
  Property_status.emit emitter ~hyps:[] p ~distinct:true status

let add_annotation tbl alarm emitter ?kf kinstr annot status =
  let add kf stmt =
    Annotations.add_code_annot emitter ~kf stmt annot;
    let id = Rank.next () in
    D.Hashtbl.add tbl alarm (annot, kf, stmt, id);
    Extlib.may (emit_status emitter kf stmt annot) status;
    Alarm_of_annot.add annot alarm;
  in
  match kinstr with
  | Kglobal -> 
    let kf = match kf with
      | None -> fst (Globals.entry_point ())
      | Some kf -> 
	Kernel.fatal "[Alarm] how function `%a' can be associated to a global \
program point"
	  Kernel_function.pretty kf
    in
    (try
       let stmt = Kernel_function.find_first_stmt kf in
       add kf stmt
     with Kernel_function.No_Statement ->
       Kernel.fatal "[Alarm] the main function has no code")
  | Kstmt stmt ->
    let kf = match kf with 
      | None -> Kernel_function.find_englobing_kf stmt
      | Some kf -> kf
    in
    add kf stmt

let get_name = function
  | Division_by_zero _ -> "division_by_zero"
  | Memory_access _ -> "mem_access"
  | Logic_memory_access _ -> "logic_mem_access"
  | Index_out_of_bound _ -> "index_bound"
  | Invalid_shift _ -> "shift"
  | Pointer_comparison _ -> "ptr_comparison"
  | Differing_blocks _ -> "differing_blocks"
  | Overflow(s, _, _, _) -> string_of_overflow_kind s
  | Not_separated _ -> "separation"
  | Overlap _ -> "overlap"
  | Uninitialized _ -> "initialisation"
  | Is_nan_or_infinite _ -> "is_nan_or_infinite"
  | Float_to_int _ -> "float_to_int"
  | Valid_string _ -> "valid_string"

let overflowed_expr_to_term e = 
  let loc = e.eloc in
  match e.enode with
  | UnOp(op, e, ty) -> 
    let t = Logic_utils.expr_to_term ~cast:true e in
    let ty = Logic_utils.typ_to_logic_type ty in
    Logic_const.term ~loc (TUnOp(op, t)) ty
  | BinOp(op, e1, e2, ty) -> 
    let t1 = Logic_utils.expr_to_term ~cast:true e1 in
    let t2 = Logic_utils.expr_to_term ~cast:true e2 in
    let ty = Logic_utils.typ_to_logic_type ty in
    Logic_const.term ~loc (TBinOp(op, t1, t2)) ty
  | _ -> Logic_utils.expr_to_term ~cast:true e

let create_predicate ?(loc=Location.unknown) alarm = 
  let aux = function
  | Division_by_zero e -> 
    (* e != 0 *)
    let loc = e.eloc in
    let t = Logic_utils.expr_to_term ~cast:true e in
    Logic_const.prel ~loc (Rneq, t, Cil.lzero ())

  | Memory_access(lv, read) -> 
    (* \valid(lv) or \valid_read(lv) according to read *)
    let valid = match read with
      | For_reading -> Logic_const.pvalid_read
      | For_writing -> Logic_const.pvalid
    in
    let e = Cil.mkAddrOrStartOf ~loc lv in
    let t = Logic_utils.expr_to_term ~cast:true e in
    valid ~loc (Logic_const.here_label, t)

  | Logic_memory_access(t, read) -> 
    (* \valid(lv) or \valid_read(lv) according to read *)
    let valid = match read with
      | For_reading -> Logic_const.pvalid_read
      | For_writing -> Logic_const.pvalid
    in
    valid ~loc (Logic_const.here_label, t)

  | Index_out_of_bound(e1, e2) ->
    (* 0 <= e1 < e2, left part if None, right part if Some e *)
    let loc = e1.eloc in
    let t1 = Logic_utils.expr_to_term ~cast:true e1 in
    (match e2 with
    | None -> Logic_const.prel ~loc (Rle, Cil.lzero (), t1)
    | Some e2 ->
      let t2 = Logic_utils.expr_to_term ~cast:true e2 in
      Logic_const.prel ~loc (Rlt, t1, t2))

  | Invalid_shift(e, n) -> 
    (* 0 <= e < n *)
    let loc = e.eloc in
    let t = Logic_utils.expr_to_term ~cast:true e in
    let low_cmp = Logic_const.prel ~loc (Rle, Cil.lzero (), t) in
    (match n with
    | None -> low_cmp
    | Some n ->
      let tn = Logic_const.tint ~loc (Integer.of_int n) in
      let up_cmp = Logic_const.prel ~loc (Rlt, t, tn) in
      Logic_const.pand ~loc (low_cmp, up_cmp))

  | Pointer_comparison(e1, e2) ->
    (* \pointer_comparable(e1, e2) *)
    let loc = e2.eloc in
    let t1 = match e1 with
      | None -> Cil.lzero ()
      | Some e -> Logic_utils.expr_to_term ~cast:true e
    in
    let t2 = Logic_utils.expr_to_term ~cast:true e2 in
    Logic_utils.pointer_comparable ~loc t1 t2

  | Valid_string(e) ->
    let loc = e.eloc in
    let t =  Logic_utils.expr_to_term ~cast:true e in
    Logic_utils.points_to_valid_string ~loc t

  | Differing_blocks(e1, e2) ->
    (* \base_addr(e1) == \base_addr(e2) *)
    let loc = e1.eloc in
    let t1 = Logic_utils.expr_to_term ~cast:true e1 in
    let here = Logic_const.here_label in
    let typ = Ctype Cil.charPtrType in
    let t1 = Logic_const.term ~loc:e1.eloc (Tbase_addr(here, t1)) typ in
    let t2 = Logic_utils.expr_to_term ~cast:true e2 in
    let t2 = Logic_const.term ~loc:e2.eloc (Tbase_addr(here, t2)) typ in
    Logic_const.prel ~loc (Req, t1, t2)

  | Overflow(_, e, n, bound) -> 
    (* n <= e or e <= n according to bound *)
    let loc = e.eloc in
    let t = overflowed_expr_to_term e in
    let tn = Logic_const.tint ~loc n in
    Logic_const.prel ~loc
      (match bound with Lower_bound -> Rle, tn, t | Upper_bound -> Rle, t, tn)

  | Float_to_int(e, n, bound) -> 
    (* n < e or e < n according to bound *)
    let loc = e.eloc in
    let t = overflowed_expr_to_term e in
    let n = 
      (match bound with Lower_bound -> Integer.sub | Upper_bound -> Integer.add)
	n Integer.one 
    in
    let tn = Logic_const.tint ~loc n in
    Logic_const.prel ~loc
      (match bound with Lower_bound -> Rlt, tn, t | Upper_bound -> Rlt, t, tn)

  | Not_separated(lv1, lv2) -> 
    (* \separated(lv1, lv2) *)
    let e1 = Cil.mkAddrOf ~loc lv1 in
    let t1 = Logic_utils.expr_to_term ~cast:true e1 in
    let e2 = Cil.mkAddrOf ~loc lv2 in
    let t2 = Logic_utils.expr_to_term ~cast:true e2 in
    Logic_const.pseparated ~loc [ t1; t2 ]

  | Overlap(lv1, lv2) -> 
    (* (lv1 == lv2) || \separated(lv1, lv2) *)
    let e1 = Cil.mkAddrOf ~loc lv1 in
    let t1 = Logic_utils.expr_to_term ~cast:true e1 in
    let e2 = Cil.mkAddrOf ~loc lv2 in
    let t2 = Logic_utils.expr_to_term ~cast:true e2 in
    let eq = Logic_const.prel ~loc (Req, t1, t2) in
    let sep = Logic_const.pseparated ~loc [ t1; t2 ] in
    Logic_const.por ~loc (eq, sep)

  | Uninitialized lv -> 
    (* \initialized(lv) *)
    let e = Cil.mkAddrOrStartOf ~loc lv in
    let t = Logic_utils.expr_to_term ~cast:false e in
    Logic_const.pinitialized ~loc (Logic_const.here_label, t)

  | Is_nan_or_infinite (e, fkind) -> 
    (* \is_finite((fkind)e) *)
    let loc = e.eloc in
    let t = Logic_utils.expr_to_term ~cast:true e in
    let typ = match fkind with
      | FFloat -> Cil.floatType
      | FDouble -> Cil.doubleType
      | FLongDouble -> Cil.longDoubleType
    in
    let t = Logic_utils.mk_cast ~loc typ t in
    (* Different signatures, depending on the type of the argument *)
    let all_is_finite = Logic_env.find_all_logic_functions "\\is_finite" in
    let compatible li =
      Logic_type.equal t.term_type (List.hd li.l_profile).lv_type
    in
    let pi =
      try List.find compatible all_is_finite
      with Not_found ->
        Kernel.fatal "Unexpected type %a for predicate \\is_finite"
          Printer.pp_logic_type t.term_type
    in
    Logic_const.unamed ~loc (Papp (pi, [], [ t ]))
  in
  let p = aux alarm in
  assert (p.name = []);
  { p with name = [ get_name alarm ] }

exception Found of (code_annotation * kernel_function * stmt * int)
let find_alarm_in_emitters tbl alarm =
  try
    Usable_emitter.Hashtbl.iter
      (fun _ h ->
	try
	  let triple = D.Hashtbl.find h alarm in
	  raise (Found triple)
	with Not_found ->
	  ())
      tbl;
    None
  with Found x ->
    Some x

let register
    emitter ?kf kinstr ?(loc=Kinstr.loc kinstr) ?status ?(save=true) alarm 
    = 
(*  Kernel.debug "registering alarm %a" D.pretty alarm;*)
  let add by_emitter alarm =
(*    Kernel.debug "adding alarm %a" D.pretty alarm;*)
    let e = Emitter.get emitter in
    let tbl = 
      try Usable_emitter.Hashtbl.find by_emitter e
      with Not_found ->
	let h = D.Hashtbl.create 7 in
	Usable_emitter.Hashtbl.add by_emitter e h;
	h
    in
    let pred = create_predicate ~loc alarm in
    let annot = Logic_const.new_code_annotation (AAssert([], pred)) in
    if save then add_annotation tbl alarm emitter ?kf kinstr annot status;
    annot
  in
  try
    let by_emitter = State.find kinstr in
    match find_alarm_in_emitters by_emitter alarm with
    | None ->
      (* somes alarms already associated to this [kinstr], 
	 but not this [alarm] *)
      add by_emitter alarm, true
    | Some (annot, kf, stmt, _) ->
      (* this alarm was already emitted *)
      Extlib.may (emit_status emitter kf stmt annot) status;
      annot, false
  with Not_found ->
    (* no alarm associated to this [kinstr] *)
    let by_emitter = Usable_emitter.Hashtbl.create 7 in
    State.add kinstr by_emitter;
    add by_emitter alarm, true

let iter f =
  State.iter
    (fun _ by_emitter ->
      Usable_emitter.Hashtbl.iter
	(fun e h ->
	  D.Hashtbl.iter
	    (fun alarm (annot, kf, stmt, rank) -> 
	      f (Usable_emitter.get e) kf stmt ~rank alarm annot) 
	    h)
	by_emitter)

let fold f =
  State.fold
    (fun _ by_emitter acc ->
      Usable_emitter.Hashtbl.fold
	(fun e h acc ->
	  D.Hashtbl.fold
	    (fun alarm (annot, kf, stmt, rank) acc -> 
	      f (Usable_emitter.get e) kf stmt ~rank alarm annot acc) 
	    h
	    acc)
	by_emitter
	acc)

let find annot = 
  try Some (Alarm_of_annot.find annot)
  with Not_found -> None

let unsafe_remove ?filter ?kinstr e = 
  let usable_e = Emitter.get e in
  let remove also_alarm by_emitter = 
    try
      let tbl = Usable_emitter.Hashtbl.find by_emitter usable_e in
      let to_be_removed = D.Hashtbl.create 7 in
      let stmt_ref = ref Cil.dummyStmt in
      let extend_del a (annot, _, stmt, _ as t) = 
	D.Hashtbl.add to_be_removed a t;
	Alarm_of_annot.remove annot;
	stmt_ref := stmt
      in
      D.Hashtbl.iter
	(fun alarm v -> 
	  match filter with
	  | Some f when not (f alarm) -> ()
	  | _ -> extend_del alarm v)
	tbl;
      if also_alarm then begin
	let remove alarm _ = D.Hashtbl.remove tbl alarm in
	D.Hashtbl.iter remove to_be_removed;
      end; (* else the alarm is removed by the global [remove] of
	      [filtered_remove] *)
      State.apply_hooks_on_remove
	(Emitter.get e)
	(Kstmt !stmt_ref) 
	to_be_removed
    with Not_found ->
      ()
  in
  let filtered_remove tbl = match filter with 
    | None -> 
      remove false tbl;
      Usable_emitter.Hashtbl.remove tbl usable_e
    | Some _ -> 
      remove true tbl
  in
  match kinstr with
  | None -> 
    State.iter (fun _ by_emitter -> filtered_remove by_emitter)
  | Some ki ->
    try
      let by_emitter = State.find ki in
      filtered_remove by_emitter
    with Not_found ->
      ()

let remove ?filter ?kinstr e =
  must_remove_annot := true;
  unsafe_remove ?filter ?kinstr e

let () = 
  Annotations.remove_alarm_ref :=
    (fun e stmt annot ->
      try
	let a = Alarm_of_annot.find annot in
	must_remove_annot := false;
	(* [JS 2013/01/09] could be more efficient but seems we only consider
	   the alarms of one statement, it should be enough yet *)
	let filter a' = a == a' in
	let kinstr = Kstmt stmt in
	remove ~filter ~kinstr (Emitter.Usable_emitter.get e)
      with Not_found ->
	())

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

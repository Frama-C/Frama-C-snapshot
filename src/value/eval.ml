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

(* $Id: eval.ml,v 1.197 2009-03-11 12:15:07 uid589 Exp $ *)

(** Analysis for values and pointers *)

open Cil_types
open Cil
open Cilutil
open Db_types
open Locations
open Abstract_interp
open Abstract_value
open Bit_utils
open Cvalue_type
open Extlib
open Ast_printer
open Value_util

let make_status st = Checked {emitter = "value analysis"; valid = st}
let status_true = make_status Cil_types.True
let status_false = make_status Cil_types.False
let status_maybe = make_status Cil_types.Maybe

module Status: sig
  val join: code_annotation -> annotation_status -> unit
  val join_predicate: identified_predicate -> annotation_status -> unit
end =
struct

  module S =
    Properties_status.Make_updater
      (struct
	 let name = "Value"
	 let dependencies = [ Db.Value.self ]
       end)

(*  let () =
    List.iter (fun self ->
		 Project.Computation.add_dependency
		   Db.Value.self
		   self
	      )
      []
      (* used to be dependent on a state such as
	 Properties_status.RTE_Status_Proxy, but not
	 a good idea after all
      *)
*)
  let merge old_s new_s = match old_s, new_s with
    | Cil_types.Unknown, status | status, Cil_types.Unknown ->
        status
    | Checked { valid = Maybe },Checked _
    | Checked _ ,Checked { valid = Maybe }
        -> status_maybe
    | Checked {valid = s1 }, Checked {valid = s2 } when s1 = s2 ->
	(* Do not share with argument to be on the safe side *)
        make_status s1
    | _ -> status_maybe

  let join ca status =
    ignore (S.CodeAnnotation.update ca (fun old -> merge old status))

  let join_predicate pred status =
    ignore (S.Predicate.update pred (fun old -> merge old status))

end

type cond =
    { exp: exp; (* The condition of the branch*)
      positive: bool; (* true: normal false: negated *)}

let get_slevel kf =
  let name = Kernel_function.get_name kf in
  Value_parameters.SlevelFunction.find name

(* Display a warning, and optionally reduce the values of [ev1] and [ev2],
   knowing that they are involved in a comparison *)
let check_comparable ~with_alarms ev1 ev2 =
  try
    if not (Location_Bytes.is_included ev1 Location_Bytes.top_int)
      || not (Location_Bytes.is_included ev2 Location_Bytes.top_int)
    then begin
      (* First check if a non-zero integer is compared to an address *)
      let null_1, rest_1 = Location_Bytes.split Base.null ev1 in
      let null_2, rest_2 = Location_Bytes.split Base.null ev2 in
      if (not (Ival.is_included null_1 Ival.zero)) &&
	(not (Location_Bytes.equal rest_2 Location_Bytes.bottom ))
      then raise Not_found;
      if (not (Ival.is_included null_2 Ival.zero)) &&
	(not (Location_Bytes.equal rest_1 Location_Bytes.bottom ))
      then raise Not_found;

      (* If both addresses are valid, they can be compared.
	 If one/both is not valid, the only way they can be
	 compared is if they are offsets in a same array t.
	 In this case, if t+n is the address of the last valid
	 location in t, t+n+1 is allowed in the comparison.
      FIXME: Take string literals into account. *)
      let loc1 = make_loc (loc_bytes_to_loc_bits rest_1) Int_Base.one in
      if (not (Locations.is_valid loc1)) ||
	let loc2 = make_loc (loc_bytes_to_loc_bits rest_2) Int_Base.one in
	(not (Locations.is_valid loc2))
      then begin
	let base_1, _offs_1 = Location_Bytes.find_lonely_key rest_1 in
	let base_2, _offs_2 = Location_Bytes.find_lonely_key rest_2 in
	if Base.compare base_1 base_2 <> 0 then raise Not_found;
	(* TODO *)
      end
    end;
    ev1, ev2
  with Not_found ->
    CilE.warn_pointer_comparison with_alarms;
    ev1, ev2


module type Domain = sig
  type state
  val eval_expr :
    with_alarms:CilE.warn_mode -> state -> exp -> state*Cvalue_type.V.t
  val do_assign : with_alarms:CilE.warn_mode -> state -> lval -> exp -> state
  val eval_cond : with_alarms:CilE.warn_mode -> state -> cond -> state
  val widen : state -> state -> state
  val join : state -> state -> state
  val call : kernel_function -> state -> state
  val return : kernel_function -> state -> state

end

module PtrRelational = struct
  type state = Relations_type.Model.t
  let eval_expr ~with_alarms state expr = match expr.enode with
  | BinOp ((MinusA | MinusPP | Eq | Ne | Ge | Le | Gt | Lt as op),e1,e2,_) ->
      let state, ev1 = !Db.Value.eval_expr_with_state ~with_alarms state e1 in
      let state, ev2 = !Db.Value.eval_expr_with_state ~with_alarms state e2 in
      CilE.set_syntactic_context (CilE.SyBinOp (op,e1,e2));
      begin
        match unrollType (typeOf e1) with
        | TFloat _ ->
            state,Cvalue_type.V.top
        | TInt _ | TPtr (_, _) | _ (* Enum ? *) ->
	    let compute_diff acc =
	      let lv1 = !Db.Value.find_lv_plus ~with_alarms state e1 in
	      let lv2 = !Db.Value.find_lv_plus ~with_alarms state e2 in
	      List.fold_left
	        (fun acc (lv1, offs1)  ->
		   let loc1 = !Db.Value.lval_to_loc_state state lv1 in
		   List.fold_left
		     (fun acc (lv2, offs2)  ->
		        let loc2 = !Db.Value.lval_to_loc_state state lv2 in
		        try
			  let new_v =
			    V.location_shift
			      (Ival.sub offs1 offs2)
			      (Relations_type.Model.compute_diff
				  state loc1 loc2)
			  in
			  assert (V.is_included new_v acc);
			  new_v
		        with Relations_type.Use_Main_Memory -> acc)
		     acc
		     lv2)
	        acc
	        lv1
            in
            match op with
            | MinusA -> state,compute_diff Cvalue_type.V.top
            | MinusPP ->
                state,let minus_val = compute_diff Cvalue_type.V.top in
                ( try
	            let size = Int_Base.project
	              (sizeof_pointed(Cil.typeOf e1))
	            in
                    let size = Int.div size (Int.of_int 8) in
                    if Int.equal size Int.one then
                      minus_val
                    else
                      let minus_val = Cvalue_type.V.find_ival minus_val in
	              Cvalue_type.V.inject_ival
	                (Ival.scale_div ~pos:true size minus_val)
                  with
	            Int_Base.Error_Top
		  | Cvalue_type.V.Not_based_on_null
		  | Not_found ->
	              V.join
	                (V.topify_arith_origin ev1)
	                (V.topify_arith_origin ev2))
            | Eq | Ne | Ge | Le | Gt | Lt ->
		state,let ev1, ev2 = check_comparable ~with_alarms ev1 ev2 in
		let f = match op with
		| Eq -> V.check_equal true
		| Ne -> V.check_equal false
		| Ge -> V.comparisons ">=" V.do_ge
		| Le -> V.comparisons "<=" V.do_le
		| Gt -> V.comparisons ">" V.do_gt
		| Lt -> V.comparisons "<" V.do_lt
		| _ -> assert false
		in
		let diff = compute_diff V.top in
		let result = f diff V.singleton_zero in
		if V.cardinal_zero_or_one result
		then result
		else f ev1 ev2
            | _ -> state,Cvalue_type.V.top

      end
  | _ -> state,Cvalue_type.V.top
end


(* set the value to false for debugging value analysis without relations *)
module UseRelations =
  Computation.Ref
    (struct include Datatype.Bool let default () = true end)
    (struct let name = "UseRelations" let dependencies = [] end)

let () = Project.Computation.add_dependency Db.Value.self UseRelations.self

let compute_call_ref = ref (fun _ -> assert false)

let remember_bases_with_locals bases_containing_locals left_loc evaled_exp =
  if Cvalue_type.V.contains_addresses_of_any_locals evaled_exp then
    let clobbered_set = Location_Bits.get_bases left_loc.loc  in
    bases_containing_locals :=
      Location_Bits.Top_Param.join clobbered_set !bases_containing_locals

let timer = ref 0

let set_loc kinstr =
  match kinstr with
  | Kglobal -> CurrentLoc.clear ()
  | Kstmt s -> CurrentLoc.set (Ast_info.loc_stmt s)

exception Leaf (* raised when nothing is known for a function :
                  no source nor specification *)

exception Not_an_exact_loc

exception Reduce_to_bottom

let pretty_call_stack fmt callstack =
  Pretty_utils.pp_flowlist ~left:"" ~sep:" <-" ~right:""
     (fun fmt {called_kf = kf} -> Kernel_function.pretty_name fmt kf)
    fmt
    callstack

let pop_call_stack () =
  call_stack := List.tl !call_stack

let push_call_stack cf =
  call_stack := cf :: !call_stack

let current_kf () = (List.hd !call_stack).called_kf

module Got_Imprecise_Value =
  Computation.Ref
    (struct include Datatype.Bool let default () = false end)
    (struct
       let name = "Eval.Got_Imprecise_Value"
       let dependencies = [ Db.Value.self ]
     end)

module Location_list = Datatype.List (Locations.Location.Datatype)

module Non_linear_assignments =
  Cil_computation.VarinfoHashtbl
    (Cil_datatype.InstrHashtbl(Location_list))
    (struct
      let name = "Non linear assignments"
      let size = 37
      let dependencies = [ Ast.self ]
    end)

let for_callbacks_stack () =
    List.map (fun {called_kf = kf; call_site = ki} -> kf,ki) !call_stack

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty_name fmt (current_kf())

exception Offset_not_based_on_Null of
	  Locations.Zone.t option * Location_Bytes.t

let warn_locals_escape is_block fundec k =
  (*TODO: find a better alarm for variables escaping block scope *)
  Value_parameters.warning ~current:true ~once:true
    "local escaping the scope of %t%a through %a"
    (swap (Pretty_utils.pp_cond is_block) "a block of ")
    !d_var fundec.svar
    Base.pretty k

let warn_locals_escape_result fundec =
  Value_parameters.warning ~current:true ~once:true
    "local escaping the scope of %a through \\result"
    !d_var fundec.svar

let do_cast ~with_alarms t expr =
  let treat inttype =
    match inttype with
    | TInt(kind,_) ->
        let size = Int.of_int (bitsSizeOf inttype) in
        let signed = isSigned kind in
	V.cast ~with_alarms ~signed ~size expr
    | TFloat _ ->
        let size = Int.of_int (bitsSizeOf inttype) in
	let result =
	  V.cast ~with_alarms ~signed:true ~size expr
	in
	result
    | _ -> assert false
  in
  match unrollType t with
  | TInt _ | TFloat _  as t' ->
      treat t'
  | TPtr _ ->
      treat theMachine.upointType
  | TEnum _ ->
      if theMachine.enum_are_signed then
        treat (TInt(IInt,[]))
      else treat (TInt(IUInt,[]))
  | TComp _ -> expr (* see test [struct_call.c] *)
  | TBuiltin_va_list _ ->
      (match with_alarms.CilE.imprecision_tracing with
       | CilE.Aignore -> ()
       | CilE.Acall f -> f ()
       | CilE.Alog ->
	   Value_parameters.warning ~once:true ~current:true
             "cast to __builtin_va_list is not precisely implemented yet");
      V.topify_arith_origin expr
  | TFun _ -> expr
  | TNamed _ -> assert false
  | TVoid _ -> assert false
  | TArray _ -> assert false

let do_promotion ~with_alarms ~src_typ ~dest_type v =
  match dest_type, src_typ with
  | TFloat _, TInt _ ->
      Cvalue_type.V.cast_int_to_float ~with_alarms (get_rounding_mode()) v
  | TInt _, TFloat _ -> Cvalue_type.V.cast_float_to_int ~with_alarms v
  | _, _ -> v

let handle_signed_overflow ~with_alarms syntactic_context typ e interpreted_e =
  match typ with
    TInt(kind, _)
      when Value_parameters.SignedOverflow.get()
	&& isSigned kind ->
	  let size = bitsSizeOf typ in
	  let mn, mx =
	    let b = Int.power_two (size-1) in
	    Int.neg b, Int.pred b
	  in
	  let all_values =
	    Cvalue_type.V.inject_ival (Ival.inject_range (Some mn) (Some mx))
	  in
	  if V.is_included interpreted_e all_values
	  then interpreted_e
	  else begin
	      CilE.set_syntactic_context syntactic_context;
	      CilE.warn_signed_overflow with_alarms e
		(Int.to_int64 mn) (Int.to_int64 mx);
	      let r = V.narrow all_values interpreted_e in
	      Value_parameters.debug
		"signed overflow: %a reduced to %a@."
		V.pretty interpreted_e
		V.pretty r;
	      r
	    end
  | _ -> interpreted_e

exception Cannot_find_lv

exception Too_linear

let warn_lval_read lv loc contents =
  let pretty_param fmt param =
    match param with
    | Location_Bits.Top_Param.Top -> Format.fprintf fmt "is imprecise"
    | Location_Bits.Top_Param.Set _s ->
        Format.fprintf fmt "is a garbled mix of %a"
          Location_Bits.Top_Param.pretty param
  in
  let pretty_param_b fmt param =
    match param with
    | Location_Bytes.Top_Param.Top ->
	Format.fprintf fmt "The contents is imprecise"
    | Location_Bytes.Top_Param.Set _s ->
          Format.fprintf fmt "It contains a garbled mix of %a"
            Location_Bytes.Top_Param.pretty param
  in
  let something_to_warn =
    match loc.loc with Location_Bits.Top _ -> true
      | Location_Bits.Map _ ->
          match contents with
          | Location_Bytes.Top _ -> true
          | Location_Bytes.Map _ -> false
  in
  if something_to_warn then
    Value_parameters.result ~current:true ~once:true
      "reading left-value @[%a@].@ @[%t%t@]"
      !Ast_printer.d_lval lv
      (fun fmt ->
         match lv with
         | Mem _, _ ->
             (match loc.loc with
             | Location_Bits.Top (param,o) when Origin.equal o Origin.top  ->
                 Format.fprintf fmt "The location %a. "
                   pretty_param param
             | Location_Bits.Top (param,orig) ->
                 Format.fprintf fmt "The location @[%a@]@ because of@ @[%a@],@ "
                   pretty_param param
                   Origin.pretty orig
             | Location_Bits.Map _ ->
                 Format.fprintf fmt "The location is @[%a@].@ "
                   Location_Bits.pretty loc.loc)
         | Var _, _ -> ())
      (fun fmt ->
         match contents with
         | Location_Bytes.Top (param,o) when Origin.equal o Origin.top ->
                 Format.fprintf fmt "@[%a.@]"
                   pretty_param_b param
         | Location_Bytes.Top (param,orig) ->
             Format.fprintf fmt "@[%a@]@ because of@ @[%a.@]"
               pretty_param_b param
               Origin.pretty orig
         | Location_Bytes.Map _ -> ())

let rec lval_to_loc ~with_alarms state lv =
  let _,_,r =
    lval_to_loc_deps_option
      ~with_alarms
      ~deps:None
      ~reduce_valid_index:(Parameters.SafeArrays.get ())
      state
      lv
  in
  r

and lval_to_loc_deps_option
    ~with_alarms ~deps (state:Relations_type.Model.t) ~reduce_valid_index
    (base,offset as lv)  =
  if not (Relations_type.Model.is_reachable state) then
    state, deps, loc_bottom
  else
    let typ = match base with
    | Var host -> host.vtype
    | Mem x -> typeOf x
    in
    try
      let state, deps, offs =
	eval_offset
          ~reduce_valid_index
          ~with_alarms deps typ state offset
      in
      base_to_loc ~with_alarms ?deps state lv base offs
    with Offset_not_based_on_Null(deps,offset) ->
      let state, deps, loc_if_there_wasnt_offset =
	base_to_loc ~with_alarms ?deps state lv base Ival.zero
      in
      state, deps,
      loc_bits_to_loc lv
	(Location_Bits.join
	    (loc_bytes_to_loc_bits offset)
	    loc_if_there_wasnt_offset.loc)

(* pc says: only called in addrOf *)
and lval_to_loc_with_offset_deps_only
    ~deps (state:Relations_type.Model.t) v
    =
  lval_to_loc_with_offset_deps_only_option ~deps:(Some deps) state v

and lval_to_loc_with_deps ~deps state lv ~with_alarms =
  lval_to_loc_deps_option ~with_alarms ~deps:(Some deps) state lv

(* pc says: only called in addrOf *)
and lval_to_loc_with_offset_deps_only_option
    ~with_alarms ~deps (state:Relations_type.Model.t) (_base, _offset as v)
    =
  lval_to_loc_deps_option
    ~with_alarms ~deps (state:Relations_type.Model.t) (v)
    ~reduce_valid_index:false


(** Detects if an expression can be considered as a lvalue even though
    it is hidden by a cast that does not change the lvalue.
    Raises [exn] if it is not an lvalue.

    TODO: When the goal is to recognize the form (cast)l-value == expr,
    it would be better and more powerful to have chains of inverse functions *)

and pass_cast ~with_alarms state exn typ e =
  (* type might be the same but their attributes.
     But volatile attribute cannot be skipped *)
  if not (Cilutil.equals
             (typeSigWithAttrs (filterAttributes "volatile") typ)
             (typeSigWithAttrs (filterAttributes "volatile") (typeOf e)))
  then
    (try
	let typeofe = typeOf e in
	(* Any volatile attribute may have an effect on the expression value *)
	if hasAttribute "volatile" (typeAttrs typeofe)
	  || hasAttribute  "volatile" (typeAttrs typ)
	then raise exn;
	let sztyp = sizeof typ in
	let szexpr = sizeof typeofe in
	let typ_ge_typeofe =
	  match sztyp,szexpr with
	    Int_Base.Value styp, Int_Base.Value sexpr -> Int.ge styp sexpr
	  | _ -> false
	in
	if typ_ge_typeofe then
	  let sityp = is_signed_int_enum_pointer typ in
	  let sisexpr = is_signed_int_enum_pointer (typeOf e) in
	  if sityp = sisexpr then ()
            (* destination type is larger and has the same sign as
	       the original type *)
	  else begin (* try to ignore the cast if it acts as identity
			on the value [e] even if signed/unsigned
			conflict. *)
	      match unrollType typ with
	      | TInt _ | TEnum _ ->
		  let size = Int.of_int (bitsSizeOf typ) in
		  let signed = sityp in
		  (try
		      let old_ival = V.find_ival
			(eval_expr ~with_alarms state e)
		      in
		      if (Ival.equal
			     old_ival
			     (Ival.cast ~size ~signed ~value:old_ival))
		      then () (* [e] is not sensitive to cast *)
		      else raise exn
		    with
		    | Not_found
		    | V.Not_based_on_null ->
			raise exn)
		    (* this is not always injective, thus cannot be
		       easily reverted. *)
	      | _ -> raise exn
	    end
	else raise exn
      with Neither_Int_Nor_Enum_Nor_Pointer
	-> raise exn)

and find_lv ~with_alarms (state:Relations_type.Model.t) ee =
  (* [BM] Do not recognize an lval whenever a volatile is involved to
     prevent copy/paste optimization. IS THIS THE RIGHTPLACE PC ?*)
  if hasAttribute "volatile" (typeAttrs (typeOf ee)) then
    raise Cannot_find_lv;
  match ee.enode with
  | Lval lv -> lv
  | CastE (typ,e) ->
      ( match unrollType typ, unrollType (typeOf e) with
	TFloat _, TFloat _ -> find_lv ~with_alarms state e
	  (* see remark at pass_cast about inverse functions *)
      | _ ->
	  pass_cast ~with_alarms state Cannot_find_lv typ e;
	  find_lv ~with_alarms state e)
  | _ -> raise Cannot_find_lv

and find_lv_plus ~with_alarms state e =
  let acc = ref [] in
  let rec find_lv_plus_rec e current_offs =
    try
      let lv = find_lv ~with_alarms state e in
      if not (hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lv)))
      then acc := (lv,current_offs) :: !acc
    with Cannot_find_lv ->
      match e.enode with
	BinOp(op, e1, e2, typ) ->
	  begin
	    match unrollType typ with
	      TFloat _ -> ()
	    | _ -> begin
		  match op with
		    PlusA ->
		      let ev1 = eval_expr ~with_alarms state e1 in
		      let ev2 = eval_expr ~with_alarms state e2 in
		      ( try
			  let ival1 = V.find_ival ev1 in
			  find_lv_plus_rec e2 (Ival.add current_offs ival1)
			with V.Not_based_on_null -> ());
		      ( try
			  let ival2 = V.find_ival ev2 in
			  find_lv_plus_rec e1 (Ival.add current_offs ival2)
			with V.Not_based_on_null -> ());
		  | (MinusA|MinusPI|PlusPI|IndexPI as b) ->
		      let ev2 = eval_expr ~with_alarms state e2 in
		      ( try
			  let ival2 = V.find_ival ev2 in
			  let ival2 =
			    if b = MinusA
			    then ival2
			    else
			      let ival2 =
				Ival.scale
				  (Int_Base.project (osizeof_pointed typ))
				  ival2
			      in
			      if b = MinusPI
			      then ival2
			      else Ival.neg ival2
			  in
			  find_lv_plus_rec e1 (Ival.sub current_offs ival2)
			with V.Not_based_on_null | Int_Base.Error_Top-> ());
		  | _ -> ()
	      end
	  end
      | CastE(typ,e) ->
	  ( try
	      pass_cast ~with_alarms  state Cannot_find_lv typ e;
	    find_lv_plus_rec e current_offs
	    with Cannot_find_lv -> ())
      | _ -> ()
  in
  find_lv_plus_rec e Ival.singleton_zero;
  (*List.iter
    (fun (lv,ival) ->
    ignore (Pretty.printf "find_lv_plus %a : %s\n"
    d_lval lv
    (pretty_to_string Ival.pretty ival)))
    !acc;*)
  !acc

and base_to_loc ~with_alarms ?deps state lv base offs =
  if Ival.equal Ival.bottom offs
  then begin
      Relations_type.Model.bottom,
    (Some Zone.bottom),
    loc_bits_to_loc lv Location_Bits.bottom
    end
  else
    let result = match base with
    | Var host ->
	let base = Base.create_varinfo host in
	state, deps,
	loc_bits_to_loc lv (Location_Bits.inject base offs)
    | Mem x ->
	let state, deps, loc_lv =
	  eval_expr_with_deps_state ~with_alarms deps state x
	in
	let loc_bits =
	  Location_Bits.location_shift
	    offs
	    (loc_bytes_to_loc_bits loc_lv)
	in
	state, deps, loc_bits_to_loc lv loc_bits
    in
    CilE.set_syntactic_context (CilE.SyMem lv);
    result

and eval_expr ~with_alarms state e =
  snd (eval_expr_with_deps ~with_alarms None state e)

and get_influential_vars ~with_alarms state cond =
  (*  Format.printf "get_influential cond:%a@.state:%a@."
      !d_exp cond
      Relations_type.Model.pretty state; *)
  let rec get_vars acc cond =
    match cond.enode with
    | Lval (Var v, off as lv) ->
	let offset =
	  try
            let _, _, offset =
	      eval_offset ~reduce_valid_index:true ~with_alarms None
		v.vtype state off
	    in
	    offset
          with Offset_not_based_on_Null _ ->
            Ival.top
	in
	if Ival.cardinal_zero_or_one offset
	then
	  (* no variable in offset can be influential *)
          let varid = Base.create_varinfo v in
          let loc =
            Locations.make_loc
              (Locations.Location_Bits.inject varid offset)
              (sizeof_lval lv)
          in
	  let contents = Relations_type.Model.find state ~with_alarms loc in
          if Location_Bytes.cardinal_zero_or_one contents
	  then (
	      (*	      Format.printf "cond:%a@.var contents:%a@.state:%a@."
			      !d_exp cond
			      Location_Bytes.pretty contents
			      Relations_type.Model.pretty state; *)
	      acc (* it's not influential *)
	    )
	  else loc :: acc
	else
	  (* a variable in offset can be influential *)
	  get_vars_offset acc off
    | Lval (Mem e, off) ->
	get_vars_offset (get_vars acc e) off
    | BinOp(_,v1,v2,_) ->
	get_vars (get_vars acc v1) v2
    | UnOp(_,v1,_) ->
	get_vars acc v1
    | CastE (_typ,exp) ->
	get_vars acc exp
    | _ -> acc
  and get_vars_offset acc offset =
    match offset with
      NoOffset -> acc
    | Field (_,off) -> get_vars_offset acc off
    | Index (ind,off) -> get_vars (get_vars_offset acc off) ind
  in
  get_vars [] cond

and reduce_by_valid_expr ~with_alarms ~positive exp state =
  try
    ignore (with_alarms);
    let lv =
      match exp.enode with
	Lval lv -> lv
      | _ -> raise Cannot_find_lv
    in
    (* TODO: utiliser find_lv_plus pour traiter plus d'expressions *)
    let loc = lval_to_loc ~with_alarms:CilE.warn_none_mode state lv in
    if not (Locations.valid_cardinal_zero_or_one loc)
    then state
    else
      let value = Relations_type.Model.find
	~with_alarms:CilE.warn_none_mode
	state
	loc
      in
      ( match value with
	Location_Bytes.Top _ ->
	  (* we won't reduce anything anyway,
	     and we may lose information if loc contains misaligned data *)
	  raise Cannot_find_lv
      | _ -> () );
      let value_as_loc =
	make_loc
	  (loc_bytes_to_loc_bits value)
	  (sizeof_pointed (Cil.typeOfLval lv))
      in
      let reduced_value =
	loc_to_loc_without_size
	  (if positive
	    then valid_part value_as_loc
	    else invalid_part value_as_loc )
      in
      if Location_Bytes.equal value reduced_value
      then state
      else begin
	  if Location_Bytes.equal Location_Bytes.bottom reduced_value
	  then Relations_type.Model.bottom
	  else
	    Relations_type.Model.reduce_binding
	      state
	      loc
	      reduced_value
	end
  with Cannot_find_lv -> state

and eval_expr_with_deps ~with_alarms deps (state : Relations_type.Model.t) e =
  let _,deps,r = eval_expr_with_deps_state ~with_alarms deps state e in
  deps, r

and eval_BinOp ~with_alarms e deps state =
  match e.enode with
    BinOp (op, e1, e2, typ) ->
      let state, deps, ev1 =
	eval_expr_with_deps_state ~with_alarms deps state e1
      in
      if V.is_bottom ev1
      then Relations_type.Model.bottom, (Some Zone.bottom) ,V.bottom
      else
	let state, deps, ev2 =
	  eval_expr_with_deps_state ~with_alarms deps state e2
	in
	if V.is_bottom ev2
	then Relations_type.Model.bottom, (Some Zone.bottom) ,V.bottom
	else begin
            let syntactic_context = CilE.SyBinOp (op,e1,e2) in
	    CilE.set_syntactic_context syntactic_context;
	    begin match unrollType (typeOf e1) with
	    | TFloat _ ->
		let interpreted_expr =
		  (* refactor: shouldn't this be somewhere else? *)
		  try
		    let f1 =
		      try
			let v1 = V.find_ival ev1 in
			Ival.project_float v1
		      with V.Not_based_on_null
		      | Ival.Float_abstract.Nan_or_infinite ->
			  Value_parameters.warning ~current:true ~once:true
                            "float value must be finite: assert(TODO)";
			  Ival.Float_abstract.top
		    in
		    let f2 =
		      try
			let v2 = V.find_ival ev2 in
			Ival.project_float v2
		      with V.Not_based_on_null
		      | Ival.Float_abstract.Nan_or_infinite ->
			  Value_parameters.warning ~current:true ~once:true
                            "converting value to float: assert(TODO)";
			  Ival.Float_abstract.top
		    in
		    let binary_float_floats _name f =
		      try
			let alarm, f = f (get_rounding_mode ()) f1 f2 in
			if alarm then
			  CilE.warn_result_nan_infinite with_alarms ;
			V.inject_ival (Ival.inject_float f)
		      with
			Ival.Float_abstract.Nan_or_infinite ->
			  CilE.warn_result_nan_infinite with_alarms ;
			  V.top_float
		      | Ival.Float_abstract.Bottom ->
			  CilE.warn_result_nan_infinite with_alarms ;
			  V.bottom
		    in
		    begin match op with
		    | PlusA ->
			binary_float_floats "+."
			  Ival.Float_abstract.add_float
		    | MinusA ->
			binary_float_floats "-."
			  Ival.Float_abstract.sub_float
		    | Mult ->
			binary_float_floats "*."
			  Ival.Float_abstract.mult_float
		    | Div ->
			if Ival.Float_abstract.contains_zero f2
			then
			  Value_parameters.warning ~once:true ~current:true
                            "float division: assert(TODO)";
			binary_float_floats "/."
			  Ival.Float_abstract.div_float
		    | Eq ->
			let contains_zero, contains_non_zero =
			  Ival.Float_abstract.equal_float_ieee f1 f2
			in
			V.interp_boolean ~contains_zero ~contains_non_zero
		    | Ne ->
			let contains_non_zero, contains_zero =
			  Ival.Float_abstract.equal_float_ieee f1 f2
			in
			V.interp_boolean ~contains_zero ~contains_non_zero
		    | Lt ->
			V.interp_boolean
			  ~contains_zero:(Ival.Float_abstract.maybe_le_ieee_float f2 f1)
			  ~contains_non_zero:(Ival.Float_abstract.maybe_lt_ieee_float f1 f2)
		    | Le ->
			V.interp_boolean
			  ~contains_zero:(Ival.Float_abstract.maybe_lt_ieee_float f2 f1)
			  ~contains_non_zero:(Ival.Float_abstract.maybe_le_ieee_float f1 f2)
		    | Gt ->
			V.interp_boolean
			  ~contains_zero:(Ival.Float_abstract.maybe_le_ieee_float f1 f2)
			  ~contains_non_zero:(Ival.Float_abstract.maybe_lt_ieee_float f2 f1)
		    | Ge ->
			V.interp_boolean
			  ~contains_zero:(Ival.Float_abstract.maybe_lt_ieee_float f1 f2)
			  ~contains_non_zero:(Ival.Float_abstract.maybe_le_ieee_float f2 f1)
		    | _ -> raise V.Not_based_on_null
		    end
		  with V.Not_based_on_null | Ival.F.Nan_or_infinite ->
		    Value_parameters.warning ~once:true ~current:true "float operation on address: assert (TODO)";

		    V.join
		      (V.topify_arith_origin ev1)
		      (V.topify_arith_origin ev2)
		in
		state, deps, interpreted_expr
	    | TInt _ | TPtr (_, _) | _ ->
		let interpreted_expr = begin match op with
		| PlusPI | IndexPI ->
		    V.add_untyped (osizeof_pointed typ) ev1 ev2
		| MinusPI ->
		    V.add_untyped (Int_Base.neg (osizeof_pointed typ)) ev1 ev2
		| PlusA ->
		    V.add_untyped (Int_Base.inject Int.one) ev1 ev2
		| MinusA | MinusPP ->
		    let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
		    if op = MinusA
		    then minus_val
		    else (* MinusPP *)
		      ( try
			  let size =
                            Int_Base.project (sizeof_pointed(Cil.typeOf e1))
			  in
			  let size = Int.div size (Int.of_int 8) in
			  if Int.equal size Int.one then
                            minus_val
			  else
                            let minus_val = Cvalue_type.V.find_ival minus_val in
                            Cvalue_type.V.inject_ival
			      (Ival.scale_div ~pos:true size minus_val)
			with
			  Int_Base.Error_Top
			| Cvalue_type.V.Not_based_on_null
			| Not_found ->
			    V.join
			      (V.topify_arith_origin ev1)
			      (V.topify_arith_origin ev2))
		| Mod -> V.c_rem ~with_alarms ev1 ev2
		| Div -> V.div ~with_alarms ev1 ev2
		| Mult -> V.arithmetic_function ~with_alarms "*" Ival.mul ev1 ev2
		| LOr ->
		    assert false
		      (* This code makes a strict evaluation: V.interp_boolean
			 ~contains_zero: (V.contains_zero ev1 &&
			 V.contains_zero ev2) ~contains_non_zero:
			 (V.contains_non_zero ev1 || V.contains_non_zero
			 ev2)*)
		| LAnd ->
		    assert false
		      (* This code makes a strict evaluation:
			 V.interp_boolean ~contains_zero: (V.contains_zero
			 ev1 || V.contains_zero ev2) ~contains_non_zero:
			 (V.contains_non_zero ev1 && V.contains_non_zero
			 ev2)*)
		| BOr -> V.oper_on_values ~with_alarms "|" Int.logor ev1 ev2
		| BXor -> V.oper_on_values ~with_alarms "^" Int.logxor ev1 ev2
		| BAnd ->
		    ( try
			let size = bitsSizeOf (typeOf e1)
			in
			V.bitwise_and ~size ev1 ev2
		      with SizeOfError _ ->
			V.join
			  (V.topify_arith_origin ev1)
			  (V.topify_arith_origin ev2))

		| Eq | Ne | Ge | Le | Gt | Lt ->
		    let ev1, ev2 = check_comparable ~with_alarms ev1 ev2 in
		    let f = match op with
		    | Eq -> V.check_equal true
		    | Ne -> V.check_equal false
		    | Ge -> V.comparisons ">=" V.do_ge
		    | Le -> V.comparisons "<=" V.do_le
		    | Gt -> V.comparisons ">" V.do_gt
		    | Lt -> V.comparisons "<" V.do_lt
		    | _ -> assert false
		    in
                    f ev1 ev2
		| Shiftrt ->
		    begin try
			let signed = is_signed_int_enum_pointer typ in
			V.shift_right ~with_alarms ~size:(bitsSizeOf typ) ~signed ev1 ev2
		      with SizeOfError _ ->
			(match with_alarms.CilE.imprecision_tracing with
			| CilE.Aignore -> ()
			| CilE.Acall f -> f ()
			| CilE.Alog -> Value_parameters.result "shifting value of unknown size");
			V.top  (* TODO: topify ... *)
		    end
		| Shiftlt ->
		    begin try
			V.shift_left ~with_alarms ~size:(bitsSizeOf typ) ev1 ev2
		      with SizeOfError _ ->
			(match with_alarms.CilE.imprecision_tracing with
			| CilE.Aignore -> ()
			| CilE.Acall f -> f ()
			| CilE.Alog -> Value_parameters.result "shifting value of unknown size");
			V.top (* TODO: topify ... *)
		    end
		  end
		in
		(* Warn if overflow in a signed int binop *)
		let interpreted_expr =
		  match op with
		    Shiftlt|Mult|MinusPP|MinusPI|IndexPI|
			PlusPI|PlusA|Div|Mod|MinusA ->
			  handle_signed_overflow
			    ~with_alarms
			    syntactic_context
			    typ
			    e
			    interpreted_expr
		  | _ -> interpreted_expr
		in
		state, deps, interpreted_expr
	    end
	  end
  | _ -> assert false

and eval_expr_with_deps_state
    ~with_alarms deps (state : Relations_type.Model.t) e =
  let state, deps, expr =
    let orig_expr = Cil.stripInfo e in
    match orig_expr.enode with
    | Info _ -> assert false
    | Const v ->
	let r =
	  begin match v with
	  | CInt64 (i,k,_s) ->
	      V.inject_int (
		  if isSigned k then Int.of_int64 i
		  else (* For 64 bits type we need to reinterpret the sign *)
		    let s = Printf.sprintf "%Lu" i in
		    Int.of_string s)
	  | CChr c ->
	      (match charConstToInt c with
              | CInt64 (i,_,_) -> V.inject_int (Int.of_int64 i)
              | _ -> assert false)
	  | CReal (f, _fsize, _) ->
	      Value_parameters.result ~once:true
		"float support is experimental";
	      let f = Ival.F.of_float f in
	      let overflow, af =
		try
		  Ival.Float_abstract.inject_r f f
		with Ival.Float_abstract.Bottom -> assert false
	      in
	      assert (not overflow);
	      V.inject_ival (Ival.inject_float af)
	  | CWStr _ ->
              Value_parameters.result "approximation because of a wide string";
              (* TODO *) V.top_int
	  | CStr s ->
              V.inject (Base.create_string s) Ival.zero
	  | CEnum {eival = e} ->
	      let _,_, r =
		eval_expr_with_deps_state ~with_alarms deps state e
	      in
	      r
	  end
	in
	state, deps, r
    | BinOp _  ->
	eval_BinOp ~with_alarms orig_expr deps state
    | Lval lv ->
	eval_lval ~with_alarms deps state lv
    | AddrOf v | StartOf v ->
	let state, deps, r =
	  lval_to_loc_with_offset_deps_only_option ~with_alarms ?deps state v
	in
	state, deps, loc_to_loc_without_size r

    | CastE (typ, e) ->
	let deps, evaled_expr =
	  eval_expr_with_deps ~with_alarms deps state e
	in
	let src_typ = unrollType (typeOf e) in
	let dest_type = unrollType typ in
	state, deps, do_promotion ~with_alarms ~dest_type ~src_typ evaled_expr

    | SizeOf typ ->
	let r =
	  try V.inject_ival
            (Ival.inject_singleton ((Int.of_int ((bitsSizeOf typ) / 8))))
	  with SizeOfError _ ->
	    error "cannot interpret sizeof(incomplete type)";
	    V.top_int
	in
	state, deps, r
    | SizeOfE e ->
	let typ = typeOf e in
	let r =
	  try V.inject_ival
	    (Ival.inject_singleton ((Int.of_int ((bitsSizeOf typ) / 8))))
	  with SizeOfError _ ->
	    error "cannot interpret sizeof(incomplete type)";
	    V.top_int
	in
	state, deps, r

    | UnOp (LNot, e, _) ->
	(* TODO:  on float, LNot is equivalent to == 0.0 *)
	let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
	CilE.set_syntactic_context (CilE.SyBinOp (Eq, Cil.zero, e));
	let _, expr =
	  check_comparable ~with_alarms V.singleton_zero expr
	in
	CilE.set_syntactic_context (CilE.SyUnOp e);
	let t1 = typeOf e in
	if isIntegralType t1 || isPointerType t1
	then
	  state, deps, V.interp_boolean
	    ~contains_zero:(V.contains_non_zero expr)
	    ~contains_non_zero:(V.contains_zero expr)
	else state, deps, V.zero_or_one

    | UnOp (Neg, e, t) ->
	let t = unrollType t in
	let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
	let syntactic_context = CilE.SyUnOp orig_expr in
	CilE.set_syntactic_context syntactic_context;
	( match t with TFloat _ ->
	  let result =
	    try
	      let v = V.find_ival expr in
	      let f =
		Ival.project_float v
	      in
	      V.inject_ival
		(Ival.inject_float (Ival.Float_abstract.neg_float f))
	    with
	      V.Not_based_on_null ->
		begin match with_alarms.CilE.others with
		  CilE.Aignore -> ()
		| CilE.Acall f -> f()
		| CilE.Alog ->
		    Value_parameters.warning ~once:true ~current:true
                      "converting address to float: assert(TODO)"
		end;
		V.topify_arith_origin expr
	    | Ival.Float_abstract.Nan_or_infinite ->
		begin match with_alarms.CilE.others with
		  CilE.Aignore -> ()
		| CilE.Acall f -> f()
		| CilE.Alog ->
		    Value_parameters.warning ~once:true ~current:true
		      "converting value to float: assert (TODO)"
		end;
		V.top_float
	  in
	  state, deps, result
	| _ ->
	    let result =
	      try
		let v = V.find_ival expr in
		V.inject_ival (Ival.neg v)
	      with V.Not_based_on_null -> V.topify_arith_origin expr
	    in
	    let result =
	      handle_signed_overflow ~with_alarms
		syntactic_context t orig_expr result
	    in
	    state, deps, result)

    | UnOp (BNot, e, _) ->
	let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
	CilE.set_syntactic_context (CilE.SyUnOp e);
	let result =
	  try
	    let v = V.find_ival expr in
	    V.inject_ival
              (Ival.apply_set_unary "~" Int.lognot v)
	  with V.Not_based_on_null -> V.topify_arith_origin expr
	in
	state, deps, result
    | AlignOfE _|AlignOf _|SizeOfStr _
	->
	Value_parameters.result
	  "C construct alignof or sizeof string not precisely handled";
	  state, deps, V.top_int
  in
  let r =
    if hasAttribute "volatile" (typeAttrs (typeOf e))
      && not (Cvalue_type.V.equal Cvalue_type.V.bottom expr)
    then V.top_int
    else
      expr
  in
  let state,r_ptr = PtrRelational.eval_expr ~with_alarms state e in
  let r = Cvalue_type.V.narrow r_ptr r in
  let r = do_cast ~with_alarms (typeOf e) r in
  state, deps, r

and eval_expr_with_deps_state_subdiv ~with_alarms deps
    (state : Relations_type.Model.t) e =
  let ((state_without_subdiv, deps_without_subdiv, result_without_subdiv) as result) =
    eval_expr_with_deps_state  ~with_alarms deps
      (state : Relations_type.Model.t) e
  in
  let subdivnb = Value_parameters.Subdivide_float_in_expr.get() in
  if subdivnb=0
  then result
  else if not (Locations.Location_Bytes.is_included result_without_subdiv Locations.Location_Bytes.top_int)
  then begin
      Value_parameters.debug ~level:2
	"subdivfloatvar: expression has an address result";
      result
    end
  else
    let compare_min, compare_max =
      if Locations.Location_Bytes.is_included result_without_subdiv Locations.Location_Bytes.top_float
      then begin
          Value_parameters.debug ~level:2
	    "subdivfloatvar: optimizing floating-point expression %a=%a"
	    !d_exp e
	    Locations.Location_Bytes.pretty result_without_subdiv;
	  Cvalue_type.V.compare_min_float, Cvalue_type.V.compare_max_float
        end
      else begin
          Value_parameters.debug ~level:2
	    "subdivfloatvar: optimizing integer expression %a=%a"
	    !d_exp e
	    Locations.Location_Bytes.pretty result_without_subdiv;
	  Cvalue_type.V.compare_min_int, Cvalue_type.V.compare_max_int
        end
    in
    let vars =
      get_influential_vars ~with_alarms:CilE.warn_none_mode state e
    in
    Value_parameters.debug ~level:2 "subdivfloatvar: variable list=%a"
      (Pretty_utils.pp_list Locations.pretty)
      vars;
    let rec try_sub vars =
      match vars with
	[] | [ _ ] ->
	  result
      | v :: tail ->
	  try
	    if not (List.exists (fun x -> Locations.loc_equal v x) tail)
	    then raise Too_linear;
	    let v_value =
	      Relations_type.Model.find
		~with_alarms:CilE.warn_none_mode
		state
		v
	    in
            Value_parameters.debug ~level:2
	      "subdivfloatvar: considering optimizing variable %a (value %a)"
	      Locations.pretty v Cvalue_type.V.pretty v_value;
	    if not (Locations.Location_Bytes.is_included v_value Locations.Location_Bytes.top_float)
	    then raise Too_linear;

	    let working_list = ref [ (v_value, result_without_subdiv) ] in
	    let had_bottom = ref false in
	    let subdiv_for_bound better_bound =
	      let rec insert_subvalue_in_list (_, exp_value as p) l =
		match l with
		  [] -> [p]
		| (_, exp_value1 as p1) :: tail ->
		    if better_bound exp_value1 exp_value >= 0
		    then p :: l
		    else p1 :: (insert_subvalue_in_list p tail)
	      in
	      let exp_subvalue subvalue l =
		let substate =
		  (* FIXME: should be relation-aware primitive *)
		  Relations_type.Model.add_binding
		    ~with_alarms:CilE.warn_none_mode
		    ~exact:true
		    state
		    v
		    subvalue
		in
		let subexpr = eval_expr ~with_alarms substate e in
		Value_parameters.debug ~level:2
		  "subdivfloatvar: computed var=%a expr=%a"
		  V.pretty subvalue
                  V.pretty subexpr;
		if Cvalue_type.V.is_bottom subexpr
		then begin
		    had_bottom := true;
		    l
		  end
		else
		  insert_subvalue_in_list (subvalue, subexpr) l
	      in
	      let subdiv l =
		match l with
		  [] ->
		    Value_parameters.debug
		      "subdivfloatvar: all reduced to bottom!!";
		    raise Ival.Can_not_subdiv
		| (value, _exp_value) :: tail ->
		    let (subvalue1, subvalue2) =
		      Cvalue_type.V.subdiv_float_interval value
		    in
		    let s = exp_subvalue subvalue1 tail
		    in
		    exp_subvalue subvalue2 s
	      in
	      try
	        for i = 1 to subdivnb do
		  working_list := subdiv !working_list;
	        done
	      with Ival.Can_not_subdiv -> ()
	    in
	    subdiv_for_bound compare_min ;
	    (* sort working_list in decreasing order
	       on the upper bounds of exp_value *)
	    let comp_exp_value (_value1,exp_value1) (_value2,exp_value2) =
	      compare_max exp_value1 exp_value2
	    in
	    working_list := List.sort comp_exp_value !working_list ;
	    if Value_parameters.debug_atleast 2 then
              List.iter
	        (function (x, e) ->
		  Value_parameters.debug
		    "subdivfloatvar: elements of list max %a %a"
	            V.pretty x V.pretty e)
		!working_list;
	    subdiv_for_bound compare_max ;
	    let working_list = !working_list in
	    if Value_parameters.debug_atleast 2 then
              List.iter
	        (function (x, e) ->
		  Value_parameters.debug
		    "subdivfloatvar: elements of final list %a %a"
	            V.pretty x V.pretty e)
		working_list;
	    let reduced_state, optimized_exp_value =
	      if !had_bottom
	      then
		let reduced_var, optimized_exp_value =
	          List.fold_left
		    (fun (accv,acce) (value, exp_value)  ->
		      Cvalue_type.V.join value accv,
		      Cvalue_type.V.join exp_value acce)
		    (Cvalue_type.V.bottom,
		    Cvalue_type.V.bottom)
		    working_list
		in
		(* FIXME: should be relation-aware primitive *)
		Relations_type.Model.add_binding
		  ~with_alarms:CilE.warn_none_mode
		  ~exact:true
		  state
		  v
		  reduced_var,
	      optimized_exp_value
	      else
		state_without_subdiv,
	      List.fold_left
		(fun acc (_value, exp_value)  ->
		  Cvalue_type.V.join exp_value acc)
		Cvalue_type.V.bottom
		working_list
	    in
	    reduced_state, deps_without_subdiv, optimized_exp_value
	  with Not_less_than | Too_linear ->
	    try_sub tail
    in
    try_sub vars

and eval_lval_using_main_memory ~with_alarms
    deps (state:Relations_type.Model.t) lv
    =
  let state,deps,loc =
    lval_to_loc_deps_option ~with_alarms ?deps state lv
      ~reduce_valid_index:(Parameters.SafeArrays.get ())
  in
  CilE.set_syntactic_context (CilE.SyMem lv);
  let result = Relations_type.Model.find ~with_alarms state loc in
  (* TODO: move into Model.find *)
  let valid_loc = Locations.valid_part loc in
  let state =
    if Location_Bits.equal loc.Locations.loc valid_loc.Locations.loc
    then state
    else begin
	match lv with
	  (*	  Mem (Lval ((_,_) as lv_mem)),NoOffset ->
		  let loc_mem =
		  lval_to_loc ~with_alarms:warn_none_mode state lv_mem
		  in
		  if Location_Bits.cardinal_zero_or_one loc_mem.Locations.loc
		  then Relations_type.Model.reduce_binding
		  state loc_mem
		  (loc_bits_to_loc_bytes valid_loc.loc)
		  else state *)
	  Mem (exp_mem),NoOffset ->
	    let lv_mem_plus_list =
	      find_lv_plus ~with_alarms:CilE.warn_none_mode state exp_mem
	    in
	    let treat_lv_mem_plus (lv_mem, plus) state =
	      let loc_mem =
		lval_to_loc ~with_alarms:CilE.warn_none_mode state lv_mem
	      in
	      if Location_Bits.cardinal_zero_or_one loc_mem.Locations.loc
	      then
		let new_val =
		  Location_Bytes.location_shift
		    (Ival.neg plus)
		    (loc_bits_to_loc_bytes valid_loc.loc)
		in
		Relations_type.Model.reduce_binding
		  state loc_mem new_val
	      else state
	    in
	    List.fold_right treat_lv_mem_plus lv_mem_plus_list state
	| _ -> state
      end
  in
  (match with_alarms.CilE.imprecision_tracing with
  | CilE.Aignore -> ()
  | CilE.Acall f -> f ()
  | CilE.Alog -> warn_lval_read lv loc result);
  let new_deps =
    match deps with
    | None -> None
    | Some deps -> Some (Zone.join deps (valid_enumerate_bits loc))
  in
  state, new_deps, result

and eval_lval ~with_alarms deps state (base,offset as lv) =
  let state, deps, result_from_main_memory =
    eval_lval_using_main_memory ~with_alarms deps state lv
  in
  let find_loc_mem sub_lv offs =
    try
      let loc = lval_to_loc ~with_alarms state sub_lv in
      let size = sizeof_lval lv in
      CilE.set_syntactic_context (CilE.SyMem lv);
      Relations_type.Model.find_mem loc size offs state
    with Relations_type.Use_Main_Memory ->
      result_from_main_memory
  in
  let result = match base with
  | Mem({enode = Lval sub_lv} as e) when UseRelations.get () ->
      let typ = typeOf e in
      begin try
          let _, _, offs =
	    eval_offset ~reduce_valid_index:(Parameters.SafeArrays.get ())
              ~with_alarms None typ state offset
          in
          find_loc_mem sub_lv offs
	with
          Offset_not_based_on_Null _ ->
            result_from_main_memory
      end
  | Mem({enode = BinOp((PlusPI|IndexPI|MinusPI as op),
                      {enode = Lval sub_lv} ,
                      e2,_)}
           as e)
      when UseRelations.get () ->
      begin
        let e2 = eval_expr ~with_alarms state e2 in
        let typ = typeOf e in
        try
          let ival = Cvalue_type.V.find_ival e2 in
          let ival = if op=MinusPI then Ival.neg ival else ival in
          let _, _, offs =
            eval_offset ~reduce_valid_index:(Parameters.SafeArrays.get ())
              ~with_alarms None typ state offset in
          let offs = (* convert to bits *)
            Ival.add
              (Ival.scale
                  (Int_Base.project (sizeof_pointed typ))
                  ival)
              offs
          in
          let result = find_loc_mem sub_lv offs in
          result
        with
        | Offset_not_based_on_Null _
        | Int_Base.Error_Top
        | Cvalue_type.V.Not_based_on_null -> result_from_main_memory
      end
  | _e ->
      result_from_main_memory
  in
  let result_inter = Cvalue_type.V.narrow result_from_main_memory result in
  state, deps, result_inter

and eval_offset ~reduce_valid_index ~with_alarms deps typ state offset =
  match offset with
  | NoOffset ->
      state, deps, Ival.singleton_zero
  | Index (exp,remaining) ->
      let typ_pointed,array_size = match (unrollType typ) with
      | TArray (t,size,_,_) -> t, size
      | TPtr(t,_) ->
          (match unrollType t with
          | TArray (t,size,_,_) -> t,size (* pointer to start of an array *)
          | _ ->
              error "Got type '%a'" !Ast_printer.d_type t;
              assert false)
      | t ->
          error "Got type '%a'" !Ast_printer.d_type t;
          assert false
      in
      let state, deps, current =
	eval_expr_with_deps_state ~with_alarms deps state exp
      in
      if V.is_bottom current
      then Relations_type.Model.bottom, (Some Zone.bottom), Ival.bottom
      else
        let state, offset =
	  try
            let v = V.find_ival current in
            let state, v =
	      if reduce_valid_index then
		try
                  let array_siz = lenOfArray64 array_size in
                  let new_v =
                    Ival.narrow (Ival.inject_range
                                    (Some Int.zero)
                                    (Some (Int.of_int64 (Int64.pred array_siz)))) v
                  in
		  let new_state =
                    if Ival.equal new_v v
		    then state
		    else begin
			begin
			  match with_alarms.CilE.others with
			  | CilE.Aignore -> ()
			  | CilE.Acall f -> f ()
			  | CilE.Alog ->
			      CilE.set_syntactic_context
				(CilE.SyBinOp
				    (IndexPI,
				    exp,
				    Cilutil.out_some array_size));
			      CilE.warn_index  with_alarms "accessing"
			end;
			state (* TODO : if the index is a variable, reduce *)
		      end
		  in
                  new_state, new_v
		with LenOfArray -> state, v
              else state, v
            in
            state, v
	  with V.Not_based_on_null ->
	    let deps, offset =
	      topify_offset
                ~with_alarms
	        deps
	        state
	        (Cvalue_type.V.topify_arith_origin current)
	        remaining
            in
	    raise (Offset_not_based_on_Null (deps,offset))
        in
	let state, deps, r =
	  eval_offset ~reduce_valid_index ~with_alarms
	    deps typ_pointed state remaining
	in
        let offset = Ival.scale_int64base (sizeof typ_pointed) offset in
        state, deps, Ival.add offset r
  | Field (fi,remaining) ->
      let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
      let state, deps, r =
	eval_offset ~reduce_valid_index ~with_alarms
	  deps
	  fi.ftype
	  state
	  remaining
      in
      state, deps, Ival.add (Ival.of_int current) r
and topify_offset ~with_alarms deps state acc offset =
  match offset with
  | NoOffset -> deps,acc
  | Field (_fi,remaining) -> topify_offset ~with_alarms deps state acc remaining
  | Index (exp,remaining) ->
      let deps, loc_index = eval_expr_with_deps ~with_alarms deps state exp in
      let acc = Location_Bytes.join
        (Cvalue_type.V.topify_arith_origin loc_index)
        acc
      in
      topify_offset ~with_alarms deps state acc remaining

(** raises [Reduce_to_bottom] and never returns [Relations_type.Model.bottom]*)
let rec eval_cond ~with_alarms state cond =
  (* Do not reduce anything if the cond is volatile.
     (This test is dumb because the cond may contain volatile l-values
     without the "volatile" attribute appearing at toplevel. pc 2007/11) *)
  if hasAttribute "volatile" (typeAttr (typeOf cond.exp)) then state
  else
    let eval_symetric_int positive binop cond_expr value =
      match positive,binop with
      | false, Eq | true, Ne -> V.diff_if_one value cond_expr
      | true, Eq | false, Ne -> V.narrow value cond_expr
      | _,_ -> value
    in
    let eval_symetric_float = eval_symetric_int in
    let eval_antisymetric_int positive binop cond_expr value =
      try match positive,binop with
      | true, Le | false, Gt ->
	  V.filter_le value ~cond_expr
      | true, Ge | false, Lt ->
	  V.filter_ge value ~cond_expr
      | false, Le | true, Gt ->
	  V.filter_gt value ~cond_expr
      | false, Ge | true, Lt ->
	  V.filter_lt value ~cond_expr
      | _,_ -> value
      with V.Error_Bottom -> V.bottom
    in
    let eval_antisymetric_float positive binop cond_expr value =
      try match positive,binop with
      | true, Le | false, Gt ->
	  V.filter_le_float value ~cond_expr
      | true, Ge | false, Lt ->
	  V.filter_ge_float value ~cond_expr
      | false, Le | true, Gt ->
	  V.filter_gt_float value ~cond_expr
      | false, Ge | true, Lt ->
	  V.filter_lt_float value ~cond_expr
      | _,_ -> value
      with V.Error_Bottom -> V.bottom
    in
    let eval_as_exact_loc state e =
      try
	let lv = find_lv ~with_alarms state e in
	let loc = lval_to_loc ~with_alarms state lv in
	if valid_cardinal_zero_or_one loc then
	  let value_for_loc =
	    do_cast
              ~with_alarms
  (*           Using (typeOf e) caused imprecisions with
	       the condition char c; ... if (c>0)
	       being transformed in if (((int)c)>0) by Cil. *)
	      (typeOfLval lv)
              (Relations_type.Model.find ~with_alarms state loc)
	  in
	  loc,value_for_loc
	else raise Not_an_exact_loc
      with Cannot_find_lv ->
	raise Not_an_exact_loc
    in
    let rec aux cond state =
      match cond.positive,cond.exp.enode with
      | _positive, BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), exp1, exp2, _typ) ->
	  let eval_eq_ineq eval_symetric eval_antisymetric =
	    let loc1 = ref None in
	    let loc2 = ref None in
	    let result1 =
              try
		let left_loc,value_for_loc = eval_as_exact_loc state exp1 in
		loc1 := Some left_loc;
		let cond_expr = eval_expr ~with_alarms state exp2 in
		let v_sym =
		  eval_symetric cond.positive binop cond_expr value_for_loc
		in
		let v_asym =
		  eval_antisymetric cond.positive binop cond_expr v_sym
		in
		if (V.equal v_asym V.bottom) then raise Reduce_to_bottom;
		Relations_type.Model.reduce_binding state left_loc v_asym
	      with Not_an_exact_loc  -> state
	    in
	    let result2 = try
		let right_loc,value_for_loc = eval_as_exact_loc state exp2 in
		loc2 := Some right_loc;
		let cond_expr = eval_expr ~with_alarms state exp1
		in
		let v_sym = eval_symetric
		  cond.positive binop cond_expr value_for_loc
		in
		let v_asym = eval_antisymetric cond.positive
		  (match binop with Gt -> Lt | Lt -> Gt | Le -> Ge | Ge -> Le
		  | _ -> binop)
		  cond_expr
		  v_sym
		in
		if V.equal v_asym V.bottom then
		  raise Reduce_to_bottom;
		Relations_type.Model.reduce_binding result1 right_loc v_asym
	      with Not_an_exact_loc -> result1
	    in
	    let result3 =
	      begin match (cond.positive, binop), !loc1, !loc2 with
		((true,Eq)|(false, Ne)), Some(left_loc), Some(right_loc) ->
		  Relations_type.Model.reduce_equality
		    result2 left_loc right_loc
	      | _ -> result2
	      end
	    in
	    result3
	  in
	  let t1 = unrollType (typeOf exp1) in
	  if isIntegralType t1 || isPointerType t1
	  then
	    eval_eq_ineq eval_symetric_int eval_antisymetric_int
	  else
	    eval_eq_ineq eval_symetric_float eval_antisymetric_float
      | true, BinOp (LAnd, exp1, exp2, _)
      | false, BinOp (LOr, exp1, exp2, _) ->
          let new_state = aux {cond with exp = exp1} state in
	  let result = aux {cond with exp = exp2} new_state in
	  result
      | false, BinOp (LAnd, exp1, exp2, _)
      | true, BinOp (LOr, exp1, exp2, _) ->
          let new_v1 = try aux {cond with exp = exp1} state
            with Reduce_to_bottom -> Relations_type.Model.bottom
          in let new_v2 = try aux {cond with exp = exp2} state
            with Reduce_to_bottom -> Relations_type.Model.bottom
          in
             Relations_type.Model.join new_v1 new_v2

      | _, UnOp(LNot,exp,_) ->
          aux
            { positive = not cond.positive;
              exp = exp; }
            state
      | _, Lval _
          when let t = typeOf cond.exp in
               isIntegralType t || isPointerType t
               ->
          (* "if (c)" is equivalent to "if(!(c==0))" *)
          (try
              let loc,value_for_loc = eval_as_exact_loc state cond.exp  in
              let new_value =
		eval_symetric_int (not cond.positive)
                  Eq
                  (V.inject_ival Ival.singleton_zero)
                  value_for_loc
              in
              if V.equal new_value V.bottom then
		raise Reduce_to_bottom
              else
		Relations_type.Model.reduce_binding
                  state loc new_value
            with Not_an_exact_loc  -> state)
      | _ -> state
    in
    let result =
      aux cond state
    in
    let condition_may_still_be_true_in_state env =
      let cond_interp = eval_expr ~with_alarms env cond.exp in
      (not cond.positive || V.contains_non_zero cond_interp) &&
	(cond.positive || V.contains_zero cond_interp)
    in
    if (not (Relations_type.Model.equal result state)) &&
      (not (condition_may_still_be_true_in_state result))
    then raise Reduce_to_bottom;
    let is_enumerable v =
      let v_interp =
	Relations_type.Model.find ~with_alarms result v in
      ignore (Location_Bytes.cardinal_less_than v_interp 6);
      v_interp
    in
    let rec enumerate_one_var l =
      match l with
      | [] -> raise Not_found
      | v::t ->
          try
            let v_interp = is_enumerable v in
            v,v_interp,t
          with Abstract_interp.Not_less_than ->
	    enumerate_one_var t
    in
    let invert_cond vl =
      try
	let v1,v_interp1, _tail = enumerate_one_var vl in
	let f one_val acc =
	  (* interpret cond in an environment where v -> one_val
	  *)
	  let env =
            Relations_type.Model.reduce_binding
	      result v1 one_val
	  in
	  if condition_may_still_be_true_in_state env
	  then begin
	      (* stays *)
	      Location_Bytes.join one_val acc
	    end
	  else begin
	      (* goes *)
	      acc
	    end
	in
	let new_v_interp =
          Location_Bytes.fold_enum
	    ~split_non_enumerable:2
            f v_interp1 Location_Bytes.bottom
	in
	let state_value =
	  if V.equal new_v_interp V.bottom
	  then raise Reduce_to_bottom
          else Relations_type.Model.reduce_binding result v1 new_v_interp
	in
	state_value
      with Not_found -> result
    in
    let result1 =
      invert_cond (get_influential_vars ~with_alarms result cond.exp)
    in
    if not (Relations_type.Model.is_reachable result1)
    then raise Reduce_to_bottom
    else result1

exception Ignore
  (* raised to completely ignore an instruction or statement *)

(* See bug report fs#182 *)
let resolv_func_vinfo ~with_alarms deps state funcexp =
  match funcexp.enode with
  | Lval (Var vinfo,NoOffset) ->
      deps, Kernel_function.Set.singleton (Globals.Functions.get vinfo)
  | Lval (Mem v,NoOffset) ->
      let deps, loc = eval_expr_with_deps ~with_alarms deps state v in
      let fundecs = List.fold_left
	(fun acc varid ->
	   match varid with
           | Base.String (_,_) ->
               Value_parameters.warning ~once:true ~current:true
		 "Function pointer call at string position in memory: ignoring this particular value: assert(TODO)";
               acc
	   | Base.Null ->
               Value_parameters.warning ~once:true ~current:true
                 "Function pointer call at absolute position in memory: ignoring this particular value: assert(TODO)";
               acc
	   | Base.Cell_class _ ->
               Value_parameters.warning ~once:true ~current:true
		 "Function pointer call at memory cell class: ignoring this particular value: assert(TODO)";
               acc
	   | Base.Var (v,_) | Base.Initialized_Var (v,_) ->
	       Kernel_function.Set.add (Globals.Functions.get v) acc
        )
	Kernel_function.Set.empty
	(try
           Location_Bytes.get_keys_exclusive Ival.zero loc
         with Location_Bytes.Not_all_keys ->
           Value_parameters.warning ~once:true ~current:true
             "Function pointer call is completly unknown: assuming no effects: assert(TODO)";
	   raise Leaf)
      in
      (* (ignore (Errormsg.log
         "Function pointer resolved to %d functions.\n"
         (List.length fundecs)); true);*)
      deps, fundecs
  | _ ->
      assert false

let make_well size hidden_base state loc =
  let well = Cvalue_type.V.inject_top_origin
    Origin.Well
    (Cvalue_type.V.Top_Param.O.singleton hidden_base)
  in
  let well_loc =
    Locations.make_loc
      (Location_Bits.inject hidden_base Ival.zero)
      (Int_Base.inject size)
  in
  let state_with_well =
    Relations_type.Model.add_binding
      ~with_alarms:CilE.warn_none_mode
      ~exact:true
      state
      well_loc
      well
  in
  Relations_type.Model.add_binding
    ~with_alarms:CilE.warn_none_mode
    ~exact:true
    state_with_well
    loc
    well


(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  CurrentLoc.set varinfo.vdecl;
  let rec add_offsetmap depth v name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = loc_of_typoffset v typ_orig offset_orig in
    let must_initialize =
      (not (hasAttribute "const" (typeAttrs typ))) ||
        (Cvalue_type.V.equal
            (Relations_type.Model.find ~with_alarms:CilE.warn_none_mode state loc)
            Cvalue_type.V.top)
    in
    if not must_initialize
      (* if we do not have an initializer for this const, we generate
	 a formal constant *)
    then state else
      match typ with
      | TInt _ | TEnum (_, _)->
          (*	  (match Base.validity v with
                  | Base.Known _ -> *)
          Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
	    ~exact:true
	    state
	    loc
	    Cvalue_type.V.top_int
            (*       | Base.Unknown ->
                     Relations_type.Model.create_initial
                     ~v:Cvalue_type.V.top_int
                     ~state
                     ~base:v
                     ~modu:Int.one
                     ~size:(8+sizeofpointer ())


                     | Base.All -> assert false) *)
      | TFloat _ ->
          Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
	    ~exact:true
	    state
	    loc
	    Cvalue_type.V.top_float
      | TFun _ ->
	  Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
	    ~exact:true
	    state
	    loc
	    (Cvalue_type.V.top_leaf_origin ())
      | TPtr (typ, _) as full_typ
	  when depth <= Value_parameters.AutomaticContextMaxDepth.get () ->
          let attr = typeAttr full_typ in

          if not (isVoidType typ) && not (isFunctionType typ) then
            let i = match findAttribute "arraylen" attr with
            | [AInt i] -> i
            | _ -> Value_parameters.AutomaticContextMaxWidth.get ()
            in
            let pointed_typ = TArray(typ,Some (integer i),empty_size_cache (), [])
            in
	    (* first create a new varid and offsetmap for the
	       "hidden location" *)
	    let hidden_var_name =
	      Cabs2cil.fresh_global ("star_" ^ name)
	    in
            let name_desc = "*"^name_desc in
	    let hidden_var =
              makeGlobalVar
                ~generated:false ~logic:true hidden_var_name pointed_typ
            in
            hidden_var.vdescr <- Some name_desc;
            let hidden_base = Base.create_logic
              hidden_var
              (match Base.validity_from_type hidden_var with
               | Base.Known (a,b)
		   when not (Value_parameters.AllocatedContextValid.get ()) ->
		   Base.Unknown (a,b)
               | (Base.All |  Base.Unknown _ | Base.Known _)  as s -> s)
            in
            let state = add_offsetmap
	      (depth + 1)
	      hidden_base
	      name_desc
	      hidden_var_name
	      pointed_typ
	      NoOffset
	      pointed_typ
	      state
	    in
	    let value = Cvalue_type.V.inject hidden_base (Ival.zero)
	    in
	    let value =
	      if Value_parameters.AllocatedContextValid.get ()
	      then value
	      else Cvalue_type.V.join Cvalue_type.V.singleton_zero value
	    in
	    Relations_type.Model.add_binding
	      ~with_alarms:CilE.warn_none_mode
	      ~exact:true
	      state
	      loc
	      value
          else
            let hidden_var_name =
	      Cabs2cil.fresh_global ("star_" ^ name)
	    in
            let name_desc = "*"^name_desc in
	    let hidden_var =
              makeGlobalVar ~generated:false ~logic:true hidden_var_name typ
            in
            hidden_var.vdescr <- Some name_desc;
            let hidden_base =
              Base.create_logic
		hidden_var
		(if Value_parameters.AllocatedContextValid.get () then
		   Base.Known (Int.zero,Bit_utils.max_bit_address ())
		 else
		   Base.Unknown (Int.zero,Bit_utils.max_bit_address ()))
            in
            make_well (Bit_utils.max_bit_size ()) hidden_base state loc

      | TArray (typ, len, _, _) ->
          begin try
            let size = lenOfArray len in
            let state = ref state in
            let treat_index (i : int) =
	      let offset =
	        addOffset
	          (Index (integer i, NoOffset))
	          offset_orig
	      in
	      let name = name ^ "_" ^ (string_of_int i) ^ "nth" in
	      let name_desc = name_desc ^ "[" ^ (string_of_int i) ^ "]" in
	      state := (add_offsetmap depth v
                          name_desc name typ
                          offset typ_orig !state)
            in
            for i = 0 to pred size do
	      treat_index i
            done;
            !state
          with LenOfArray ->
            Value_parameters.result ~once:true ~current:true "could not find a size for array";
            state
          end
      | TComp ({cstruct=true;} as compinfo, _, _) -> (* Struct *)
          let treat_field (next_offset,state) field =
            let new_offset = Field (field, NoOffset) in
            let offset =
	      addOffset
	        new_offset
	        offset_orig
            in
            let field_offset,field_width = bitsOffset typ_orig offset in
            let state =
              if field_offset>next_offset then (* padding bits needs filling*)
                let loc = make_loc
                  (Location_Bits.inject v (Ival.of_int next_offset))
                  (Int_Base.inject (Int.of_int (field_offset-next_offset)))
                in
	        Relations_type.Model.add_binding_unspecified
	          state
	          loc
              else state
            in
            field_offset+field_width,
            add_offsetmap
	      depth
	      v
	      (name_desc ^ "." ^ field.fname)
	      (name^"__"^field.fname)
	      field.ftype
	      offset
	      typ_orig
	      state
          in
	  begin try
            let boff,bwidth = bitsOffset typ_orig offset_orig in
            let last_offset,state= List.fold_left
              treat_field
              (boff,state)
              compinfo.cfields
            in
            if last_offset<(boff+bwidth) then (* padding at end of struct*)
              let loc = make_loc
		(Location_Bits.inject v (Ival.of_int last_offset))
		(Int_Base.inject (Int.of_int (boff+bwidth-last_offset)))
              in
	      Relations_type.Model.add_binding_unspecified
		state
		loc
            else state
	  with Cil.SizeOfError _ -> state
	  end
      | TComp ({cstruct=false}, _, _) when
          is_fully_arithmetic typ
          -> (* Union of arithmetic types *)
          Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue_type.V.top_int

      | TPtr _ when Value_parameters.AllocatedContextValid.get () ->
          (* deep pointers map to NULL in this case *)
          Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue_type.V.singleton_zero

      | TBuiltin_va_list _ | TComp _ | TVoid _  | TPtr  _ ->
          (* variable arguments or union with non-arithmetic type or deep pointers *)

          (* first create a new varid and offsetmap for the
             "hidden location" *)
          let hidden_var_name =
            Cabs2cil.fresh_global (name^"_WELL")
          in
          let hidden_var =
            makeGlobalVar ~logic:true hidden_var_name charType
          in
          hidden_var.vdescr <- Some (name_desc^"_WELL");
          let size = Bit_utils.max_bit_size () in
          let hidden_base =
            Base.create_logic
              hidden_var
              (Base.Known (Int.zero,Bit_utils.max_bit_address ()))
          in
          make_well size hidden_base state loc
      | TNamed (_, _)  -> assert false
  in
  add_offsetmap
    0
    (Base.create_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state

let initial_state_only_globals =
  let module S =
    Computation.OptionRef
      (Relations_type.Model.Datatype)
      (struct
	 let name = "only_globals"
	 let dependencies =
	   [ Ast.self; Parameters.LibEntry.self; Parameters.MainFunction.self ]
       end)
  in
function () ->
 let compute ()  =
  Value_parameters.debug ~level:2 "Computing globals values";
  let state = ref Relations_type.Model.empty in
  let complete_init last_bitsoffset typ _l lval =
    (* Now process the non initialized bits defaulting to 0 *)
    begin try
      let size_to_add, offset =
        bitsSizeOf typ - last_bitsoffset,
        Ival.inject_singleton (Int.of_int last_bitsoffset)
      in
      assert (size_to_add >= 0);
      if size_to_add <> 0 then
	let loc =
	  match lval with
          | (Var vinfo, _ (* In case of a string this is not [NoOffset] ! *)) -> let base = Base.create_varinfo vinfo in
            let loc =
	      Location_Bits.inject base offset
            in
            make_loc
	      loc
              (Int_Base.inject (Int.of_int size_to_add))
          | _ -> error "Whacky initializer ? Please report.";
	      assert false
	in
	let v =
	  if hasAttribute "volatile" (typeAttrs typ)
	  then V.top_int
	  else V.singleton_zero
	in
        state :=
          Relations_type.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            !state
	    loc
            v
    with Cil.SizeOfError _ ->
      Value_parameters.result ~once:true ~current:true
        "cannot provide a default initializer: size is unknown"
    end
  in
  let rec eval_init lval init =
    match init with
    | SingleInit exp ->
	let loc = lval_to_loc ~with_alarms:CilE.warn_none_mode Relations_type.Model.empty lval
	in
	let exact = cardinal_zero_or_one loc in
	assert (if not exact then (Cil.warning "In global initialisation, the location can not be represented. Aborting@."; exit 1); true);
	let value =
	  eval_expr ~with_alarms:(warn_all_quiet_mode ())
	    Relations_type.Model.empty
	    exp
	in
	let v =
	  if hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lval))
	  then V.top_int
	  else value
	in
	state :=
	  Relations_type.Model.add_binding ~with_alarms:CilE.warn_none_mode ~exact
	    !state loc v

    | CompoundInit (base_typ, l) ->
        if not (hasAttribute "volatile" (typeAttrs base_typ)) then
          let last_bitsoffset = foldLeftCompound
            ~implicit:false
            ~doinit:
            (fun off init typ (acc:int) ->
               let o,w = bitsOffset base_typ off in
               if acc<o then begin (* topify the padding bits *)
                 let vi, (base_off,_) =
		   (match lval with
                    | Var vinfo, abs_offset ->
                        vinfo,
                        (bitsOffset vinfo.vtype abs_offset)
                    | _ ->
                        Value_parameters.fatal "Whacky initializer?")
                 in
                 let loc_bits =
                   Location_Bits.inject
                     (Base.create_varinfo vi)
                     (Ival.inject_singleton (Int.of_int (base_off+acc)))
                 in
                 let loc_size = Int_Base.inject (Int.of_int (o-acc)) in
                 state := Relations_type.Model.add_binding_unspecified
                   !state
                   (make_loc loc_bits loc_size)
               end else assert (acc=o);
               if hasAttribute "volatile" (typeAttrs typ) then
                 Value_parameters.warning ~current:true ~once:true
                   "global initialization of volatile value ignored"
               else
                 eval_init (addOffsetLval off lval) init;
               o+w)
            ~ct:base_typ
            ~initl:l
            ~acc:0 in
          complete_init last_bitsoffset base_typ l lval
        else ()
  in
  Globals.Vars.iter
    (fun varinfo init ->
       if not varinfo.vlogic then begin
	 CurrentLoc.set varinfo.vdecl;
	 match init.init with
	 | None (*when
		  isCompleteType varinfo.vtype*)
           -> (* Default to zero init thanks to ANSI p126 6.7.8.10 *)
             (* eval_init (Var varinfo, NoOffset) (makeZeroInit varinfo.vtype)*)
             if varinfo.vstorage = Extern then
               (* Must not initialize when the storage is extern. *)
               state := initialize_var_using_type varinfo !state
             else complete_init 0 varinfo.vtype [] (Var varinfo,NoOffset)
	       (*       | None ->
               (* Cannot initialize with a default when type is incomplete. *)
			()*)
	 | Some i ->
             eval_init (Var varinfo,NoOffset) i
       end);

  (** Bind the declared range for NULL to uninitialized *)
  if Int.le
    (Base.min_valid_absolute_address ())
    (Base.max_valid_absolute_address ())
  then begin
    let loc_bits = Location_Bits.inject_ival
      (Ival.inject_singleton (Base.min_valid_absolute_address ()))
    in
    let loc_size =
      Int_Base.inject
	(Int.length
	   (Base.min_valid_absolute_address ())
	   (Base.max_valid_absolute_address ()))
    in
    if true (* TODO: command line option *)
    then
      state :=
	Relations_type.Model.add_binding
          ~with_alarms:CilE.warn_none_mode
	  ~exact:true
	  !state
	  (make_loc loc_bits loc_size)
	  Cvalue_type.V.top_int
    else
      state :=
	Relations_type.Model.add_binding_unspecified
	  (*          ~with_alarms:warn_none_mode
		      ~exact:true *)
	  !state
	  (make_loc loc_bits loc_size)
	  (*	  Cvalue_type.V.bottom *)
  end;
  let result = !state in
  result
in
 S.memo compute



type predicate_value = True | False | Unknown
exception Stop
let lop_to_cop op =
  match op with
  | Req -> Eq
  | Rneq -> Ne
  | Rle -> Le
  | Rge -> Ge
  | Rlt -> Lt
  | Rgt -> Gt

let rec fold_on_disjunction f p acc =
  match p.content with
  | Por (p1,p2 ) -> fold_on_disjunction f p2 (fold_on_disjunction f p1 acc)
  | _ -> f p acc

let count_disjunction p = fold_on_disjunction (fun _pred -> succ) p 0

exception Predicate_alarm

let raise_predicate_alarm () = raise Predicate_alarm

let warn_raise_mode =
  { CilE.imprecision_tracing = CilE.Aignore ;
    others = CilE.Acall raise_predicate_alarm ;
    unspecified = CilE.Acall raise_predicate_alarm }

let rec reduce_by_predicate ~result state positive p =
  let result =
    match positive,p.content with
    | true,Ptrue | false,Pfalse -> state
    | true,Pfalse | false,Ptrue -> Relations_type.Model.bottom
    | true,Pand (p1,p2 ) | false,Por(p1,p2)->
        reduce_by_predicate ~result
	  (reduce_by_predicate ~result state positive p1)
	  positive
	  p2
    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
        Relations_type.Model.join
          (reduce_by_predicate ~result state positive p1)
          (reduce_by_predicate ~result state positive p2)
    | _,Pnot p -> reduce_by_predicate ~result state (not positive) p
    | true,Piff (p1, p2) ->
	let red1 =
          reduce_by_predicate ~result state true (Logic_const.pand (p1, p2))
	in
	let red2 =
          reduce_by_predicate ~result state false
	    (Logic_const.por (p1, p2))
	in
	Relations_type.Model.join red1 red2
    | false,Piff (p1, p2) ->
	reduce_by_predicate ~result  state true
	  (Logic_const.por
	     (Logic_const.pand (p1, Logic_const.pnot p2),
	      Logic_const.pand (Logic_const.pnot p1, p2)))
    | _,Pxor(p1,p2) ->
	reduce_by_predicate ~result  state (not positive) (Logic_const.piff(p1, p2))
    | _,Prel (op,t1,t2) ->
        begin try
          let c1 = !Db.Properties.Interp.term_to_exp ~result t1 in
          let c2 = !Db.Properties.Interp.term_to_exp ~result t2 in
          let t = dummy_exp (BinOp(lop_to_cop op, c1, c2, intType)) in
          let state =
	    eval_cond ~with_alarms:warn_raise_mode
	      state { positive = positive ; exp = t }
          in
          state
        with
          Invalid_argument "not an lvalue" -> state
	| Reduce_to_bottom ->
	    Relations_type.Model.bottom
	      (* if the exception was obtained without an alarm emitted,
		 it is correct to return the bottom state *)
	| Predicate_alarm -> state
      end

    | _,Pvalid tsets ->
        begin try
          let exps = !Db.Properties.Interp.loc_to_exp ~result tsets in
          List.fold_left
	    (fun state exp ->
               reduce_by_valid_expr ~with_alarms:warn_raise_mode ~positive
                 exp state) state exps
	  with Invalid_argument "not an lvalue" -> state
	  | Predicate_alarm -> state
	end

    | true,Pimplies (_,_) -> state

    | false,Pimplies (_,_) -> state

    | _,Papp _ (* | _,Pnamed _ *) | _,Pold _ | _,Pat _ -> state
    | _,Pexists (_varl, _p1) | _,Pforall (_varl, _p1) -> state
    | _,Pfresh _
    | _,Pvalid_range (_, _, _)| _,Pvalid_index (_, _)
    | _,Plet (_, _) | _,Pif (_, _, _)
    | _,Psubtype _
        -> state
    | _, Pseparated _ -> state

  in
  result

exception Does_not_improve

let reduce_by_disjunction ~result states n p =
  if State_set.is_empty states
  then states
  else if (State_set.length states) * (count_disjunction p) <= n
  then begin
      let treat_state state acc =
	let treat_pred pred acc =
	  let result = reduce_by_predicate ~result  state true pred in
	  if Relations_type.Model.equal result state
	  then raise Does_not_improve
	  else State_set.add result acc
	in
	try
	  fold_on_disjunction treat_pred p acc
	with
	  Does_not_improve -> State_set.add state acc
      in
      State_set.fold treat_state states State_set.empty
    end
  else
    State_set.fold
      (fun state acc ->
	State_set.add (reduce_by_predicate ~result state true p) acc)
      states
      State_set.empty

let eval_predicate ~result state pred =
  let rec do_eval state p =
    match p.content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
        begin match do_eval state p1 with
        | True -> do_eval state p2
        | False -> False
        | Unknown ->
	    begin match do_eval (reduce_by_predicate ~result state true p1) p2 with
	      False -> False
	    | _ -> Unknown
	    end
        end
    | Por (p1,p2 ) ->
(*        begin match do_eval state p1,do_eval state p2 with
        | True, _| _, True ->  True
        | False, False -> False
        | _ -> Unknown
        end *)
	begin match do_eval state p1 with
        | True ->  True
	| False -> do_eval state p2
	| Unknown ->
            begin match do_eval (reduce_by_predicate ~result state false p1) p2 with
	      True -> True
	    | _ -> Unknown
	    end
	end
    | Pxor (p1,p2) ->
	begin match do_eval state p1, do_eval state p2 with
	  | True, True -> False
	  | False, False -> False
	  | True, False | False, True -> True
	  | Unknown, _ | _, Unknown -> Unknown
	end
    | Piff (p1,p2 ) ->
        begin match do_eval state p1,do_eval state p2 with
        | True, True | False, False ->  True
        | Unknown, _ | _, Unknown -> Unknown
        | _ -> False
        end
    | Papp _ (* | Pnamed _ *) | Pold _ | Pat _ -> Unknown
    | Pvalid tsets -> begin
        try
          let cexps = !Db.Properties.Interp.loc_to_exp ~result tsets in
          List.fold_left
            (fun res cexp ->
               match res with
                   Unknown | False -> res
                 | True ->
                    let typ = typeOf cexp in
                     if isPointerType typ then
                       let evaled =
                         loc_bytes_to_loc_bits
                           (eval_expr ~with_alarms:warn_raise_mode state cexp)
                       in
                       let size = sizeof_pointed typ in
                       let loc = Locations.make_loc evaled size in
                       if Locations.is_valid loc
	               then True
	               else Unknown
	                 (* TODO: the else case can be improved
                            by distinguishing the locations made only
                            of invalid values (-> False)*)
                     else Unknown(*TODO: global arrays fall here *))
            True cexps
        with
            Invalid_argument "not an lvalue" -> Unknown
	  | Predicate_alarm -> Unknown
      end
    | Prel (op,t1,t2) ->
        begin
          try
            let cexp1 = !Db.Properties.Interp.term_to_exp ~result t1 in
            let cexp2 = !Db.Properties.Interp.term_to_exp ~result t2 in
            let cops =
              dummy_exp (BinOp(lop_to_cop op,
                    cexp1,
                    cexp2,
                    intType))
            in
            let evaled = eval_expr ~with_alarms:warn_raise_mode state cops in
            if Location_Bytes.equal
              evaled
              Location_Bytes.singleton_zero
            then
              False
            else if Location_Bytes.equal
              evaled
              Location_Bytes.singleton_one
            then
              True
            else Unknown
          with
            Invalid_argument "not an lvalue" -> Unknown
	  | Predicate_alarm -> Unknown
        end
    | Pexists (varl, p1) | Pforall (varl, p1) ->
        let result =
	  begin try
          let state = List.fold_left
            (fun acc var ->
               match var.lv_origin with
                 None -> raise Exit
               | Some vi ->
                   let loc = loc_of_varinfo vi in
                   Relations_type.Model.add_binding
		     ~with_alarms:warn_raise_mode ~exact:true
                     acc loc Location_Bytes.top)
            state
            varl
          in
          do_eval state p1
        with
	  Exit -> Unknown
	| Predicate_alarm -> Unknown
        end
        in
        begin match p.content with
        | Pexists _ -> if result = False then False else Unknown
        | Pforall _ -> if result = True then True else Unknown
        | _ -> assert false
        end

    | Pnot p ->  begin match do_eval state p with
      | True -> False
      | False -> True
      | Unknown -> Unknown
      end
    | Pimplies (p1,p2) ->
	do_eval state (Logic_const.por ((Logic_const.pnot p1), p2))
    | Pseparated (_tset_l) -> Unknown
    | Pfresh _
    | Pvalid_range (_, _, _)| Pvalid_index (_, _)
    | Plet (_, _) | Pif (_, _, _) -> Unknown
    | Psubtype _
        -> Unknown

  in
  try
    match State_set.fold
      (fun s acc ->
         match do_eval s pred with
         | Unknown -> raise Stop
         |( True | False ) as arg ->
            (match acc with
             | None -> Some arg
             | Some old when old = arg -> Some arg
             | _ -> raise Stop))
      state
      None
    with
    | None -> True
    | Some v -> v
  with Stop -> Unknown

let string_of_status result =
  (match result with
  | Unknown -> "unknown"
  | True -> "valid"
  | False -> "invalid")

let check_postconditions ~result ~slevel header init_state state kind behaviors =
  let incorporate_behavior state b =
    if b.b_post_cond = [] then state
    else
      let vc = Ast_info.behavior_postcondition b kind in
      let assumes =
        (Logic_const.pands
           (List.map Logic_const.pred_of_id_pred b.b_assumes))
      in
      let activated = eval_predicate ~result:None init_state assumes in
      let update_status st =
	List.iter
	  (fun (k, post) -> if k = kind then Status.join_predicate post st)
	  b.b_post_cond
      in
      match activated with
      | True ->
          (let res = eval_predicate ~result state vc in
	   Value_parameters.result ~once:true ~current:true
             "%s behavior %s: postcondition got status %s"
	     header b.b_name
	     (string_of_status res);
	   match res with
           | False -> update_status status_false; State_set.empty
           | True ->
               update_status status_true;
               (* The reduction is needed in the True case,
		  because the function is "reduce_by_disjunction".
		  Example: //@ assert x<0 || x>=0; *)
               reduce_by_disjunction ~result state slevel vc
           | Unknown ->
               update_status status_maybe;
               reduce_by_disjunction ~result state slevel vc)
      | Unknown ->
	  (let res = eval_predicate ~result  state vc in
	   Value_parameters.result ~once:true ~current:true
             "%s behavior %s: postcondition got status %s"
	     header b.b_name
	     (string_of_status res);
	   match res with
	     Unknown | False ->
	       update_status status_maybe;
	       Value_parameters.result ~once:true ~current:true
		 "%s behavior %s: postcondition got status %s, but it is unknown if the behavior is active"
		 header b.b_name (string_of_status res);
	       state
	   | True ->
	       update_status status_true;
	       Value_parameters.result ~once:true ~current:true
		 "%s behavior %s: postcondition got status valid"
		 header b.b_name;
	       state)
      | False ->
          (* if assumes is false, post-condition status is not updated *)
          Value_parameters.result ~once:true ~current:true
            "%s behavior %s: assumption got status invalid; \
                  post-condition not evaluated"
            header b.b_name;
          state
  in
  List.fold_left
    incorporate_behavior
    state behaviors

let check_fct_postconditions ~result kf init_state state kind =
  try
    let spec = (Kernel_function.get_spec kf).spec_behavior in
    let slevel = get_slevel kf in
    check_postconditions ~result ~slevel
      (Pretty_utils.sfprintf "Function %a,@?" Kernel_function.pretty_name kf)
      init_state state kind spec
  with Not_found -> state

let check_preconditions ~slevel header state requires =
  match requires with
    | [] -> state
    | _ ->
        let vc = Logic_const.pands
          (List.map Logic_const.pred_of_id_pred requires)
        in
        let res = eval_predicate ~result:None state vc in
	(match res with
	   | True ->
	       List.iter (fun p -> Status.join_predicate p (status_true))
		 requires
	   | False ->
	       List.iter (fun p -> Status.join_predicate p (status_false))
		 requires
	   | Unknown ->
	       List.iter (fun p -> Status.join_predicate p (status_maybe))
		 requires);
        Value_parameters.result ~current:true ~once:true
          "Precondition of %s got status %s."
          header (string_of_status res) ;
        reduce_by_disjunction ~result:None state slevel vc

let check_fct_preconditions kf state =
  try
    let spec = (Kernel_function.get_spec kf).spec_requires in
    let slevel = get_slevel kf in
    check_preconditions ~slevel
      (Pretty_utils.sfprintf "%a@?" Kernel_function.pretty_name kf)
      (State_set.singleton state) spec
  with Not_found -> (State_set.singleton state)

let extract_valid_behaviors state behavior =
  List.filter
    (fun b ->
       let assumes = Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes) in
       match eval_predicate ~result:None state assumes with
       | True | Unknown -> true
       | False -> false)
    behavior.spec_behavior

(* state before entering the given function *)
let valid_behaviors kf state =
  extract_valid_behaviors
    (State_set.singleton state)
    (Kernel_function.get_spec kf)

let () = Db.Value.valid_behaviors := valid_behaviors


(* SEE eval_lval and do_assign to be consistent.
   Same match cases must exist in order to be precise.
   May raise [Lmap.Cannot_copy].
*)
let copy_offsetmap_from_virtual ~with_alarms
    loc1 lv2 loc2 (state:Relations_type.Model.t) =
  if (not (Int_Base.equal loc1.size loc2.size))
    || (try
          ignore
	    (Location_Bits.cardinal_less_than loc2.loc
	       (Value_parameters.ArrayPrecisionLevel.get ()));
          false
        with Not_less_than -> true)
  then begin
    raise Lmap.Cannot_copy
  end;
  let target_offset = snd lv2 in
(*  let target_size = sizeof_lval lv2 in
  let target_size = Int_Base.project target_size in*)
  let target_size =
    try Int_Base.project loc2.size
    with Int_Base.Error_Top -> raise Lmap.Cannot_copy
  in
  let result_relations =
    match fst lv2 with
    | Mem({enode = Lval slv} as e) when UseRelations.get () ->
	let sub_left_loc = lval_to_loc ~with_alarms state slv in
        begin try
	  let _, _, target_offset =
	    try (*TODO: with_alarms:false should be used ? *)
              eval_offset ~reduce_valid_index:(Parameters.SafeArrays.get ())
		~with_alarms None (typeOf e) state target_offset
            with Offset_not_based_on_Null _ -> raise Lmap.Cannot_copy
	  in
	  let offsetmap =
	    Relations_type.Model.copy_from_virtual
	      sub_left_loc target_offset target_size state
	  in
	  offsetmap
	with Relations_type.Use_Main_Memory ->
	  Cvalue_type.V_Offsetmap.empty
	end
    | Mem({enode = BinOp((PlusPI|IndexPI|MinusPI as op),
                         {enode = Lval slv},e2,_)}  as e)
        when UseRelations.get () ->
        let typ = typeOf e in
        let e2 = eval_expr ~with_alarms state e2 in
	begin try

          let ival = (Cvalue_type.V.find_ival e2) in
          let ival = if op=MinusPI then Ival.neg ival else ival in
          let ival = Ival.scale
	    (Int_Base.project (sizeof_pointed typ))
	    ival
          in
	  let sub_left_loc = lval_to_loc ~with_alarms state slv in
          (*TODO: with_alarms:false should be used ? *)
	  let _, _, target_offset = eval_offset
            ~reduce_valid_index:(Parameters.SafeArrays.get ())
            ~with_alarms None typ state target_offset in
          let target_offset = Ival.add target_offset ival in
	  let offsetmap =
            Relations_type.Model.copy_from_virtual
	      sub_left_loc target_offset target_size state
          in
	  offsetmap
	with Relations_type.Use_Main_Memory | Cvalue_type.V.Not_based_on_null ->
	  Cvalue_type.V_Offsetmap.empty
	end

    | _ ->
	 Cvalue_type.V_Offsetmap.empty
    in
    result_relations

(** May raise [Lmap.Cannot_copy]. *)
let copy_paste_locations ~with_alarms ~exp_lv ~left ~right size_int old_state =
   (* directly copy the old value without trying to recompose it.
      Useful for structs assignment. *)
  let size = Int_Base.inject size_int in
  let right_loc = Locations.make_loc right size in
  let left_loc = Locations.make_loc left size in
  let offsetmap_relations =
    try
      copy_offsetmap_from_virtual
        ~with_alarms
	right_loc exp_lv left_loc old_state
    with Lmap.Cannot_copy ->
      Cvalue_type.V_Offsetmap.empty
  in
  let offsetmap_memory =
    try
      match Relations_type.Model.copy_offsetmap right_loc old_state with
      | Some v -> v
      | None -> raise Lmap.Cannot_copy
    with Lmap.Cannot_copy ->
      Cvalue_type.V_Offsetmap.empty
  in
  let offsetmap =
    Cvalue_type.V_Offsetmap.over_intersection
      offsetmap_relations
      offsetmap_memory
  in
  if not (Cvalue_type.V_Offsetmap.is_empty offsetmap)
  then begin
    try
      CilE.set_syntactic_context (CilE.SyMem exp_lv);
      Relations_type.Model.paste_offsetmap
	offsetmap left_loc.loc Int.zero size_int old_state
    with Lmap.Cannot_copy as e -> raise e
  end
  else raise Lmap.Cannot_copy

let need_cast t1 t2 =
  match (unrollType t1, unrollType t2) with
    | (TInt _| TEnum _| TPtr _),(TInt _| TEnum _| TPtr _)
    | (TFloat _,TFloat _)
    | (TComp _, TComp _) ->
	(try bitsSizeOf t1 <> bitsSizeOf t2
	 with SizeOfError _ -> true)
    | _ -> true


module Computer
  (AnalysisParam:sig
    val stmt_can_reach : stmt -> stmt -> bool
    val is_natural_loop : stmt -> bool
    val blocks_closed_by_edge: stmt -> stmt -> block list
    val slevel: int
    val initial_state : State_set.t
    end) =

struct

  let name = "Values analysis"

  let stmt_can_reach = AnalysisParam.stmt_can_reach
  let slevel = AnalysisParam.slevel
  let debug = ref false

  type record =
      {
	superposition : State_set.t ;
	widening : int ;
	widening_state : Relations_type.Model.t ;
      }

  let empty_record =
    { superposition = State_set.empty ;
      widening = Value_parameters.WideningLevel.get () ;
      widening_state = Relations_type.Model.bottom }

  let (current_table: record InstrHashtbl.t) =
    InstrHashtbl.create 317

  let find_current kinstr =
    try
      InstrHashtbl.find current_table kinstr
    with Not_found -> empty_record

  let update_current_exn v =
    let kinstr = CilE.current_stmt () in
    assert (kinstr <> Kglobal);
    let old = find_current kinstr in
    let new_superposition =
      State_set.merge_into v old.superposition in
    let new_widening = old.widening in
    InstrHashtbl.replace current_table kinstr
      { widening = new_widening ; superposition = new_superposition ;
	widening_state = old.widening_state }

  let update_current v =
    try
      update_current_exn v
    with State_set.Unchanged -> ()

  let merge_current ~degenerate =
    Value_parameters.debug "merge phase 1";
    let treat_instr k record =
      let current_state = Db.Value.noassert_get_state k in
      let is_top_already =
	Relations_type.Model.equal Relations_type.Model.empty current_state
      in
	if not is_top_already
	then begin
(*	    Value_parameters.debug "merge: %d states"
	      (State_set.length record.superposition); *)
	    let sum = State_set.join_dropping_relations record.superposition in
	    Value_parameters.debug "merge: join done" ;
	    Db.Value.update_table k sum
	  end
    in
    InstrHashtbl.iter treat_instr current_table;
    Value_parameters.debug "merge phase 2";
    if not degenerate &&
      ((not (Db.Value.Record_Value_Callbacks.is_empty ())) ||
         (not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())))
    then begin
      let stack_for_callbacks = for_callbacks_stack () in

      if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
      then begin
	let current_superpositions = 
	  InstrHashtbl.create (InstrHashtbl.length current_table)
	in
	InstrHashtbl.iter
	  (fun k record ->
	     InstrHashtbl.add current_superpositions k record.superposition)
	  current_table;

        Value_parameters.feedback "now calling Record_Value_Superposition callbacks";
	Db.Value.Record_Value_Superposition_Callbacks.apply
	  (stack_for_callbacks, current_superpositions);

      end ;
      if not (Db.Value.Record_Value_Callbacks.is_empty ())
      then begin
        Value_parameters.feedback "now calling Record_Value callbacks";
	let current_states = 
	  InstrHashtbl.create (InstrHashtbl.length current_table)
	in
	InstrHashtbl.iter
	  (fun k record ->
	     InstrHashtbl.add current_states k
	       (State_set.join_dropping_relations record.superposition))
	  current_table;

	Db.Value.Record_Value_Callbacks.apply
	  (stack_for_callbacks, current_states);
      end
    end;
    Value_parameters.debug "merge phase 3";
    InstrHashtbl.clear current_table;
    Value_parameters.debug "merge phase 4"

  type u =
      { counter_unroll : int; (* how many times this state has been crossed *)
	value : State_set.t ref;

 }

  module StmtStartData =
    Dataflow.StmtStartData(struct type t = u let size = 107 end)

  type t = u

  let copy (d: t) = d

  let display_one fmt v =
    State_set.iter (fun value ->
                 if not (Relations_type.Model.is_reachable value) then begin
                   Format.fprintf fmt "Statement (x%d): UNREACHABLE@\n"
                     v.counter_unroll ;
                 end
                 else
                   (Format.fprintf fmt "Statement (x%d)@\n%a"
                      v.counter_unroll
                      Relations_type.Model.pretty
                      value))
      !(v.value)

  let pretty fmt (d: t) = display_one fmt d

  let computeFirstPredecessor (_s: stmt) state =
    {
      counter_unroll = 0;
      value = state.value;}

  let getWidenHints (s: stmt) =
    Widen.getWidenHints (current_kf()) s

  let counter_unroll_target = ref 100

  let combinePredecessors (s: stmt) ~old new_ =
    if (State_set.length !(new_.value)) = 0
    then None
    else begin
	if old.counter_unroll >= slevel
	then
	  let stored_value = find_current (Kstmt s) in
	  let sum =
	    Relations_type.Model.join
              (State_set.join !(new_.value))
	      (State_set.join !(old.value))
	  in
	  if (State_set.exists
		 (fun e -> Relations_type.Model.is_included sum e)
		 stored_value.superposition)
	  then None
	  else begin
	      Some {counter_unroll = old.counter_unroll ;
		    value = ref (State_set.singleton sum);}

	    end
	else begin try
	  let merged = State_set.merge_into !(new_.value) !(old.value) in
	  let new_counter_unroll =
	    old.counter_unroll + (State_set.cardinal !(new_.value)) in
	  if new_counter_unroll >= !counter_unroll_target
	  then begin
	    Value_parameters.result ~once:true
              "Semantic level unrolling superposing up to %d states"
	      !counter_unroll_target;
	    counter_unroll_target := !counter_unroll_target + 100;
	  end;
	  let result =
	    Some
	      { value = ref merged ;
                counter_unroll =
		  old.counter_unroll + (State_set.cardinal !(new_.value)) }
	  in
	  result
	with State_set.Unchanged -> None
	end
    end




  (** Precondition: the type of [exp] and the type [loc_lv] may be different only
      if the cast from [typeOf exp] and [typeOf loc_lv] is a truncation or an extension.
      This function will not perform any conversion (float->int, int->float, ...).
      [exp] should not be bottom for optimization purposes in the caller.
  *)
  let do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp =
    assert (not (Cvalue_type.V.is_bottom exp));
    (* Or one may propagate bottoms uselessly for too long. *)
    let exp = (* truncate the value if the [lv] is too small: this may
                 happen when the [lv] is a bitfield. Otherwise, the
                 cast is explicit thanks to Cil and no truncation is
                 necessary. *)
      try
        (* if it is a bitfield, the size is statically known. *)
        let size = Int_Base.project loc_lv.size in
        try
          let old_ival = V.find_ival exp in
          let exp =
            V.inject_ival (* Inject on null as [find_ival] did not raise
                                       [Not_based_on_null] *)
              (Ival.cast
		 ~size
		 ~signed:(signof_typeof_lval lv)
		 (* the sign can be computed on integral types. *)
		 ~value:old_ival)
	  in
	  exp
        with
        | V.Not_based_on_null (* from [find_ival] *) ->
            (* The exp is a pointer: check there are enough bits in
               the bitfield to contain it. *)
            if Int.compare size (Int.of_int (sizeofpointer ())) >= 0
              || V.is_top exp
            then exp
            else begin
              Value_parameters.result "casting address to a bitfield of %s bits: this is smaller than sizeof(void*)" (Int.to_string size);
	      V.topify_arith_origin exp
	    end
        | Neither_Int_Nor_Enum_Nor_Pointer
            (* from [signof_typeof_lval] *) -> exp
      with
      | Int_Base.Error_Top | Int_Base.Error_Bottom ->
          (* from [project]: size is not known  *)
          exp
    in
    let pretty_org fmt org = if not (Origin.is_top org) then
      Format.fprintf fmt " because of %a" Origin.pretty org
    in
    (match loc_lv.loc with
    | Location_Bits.Top (Location_Bits.Top_Param.Top, orig) ->
        Value_parameters.result
          "State before degeneration:@\n======%a@\n======="
          Relations_type.Model.pretty state;
        Value_parameters.warning ~once:true
          "writing at a completely unknown address@[%a@].@\nAborting."
          pretty_org orig;
        do_degenerate (Some lv)

    | Location_Bits.Top((Location_Bits.Top_Param.Set _) as param,orig) ->
        Value_parameters.result ~current:true ~once:true
          "writing somewhere in @[%a@]@[%a@]."
          Location_Bits.Top_Param.pretty param
          pretty_org orig
    | Location_Bits.Map _ -> (* everything is normal *) ());
    let exact = valid_cardinal_zero_or_one loc_lv in
    let value =
      Relations_type.Model.add_binding ~with_alarms ~exact
	state loc_lv exp
    in
   value

  (** Clobber list for bases containing addresses of local variables. *)
  let bases_containing_locals = ref Location_Bits.Top_Param.bottom
  let remember_bases_with_locals = remember_bases_with_locals bases_containing_locals

  (** Precondition: the type of [exp] and the type [loc_lv] may be different only
      if the cast from [typeOf exp] and [typeOfPointed lv] is a truncation or an extension.
      This function will not perform any conversion (float->int, int->float, ...).
  *)
  let do_assign_abstract_value ~with_alarms
      ~former_state
      (state:Relations_type.Model.t)
      lv
      exp =
    let state_for_lv =
      if true (*!Cabs2cil.forceRLArgEval*) then state
      else former_state
    in
    let loc_lv = lval_to_loc ~with_alarms state_for_lv lv in
    remember_bases_with_locals loc_lv exp;
    CilE.set_syntactic_context (CilE.SyMem lv);
    do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp


  let offsetmap_top_addresses_of_locals is_local =
    let is_local_bytes = Location_Bytes.contains_addresses_of_locals is_local in
    fun offsetmap ->
      if Cvalue_type.V_Offsetmap.is_empty offsetmap
      then offsetmap, true
      else
        let found_locals = ref false in
        let loc_contains_addresses_of_locals t =
	  let l =
            is_local_bytes t.Cvalue_type.V_Or_Uninitialized.v
	  in
	  found_locals := !found_locals
	  || (l
              && (match t.Cvalue_type.V_Or_Uninitialized.v with
                  | Location_Bytes.Top (Location_Bytes.Top_Param.Top,_) -> false
                      (* Do not be too verbose if the value is top. *)
                  | _ -> true));
	  l
        in
        let result =
	  Cvalue_type.V_Offsetmap.top_stuff
            loc_contains_addresses_of_locals
            (fun v ->
               Cvalue_type.V_Or_Uninitialized.unspecify_escaping_locals
                 is_local v)
	    offsetmap
        in
        result, !found_locals

  let state_top_addresses_of_locals ~is_block
      offsetmap_top_addresses_of_locals fundec
      =
    let f k offsm =
      let r,found_locals = offsetmap_top_addresses_of_locals offsm in
      if found_locals then
	warn_locals_escape is_block fundec k;
      r
    in
    (fun (state:Relations_type.Model.t) ->
       (* let's forget relations *)
       let simple_state = Relations_type.Model.value_state state in
       let f base acc =
         try
           let offset_to_clean = Cvalue_type.Model.find_base base simple_state in
           let cleaned_offsetmap = f base offset_to_clean in
           Cvalue_type.Model.add_offsetmap base cleaned_offsetmap acc
         with Not_found -> acc
       in
       try
         Relations_type.Model.inject
           (Location_Bits.Top_Param.fold
              f
              !bases_containing_locals
              (f Base.null simple_state))
       with Location_Bits.Top_Param.Error_Top ->
         begin
           let f k offsm acc =
             let r,found_locals = offsetmap_top_addresses_of_locals offsm in
             if found_locals then
               warn_locals_escape is_block fundec k;
             Cvalue_type.Model.add_offsetmap k r acc
           in
           let result =
             try
               Relations_type.Model.inject
                 (Cvalue_type.Model.fold_base_offsetmap
                    f
                    (Relations_type.Model.value_state state)
                    Cvalue_type.Model.empty)
             with Cvalue_type.Model.Error_Bottom -> Relations_type.Model.bottom
           in
           result

         end)

  let top_addresses_of_locals fundec =
    let entry_point = Globals.entry_point () in
    if snd entry_point (* lib *) ||
      current_kf() != fst entry_point (* not entry point *)
    then
      let offsetmap_top_addresses_of_locals =
        offsetmap_top_addresses_of_locals (swap Base.is_formal_or_local fundec)
      in
      let state_top_addresses_of_locals =
        state_top_addresses_of_locals ~is_block:false
          offsetmap_top_addresses_of_locals fundec
      in
      offsetmap_top_addresses_of_locals, state_top_addresses_of_locals
    else (fun x -> x,false),(fun x -> x)

  let block_top_addresses_of_locals blocks =
   match List.flatten (List.map (fun b -> b.blocals) blocks) with
       [] ->
         fun x -> x (* no need to change the state if there is no local
                           variable
                         *)
       | _ ->
           let offsetmap_top_addresses_of_locals =
             offsetmap_top_addresses_of_locals
               (fun v -> List.exists (Base.is_block_local v) blocks)
           in
           let state_top_addresses_of_locals =
             state_top_addresses_of_locals ~is_block:true
             offsetmap_top_addresses_of_locals
             (Kernel_function.get_definition (current_kf()))
           in state_top_addresses_of_locals

 (* Assigns [exp] to [lv] in [state] *)
  let do_assign ~with_alarms old_state lv exp =
    assert (Relations_type.Model.is_reachable old_state);
    let fresh_flags () =
      let flag = ref false in
      (fun () -> flag := true),
      fun () -> !flag
    in
    let set_alarm, get_alarm = fresh_flags () in
    let logger v =
      if v <> CilE.Aignore
      then CilE.Acall set_alarm
      else CilE.Aignore
    in
    let warn_remember_mode =
      { CilE.imprecision_tracing = logger with_alarms.CilE.imprecision_tracing;
	others = with_alarms.CilE.others;
	unspecified = logger with_alarms.CilE.unspecified}
    in
    let reduced_state, _, evaled_exp =
      eval_expr_with_deps_state_subdiv ~with_alarms:warn_remember_mode None
	old_state
	exp
    in
    Value_parameters.debug ~level:2 "do_assign %a = (%a)(%a)"
      !d_lval lv
      !d_exp exp
      V.pretty evaled_exp;
    let left_loc = lval_to_loc ~with_alarms old_state lv in
    remember_bases_with_locals left_loc evaled_exp;
    let warn_right_exp_imprecision () =
      (match with_alarms.CilE.imprecision_tracing with
       | CilE.Aignore -> ()
       | CilE.Acall f -> f ()
       | CilE.Alog ->
           match evaled_exp with
           | Cvalue_type.V.Top(_topparam,origin) ->
               Value_parameters.result ~once:true ~current:true
                 "assigning imprecise value to @[%a@]@[%t@]@[%a@]"
                 !Ast_printer.d_lval lv
                 (fun fmt -> match lv with
                  | (Mem _, _) ->
		      Format.fprintf fmt " (i.e. %a)" Locations.pretty left_loc
                  | (Var _, _) -> ())
                 (fun fmt org ->
                    if not (Origin.is_top origin) then
                      Format.fprintf fmt ".@ The imprecision originates from %a"
			Origin.pretty org)
                 origin
           | Cvalue_type.V.Map _ ->
               if not (Got_Imprecise_Value.get ()) &&
	         not (Cvalue_type.V.cardinal_zero_or_one evaled_exp)
	       then begin
                 Got_Imprecise_Value.set true;
                 Value_parameters.result ~current:true
		   "assigning non deterministic value for the first time";
               end)
    in
    let reduced_state =
      match lv with
	Mem mem_e,NoOffset ->
	  let new_reduced_state =
	    reduce_by_valid_expr ~with_alarms ~positive:true mem_e reduced_state
	  in
	  if not (Relations_type.Model.is_reachable new_reduced_state)
	  then begin
	    CilE.set_syntactic_context (CilE.SyMem lv);
	    CilE.warn_mem_write with_alarms ;
	    Value_parameters.result ~current:true
	      "all target addresses were invalid. This path is assumed to be dead.";
	  end;
	  new_reduced_state
            (*      | Var _ , Index _ -> assert false
                    TODO: do something for "TAB[i] = expr"
            *)
      | _ -> reduced_state
    in
    if Location_Bits.equal left_loc.loc Location_Bits.bottom  ||
      not (Relations_type.Model.is_reachable reduced_state)
    then Relations_type.Model.bottom
    else
      let default () =
        warn_right_exp_imprecision ();
	if get_alarm() then
	  (* log alarms that have not been logged the first time *)
          ignore
	    (eval_expr
	       ~with_alarms:
	       {CilE.imprecision_tracing=with_alarms.CilE.imprecision_tracing;
                others=CilE.Aignore;
                unspecified=with_alarms.CilE.unspecified}
	       old_state
	       exp);

	CilE.set_syntactic_context (CilE.SyMem lv);
        if Cvalue_type.V.is_bottom evaled_exp
	then Relations_type.Model.bottom
        else
          do_assign_abstract_value_to_loc ~with_alarms
            reduced_state
            lv
            left_loc
            evaled_exp
      in
      let default_lval exp_lv =
        (* directly copy the old value without trying to recompose it.
	   Useful for structs assignment. *)
        let right_loc = lval_to_loc ~with_alarms old_state exp_lv in

        CilE.set_syntactic_context (CilE.SyMem exp_lv);
        let full_val =
          Relations_type.Model.find_unspecified
            ~with_alarms:CilE.warn_none_mode
            old_state
            right_loc
        in
        if V_Or_Uninitialized.equal full_val V_Or_Uninitialized.bottom
        then raise Lmap.Cannot_copy
        else begin
          match right_loc.size, left_loc.size with
	  | Int_Base.Value size, Int_Base.Value other_size
	      when Int.equal other_size size ->
              let offsetmap_relations =
	        try
		  copy_offsetmap_from_virtual
                    ~with_alarms
		    right_loc exp_lv left_loc old_state
	        with Lmap.Cannot_copy ->
		  Cvalue_type.V_Offsetmap.empty
	      in
	      let offsetmap_memory =
	        try
		  match Relations_type.Model.copy_offsetmap right_loc old_state with
                  | Some v -> v
                  | None -> raise Lmap.Cannot_copy (* invalid copy paste *)
	        with Lmap.Cannot_copy ->
		  Cvalue_type.V_Offsetmap.empty
	      in
	      let offsetmap =
	        Cvalue_type.V_Offsetmap.over_intersection
		  offsetmap_relations
		  offsetmap_memory
	      in
	      if not (Cvalue_type.V_Offsetmap.is_empty offsetmap)
	      then begin
		CilE.set_syntactic_context (CilE.SyMem lv);
		let copy_paste_succeeded =
                  Relations_type.Model.paste_offsetmap
		    offsetmap left_loc.loc Int.zero size reduced_state
                in
                (* Shall we warn about imprecise contents just copied? *)
                let module L = struct exception Got_imprecise end in
                (try
                   Cvalue_type.V_Offsetmap.iter_contents
                     (fun v ->
                        match v.Cvalue_type.V_Or_Uninitialized.v with
                        | Location_Bytes.Map _ -> ()
                        | _ -> raise L.Got_imprecise)
                     offsetmap
                     size
                 with L.Got_imprecise ->
                   warn_right_exp_imprecision ());
                copy_paste_succeeded
	      end
	      else raise Lmap.Cannot_copy
	  | _ -> raise Lmap.Cannot_copy
        end
      in
      let new_main_memory_state =
        try
          (* An lval assignement might be hidden by a dummy cast *)
          let lv = find_lv ~with_alarms old_state exp in
          default_lval lv
        with | Cannot_find_lv | Lmap.Cannot_copy
            (* from Relations_type.Model.paste_offsetmap or directly default_lval *) ->
              default ()
      in (* The main memory state is now computed. *)
      (* Let's try to improve it with relations *)
      if UseRelations.get ()
	&& Relations_type.Model.is_reachable new_main_memory_state
      then begin
	if (* hasAttribute "volatile" (typeAttrs (typeOf exp))
	      || hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lv)) doesn't work *)
	  false
	then begin
          Relations_type.Model.propagate_change_from_real_to_virt
	    []
	    left_loc
	    new_main_memory_state
	    evaled_exp
	end
	else
          let list_lv = find_lv_plus ~with_alarms old_state exp in
          try
            let (_lv_right,offs_right) =
	      List.find
		(fun (lv_right,_offs_right) ->
                   Location_Bits.equal left_loc.loc
                     (lval_to_loc ~with_alarms:CilE.warn_none_mode old_state lv_right).loc)
		list_lv (* Check for a self assignement *)
            in
            Relations_type.Model.shift_location
	      new_main_memory_state
	      left_loc
	      offs_right
	      evaled_exp
          with Not_found -> (* not a self assignement *)
            let protected_clusters,optimized_state_value =
	      match lv with
	      | Mem({enode = Lval slv} as e),offs ->
                  let sub_left_loc =
		    lval_to_loc ~with_alarms old_state slv
		  in
                  if Location_Bits.cardinal_zero_or_one sub_left_loc.loc then
                    Relations_type.Model.add_mem
		      sub_left_loc
		      (sizeof_lval lv)
		      (try
			 let _,_,offset =
			   eval_offset
                             ~reduce_valid_index:(Parameters.SafeArrays.get ())
                             ~with_alarms
			     None (typeOf e) old_state offs
			 in
			 offset
		       with Offset_not_based_on_Null _ -> Ival.top)
		      new_main_memory_state
		      evaled_exp
                  else [],new_main_memory_state

	      | Mem({enode = BinOp((PlusPI|IndexPI|MinusPI as op),
                                   { enode = Lval slv},e2,_)} as e),
		  offs ->
                  let typ = typeOf e in
                  let e2 = eval_expr ~with_alarms old_state e2 in
                  begin try
		    let ival = Cvalue_type.V.find_ival e2 in
		    let ival = if op = MinusPI then Ival.neg ival else ival
		    in
		    let _, _, offs =
		      eval_offset
                        ~reduce_valid_index:(Parameters.SafeArrays.get ())
                        ~with_alarms
			None typ old_state offs
		    in
		    let offs = (* convert to bits *)
		      Ival.add
                        (Ival.scale
			   (Int_Base.project (sizeof_pointed typ))
			   ival)
                        offs
		    in
		    let sub_left_loc =
		      lval_to_loc ~with_alarms old_state slv
		    in
		    if Location_Bits.cardinal_zero_or_one sub_left_loc.loc
		    then
                      Relations_type.Model.add_mem
                        sub_left_loc
                        (sizeof_lval lv)
                        offs
                        new_main_memory_state
                        evaled_exp
		    else [],new_main_memory_state
                  with
                  | Offset_not_based_on_Null _
                  | Int_Base.Error_Top
                  | Cvalue_type.V.Not_based_on_null -> [],new_main_memory_state
                  end
	      | _ -> [],new_main_memory_state
            in
            (* Let's clean the obsoleted relations. *)
            let optimized_state_value =
	      Relations_type.Model.propagate_change_from_real_to_virt
		protected_clusters
		left_loc
		optimized_state_value
		evaled_exp
            in
            let rec optimize_list_lv l =
	      match l with
	        [] -> optimized_state_value
	      | (lvr,offset) :: tail ->
	          if Ival.is_singleton_int offset
	          then begin
		    let locr = lval_to_loc ~with_alarms old_state lvr in
		    if Location_Bits.cardinal_zero_or_one locr.loc
		    then
		      Relations_type.Model.add_equality
			?offset:(Some (Ival.neg offset))
		        optimized_state_value left_loc locr
		    else optimize_list_lv tail
		  end
	          else optimize_list_lv tail
            in
            optimize_list_lv list_lv
      end
      else new_main_memory_state



  let do_assign ~with_alarms old_state lv exp =
    if true then do_assign ~with_alarms old_state lv exp
    else
      let vars =
	get_influential_vars ~with_alarms:CilE.warn_none_mode old_state exp
      in
      let rec try_sub vars =
	match vars with
	  [] | [ _ ] ->
	    do_assign ~with_alarms old_state lv exp
	| v :: tail ->
	    try
	      if not (List.exists (fun x -> Locations.loc_equal v x) tail)
	      then raise Too_linear;
	      let value =
		Relations_type.Model.find
		  ~with_alarms:CilE.warn_none_mode
		  old_state
		  v
	      in

	      if Locations.Location_Bytes.is_included value Locations.Location_Bytes.top_float
	      then raise Too_linear;

	      ignore (Cvalue_type.V.splitting_cardinal_less_than
			 ~split_non_enumerable:42 value 142);
	      Value_parameters.debug "subdiv assignment: candidate %a value %a@."
		Locations.pretty v
		Cvalue_type.V.pretty value;
	      let treat_subdiv subvalue acc =
		let sub_oldstate =
		  (* FIXME: should be relation-aware primitive *)
		  Relations_type.Model.add_binding
		    ~with_alarms:CilE.warn_none_mode
		    ~exact:true
		    old_state
		    v
		    subvalue
		in
		let sub_newstate =
		  do_assign ~with_alarms sub_oldstate lv exp
		in
		Relations_type.Model.join acc sub_newstate
	      in
	      Location_Bytes.fold_enum
		~split_non_enumerable:42
		treat_subdiv
		value
		Relations_type.Model.bottom
	    with
	      Not_less_than | Too_linear ->
		try_sub tail
	    | Location_Bytes.Error_Top ->
		assert false;
      in
      try_sub vars

  exception Got_bottom

  let reachables d =
    (not (State_set.is_empty !(d.value))),!(d.value)

  let empty_interpretation_result =
    None, Relations_type.Model.bottom, Location_Bits.Top_Param.bottom

  let interp_call stmt lval_to_assign funcexp argl d_value =
    let call_site_loc = CurrentLoc.get () in
    let with_alarms = warn_all_quiet_mode () in
    let treat_one_state state acc =
      let new_state_after_call =
	try
	    let _, functions =
              resolv_func_vinfo ~with_alarms
		None state funcexp
            in
	    let actuals =
	      List.map
		(fun e ->
                  let v =
		    eval_expr ~with_alarms state e
		  in
		  if V.equal v V.bottom
		  then begin
                    Value_parameters.result ~current:true
		      "Non-termination in evaluation of function call argument";
		      raise Got_bottom
		    end;
		  (e,v))
		argl
            in
	    let treat_one_call f (acc_rt,acc_res,acc_clobbered_set) =
              let caller =
                match !call_stack with
                | [] -> assert false
                | {called_kf=ckf }::_ -> ckf,stmt
              in
	      Kf_state.add_caller f ~caller;
	      let return, result, clobbered_set =
		!compute_call_ref
		  f
		  ~call_kinstr:(Kstmt stmt)
                  state
                  actuals
	      in
              CurrentLoc.set call_site_loc;
	      (match acc_rt,return with
	      | None,_ -> return
	      | Some _, None -> acc_rt
	      | Some acc_rt, Some return ->
		  Some (snd (V_Offsetmap.join
				acc_rt
				return))),
	      Relations_type.Model.join acc_res result,
              Location_Bits.Top_Param.join acc_clobbered_set clobbered_set
	    in
	    let return,new_state,clobbered_set =
	      Kernel_function.Set.fold
		treat_one_call
		functions
		empty_interpretation_result
	    in

	    bases_containing_locals :=
	      Location_Bits.Top_Param.join
		!bases_containing_locals
		clobbered_set;

	    match lval_to_assign with
	    | None -> new_state
	    | Some lv ->
		begin match return with
		| Some return ->
		    let loc =
		      lval_to_loc
			~with_alarms new_state lv
		    in
		    let rtype =
		      getReturnType (typeOf funcexp)
		    in
		    let lvtyp = typeOfLval lv in
		    let default () =
		      let {Cvalue_type.V_Or_Uninitialized.v=value;
			   initialized = init;
			   no_escaping_adr = no_esc}
			  =
			V_Offsetmap.find_ival
			  ~validity:Base.All
			  ~with_alarms:CilE.warn_none_mode
			  Ival.zero
			  return
			  (Int.of_int (bitsSizeOf rtype))
			  Cvalue_type.V_Or_Uninitialized.bottom
		      in
		      if not init
		      then CilE.warn_uninitialized with_alarms;
		      if not no_esc
		      then CilE.warn_escapingaddr with_alarms;
		      if Cvalue_type.V.is_bottom value
			&& not (init && no_esc)
		      then
			Value_parameters.result ~current:true
			  "Function call returned an unspecified value. This path is assumed to be dead.";

		      let exact = valid_cardinal_zero_or_one loc in
		      let evaled_exp =
			do_cast
			  ~with_alarms:CilE.warn_none_mode
			  lvtyp
			  value
		      in
		      remember_bases_with_locals loc evaled_exp;
		      Relations_type.Model.add_binding
			~with_alarms:CilE.warn_none_mode
			~exact
			new_state
			loc
			evaled_exp
		    in
		    if need_cast lvtyp rtype
		    then
		      default ()
		    else
		      (try
			  let result =
			    Relations_type.Model.paste_offsetmap
			      return
			      loc.loc
			      Int.zero
			      (Int_Base.project loc.size)
			      new_state
			  in
			  let evaled_exp=
			    (V_Offsetmap.find_ival
				~validity:Base.All
				~with_alarms:CilE.warn_none_mode
				Ival.zero
				return
				(Int.of_int (bitsSizeOf rtype))
				Cvalue_type.V_Or_Uninitialized.bottom)
			      .Cvalue_type.V_Or_Uninitialized.v
			  in
			  remember_bases_with_locals loc evaled_exp;
			  result
			with Lmap.Cannot_copy -> default ())
		| None ->
		    (if Relations_type.Model.is_reachable new_state
		      then
			Value_parameters.warning ~current:true
			  "In function %t: called function returns void but returned value is assigned; ignoring assignment"
			  pretty_current_cfunction_name;
		     new_state)
		end
	  with
	  | Ignore ->
	      CurrentLoc.set call_site_loc;
	      state
	  | Got_bottom ->
	      CurrentLoc.set call_site_loc;
	      Relations_type.Model.bottom
	  | Leaf ->
	      CurrentLoc.set call_site_loc;
	      (match lval_to_assign with
	      | None ->  state
	      | Some lv ->
		  let evaled_exp = V.top_leaf_origin () in
		  do_assign_abstract_value
		    ~with_alarms
		    ~former_state:state
		    state
		    lv
		    evaled_exp)
      in
      State_set.add new_state_after_call acc
    in
    State_set.fold
      treat_one_state
      d_value
      State_set.empty

  let doInstr stmt (i: instr) (d: t) =
    !Db.progress ();
    CilE.start_stmt (Kstmt stmt);
    let reachable,reachables = reachables d in
    let result =
      if (not reachable) then
        Dataflow.Done d
      else begin
	  let apply_each_state f =
	    Dataflow.Done
              {
                counter_unroll = 0;
		value =
                  ref
		    (State_set.fold
                        (fun state_value acc ->
                          State_set.add
			    (f state_value)
			    acc)
                        reachables
                        State_set.empty) }
	  in
          (* update current statement *)
          match i with
          | Set (lv,exp,_loc) ->
	      apply_each_state
		(fun state_value ->
		  do_assign
		    ~with_alarms:(warn_all_quiet_mode ())
		    state_value
		    lv
		    exp)
          | Call (None,
                 {enode = Lval (Var {vname="__builtin_va_start"},NoOffset)},
                 [{enode = Lval lv}],_loc) ->
              apply_each_state
		(fun state_value ->
		  do_assign_abstract_value
		    ~with_alarms:(warn_all_quiet_mode ())
		    ~former_state:state_value
		    state_value
		    lv
		    Cvalue_type.V.top_int)
          | Call (lval_to_assign,funcexp,argl,_loc) ->
              Dataflow.Done
		{
                  counter_unroll = 0;
                  value =
		    ref (interp_call stmt lval_to_assign funcexp argl reachables)
		}
          | Asm _ ->
	      Value_parameters.warning ~once:true ~current:true
		"assuming assembly code has no effects in function %t"
		pretty_current_cfunction_name;
              Dataflow.Default
          | Skip _ ->
	      Dataflow.Default
          | Code_annot (_,_) -> (* processed in dostmt from Db *)
	      Dataflow.Default
	end
    in
    CilE.end_stmt ();
    result

  let interp_annot state ca =
    match ca.annot_content with
    | AAssert (behav,p,_) ->
	let in_behavior =
	  match behav with
	    [] -> True
	  | [ behav ] ->
	      let initial_state_single =
		State_set.join AnalysisParam.initial_state in
	      let _valid_behaviors =
		valid_behaviors
		  (current_kf())
		  initial_state_single
	      in
	      if List.exists (fun b -> b.b_name = behav) _valid_behaviors
	      then Unknown
	      else False
	  | _ -> Unknown
	in
	if in_behavior = False
	then state
	else
          let result = eval_predicate ~result:None state p in
	  let message, result =
	    (match result, in_behavior with
            | Unknown, _ | False, Unknown ->
		Status.join ca (status_maybe);
		"unknown", state
            | True, _ ->
		Status.join ca (status_true);
		"valid", state
            | False, True ->
		Status.join ca (status_false);
		"invalid (stopping propagation).", State_set.empty
	    | _, False -> assert false)
	  in
	  let result =
	    if in_behavior = True
	    then
	      reduce_by_disjunction ~result:None
	        result
	        AnalysisParam.slevel
	        p
	    else result
	  in
          Value_parameters.result ~once:true ~current:true
	    "Assertion got status %s." message;
	  result
    | APragma _
    | AInvariant _ (*TODO*)
    | AVariant _ | AAssigns _
    | AStmtSpec _ (*TODO*) -> state

  let check_non_overlapping state lvs1 lvs2 =
    List.iter
      (fun lv1 ->
         List.iter
           (fun lv2 ->
              let zone1 =
                Locations.valid_enumerate_bits
                  (lval_to_loc ~with_alarms:CilE.warn_none_mode state lv1)
              in
              let zone2 =
                Locations.valid_enumerate_bits
                  (lval_to_loc ~with_alarms:CilE.warn_none_mode state lv2)
              in
              if Locations.Zone.intersects zone1 zone2
	      then begin
                CilE.set_syntactic_context
                  (CilE.SySep (Cil.mkAddrOf lv1, Cil.mkAddrOf lv2));
                CilE.warn_separated CilE.warn_all_mode
              end)
	   lvs2)
      lvs1

  let check_unspecified_sequence state seq =
    let rec check_one_stmt ((stmt1,_,writes1,_) as my_stmt) = function
        [] -> ()
      | (stmt2,_,_,_)::seq when stmt1 == stmt2 -> check_one_stmt my_stmt seq
      | (stmt2,modified2,writes2,reads2) :: seq ->
          let unauthorized_reads =
            (* TODO: try to have a more semantical interpretation of modified *)
            List.filter
              (fun x ->
                 (List.for_all
		     (fun y -> LvalComparable.compare x y <> 0)
                     modified2))
	      writes1
          in
          check_non_overlapping state unauthorized_reads reads2;
          if stmt1.sid < stmt2.sid then
            check_non_overlapping state writes1 writes2;
          check_one_stmt my_stmt seq
    in
    if Parameters.UnspecifiedAccess.get () then
      List.iter (fun x -> check_one_stmt x seq) seq

  let doStmt (s: stmt) (d: t) =
    let reachable, _ = reachables d in
    let kinstr = Kstmt s in
    CilE.start_stmt kinstr;
    let changed =
      try
        update_current_exn !(d.value);
	true
      with State_set.Unchanged -> false
    in
    CilE.end_stmt ();

    let annots_before, contract =
      Annotations.single_fold_stmt
        (fun a (before, spec as acc) -> match a with
         | Before
	     (User { annot_content = AStmtSpec spec' }
	     | AI (_,{annot_content = AStmtSpec spec' }) )
           ->
             let spec = match spec with
               | None -> spec'
               | Some s -> Logic_utils.merge_funspec s spec'; s
             in
             (before, Some spec)
         | Before (AI (_, b) | User b) -> b :: before, spec
         | After _ -> acc)
	s
        ([], None)
    in
    CilE.start_stmt kinstr;
    List.iter
      (fun annot ->
         d.value := interp_annot !(d.value) annot)
      annots_before;
    Extlib.may
      (fun spec ->
         d.value:=check_preconditions ~slevel "statement" !(d.value) spec.spec_requires)
      contract;
    CilE.end_stmt ();

    let states = !(d.value) in
    d.value := State_set.empty;

    if (not reachable) || (not changed) then
      Dataflow.SDefault
    else begin
	let current = find_current kinstr in
	let d =
	  if d.counter_unroll >= AnalysisParam.slevel
	  then begin
	      let state = State_set.join states in
	      let joined =
		Relations_type.Model.join
		  current.widening_state
		  state
	      in
	      let r =
		if (AnalysisParam.is_natural_loop s) &&
		  (current.widening = 0)
		then
		  let wh_key_set, wh_hints = getWidenHints s in
		  let widen_hints =
                    true, wh_key_set(* no longer used thanks to 0/1 widening*),
                    wh_hints
		  in
		  let _,result = Relations_type.Model.widen
		    widen_hints
		    current.widening_state
		    joined
                  in
                  result
		else
		  joined
	      in
	      let new_widening =
		if current.widening = 0 then 1 else pred current.widening
	      in
	      InstrHashtbl.replace current_table kinstr
		{ current with widening = new_widening ; widening_state = r };

              {
		counter_unroll = d.counter_unroll;
		value = ref (State_set.singleton r);
	      }

	  end
	  else { d with value = ref states }
	in
        CilE.start_stmt kinstr;
        update_current !(d.value);
        CilE.end_stmt ();
	match s.skind with
        | Return _ ->
            Dataflow.SUse d
        | Loop _ ->
            if d.counter_unroll >= AnalysisParam.slevel
	    then
              Value_parameters.result ~once:true ~current:true
                "entering loop for the first time";
            Dataflow.SUse d
        | UnspecifiedSequence seq ->
            CilE.start_stmt kinstr;
            State_set.iter
              (fun state -> check_unspecified_sequence state seq) states;
            CilE.end_stmt ();
            Dataflow.SUse d
        | _ -> Dataflow.SUse d
    end

  let doEdge s succ d =
    let kinstr = Kstmt s in
    (* Check if there are some after-annotations to verify *)
    let annots_after, specs =
      Annotations.single_fold_stmt
        (fun annot (annot_after,spec as acc) ->
           match annot with
             | Before
		 (User { annot_content = AStmtSpec spec' }
		 | AI (_,{annot_content = AStmtSpec spec' }) )
               ->
                 let spec = match spec with
                   | None -> spec'
                   | Some s -> Logic_utils.merge_funspec s spec'; s
                 in
                 (annot_after, Some spec)
             | After
		 (User { annot_content = AStmtSpec _spec' }
		 | AI (_,{annot_content = AStmtSpec _spec' }) ) ->
                 CilE.warn_once
                   "Ignoring statement contract rooted after statement";
                   acc
             | After (AI (_, a) | User a) -> a :: annot_after, spec
             | Before _ -> acc)
        s
        ([], None)
    in
    CilE.start_stmt kinstr;
    List.iter
      (fun annot ->
         d.value := interp_annot !(d.value) annot)
      annots_after;
    Extlib.may
      (fun spec ->
         let init_state = find_current kinstr in
         d.value :=
           check_postconditions ~result:None
	     ~slevel
             "statement"
	     init_state.superposition
	     !(d.value)
	     Normal
	     spec.spec_behavior)
      specs;
    CilE.end_stmt ();
    match AnalysisParam.blocks_closed_by_edge s succ with
        [] -> d
      | closed_blocks ->
          CilE.start_stmt kinstr;
          let d = copy d in
          d.value :=
            State_set.fold
              (fun state set ->
                 let state =
                   Relations_type.Model.uninitialize_locals closed_blocks state
                 in
                 State_set.add
                   (block_top_addresses_of_locals closed_blocks state) set)
              !(d.value) State_set.empty;
          CilE.end_stmt ();
          d

  let filterStmt _stmt = true

  (* Remove all local variables and formals from table *)
  let externalize return kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (fundec,_loc) ->
	assert
	  (StmtStartData.iter
	      (fun k v ->
                if State_set.is_empty !(v.value)
                then ()
                else (Value_parameters.fatal "sid:%d@\n%a@\n"
                         k
                         State_set.pretty !(v.value)));
	        true);
	let superpos = (find_current return).superposition in
        let init_state =
          find_current (Kstmt (Kernel_function.find_first_stmt kf))
        in
        let superpos =
	  let result =
            match return with
	    | Kstmt {skind = Return (Some ({enode = Lval (Var v,_)}),_)} ->
		Some v
	    | _ -> None
	  in
          check_fct_postconditions ~result
	    kf
	    init_state.superposition
	    superpos
	    Normal
        in
	let state = State_set.join_dropping_relations superpos in
	Value_parameters.feedback "Recording results for %a"
	  Kernel_function.pretty_name kf;
        merge_current ~degenerate:false;
	Value_parameters.debug "Recording results phase 1";
        let ret_val =
          (match return with
	   | Kstmt {skind = Return (Some ({enode = Lval lv}),_)} ->
	       CilE.set_syntactic_context (CilE.SyMem lv);
               let loc_to_read =
		 lval_to_loc ~with_alarms:(warn_all_quiet_mode ()) state lv
	       in
	       let result =
	         try
                   Relations_type.Model.copy_offsetmap loc_to_read state
                  with Lmap.Cannot_copy -> (* because [loc_to_read] is an lval: *)
                   assert false
	       in
               result
	   | Kstmt {skind = Return (None,_)} -> None
	   | _ -> assert false)
	in
	Value_parameters.debug "Recording results phase 2";
        let state =
	  Relations_type.Model.clear_state_from_locals fundec state
        in
        let offsetmap_top_addresses_of_locals, state_top_addresses_of_locals =
          top_addresses_of_locals fundec
        in
	Value_parameters.debug "Recording results phase 3";
        let result =
          (match ret_val with
           | None -> ret_val
           | Some ret_val ->
	       let r,warn = offsetmap_top_addresses_of_locals ret_val
	       in
	       if warn then warn_locals_escape_result fundec;
               Some r),
	  state_top_addresses_of_locals state,
          !bases_containing_locals
	in
	Value_parameters.debug "Recording results phase 4";
        result


  let doGuard stmt exp t =
    if State_set.is_empty !(t.value)
    then Dataflow.GUnreachable
    else begin
	CilE.start_stmt (Kstmt stmt);
	let with_alarms = warn_all_quiet_mode () in
	let new_values =
	  State_set.fold
            (fun state acc ->
              let test =
		eval_expr
		  ~with_alarms
		  state exp
	      in
	      CilE.set_syntactic_context
		(CilE.SyBinOp (Ne, Cil.zero, exp));
	      let _, test =
		check_comparable ~with_alarms V.singleton_zero test
	      in
              let do_it =
		let t1 = unrollType (typeOf exp) in
		if isIntegralType t1 || isPointerType t1
		then V.contains_non_zero test
		else true (* TODO: a float condition is true iff != 0.0 *)
	      in
              if do_it then
		try
		  State_set.add
		    (eval_cond ~with_alarms:CilE.warn_none_mode
			state {positive = true; exp = exp})
		    acc
		with Reduce_to_bottom -> acc
              else acc)
            !(t.value)
            State_set.empty
	in
	let result =
	  if State_set.is_empty new_values then Dataflow.GUnreachable
	  else Dataflow.GUse {t with value = ref new_values}
	in
	CilE.end_stmt ();
	result

      end
end

let dummy_non_linear_assignment = InstrHashtbl.create 1

module Loc_hashtbl = Hashtbl.Make (Location_Bits)

class do_non_linear_assignments = object(self)
  inherit
    Visitor.generic_frama_c_visitor (Project.current ()) (Cil.inplace_visit ())
    as super
  val mutable current_locs = None
  val mutable assigns_table =
    (InstrHashtbl.create 91 : Location_list.t InstrHashtbl.t)

  method result = assigns_table

  method vstmt s =
    current_locs <- None;
    match s.skind with
      | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_) ->
               ignore (visitCilStmt (self:>cilVisitor) stmt))
            seq;
          SkipChildren (* do not visit the additional lvals *)
      | _ -> super#vstmt s

  method vlval lv =
    match current_locs with
      None -> SkipChildren
    | Some current_locs ->
	begin match lv with
	  Mem _e, _ -> DoChildren
	| Var v, NoOffset ->
	    let loc = Locations.loc_of_varinfo v in
	    ignore (Loc_hashtbl.find current_locs loc.loc);
	    SkipChildren
	| Var _v, (Index _ | Field _) -> DoChildren
	end

(*

    try

    let deps,loc =
      !Value.lval_to_loc_with_deps
        ~with_alarms:CilE.warn_none_mode
	~deps:Zone.bottom
	current_stmt lv
    in
    let bits_loc = valid_enumerate_bits loc in
    self#join deps;
    self#join bits_loc;
    SkipChildren
*)

  method vcode_annot _ = SkipChildren

  method visit_addr lv =
    begin match lv with
      Var v, offset ->
	let offset' = visitCilOffset (self :> cilVisitor) offset in
	let v' = Cil.get_varinfo self#behavior v in
	if offset' == offset && v == v'
	then SkipChildren
	else ChangeTo (Var v', offset')
    | Mem e, offset ->
	let e' = visitCilExpr (self :> cilVisitor) e in
	let offset' = visitCilOffset (self :> cilVisitor) offset in
	if offset' == offset && e == e'
	then SkipChildren
	else ChangeTo (Mem e', offset')
    end;

  method vinst i =
    match i with
    | Set (lv,exp,_) ->
	current_locs <- Some (Loc_hashtbl.create 7);
	begin match lv with
	  Var _, offset ->
	    ignore (self#voffs offset);
	| Mem e, offset ->
	    ignore (self#vexpr e);
	    ignore (self#voffs offset);
	end;
	ignore (self#vexpr exp);
	(* TODO: do some stuff with self#current_stmt *)
	SkipChildren
    | _ -> SkipChildren

  method vexpr exp =
    match exp.enode with
    | AddrOf _lv | StartOf _lv ->
	SkipChildren (* TODO: do better stuff *)
    | _ -> DoChildren

end

let no_pretty _ _ = ()

let compute_non_linear_assignments f =
  let vis = new do_non_linear_assignments in
  ignore (Visitor.visitFramacFunction (vis:>Visitor.frama_c_visitor) f);
  vis#result

let compute_using_cfg kf ~call_kinstr initial_state =
  match kf.fundec with
  | Declaration _ -> assert false
  | Definition (f,_loc) ->
      (*if let (_,_,variadic,_) = splitFunctionTypeVI f.svar in variadic
        then raise Leaf (* Do not visit variadic bodies *)
        else *)
(* PH: RTE_Generated is declined in a number of RTE_..._Generated
   TO CHECK
*)

      Properties_status.RTE_Signed_Generated.set kf true;
      Properties_status.RTE_DivMod_Generated.set kf true;
      Properties_status.RTE_MemAccess_Generated.set kf true;

      begin
	let f_varinfo = f.svar in
        let module Computer =
          Computer
	    (struct
	      let stmt_can_reach = Stmts_graph.stmt_can_reach kf
              let is_natural_loop = Loop.is_natural kf
	      let non_linear_assignments =
		try
		  Non_linear_assignments.find f_varinfo
		with
		  Not_found ->
		    let n = compute_non_linear_assignments f in
		    Non_linear_assignments.add f_varinfo n;
		    n
              let blocks_closed_by_edge =
		(* TODO: if only an alias, then isn't it useless?*)
                Kernel_function.blocks_closed_by_edge
	      let slevel = get_slevel kf
	      let initial_state = initial_state (* for future reference *)
            end)
        in
        let module Compute = Dataflow.ForwardsDataFlow(Computer) in
        List.iter
          (function {called_kf = g} ->
	     if kf == g
	     then begin
               Value_parameters.warning ~current:true ~once:true
	         "ignoring recursive call during value analysis of %a (%a)"
	         Ast_info.pretty_vname f_varinfo
                 pretty_call_stack !call_stack ;
               raise Leaf
             end)
          !call_stack;
        push_call_stack {called_kf = kf;
                         call_site = call_kinstr;
                         called_merge_current = Computer.merge_current};
        match f.sbody.bstmts with
          [] -> assert false
        | start :: _ ->
            let ret_id = Kernel_function.find_return kf in
            (* We start with only the start block *)
            Computer.StmtStartData.add
              start.sid
              (Computer.computeFirstPredecessor
                 start
                 {
                   Computer.counter_unroll = 0;
                   value = ref initial_state});
            begin try
              Compute.compute [start]
            with Db.Value.Aborted as e ->
              (* Computation was aborted: pop the call stack and inform
	         the caller *)
 	      pop_call_stack ();
              raise e
            end;
            let last_ret,last_s,last_clob as last_state =
              try
                let _,state,_ as result =
		  try
		    Computer.externalize (Kstmt ret_id) kf
		  with Not_found -> assert false
	        in
                if Relations_type.Model.is_reachable state
                then begin
                  try
                    if hasAttribute "noreturn" f_varinfo.vattr
                    then
                      Value_parameters.warning ~current:true ~once:true
                        "function %a may terminate but has the noreturn attribute"
                        Kernel_function.pretty_name kf;
                    Kf_state.mark_as_terminates kf
                  with Not_found -> assert false
                end
                else raise Not_found;
                result
              with Not_found -> begin
                (
                  (*     Computer.merge_current (); this may already have been
		         done by externalize, and should not be done twice
		         because the callbacks are done there.

		         TODO: examine the usefulness of this statement
                  (* Save the values computed even
                         if the function does not terminate *) *)

		  Kf_state.mark_as_never_terminates kf);
                None,
                Relations_type.Model.bottom,
                Location_Bits.Top_Param.bottom
              end
            in
            Value_parameters.debug
              "@[RESULT FOR %a <-%a:@\n\\result -> %a@\n%a@\nClobered set:%a@]"
                Kernel_function.pretty_name kf
              pretty_call_stack !call_stack
              (fun fmt v ->
		 match v with
		 | None -> ()
		 | Some v -> V_Offsetmap.pretty fmt v)
	      last_ret
              no_pretty last_s
              Location_Bits.Top_Param.pretty
              last_clob;
	    pop_call_stack ();
            last_state
      end

(** Associates [kernel_function] to a fresh base for the address returned by
    the [kernel_function]. *)
module Leaf_Table =
  Kernel_function.Make_Table
    (Base.Datatype)
    (struct
       let dependencies = [Db.Value.self]
       let size = 7
       let name = "Leaf_Table"
     end)

let return_value return_type kf state =
  (* Process return of function *)
  let return_type = unrollType return_type in
  match return_type with
  | TComp _ when is_fully_arithmetic return_type ->
      Cvalue_type.V.top_int, state
  | TPtr(typ,_) | (TComp _ as typ) -> begin
      let new_base =
	Leaf_Table.memo
	  (fun kf ->
             let new_varinfo =
               makeGlobalVar
                 ~logic:true
                 (Cabs2cil.fresh_global
                    ("alloced_return_" ^ Kernel_function.get_name kf))
                 typ
             in
             let new_offsetmap =
	       Cvalue_type.V_Offsetmap.sized_zero (memory_size ())
	     in
             Cvalue_type.Default_offsetmap.create_initialized_var
               new_varinfo
               (Base.Known (Int.zero, max_bit_address ()))
               new_offsetmap)
	  kf
      in
      let initial_value =
        if isIntegralType typ
        then Cvalue_type.V.top_int
        else if isFloatingType typ
        then Cvalue_type.V.top_float
        else
          Cvalue_type.V.inject_top_origin
            (Origin.Leaf (LocationSetLattice.currentloc_singleton()))
            (Cvalue_type.V.Top_Param.O.singleton new_base)
            (*top_leaf_origin ()*)
      in
      let modu = try
        if isVoidType typ then Int.one else Int_Base.project (osizeof typ)
      with Int_Base.Error_Top ->
        assert (Cvalue_type.V.is_isotropic initial_value);
        Int.one
      in
      let returned_loc =
        try
          Location_Bytes.inject
            new_base
            (Ival.filter_ge_int (Some Int.zero)
               (Ival.create_all_values
		   ~signed:true
                   ~modu
                   ~size:(sizeofpointer ())))
        with Int_Base.Error_Top ->
          Location_Bytes.inject
            new_base
            Ival.top
      in
      let state =
        Relations_type.Model.create_initial
          ~base:new_base
          ~v:initial_value ~modu:(Int.mul Int.eight modu) ~state
      in
      returned_loc, state
    end
  | TInt _ | TEnum _ ->  Cvalue_type.V.top_int, state
  | TFloat _ ->  Cvalue_type.V.top_float, state
  | TBuiltin_va_list _ ->
      Cvalue_type.V.top_leaf_origin()
	(* Only some builtins may return this type *),
      state
  | TVoid _ -> Cvalue_type.V.top (* this value will never be used *), state
  | TFun _ | TNamed _ | TArray _ -> assert false

exception Deref_lvals of Cil_types.lval list

let compute_using_prototype kf  ~state_with_formals =
  match kf.fundec with
  | Definition (_,_) -> assert false
  | Declaration (_,vi,_,_) when Cil.hasAttribute "noreturn" vi.vattr ->
      None, Relations_type.Model.bottom, Location_Bits.Top_Param.bottom
  | Declaration (_spec,varinfo,_,_) ->
      let return_type,_formals_type,_inline,_attr =
	splitFunctionType (Kernel_function.get_type kf)
      in
      let behaviors = valid_behaviors kf state_with_formals in
      let assigns = Ast_info.merge_assigns behaviors in
      let returned_value, state_with_formals =
	return_value return_type kf state_with_formals
      in
      let returned_value = ref returned_value in
      let clobbered_set = ref Location_Bits.Top_Param.bottom in
      let state =
	match assigns with
        | [] -> state_with_formals
        | assigns -> (*VP: same behavior as before, but it is weird:
                       \from \nothing has the
                       same meaning as unspecified \from...
		     *)
	    let treat_assign acc (out, ins) =
	      let input_contents =
                try
		  List.fold_left
		    (fun acc term ->
		       let input_loc =
			 !Db.Properties.Interp.identified_term_zone_to_loc
			   ~result:None
			   state_with_formals
			   term
		       in
		       let r =
			 Relations_type.Model.find
			   ~with_alarms:CilE.warn_none_mode
			   state_with_formals
			   input_loc
		       in
	(*		 Format.printf "loc %a r %a@."
			   Locations.pretty input_loc
			   Cvalue_type.V.pretty r; *)
			 Cvalue_type.V.join acc r)
		    Cvalue_type.V.top_int
		    ins
                with Invalid_argument "not an lvalue" ->
		  Value_parameters.result
		    ~once:true ~current:true
		    "cannot interpret assigns in function %a"
		    Kernel_function.pretty_name kf;
		  Cvalue_type.V.top
	      in
	      let treat_output_loc loc acc =
		remember_bases_with_locals
		  clobbered_set
		  loc
		  input_contents;
		let bound =
		  Relations_type.Model.add_binding
                    ~with_alarms:CilE.warn_none_mode
		    ~exact:false acc loc input_contents
		in
		Relations_type.Model.join bound acc
	      in
	      try
		let lvals_out =
		  try
		    match out with
		    | Location out ->
                        !Db.Properties.Interp.loc_to_lval
			  ~result:None
			  out.it_content
		    | Nothing -> []
		  with
		    Invalid_argument "not an lvalue" as e ->
		      begin match out with
			Location {it_content={term_node=
			    TLval (TMem {term_node=TBinOp((IndexPI|PlusPI) ,
							 t1,_o1)},
				  _o2)}} ->
			  let deref_lvals =
			    !Db.Properties.Interp.loc_to_lval ~result:None t1
			  in
(*			  Format.printf "input: %a@."
			    Cvalue_type.V.pretty input_contents ; *)
			  raise (Deref_lvals deref_lvals)
		      | _ -> raise e
		      end
		in
                List.fold_left
                  (fun acc lval ->
		    let loc =
		      lval_to_loc  ~with_alarms:CilE.warn_none_mode
			state_with_formals lval
		    in
		    treat_output_loc loc acc
		  )
                  acc
                  lvals_out
	      with
		Invalid_argument "not an lvalue" ->
		  (match out with
		    Location out when
                        Logic_utils.is_result out.it_content ->
		          returned_value :=
			    Cvalue_type.V.join
			      (Cvalue_type.V.topify_arith_origin
				  input_contents)
			      !returned_value;
		          acc
                  | Location _ ->
                      Value_parameters.warning ~once:true ~current:true
                        "Can not interpret assigns in function %a; \
                            effects will be ignored"
                        Kernel_function.pretty_name kf; acc
                  | Nothing -> assert false (* should not raise a failure
                                               when converting to cloc *))
	      | Deref_lvals deref_lvals ->
		  let deref_loc =
		    List.fold_left
		      (fun acc lv ->
			Location_Bits.join
			  (lval_to_loc ~with_alarms:CilE.warn_none_mode
			      state_with_formals lv).loc
			  acc)
		      Location_Bits.bottom
		      deref_lvals
		  in
		  let deref_loc = Location_Bits.topify_arith_origin deref_loc
		  in
		  let loc_bytes =
		    Relations_type.Model.find ~with_alarms:CilE.warn_none_mode
		      state_with_formals
		      (make_loc deref_loc Int_Base.top)
		  in
		  let loc =
		    make_loc (loc_bytes_to_loc_bits loc_bytes) Int_Base.top
		  in
		  treat_output_loc loc acc
	    in
	    (List.fold_left treat_assign state_with_formals assigns)
      in
      let retres_vi, state =
	if isVoidType return_type
	then None, state
	else
	  let offsetmap =
	    V_Offsetmap.update_ival
	      ~with_alarms:CilE.warn_none_mode
	      ~validity:Base.All
	      ~offsets:Ival.zero
	      ~exact:true
	      ~size:(Int.of_int (bitsSizeOf return_type))
	      V_Offsetmap.empty
	      (Cvalue_type.V_Or_Uninitialized.initialized !returned_value)
	  in
	  Library_functions.add_retres_to_state
	    varinfo
	    offsetmap
	    state
      in
      retres_vi, state, !clobbered_set


(* Replace in [initial_state] all keys in [mem_outs] by their value in
  [mem_final_state]. *)
let compute_using_mem
    _kf
    (initial_state:Relations_type.Model.t)
    (new_return_v,mem_final_state)
    mem_outs
    instanciation =
  let (a,clobbered_bases) =
     Relations_type.Model.compute_actual_final_from_generic
       initial_state
       mem_final_state
       mem_outs
       instanciation
  in
  (*TODO: new_return_v MUST be substituted! *)
  new_return_v,a,clobbered_bases


(** Compute only once the initial values for globals and NULL *)
let initial_state_contextfree_only_globals =
  let module S =
    Computation.OptionRef
      (Relations_type.Model.Datatype)
      (struct
	 let name = "contextfree_only_globals"
	 let dependencies =
	   [ Ast.self; Parameters.LibEntry.self; Parameters.MainFunction.self ]
       end)
  in
  function () ->
    let compute ()  =
      let computed_state = ref (initial_state_only_globals ()) in
      Globals.Vars.iter
	(fun varinfo _init ->
           CurrentLoc.set varinfo.vdecl;
	   computed_state :=
	     initialize_var_using_type
	       varinfo
	       !computed_state);
      !computed_state
    in
    S.memo compute

let initial_state_formals kf (state:Relations_type.Model.t) =
  match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
        List.fold_right
          initialize_var_using_type
          f.sformals
          state

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ ->
      Value_parameters.result ~once:true ~current:true "not enough arguments in function call.";
      acc
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals kf state actuals =
  let formals = Kernel_function.get_formals kf in
  fold_left2_best_effort
    (fun acc (_,actual) formal ->
       let loc_without_size =
	 Location_Bytes.inject
	   (Base.create_varinfo formal)
	   (Ival.zero)
       in
       let loc = make_loc
	 (loc_bytes_to_loc_bits loc_without_size)
	 (sizeof_vid formal)
       in
       Relations_type.Model.add_binding
	 ~with_alarms:CilE.warn_none_mode ~exact:true
	 acc loc actual)
    state
    actuals
    formals

(* In the state [initial_state] globals and formals are present
   but locals of [kf] are not.*)
let compute_with_initial_state kf initial_state =
  match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
        let initial_state =
	  List.fold_left
	    (fun acc local ->
               Relations_type.Model.add_binding_unspecified
                 acc
                 (Locations.loc_of_varinfo local))
	    initial_state
	    f.slocals
        in
        let initial_state = check_fct_preconditions kf initial_state in
	compute_using_cfg kf initial_state

let compute_entry_point kf ~library =
  Kf_state.mark_as_called kf;
  Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
    (if library then "n in" else " ")
    Kernel_function.pretty_name kf;
  let initial_state_globals =
    if Db.Value.globals_use_supplied_state () then (
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state supplied by the user";
      Value_parameters.debug "@[<hov 0>Values of globals@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r)
    else (
      Value_parameters.feedback "Computing initial state";
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state computed";
      Value_parameters.result
        "@[<hov 0>Values of globals at initialization@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r
    ) in
  Db.Value.update_table Kglobal initial_state_globals;

  Mark_noresults.run();

  let with_formals = match Db.Value.fun_get_args () with
    | None -> initial_state_formals kf initial_state_globals
    | Some formals -> actualize_formals kf initial_state_globals
        (List.map (fun f -> (), f) formals)
  in
  Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
  let result =
    compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
  in
  Value_parameters.feedback "done for function %a"
    Kernel_function.pretty_name kf;
  result

exception Not_modular
exception Invalid_CEA_alloc
exception Invalid_CEA_memcpy

module Dynamic_Alloc_Table =
  Computation.Hashtbl
    (struct
       type t = string
       let hash = Hashtbl.hash
       let equal = (=)
     end)
    (Location_Bytes.Datatype)
    (struct
       let dependencies = [Db.Value.self]
       let size = 79
       let name = "Dynamic_Alloc_Table"
     end)

module Mem_Exec_Datatype =
  Project.Datatype.Register
    (struct
       type t =
	   Relations_type.Model.t
	   * (Datatype.Option(V_Offsetmap.Datatype).t * Relations_type.Model.t)
	   * Locations.Zone.t (* in *)
	   * Locations.Zone.t (* out *)
       let copy _ = assert false (* TODO: deep copy *)
       let descr = Project.no_descr
       let name = "Mem_Exec"
     end)

module Mem_Exec =
  Kernel_function.Make_Table
    (Mem_Exec_Datatype)
    (struct
       let name = "Mem_Exec"
       let size = 7
       let dependencies = [ Db.Value.self ]
     end)

exception Not_found_lonely_key

let wrap_ptr i =
  Some
    (V_Offsetmap.update_ival
       ~with_alarms:CilE.warn_none_mode
       ~validity:Base.All
       ~offsets:Ival.zero
       ~exact:true
       ~size:(Int.of_int (bitsSizeOf intPtrType))
       V_Offsetmap.empty
       (V_Or_Uninitialized.initialized i))

let compute_call kf ~call_kinstr
    (initial_state:Relations_type.Model.t) actuals =
  let initial_state = Relations_type.Model.drop_relations initial_state in
  let with_formals = actualize_formals kf initial_state actuals in
  Db.Value.merge_initial_state kf with_formals;
  let stack_without_call = for_callbacks_stack () in
  Db.Value.Call_Value_Callbacks.apply
    (with_formals, ((kf, call_kinstr) :: stack_without_call));
  let name = Kernel_function.get_name kf in
  (* function whose name starts with 'CEA_'
     print their arguments on stdout during computations.*)
  let result =
    if Ast_info.is_cea_dump_function name then begin
	let l = fst (CurrentLoc.get ()) in
	Value_parameters.result "DUMPING STATE of file %s line %d@\n%a=END OF DUMP=="
          l.Lexing.pos_fname l.Lexing.pos_lnum
          Relations_type.Model.pretty initial_state;
	None, initial_state, Location_Bits.Top_Param.bottom
      end
    else if Ast_info.is_cea_alloc name
    then begin
	try
          let file = match actuals with
          | [_,file] -> file
          | _ -> raise Invalid_CEA_alloc
          in
          let file_base,_file_offset =
	    try
	      Cvalue_type.V.find_lonely_key file
	    with Not_found -> raise Not_found_lonely_key
          in
          let file = match file_base with
          | Base.String (_,s) -> s
          | Base.Var (s,_) | Base.Initialized_Var (s,_) -> s.vname
          | Base.Null | Base.Cell_class _ -> raise Invalid_CEA_alloc

          in
          let loc =
	    Dynamic_Alloc_Table.memo
	      (fun file ->
		let new_name =
                  if Extlib.string_prefix ~strict:true "Frama_C_alloc_" file
	          then file
	          else Format.sprintf "Frama_C_alloc_%s" file
		in
		let new_name = Cabs2cil.fresh_global new_name in
		let unbounded_type =
                  TArray(intType,Some (new_exp (Const (CStr "NOSIZE"))),empty_size_cache (),[])
		in
		let new_varinfo =
	          makeGlobalVar ~logic:true new_name unbounded_type
		in
		let new_offsetmap =
	          Cvalue_type.V_Offsetmap.sized_zero (memory_size ())
		in
		let new_base =
	          Cvalue_type.Default_offsetmap.create_initialized_var
		    new_varinfo
		    Base.All
		    new_offsetmap
		in
		Location_Bytes.inject new_base Ival.zero)
	      file
          in
          wrap_ptr loc, initial_state, Location_Bits.Top_Param.bottom
	with
	| Ival.Error_Top | Invalid_CEA_alloc
	| Not_found_lonely_key (* from [find_lonely_key] *)
          -> Value_parameters.error
	    "Invalid argument for Frama_C_alloc_infinite function";
	    do_degenerate None;
	    raise Db.Value.Aborted
	| Not_found -> assert false
      end
    else
      try
	let abstract_function = Builtins.find_builtin name in
	abstract_function initial_state actuals
      with Not_found ->
	if Ast_info.is_cea_alloc_with_validity name then begin
	    try
	      let size = match actuals with
	      | [_,size] -> size
	      | _ -> raise Invalid_CEA_alloc
	      in
	      let size =
		try
		  let size = Cvalue_type.V.find_ival size in
		  Ival.project_int size
		with Ival.Not_Singleton_Int | V.Not_based_on_null ->
		  raise Invalid_CEA_alloc
	      in
	      if Int.le size Int.zero then raise Invalid_CEA_alloc;
	      let new_name =
		Format.sprintf "Frama_C_alloc"
	      in
	      let new_name = Cabs2cil.fresh_global new_name in
	      let bounded_type =
		TArray(charType,
		      Some (new_exp (Const (CInt64 (Int.to_int64 size,IInt ,None)))),
		      empty_size_cache (),
		      [])
	      in
	      let new_varinfo = makeGlobalVar ~logic:true new_name bounded_type in
	      let size_in_bits = Int.mul (sizeofchar()) size in
	      let new_offsetmap =
		Cvalue_type.V_Offsetmap.sized_zero ~size_in_bits
	      in
	      let new_base =
		Cvalue_type.Default_offsetmap.create_initialized_var
		  new_varinfo
		  (Base.Known (Int.zero, Int.pred size_in_bits))
		  new_offsetmap
	      in
	      let loc_without_size = Location_Bytes.inject new_base Ival.zero in
	      (*      Hashtbl.add dynamic_alloc_table file loc_without_size; *)
	      (wrap_ptr loc_without_size),initial_state, Location_Bits.Top_Param.bottom
	    with Ival.Error_Top | Invalid_CEA_alloc
	    | Not_found (* from [find_lonely_key]*)
	      ->
		Value_parameters.error
		  "Invalid argument for Frama_C_alloc_size function";
		do_degenerate None;
		raise Db.Value.Aborted
	  end else if Ast_info.is_cea_function name then begin
	      Value_parameters.result "Called %s%a"
		name
		(Pretty_utils.pp_flowlist (fun fmt (_,x) -> V.pretty fmt x))
		actuals;
	      None,initial_state, Location_Bits.Top_Param.bottom
	    end
	    else if name = "Frama_C_memcpy"
	    then begin
		match actuals with
		| [exp_dst,dst; _,src ; _,size] ->
		    begin try
			let exp_lv = mkMem ~addr:exp_dst ~off:NoOffset in
			let size =
			  Int.mul
			    (Int.of_int 8)
			    (let size = Cvalue_type.V.find_ival size in
			     Ival.project_int size)
			in
			let right = loc_bytes_to_loc_bits src in
			None,
		      copy_paste_locations
			~with_alarms:(warn_all_quiet_mode ())
			~exp_lv
			~left:(loc_bytes_to_loc_bits dst)
			~right
			size
			initial_state,
		      Location_Bits.get_bases right
		      with
			Ival.Not_Singleton_Int | V.Not_based_on_null | Lmap.Cannot_copy ->
			  Value_parameters.error
			    "Invalid call to Frama_C_memcpy function(%a, %a, %a)"
			    Cvalue_type.V.pretty dst
			    Cvalue_type.V.pretty src
			    Cvalue_type.V.pretty size;
			  do_degenerate None;
			  raise Db.Value.Aborted
		    end
		| _ -> Value_parameters.error
		    "Invalid argument for Frama_C_memcpy function\n";
		    do_degenerate None;
		    raise Db.Value.Aborted
	      end
	    else begin
		Value_parameters.feedback "computing for function %a <-%a.@\nCalled from %a."
		  Kernel_function.pretty_name kf
		  pretty_call_stack !call_stack
		  pretty_loc_simply
		  (CilE.current_stmt());

		Kf_state.mark_as_called kf;
		let modular =
		  Value_parameters.MemExecAll.get ()
		  || Cilutil.StringSet.mem name (Value_parameters.MemFunctions.get ())
		in
		let result =
		  match kf.fundec with
		  | Definition _ ->
		      begin try
			  if not modular then raise Not_modular;
			let mem_initial_state, mem_final_state, mem_in, mem_outs =
			  !Db.Value.memoize kf;
			  try Mem_Exec.find kf with Not_found -> raise Not_modular
			in
			try
			  let instanciation =
			    Relations_type.Model.is_included_actual_generic
			      (Zone.join mem_in mem_outs)
			      with_formals
			      mem_initial_state
			  in
			  Value_parameters.result ~current:true "Instanciation succeeded: %a"
			    (BaseUtils.BaseMap.pretty Location_Bytes.pretty)
			    instanciation;
			  compute_using_mem kf
			    initial_state
			    mem_final_state
			    mem_outs
			    instanciation
			with Is_not_included ->
			  Value_parameters.result ~current:true ~once:true
			    "Failed to see context as an instance of the generic context: inlining call to %a."
			    Kernel_function.pretty_name kf;
			  raise Not_modular
			with Not_modular ->
			  compute_with_initial_state kf ~call_kinstr with_formals
		      end
		  | Declaration (_,varinfo,_,_) ->
		      let stateset = check_fct_preconditions kf with_formals in
		      (* TODO: This is a hack. Use a function that checks preconditions
			 without multiplying the states instead -- or
			 compute_using_prototype several times *)
		      let state_with_formals = State_set.join stateset in
		      let retres_vi, result_state, thing =
			compute_using_prototype kf ~state_with_formals in
		      let result_state =
			check_fct_postconditions ~result:retres_vi kf
			  (State_set.singleton state_with_formals)
			  (State_set.singleton result_state)
			  Normal
		      in
		      let result_state = State_set.join result_state in
		      let result, is_retres =
			match retres_vi with
			  None -> None, (fun _ -> false)
			| Some vi ->
			    let value_state =
			      Relations_type.Model.value_state result_state
			    in
			    let retres_base = Base.create_varinfo vi in
			    Some
			      (Cvalue_type.Model.find_base
				  retres_base
				  value_state),
			    (fun b -> Base.equal b retres_base)
		      in
		      let result_state =
			Relations_type.Model.filter_base
			  (* TODO: Just remove the formals without iterating
			     over the state *)
			  (fun base ->
			    (not (Base.is_formal_of_prototype base varinfo))
			    && not (is_retres base) )
			  result_state
		      in
		      result, result_state, thing
		in
		Value_parameters.feedback "Done for function %a"
		  Kernel_function.pretty_name kf;
		result
	      end
  in
  result

let memoize kf =
  try
    ignore
      (Mem_Exec.memo
	 (fun kf ->
	    Kf_state.mark_as_called kf;
	    let with_globals = initial_state_contextfree_only_globals () in
	    let with_formals = initial_state_formals kf with_globals in
	    let (a,b,_) =
	      compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
	    in
            let result = a,b in
	    let ins =
	      (!Db.InOutContext.get_internal kf).Inout_type.over_inputs
	    in
	    let outs = !Db.Outputs.get_external kf in
	    with_formals,result,ins,outs)
	 kf)
  with Db.Value.Aborted ->
    (* the function will not be memoized. TODO: inform the user
       that the analyzer will behave as if the option was not set *)
    ()

let force_compute () =
  try
    let kf, library = Globals.entry_point () in
    ignore (compute_entry_point kf ~library);
    (* Move all alarms to Db *)
    Db.Properties.synchronize_alarms [ Db.Value.self ];
    Db.Value.mark_as_computed ();
    (* Cleanup trivially redundant alarms *)
    !Db.Scope.rm_asserts ()
  with
  | Db.Value.Aborted ->
      (* This case is reached only if [do_degenerate] did not raise another
         exception to handle abortion properly. See the behavior of the GUI
         in case of degeneration to understand the machinery. *)
      Db.Value.mark_as_computed ();
      Value_parameters.abort
	"Degeneration occured:@\nresults are not correct for lines of code that can be reached from the degeneration point."
  | Globals.No_such_entry_point _ as exn -> raise exn
  | exn -> Db.Value.mark_as_computed (); raise exn

let () = compute_call_ref := compute_call

let _self =
  Db.register_compute "Value.compute"
    [ Db.Value.self ]
    Db.Value.compute
    force_compute

let () = Db.Value.memoize := memoize
let () = Db.Value.initial_state_only_globals :=
  (fun () -> if snd(Globals.entry_point ()) then
     initial_state_contextfree_only_globals ()
   else
     initial_state_only_globals ()
  )
let () = Db.Value.find_lv_plus := find_lv_plus
let () = Db.Value.eval_expr_with_state :=
  (fun ~with_alarms state expr ->
     let (s,_,v) = eval_expr_with_deps_state ~with_alarms None state expr in
     s,v)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

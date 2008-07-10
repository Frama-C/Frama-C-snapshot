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

(* $Id: eval.ml,v 1.149 2008/07/02 13:33:17 uid528 Exp $ *)

(** Analysis for values and pointers *)

open Cil_types
open Cil
open Db
open Db_types
open Locations
open Abstract_interp
open Abstract_value
open Bit_utils
open Cvalue_type
open CilE

let set_loc kinstr =
  match kinstr with
  | Kglobal -> currentLoc:= locUnknown
  | Kstmt s -> currentLoc:= Ast_info.loc_stmt s

exception Leaf (* raised when nothing is known for a function :
                  no source nor specification *)

exception Not_an_exact_loc

exception Reduce_to_bottom

type cond =
    { exp: exp; (* The condition of the branch*)
      positive: bool; (* true: normal false: negated *)}

type called_function =
    { called_kf : kernel_function;
      call_site : kinstr;
      called_merge_current : degenerate:bool -> unit}

let call_stack : called_function list ref = ref []

let pop_call_stack () =
(*  Format.printf "Popping call stack@.";
  List.iter
    (fun cf ->
      Format.printf "%s@." (Kernel_function.get_name cf.called_kf))
    !call_stack;
  Format.printf ".@."; *)
  call_stack := List.tl !call_stack

let push_call_stack cf =
(*  Format.printf "Pushing %s on call stack@."
    (Kernel_function.get_name cf.called_kf); *)
  call_stack := cf :: !call_stack

let current_kf () = (List.hd !call_stack).called_kf

module Got_Imprecise_Value =
  Computation.Ref
    (struct include Datatype.Bool let default = false end)
    (struct
       let name = Project.Computation.Name.make "Eval.Got_Imprecise_Value"
       let dependencies = [ Value.self ]
     end)

let do_degenerate lv =
  List.iter
    (fun {called_merge_current = merge_current } -> merge_current ~degenerate:true)
    !call_stack;
  !Db.Value.degeneration_occurred (CilE.current_stmt ()) lv

let for_callbacks_stack () =
    List.map (fun {called_kf = kf; call_site = ki} -> kf,ki) !call_stack

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty_name fmt (current_kf())

exception Offset_not_based_on_Null of
	  Locations.Zone.t option * Location_Bytes.t

let offsetmap_top_adresses_of_locals fundec =
  let cached_f = Location_Bytes.contains_adresses_of_locals fundec
  in
  fun offsetmap ->
    if Cvalue_type.V_Offsetmap.is_empty offsetmap
    then offsetmap, true
    else
      let found_locals = ref false in
      let loc_contains_adresses_of_locals t =
	let l =
          cached_f t.Cvalue_type.V_Or_Uninitialized.v
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
          loc_contains_adresses_of_locals
          (fun v -> Cvalue_type.V_Or_Uninitialized.unspecify_escaping_locals fundec v)
	  offsetmap
      in
      result, !found_locals

let warn_locals_escape fundec k =
  (*TODO [BM->VP] use the right name for function name mangling *)
      CilE.warn_once "local escaping the scope of %s through %a"
	(fundec.svar.vname) Base.pretty k


let warn_locals_escape_result fundec =
  (*TODO [BM->VP] use the right name for function name mangling *)
  CilE.warn_once "local escaping the scope of %s through \\result"
    (fundec.svar.vname)

external getperfcount : unit -> int = "getperfcount"

let top_adresses_of_locals fundec =
  let offsetmap_top_adresses_of_locals = offsetmap_top_adresses_of_locals fundec
  in
  let state_top_adresses_of_locals state =
    let entry_point = Globals.entry_point () in
    if snd entry_point (* lib *) ||
      current_kf() != fst entry_point (* not entry point *)
    then begin
	let f k offsm acc =
	  let r,found_locals = offsetmap_top_adresses_of_locals offsm in
	  if found_locals then
	    warn_locals_escape fundec k;
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
      end
    else state
  in
  offsetmap_top_adresses_of_locals, state_top_adresses_of_locals

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
        (*	  Format.printf "debug do_cast: %a -> %a@."
	          V.pretty expr V.pretty result; *)
	result
    | _ -> assert false
  in
  match unrollType t with
  | TInt _ | TFloat _  as t' ->
      treat t'
  | TPtr _ ->
      treat !upointType
  | TEnum _ ->
      if !enum_are_signed then
        treat (TInt(IInt,[]))
      else treat (TInt(IUInt,[]))
  | TComp _ -> expr (* see test [struct_call.c] *)
  | TBuiltin_va_list _ ->
      (match with_alarms.imprecision_tracing with
       | Aignore -> ()
       | Acall f -> f ()
       | Alog -> CilE.warn_once "cast to __builtin_va_list is not implemented yet.");
      V.topify_arith_origin expr
  | TFun _ -> expr
  | TNamed _ -> assert false
  | TVoid _ -> assert false
  | TArray _ -> assert false

let do_promotion ~with_alarms ~src_typ ~dest_type v =
  match dest_type, src_typ with
  | TFloat _, TInt _ -> Cvalue_type.V.cast_int_to_float ~with_alarms v
  | TInt _, TFloat _ -> Cvalue_type.V.cast_float_to_int ~with_alarms v
  | _, _ -> v


exception Cannot_find_lv

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

      (* If both adresses are valid, they can be compared.
	 If one/both is not valid, the only way they can be
	 compared is if they are offsets in a same array t.
	 In this case, if t+n is the address of the last valid
	 location in t, t+n+1 is allowed in the comparison *)
      if (not (Locations.is_valid (make_loc (loc_bytes_to_loc_bits rest_1) Int_Base.one))) ||
(not (Locations.is_valid (make_loc (loc_bytes_to_loc_bits rest_2) Int_Base.one)))
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
    (*Format.printf "debugging alarm adress comparison: '%a' '%a'@."
      Cvalue_type.V.pretty ev1
      Cvalue_type.V.pretty ev2;*)
    ev1, ev2


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
    CilE.warn_once "reading left-value @[%a@].@ @[%t%t@]"
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
  snd (lval_to_loc_deps_option
         ~with_alarms
         ~deps:None
         state
         lv)

and lval_to_loc_deps_option
    ~with_alarms ~deps (state:Relations_type.Model.t)
    (base,offset as lv)  =
  if not (Relations_type.Model.is_reachable state) then
    deps, loc_bottom
  else
    let typ = match base with
    | Var host -> host.vtype
    | Mem x -> typeOf x
    in
    try
      let deps, offs =
	eval_offset
          ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
          ~with_alarms deps typ state offset
      in
      base_to_loc ~with_alarms ?deps state lv base offs
    with Offset_not_based_on_Null(deps,offset) ->
      let deps, loc_if_there_wasnt_offset =
	base_to_loc ~with_alarms ?deps state lv base Ival.zero
      in
      deps,
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
    ~with_alarms ~deps (state:Relations_type.Model.t) (base, offset as v)
    =
  if not (Relations_type.Model.is_reachable state) then
    deps,loc_bottom
  else
    let typ = match base with
    | Var host -> host.vtype
    | Mem x -> typeOf x
    in
    try
      let deps, offs =
        eval_offset
          ~reduce_valid_index:false
          ~with_alarms deps typ state offset
      in
      let _, loc = base_to_loc ~with_alarms state v base offs in
      deps, loc
    with Offset_not_based_on_Null (deps,offset) ->
      let _deps_base, loc_if_there_wasnt_offset =
	base_to_loc ~with_alarms ?deps:None state v base Ival.zero
      in
      deps,
      loc_bits_to_loc v
	(Location_Bits.join
	    (loc_bytes_to_loc_bits offset)
	    loc_if_there_wasnt_offset.loc)

(** Detects if an expression can be considered as a lvalue even though
    it is hidden by a cast that does not change the lvalue.
    Raises [exn] if it is not a lvalue.

    TODO: When the goal is to recognize the form (cast)l-value == expr,
    it would be better and more powerful to have chains of inverse functions *)

and pass_cast ~with_alarms state exn typ e =
  (*Format.printf "Cast to %S the expression %S@\n"
    (Pretty.sprint ~width:80 (d_type () typ))
    (Pretty.sprint ~width:80 (d_exp () e));*)

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
	if match sztyp,szexpr with Int_Base.Value styp, Int_Base.Value sexpr -> Int.ge styp sexpr | _ -> false then
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
  match ee with
  | Lval lv -> lv
  | CastE (typ,e) ->
      (*Format.printf "Trying to pass cast of %a of type %a to type %a@\n"
        d_exp e
        d_type (typeOf e)
        d_type typ;*)
      ( match unrollType typ, unrollType (typeOf e) with
	TFloat _, TFloat _ -> find_lv ~with_alarms state e
	  (* see remark at pass_cast about inverse functions *)
      | _ ->
	  pass_cast ~with_alarms state Cannot_find_lv typ e;
	  (*Format.printf "Passed@\n";*)
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
      match e with
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
  then
    (Some Zone.bottom), loc_bits_to_loc lv Location_Bits.bottom
  else
    let result = match base with
    | Var host ->
	let base = Base.create_varinfo host in
	deps,
	loc_bits_to_loc lv (Location_Bits.inject base offs)
    | Mem x ->
	let deps, loc_lv = eval_expr_with_deps ~with_alarms deps state x in
	let loc_bits =
	  Location_Bits.location_shift
	    offs
	    (loc_bytes_to_loc_bits loc_lv)
	in
	deps, loc_bits_to_loc lv loc_bits
    in
    CilE.set_syntactic_context (CilE.SyMem lv);
    result

and eval_expr ~with_alarms state e =
  snd (eval_expr_with_deps ~with_alarms None state e)

and reduce_by_valid_expr ~with_alarms ~positive exp state =
  try
    let lv = find_lv ~with_alarms state exp in
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
(*          	  CilE.warn_once "reducing by validity %a -> %a@."
		  Location_Bytes.pretty value
		    Location_Bytes.pretty reduced_value;*)
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
  (* Pretty.printf "EXPR to EVAL:%a\n" d_exp e; *)
  let deps,expr = match Cil.stripInfo e with
  | Info _ -> assert false
  | Const v ->
      deps, begin match v with
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
	  CilE.log_once "Warning: float support is experimental";
	  (*	  Format.printf "f:%f@." f; *)
	  V.inject_ival (Ival.inject_float (Ival.Float_abstract.inject f f))
      | CWStr _ ->
          CilE.warn_once "approximation because of a wide string";
          (* TODO *) V.top_int
      | CStr s ->
          V.inject (Base.create_string s) Ival.zero
      | CEnum (_,_,_) -> assert false(*TODO*)
	end
  | BinOp (op, e1 , e2 , typ) ->
      let deps, ev1 = eval_expr_with_deps ~with_alarms deps state e1 in
      let deps, ev2 = eval_expr_with_deps ~with_alarms deps state e2 in
      if V.is_bottom ev1 || V.is_bottom ev2 then  (Some Zone.bottom),V.bottom
      else begin
          CilE.set_syntactic_context (CilE.SyBinOp (op,e1,e2));
	  begin match unrollType (typeOf e1) with
	  | TFloat _ ->
	      let interpreted_expr =
		try
		  let f1 =
		    try
		      let v1 = V.find_ival ev1 in
		      Ival.project_float v1
		    with V.Not_based_on_null
		    | Ival.Float_abstract.Nan_or_infinite ->
			CilE.warn_once "alarm: float value must be finite";
			Ival.Float_abstract.top
		  in
		  let f2 =
		    try
		      let v2 = V.find_ival ev2 in
		      Ival.project_float v2
		    with V.Not_based_on_null
		    | Ival.Float_abstract.Nan_or_infinite ->
			CilE.warn_once "alarm: converting value to float";
			Ival.Float_abstract.top
		  in
		  let binary_float_floats _name f =
		    try
		      V.inject_ival (Ival.inject_float (f f1 f2))
		    with
		      Ival.Float_abstract.Nan_or_infinite ->
			warn_result_nan_infinite with_alarms ;
			V.top_float
		    | Ival.Float_abstract.Bottom ->
			CilE.warn_result_nan_infinite with_alarms ;
			V.bottom
		  in
		  begin match op with
		  | PlusA ->
		      binary_float_floats "+." Ival.Float_abstract.add_float
		  | MinusA ->
		      binary_float_floats "-." Ival.Float_abstract.sub_float
		  | Mult ->
		      binary_float_floats "*." Ival.Float_abstract.mult_float
		  | Div ->
		      if Ival.Float_abstract.contains_zero f2
		      then CilE.warn_once "alarm: %s" "/.";
		      binary_float_floats "/." Ival.Float_abstract.div_float
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
		  CilE.warn_once "alarm: float operation on address";

		  V.join
		    (V.topify_arith_origin ev1)
		    (V.topify_arith_origin ev2)
	      in
	      deps, interpreted_expr
	  | TInt _ | TPtr (_, _) | _ ->
	      let compute_diff acc =
		if Cmdline.UseRelations.get ()
		then begin
		    let lv1 = find_lv_plus ~with_alarms state e1 in
		    let lv2 = find_lv_plus ~with_alarms state e2 in
		    List.fold_left
		      (fun acc (lv1, offs1)  ->
			let loc1 = lval_to_loc ~with_alarms state lv1 in
			(*  Format.printf "lv1:%a + %a@\n"
			    Locations.pretty loc1
			    Ival.pretty offs1; *)
			List.fold_left
			  (fun acc (lv2, offs2)  ->
			    let loc2 = lval_to_loc ~with_alarms state lv2 in
			    (* Format.printf "lv2:%a + %a@\n"
			       Locations.pretty loc2
			       Ival.pretty offs2;*)
			    try
			      let new_v =
				V.location_shift
				  (Ival.sub offs1 offs2)
				  (Relations_type.Model.compute_diff state loc1 loc2)
			      in
			      (* Format.printf "newv:%a@\n" V.pretty new_v;*)
			      assert (V.is_included new_v acc);
			      new_v
			    with Relations_type.Use_Main_Memory -> acc)
			  acc
			  lv2)
		      acc
		      lv1
		  end
		else acc
	      in
	      let interpreted_expr = begin match op with
	      | PlusPI | IndexPI ->
		  V.add_untyped (osizeof_pointed typ) ev1 ev2
	      | MinusPI ->
		  V.add_untyped (Int_Base.neg (osizeof_pointed typ)) ev1 ev2
	      | PlusA ->
		  V.add_untyped (Int_Base.inject Int.one) ev1 ev2
	      | MinusA | MinusPP ->

		  let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
		  (* Format.printf "minus1:%a@\n" V.pretty minus_val;*)
		  let minus_val = compute_diff minus_val in
		  (* Format.printf "minus2:%a@\n" V.pretty minus_val;*)
		  if op = MinusA
		  then minus_val
		  else (* MinusPP *)
		    ( try
			let size = Int_Base.project
			  (sizeof_pointed(Cil.typeOf e1))
			in
			let size = Int.div size (Int.of_int 8) in
			let k1,v1 = Cvalue_type.V.find_lonely_key ev1 in
			let k2,v2 = Cvalue_type.V.find_lonely_key ev2 in
			if Base.compare k1 k2 <> 0 then raise Not_found;
			let diff = Ival.sub v1 v2 in
			(*	Format.printf "v1 %a v2 %a diff %a@\n"
				Ival.pretty v1
				Ival.pretty v2
				Ival.pretty diff; *)
			Cvalue_type.V.inject_ival
			  (Ival.scale_div ~pos:true size diff)
		      with
			Int_Base.Error_Top | Not_found ->
			  V.join
			    (V.topify_arith_origin ev1)
			    (V.topify_arith_origin ev2))
	      | Mod -> V.arithmetic_function ~with_alarms "%" Ival.c_rem ev1 ev2
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
		  let diff = compute_diff V.top in
		  let result = f diff V.singleton_zero in
		  if V.cardinal_zero_or_one result
		  then result
		  else f ev1 ev2
	      | Shiftrt ->
		  begin try
		      let signed = is_signed_int_enum_pointer typ in
		      V.shift_right ~with_alarms ~size:(bitsSizeOf typ) ~signed ev1 ev2
			(*if signed then
			  V.oper_on_values ~with_alarms ">>" Int.shift_right ev1 ev2
			  else
			  V.oper_on_values ~with_alarms ">>" Int.log_shift_right ev1 ev2*)
		    with SizeOfError _ ->
		      (match with_alarms.imprecision_tracing with
                       | Aignore -> ()
                       | Acall f -> f ()
                       | Alog ->  warn_once "shifting value of unknown size");
		      V.top  (* TODO: topify ... *)
		  end
	      | Shiftlt ->
		  begin try
		      V.shift_left ~with_alarms ~size:(bitsSizeOf typ) ev1 ev2
		  with SizeOfError _ ->
		    (match with_alarms.imprecision_tracing with
                     | Aignore -> ()
                     | Acall f -> f ()
                     | Alog ->  warn_once "shifting value of unknown size");
		      V.top (* TODO: topify ... *)
		  end
		end
	      in deps, interpreted_expr
	  end
	end
  | Lval lv ->
      eval_lval ~with_alarms deps state lv

  | AddrOf v | StartOf v ->
      let deps, r =
	lval_to_loc_with_offset_deps_only_option ~with_alarms ?deps state v
      in
      deps, loc_to_loc_without_size r

  | CastE (typ, e) ->
      let deps, evaled_expr =
	eval_expr_with_deps ~with_alarms deps state e
      in
      let src_typ = unrollType (typeOf e) in
      let dest_type = unrollType typ in
      deps,do_promotion ~with_alarms ~dest_type ~src_typ evaled_expr

  | SizeOf typ ->
      deps,(try V.inject_ival
          (Ival.inject_singleton ((Int.of_int ((bitsSizeOf typ) / 8))))
	with SizeOfError _ ->
	  error "cannot interpret sizeof(incomplete type)";
	  V.top_int)
  | SizeOfE e -> let typ = typeOf e in
		 deps, (try V.inject_ival
		     (Ival.inject_singleton ((Int.of_int ((bitsSizeOf typ) / 8))))
		   with SizeOfError _ ->
		     error "cannot interpret sizeof(incomplete type)";
		     V.top_int)

  | UnOp (LNot, e, _) ->
      (* TODO:  on float, LNot is equivalent to == 0.0 *)
      let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
      CilE.set_syntactic_context (CilE.SyUnOp e);
      let t1 = typeOf e in
      if isIntegralType t1 || isPointerType t1
      then
	deps, V.interp_boolean
	  ~contains_zero:(V.contains_non_zero expr)
	  ~contains_non_zero:(V.contains_zero expr)
      else deps, V.zero_or_one

  | UnOp (Neg, e, t) ->
      let t = unrollType t in
      ( match t with TFloat _ ->
	let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
	CilE.set_syntactic_context (CilE.SyUnOp e);
	let result =
	  try
	    let v = V.find_ival expr in
	    let f =
	      Ival.project_float v
	    in
	    V.inject_ival (Ival.inject_float (Ival.Float_abstract.neg_float f))
	  with
	    V.Not_based_on_null ->
	      CilE.warn_once "alarm: converting address to float";
	      V.topify_arith_origin expr
	  | Ival.Float_abstract.Nan_or_infinite ->
	      CilE.warn_once "alarm: converting value to float";
	      V.top_float
	in
	deps, result
      | _ ->
	  let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
	  CilE.set_syntactic_context (CilE.SyUnOp e);
	  let result =
	    try
	      let v = V.find_ival expr in
	      V.inject_ival (Ival.neg v)
	    with V.Not_based_on_null -> V.topify_arith_origin expr
	  in
	  deps, result)

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
      deps, result
  | AlignOfE _|AlignOf _|SizeOfStr _
      ->
      CilE.warn_once "unsupported C construct alignof or sizeof string";
	deps, V.top_int
  in
  deps, let r =
    if hasAttribute "volatile" (typeAttrs (typeOf e))
      && not (Cvalue_type.V.equal Cvalue_type.V.bottom expr)
    then ((*CilE.warn_once "evaluation of volatile value leads to top";*)
      V.top_int)
    else
      expr
  in
  (*Cil.warn "Expr to eval : %a\nBefore cast:%s After cast:%s\n"
    d_expr e
    (pretty_to_string V.pretty expr)
    (pretty_to_string V.pretty r);*)
  (*(match deps with
    | Some deps ->
    Format.printf "Deps: %a@\n" Zone.pretty deps
    | _ -> ());*)
  let result = do_cast ~with_alarms (typeOf e) r in
(*  if V.equal V.top result then (Cil.warn "Got TOP for %a@." d_exp e;
                                do_degenerate None);*)
  result

and eval_lval_using_main_memory ~with_alarms deps (state:Relations_type.Model.t)
    lv
    =
  let deps,loc = lval_to_loc_deps_option ~with_alarms ?deps state lv in
  CilE.set_syntactic_context (CilE.SyMem lv);
  let result = Relations_type.Model.find ~with_alarms state loc in
  (*
    Format.printf "debug eval_lval lv:%a loc:%a value:%a@\n"
    d_lval lv
    Locations.pretty loc
    V.pretty result;*)
  (match with_alarms.imprecision_tracing with
   | Aignore -> ()
   | Acall f -> f ()
   | Alog ->  warn_lval_read lv loc result);
  let new_deps =
    match deps with
    | None -> None
    | Some deps -> Some (Zone.join deps (valid_enumerate_bits loc))
  in
  new_deps, result

and eval_lval ~with_alarms deps state (base,offset as lv) =
  let deps, result_from_main_memory =
    eval_lval_using_main_memory ~with_alarms deps state lv
  in
  (* Format.printf "eval_lval got:%a@\n"
     Location_Bytes.pretty result_from_main_memory; *)
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
  | Mem(Lval sub_lv as e) when Cmdline.UseRelations.get () ->
      let typ = typeOf e in
      begin try
        let _, offs =
	  eval_offset ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
            ~with_alarms None typ state offset
        in
        find_loc_mem sub_lv offs
      with
        Offset_not_based_on_Null _ ->
          result_from_main_memory
      end
  | Mem(BinOp((PlusPI|IndexPI|MinusPI as op), Lval sub_lv ,e2,_) as e)
      when Cmdline.UseRelations.get () ->
      begin
        let e2 = eval_expr ~with_alarms state e2 in
        let typ = typeOf e in
        try
          let ival = Cvalue_type.V.find_ival e2 in
          let ival = if op=MinusPI then Ival.neg ival else ival in
          let _, offs =
            eval_offset ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
              ~with_alarms None typ state offset in
          let offs = (* convert to bits *)
            Ival.add
              (Ival.scale
                 (Int_Base.project (sizeof_pointed typ))
                 ival)
              offs
          in
          (*Format.printf "eval_lval *(lval+e)@\n";*)
          let result = find_loc_mem sub_lv offs in
          (*Format.printf "eval_lval leads to %a@\n" Cvalue_type.V.pretty result;*)
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
  (*  Format.printf "eval_lval@\nstate=%a@\nlval=%a@\nresult(rel)=%a@\nresult(main)=%a@\ninter=%a@\n"
      Relations_type.Model.pretty state
      d_lval lv
      Cvalue_type.V.pretty result
      Cvalue_type.V.pretty result_from_main_memory
      Cvalue_type.V.pretty result_inter; *)
  deps,result_inter

and eval_offset ~reduce_valid_index ~with_alarms deps typ state offset =
  match offset with
  | NoOffset ->
      deps, Ival.singleton_zero
  | Index (exp,remaining) ->
      let typ_pointed,array_size = match (unrollType typ) with
      | TArray (t,size,_) -> t, size
      | TPtr(t,_) ->
          (match unrollType t with
           | TArray (t,size,_) -> t,size (* pointer to start of an array *)
           | _ ->
               error "Got type '%a'" !Ast_printer.d_type t;
               assert false)
      | t ->
          error "Got type '%a'" !Ast_printer.d_type t;
          assert false
      in
      let deps, current = eval_expr_with_deps ~with_alarms deps state exp in
      if V.is_bottom current then (Some Zone.bottom),Ival.bottom
      else
        let offset =
	  try
            let v = V.find_ival current in
            let v = if reduce_valid_index then
              try
                let array_siz = lenOfArray64 array_size in
                let new_v =
                  Ival.narrow (Ival.inject_range
                                 (Some Int.zero)
                                 (Some (Int.of_int64 (Int64.pred array_siz)))) v
                in
                if not (Ival.equal new_v v) then
                  (match with_alarms.others with
                   | Aignore -> ()
                   | Acall f -> f ()
                   | Alog ->
                       CilE.set_syntactic_context
                         (CilE.SyBinOp (IndexPI,exp,Cilutil.out_some array_size));
                       CilE.warn_index  with_alarms "accessing");
                new_v
            with LenOfArray -> v
            else v
            in
            v
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
        in let deps, r =
	  eval_offset ~reduce_valid_index ~with_alarms
	    deps typ_pointed state remaining
	in
        let offset = Ival.scale_int64base (sizeof typ_pointed) offset in
        deps, Ival.add offset r
  | Field (fi,remaining) ->
      let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
      let deps, r = eval_offset ~reduce_valid_index ~with_alarms deps
	fi.ftype state remaining
      in
      deps, Ival.add (Ival.of_int current) r
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
      (*Format.printf "eval_as_exact_loc: %S@\n"
	(Pretty.sprint ~width:80 (d_exp () e));*)
      try
	let lv = find_lv ~with_alarms state e in
	(*Format.printf "eval_as_exact_loc: got lv %S@\n"
	  (Pretty.sprint ~width:80 (d_lval () lv));*)
	let loc = lval_to_loc ~with_alarms state lv in
	if valid_cardinal_zero_or_one loc then
	  let value_for_loc =
	    do_cast
              ~with_alarms
              (typeOf e)
              (Relations_type.Model.find ~with_alarms state loc)
	  in
          (*Format.printf "Got exact loc@\n";*)
	  loc,value_for_loc
	else ((*Format.printf "Not exact@\n";*)raise Not_an_exact_loc)
      with Cannot_find_lv ->
	((*Format.printf "Not exact (2)@\n";*)
	  raise Not_an_exact_loc)
    in
    let rec aux cond state =
      match cond.positive,cond.exp with
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
		(*Format.printf "val : %a -> %a -> %a (%a)@\n"
		  V.pretty value_for_loc
		  V.pretty v_sym
		  V.pretty v_asym
		  V.pretty cond_expr;*)
		if (V.equal v_asym V.bottom) then
		  ((*Format.printf "left_loc was :%a with value %a@\nloc is bottom@\n"
		     Locations.pretty left_loc V.pretty value_for_loc;*)
		    raise Reduce_to_bottom);
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
		  ((*Format.printf "right_loc was :%a with value %a@\nloc is bottom@\n"
		     Locations.pretty right_loc V.pretty value_for_loc;*)
		    raise Reduce_to_bottom);
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
            (*Format.printf "eval_cond: %S of %S and %S (type %S)@\n"
              (Pretty.sprint ~width:80 (d_binop () binop))
              (Pretty.sprint ~width:80 (d_exp () exp1))
              (Pretty.sprint ~width:80 (d_exp () exp2))
              (Pretty.sprint ~width:80 (d_type () typ));*)
	    eval_eq_ineq eval_symetric_int eval_antisymetric_int
	  else
	    eval_eq_ineq eval_symetric_float eval_antisymetric_float
      | true, BinOp (LAnd, exp1, exp2, _)
      | false, BinOp (LOr, exp1, exp2, _) ->
          let new_state = aux {cond with exp = exp1} state in
	  let result = aux {cond with exp = exp2} new_state in
          (*	Format.printf "(%s,%b) :  %a %a@\n"
	        (if op = LAnd then "&&" else "||")
	        cond.positive
	        Model.pretty new_v Model.pretty result; *)
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
          (*Format.printf "Negation of %S@\n" (Pretty.sprint ~width:80 (d_exp () exp));*)
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
		((*Format.printf "loc was :%a with value %a@\nloc is bottom@\n"
                   Locations.pretty loc V.pretty value_for_loc;*)
                  raise Reduce_to_bottom)
              else
		Relations_type.Model.reduce_binding
                  state loc new_value
            with Not_an_exact_loc  -> state)
      | _ -> state
    in
    let result =
      aux cond state
    in
    (*   Format.printf "eval_cond phase 1: %a --> %a@\n"
	 Relations_type.Model.pretty state
	 Relations_type.Model.pretty result;
    *)
    let rec get_vars acc cond =
      match cond with
      | Lval (Var v, off as lv) ->
          let offset = try
              snd (eval_offset ~reduce_valid_index:true ~with_alarms None v.vtype result off)
            with Offset_not_based_on_Null _ ->
              Ival.top
          in
          if Ival.cardinal_zero_or_one offset
          then
            let varid = Base.create_varinfo v in
            let loc =
              Locations.make_loc
                (Locations.Location_Bits.inject varid offset)
                (sizeof_lval lv)
            in
            loc :: acc
          else
	    get_vars_offset acc off
      | Lval (Mem e,_off) ->
          get_vars acc e
      | BinOp(_,v1,v2,_) ->
          get_vars (get_vars acc v1) v2
      | UnOp(_,v1,_) ->
          get_vars acc v1
      | _ -> acc (* TODO : more cases can be done *)
    and get_vars_offset acc offset =
      match offset with
        NoOffset -> acc
      | Field (_,off) -> get_vars_offset acc off
      | Index (ind,off) -> get_vars (get_vars_offset acc off) ind
    in
    let is_enumerable v =
      (*try*) let v_interp =
	Relations_type.Model.find ~with_alarms result v in
	      ignore (Location_Bytes.cardinal_less_than v_interp 6);
      v_interp
	(*with Failure "bound" ->
	  Format.printf "Bound got:@\n%a@\n" Relations_type.Model.pretty result;
	  exit 2*)
    in
    let rec enumerate_one_var l =
      match l with
      | [] -> raise Not_found
      | v::t ->
          try
            let v_interp = is_enumerable v in
            (*	  Format.printf "ok. Condition inversion@\n";*)
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
	  let cond_interp = eval_expr ~with_alarms env
	    cond.exp in
          (*	    let val_appears =
	            if cond.positive
	            then Ival.singleton_one
	            else Ival.singleton_zero
	            in*)
          (*	    Format.printf "val=%a "
	            Location_Bytes.pretty one_val;*)
          (*	    if Location_Bytes.is_included
	            (V.inject_ival val_appears)
	            cond_interp*)
	  if (not cond.positive || V.contains_non_zero cond_interp) &&
	    (cond.positive || V.contains_zero cond_interp)
	  then begin
	      (* Format.printf "stays@\n"; *)
	      Location_Bytes.join one_val acc
	    end
	  else begin
	      (* Format.printf "goes@\n"; *)
	      acc
	    end
	in
	let new_v_interp =
          Location_Bytes.fold_enum
            f v_interp1 Location_Bytes.bottom
	in
	let state_value =
	  if V.equal new_v_interp V.bottom
	  then begin
              (*             Format.printf "while inverting: loc was :%a with value %a@\ncond loc is bottom@\n"
                             Locations.pretty v V.pretty new_v_interp; *)
              raise Reduce_to_bottom
	    end
          else
            Relations_type.Model.reduce_binding
	      result v1 new_v_interp
	in
	state_value
      with Not_found -> result
    in
    let result1 =  invert_cond (get_vars [] cond.exp) in
    (*  Format.printf "evl_cond before inversion %a after %a@\n"
	Relations_type.Model.pretty result.value
	Relations_type.Model.pretty result1.value; *)
    if not (Relations_type.Model.is_reachable result1)
    then raise Reduce_to_bottom
    else result1

exception Ignore
  (* raised to completely ignore an instruction or statement *)

(* See bug report fs#182 *)
let resolv_func_vinfo ~with_alarms deps state funcexp =
  match funcexp with
  | Lval (Var vinfo,NoOffset) -> deps, [ Globals.Functions.get vinfo]
  | Lval (Mem v,NoOffset) ->
      let deps, loc = eval_expr_with_deps ~with_alarms deps state v in
      let fundecs = List.fold_left
	(fun acc varid ->
	   match varid with
           | Base.String (_,_) ->
               CilE.warn_once
		 "Function pointer call at string position in memory: ignoring this particular value";
               acc
	   | Base.Null ->
               CilE.warn_once
                 "Function pointer call at absolute position in memory: ignoring this particular value";
               acc
	   | Base.Cell_class _ ->
	       CilE.warn_once
		 "Function pointer call at memory cell class: ignoring this particular value";
               acc
	   | Base.Var (v,_) | Base.Initialized_Var (v,_) ->
	       Globals.Functions.get v :: acc
        )
	[]
	(try
           Location_Bytes.get_keys_exclusive Ival.zero loc
         with Location_Bytes.Not_all_keys ->
           CilE.warn_once
             "Function pointer call is completly unknown: assuming no effects";
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
      ~with_alarms:warn_none_mode
      ~exact:true
      state
      well_loc
      well
  in
  Relations_type.Model.add_binding
    ~with_alarms:warn_none_mode
    ~exact:true
    state_with_well
    loc
    well


(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  currentLoc := varinfo.vdecl;
  (*Format.printf "contextfree varinfo: %s@\n"
    varinfo.vname; *)
  let rec add_offsetmap depth v name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = loc_of_typoffset v typ_orig offset_orig in
    let must_initialize =
      if hasAttribute "const" (typeAttrs typ) then
        if Cvalue_type.V.equal
          (Relations_type.Model.find ~with_alarms:warn_none_mode state loc)
          Cvalue_type.V.top
        then
          (CilE.warn_once
             "no initializer for the const variable %a"
	     Ast_info.pretty_vname varinfo;
           true)
        else false
      else true
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
            ~with_alarms:warn_none_mode
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
            ~with_alarms:warn_none_mode
	    ~exact:true
	    state
	    loc
	    Cvalue_type.V.top_float
      | TFun _ ->
	  Relations_type.Model.add_binding
            ~with_alarms:warn_none_mode
	    ~exact:true
	    state
	    loc
	    (Cvalue_type.V.top_leaf_origin ())
      | TPtr (typ, _) as full_typ
	  when depth <= Cmdline.AutomaticContextMaxDepth.get () ->
          (*Format.printf "Allocationg one pointer %a for %s@." d_type typ varinfo.vname;*)
          let attr = typeAttr full_typ in

          if not (isVoidType typ) && not (isFunctionType typ) then
            let i = match findAttribute "arraylen" attr with
            | [AInt i] -> i
            | _ -> Cmdline.AutomaticContextMaxWidth.get ()
            in
            let pointed_typ = TArray(typ,Some (integer i), [])
            in
	    (* first create a new varid and offsetmap for the
	       "hidden location" *)
	    let hidden_var_name =
	      Cabs2cil.fresh_global ("star_" ^ name)
	    in
            let name_desc = "*"^name_desc in
	    let hidden_var =
              makeGlobalVar ~logic:true hidden_var_name pointed_typ
            in
            hidden_var.vdescr <- Some name_desc;
            let hidden_base = Base.create_logic
              hidden_var
              (match Base.validity_from_type hidden_var with
               | Base.Known (a,b) -> Base.Unknown (a,b)
               | (Base.All |  Base.Unknown _)  as s -> s)
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
	    Relations_type.Model.add_binding
              ~with_alarms:warn_none_mode
	      ~exact:true
	      state
	      loc
	      (Cvalue_type.V.join
                 Cvalue_type.V.singleton_zero
                 (Cvalue_type.V.inject hidden_base (Ival.zero)))

          else
            let hidden_var_name =
	      Cabs2cil.fresh_global ("star_" ^ name)
	    in
            let name_desc = "*"^name_desc in
	    let hidden_var =
              makeGlobalVar ~logic:true hidden_var_name typ
            in
            hidden_var.vdescr <- Some name_desc;
            let hidden_base =
              Base.create_logic
		hidden_var
		(if Cmdline.AllocatedContextValid.get () then
		   Base.Known (Int.zero,Bit_utils.max_bit_address ())
		 else
		   Base.Unknown (Int.zero,Bit_utils.max_bit_address ()))
            in
            make_well (Bit_utils.max_bit_size ()) hidden_base state loc

      | TArray (typ, len, _) ->
          (*Format.printf "Allocationg one array@.";*)
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
            warn_once "could not find a size for array. ";
            state
          end
      | TComp ({cstruct=true;} as compinfo, _) -> (* Struct *)
          (*          Format.printf "Allocationg one struct@\n"; *)
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
                ((*Format.printf "Padding foff:%d fwidht:%d noff:%d@."
                   field_offset field_width next_offset;*)
                 let loc = make_loc
                   (Location_Bits.inject v (Ival.of_int next_offset))
                   (Int_Base.inject (Int.of_int (field_offset-next_offset)))
                 in
	         Relations_type.Model.add_binding_unspecified
	           state
	           loc)
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
      | TComp ({cstruct=false;} as _compinfo, _) when
          is_fully_arithmetic typ
          -> (* Union of arithmetic types *)
          (*          Format.printf "Allocationg one arith. union@\n";*)
          Relations_type.Model.add_binding
            ~with_alarms:warn_none_mode
            ~exact:true
            state
            loc
            Cvalue_type.V.top_int

      | TPtr _ when Cmdline.AllocatedContextValid.get () ->
          (* deep pointers map to NULL in this case *)
          Relations_type.Model.add_binding
            ~with_alarms:warn_none_mode
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

let initial_state_only_globals () =
  Format.printf "Computing globals values@.";
  let state = ref Relations_type.Model.empty in
  let complete_init last_bitsoffset typ _l lval =
    (* Now process the non initialized bits defaulting to 0 *)
    (* Format.printf "Completing array init of %a@." d_lval lval;*)
    begin try
      let size_to_add, offset =
        bitsSizeOf typ - last_bitsoffset,
        Ival.inject_singleton (Int.of_int last_bitsoffset)
      in
      assert (size_to_add >= 0);
      (*Format.printf "Init missing %d->%d (%d) %a@."
        last_bitsoffset
        (bitsSizeOf typ)
        size_to_add d_type (unrollTypeDeep typ);*)
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
            ~with_alarms:warn_none_mode
            ~exact:true
            !state
	    loc
            v
    with Cil.SizeOfError _ ->
      CilE.warn_once "cannot provide a default initializer: size is unknown"
    end
  in
  let rec eval_init lval init =
    match init with
    | SingleInit exp ->
(*	CilE.warn_once "Computing init globals values for %a" d_exp exp; *)
	let loc = lval_to_loc ~with_alarms:warn_none_mode Relations_type.Model.empty lval
	in
	let exact = cardinal_zero_or_one loc in
	assert (if not exact then (Cil.warn "In global initialisation, the location can not be represented. Aborting@."; exit 1); true);
	let value = eval_expr ~with_alarms:warn_all_mode Relations_type.Model.empty exp in
(*	Format.printf "initial singleinit : %a <- %a@."
	    Locations.pretty loc
	    V.pretty value; *)
	let v =
	  if hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lval))
	  then V.top_int
	  else value
	in
	state :=
	  Relations_type.Model.add_binding ~with_alarms:warn_none_mode ~exact
	    !state loc v

    | CompoundInit (base_typ, l) ->
(*      Format.printf "Init: %d@." (List.length l); *)
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
                        error "Whacky initializer ? Please report.";
                        assert false)
                 in
                 (*Format.printf "Padding: base:%d in %d..%d@\n" base_off acc o;*)
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
                 CilE.warn_once
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
       currentLoc:=varinfo.vdecl;
       (*Format.printf "Initializing global %s@." varinfo.vname;*)
       match init.init with
       | None (*when
           isCompleteType varinfo.vtype*)
           -> (* Default to zero init thanks to ANSI p126 6.7.8.10 *)
           (* eval_init (Var varinfo, NoOffset) (makeZeroInit varinfo.vtype)*)
           if varinfo.vstorage = Extern then
             (* Must not initialize when the storage is extern. *)
             ((* Format.printf "Ignoring extern init for %s@." varinfo.vname;*)
               state := initialize_var_using_type varinfo !state )
           else complete_init 0 varinfo.vtype [] (Var varinfo,NoOffset)
(*       | None ->
           (* Cannot initialize with a default when type is incomplete. *)
           ()*)
       | Some i ->
           eval_init (Var varinfo,NoOffset) i
       end);

  (** Bind the declared range for NULL to uninitialized *)
  if Int.le
    (Cmdline.MinValidAbsoluteAddress.get ())
    (Cmdline.MaxValidAbsoluteAddress.get ())
  then begin
    let loc_bits = Location_Bits.inject_ival
      (Ival.inject_singleton (Cmdline.MinValidAbsoluteAddress.get ()))
    in
    let loc_size =
      Int_Base.inject
	(Int.length
	   (Cmdline.MinValidAbsoluteAddress.get ())
	   (Cmdline.MaxValidAbsoluteAddress.get ()))
    in
    if true (* TODO: command line option *)
    then
      state :=
	Relations_type.Model.add_binding
          ~with_alarms:warn_none_mode
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
  Value.update_table Kglobal result; (* stores the globals *)
(*  Format.printf "Initial state initially:@\n%a@."
    Relations_type.Model.pretty result; *)
  result



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
  { imprecision_tracing = Aignore ;
    others = Acall raise_predicate_alarm ;
    unspecified = Acall raise_predicate_alarm }

let rec reduce_by_predicate state positive p =
  let result =
    match positive,p.content with
    | true,Ptrue | false,Pfalse -> state
    | true,Pfalse | false,Ptrue -> Relations_type.Model.bottom
    | true,Pand (p1,p2 ) | false,Por(p1,p2)->
        reduce_by_predicate (reduce_by_predicate state positive p1) positive p2
    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
        Relations_type.Model.join
          (reduce_by_predicate state positive p1)
          (reduce_by_predicate state positive p2)
    | _,Pnot p -> reduce_by_predicate state (not positive) p
    | _,Piff (p1,p2 ) ->
        reduce_by_predicate state positive
	  (Logic_const.pand
              (Logic_const.pimplies(p1,p2),
              Logic_const.pimplies(p2,p1)))
    | _,Pxor(p1,p2) ->
	reduce_by_predicate state positive
	  (Logic_const.por
	     (Logic_const.pand (p1, Logic_const.pnot p2),
	      Logic_const.pand (Logic_const.pnot p1, p2)))
    | _,Prel (op,t1,t2) ->
        begin try
          let c1 = !Properties.Interp.term_to_exp t1 in
          let c2 = !Properties.Interp.term_to_exp t2 in
          let t = BinOp(lop_to_cop op, c1, c2, intType) in
          let state =
	    eval_cond ~with_alarms:warn_raise_mode
	      state { positive = positive ; exp = t }
          in
          state
        with
          Invalid_argument "not a lvalue" -> state
	| Reduce_to_bottom ->
	    Relations_type.Model.bottom
	      (* if the exception was obtained without an alarm emitted,
		 it is correct to return the bottom state *)
	| Predicate_alarm -> state
      end

    | _,Pvalid tsets ->
        begin try
          let exps = !Properties.Interp.tsets_to_exp tsets in
          List.fold_left
	    (fun state exp ->
               reduce_by_valid_expr ~with_alarms:warn_raise_mode ~positive
                 exp state) state exps
	  with Invalid_argument "not a lvalue" -> state
	  | Predicate_alarm -> state
	end

    | true,Pimplies (_,_) -> state

    | false,Pimplies (_,_) -> state

    | _,Papp _ (* | _,Pnamed _ *) | _,Pold _ | _,Pat _ -> state
    | _,Pexists (_varl, _p1) | _,Pforall (_varl, _p1) -> state
    | _,Pfresh _
    | _,Pvalid_range (_, _, _)| _,Pvalid_index (_, _)
    | _,Plet (_, _, _) | _,Pif (_, _, _)
    | _,Psubtype _
        -> state

  in
(*  Format.printf "reduce_by_predicate@\n%a@\nin state:@\n%a@\nresult:@\n%a@\n"
    d_predicate p
    Relations_type.Model.pretty state
    Relations_type.Model.pretty result;*)
  result

exception Does_not_improve

let reduce_by_disjunction states n p =
  if (State_set.length states) * (count_disjunction p) <= n
  then begin
      let treat_state state acc =
	let treat_pred pred acc =
	  let result = reduce_by_predicate state true pred in
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
	State_set.add (reduce_by_predicate state true p) acc)
      states
      State_set.empty

let eval_predicate state pred =
(*  Format.printf "eval_predicate:@\n%a@\nin state:@\n%a@\n"
    d_predicate pred
    State_set.pretty state; *)
  let rec do_eval state p =
    match p.content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
        begin match do_eval state p1 with
        | True -> do_eval state p2
        | False -> False
        | Unknown ->
	    begin match do_eval (reduce_by_predicate state true p1) p2 with
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
            begin match do_eval (reduce_by_predicate state false p1) p2 with
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
(*	  Format.printf "pvalid@."; *)
          let cexps = !Properties.Interp.tsets_to_exp tsets in
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
                       (*	    Format.printf "pvalid2@."; *)
                       let size = sizeof_pointed typ in
                       let loc = Locations.make_loc evaled size in
                       (*          Format.printf "Got size %a and value %a@\n"
                                   Int_Base.pretty size
                                   Location_Bits.pretty evaled; *)
                       if Locations.is_valid loc
	               then begin
                         (*Format.printf "pvalid->true@."; *)
		         True
	               end
	               else begin
	                 (*Format.printf "pvalid->unknown@.";*)
		         Unknown
		       end
	                 (* TODO: the else case can be improved
                            by distinguishing the locations made only
                            of invalid values (-> False)*)
                     else Unknown(*TODO: global arrays fall here *))
            True cexps
        with
            Invalid_argument "not a lvalue" -> Unknown
	  | Predicate_alarm -> Unknown
      end
    | Prel (op,t1,t2) ->
        begin
          try
            let cexp1 = !Properties.Interp.term_to_exp t1 in
            let cexp2 = !Properties.Interp.term_to_exp t2 in
            let cops =
              BinOp(lop_to_cop op,
                    cexp1,
                    cexp2,
                    intType)
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
            Invalid_argument "not a lvalue" -> Unknown
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
        begin match do_eval state p1 with
        | False -> True
        | True -> do_eval state p2
        | Unknown ->
            let state = reduce_by_predicate state true p1 in
            do_eval state p2
        end
    | Pfresh _
    | Pvalid_range (_, _, _)| Pvalid_index (_, _)
    | Plet (_, _, _) | Pif (_, _, _)
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


let check_postconditions header state behaviors =
  List.fold_left
    (fun state b ->
       if b.b_ensures = [] && b.b_assumes = [] then state
       else
         let vc = Ast_info.behavior_postcondition b in
         let res = eval_predicate state vc in
         CilE.warn_once "%s behavior %s: postcondition got status %s"
	   header b.b_name
	   (string_of_status res);
         match res with False -> State_set.empty
           | True | Unknown ->
             reduce_by_disjunction state
               (Cmdline.SemanticUnrollingLevel.get ())
               vc
    )
    state behaviors

let check_fct_postconditions kf state =
  (*  Value.pretty_state Format.std_formatter (snd state);*)
  try
    check_postconditions
      (Format.fprintf Format.str_formatter
         "Function %a," Kernel_function.pretty_name kf;
       Format.flush_str_formatter ())
      state (Kernel_function.get_spec kf).spec_behavior
  with Not_found -> state

let check_precondition kf state =
  (*  Value.pretty_state Format.std_formatter (snd state);*)
  let spec = (Kernel_function.get_spec kf).spec_requires in
  match spec with
  | [] -> state
  | _ ->
      let vc = Logic_const.pands
        (List.map Logic_const.pred_of_id_pred spec)
      in
      let result = eval_predicate (State_set.singleton state) vc in
      CilE.warn_once "Precondition of %a got status %s."
        Kernel_function.pretty_name kf (string_of_status result) ;
      reduce_by_predicate state true vc

let extract_valid_behaviors state behavior =
  List.filter
    (fun b ->
       let assumes = Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes) in
       match eval_predicate state assumes with
       | True | Unknown -> true
       | False -> false)
    behavior.spec_behavior

(* state before entering the given function *)
let valid_behaviors kf state =
  extract_valid_behaviors
    (State_set.singleton state)
    (Kernel_function.get_spec kf)

let () = Value.valid_behaviors := valid_behaviors


(* SEE eval_lval and do_assign to be consistent.
   Same match cases must exist in order to be precise.
   May raise [Lmap.Cannot_copy].
*)
let copy_offsetmap_from_virtual ~with_alarms loc1 lv2 loc2 (state:Relations_type.Model.t) =
    (* Format.printf "copy_offsetmap_from_virtual
       before:%a@\nloc1:%a@\nloc2:%a@\nlv2:%s@\n"
       Relations_type.Model.pretty state Location_Bits.pretty loc1.loc
       Location_Bits.pretty loc2.loc (Pretty.sprint ~width:80 (d_lval
       () lv2));*)
  if (not (Int_Base.equal loc1.size loc2.size))
    || (try
          ignore
	    (Location_Bits.cardinal_less_than loc2.loc
	       (Cmdline.ArrayPrecisionLevel.get ()));
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
    | Mem(Lval slv as e) when Cmdline.UseRelations.get () ->
	let sub_left_loc = lval_to_loc ~with_alarms state slv in
        begin try
	  let _, target_offset =
	    try (*TODO: with_alarms:false should be used ? *)
              eval_offset ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get())) ~with_alarms None (typeOf e) state target_offset
            with Offset_not_based_on_Null _ -> raise Lmap.Cannot_copy
	  in
	  let offsetmap =
	    Relations_type.Model.copy_from_virtual
	      sub_left_loc target_offset target_size state
	  in
	  (*Format.printf "copy_offsetmap_from_virtual:offsetmap:%a@\n"
	    (Cvalue_type.Model.LOffset.pretty None)
	    offsetmap;*)

(*          Relations_type.Model.paste_offsetmap offsetmap loc2.loc Int.zero target_size state*)
	  offsetmap
	with Relations_type.Use_Main_Memory ->
	  Cvalue_type.V_Offsetmap.empty
	end
    | Mem(BinOp((PlusPI|IndexPI|MinusPI as op),Lval slv,e2,_) as e)
        when Cmdline.UseRelations.get () ->
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
	  let _, target_offset = eval_offset
            ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
            ~with_alarms None typ state target_offset in
          let target_offset = Ival.add target_offset ival in
          (*Format.printf
            "copy_offsetmap_from_virtual: sub_left_loc:%a@\ntarget_offset:%a@\n"
            Location_Bits.pretty sub_left_loc.loc
            Ival.pretty target_offset;*)
	  let offsetmap =
            Relations_type.Model.copy_from_virtual sub_left_loc target_offset target_size state
          in
	  (*Format.printf "copy_offsetmap_from_virtual:offsetmap:%a@\n"
	    (Cvalue_type.V_Offsetmap.pretty None)
	    offsetmap;*)
	  offsetmap
	with Relations_type.Use_Main_Memory | Cvalue_type.V.Not_based_on_null ->
	  Cvalue_type.V_Offsetmap.empty
	end

    | _ ->
	 Cvalue_type.V_Offsetmap.empty
    in
(*   Format.printf "copy_offsetmap_from_virtual after:%a@\n"
      Relations_type.Model.pretty result;*)
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


module Computer (REACH:sig
                   val stmt_can_reach : stmt -> stmt -> bool
                   val is_natural_loop : stmt -> bool
                 end) = struct

  let name = "Values analysis"

  let stmt_can_reach = REACH.stmt_can_reach

  let debug = ref false

  type record =
      {
	superposition : State_set.t ;
	widening : int ;
	widening_state : Relations_type.Model.t ;
      }

  let empty_record =
    { superposition = State_set.empty ;
      widening = Cmdline.WideningLevel.get () ;
      widening_state = Relations_type.Model.bottom }

  let (current_table: record Cil.InstrHashtbl.t) =
    InstrHashtbl.create 1975

  let find_current kinstr =
    try
      InstrHashtbl.find current_table kinstr
    with Not_found -> empty_record

  let update_current_exn v =
    let kinstr = CilE.current_stmt () in
    assert (kinstr <> Kglobal);
    let old = find_current kinstr in
    let new_superposition =
(*      Format.printf "update_current %d %d@."
	(State_set.cardinal v)
	(State_set.cardinal old.superposition); *)
      State_set.merge_into v old.superposition in
    let new_widening = old.widening in
    (*	match old.widening with
	None -> None
	| Some w ->
	Some
	(State_set.fold
	(fun v acc -> Relations_type.Model.join v acc)
	v
	w)
    *)

      InstrHashtbl.replace current_table kinstr
	{ widening = new_widening ; superposition = new_superposition ;
	widening_state = old.widening_state }

  let update_current v =
    try
      update_current_exn v
    with State_set.Unchanged -> ()

  let merge_current ~degenerate =
(*    Format.printf "Merging current (%d)@." (InstrHashtbl.length current_table);*)
    let treat_instr k record =
      let sum = State_set.join record.superposition in
(*      Format.printf "Next instr:%d@\nState:%a@."
	(get_sid k)
	Relations_type.Model.pretty sum;
*)
      Value.update_table k sum
    in
    InstrHashtbl.iter treat_instr current_table;
    (*Format.printf "Done Merging current@.";*)

    if not degenerate &&
      not (Db.Value.Record_Value_Callbacks.is_empty ())
    then begin
(* 	Format.printf "[values] now calling Record_Value callbacks@."; *)

	let current_states = InstrHashtbl.create 17 in
	InstrHashtbl.iter
	  (fun k record ->
	    InstrHashtbl.add current_states k (State_set.join record.superposition))
	  current_table;

	Db.Value.Record_Value_Callbacks.apply
	  ((for_callbacks_stack ()), current_states);
      end;
    InstrHashtbl.clear current_table;

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
                   Format.printf "Statement (x%d): UNREACHABLE@\n"
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
    Widen.getWidenHints (List.hd !call_stack).called_kf s

  let counter_unroll_target = ref 100

  let combinePredecessors (s: stmt) ~old new_ =
(*        Format.printf "Combine: Sid=%d Counter_unroll = %d |old|=%d |new|=%d@\n"
	  s.sid
	  old.counter_unroll
	  (State_set.length !(old.value))
	  (State_set.length !(new_.value)); *)
    if (State_set.length !(new_.value)) = 0
    then None
    else begin
	if old.counter_unroll >= Cmdline.SemanticUnrollingLevel.get ()
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
	    (*
	    *)
	else (
	    (*   Format.printf "NEW:%d@\n%a@\nOLD:%d@\n%a@\n====@\n"
		 (State_set.cardinal !(new_.value))
		 display_one new_
		 (State_set.cardinal !(old.value))
		 display_one old; *)

	    begin try
		let merged =
		  (*	      Format.printf "combine %d %d@."
			      (State_set.cardinal !(new_.value))
			      (State_set.cardinal !(old.value)); *)
		  (State_set.merge_into !(new_.value) !(old.value) )
		in
		let new_counter_unroll =
		  old.counter_unroll + (State_set.cardinal !(new_.value)) in
		if new_counter_unroll >= !counter_unroll_target
		then begin
		    CilE.log_once
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
	  )
      end




  (** Precondition: the type of [exp] and the type [loc_lv] may be different only
      if the cast from [typeOf exp] and [typeOf loc_lv] is a truncation or an extension.
      This function will not perform any conversion (float->int, int->float, ...).
      [exp] should not be bottom for optimization purposes in the caller.
  *)
  let do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp =
    assert (not (Cvalue_type.V.is_bottom exp)); (* Or one may propagate bottoms too long. *)
    (*Format.printf "do_assign_abs in@\n%aAt %a<-%a@\n"
       Relations_type.Model.pretty state
       Locations.pretty loc_lv
      V.pretty exp;*)
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
	 (* Format.printf "do_assign: casting -> %a@\n" V.pretty exp; *)
	  exp
        with
        | V.Not_based_on_null (* from [find_ival] *) ->
            (* The exp is a pointer: check there are enough bits in
               the bitfield to contain it. *)
            if Int.compare size (Int.of_int (sizeofpointer ())) >= 0
              || V.is_top exp
            then exp
            else begin
              CilE.warn_once "casting address to a bitfield of %s bits: this is smaller than sizeof(void*)" (Int.to_string size);
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
	if not (Cmdline.Quiet.get ()) then
          Format.eprintf
            "State before degeneration:@\n======%a@\n=======@\n"
            Relations_type.Model.pretty state;
        CilE.warn_once
          "writing at a completely unknown address@[%a@].@\nAborting."
          pretty_org orig;
        do_degenerate (Some lv)

    | Location_Bits.Top((Location_Bits.Top_Param.Set _) as param,orig) ->
        CilE.warn_once
          "writing somewhere in @[%a@]@[%a@]."
          Location_Bits.Top_Param.pretty param
          pretty_org orig
    | Location_Bits.Map _ -> (* everything is normal *) ());
    let exact = valid_cardinal_zero_or_one loc_lv in
    let value =
      Relations_type.Model.add_binding ~with_alarms ~exact
	state loc_lv exp
    in
     (*Format.printf "do_assign_abs' in@\n%aAt %a<-%a (exact:%b)
       Leads to %a@\n"
       Relations_type.Model.pretty state
       Locations.pretty loc_lv
       V.pretty exp
       exact
       Relations_type.Model.pretty value;*)
   value

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
(*    Format.printf "do_assign_abstract_value: loc=%a@."
      Locations.pretty loc_lv;*)
    CilE.set_syntactic_context (CilE.SyMem lv);
    do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp

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
      if v <> Aignore
      then Acall set_alarm
      else Aignore
    in
    let warn_remember_mode =
      { imprecision_tracing = logger with_alarms.imprecision_tracing;
	others = with_alarms.others;
	unspecified = logger with_alarms.unspecified}
    in
    (*Format.printf "WITH_ALARMS:%a@." (fun fmt x -> match x with
                                      | Alog -> Format.fprintf fmt "Alog"
                                      | Aignore -> Format.fprintf fmt "Aignore"
                                      | Acall _ -> Format.fprintf fmt "ACall")
      warn_remember_mode.others;*)
    let evaled_exp =
      eval_expr ~with_alarms:warn_remember_mode old_state exp
    in
    (* Format.printf "do_assign %a@\n" V.pretty evaled_exp; *)
    let left_loc = lval_to_loc ~with_alarms old_state lv in
    let warn_right_exp_imprecision () =
      (match with_alarms.imprecision_tracing with
       | Aignore -> ()
       | Acall f -> f ()
       | Alog ->
           match evaled_exp with
           | Cvalue_type.V.Top(_topparam,origin) ->
               CilE.warn_once "assigning imprecise value to @[%a@]@[%t@]@[%a@]"
                 !Ast_printer.d_lval lv
                 (fun fmt -> match lv with
                  | (Mem _, _) -> Format.fprintf fmt " (i.e. %a)" Locations.pretty left_loc
                  | (Var _, _) -> ())
                 (fun fmt org ->
                    if not (Origin.is_top origin) then
                      Format.fprintf fmt ".@ The imprecision originates from %a" Origin.pretty org)
                 origin
           | Cvalue_type.V.Map _ ->
               if not (Got_Imprecise_Value.get ()) &&
	         not (Cvalue_type.V.cardinal_zero_or_one evaled_exp)
	       then begin
                 Got_Imprecise_Value.set true;
                 CilE.warn_once "assigning non deterministic value for the first time";
               end)
    in
    let old_state =
      match lv with
	Mem mem_e,NoOffset ->
	  let new_old_state =
	    reduce_by_valid_expr ~with_alarms ~positive:true mem_e old_state
	  in
	  if not (Relations_type.Model.is_reachable new_old_state)
	  then begin
	    CilE.set_syntactic_context (CilE.SyMem lv);
	    CilE.warn_mem_write with_alarms ;
	    CilE.warn_once
	      "all target addresses were invalid. This path is assumed to be dead.";
	  end;
	  new_old_state
            (*      | Var _ , Index _ -> assert false
                    TODO: do something for "TAB[i] = expr"
            *)
      | _ -> old_state
    in
    if Location_Bits.equal left_loc.loc Location_Bits.bottom  ||
      not (Relations_type.Model.is_reachable old_state)
    then Relations_type.Model.bottom
    else
      let default () =
        warn_right_exp_imprecision ();
	if get_alarm() then
	  (* log alarms that have not been logged the first time *)
          ignore (eval_expr ~with_alarms:{imprecision_tracing=with_alarms.imprecision_tracing;
                                          others=Aignore;
                                          unspecified=with_alarms.unspecified} old_state exp);

        (*Format.printf "do_assign ev_exp:%a@\n" Cvalue_type.V.pretty evaled_exp;*)
	CilE.set_syntactic_context (CilE.SyMem lv);
        if Cvalue_type.V.is_bottom evaled_exp then
          Relations_type.Model.bottom
        else
          do_assign_abstract_value_to_loc ~with_alarms
            old_state
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
            ~with_alarms:warn_none_mode
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
		let copy_paste_successed =
                  Relations_type.Model.paste_offsetmap
		    offsetmap left_loc.loc Int.zero size old_state
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
                copy_paste_successed
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
      if Cmdline.UseRelations.get ()
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
                     (lval_to_loc ~with_alarms:warn_none_mode old_state lv_right).loc)
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
	      | Mem(Lval slv as e),offs ->
                  let sub_left_loc =
		    lval_to_loc ~with_alarms old_state slv
		  in
                  if Location_Bits.cardinal_zero_or_one sub_left_loc.loc then
                    Relations_type.Model.add_mem
		      sub_left_loc
		      (sizeof_lval lv)
		      (try
			 snd (eval_offset
                                ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
                                ~with_alarms
				None (typeOf e) old_state offs)
		       with Offset_not_based_on_Null _ -> Ival.top)
		      new_main_memory_state
		      evaled_exp
                  else [],new_main_memory_state

	      | Mem(BinOp((PlusPI|IndexPI|MinusPI as op),Lval slv,e2,_) as e),
		  offs ->
                  let typ = typeOf e in
                  let e2 = eval_expr ~with_alarms old_state e2 in
                  begin try
		    let ival = Cvalue_type.V.find_ival e2 in
		    let ival = if op = MinusPI then Ival.neg ival else ival
		    in
		    let _, offs =
		      eval_offset
                        ~reduce_valid_index:(not (Cmdline.UnsafeArrays.get()))
                        ~with_alarms
			None typ old_state offs in
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
		      ( (*Format.printf "do_assign *(lval+e)@\n";*)
                        Relations_type.Model.add_mem
                          sub_left_loc
                          (sizeof_lval lv)
                          offs
                          new_main_memory_state
                          evaled_exp)
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
            (*Format.printf "do_assign(clean) %a@\n"
	      Relations_type.Model.pretty optimized_state_value;*)

            let rec optimize_list_lv l =
	      match l with
	        [] -> optimized_state_value
	      | (lvr,offset) :: tail ->
	          if Ival.is_singleton_int offset
	          then begin
                    (*			Format.printf "optimiz:%a@." Ival.pretty offset; *)
		    let locr = lval_to_loc ~with_alarms old_state lvr in
		    (*	      Format.printf "optimize_list_lv:%a@\n"
			      Locations.pretty locr;*)
		    if Location_Bits.cardinal_zero_or_one locr.loc
		    then
		      Relations_type.Model.add_equality ?offset:(Some (Ival.neg offset))
		        optimized_state_value left_loc locr
		    else optimize_list_lv tail
		  end
	          else optimize_list_lv tail
            in
            optimize_list_lv list_lv
      end
      else new_main_memory_state

  exception Got_bottom

  let reachables d =
    (not (State_set.is_empty !(d.value))),!(d.value)

  let doInstr stmt (i: instr) (d: t) =
    !Db.progress ();
    CilE.start_stmt (Kstmt stmt);
    let reachable,reachables = reachables d in
    let d_value = !(d.value) in

    let result =
      if (not reachable) then
        Dataflow.Done d
      else begin
        (*        Format.printf "DOINSTR NOW:%a@\n" display_one d; *)
        (* update current statement *)
        match i with
        | Set (lv,exp,_loc) ->
            Dataflow.Post
	      (fun _state ->
                 CilE.start_stmt (Kstmt stmt);
	         let result =
                   {
                     counter_unroll = 0;
		     value =
                       ref (State_set.fold
                              (fun state_value acc ->
                                 State_set.add
				   (do_assign ~with_alarms:warn_all_mode state_value lv exp) acc)
                              reachables
                              State_set.empty) }
                 in
                 CilE.end_stmt ();
                 result)
        | Call (None,Lval (Var {vname="__builtin_va_start"},NoOffset),[Lval lv],_loc) ->
            Dataflow.Post
	      (fun _state ->
                 CilE.start_stmt (Kstmt stmt);
	         let result =
                   {
                     counter_unroll = 0;
		     value =
                       ref (State_set.fold
                              (fun state_value acc ->
                                 State_set.add
				   (do_assign_abstract_value ~with_alarms:warn_all_mode ~former_state:state_value state_value lv Cvalue_type.V.top_int) acc)
                              reachables
                              State_set.empty) }
                 in
                 CilE.end_stmt ();
                 result)

        | Call (lval_to_assign,funcexp,argl,_loc) ->
            let call_site_loc = !currentLoc in
            Dataflow.Done
              {
                counter_unroll = 0;
                value = ref
                  (State_set.fold
                     (fun state acc ->
                        State_set.add
                          (try
	                     let _,functions =
                               resolv_func_vinfo ~with_alarms:warn_all_mode
				 None state funcexp
                             in
	                     let actuals =
		               List.map
		                 (fun e ->
                                    let v =
				      eval_expr ~with_alarms:warn_all_mode
					state e
				    in
		                    if V.equal v V.bottom
		                    then begin
                                      warn_once
				        "Evaluation of argument led to bottom in function call";
			              raise Got_bottom
		                    end;
		                    (e,v))
		                 argl
                             in
	                     let process_one_call func =
                               let caller =
                                 match !call_stack with
                                 | [] -> assert false
                                 | {called_kf=ckf }::_ -> ckf,stmt
                               in
		               Kf_state.add_caller func ~caller;
		               !Value.compute_call
		                 func
				 ~call_kinstr:(Kstmt stmt)
                                 state
                                 actuals
	                     in
	                     let return,new_state = List.fold_left
                               (fun (acc_rt,acc_res) f ->
		                  let return,result = process_one_call f in
		                  (*Format.printf "return:%a(%a)@."
		                    V.pretty return
		                    V.pretty acc_rt;*)
                                  currentLoc := call_site_loc;
		                  (match acc_rt,return with
				   | None,_ -> return
				   | Some _, None -> acc_rt
				   | Some acc_rt, Some return ->
				       Some (snd (V_Offsetmap.join
						    acc_rt
						    return))),
		                  Relations_type.Model.join acc_res result)
                               (None, Relations_type.Model.bottom)
                               functions
	                     in
	                     (*Format.printf "Before return: %a@."
		               Relations_type.Model.pretty new_state.value;*)
	                     match lval_to_assign with
	                     | None -> new_state
	                     | Some lv ->
                                 begin match return with
				 | Some return ->
				     let loc =
				       lval_to_loc
					 ~with_alarms:warn_all_mode new_state lv
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
					   ~with_alarms:warn_none_mode
					   Ival.zero
					   return
					   (Int.of_int (bitsSizeOf rtype))
					   Cvalue_type.V_Or_Uninitialized.bottom
				       in
                                       if not init
				       then warn_uninitialized warn_all_mode;
                                       if not no_esc
				       then warn_escapingaddr warn_all_mode;
                                       if Cvalue_type.V.is_bottom value
					 && not (init && no_esc)
				       then
                                           warn_once
                                             "Function call returned an unspecified value. This path is assumed to be dead.";

                                       let exact = valid_cardinal_zero_or_one loc in
				       Relations_type.Model.add_binding
					 ~with_alarms:warn_none_mode
					 ~exact
					 new_state
					 loc
					 (do_cast
					    ~with_alarms:warn_none_mode
					    lvtyp
					    value)
                                     in
				     if need_cast lvtyp rtype
				     then
				      default ()
				     else
				       (try
                                          Relations_type.Model.paste_offsetmap
					 return
					 loc.loc
					 Int.zero
					 (Int_Base.project loc.size)
					 new_state
                                        with Lmap.Cannot_copy -> default ())
		                 | None ->
                                     (if Relations_type.Model.is_reachable new_state
				      then
					CilE.warn_once
                                          "In function %t: called function returns void but returned value is assigned; ignoring assignment"
				          pretty_current_cfunction_name;
				      new_state)
				 end
	                   with
	                   | Ignore ->
                               currentLoc := call_site_loc;
                               state
                           | Got_bottom ->
                               currentLoc := call_site_loc;
                               Relations_type.Model.bottom
	                   | Leaf ->
                               currentLoc := call_site_loc;
                               (match lval_to_assign with
		                | None ->  state
		                | Some lv ->
                                    do_assign_abstract_value
				      ~with_alarms:warn_all_mode
                                      ~former_state:state
                                      state
                                      lv
				      (V.top_leaf_origin ())))
                          acc)
                     d_value
                     State_set.empty)}

        | Asm _ ->
	    CilE.warn_once
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
    let fold state pred =
      let result = eval_predicate state pred in
      CilE.warn_once
	        "Assertion got status %s."
	        (match result with
                 | Unknown -> "unknown"
                 | True -> "valid"
               | False -> "invalid (stopping propagation).");
      ( match result with
        | False -> State_set.empty
        | True | Unknown ->
	    reduce_by_disjunction
	      state
	      (Cmdline.SemanticUnrollingLevel.get ())
	      pred)
    in
    List.fold_left fold state ca

  let doStmt (s: stmt) (d: t) =
    let reachable, _ = reachables d in
    (*           Format.printf
                 "Processing stmt %d. Reachable:%b@\n" s.sid reachable;*)
    let kinstr = Kstmt s in
    CilE.start_stmt kinstr;
    let changed =
      try
        update_current_exn !(d.value);
	true
      with State_set.Unchanged -> false
    in
    CilE.end_stmt ();

    let annots_before,annots_after,contract = Db.Properties.predicates_on_stmt s
    in
    CilE.start_stmt kinstr;
    d.value := interp_annot !(d.value) annots_before;
    let valid_behaviors =
      match contract with
          None -> []
        | Some c -> extract_valid_behaviors !(d.value) c
    in
    CilE.end_stmt ();

    let states = !(d.value) in
    d.value := State_set.empty;

    if (not reachable) || (not changed) then
      Dataflow.SDefault
    else begin
	let current = find_current kinstr in
	let d =
	  if d.counter_unroll >= Cmdline.SemanticUnrollingLevel.get ()
	  then begin
	      let state = State_set.join states in
	      let joined =
		Relations_type.Model.join
		  current.widening_state
		  state
	      in
	      let r =
		if (REACH.is_natural_loop s) &&
		  (current.widening = 0)
		then
		  let wh_key_set, wh_hints = getWidenHints s in
		  let widen_hints =
                    true, wh_key_set (* no longer used thanks to 0/1 widening*),
                    wh_hints
		  in
		  let _,result = Relations_type.Model.widen
		    widen_hints
		    current.widening_state
		    joined
                  in
(*                  Format.printf "Widening at %d from @\n%a@\nAND@\n%a@\nTO%a@."
                    s.sid
                    Relations_type.Model.pretty current.widening_state
                    Relations_type.Model.pretty joined
                    Relations_type.Model.pretty result;*)
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
        d.value := interp_annot !(d.value) annots_after;
	d.value := check_postconditions "statement" !(d.value) valid_behaviors;
        update_current !(d.value);
        CilE.end_stmt ();
	(*	Format.printf "S====@\n%a@\nE====@\n" display_one d;*)
	match s.skind with
        | Return _ ->
            Dataflow.SUse d
        | Loop _ ->
            if d.counter_unroll >= Cmdline.SemanticUnrollingLevel.get () then
              CilE.warn_once "entering loop for the first time";
            Dataflow.SUse d
              (* | Switch (_, _, l, _) ->
                 Format.printf
                 "switch with %d successors.@\n"
                 (List.length s.succs) ;
                 Dataflow.SUse new_d *)
        | _ ->
            Dataflow.SUse d

      end

  let filterStmt _stmt = true

  (* Remove all local variables and formals from table *)
  let externalize return kf =
    match kf.fundec with
    | Declaration _ -> invalid_arg
	"externalize cannot be called on leaf functions"
    | Definition (fundec,_loc) ->
	assert (
	    StmtStartData.iter (fun k v ->
              if State_set.is_empty !(v.value)
              then ()
              else (
                  Format.printf "sid:%d@\n%a@\n"
                    k
                    State_set.pretty !(v.value);
                  assert false));
	    true);
	let superpos = (find_current return).superposition in
        let superpos = 	check_fct_postconditions kf superpos in
	let state = State_set.join superpos in
	Format.printf "[values] Recording results for %a@."
          Kernel_function.pretty_name kf;
	(*Format.printf "EXTERNALIZING(%d):%a@\n"
          (Ast_info.get_sid return) Relations_type.Model.pretty state;*)
	merge_current ~degenerate:false;
	(match return with
	| Kstmt {skind = Return (Some (Lval lv),_)} ->
	    CilE.set_syntactic_context (CilE.SyMem lv);
            let loc_to_read = lval_to_loc ~with_alarms:warn_all_mode state lv in
	    let result = (* [BM->PC] Lmap.Cannot_copy may be raised here? *)
	      try Relations_type.Model.copy_offsetmap
		loc_to_read state
              with Lmap.Cannot_copy -> assert false
	    in
            result
	| Kstmt {skind = Return (None,_)} -> None
	| _ -> assert false)
	  ,
	Relations_type.Model.clear_state_from_locals fundec state
(*
  let clear_for_function f =
    StmtStartData.clear ();
    IH.iter StmtStartData.add f
      (*    List.iter
            (fun s -> Inthash.remove stmtStartData s.sid)
      f.sbody.bstmts
      *)
*)
  let doGuard stmt exp t =
    if State_set.is_empty !(t.value)
    then Dataflow.GUnreachable
    else begin
	CilE.start_stmt (Kstmt stmt);
	let new_values =
	  State_set.fold
            (fun state acc ->
              let test = eval_expr ~with_alarms:warn_all_mode state exp in
              let do_it =
		let t1 = unrollType (typeOf exp) in
		if isIntegralType t1 || isPointerType t1
		then V.contains_non_zero test
		else true (* TODO: a float condition is true iff != 0.0 *)
	      in
              if do_it then
		try
		  State_set.add
		    (eval_cond ~with_alarms:warn_none_mode
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

let compute_using_cfg kf ~call_kinstr initial_state =
  match kf.fundec with
  | Declaration _ -> invalid_arg
      "compute_using_cfg cannot be called on leaf functions"
  | Definition (f,_loc) ->
      (*if let (_,_,variadic,_) = splitFunctionTypeVI f.svar in variadic
      then raise Leaf (* Do not visit variadic bodies *)
      else *)
      begin
      let module Computer =
        Computer(struct
		   let stmt_can_reach = Stmts_graph.stmt_can_reach kf
                   let is_natural_loop = Loop.is_natural kf
                 end)
      in
      let module Compute = Dataflow.ForwardsDataFlow(Computer) in
      List.iter
        (function {called_kf = g} ->
	   if kf == g
	   then begin
             error
	       "ignoring recursive call during value analysis of %a"
	       Kernel_function.pretty_name kf;
	     List.iter
	       (function {called_kf = kf } ->
		  error "call stack: %a"
		    Kernel_function.pretty_name kf;)
	       !call_stack;
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
          (*Format.eprintf
            "@[Return for %s is %a@]@."
            (Kernel_function.get_name kf) d_stmt ret_id; *)
          (* We start with only the start block *)
          Computer.StmtStartData.add
            start.sid
            (Computer.computeFirstPredecessor
               start
               {
                 Computer.counter_unroll = 0;
                 value = initial_state});
          begin try
            Compute.compute [start]
          with Value.Aborted as e ->
            (* Computation was aborted: pop the call stack and inform
	       the caller *)
(*	    Format.printf "should we pop the call stack?@."; *)
 	    pop_call_stack ();
            raise e
          end;
          let last_state =
            try
              (*CilE.warn_once "LAST STATE(%s)@." (Kernel_function.get_name kf);*)
              let (ret,st) =
		try
		  Computer.externalize (Kstmt ret_id) kf
		with Not_found -> assert false
	      in
	      let offsetmap_top_adresses_of_locals,
		state_top_adresses_of_locals =
		top_adresses_of_locals f
	      in
	      let result =
		(match ret with
		| None -> ret
		| Some ret ->
		    let r,warn = offsetmap_top_adresses_of_locals ret
		    in
		    if warn then warn_locals_escape_result f;
                  Some r),
		  state_top_adresses_of_locals st
		in
                if Relations_type.Model.is_reachable (snd result)
                then begin
                  if hasAttribute "noreturn" (Kernel_function.get_vi kf).vattr
                  then
                    CilE.warn_once
                      "function %a may terminate but has the noreturn attribute"
                      Kernel_function.pretty_name kf;
                  (*CilE.warn_once "END REACHED for (%s)@." (get_name kf);*)
                  Kf_state.mark_as_terminates kf
                end
                else begin
(*		    Format.printf "END NOT REACHED 1@."; *)
		    raise Not_found;
		  end;
                result
              with Not_found -> begin
(*                  Format.printf "END NOT REACHED 2 for (%s)@."
		    (Kernel_function.get_name kf);  *)
(
           (*     Computer.merge_current (); this may already have been
		  done by externalize, and should not be done twice
		  because the callbacks are done there.

		  TODO: examine the usefulness of this statement
                 (* Save the values computed even
                    if the function does not terminate *) *)

		 Kf_state.mark_as_never_terminates kf);
                (* Computer.display
                  Format.std_formatter
                  Computer.stmtStartData;*)
                None,
                Relations_type.Model.bottom
              end
            in
            if Cmdline.Debug.get () > 0
	    then begin
                Format.printf "@\n@[RESULT FOR %a%s:@\n\\result -> %a@\n%a@]"
                  Kernel_function.pretty_name kf
                  (let s = ref "" in
                   List.iter
                     (function {called_kf = kf} -> s := !s^" <-"^
                        (fprintf_to_string "%a" Kernel_function.pretty_name kf))
                     !call_stack;
                   !s)
                  (fun fmt v ->
		     match v with
		       | None -> ()
		       | Some v -> V_Offsetmap.pretty fmt v)
		  (fst last_state)
                  Relations_type.Model.pretty (snd last_state)
              end;
	  pop_call_stack ();


            last_state
      end

(** Associates [kernel_function] to a fresh base for the adress returned by
    the [kernel_function]. *)
module Leaf_Table =
  Kernel_function.Make_Table
    (Base.Datatype)
    (struct
       let dependencies = [Value.self]
       let size = 7
       let name = Project.Computation.Name.make "Leaf_Table"
     end)

let return_value return_type kf state =
  (* Process return of function *)
  let return_type = unrollType return_type in
  match return_type with
  | TComp _ when is_fully_arithmetic return_type -> Cvalue_type.V.top_int, state
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
             let new_offsetmap = Cvalue_type.V_Offsetmap.sized_zero (memory_size ()) in
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
      (*Format.printf "Got new_state:%a@\n"
        Relations_type.Model.pretty new_state;*)
      returned_loc, state
    end
  | TInt _ | TEnum _ ->  Cvalue_type.V.top_int, state
  | TFloat _ ->  Cvalue_type.V.top_float, state
  | TVoid _ -> Cvalue_type.V.top (* this value will never be used *), state
  | TFun _ | TNamed _ | TArray _ | TBuiltin_va_list _ -> assert false


let compute_using_prototype kf  ~state_with_formals =
  match kf.fundec with
  | Definition (_,_) -> assert false
  | Declaration (_spec,varinfo,_,_) ->
      if Cil.hasAttribute "noreturn" varinfo.vattr then
        None, Relations_type.Model.bottom
      else begin
	let return_type,_formals_type,_inline,_attr =
	  splitFunctionType (Kernel_function.get_type kf)
	in
        let behaviors = valid_behaviors kf state_with_formals in
	let assigns = Ast_info.merge_assigns behaviors in
	let returned_value, state_with_formals =
	  return_value return_type kf state_with_formals
	in
	let returned_value = ref returned_value in
	let state =
	  match assigns with
          | [] -> state_with_formals
          | assigns -> (*VP: same behavior as before, but it is weird:
                         \from \nothing has the
                         same meaning as unspecified \from...
                        *)
	      let treat_assign acc (out, ins) =
		let input =
                  (try
                     List.fold_left
		       (fun acc loc ->
                          List.fold_left
                            (fun acc lv ->
			       Cvalue_type.V.join acc
                                 (snd
                                    (eval_lval ~with_alarms:warn_none_mode None
                                       state_with_formals lv)))
                            acc
                            (match loc with
                                 Location loc ->
                                   !Properties.Interp.tsets_to_lval
                                     loc.its_content
                               | Nothing -> []
                            ))
		       Cvalue_type.V.top_int ins
                   with Invalid_argument "not a lvalue" ->
                     CilE.warn_once
                       "Can not interpret assigns in function %a"
                       Kernel_function.pretty_name kf;
                     Cvalue_type.V.top
                  )
		in
		try
                  List.fold_left
                    (fun acc lval ->
		       let loc =
		         lval_to_loc  ~with_alarms:warn_none_mode
			   state_with_formals lval
		       in
		       let bound =
		         Relations_type.Model.add_binding ~with_alarms:warn_none_mode
			   ~exact:false acc loc input
		       in
		       Relations_type.Model.join bound acc)
                    acc (
                      match out with
                          Location out ->
                            !Properties.Interp.tsets_to_lval out.its_content
                        | Nothing -> []
                    )
		with
		  Invalid_argument "not a lvalue" as e ->
		    (match out with
                         Location out when
                           Logic_const.tsets_is_result out.its_content ->
		             returned_value :=
			       Cvalue_type.V.join
			         (Cvalue_type.V.topify_arith_origin input)
			         ! returned_value;
		             acc
                       | _ -> raise e)
	      in
	      (List.fold_left treat_assign state_with_formals assigns)
	in
	  (if isVoidType return_type then None else
	     let offsetmap =
	       V_Offsetmap.update_ival
		 ~with_alarms:warn_none_mode
		 ~validity:Base.All
		 ~offsets:Ival.zero
		 ~exact:true
		 ~size:(Int.of_int (bitsSizeOf return_type))
		 V_Offsetmap.empty
		 (Cvalue_type.V_Or_Uninitialized.initialized !returned_value)
	     in
	       Some offsetmap),
	(Relations_type.Model.filter_base
	   (fun base -> not (Base.is_formal_of_prototype base varinfo))
	   state)
      end

(* Replace in [initial_state] all keys in [mem_outs] by their value in
  [mem_final_state]. *)
let compute_using_mem
    _kf
    (initial_state:Relations_type.Model.t)
    (new_return_v,mem_final_state)
    mem_outs
    instanciation =
  new_return_v,
let r =
  Relations_type.Model.compute_actual_final_from_generic
    initial_state
    mem_final_state
    mem_outs
    instanciation
in
(*Format.printf "debugging compute_using_mem initial_state:%a final:%a@."
  Relations_type.Model.pretty initial_state
  Relations_type.Model.pretty r; *)
r

(** Compute only once the initial values for globals and NULL *)
let initial_state_contextfree_only_globals =
  let module S =
    Computation.OptionRef
      (Relations_type.Model.Datatype)
      (struct
	 let name = Project.Computation.Name.make "contextfree_only_globals"
	 let dependencies =
	   [ Cil_state.self; Cmdline.LibEntry.self; Cmdline.MainFunction.self ]
       end)
  in
  function () ->
    let compute ()  =
      let computed_state = ref (Value.globals_state ()) in
      Globals.Vars.iter
	(fun varinfo _init ->
           currentLoc := varinfo.vdecl;
	   computed_state :=
	     initialize_var_using_type
	       varinfo
	       !computed_state);
      Value.update_table Kglobal !computed_state;
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
  | [],_ -> CilE.warn_once "not enough arguments in function call."; acc
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals kf state actuals =
  let formals = Kernel_function.get_formals kf in
  (*List.iter (fun vi -> Format.printf "%s vid:%d@." vi.vname vi.vid) formals;*)
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
       Relations_type.Model.add_binding ~with_alarms:warn_none_mode ~exact:true acc loc actual)
    state
    actuals
    formals

(* In the state [initial_state] globals and formals are present
   but locals of [kf] are not.*)
let compute_with_initial_state kf initial_state =
  match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
        let initial_state = check_precondition kf initial_state in
        let initial_state =
	  List.fold_left
	    (fun acc local ->
               Relations_type.Model.add_binding_unspecified
                 acc
                 (Locations.loc_of_varinfo local))
	    initial_state
	    f.slocals
        in
	compute_using_cfg kf (ref (State_set.singleton initial_state))

let compute_entry_point kf ~library =
  Kf_state.mark_as_called kf;
  Format.printf "[values] computing for function %a@."
    Kernel_function.pretty_name kf;
  Format.printf "====== INITIAL STATE ======@.";
  let initial_state_globals = if library then
    initial_state_contextfree_only_globals ()
  else
    Value.globals_state ()
  in
  Format.printf "====== INITIAL STATE COMPUTED ======@.";
  if not (Cmdline.Quiet.get ()) then
    begin
      Format.printf "@[<hov 0>Values of globals at initialization @\n";
      Value.pretty_state_without_null
	Format.std_formatter initial_state_globals;
      Format.printf "@]@\n";
    end;
  let with_formals = initial_state_formals kf initial_state_globals in
  Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
(*  Format.printf "Now calling Call_Value callbacks for entry point@."; *)
  let result =
    compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
  in
  Format.printf "[values] done for function %a@."
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
       let dependencies = [Value.self]
       let size = 79
       let name = Project.Computation.Name.make "Dynamic_Alloc_Table"
     end)

module Mem_Exec =
  Kernel_function.Make_Table
    (Project.Datatype.Register
       (struct
	  module V_Offsetmap_option = Datatype.Option(V_Offsetmap.Datatype)
	  type t =
	      Relations_type.Model.t
	      * (V_Offsetmap_option.t * Relations_type.Model.t)
	      * Locations.Zone.t (* in *)
	      * Locations.Zone.t (* out *)
	  let copy _ = assert false (* TODO: deep copy *)
	  let rehash (generic_state, (result, result_state), ins, outs) =
	    Relations_type.Model.Datatype.rehash generic_state,
	    (V_Offsetmap_option.rehash result,
	    Relations_type.Model.Datatype.rehash result_state),
	    Locations.Zone.Datatype.rehash ins,
	    Locations.Zone.Datatype.rehash outs
	  include Datatype.Nop
	  let name = Project.Datatype.Name.make "Mem_Exec_tuple"
	  let dependencies =
	    [ Relations_type.Model.Datatype.self;
	      Cvalue_type.V.Datatype.self;
	      V_Offsetmap_option.self;
	      Locations.Zone.Datatype.self ]
	end))
    (struct
       let name = Project.Computation.Name.make "Mem_Exec"
       let size = 7
       let dependencies =
	 [ Cil_state.self; Cmdline.LibEntry.self; Cmdline.MainFunction.self ]
     end)

exception Not_found_lonely_key
exception Found_misaligned_base

let wrap_int i =
  Some
    (V_Offsetmap.update_ival
        ~with_alarms:warn_none_mode
        ~validity:Base.All
        ~offsets:Ival.zero
        ~exact:true
        ~size:(Int.of_int (bitsSizeOf intType))
        V_Offsetmap.empty
        (V_Or_Uninitialized.initialized i))

let wrap_double i =
  Some
    (V_Offsetmap.update_ival
        ~with_alarms:warn_none_mode
        ~validity:Base.All
        ~offsets:Ival.zero
        ~exact:true
        ~size:(Int.of_int (bitsSizeOf doubleType))
        V_Offsetmap.empty
        (V_Or_Uninitialized.initialized i))

let wrap_ptr i =
  Some (V_Offsetmap.update_ival
    ~with_alarms:warn_none_mode
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
  Value.merge_initial_state kf with_formals;
  let stack_without_call = for_callbacks_stack () in
  (*  Format.printf "Now calling Call_Value callbacks@."; *)
  Db.Value.Call_Value_Callbacks.apply
    (with_formals, ((kf, call_kinstr) :: stack_without_call));
  let name = Kernel_function.get_name kf in
  (* function whose name starts with 'CEA_'
     print their arguments on stdout during computations.*)
  if Ast_info.is_cea_dump_function name then begin
    Format.printf "DUMPING STATE of file %s line %d@\n%a=END OF DUMP==@."
      (fst !currentLoc).Lexing.pos_fname
      (fst !currentLoc).Lexing.pos_lnum
      Relations_type.Model.pretty initial_state;
    None,initial_state
  end else if Ast_info.is_frama_c_base_aligned name then
    try begin
      match actuals with
	[_,x; _,y] ->
	  let i = Cvalue_type.V.find_ival y in
	  begin match i with
	    Ival.Set si ->
	      Location_Bytes.fold_i
		(fun b _o () ->
		   Ival.O.iter
		     (fun int ->
			if not (Base.is_aligned_by b int)
			then raise Found_misaligned_base)
		     si)
		x
		();
	      (wrap_int Cvalue_type.V.singleton_one), initial_state
	  | _ -> raise Found_misaligned_base
	  end
      | _ -> raise Invalid_CEA_alloc
    end
    with Invalid_CEA_alloc ->
      ignore (Errormsg.error
		"Invalid arguments for Frama_C_is_base_aligned function\n");
      flush !Errormsg.logChannel;
      do_degenerate None;
      raise Value.Aborted
    | Found_misaligned_base
    | Not_found (* from find_ival *) ->
	(wrap_int Cvalue_type.V.zero_or_one), initial_state

  else if Ast_info.is_cea_offset name then
    try begin
      match actuals with
	[_,x] ->
          begin
	    let value =
	      try
		let offsets =
		  Location_Bytes.fold_i
		    (fun _b o a -> Ival.join a o)
		    x
		    Ival.bottom
		in
		Cvalue_type.V.inject_ival offsets
	      with Location_Bytes.Error_Top ->
		error
		  "The builtin %a is applied to a value that is not guaranteed \
                 to be an address."
                  Kernel_function.pretty_name kf;
		Cvalue_type.V.top_int
	    in
	    (wrap_int value), initial_state
          end
      | _ -> raise Invalid_CEA_alloc
    end
    with Invalid_CEA_alloc ->
      ignore (Errormsg.error
		"Invalid arguments for Frama_C_offset function\n");
      flush !Errormsg.logChannel;
      do_degenerate None;
      raise Value.Aborted
  else if Ast_info.is_cea_alloc name then begin
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
	       if String.length file >= 7 && String.sub file 0 6 = "alloc_"
	       then file
	       else Format.sprintf "alloc_%s" file
             in
	     let new_name = Cabs2cil.fresh_global new_name in
             let unbounded_type =
	       TArray(intType,Some (Const (CStr "NOSIZE")),[])
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
      wrap_ptr loc, initial_state
    with
    | Ival.Error_Top | Invalid_CEA_alloc
    | Not_found_lonely_key (* from [find_lonely_key] *)
      -> CilE.warn_once
        "Invalid argument for Frama_C_alloc_infinite function\n";
        do_degenerate None;
        raise Value.Aborted
    | Not_found -> assert false
  end
  else if Ast_info.is_cea_alloc_with_validity name then begin
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
        Format.sprintf "alloc"
      in
      let new_name = Cabs2cil.fresh_global new_name in
      let bounded_type =
	TArray(charType,Some (Const (CInt64 (Int.to_int64 size,IInt ,None) )),
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
      (wrap_ptr loc_without_size),initial_state
    with Ival.Error_Top | Invalid_CEA_alloc
    | Not_found (* from [find_lonely_key]*)
      -> CilE.warn_once
        "Invalid argument for Frama_C_alloc_size function\n";
        do_degenerate None;
        raise Value.Aborted
  end else if Ast_info.is_cea_function name then begin
    List.iter
      (fun (_,e) ->
	 Format.printf "Argument of %s: %a@."
	   name
	   V.pretty
	   e)
      actuals;
    None,initial_state
  end
  else if name = "Frama_C_sqrt"
  then begin
    match actuals with
      [_, arg] -> begin
	let r =
	  try
	    let i = Cvalue_type.V.find_ival arg in
	    let f = Ival.project_float i in
	    Cvalue_type.V.inject_ival
	      (Ival.inject_float (Ival.Float_abstract.sqrt_float f))
	  with Cvalue_type.V.Not_based_on_null ->
	    CilE.warn_once "float sqrt applied to address";
	    Cvalue_type.V.topify_arith_origin arg
	in
	(wrap_double r), initial_state
      end
    | _ -> CilE.warn_once
        "Invalid argument for Frama_C_sqrt function\n";
        do_degenerate None;
        raise Value.Aborted
  end
  else if name = "Frama_C_cos"
  then begin
    match actuals with
      [_, arg] -> begin
	let r =
	  try
	    let i = Cvalue_type.V.find_ival arg in
	    let f = Ival.project_float i in
	    Cvalue_type.V.inject_ival
	      (Ival.inject_float (Ival.Float_abstract.cos_float f))
	  with Cvalue_type.V.Not_based_on_null ->
	    CilE.warn_once "float cos applied to address";
	    Cvalue_type.V.topify_arith_origin arg
	in
	(wrap_double r), initial_state
      end
    | _ -> CilE.warn_once
        "Invalid argument for Frama_C_cos function\n";
        do_degenerate None;
        raise Value.Aborted
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
          None,
          copy_paste_locations
            ~with_alarms:warn_all_mode
            ~exp_lv
            ~left:(loc_bytes_to_loc_bits dst)
            ~right:(loc_bytes_to_loc_bits src)
            size
            initial_state
	with
          Ival.Not_Singleton_Int | V.Not_based_on_null | Lmap.Cannot_copy ->
            CilE.warn_once
              "Invalid call to Frama_C_memcpy function(%a, %a, %a)\n"
	      Cvalue_type.V.pretty dst
	      Cvalue_type.V.pretty src
	      Cvalue_type.V.pretty size;
            do_degenerate None;
            raise Value.Aborted
        end
    | _ -> CilE.warn_once
        "Invalid argument for Frama_C_memcpy function\n";
        do_degenerate None;
        raise Value.Aborted
  end
  else begin
    Format.printf "[values] computing for function %a%s@."
      Kernel_function.pretty_name kf
      (let s = ref "" in
       List.iter
         (function {called_kf = kf} -> s := !s^" <-"^
            (fprintf_to_string "%a" Kernel_function.pretty_name kf))
         !call_stack;
       !s);
    Format.printf "[values] called from %a@." pretty_loc_simply
      (CilE.current_stmt());

    flush !Errormsg.logChannel;
    Kf_state.mark_as_called kf;
    let modular =
      Cmdline.MemExecAll.get ()
      || Cilutil.StringSet.mem name (Cmdline.MemFunctions.get ())
    in
    let result =
      match kf.fundec with
      | Definition _ ->
          begin try
            if not modular then raise Not_modular;
            let mem_initial_state, mem_final_state, mem_in, mem_outs =
              !Value.memoize kf;
	      try Mem_Exec.find kf with Not_found -> raise Not_modular
            in
            try
	      let instanciation =
		Relations_type.Model.is_included_actual_generic
		  (Zone.join mem_in mem_outs)
		  with_formals
                  mem_initial_state
	      in
	      Format.printf "Instanciation succeeded: %a@\n"
		(BaseUtils.BaseMap.pretty Location_Bytes.pretty)
		instanciation;
              compute_using_mem kf
                initial_state
                mem_final_state
                mem_outs
		instanciation
	    with Is_not_included ->
              CilE.warn_once
                "Failed to see context as an instance of the generic context: inlining call to %a."
                Kernel_function.pretty_name kf;
              raise Not_modular
	  with Not_modular ->
            compute_with_initial_state kf ~call_kinstr with_formals
          end
      | Declaration _ ->
	  let r = compute_using_prototype kf ~state_with_formals:with_formals in
	  r
    in
    Format.printf "[values] done for function %a@."
      Kernel_function.pretty_name kf;
    result
  end

let memoize kf =
  try
    ignore
      (Mem_Exec.memo
	 (fun kf ->
	    Kf_state.mark_as_called kf;
	    let with_globals = initial_state_contextfree_only_globals () in
	    let with_formals = initial_state_formals kf with_globals in
	    let result =
	      compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
	    in
	    let ins =
	      (!InOutContext.get_internal kf).Inout_type.over_inputs
	    in
	    let outs = !Outputs.get_external kf in
	    with_formals,result,ins,outs)
	 kf)
  with Value.Aborted ->
    (* the function will not be memoized. TODO: inform the user
       that the analyzer will behave as if the option was not set *)
    ()

let compute, _ =
  Computation.apply_once
    (Project.Computation.Name.make "Eval.compute")
    [ Value.self ]
    (fun () ->
       try
	 let kf, library = Globals.entry_point () in
	 ignore (compute_entry_point kf ~library);
	 (* Move all alarms to Db *)
	 Properties.synchronize_alarms ();
	 Value.mark_as_computed ();
       with
       | Value.Aborted ->
	   Value.mark_as_computed ();
	   Format.printf
	     "Degeneration occured:@\nresults are not correct for lines of code that can be reached from the degeneration point.@.";
	   exit 1
       | exn -> Value.mark_as_computed (); raise exn)

let () = Value.compute := compute
let () = Value.compute_call := compute_call
let () = Value.memoize := memoize
let () = Value.initial_state_only_globals := initial_state_only_globals

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

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

type ('a, 'b) alarm_gen = 
    remove_trivial:bool -> warning:bool -> kernel_function -> kinstr -> 'a -> 'b

let annotations = ref []
let generated_annotations () = !annotations
let reset_generated_annotations () = annotations := []

let save_alarms = ref true

let register_alarm e kf ki ?status alarm = 
  let a, _ = Alarms.register e ~kf ki ?status ~save:!save_alarms alarm in
  annotations := a :: !annotations;
  a

(* [JS 2012/10/17] pretty printing hack to preserve previous behavior
   which does not display labels of generated assertions. *)
let local_printer: Printer.extensible_printer = object (self)
  inherit Printer.extensible_printer ()
  method! code_annotation fmt ca = match ca.annot_content with
  | AAssert(_, p) ->  
    (* ignore the name *) 
    Format.fprintf fmt "%a" self#predicate p.content
  | _ -> assert false
end 

(* Tries to evaluate expr as a constant value (Int64.t).
   Uses Cil constant folding (e.g. for (-0x7ffffff -1) => Some (-2147483648)) on
   32 bits *)
let get_expr_val expr =
  let cexpr = Cil.constFold true expr in
  match cexpr.enode with
  | Const c ->
    let rec get_constant_expr_val e =
      match e with
      | CChr c -> get_constant_expr_val (Cil.charConstToInt c)
      | CInt64 (d64,_,_) -> Some d64
      | _ -> None
    in 
    get_constant_expr_val c
  | _ -> 
    None

(* Creates [0 <= e] and [e < size] assertions *)
let valid_index ~remove_trivial kf kinstr e size =
  let emit left =
    ignore
      (register_alarm
	 Generator.emitter
	 kf 
	 kinstr
	 (Alarms.Index_out_of_bound(e, if left then None else Some size)))
  in
  if remove_trivial then begin
    (* See if the two assertions do not trivially hold. In this
       case, do not return then *)
    let v_e = get_expr_val e in
    let v_size = get_expr_val size in
    let neg_ok =
      Extlib.may_map ~dft:false (Integer.le Integer.zero) v_e 
      || Cil.isUnsignedInteger (Cil.typeOf e)
    in
    if not neg_ok then emit true;
    let pos_ok = match v_e, v_size with
      | Some v_e, Some v_size -> Integer.lt v_e v_size
      | None, _ | _, None -> false
    in
    if not pos_ok then emit false
  end else begin
    emit true;
    emit false
  end

(* returns the assertion associated with an lvalue:
   returns non empty assertions only on pointer dereferencing and array access.
   Dereferencing a function pointer generates no assertion (but a warning
   is emitted). The validity assertions are emitted using [valid] if
   [~read_only] is false, or with [valid_read] otherwise *)
let lval_assertion ~read_only ~remove_trivial ~warning:_ kf kinstr lv =
  (* For accesses to known arrays we generate an assertions that constrains
     the index. This is simpler than the [\valid] assertion *)
  let rec check_array_access default off typ in_struct = 
    match off with
    | NoOffset -> 
      if default then 
	ignore
	  (register_alarm Generator.emitter kf kinstr
	     (Alarms.Memory_access(lv, read_only)))
    | Field (fi, off) ->
      (* Mark that we went through a struct field, then recurse *)
      check_array_access default off fi.ftype true
    | Index (e, off) ->
      match Cil.unrollType typ with
      | TArray (bt, Some size, _, _) ->
        if Kernel.SafeArrays.get () || not in_struct then begin
          (* Generate an assertion for this access, then go deeper in
             case other accesses exist *)
          valid_index kf kinstr e size ~remove_trivial;
          check_array_access default off bt in_struct
        end else
          (* Access to an array embedded in a struct with option
             [-unsafe-arrays]. Honor the option and generate only
             the default [\valid] assertion *)
          check_array_access true off bt in_struct
      | TArray (bt, None, _, _) -> check_array_access true off bt in_struct
      | _ -> assert false
  in
  match lv with		
  | Var vi , off -> check_array_access false off vi.vtype false
  | Mem _exp as lh, off ->
    let dft = 
      if Cil.isFunctionType (Cil.typeOfLval lv) then begin
	Options.warn
	  "no predicate available yet to check validity of function pointer \
 dereferencing %a"
	  Printer.pp_lval lv;
	false
      end else
	true
    in
    check_array_access dft off (Cil.typeOfLhost lh) false

(* assertion for unary minus signed overflow *)
let uminus_assertion ~remove_trivial ~warning kf kinstr exp =
  (* - expr overflows if exp is TYPE_MIN *)
  let t = Cil.unrollType (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  let min_ty = Cil.min_signed_number size in
  (* alarm is bound <= exp, hence bound must be MIN_INT+1 *)
  let bound = Integer.add Integer.one min_ty in
  let assertion ?status () =
    register_alarm
      Generator.emitter ?status kf kinstr
      (Alarms.Overflow(Alarms.Signed, exp, bound, Alarms.Lower_bound))
  in
    if remove_trivial then begin
      match get_expr_val exp with
      | None -> ignore (assertion ())
      | Some a64 -> 
	(* constant operand *)
        if Integer.equal a64 min_ty then
          let a = assertion ~status:Property_status.False_if_reachable () in
          if warning then
	    Options.warn "unary minus assert broken: %a" 
	      local_printer#code_annotation a
    end

(* assertions for multiplication/addition/subtraction signed overflow *)
let mult_sub_add_assertion
    ~remove_trivial ~warning kf kinstr (signed, exp, op, lexp, rexp) =
  (* signed multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than TYPE_MAX or strictly less than TYPE_MIN *)
  let t = Cil.unrollType (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  let min_ty, max_ty = 
    if signed then Cil.min_signed_number size, Cil.max_signed_number size
    else Integer.zero, Cil.max_unsigned_number size 
  in
  let assertion ?status up = 
    let bound, b = 
      if up then max_ty, Alarms.Upper_bound else min_ty, Alarms.Lower_bound 
    in
    register_alarm
      Generator.emitter ?status kf kinstr
      (Alarms.Overflow
	 ((if signed then Alarms.Signed else Alarms.Unsigned), exp, bound, b))
  in
  let full_assertion () = 
    ignore (assertion false);
    ignore (assertion true)
  in
  if remove_trivial then
    match get_expr_val lexp, get_expr_val rexp with
      | None, None -> full_assertion ()
      | Some a64, None | None, Some a64 ->  
	(* one operand is constant *)
	(match op with
	  | MinusA -> ignore (assertion false)
	  | PlusA -> ignore (assertion true)
	  | Mult ->
          (* multiplying by 1 or 0 is not dangerous *)
            if not (Integer.equal a64 Integer.zero ||
		      Integer.equal a64 Integer.one)
            then
            (* multiplying by -1 is dangerous (albeit seldom) *)
              if Integer.equal a64 Integer.minus_one then
		ignore (assertion false)
              else 
		full_assertion ()
	  | _ -> assert false)
      | Some big_a64, Some big_b64 -> 
	let warn up =
	  let status = Property_status.False_if_reachable in
          let a = assertion ~status up in
          if warning then
	    Options.warn "%s overflow assert broken: %a" 
	      (if signed then "signed" else "unsigned")
	      local_printer#code_annotation a
	in
	(* both operands are constant *)
        (match op with
	  | MinusA ->
            let big_diff = Integer.sub big_a64 big_b64 in
	    if Integer.lt big_diff min_ty then warn false
	  | PlusA ->
            let big_add = Integer.add big_a64 big_b64 in
	    if Integer.gt big_add max_ty then warn true
	  | Mult ->
            let big_mult = Integer.mul big_a64 big_b64 in
            if Integer.gt big_mult max_ty then warn true
            else if Integer.lt big_mult min_ty then warn false
	  | _ -> assert false)
  else
    full_assertion ()

(* assertions for division and modulo (divisor is 0) *)
let divmod_assertion ~remove_trivial ~warning kf kinstr divisor =
  (* division or modulo: overflow occurs when divisor is equal to zero *)
  let assertion ?status () =
    register_alarm
      Generator.emitter ?status kf kinstr (Alarms.Division_by_zero divisor)
  in
  if remove_trivial then
    (match get_expr_val divisor with
    | None -> 
      (* divisor is not a constant (or it's value has not
         been computed) *)
      ignore (assertion ());
    | Some v64 ->
      if Integer.equal v64 Integer.zero then
        (* divide by 0 *)
	let a = assertion ~status:Property_status.False_if_reachable () in
        if warning then 
	  Options.warn "divisor assert broken: %a" 
	    local_printer#code_annotation a;
      (* else divide by constant which is not 0: nothing to assert *))
  else
    ignore (assertion ())

(* assertion for signed division overflow *)
let signed_div_assertion ~remove_trivial ~warning kf kinstr (exp, lexp, rexp) =
  (* Signed division: overflow occurs when dividend is equal to the
     the minimum (negative) value for the signed integer type,
     and divisor is equal to -1. Under the hypothesis (cf Value) that
     integers are represented in two's completement.
     Nothing done for modulo (the result of TYPE_MIN % -1 is 0, which does not
     overflow).
     Still it may be dangerous on a number of compilers / architectures
     (modulo may be performed in parallel with division) *)
  let t = Cil.unrollType (Cil.typeOf rexp) in
  let size = Cil.bitsSizeOf t in
  (* check dividend_expr / divisor_expr : if constants ... *)
  (* compute smallest representable "size bits" (signed) integer *)
  let max_ty = Cil.max_signed_number size in
  let emit ?status () =
    register_alarm ?status Generator.emitter kf kinstr
      (Alarms.Overflow(Alarms.Signed, exp, max_ty, Alarms.Upper_bound))
  in
  if remove_trivial then
    let min = Cil.min_signed_number size in
    match get_expr_val lexp, get_expr_val rexp with
    | Some e1, _ when not (Integer.equal e1 min) ->
	(* divident is constant, with an unproblematic value *)
      ()
    | _, Some e2 when not (Integer.equal e2 Integer.minus_one) ->
	(* divisor is constant, with an unproblematic value *)
      ()
    | Some _, Some _ ->
	(* invalid constant division *)
      let a = emit ~status:Property_status.False_if_reachable () in
      if warning then
	Options.warn "signed overflow assert broken: %a" 
	  local_printer#code_annotation a;
    | None, Some _ | Some _, None | None, None -> 
	(* at least one is not constant: cannot conclude *)
      ignore (emit ())
  else 
    ignore (emit ())

let shift_alarm ~remove_trivial ~warning kf kinstr (exp, upper_bound) =
  let alarm ?status () =
    register_alarm Generator.emitter kf kinstr ?status
      (Alarms.Invalid_shift(exp, upper_bound))
  in
  if remove_trivial then
    (match get_expr_val exp with
    | None -> ignore (alarm ())
    | Some c64 ->
      (* operand is constant:
         check it is nonnegative and stricly less than the upper bound (if
	 any) *)
      let upper_bound_ok = match upper_bound with
	| None -> true
	| Some u -> Integer.lt c64 (Integer.of_int u)
      in
      if not (Integer.ge c64 Integer.zero && upper_bound_ok) then begin
	let a = alarm ~status:Property_status.False_if_reachable () in
	if warning then 
	  Options.warn "shift assert broken (bad operand): %a" 
	    local_printer#code_annotation a
      end)
  else
    ignore (alarm ())

(* assertions for bitwise left/right shift signed overflow *)
let signed_shift_assertion
    ~remove_trivial ~warning kf kinstr (exp, op, lexp, rexp) =
  (* - (1) right operand should be nonnegative and
     strictly less than the width of promoted left operand:
     now done by shift_right_operand_assertion
     - (2) (since signed version) left operand should be nonnegative
     (implementation defined for right shift, undefined for left shift)
     - (3) (for signed left shift): result should be representable in result
     type *)
  let t = Cil.unrollType (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  if size <> Cil.bitsSizeOf (Cil.typeOf lexp) then
    (* size of result type should be size of left (promoted) operand *)
    Options.warn "problem with bitsSize of %a: not treated" Printer.pp_exp exp;
  shift_alarm remove_trivial warning kf kinstr (lexp, None);
  if op = Shiftlt then
    (* compute greatest representable "size bits" (signed) integer *)
    let maxValResult = Cil.max_signed_number size in
    let overflow_alarm ?status () =
      register_alarm Generator.emitter kf kinstr ?status
	(Alarms.Overflow(Alarms.Signed, exp, maxValResult, Alarms.Upper_bound))
    in
    if remove_trivial then
      (match get_expr_val lexp, get_expr_val rexp with
      | None,_ | _, None -> 
	ignore (overflow_alarm ())
      | Some lval64, Some rval64 ->
	(* both operands are constant: check result is representable in
	   result type *)
	if Integer.ge rval64 Integer.zero 
	  && Integer.gt (Integer.shift_left lval64 rval64) maxValResult then
	  let a = 
	    overflow_alarm ~status:Property_status.False_if_reachable () 
	  in
	  if warning then
	    Options.warn "shift assert broken (signed overflow): %a"
	      local_printer#code_annotation a)
    else
      ignore (overflow_alarm ())

(* assertion for downcasting an integer to an unsigned integer type
   without requiring modification of value to reach target domain
   (well-defined behavior though) *)
let unsigned_downcast_assertion ~remove_trivial ~warning kf kinstr (ty, exp) =
  let e_typ = Cil.unrollType (Cil.typeOf exp) in
  match e_typ with
  | TInt (kind,_) ->
    let szTo = Cil.bitsSizeOf ty in
    let szFrom = Cil.bitsSizeOf e_typ in
    (if szTo < szFrom || Cil.isSigned kind then
      (* case signed to unsigned:
	 requires signed to be >= 0 and also <= max of unsigned size *)
      (* cast unsigned to unsigned:
	 ok is same bit size ;
	 if target is <, requires <= max target *)
	let max_ty = Cil.max_unsigned_number szTo in
	let alarms ?status bound =
	  register_alarm Generator.emitter kf kinstr ?status
	    (Alarms.Overflow
	       (Alarms.Unsigned_downcast, 
		exp, 
		(match bound with 
		| Alarms.Lower_bound -> Integer.zero
		| Alarms.Upper_bound -> max_ty), 
		bound))
	in
	let full_alarms () =
	  if Cil.isSigned kind then begin (* signed to unsigned *) 
	    ignore (alarms Alarms.Upper_bound);
	    ignore (alarms Alarms.Lower_bound)
	  end else (* unsigned to unsigned *)
	    ignore (alarms Alarms.Upper_bound)
	in
	if remove_trivial then
          match get_expr_val exp with
	  | None -> full_alarms ()
	  | Some a64 ->
	    (if Integer.lt a64 Integer.zero then begin
	      let a = 
		alarms ~status:Property_status.False_if_reachable 
		  Alarms.Lower_bound 
	      in
	      if warning then
		Options.warn "unsigned downcast assert broken: %a" 
		  local_printer#code_annotation a
	    end else if Integer.gt a64 max_ty then
		let a = 
		  alarms ~status:Property_status.False_if_reachable 
		    Alarms.Upper_bound
		in
		if warning then
		  Options.warn "unsigned downcast assert broken: %a" 
		    local_printer#code_annotation a)
	else
	  full_alarms ());
    true
  | _ ->
    false

(* assertion for downcasting an integer to a signed integer type *)
(* which can raise an implementation defined behavior *)
let signed_downcast_assertion ~remove_trivial ~warning kf kinstr (ty, exp) =
  let e_typ = Cil.unrollType (Cil.typeOf exp) in
  match e_typ with
  | TInt (kind,_) ->
    (let szTo = Cil.bitsSizeOf ty in
     let szFrom = Cil.bitsSizeOf e_typ in
     if szTo < szFrom || (szTo == szFrom && not (Cil.isSigned kind)) then
      (* downcast: the expression result should fit on szTo bits *)
       let min_ty = Cil.min_signed_number szTo in
       let max_ty = Cil.max_signed_number szTo in
       let alarms ?status bound =
	 register_alarm Generator.emitter kf kinstr ?status
	   (Alarms.Overflow
	      (Alarms.Signed_downcast, exp,
               (match bound with
	       | Alarms.Lower_bound -> min_ty
	       | Alarms.Upper_bound -> max_ty), 
	       bound))
       in
       let full_alarms () =
	 if Cil.isSigned kind then begin
	   (* signed to signed *)
	   ignore (alarms Alarms.Upper_bound);
	   ignore (alarms Alarms.Lower_bound)
	 end else
	   ignore (alarms Alarms.Upper_bound)
       in
       if remove_trivial then
         match get_expr_val exp with
	 | None -> full_alarms ()
         | Some a64 -> 
	   (if Integer.lt a64 min_ty then begin
	     let a = 
	       alarms ~status:Property_status.False_if_reachable 
		 Alarms.Lower_bound 
	     in
	     if warning then
	       Options.warn "signed downcast assert broken: %a" 
		 local_printer#code_annotation a
	   end else if Integer.gt a64 max_ty then
	       let a = 
		 alarms ~status:Property_status.False_if_reachable 
		   Alarms.Upper_bound 
	       in
	       if warning then
		 Options.warn "signed downcast assert broken: %a" 
		   local_printer#code_annotation a)
       else 
	 full_alarms ());
    true
  | _ ->
    false

(* assertion for casting a floating-point value to an integer *)
let float_to_int_assertion ~remove_trivial ~warning kf kinstr (ty, exp) =
  let e_typ = Cil.unrollType (Cil.typeOf exp) in
  match e_typ, ty with
  | TFloat _, TInt (ikind,_) ->
    let szTo = Cil.bitsSizeOf ty in
    let min_ty, max_ty =
      if Cil.isSigned ikind then
        Cil.min_signed_number szTo, Cil.max_signed_number szTo
      else
        Integer.zero, Cil.max_unsigned_number szTo
    in
    let alarms ?status bound =
      register_alarm Generator.emitter kf kinstr ?status
	(Alarms.Float_to_int
	   (exp, 
	    (match bound with 
	    | Alarms.Lower_bound -> min_ty
	    | Alarms.Upper_bound -> max_ty), 
	    bound))
    in
    let full_alarms () =
      ignore (alarms Alarms.Upper_bound);
      ignore (alarms Alarms.Lower_bound);
    in
    let f = match exp.enode with
      | Const (CReal (f, _, _)) -> Some f
      | UnOp (Neg, { enode = Const (CReal (f, _, _))}, _) -> Some (-. f)
      | _ -> None
    in
    (match remove_trivial, f with
      | true, Some f ->
        begin
          try
            let fint = Floating_point.truncate_to_integer f in
	    if Integer.lt fint min_ty then (
	      let a = 
		alarms ~status:Property_status.False_if_reachable 
		  Alarms.Lower_bound 
	      in
	      if warning then
	        Options.warn "float to int assert broken: %a"
	          local_printer#code_annotation a;
              true)
            else if Integer.gt fint max_ty then (
	      let a = 
		alarms ~status:Property_status.False_if_reachable 
		  Alarms.Upper_bound 
	      in
	      if warning then
	        Options.warn "float to int assert broken: %a"
	          local_printer#code_annotation a;
              true)
            else false
          with Floating_point.Float_Non_representable_as_Int64 ->
            (* One of the alarms is False, but which one? ... *)
            full_alarms (); 
	    true
        end
      | _ -> 
	full_alarms (); 
	true)
  | _ ->
    false

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

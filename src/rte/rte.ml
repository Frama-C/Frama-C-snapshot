(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

(** Runtime Error annotation generation plugin *)

(* [TODO] should we check for plugin option -no-overflow: disable
   signed overflow annotation generation ? *)

open Cil
open Cil_types
open Rte_parameters


let rte_prefix = "rte" (* prefix for generated predicates (not behaviors) *)
let precond_prefix = "pre" (* prefix for generated behaviors *)

module Int = Abstract_interp.Int (* for integer computation *)

module type Generated = sig
  val is_computed: kernel_function -> bool
  val set: kernel_function -> bool -> unit
  val self: State.t
  val emitter: Emitter.t
end

module GENERATED
  (M:sig
    val name:string
    val default: kernel_function -> bool
    val parameter: Parameter.t
    val additional_parameters: Parameter.t list
    val emitter_kinds: Emitter.kind list
  end) 
  : Generated =
struct

  module H = 
    Kernel_function.Make_Table
      (Datatype.Bool)
      (struct
	let name = M.name
	let size = 17
	let dependencies =
          let extract p = State.get p.Parameter.name in
	  List.map extract (M.parameter :: M.additional_parameters) @
            [ Ast.self; Trivial.self]
       end)

  let is_computed kf = H.memo M.default kf
  let set = H.replace
  let self = H.self

  let emitter =
    Emitter.create
      M.name
      M.emitter_kinds
      ~correctness:(Trivial.parameter :: M.additional_parameters)
      ~tuning:[]

end

module RTE_Signed_Generated =
  GENERATED
    (struct
       let name = "Signed overflow"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoSignedOverflow.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Property_status; Emitter.Code_annot ]
     end)

module RTE_MemAccess_Generated =
  GENERATED
    (struct
       let name = "Mem access"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoMemAccess.parameter
       let additional_parameters = [Kernel.SafeArrays.parameter]
       let emitter_kinds = [ Emitter.Code_annot ]
     end)

module RTE_DivMod_Generated =
  GENERATED
    (struct
       let name = "Div/mod"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoDivMod.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Property_status; Emitter.Code_annot ]
     end)

module RTE_DownCast_Generated =
  GENERATED
    (struct
       let name = "Downcast"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoDownCast.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Property_status; Emitter.Code_annot ]
     end)

module RTE_UnsignedOverflow_Generated =
  GENERATED
    (struct
       let name = "Unsigned overflows"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoUnsignedOverflow.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Property_status; Emitter.Code_annot ]
     end)

module RTE_UnsignedDownCast_Generated =
  GENERATED
    (struct
       let name = "Unsigned downcasts"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoUnsignedDownCast.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Property_status; Emitter.Code_annot ]
     end)

module Called_Precond_Generated =
  GENERATED
    (struct
       let name = "Precondition"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = DoCalledPrecond.parameter
       let additional_parameters = []
       let emitter_kinds = [ Emitter.Code_annot ]
     end)

let rte_proxy =
  State_builder.Proxy.create
    "RTE" 
    State_builder.Proxy.Backward
    [ RTE_Signed_Generated.self;
      RTE_MemAccess_Generated.self;
      RTE_DivMod_Generated.self;
      RTE_DownCast_Generated.self;
      RTE_UnsignedDownCast_Generated.self;
      RTE_UnsignedOverflow_Generated.self;
      Called_Precond_Generated.self ]

let self = State_builder.Proxy.get rte_proxy
let () = Db.RteGen.self := self

let emit_status emitter ppt s =
  Extlib.may (Property_status.emit emitter ~hyps:[] ppt) s

(* warnings *)

let fmt_warn_no_valid_fptr_deref  =
  format_of_string  "no predicate available yet to check \
     validity of function pointer dereferencing %a"
    
let fmt_warn_bitsize_over_64 = 
  format_of_string "bitsSize of %a > 64: not treated"

let fmt_warn_bitsize_over_32 = 
  format_of_string "bitsSize of %a > 32: not treated"

let fmt_warn_bad_bitsize =
  format_of_string "problem with bitsSize of %a: not treated"
    
let fmt_warn_shift_assert1 =
  format_of_string "shift assert broken (signed overflow): %a"

let fmt_warn_shift_assert2 =
  format_of_string
    "shift assert broken (left operand should be nonnegative): %a"

let fmt_warn_shift_assert3 =
  format_of_string "shift assert broken (unsigned overflow): %a"

let fmt_warn_shift_assert4 =
  format_of_string "shift assert broken (bad right operand): %a" 

let fmt_warn_signed_downcast_assert =
  format_of_string "signed downcast assert broken: %a"

let fmt_warn_unsigned_downcast_assert =
  format_of_string "unsigned downcast assert broken: %a"

let fmt_unary_minus_assert = 
  format_of_string "unary minus assert broken: %a"

let fmt_signed_overflow_assert =
  format_of_string "signed overflow assert broken: %a"

let fmt_unsigned_overflow_assert =
  format_of_string "unsigned overflow assert broken: %a"

let fmt_divisor_assert = 
  format_of_string "divisor assert broken: %a"

let rte_warn ?source fmt =
  Rte_parameters.warning ?source ~current:true ~once:true (*warn*) fmt

(* compute a term from a C expr, with the property that
   arithmetic operations are evaluated in Z (integers) *)

(* utiliser (Cil.isIntegralType e_typ || Cil.isFloatingType e_typ)
let is_integral_type e_typ =
  match e_typ with
    | TFloat (_,_)
    | TInt   (_,_) -> true
    | _ -> false
*)

(* (specialized) translation of a C expression to a logical term *)
(* apply (or not) cast to C type of expression to resulting term *)
(* a cast should NOT be performed only when the expression
   must be treated as an integer (logic) *)
(* (for instance when computing the full-precision / true result
   of an arithmetic expression) *)

let translate_C_expr_to_term ?(cast=true) expr =
  (* if cast is true, the term should have the type of expr (which is a C type) *)
  (* otherwise, the term should have type integer
     (or real if the C type of expr if not a subtype of integer: TODO) *)
  let e_typ = unrollType (Cil.typeOf expr) in
  let aterm = Logic_utils.expr_to_term ~cast:false expr in
  let term =
    if cast then
      begin
        (* expr_to_term_with_cast expr *)
        (* not used since it adds casts everywhere (not pretty):
           since sub-expressions are also checked for annotation,
           might as well cast only top expression *)
        match aterm.term_node with
          | TCastE(_,_) -> (* no point in recasting *) aterm
          | TConst _    -> (* constants are not cast,
                              though in some cases they should (big const) *) aterm
          | TLval _     -> aterm
          | _           ->
              if (Cil.isIntegralType e_typ || Cil.isFloatingType e_typ)
              then
                Logic_const.term
                  (TCastE
                     (e_typ,
                      Logic_const.term
                        aterm.term_node
                        (Logic_utils.typ_to_logic_type e_typ))) (Ctype e_typ)
              else aterm
      end
    else aterm
  in
(**)
    debug ~level:2
      "input expr: %a (%a)\n" Cil.d_exp expr Cil.d_type (Cil.typeOf expr)
    ;
    let t1,t2 = Logic_utils.expr_to_term ~cast:false expr, 
      Logic_utils.expr_to_term ~cast:true expr
    in
      debug ~level:2
        "output integer term: %a (%a)\n" 
	Cil.d_term t1 Cil.d_logic_type t1.term_type
      ;
      debug ~level:2
        "output (C cast) term: %a (%a)\n" 
	Cil.d_term t2 Cil.d_logic_type t2.term_type
      ;
(**)
      term

(* tries to evaluate expr as a constant value (Int64.t) *)
(* uses Cil constant folding (e.g. for (-0x7ffffff -1) => Some (-2147483648)) on 32 bits *)
let get_expr_val expr =
  let cexpr = constFold true expr in
    match cexpr.enode with
      | Const c ->
          let rec get_constant_expr_val e =
          match e with
            | CChr c -> get_constant_expr_val (charConstToInt c)
            | CInt64 (d64,_,_) -> Some d64
            | _ -> None
        in get_constant_expr_val c
      | _ -> 
	None


(* Creates [0 <= e] and [e < size] assertions *)
let valid_index ~remove_trivial e size =
  let loc = e.eloc in
  let expr = Logic_utils.expr_to_term ~cast:true e in
  let bound = Logic_utils.expr_to_term ~cast:true size in
  let p_pos = Logic_const.prel ~loc (Rlt, expr, bound) in
  let p_neg = Logic_const.prel ~loc (Rle, lzero(), expr) in
  if remove_trivial then
    (* See if the two assertions do not trivially hold. In this
       case, do not return then *)
    let v_e = get_expr_val e in
    let v_size = get_expr_val size in
    let neg_ok =
      Extlib.may_map ~dft:false (My_bigint.le My_bigint.zero) v_e ||
        Cil.isUnsignedInteger (Cil.typeOf e)
    in
    let pos_ok = match v_e, v_size with
      | Some v_e, Some v_size -> My_bigint.lt v_e v_size
      | None, _ | _, None -> false
    in
    let proven b p = if b then [] else [p, None] in
    proven neg_ok p_neg @ proven pos_ok p_pos @ []
  else
    [p_neg, None; p_pos, None]

(* returns the assertion associated with an lvalue:
   returns non empty assertions only on pointer dereferencing and array access,
   dereferencing of a function pointer gives no assertion (but a warning
   is emitted). The validity assertions are emitted using [valid] if
   [~for_write] is true, or with [valid_read] otherwise *)
let get_lval_assertion ~remove_trivial ?(for_write=false) (lv : lval) =
  let valid =
    if for_write then Logic_const.pvalid else Logic_const.pvalid_read
  in
  (* Default [\valid] assertion on the entire l-value *)
  let default_assertion () =
    let t =
      translate_C_expr_to_term ~cast:false (mkAddrOf ~loc:(CurrentLoc.get()) lv)
    in
    debug "gen valid on term %a\n" d_term t ;      
    [ (valid (Logic_const.here_label,t), None) ]
  in
  (* For accesses to known arrays we generate an assertions that constrains
     the index. This is simpler than the [\valid] assertion *)
  let rec check_has_array_access default off typ in_struct = 
    match off with
      | NoOffset -> if default then default_assertion () else []
      | Field (fi, off) ->
          (* Mark that we went through a struct field, then recurse *)
          check_has_array_access default off fi.ftype true
      | Index (e, off) -> begin
          match Cil.unrollType typ with
            | TArray (bt, Some size, _, _) ->
                if Kernel.SafeArrays.get () || not in_struct then
                  (* Generate an assertion for this access, then go deeper in
                     case other accesses exist *)
                  valid_index e size ~remove_trivial @
                    check_has_array_access default off bt in_struct
                else
                  (* Access to an array embedded in a struct with option
                     [-unsafe-arrays]. Honor the option and generate only
                     the default [\valid] assertion *)
                  check_has_array_access true off bt in_struct

            | TArray (bt, None, _, _) ->
                check_has_array_access true off bt in_struct
            | _ -> assert false
        end
  in
  match lv with		
    | Var vi , off ->
        check_has_array_access false off vi.vtype false
    | Mem _exp as lh, off ->
	if isFunctionType (typeOfLval lv) then begin
          rte_warn fmt_warn_no_valid_fptr_deref Cil.d_lval lv;
          check_has_array_access false off (Cil.typeOfLhost lh) false
        end
        else check_has_array_access true off (Cil.typeOfLhost lh) false

(* assertions for bounding a term *)
let assertion_le term bound = Logic_const.prel (Rle, term, Cil.lconstant bound)
let assertion_ge term bound = Logic_const.prel (Rge, term, Cil.lconstant bound)

(* assertion for unary minus signed overflow *)
let get_uminus_assertion ~remove_trivial ~warning expr =
  (* - expr overflows if exp is TYPE_MIN *)
  let t = unrollType (Cil.typeOf expr) in
  let size = bitsSizeOf t
    in if (size > 64) then (
      (* should never happen *)
      rte_warn fmt_warn_bitsize_over_64 d_exp expr ;
      []
    )
    else
      let minType = min_signed_number size in
      let assertion () =
        let term = translate_C_expr_to_term expr
        in Logic_const.prel (Rneq, term, Cil.lconstant minType)
      in
        if remove_trivial then (
          match get_expr_val expr with
            | Some a64 -> (* constant operand *)
                if My_bigint.equal a64 minType then begin
                  let assertion = assertion () in
                    if warning then
                      rte_warn fmt_unary_minus_assert
			d_predicate_named assertion;
                    [ assertion, Some Property_status.False_if_reachable ]
                end else
		  []
            | None -> [ (assertion (), None) ]
        ) else [ (assertion (), None) ]

(* assertions for multiplication/addition/subtraction signed overflow *)
let get_multsubadd_assertion ~remove_trivial ~warning full_expr op expr1 expr2 =
  (* signed multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than TYPE_MAX or strictly less than TYPE_MIN *)
  let t = unrollType (Cil.typeOf full_expr) in
  let size = bitsSizeOf t
  in if (size > 64) then (
      (* should never happen *)
    rte_warn fmt_warn_bitsize_over_64 d_exp full_expr ;
    []
  )
    else
      let (minType,maxType) = (min_signed_number size, max_signed_number size)
      in
      let full_add_term () =
        (* no cast to int since we check "true result" which is an integer *)
        let term1 = translate_C_expr_to_term ~cast:false expr1
        and term2 = translate_C_expr_to_term ~cast:false expr2
        in Logic_const.term (TBinOp (op, term1,term2)) (Ctype t)
      in
      let assertion_le ()   = assertion_le (full_add_term ()) maxType
      and assertion_ge ()   = assertion_ge (full_add_term ()) minType in
      let full_assertion () = 
	Logic_const.pand (assertion_le (), assertion_ge ())
      in
      if remove_trivial then begin
        match get_expr_val expr1, get_expr_val expr2 with
        | Some a64, Some b64 -> (* both operands are constant *)
          let big_a64 = a64
          and big_b64 = b64
          in
          if op = MinusA then
            let big_diff = Int.sub big_a64 big_b64
            in if Int.lt big_diff minType then
                let assertion = assertion_ge () in
                if warning then
                  rte_warn
                    fmt_signed_overflow_assert
                    d_predicate_named assertion;
                [ assertion, Some Property_status.False_if_reachable ]
              else 
		[ ]
          else if op = PlusA then
            let big_add = Int.add big_a64 big_b64 in
	    if Int.gt big_add maxType then
              let assertion = assertion_le () in
              if warning then
                rte_warn
                  fmt_signed_overflow_assert
                  d_predicate_named assertion;
              [ assertion, Some Property_status.False_if_reachable ]
            else
	      [ ]
          else (
            assert(op = Mult) ;
            let big_mult = Int.mul big_a64 big_b64 in
            let b_ov = Int.gt big_mult maxType in
            if b_ov then
              let assertion = assertion_le () in
              if warning then
                rte_warn
		  fmt_signed_overflow_assert
                  d_predicate_named assertion ;
              [ assertion, Some Property_status.False_if_reachable ]
            else
              let b_uv = Int.lt big_mult minType in
              if b_uv then
                let assertion = assertion_ge () in
                if warning then
                  rte_warn
                    fmt_signed_overflow_assert
                    d_predicate_named assertion ;
                [ assertion, Some Property_status.False_if_reachable ]
              else [ ])
        | Some a64, None
        | None, Some a64 ->  (* one operand is constant *)
          if op = MinusA then
	    [ assertion_ge (), None ]
          else if op = PlusA then
	    [ assertion_le (), None ]
          else begin
            assert(op = Mult);
                  (* multiplying by 1 or 0 if not dangerous *)
            if (My_bigint.equal a64 My_bigint.zero) ||
              (My_bigint.equal a64 My_bigint.one)
            then []
            else
                    (* multiplying by -1 is dangerous (albeit seldom) *)
              if My_bigint.equal a64 My_bigint.minus_one then
                [ assertion_le (), None ]
              else 
		[ full_assertion (), None ]
          end
        | None,None -> 
	      (* no operand is a constant *)
	  [ full_assertion (), None ]
      end else
	[ full_assertion (), None ]

(* assertions for multiplication/addition/subtraction unsigned overflow *)
(* this is allowed by C and NOT a runtime-error *)
let get_multsubadd_unsigned_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    full_expr op expr1 expr2 =
  (* unsigned multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than TYPE_MAX or strictly less than TYPE_MIN (=0) *)
  let t = unrollType (Cil.typeOf full_expr) in
  let size = bitsSizeOf t
  in if (size > 32) then (
      (* could happen: but then it's not possible yet to represent the maximum
         possible value of the domain (2^64 - 1) as a Cil constant
         (see TODO in cil_types.mli)
      *)
      rte_warn fmt_warn_bitsize_over_32 d_exp full_expr ;
    []
  )
    else
      let (minType,maxType) = My_bigint.zero, max_unsigned_number size in
      let full_add_term () =
        (* no cast to int since we check "true result" which is an integer *)
        let term1 = translate_C_expr_to_term ~cast:false expr1
        and term2 = translate_C_expr_to_term ~cast:false expr2
        in Logic_const.term (TBinOp (op, term1,term2)) (Ctype t)
      in let assertion () =
           if op = MinusA then
             assertion_ge (full_add_term ()) minType
           else
             assertion_le (full_add_term ()) maxType
         in
         if remove_trivial then begin
           match get_expr_val expr1, get_expr_val expr2 with
           | Some big_a64, Some big_b64 -> (* both operands are constant *)
             if op = MinusA then
               let big_diff = Int.sub big_a64 big_b64
               in
               if Int.lt big_diff minType then
                 let assertion = assertion () in
                 if warning then
                   rte_warn
                     fmt_unsigned_overflow_assert
                     d_predicate_named assertion;
                 [ assertion, Some Property_status.False_if_reachable ]
               else [ ]
             else if op = PlusA then
               let big_add = Int.add big_a64 big_b64 in
               if Int.gt big_add maxType then
                 let assertion = assertion () in
                 if warning then
                   rte_warn
                     fmt_unsigned_overflow_assert
                     d_predicate_named assertion;
                 [ assertion, Some Property_status.False_if_reachable ]
               else [ ]
             else (
               assert(op = Mult) ;
               let big_mult = Int.mul big_a64 big_b64 in
               let () = assert(Int.compare big_mult Int.zero >= 0) in
               let b_ov = Int.gt big_mult maxType in
               if b_ov then
                 let assertion = assertion () in
                 if warning then
                   rte_warn
                     fmt_unsigned_overflow_assert
                     d_predicate_named assertion ;
                 [ assertion, Some Property_status.False_if_reachable ]
               else [ ])
           | Some a64, None
           | None, Some a64 ->  (* one operand is constant *)
             if op = Mult then begin
               (* multiplying by 1 or 0 if not dangerous *)
               if My_bigint.equal a64 My_bigint.zero
                 || My_bigint.equal a64 My_bigint.one
               then []
               else [ assertion (), None ]
	     end else [ assertion (), None ]
           | None, None -> 
	     (* no operand is a constant *)
	     [ assertion (), None ]
	 end else
	   [ assertion (), None ]

(* assertions for division and modulo (divisor is 0) *)
let get_divmod_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    divisor_expr =
  (* division or modulo: overflow occurs when divisor is equal to zero *)
  let badValDivisor = My_bigint.zero in
  let assertion () =
    let term = translate_C_expr_to_term divisor_expr in
    Logic_const.prel (Rneq, term, Cil.lconstant badValDivisor)
  in
  if remove_trivial then begin
    match get_expr_val divisor_expr with
    | None -> (* divisor is not a constant (or it's value has not
                 been computed) *)
      [ assertion (), None ]
    | Some v64 ->
      if My_bigint.equal v64 badValDivisor then
        (* divide by 0 *)
        let assertion = assertion () in
        if warning then
          rte_warn fmt_divisor_assert d_predicate_named assertion ;
        [ assertion, Some Property_status.False_if_reachable ]
      else
        (* divide by constant which is not 0 *)
        (* nothing to assert *)
        []
  end else
    [ assertion (), None ]

(* assertion for signed division overflow *)
let get_signed_div_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    dividend_expr divisor_expr =
  (* Signed division: overflow occurs when dividend is equal to the
     the minimum (negative) value for the signed integer type,
     and divisor is equal to -1. Under the hypothesis (cf value analysis) that integers are
     represented in two's completement.
     Nothing done for modulo (the result of TYPE_MIN % -1 is 0, which does not overflow)
     Still it may be dangerous on a number of compilers / architectures
     (modulo may be performed in parallel with division)
  *)
  let t = unrollType (Cil.typeOf divisor_expr) in
  let size = bitsSizeOf t
  in
  (* check dividend_expr / divisor_expr : if constants ... *)
  if (size > 64) then (
    (* should never happen *)
    rte_warn fmt_warn_bitsize_over_64 d_exp divisor_expr ;
    []
  )
  else
    let badValDividend =
      (* compute smallest representable "size bits" (signed) integer *)
      min_signed_number size
    (*
      let min64 = Int64.min_int
      and shiftright_value = 64 - size
      in if shiftright_value > 0 then Int64.shift_right min64 shiftright_value else min64
    *)
    and badValDivisor = My_bigint.minus_one
    in let assert_for_divisor () =
         Logic_const.prel
           (Req, translate_C_expr_to_term divisor_expr, Cil.lconstant badValDivisor)
    and assert_for_dividend () =
         Logic_const.prel
           (Req,
            translate_C_expr_to_term dividend_expr, Cil.lconstant badValDividend)
       in let assert_not_both () =
            Logic_const.pnot
              (Logic_const.pand (assert_for_divisor (), assert_for_dividend ()))
          in
          if remove_trivial then (
            let problem_with_divisor () =
              match get_expr_val divisor_expr with
              | None -> (false,false)
              | Some c64 ->
                if My_bigint.equal c64 badValDivisor then (true,true)
                else (true,false)
            and problem_with_dividend () =
              match get_expr_val dividend_expr with
              | None -> (false,false)
              | Some c64 ->
                if My_bigint.equal c64 badValDividend then (true,true)
                else (true,false)
            in
            match problem_with_divisor (), problem_with_dividend () with
            | (false,_), (false,_) ->
              (* neither divisor nor dividend is constant *)
              (* Printf.eprintf "neither divisor nor dividend is constant\n";
                 flush stderr;  *)
              [ assert_not_both (), None ]
            | (true,true), (true,true) ->
              (* divisor and dividend are constant and have both bad values *)
              (* Printf.eprintf
                 "divisor and dividend are constant and have both bad values\n";
                 flush stderr ; *)
              let assertion = assert_not_both ()
              in
              if warning then
                rte_warn
                  fmt_signed_overflow_assert
                  d_predicate_named assertion ;
              [ assertion, Some Property_status.False_if_reachable ]
            | (true,false), _
            | _ , (true,false) ->
              (* one of divisor or dividend is constant and has a good value *)
              []
            | (true,true), (false,_) ->
              (* divisor is constant and has bad value, dividend is not
                 constant *)
              [ Logic_const.pnot (assert_for_dividend ()), 
		None ]
            | (false,_), (true,true) ->
              (* divisor is not constant, dividend is constant and has bad
                 value *)
              [ Logic_const.pnot (assert_for_divisor ()), 
		None ])
          else 
	    [ assert_not_both (), None ]

(* assertions for bitwise left shift unsigned overflow *)
(* this is allowed by C and NOT a runtime-error *)
let get_bitwise_lshift_unsigned_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    exp loperand roperand =
  (* result should be representable in result type *)
  let t = unrollType (Cil.typeOf exp) in
  let size = bitsSizeOf t in
  let maxValResult =
    (* compute greatest reprensentable "size bits" unsigned integer *)
    max_unsigned_number size
  in let ov_assertion () =
       let term = translate_C_expr_to_term ~cast:false exp
       in (* unsigned result is representable in result type if loperand times 2^roperand
             (where loperand and roperand are nonnegative),
             which should be equal to term (obtained with a shift),
             is less than the maximal value for the result type *)
        (* no cast to int since we check "true result" which is an integer*)
       Logic_const.prel (Rle, term, Cil.lconstant maxValResult)
     in let problem_with_ov_assertion () =
	  if remove_trivial then (
            match get_expr_val loperand, get_expr_val roperand with
            | None,_
            | _, None -> (false,false)
            | Some lval64, Some rval64 ->
              (* both operands are constant:
                 check result is representable in result type *)
              let result_true_val = Int.shift_left lval64 rval64 in
              if Int.gt result_true_val maxValResult then
                (true,false)(* constant operators and assertion does not hold *)
              else (true,true)(* constant operators and assertion holds *)
	  ) else (false,false)
	in
	match problem_with_ov_assertion () with
	| (true,false) ->
	  let assertion = ov_assertion () in
          if warning then
            rte_warn
              fmt_warn_shift_assert3
	      d_predicate_named assertion;
          [ assertion, Some Property_status.False_if_reachable ]
	| (true,true)  -> [ ]
	| (false,_)    -> [ ov_assertion (), None ]


(* generic assertion for bitwise left/right shift on right operand  *)
(* returns (list of assert, true if right operand OK or unknown / false if KO) *)
let get_bitwise_shift_right_operand_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    exp roperand =
  let t = unrollType (Cil.typeOf exp) in
  let size = bitsSizeOf t in
  let size64 = My_bigint.of_int size in
  let right_operand_assertion () =
    let term = translate_C_expr_to_term roperand in
    Logic_const.pand
      (Logic_const.prel (Rge, term, Cil.lzero ()),
       Logic_const.prel (Rlt, term, Cil.lconstant size64))
  in
  let problem_with_operand_assertion () =
    if remove_trivial then (
      match get_expr_val roperand with
      | None -> (false,false)
      | Some c64 ->
              (* right operand is constant:
                 check it is nonnegative and stricly less than size *)
        if (My_bigint.lt c64 size64) && (My_bigint.ge c64 My_bigint.zero)
        then (true,true)  (* constant operator and assertion holds *)
        else (true,false) (* constant operator and assertion does not hold *)
    ) else (false,false)
  in
  match problem_with_operand_assertion () with
  | (true,false) ->
    let assertion = right_operand_assertion () in
    if warning then
      rte_warn
        fmt_warn_shift_assert4
	d_predicate_named assertion;
    [ assertion, Some Property_status.False_if_reachable ], false
  | (true,true) -> [ ], true
  | (false,_)  ->  [ right_operand_assertion (), None ], true


(* assertions for bitwise left/right shift signed overflow *)
let get_bitwise_shift_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    exp shiftop loperand roperand =
  (* - (1) right operand should be nonnegative and
     strictly less than the width of promoted left operand:
     now done by get_bitwise_shift_right_operand_assertion
     - (2) (since signed version) left operand should be nonnegative
     (implementation defined for right shift, undefined for left shift)
     - (3) (for signed left shift) (left operand should be nonnegative: see (2), and)
     result should be representable in result type *)
  let t = unrollType (Cil.typeOf exp) in
  let size = bitsSizeOf t in
  let () =
    if not(size = bitsSizeOf (Cil.typeOf loperand) && size <= 64)
    (* size of result type should be size of left (promoted) operand *)
    then (
      rte_warn fmt_warn_bad_bitsize d_exp exp ;
    )
  in
  let maxValResult =
    (* compute greatest representable "size bits" (signed) integer *)
    max_signed_number size
  (*
    let max64 = Int64.max_int
    and shiftright_value = 64 - size
    in if shiftright_value > 0 then Int64.shift_right max64 shiftright_value else max64 *)
  in
  let assertion_2 () =
    let term = translate_C_expr_to_term loperand in Logic_const.prel (Rge, term, Cil.lzero ())
  and assertion_3 () =
    let term = translate_C_expr_to_term ~cast:false exp in
        (* signed result is representable in result type if loperand
           times 2^roperand (where loperand and roperand are nonnegative),
           which should be equal to term (obtained with a shift),
           is less than the maximal value for the result type *)
        (* no cast to int since we check "true result" which is an integer*)
    Logic_const.prel (Rle, term, Cil.lconstant maxValResult)
  in
  let problem_with_assertion_2 () =
    if remove_trivial then (
      match get_expr_val loperand with
      | None -> (false,false)
      | Some c64 ->
              (* left operand is constant: check it is nonnegative *)
        if My_bigint.ge c64 My_bigint.zero
        then (true, true) (* constant operator and assertion holds *)
        else (true,false) (* constant operator and assertion does not hold *)
    ) else (false,false)
  and problem_with_assertion_3 () =
    if remove_trivial then (
      match get_expr_val loperand, get_expr_val roperand with
      | None,_
      | _, None -> (false,false)
      | Some lval64, Some rval64 ->
              (* both operands are constant:
                 check result is representable in result type *)

        if (My_bigint.ge rval64 (My_bigint.of_int 64)) then
          (true,false)(* constant operators and assertion does not hold *)
        else
          let result_true_val = Int.shift_left lval64 rval64 in
          if Int.gt result_true_val maxValResult
          then (true,false)
                (* constant operators and assertion does not hold *)
          else (true,true) (* constant operators and assertion holds *))
    else (false,false)
  in
  let proceed_with_assertion_3 lassert =
    if (shiftop = Shiftlt) then begin
      match problem_with_assertion_3 () with
      | (true,false) ->
        let assertion = assertion_3 () in
        if warning then
          rte_warn
            fmt_warn_shift_assert1
            d_predicate_named assertion;
        (assertion, Some Property_status.False_if_reachable)::lassert
      | (true,true)  -> lassert
      | (false,_)    -> (assertion_3 (), None)::lassert
    end else
      lassert
  in
  match problem_with_assertion_2 () with
  | true, false ->
    let assertion = assertion_2 () in
    if warning then
      rte_warn
        fmt_warn_shift_assert2
        d_predicate_named assertion;
    (* do not proceed with assertion 3: left operand is negative,
       hence result is implementation defined anyway for left shift *)
    [ assertion, Some Property_status.False_if_reachable ]
  | true, true -> proceed_with_assertion_3 [ ]
  | false, _  -> 
    proceed_with_assertion_3 [ assertion_2 (), None ]


(* assertion for downcasting an integer to an unsigned integer type *)
(* without requiring modification of value to reach target domain *)
(* (well-defined behavior though) *)
let get_unsigned_downcast_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    cast_typ expr =
  let e_typ = unrollType (Cil.typeOf expr)
  in
  match e_typ with
    | TInt (kind,_) ->
      let szTo = bitsSizeOf cast_typ
      and szFrom = bitsSizeOf e_typ
      in
      if (szTo < szFrom) || isSigned kind 
      then
(* case signed to unsigned:
   requires signed to be >= 0 and also
   <= max of unsigned size *)
(* cast unsigned to unsigned:
   ok is same bit size ;
   if target is <, requires <= max target *)

	let maxType = max_unsigned_number szTo in
	let term = translate_C_expr_to_term ~cast:false expr in
	let assertion_le ()   = assertion_le term maxType in
	let assertion_ge ()   = assertion_ge term My_bigint.zero in
	let ceval =
          if remove_trivial then (
            match get_expr_val expr with
              | Some a64 -> (* constant expr *)
		debug "Get_expr_val_returns %a" 
		  (My_bigint.pretty ~hexa:false) a64 ;
		Some (My_bigint.ge a64 My_bigint.zero,
                      My_bigint.le a64 maxType)
              | None -> None)
          else None
	in match ceval with
	  | None ->
	    if not (isSigned kind) then
	      (* unsigned to unsigned *)
	      [ assertion_le (), None ]
	    else
	      (* signed to unsigned *)
              let full_assertion () =
		Logic_const.pand (assertion_le (), assertion_ge ())
              in 
	      [ full_assertion (), None ]
	  | Some (emin,emax) -> (
            match (emin,emax) with
              | (true,true) -> []
              | (true,false) ->
		let assertion  = assertion_le () in
		if warning then
		  rte_warn
		    fmt_warn_unsigned_downcast_assert
		    d_predicate_named assertion ;
		[ assertion, Some Property_status.False_if_reachable ]
              | (false,true) ->
		let assertion = assertion_ge () in
		if warning then
		  rte_warn
		    fmt_warn_unsigned_downcast_assert
		    d_predicate_named assertion ;
		[ assertion_le (), Some Property_status.False_if_reachable ]
		  
              | (false,false) -> assert false (* should not happen *))
      else []
    | _ -> []
	

(* assertion for downcasting an integer to a signed integer type *)
(* which can raise an implementation defined behavior *)
let get_downcast_assertion
    ~remove_trivial:remove_trivial
    ~warning:warning
    cast_typ expr =
  let e_typ = unrollType (Cil.typeOf expr)
  in
  match e_typ with
    | TInt (kind,_) ->
      let szTo = bitsSizeOf cast_typ
      and szFrom = bitsSizeOf e_typ
      in
      if (szTo < szFrom) || (szTo == szFrom && not(isSigned kind))
      then
      (* downcast: the expression result should fit on szTo bits *)
	let (minType,maxType) =
          (min_signed_number szTo, max_signed_number szTo)
	in
	let term = translate_C_expr_to_term ~cast:(not(isSigned kind)) expr in
	let le_assertion ()   = assertion_le term maxType in
	let ge_assertion ()   = assertion_ge term minType in
	let ceval =
          if remove_trivial then (
            match get_expr_val expr with
              | Some a64 -> (* constant expr *)
		debug "Get_expr_val_returns %a" 
		  (My_bigint.pretty ~hexa:false) a64 ;
		Some (My_bigint.ge a64 minType,
                      My_bigint.le a64 maxType)
              | None -> None)
          else None
	in match ceval with
	  | None ->
	    if not(isSigned kind) then 
	      (* unsigned to signed *)
	      [ le_assertion (), None ]
	    else
	      (* signed to signed *)
              let full_assertion () =
		Logic_const.pand (le_assertion (), ge_assertion ())
              in 
	      [ full_assertion (), None ]
	  | Some (emin,emax) -> (
            match (emin,emax) with
              | (true,true) -> []
              | (true,false) ->
		let assertion  = le_assertion () in
		if warning then
		  rte_warn
		    fmt_warn_signed_downcast_assert
		    d_predicate_named assertion ;
		[ assertion, Some Property_status.False_if_reachable ]
              | (false,true) ->
		let assertion = ge_assertion () in
		if warning then
		  rte_warn
		    fmt_warn_signed_downcast_assert
		    d_predicate_named assertion ;
		[ assertion, Some Property_status.False_if_reachable ]
		  
              | (false,false) -> assert false (* should not happen *))
      else []
    | _ -> []

(* assertion for preconditions *)
type orig_lval = (* StartOfOrig | *) AddrOfOrig | LvalOrig

let mk_term node typ = Logic_const.term node typ

let rec find_term_to_replace vinfo fa_terms =
  match fa_terms with
    | [] -> None
    | (formal,term) :: tl ->
        if vinfo.vid = formal.vid then Some term
        else find_term_to_replace vinfo tl

exception AddrOfFormal
exception NoResult

let tlval_fetcher_visitor () = object(self)

  inherit nopCilVisitor

  val mutable lvals : term list = []

  method private add_tlval lv =
    lvals <- lv :: lvals

  method fetch_lvals () = lvals

  method vterm t =
    match t.term_node with
      | TConst _ | TSizeOf _ | TSizeOfStr _
      | TAlignOf _ | Tnull | Ttype _ | Tempty_set -> SkipChildren

      | Tat _ -> self#add_tlval t ; SkipChildren

      | Tunion _
      | Tinter _
      | TLval _ -> self#add_tlval t ; DoChildren

      | _ -> DoChildren

(* TAddrOf tlv / TStartOf tlv not taken into account *)


end

(*   for each lval, replace each logic_variable which
     stems from a C variable by the term corresponding
     to the variable at this point iff it is a formal *)

let treat_tlval fa_terms ret_opt origin tlval =
  let prefix_origin ntlval =
    match origin with
    | LvalOrig -> TLval ntlval
    | AddrOfOrig -> TAddrOf ntlval
  in
  let (t_lhost, t_offset) = tlval in
  match t_lhost with

  | TMem _st -> DoChildren

  | TResult _ty -> ( (* for post-conditions and assigns containing a \result *)
    match ret_opt with
    | None -> raise NoResult (* BTS 692 *)
    | Some trm ->
                (* [VP] What happens if t_offset <> TNoOffset? *)
      ChangeTo (prefix_origin trm)
  )

  | TVar { lv_origin = Some vinfo } when vinfo.vformal ->
    (match find_term_to_replace vinfo fa_terms with
    | None -> DoChildren
             (* ? can this happen ? is it correct ? *)
    | Some nt ->
      let make_li tmp_lvar = {
        l_var_info = tmp_lvar; l_body = LBterm nt;
        l_type = None; l_tparams = [];
        l_labels = []; l_profile = [];
      }
      in
      let make_tlet () =
        let tmp_lvar = make_temp_logic_var nt.term_type in
        Tlet
          (make_li tmp_lvar,
           mk_term
             (prefix_origin (TVar tmp_lvar, t_offset))
             nt.term_type)
      in
      let tlet_or_ident () =
        if t_offset = TNoOffset then
                     (* Nothing to substitute afterwards. *)
          ChangeTo nt.term_node
        else
                     (* May need substitution in t_offset. *)
          ChangeDoChildrenPost (make_tlet (), fun x -> x)
      in
      let add_offset lval = Logic_const.addTermOffsetLval t_offset lval in
      match nt.term_node with
      | TLval lv ->
        ChangeDoChildrenPost
          (prefix_origin (add_offset lv), fun x -> x)
      | TStartOf lv ->
        let lv = add_offset lv in
        let t =
          match origin with
            LvalOrig -> TStartOf lv
          | AddrOfOrig -> TAddrOf lv
        in
        ChangeDoChildrenPost(t,fun x->x)
                     (* [VP]: TAddrOf must be treated as the other
                        non-lval arguments. *)
                     (*| TAddrOf (lhost,off) ->
                       let prefix_origin2 lv =
                       match nt.term_node with
                       | TLval _ -> TLval lv
                       | TStartOf _ -> TStartOf lv
                       | _ -> TAddrOf lv
                       in
                       ChangeDoChildrenPost
                       ((let ntlval = Logic_const.addTermOffsetLval t_offset (lhost,off)
                       in prefix_origin2 ntlval), fun x -> x)
                     *)
      | TCastE(ty,{ term_node = TLval lv | TStartOf lv }) ->
        (match origin with
          LvalOrig -> tlet_or_ident()
        | AddrOfOrig when t_offset = TNoOffset ->
          let t =
            Logic_const.taddrof lv (typeOfTermLval lv)
          in
          ChangeTo (TCastE(TPtr(ty,[]), t))
        | AddrOfOrig  ->
          let lh = TMem nt in
          ChangeDoChildrenPost
            (TAddrOf (lh,t_offset),fun x -> x))
      | _ when origin = AddrOfOrig ->
        rte_warn ~source:(fst nt.term_loc)
          "Cannot substitute a non-lval \
                               parameter under an addrof operation";
        raise AddrOfFormal
      | _  -> tlet_or_ident ())
  | _ -> DoChildren


let replacement_visitor replace_pre fa_terms ret_opt = object
  (* for each term, replace each logic_variable which
     stems from a C variable by the term corresponding
     to the variable at this point iff it is a formal *)

  (*  BTS 1052: must use a copy visitor *)
  inherit genericCilVisitor ~prj:(Project.current ()) (copy_visit ())

  method vterm_node t =
    match t with
      | TConst _ | TSizeOf _ | TSizeOfStr _
      | TAlignOf _ | Tnull | Ttype _ | Tempty_set -> SkipChildren

      | TLval tlval -> treat_tlval fa_terms ret_opt LvalOrig tlval
      | TAddrOf tlval -> treat_tlval fa_terms ret_opt AddrOfOrig tlval
      | TStartOf _ (* [VP] Neither parameters nor returned value can be
                      an array in a C function. Hence, TStartOf can not have
                      \result or a formal as base.
                   *)
      | _ -> DoChildren

  method vlogic_label l =
    match l with
      | StmtLabel _ -> DoChildren
      | LogicLabel _ 
          when Cil_datatype.Logic_label.equal l Logic_const.pre_label ->
        ChangeDoChildrenPost(replace_pre, fun x->x)
      | LogicLabel _ -> DoChildren

end

let treat_pred replace_pre pred fa_terms (ret_opt : term_lval option)  =
  let visitor = replacement_visitor replace_pre fa_terms ret_opt in
  visitCilPredicate (visitor :> cilVisitor) pred
    
let treat_term replace_pre trm fa_terms ret_opt =
  let visitor = replacement_visitor replace_pre fa_terms ret_opt in
    visitCilTerm (visitor :> cilVisitor) trm

(* AST inplace visitor for runtime annotation generation *)

(* module for bypassing categories of annotation generation for certain expression ids ;
   useful in a case such as

   signed char cx,cy,cz;
   cz = cx * cy;

   which translates to

   cz = (signed char) ((int) cx * (int) cz) ;

   which would in this case be annotated both by

   assert
   (((int )cx+(int )cy <= 2147483647) and
   ((int )cx+(int )cy >= (-0x7FFFFFFF-1)));

   and

   assert (((int )cx+(int )cy <= 127) and ((int )cx+(int )cy >= -128));

   while we only want to keep the second assert (comes from the cast,
   and is stronger)
*)

type skipCategory = SkipBounding

module SkipId = struct
  type t = int * skipCategory
      (* (expression id, category of assertion generation to skip)  *)

  let compare : t -> t -> int = Extlib.compare_basic
end

module SkipIdSet = Set.Make(SkipId)

type annotStmtPos = Before | After

exception NontreatedAssign
exception DontKeep


(* Used to generate fresh names for the behaviors introduced by -rte-precond *)
module KfPrecondBehaviors =
  Datatype.Triple_with_collections
    (Kernel_function) (* Caller *)
    (Kernel_function) (* Callee *)
    (Datatype.String) (* Behavior *)
    (struct let module_name = "Rte.KfBehaviors" end)


class rte_annot_visitor kf = object(self)
  inherit Visitor.frama_c_inplace (* inplace since no Ast transformation:
                                     only annotations are added *)

  val mutable skip_set = SkipIdSet.empty
  val mutable index_behavior = 0

  val assertion_table
    : predicate list Cil_datatype.Stmt.Hashtbl.t 
    = Cil_datatype.Stmt.Hashtbl.create 113

  val behavior_names = KfPrecondBehaviors.Hashtbl.create 7

  method private add_to_skip_set eid skip = 
    skip_set <- SkipIdSet.add (eid,skip) skip_set

  method private is_in_skip_set eid skip = SkipIdSet.mem (eid,skip) skip_set

  method private is_DoMemAccess () =
    DoMemAccess.get () && not (RTE_MemAccess_Generated.is_computed kf)

  method private is_DoCalledPrecond () =
    DoCalledPrecond.get () && not (Called_Precond_Generated.is_computed kf)

  method private is_DoDivMod () =
    DoDivMod.get () && not (RTE_DivMod_Generated.is_computed kf)

  method private is_DoSignedOverflow () =
    DoSignedOverflow.get () && not (RTE_Signed_Generated.is_computed kf)

  method private is_DoUnsignedOverflow () =
    DoUnsignedOverflow.get () 
  && not (RTE_UnsignedOverflow_Generated.is_computed kf)

  method private is_DoDownCast () =
    DoDownCast.get () && not (RTE_DownCast_Generated.is_computed kf)

  method private is_DoUnsignedDownCast () = 
    DoUnsignedDownCast.get () 
  && not (RTE_UnsignedDownCast_Generated.is_computed kf)

  method private is_Warning () = Warn.get ()
  method private remove_trivial () = not (Trivial.get ())

  method private queue_stmt_spec spec =
    let stmt = Extlib.the (self#current_stmt) in
    (*    let kf = Extlib.the self#current_kf in*)
    Queue.add
      (fun () ->
	let annot =
	  Logic_const.new_code_annotation (AStmtSpec ([],spec))
	in
	Annotations.add_code_annot
	  Called_Precond_Generated.emitter
	  ~kf
	  stmt
	  (User annot)) 
      self#get_filling_actions

  method private queue_assertion emitter assertion_list =
    (* add an assertion (with an optionally given status) in front of current
       statement *) 
    match assertion_list with
    | [] -> ()
    | _ ->
      let stmt = Extlib.the self#current_stmt in
      (*      let kf = Extlib.the self#current_kf in*)
      let already_posted_assertions =
	try Cil_datatype.Stmt.Hashtbl.find assertion_table stmt
	with Not_found -> []
      in
      let pruned_assertion_list =
	(* do not keep an assertion if an equivalent assertion (content)
	   is already scheduled *)
	List.rev 
	  (List.fold_left
	     (fun acc (assertion, status_opt) ->
	       if not (List.exists
			 (fun p ->
			   Logic_utils.is_same_predicate
			     p assertion.content)
			 already_posted_assertions)
	       then (assertion, status_opt) :: acc
	       else acc)
	     [] 
	     assertion_list)
      in
      let loc_add_assertion assertion assertion_status_opt =
	let rte_named_assertion =
	  (* give a name to assertion in order to indicate
	     it has been generated by RTE plugin *)
	  { content = assertion.content ;
	    loc = assertion.loc ; name = [ rte_prefix ] }
	in 
	let annot =
	  Logic_const.new_code_annotation (AAssert ([], rte_named_assertion))
	in
	Annotations.add_code_annot 
	  emitter
	  ~kf
	  stmt
	  (User annot);
        let ip = Property.ip_of_code_annot kf stmt annot in
        List.iter(fun x -> emit_status emitter x assertion_status_opt) ip
      in
      (* update scheduled assertions *)
      Cil_datatype.Stmt.Hashtbl.replace assertion_table stmt
	(List.rev_append
	   (List.rev_map (fun (a,_) -> a.content) pruned_assertion_list)
	   already_posted_assertions);
      Queue.add
	(fun () ->
	  List.iter
	    (fun (assertion, assertion_status_opt) ->
	      loc_add_assertion assertion assertion_status_opt) 
	    pruned_assertion_list)
	self#get_filling_actions

  method private mk_new_behavior_name kf_callee behav =
      let fname = Kernel_function.get_name kf_callee in
      let kf = Extlib.the self#current_kf in
      let bname =
        if Cil.is_default_behavior behav then "" else "_" ^ behav.b_name
      in
      let key = kf, kf_callee, bname in
      let name = 
	try 
	  let n = KfPrecondBehaviors.Hashtbl.find behavior_names key in
	  incr n;
	  precond_prefix ^ "_" ^ fname ^ bname ^ "_" ^ string_of_int !n
	with Not_found ->
	  KfPrecondBehaviors.Hashtbl.add behavior_names key (ref 1);
	  precond_prefix ^ "_" ^ fname ^ bname
      in
      Annotations.fresh_behavior_name kf name

  method private make_stmt_contract kf formals_actuals_terms ret_opt call_stmt =
    let tret_opt = match ret_opt with
      | None -> None
      | Some lv -> Some (Logic_utils.lval_to_term_lval ~cast:true lv)
    in
    let fun_transform_pred replace_pre p =
      let p' = Logic_const.pred_of_id_pred p in
      try
	let p_unnamed =
	  Logic_const.unamed
	    (treat_pred
               replace_pre
	       p'.content
	       formals_actuals_terms tret_opt)
	in
        Logic_const.new_predicate 
	  { content = p_unnamed.content ;
	    loc = p_unnamed.loc ;
	    name = p'.name }
      with 
      | AddrOfFormal
      | NoResult ->
        (* A warning has been emitted, we simply ignore the predicate here. *)
        Logic_const.new_predicate Logic_const.ptrue
    and fun_transform_allocations allocs =  
      let treat_alloc it = 
	Logic_const.new_identified_term 
          (treat_term Logic_const.old_label it.it_content formals_actuals_terms tret_opt)
      in
      match allocs with
      | FreeAlloc (lfree_loc, lalloc_loc) ->
	FreeAlloc (List.map treat_alloc lfree_loc, List.map treat_alloc lalloc_loc)
      | FreeAllocAny -> FreeAllocAny	  
    and fun_transform_assigns assigns =
      (* substitute terms, then for each from extract lvals and
         keep those and only those as froms *)
      let treat_from it =
	let rec keep_it t =
	  match t.term_node with
	  | TLval _ -> true
	  | Tat (loc,_) -> keep_it loc
	  | TCastE (_,te) -> keep_it te
	  | Tinter locs
	  | Tunion locs -> (
	    try
	      List.iter
		(fun loc -> 
		  if not(keep_it loc) then 
		    raise DontKeep
		) locs ;
	      true
	    with DontKeep -> false )
	  | _ -> false
	in 
	(* also, discard casts in froms *)
	let rec transform_term t = 
	  match t.term_node with
	  | TCastE (_,te) -> transform_term te
	  | _ -> t
	in
	let nterm =
          treat_term Logic_const.old_label 
            it.it_content formals_actuals_terms tret_opt
        in
	if keep_it nterm then 
	  [ Logic_const.new_identified_term (transform_term nterm) ]
	else []
      (* Do not generate froms from child left values any more *)
      (*
        let visitor = tlval_fetcher_visitor () in
        let _ = visitCilTerm (visitor :> cilVisitor) nterm in
        let list_tlvals = visitor#fetch_lvals () in
	List.rev_map
	(fun lv -> 
	Logic_const.new_identified_term lv)
	(List.filter keep_it list_tlvals)
       *)
      in
      let treat_identified_term_zone_froms z =
	match z with
	| FromAny -> FromAny
	| From l -> 
	  From (List.flatten (List.rev_map treat_from l))
      in 
      let treat_assign (z,lz) =
	try
	  let nt =
	    treat_term Logic_const.old_label
	      z.it_content formals_actuals_terms tret_opt
          (* should be an lval *)
	  in
	  (* also treat union, inter and at terms *)
	  match nt.term_node with
	  | Tat _
	  | TLval _
	  | Tunion _ 
	  | Tinter _ -> 		  
	    (Logic_const.new_identified_term nt,
	     treat_identified_term_zone_froms lz)
	  | _ -> raise NontreatedAssign
	with 
	| AddrOfFormal
	| NoResult -> raise NontreatedAssign
      in 
      let treat_assigns_clause l =
	(* compute list of assigns as (terms, list of terms) ;
	   if empty list of terms => it's a Nothing, else Location ... *)
	(* then process to transform assign on \result *)
        match l with
        | WritesAny -> WritesAny
        | Writes l -> 
	  try
	    Writes (List.rev (List.rev_map treat_assign l))
	  with NontreatedAssign -> WritesAny
      in
      let final_assigns_list =
	match ret_opt with
	| None ->
	  (* no return value: there should be no assign of \result *)
	  assigns
	| Some ret ->
	  let ret_type = typeOfLval ret in
	  let nlist_assigns =
	    (* if there is a assigns \at(\result,Post) \from x
	       replace by assigns \result \from x *)
	    match assigns with
	    | WritesAny -> WritesAny
	    | Writes assigns ->
              let rec change_at_result acc assgn =
                match assgn with
                  [] -> Writes (List.rev acc)
                | (a,from)::tl ->
		  let new_a =
                    match a.it_content.term_node with
		    | Tat ({term_node=(TLval(TResult _,_) as trm)}, 
                           LogicLabel (_, "Post")) -> 
		      let ttype = Ctype ret_type
		      (* cf. bug #559 *)
		      (* Logic_utils.typ_to_logic_type
			 ret_type *)
		      in
		      Logic_const.new_identified_term 
                        (mk_term trm ttype)
		    | _ -> a
		  in
                  change_at_result ((new_a,from) :: acc) tl
	      in
              change_at_result [] assigns
	  in
	  (* add assign on result iff no assigns(\result) already appears ;
	     treat_assign will then do the job *)
          let add_assigns_result () =
	    (* add assigns \result with empty list of froms to do the job *)
	    let ttype = Ctype ret_type
	    (* bug #559 *)
	    (* Logic_utils.typ_to_logic_type ret_type *) 
            in
	    let nterm = mk_term (TLval (TResult ret_type, TNoOffset)) ttype in
	    (Logic_const.new_identified_term nterm, FromAny)
	  in
          match nlist_assigns with
          | WritesAny -> WritesAny
          | Writes l when
              List.exists 
                (fun (a,_) -> Logic_utils.is_result a.it_content) l 
              ->
            nlist_assigns
          | Writes l -> Writes (add_assigns_result()::l)
      in 
      treat_assigns_clause final_assigns_list
    and (behaviors,default_assigns) =
      let spec = 
	(* calling get_spec on a function with a contract
	   but no code generates default assigns *)
	Annotations.funspec kf 
      in
      (* [JS 2012/06/01] looks quite close of Infer_annotations.populate_spec,
	 but it is not equivalent... *)
      let bhvs = spec.spec_behavior in
      bhvs, 
	(* Looking for management of default assigns clause. *)
      (match Ast_info.merge_assigns_from_complete_bhvs ~warn:false bhvs [] with
	WritesAny -> 
	  (* Case 1: it isn't possible to find good assigns from ungarded
	     behaviors. S, looks at assigns from complete behaviors clauses. *)
	  (match Ast_info.merge_assigns_from_complete_bhvs 
	      ~warn:true ~ungarded:false bhvs spec.spec_complete_behaviors 
	   with
	  | WritesAny -> None (* Case 1.1: no better thing to do than nothing *)
	  | assigns -> 
	    (* Case 1.2: that assigns will be used as default assigns later. 
	       note: a message has been emmited. *)
	    Some assigns)
      | _ -> (* Case 2: no special thing to do *)
	None)
    in
    try
      let new_behaviors = 
	let default_allocation_assigns = ref (None, None) in
	let new_bhvs =
	  List.fold_left
	    (fun acc bhv -> 
	      (* step 1: looking for managment of allocation and assigns
		 clause. *)
	      let allocation = 
		Some (fun_transform_allocations bhv.b_allocation) 
	      in
	      let assigns,allocation,name = 
		if Cil.is_default_behavior bhv then
		  match bhv with
		  | { b_post_cond = []; 
		      b_assumes = []; 
		      b_requires = []; 
		      b_assigns = WritesAny} ->
		    (* The default bhv contents only an allocation clause.
		       So, keeps it as the new default bhv. *)
		    (* here no call to mk_new_behavior_name, 
		       need to ensure same side-effect (populate englobing func
		       spec) *)
		    ignore (Annotations.funspec (Extlib.the self#current_kf));
		    let assigns = match default_assigns with
		      | Some assigns -> 
		      (* Use these assigns as default assigns *)
			assigns
		      | None -> 
			(* No special thing to do about assigns*)
			WritesAny
		    in 
		    assigns, allocation, Cil.default_behavior_name
		  | _ -> 
		    (* The default bhv contents other clauses.
		       So, extract the allocation clause for the new bhv
		       where the eventual default assigns will be set. *)
		    default_allocation_assigns := allocation, default_assigns;
		    bhv.b_assigns, None, self#mk_new_behavior_name kf bhv
		else 
		  bhv.b_assigns,allocation, self#mk_new_behavior_name kf bhv
	      in
              (* We store a mapping between the old and the copied requires,
                 in order to position some status dependencies between them *)
              let new_requires = ref [] in
              let requires = List.map
                (fun pred ->
                  let after = fun_transform_pred Logic_const.here_label pred in
                  new_requires := (pred, after) :: !new_requires;
                  after
                ) bhv.b_requires
              in
	      let b = (* step 2: just map the current behavior *)
		 (* As said before, assigns, allocation and names have a special
		    management *)
		mk_behavior
		  ~assigns:(fun_transform_assigns assigns) 
		  ~allocation 
		  ~name 
		  ~post_cond:(List.map
				(fun (k,p) -> k, 
                                  fun_transform_pred Logic_const.old_label p) 
				bhv.b_post_cond)
		  ~assumes:(List.map 
                              (fun_transform_pred Logic_const.here_label)
                              bhv.b_assumes)
		  ~requires
		  ~extended:[]
		  ()
	      in
              (* Update the dependencies between the original require, and the
                 copy at the syntactic call-site. Done once all the requires
                 and behaviors have been created by the visitore *)
              let requires_deps () =
                let kf_call = Kernel_function.find_englobing_kf call_stmt in
                let ki_call = Kstmt call_stmt in
                let aux (old, after) =
                  let old_ip = Property.ip_of_requires kf Kglobal bhv old in
                  let new_ip =Property.ip_of_requires kf_call ki_call b after in
                  Statuses_by_call.replace_call_precondition
                    old_ip call_stmt new_ip
                in
                List.iter aux !new_requires
              in
              Queue.add requires_deps self#get_filling_actions;
	      b :: acc) 
	    []
	    behaviors
	in  (* step 3: adds the allocation clause into a default behavior *)
	match !default_allocation_assigns with
	| None,None -> new_bhvs
	| allocation,None -> mk_behavior ~allocation () :: new_bhvs
	| allocation,Some assigns -> 
	  mk_behavior ~allocation ~assigns:(fun_transform_assigns assigns) ()
	  :: new_bhvs
      in
      match new_behaviors with
      | [] -> None
      | _ :: _ ->
	Some
	  { spec_behavior = List.rev new_behaviors ;
	    spec_variant = None ;
	    spec_terminates = None ;
	    spec_complete_behaviors = [] ;
	    spec_disjoint_behaviors = [] }
    with Exit -> 
      None

  method vstmt s =
    match s.skind with
      | UnspecifiedSequence l ->
          (* UnspecifiedSequences may contain lvals for side-effects, that
             give rise to spurious assertions *)
          let no_lval =
            List.map (fun (s, _, _, _, sref) -> (s, [], [], [], sref)) l
          in
          let s' = { s with skind = UnspecifiedSequence no_lval } in
          ChangeDoChildrenPost (s', fun _ -> s)

      | _ -> DoChildren


  method vinst vi =
    (* assigned left values are checked for valid access *)
    match vi with
    | Set (lval,_,_) ->
      if self#is_DoMemAccess () then begin
	debug "lval %a: validity of potential mem access checked\n"
	  Cil.d_lval lval ;
	self#queue_assertion
	  RTE_MemAccess_Generated.emitter
	  (get_lval_assertion ~remove_trivial:(self#remove_trivial ())
             ~for_write:true lval)
      end;
      DoChildren
    | Call (ret_opt,funcexp,argl,_) -> (
      (match ret_opt, self#is_DoMemAccess () with
      | Some ret, true -> 
	debug "lval %a: validity of potential mem access checked\n"
	  Cil.d_lval ret ;	  
        self#queue_assertion
	  RTE_MemAccess_Generated.emitter
	  (get_lval_assertion ~remove_trivial:(self#remove_trivial ())
             ~for_write:true ret)
      | _ -> ());
      if not(self#is_DoCalledPrecond ()) then
	DoChildren
      else
	match funcexp.enode with
	| Lval (Var vinfo,NoOffset) ->
	  let kf =  Globals.Functions.get vinfo in	    
	  let do_no_implicit_cast () = 
	    let formals = Kernel_function.get_formals kf in
	    if (List.length formals <> List.length argl) then (
	      rte_warn
		"(%a) function call with # actuals <> # formals: not treated"
		d_stmt (Extlib.the (self#current_stmt))
	      ;
	      DoChildren
	    ) else (
	      let formals_actuals_terms =
		List.rev_map2
		  (fun formal arg_exp ->
		    (formal,
		     Logic_utils.expr_to_term ~cast:true arg_exp)
		  )
		  formals argl in
	      match self#make_stmt_contract
                kf formals_actuals_terms ret_opt (Extlib.the (self#current_stmt)) with
	      | None -> DoChildren
	      | Some contract_stmt ->
		self#queue_stmt_spec contract_stmt
		;
		DoChildren
	    )
	  in (
	    match ret_opt with
	    | None -> do_no_implicit_cast ()
	    | Some lv -> 
	      let kf_ret_type = Kernel_function.get_return_type kf 
	      and lv_type = Cil.typeOfLval lv in
	      if Cil.need_cast kf_ret_type lv_type then (
		rte_warn 
		  "(%a) function call with intermediate cast: not treated"
		  d_stmt (Extlib.the (self#current_stmt));
		DoChildren
	      )
	      else do_no_implicit_cast ()
	  )
	| Lval (Mem _,NoOffset) ->
	  rte_warn "(%a) function called through a pointer: not treated"
	    d_stmt (Extlib.the (self#current_stmt))
	  ;
	  DoChildren
	| _ -> assert false
    )
    | _ -> DoChildren


  method vexpr exp =
    debug "considering exp %a\n" Cil.d_exp exp ;
    match exp.enode with
    | BinOp((Div|Mod) as op,dividend,divisor,ttype) ->
      (     
	match unrollType ttype with 
	  TInt(kind,_) -> 
	    (* add assertion "divisor not zero" *)
	    if self#is_DoDivMod () then
	      self#queue_assertion
		RTE_DivMod_Generated.emitter
		(get_divmod_assertion
		   ~remove_trivial:(self#remove_trivial ())
		   ~warning:(self#is_Warning ())
		   divisor);
	    if (self#is_DoSignedOverflow ()) && (op = Div) && (isSigned kind) 
	    then 
	      (* treat the special case of signed division overflow
		 (no signed modulo overflow) *)
	      self#queue_assertion
		RTE_Signed_Generated.emitter
		(get_signed_div_assertion
		   ~remove_trivial:(self#remove_trivial ())
		   ~warning:(self#is_Warning ())
		   dividend divisor);
	    DoChildren
	| _ -> DoChildren
      )

    | BinOp((Shiftlt|Shiftrt) as shiftop,loperand,roperand,ttype ) ->
      (match unrollType ttype with 
      | TInt(kind,_) -> 
	let do_signed = self#is_DoSignedOverflow () in
	let do_unsigned = self#is_DoUnsignedOverflow () in
	let remove_trivial = self#remove_trivial () in
	let warning = self#is_Warning () in
	(*	feedback "SHIFT %b %b %b" do_signed do_unsigned (self#is_ConstFold ());*)
	if do_signed || do_unsigned then begin
	  let (annot, isOk) =
	    (* generate and check assertion on right operand of shift *)
	    get_bitwise_shift_right_operand_assertion
	      ~remove_trivial ~warning
	      exp roperand
	  in
	  (*	  feedback "%a OK? %b" d_exp exp isOk;*)
	  if isOk then begin
	    (* right operand is correct or unknown:
	       otherwise no need to proceed with other assertions *)
	    (* assertions specific to signed shift *)
	    if do_signed && isSigned kind then 
	      begin
		let shift = 
		  get_bitwise_shift_assertion ~remove_trivial ~warning
		    exp shiftop loperand roperand
		in
		self#queue_assertion RTE_Signed_Generated.emitter annot;
		self#queue_assertion RTE_Signed_Generated.emitter shift
	      end 
	    else
	      if do_unsigned && not (isSigned kind) then
		begin
		  (* assertions specific to unsigned shift *)
		  if (shiftop = Shiftlt)
		  then begin
		    let shift = 
		      get_bitwise_lshift_unsigned_assertion
			~remove_trivial ~warning
			exp loperand roperand
		    in
		    self#queue_assertion RTE_UnsignedOverflow_Generated.emitter annot;
		    self#queue_assertion RTE_UnsignedOverflow_Generated.emitter shift
		  end
		  else 
		    self#queue_assertion RTE_UnsignedOverflow_Generated.emitter annot
		end
	      else begin
		(* choose any emitter *)
		if do_unsigned then  
		  self#queue_assertion RTE_UnsignedOverflow_Generated.emitter annot
		else 
		  self#queue_assertion RTE_Signed_Generated.emitter annot
	      end
	  end else begin (* right operand is incorrect *)
	    (* choose any emitter *)
	    if do_unsigned then 
	      self#queue_assertion RTE_Signed_Generated.emitter annot
	    else
	      self#queue_assertion RTE_UnsignedOverflow_Generated.emitter annot
	  end
	end;
	DoChildren
      | _ -> DoChildren
      )

    | BinOp((PlusA|MinusA|Mult)
	       as op,loperand,roperand,ttype) ->
      (* may be skipped if enclosing expression is a downcast to a signed 
	 type *) 
      (
	match unrollType ttype with 
	  TInt(kind,_) when (isSigned kind) -> 
	    
	    if (self#is_DoSignedOverflow ()) &&
	      not(self#is_in_skip_set exp.eid SkipBounding) then
	      self#queue_assertion
		RTE_Signed_Generated.emitter
		(get_multsubadd_assertion
		   ~remove_trivial:(self#remove_trivial ())
		   ~warning:(self#is_Warning ())
		   exp op loperand roperand)
	    ;
	    DoChildren
	| TInt(kind,_) when not (isSigned kind) -> 
	  
	  if self#is_DoUnsignedOverflow () then
	    self#queue_assertion
	      RTE_UnsignedOverflow_Generated.emitter
	      (get_multsubadd_unsigned_assertion
		 ~remove_trivial:(self#remove_trivial ())
		 ~warning:(self#is_Warning ())
		 exp op loperand roperand)
	  ;
	  DoChildren
	| _ -> DoChildren
      )
    | UnOp(Neg,operand,ttype) ->
      (
	match unrollType ttype with 
	  TInt(kind,_) when (isSigned kind) -> 

	    if self#is_DoSignedOverflow () then
	      self#queue_assertion
		RTE_Signed_Generated.emitter
		(get_uminus_assertion
		   ~remove_trivial:(self#remove_trivial ())
		   ~warning:(self#is_Warning ())
		   operand)
	    ;
	    DoChildren
	| _ -> DoChildren
      )
    (* Note: if unary minus on unsigned integer is to be understood as
       "subtracting the promoted value from the largest value
       of the promoted type and adding one"
       the result is always representable so no overflow
     *)

    | Lval lval ->
      (* left values are checked for valid access *)
      DoChildrenPost
        (fun new_e ->
           (* Use DoChildrenPost so that inner expression and lvals are checked
              first. The order of resulting assertions will be better. *) 
           if self#is_DoMemAccess () then (
	     debug "exp %a is an lval: validity of potential mem access \
                      checked" Cil.d_exp exp ;
	     self#queue_assertion
	       RTE_MemAccess_Generated.emitter 
	       (get_lval_assertion ~remove_trivial:(self#remove_trivial ())
                  lval)
           );
           new_e)

    | CastE (ttype, e) ->
      (
	match unrollType ttype with 
	  TInt(kind,_) when (isSigned kind) -> 
	    
	    if self#is_DoDownCast () || self#is_DoUnsignedDownCast () then (
	      let downcast_asserts =
		get_downcast_assertion
		  ~remove_trivial:(self#remove_trivial ())
		  ~warning:(self#is_Warning ())
		  ttype e
	      in match downcast_asserts with
	      | [] -> ()
	      | _ ->
		self#queue_assertion
		  RTE_DownCast_Generated.emitter 
		  downcast_asserts
		;
		self#add_to_skip_set e.eid SkipBounding
	    (* expression should be skipped w.r.t
	       signed mult/add/sub arithmetic overflow *)
	    )
	    ;
	    DoChildren
        | TInt(kind,_) when not(isSigned kind) -> 
	  
	  if self#is_DoUnsignedDownCast () then (
	    let downcast_asserts =
	      get_unsigned_downcast_assertion
		~remove_trivial:(self#remove_trivial ())
		~warning:(self#is_Warning ())
		ttype e
	    in 
	    match downcast_asserts with
	    | [] -> ()
	    | _ ->
	      self#queue_assertion 
		RTE_UnsignedDownCast_Generated.emitter
		downcast_asserts
	  (*		    ;
			    self#add_to_skip_set e.eid SkipBounding
	   *)
	  (* ?? expression should be skipped w.r.t
	     signed mult/add/sub arithmetic overflow ?? *)
	  )
	  ;
	  DoChildren
	| _ -> DoChildren
      )
    (* removed, see BTS#567: no point in asserting validity
       of first cell of an array simply because its address
       is taken as &tab[0] *)
    (*
      | StartOf _lval ->
      if self#is_DoMemAccess () then
      self#queue_assertion
      [ (Logic_const.pvalid (translate_C_expr_to_term ~cast:false exp), None) ]
      ;
      DoChildren
     *)
    | StartOf _
    | AddrOf _
    | Info _
    | UnOp _
    | Const _
    | BinOp _ -> DoChildren
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _ -> SkipChildren
end

let rte_emitters_list =
  [ RTE_Signed_Generated.emitter;
    RTE_MemAccess_Generated.emitter;
    RTE_DivMod_Generated.emitter;
    RTE_DownCast_Generated.emitter;
    RTE_UnsignedOverflow_Generated.emitter;
    RTE_UnsignedDownCast_Generated.emitter;
    Called_Precond_Generated.emitter ]

let remove_annotations_kf kf =
  (* [JS 2011/08/04] fix bug #910: 
     new implementation now requires to remove by hand annotations. *)
  let annotations = Annotations.code_annot_of_kf kf in
  let remove stmt a e = Annotations.remove_code_annot e ~kf stmt a in
  List.iter
    (fun (stmt, a) -> List.iter (remove stmt a) rte_emitters_list)
    annotations

let get_rte_annotations stmt = 
  List.fold_left
    (fun acc emitter ->
      Annotations.fold_code_annot
	(fun e rooted_a acc -> 
	  if Emitter.equal e emitter then
	    match rooted_a with 
	    | User a -> a :: acc
	    | AI _ -> assert false
	  else
	    acc) 
	stmt
	acc)
    []
    rte_emitters_list

let check_compute kf get is_computed set update_acc =
(*  feedback "get %b / doall %b / is_computed %b"
    (get ())
    (DoAll.get ())
    (is_computed kf);*)
  if get () && not (is_computed kf) then
    true, (fun () -> update_acc (); set kf true)
  else
    false, update_acc
    
let annotate_kf kf =
  (* generates annotation for function kf on the basis of command-line
     options *) 
  debug "annotating function %a" Kernel_function.pretty kf;
  (* add annotations *)
  match kf.fundec with
  | Declaration _ -> ()
  | Definition(f, _) -> 
    let update = ref (fun () -> ()) in
    let module Check(M:Generated)(P:sig val get: unit -> bool end) = struct
      let compute () = 
	let b, u = check_compute kf P.get M.is_computed M.set !update in
	update := u;
	b
    end in
    let must_run1 = 
      let module C = Check(RTE_Signed_Generated)(DoSignedOverflow) in 
      C.compute ()
    in
    let must_run2 = 
      let module C = Check(RTE_MemAccess_Generated)(DoMemAccess) in 
      C.compute ()
    in
    let must_run3 =
      let module C = Check(RTE_DivMod_Generated)(DoDivMod) in 
      C.compute ()
    in
    let must_run4 =
      let module C = Check(RTE_DownCast_Generated)(DoDownCast) in 
      C.compute ()
    in 
    let must_run5 =
      let module C = 
	    Check(RTE_UnsignedOverflow_Generated)(DoUnsignedOverflow) in 
      C.compute ()
    in
    let must_run6 =
      let module C = 
	    Check(RTE_UnsignedDownCast_Generated)(DoUnsignedDownCast) in 
      C.compute ()
    in
    let must_run7 =
      let module C = Check(Called_Precond_Generated)(DoCalledPrecond) in 
      C.compute ()
    in
    if must_run1 || must_run2 || must_run3 || must_run4 || must_run5 
      || must_run6 || must_run7 
    then begin
      feedback "annotating function %a" Kernel_function.pretty kf;
      let vis = new rte_annot_visitor kf in
      let nkf = Visitor.visitFramacFunction vis f in
      assert(nkf == f);
      !update ()
    end

let do_precond kf =
  (* annotate call sites with contracts, for a given function *)
  let old_signed = RTE_Signed_Generated.is_computed kf in
  RTE_Signed_Generated.set kf true;
  let old_mem = RTE_MemAccess_Generated.is_computed kf in
  RTE_MemAccess_Generated.set kf true;
  let old_divmod = RTE_DivMod_Generated.is_computed kf in
  RTE_DivMod_Generated.set kf true;
  let old_uo = RTE_UnsignedOverflow_Generated.is_computed kf in
  RTE_UnsignedOverflow_Generated.set kf true;
  let old_ud = RTE_UnsignedDownCast_Generated.is_computed kf in
  RTE_UnsignedDownCast_Generated.set kf true;
  let old_downcast = RTE_DownCast_Generated.is_computed kf in
  RTE_DownCast_Generated.set kf true;
  DoCalledPrecond.on ();
  annotate_kf kf;
  RTE_Signed_Generated.set kf old_signed;
  RTE_MemAccess_Generated.set kf old_mem;
  RTE_DivMod_Generated.set kf old_divmod;
  RTE_UnsignedOverflow_Generated.set kf old_uo;
  RTE_UnsignedDownCast_Generated.set kf old_ud;
  RTE_DownCast_Generated.set kf old_downcast

let do_all_rte kf =
  (* annonate for all rte + unsigned overflows (which are not rte), for a given
     function *) 
  DoAll.on ();
  DoUnsignedOverflow.on ();
  let old_ud = RTE_UnsignedDownCast_Generated.is_computed kf in
  RTE_UnsignedDownCast_Generated.set kf true;
  let old_called = Called_Precond_Generated.is_computed kf in
  Called_Precond_Generated.set kf true;
  annotate_kf kf;
  RTE_UnsignedDownCast_Generated.set kf old_ud;
  Called_Precond_Generated.set kf old_called
    
let do_rte kf = 
  (* annotate for rte only (not unsigned overflows and downcasts) for a given
     function *)
  DoAll.on ();
  let old_uo = RTE_UnsignedOverflow_Generated.is_computed kf in
  RTE_UnsignedOverflow_Generated.set kf true;
  let old_ud = RTE_UnsignedDownCast_Generated.is_computed kf in
  RTE_UnsignedDownCast_Generated.set kf true;
  let old_called = Called_Precond_Generated.is_computed kf in
  Called_Precond_Generated.set kf true;
  annotate_kf kf;
  RTE_UnsignedOverflow_Generated.set kf old_uo;
  RTE_UnsignedDownCast_Generated.set kf old_ud;
  Called_Precond_Generated.set kf old_called

let compute () =
  (* compute RTE annotations, whether Enabled is set or not *)
  Ast.compute () ;
  let include_function kf =
    let fsel = FunctionSelection.get () in
    Datatype.String.Set.is_empty fsel
    || let name = Kernel_function.get_name kf in
       Datatype.String.Set.mem name fsel
  in
  Globals.Functions.iter
    (fun kf ->
      if include_function kf then begin
        match kf.fundec with
        | Declaration _ -> ()
        | Definition _ -> !Db.RteGen.annotate_kf kf
      end)

let () =
  Db.register
    (Db.Journalize
       ("RteGen.annotate_kf", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.annotate_kf
    annotate_kf

let () =
  Db.register
    (Db.Journalize
       ("RteGen.compute", Datatype.func Datatype.unit Datatype.unit))
    Db.RteGen.compute
    compute

let () =
  Db.register
    (Db.Journalize
       ("RteGen.do_precond", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.do_precond
    do_precond

let do_precond =
  Dynamic.register
    ~plugin:"RteGen"
    "do_precond"
    (Datatype.func
       Kernel_function.ty
       Datatype.unit)
    ~journalize:true
    do_precond

let () =
  Db.register
    (Db.Journalize
       ("RteGen.do_all_rte", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.do_all_rte
    do_all_rte

let do_all_rte =
  Dynamic.register
    ~plugin:"RteGen"
    "do_all_rte"
    (Datatype.func
       Kernel_function.ty
       Datatype.unit)
    ~journalize:true
    do_all_rte

let () =
  Db.register
    (Db.Journalize
       ("RteGen.do_rte", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.do_rte
    do_rte

let get_precond_status () =
  Called_Precond_Generated.emitter, 
  Called_Precond_Generated.set,
  Called_Precond_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_precond_status
    get_precond_status

let get_signedOv_status () =
  RTE_Signed_Generated.emitter, 
  RTE_Signed_Generated.set,
  RTE_Signed_Generated.is_computed
  
let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_signedOv_status
    get_signedOv_status

let get_divMod_status () =
  RTE_DivMod_Generated.emitter, 
  RTE_DivMod_Generated.set,
  RTE_DivMod_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_divMod_status
    get_divMod_status

let get_downCast_status () =
  RTE_DownCast_Generated.emitter, 
  RTE_DownCast_Generated.set,
  RTE_DownCast_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_downCast_status
    get_downCast_status

let get_memAccess_status () =
  RTE_MemAccess_Generated.emitter, 
  RTE_MemAccess_Generated.set,
  RTE_MemAccess_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_memAccess_status
    get_memAccess_status

let get_unsignedOv_status () =
  RTE_UnsignedOverflow_Generated.emitter,
  RTE_UnsignedOverflow_Generated.set,
  RTE_UnsignedOverflow_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_unsignedOv_status
    get_unsignedOv_status

let get_unsignedDownCast_status () =
  RTE_UnsignedDownCast_Generated.emitter,
  RTE_UnsignedDownCast_Generated.set,
  RTE_UnsignedDownCast_Generated.is_computed

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_unsignedDownCast_status
    get_unsignedDownCast_status

let get_all_status () =
  [ get_precond_status ();
    get_signedOv_status ();
    get_memAccess_status ();
    get_divMod_status ();
    get_downCast_status ();
    get_unsignedOv_status ();
    get_unsignedDownCast_status () ]

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_all_status
    get_all_status

(* dynamic registration *)

(* retrieve list of generated rte annotations (not precond) for
   a given kernel function *)
let get_rte_annotations =
  Dynamic.register
    ~plugin:"RteGen"
    "get_rte_annotations"
    (Datatype.func
       Cil_datatype.Stmt.ty
       (let module L = Datatype.List(Cil_datatype.Code_annotation) in L.ty))
    ~journalize:true
    get_rte_annotations

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

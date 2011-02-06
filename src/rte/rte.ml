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

(** Runtime Error annotation generation plugin *)

(* should we check for plugin option -no-overflow: disable
   signed overflow annotation generation ? *)

open Db_types
open Cil
open Cil_types
open Rte_parameters
  
let rte_prefix = "rte" (* prefix for generated predicates (not behaviors) *)
let precond_prefix = "pre" (* prefix for generate behaviors *)

module Int = Abstract_interp.Int (* for integer computation *)

(* modules used for dependency between annotations and functions *)
(* there are 2 tables: one for runtime-errors, one for contracts
   annotations at call sites ("preconditions") *)

module RteGlobalTbl = struct
  include
    State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Kernel_function))
    (Dashtbl.Default_data_marshaler(Datatype.Unit))
    (struct
       let size = 97
       let name = "rte_global"
       let dependencies = [ Ast.self ]
       let kind = `Correctness
       let internal_kind = `Correctness
     end)

  let rec get_state kf =
    match find_all kf with
      | [] ->
	  add (Kernel_function.get_name kf) kf [ ] () ;
	  get_state kf
      | [ _, s ] -> s
      | _ -> assert false

end

module RteAnnotTbl = struct

  include
    State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Kernel_function))
    (Dashtbl.Default_data_marshaler(Datatype.Unit))
    (struct
       let size = 97
       let name = "rte_annot"
       let dependencies = [ RteGlobalTbl.self ]
       let kind = `Correctness
       let internal_kind = `Correctness
     end)

  let rec get_state kf =
    let sglobal = RteGlobalTbl.get_state kf in
    match find_all kf with
    | [] ->
      add (Kernel_function.get_name kf) kf [ sglobal ] () ;
      get_state kf
    | [ _, s ] -> s
    | _ -> assert false

end


module PrecondAnnotTbl = struct

  include
    State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Kernel_function))
    (Dashtbl.Default_data_marshaler(Datatype.Unit))
    (struct
       let size = 97
       let name = "precond_annot"
       let dependencies = [ RteGlobalTbl.self ]
       let kind = `Correctness
       let internal_kind = `Correctness
     end)

  let rec get_state kf =
    let sglobal = RteGlobalTbl.get_state kf in
      match find_all kf with
	| [] -> add (Kernel_function.get_name kf) kf [ sglobal ] () ;
	    get_state kf
	| [ _, s ] -> s
	| _ -> assert false

end

let () = Db.RteGen.self := RteGlobalTbl.self

let self = !Db.RteGen.self

(* Tables of RTE generation status. *)

(* [JS 2010/02/24] should handle dependencies in order to be able to
   have two different plug-ins which generate RTE *)
(* Table for generation status *)

module type Generated = sig
  val get: kernel_function -> bool
  val set: kernel_function -> bool -> unit
  val get_state : kernel_function -> State.t
  val self: State.t
end

module GENERATED
  (M:sig
     val name:string
     val default: kernel_function -> bool
   end)
  (Proxy:sig
     val self : State.t
     val get_state : kernel_function -> State.t
   end)
  =
struct

  include State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Kernel_function))
    (Dashtbl.Default_data_marshaler(Datatype.Ref(Datatype.Bool)))
    (struct
       let size = 17
       let name = M.name
       let dependencies = [ Proxy.self ] (* [ RTE_Status_Proxy.self ] *)
       let kind = `Internal
       let internal_kind = `Correctness (* JS [2010/06/04]: Internal? *)
     end)

  let add kf deps r =
    let name = Kernel_function.get_name kf ^ " --> " ^ string_of_bool !r in
    add name kf deps r

  let get kf =
    let state = Proxy.get_state kf in
    try !(find_data kf state)
    with Not_found ->
      let def = M.default kf in
      add kf [ state ] (ref def);
      def

  let get_state kf =
    let state = Proxy.get_state kf in
    try
      find_state kf state
    with Not_found ->
      add kf [ state ] (ref (M.default kf));
      try find_state kf state with Not_found -> assert false

  let set kf b =
    let state = Proxy.get_state kf in
    try
      let v = find_data kf state in
      v := b
    with Not_found ->
      add kf [ state ] (ref b)

end

module RTEAnnot_Proxy = struct
  let self = RteAnnotTbl.self
  let get_state = RteAnnotTbl.get_state
end

module PrecondAnnot_Proxy = struct
  let self = PrecondAnnotTbl.self
  let get_state = PrecondAnnotTbl.get_state
end

module RTE_Signed_Generated =
  GENERATED
    (struct
       let name = "Signed overflow"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (RTEAnnot_Proxy)

module RTE_MemAccess_Generated =
  GENERATED
    (struct
       let name = "Mem access"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (RTEAnnot_Proxy)

module RTE_DivMod_Generated =
  GENERATED
    (struct
       let name = "Div/mod"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (RTEAnnot_Proxy)

module RTE_DownCast_Generated =
  GENERATED
    (struct
       let name = "Downcast"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (RTEAnnot_Proxy)

module RTE_UnsignedOverflow_Generated =
  GENERATED
    (struct
       let name = "Unsigned overflows"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (RTEAnnot_Proxy)

module Called_Precond_Generated =
  GENERATED
    (struct
       let name = "Precondition"
       let default kf = not (Kernel_function.is_definition kf)
     end)
    (PrecondAnnot_Proxy)


(* this module is used for annotation status registering in this plugin *)
module S =
  Properties_status.Make_updater
    (struct
       let name = "RTE"
       let emitter = self
     end)

module Parameter_map = struct

  include Datatype.String.Map.Make(Datatype.Bool)

  let print m =
    Datatype.String.Map.iter
      (fun opt_name bval -> debug "opt=%s b=%b\n" opt_name bval) m

  let is_one_true ~except:name_opt m =
    match name_opt with
      | None -> (
	  try
	    Datatype.String.Map.iter
	      (fun _ bval -> if bval then failwith "") m
	    ;
	    false
	  with _ -> true
	)
      | Some name -> (
	  try
	    Datatype.String.Map.iter
	      (fun n bval -> if bval && (n <> name) then failwith "") m
	    ;
	    false
	  with _ -> true
	)

  let is_true name m =
    try
      Datatype.String.Map.find name m
    with Not_found -> false

  (* command-line options which enforce annotation generation:
     that is, annotations are not created if [DoAll.get ()] is [false]
     and all [generating_opts] are [false] *)
  let generating_opts =
    [ (* DoSignedOverflow *)
      DoSignedOverflow.name, DoSignedOverflow.get, true (* set on DoAll *),
      RTE_Signed_Generated.set;
      (* DoMemAccess *)
      DoMemAccess.name, DoMemAccess.get, true,
      RTE_MemAccess_Generated.set;
      (* DoDivMod *)
      DoDivMod.name, DoDivMod.get, true,
      RTE_DivMod_Generated.set;
      (* DoDownCast *)
      DoDownCast.name, DoDownCast.get, true,
      RTE_DownCast_Generated.set;
      (* DoCalledPrecond *)
      DoCalledPrecond.name, DoCalledPrecond.get, true,
      Called_Precond_Generated.set;
      (* DoUnsignedOverflow *)
      DoUnsignedOverflow.name, DoUnsignedOverflow.get, false (* NOT set on DoAll *),
      RTE_UnsignedOverflow_Generated.set ;
    ]

  (* options which influence generation *)
  let other_opts =
    [ ConstFold.name, ConstFold.get; Warn.name, Warn.get (* Print not added *) ]

  (* initial parameters => use rte_parameters initial values *)
  let empty_gen =
    List.fold_left
      (fun acc (opt_name,opt_fun, _, _) ->
	 Datatype.String.Map.add opt_name (opt_fun ()) acc)
      Datatype.String.Map.empty
      generating_opts

  let empty_other =
    List.fold_left
      (fun acc (opt_name,opt_fun) ->
	 Datatype.String.Map.add opt_name (opt_fun ()) acc )
      Datatype.String.Map.empty
      other_opts

  let gen_from_command_line_options () =
    let opt_state opt_fun bset_on_do_all =
      (* DoAll is set + bset_on_do_all = set all options to true ;
	 otherwise rely on option switch *)
      if bset_on_do_all && (DoAll.get ()) then true
      else opt_fun ()
    in
      List.fold_left
	(fun acc (opt_name,opt_fun,bset_on_do_all,_) ->
	   Datatype.String.Map.add
	     opt_name
	     (opt_state opt_fun bset_on_do_all)
	     acc)
	Datatype.String.Map.empty generating_opts

  let set_precond pmap =
    Datatype.String.Map.add DoCalledPrecond.name true pmap

  let set_all_rte pmap =
    List.fold_left
      (fun acc name -> Datatype.String.Map.add name true acc)
      pmap
      [ DoSignedOverflow.name ;
	DoMemAccess.name ;
	DoDivMod.name ;
	DoDownCast.name ]

  let set_unsignedOv pmap =
    Datatype.String.Map.add DoUnsignedOverflow.name true pmap

  let other_from_command_line_options () =
    List.fold_left
      (fun acc (opt_name,opt_fun) ->
	 Datatype.String.Map.add opt_name (opt_fun ()) acc)
      Datatype.String.Map.empty other_opts
end

module StateManager = struct

  (*
     register for each function the set of options used for analyzing it
     (options = either command-line, or set through direct API-like calls)
  *)

  module FuncOptionTbl =
    Kernel_function.Make_Table
      (Datatype.Pair(Parameter_map)(Parameter_map))
      (struct
	 let size = 97
	 let name = "rte_options"
	 let dependencies = [ Ast.self ]
	 let kind = `Correctness
       end)

  let find_current_gen_options kf =
    try fst (FuncOptionTbl.find kf)
    with Not_found -> Parameter_map.empty_gen

  let find_current_other_options kf =
    try snd (FuncOptionTbl.find kf)
    with Not_found -> Parameter_map.empty_other

(*
  (* get global state corresponding to function kf *)
  let rec get_state_kf kf =
    match RteGlobalTbl.find_all kf with
      | [] ->
	  RteGlobalTbl.add (Kernel_function.get_name kf) kf [ ] () ;
	  get_state_kf kf
      | [ _, s ] -> s
      | _ -> assert false

  (* get rte state corresponding to function kf *)
  let rec get_rte_state_kf kf =
    let sglobal = get_state_kf kf in
      match RteAnnotTbl.find_all kf with
	| [] ->
	    RteAnnotTbl.add (Kernel_function.get_name kf) kf [ sglobal ] () ;
	    get_rte_state_kf kf
      | [ _, s ] -> s
      | _ -> assert false

  (* get precond state corresponding to function kf *)
  let rec get_precond_state_kf kf =
    let sglobal = get_state_kf kf in
      match PrecondAnnotTbl.find_all kf with
	| [] ->
	    PrecondAnnotTbl.add (Kernel_function.get_name kf) kf [ sglobal ] () ;
	    get_precond_state_kf kf
	| [ _, s ] -> s
	| _ -> assert false
*)

end

(* warning *)

let rte_warn ?source fmt =
  Rte_parameters.warning ?source ~current:true ~once:true (*warn*) fmt

(* build a "checked as false" annotation status *)
let make_check_false () = Checked { emitter = "rte" ; valid = False }
(* and make_check_true () = Checked { emitter = "rte" ; valid = True } *) ;;

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
  let e_typ = Cil.typeOf expr in
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
(*
    debug ~level:2
      "input expr: %a (%a)\n" Cil.d_exp expr Cil.d_type (Cil.typeOf expr)
    ;
    let t1,t2 = expr_to_term ~cast:false expr, expr_to_term ~cast:true expr
    in
      debug ~level:2
	"output integer term: %a (%a)\n" Cil.d_term t1 Cil.d_logic_type t1.term_type
      ;
      debug ~level:2
	"output (C cast) term: %a (%a)\n" Cil.d_term t2 Cil.d_logic_type t2.term_type
      ;
*)
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
      | _ -> None

type tOff = MyField of fieldinfo | MyIndex of exp

(* returns the assertion associated with an lvalue:
   returns non empty assertion only on pointer dereferencing and array access,
   dereferencing of a function pointer gives no assertion (but a warning
   is emitted)
*)
let get_lval_assertion (lv : lval) =
  (* one has to build assertions for:
     - pointer dereferencing: only if lval host is of the form (Mem expr)
     - array access: several may occur, one for each offset of the form (Index _,_)
  *)
  (* so we :
     A. compute all assertions for array accesses by
       1. transforming the offset recurvise structure
          in an ad hoc list (offsets_as_list)
       2. keeping offsets which are array accesses (all_array_offsets)
       3. rebuilding Cil offsets from these (final_array_offsets)
       4. building lvals and terms from these
          (final_array_lvals, final_array_terms)
     B. add an assertion for a potential pointer dereferencing
        (lval lv's host is a Mem expr), unless it is a function pointer
        dereferencing (in which case we emit a warning))
  *)
  let (lhost, init_offset) = lv in
  let rec fetch_all_offsets acc off =
    match off with
      | NoOffset -> acc
      | Field (fi, next_off) -> fetch_all_offsets ((MyField fi) :: acc) next_off
      | Index (e, next_off) -> fetch_all_offsets ((MyIndex e) :: acc) next_off
  in let offsets_as_list = List.rev (fetch_all_offsets [] init_offset)
  in let all_array_offsets =
      fst (
	List.fold_left
	  (fun (acc_off,acc_prefix) moff ->
	     match moff with
	       | MyIndex _ ->
		   ((moff :: acc_prefix) :: acc_off, moff :: acc_prefix)
	       | _ -> (acc_off, moff :: acc_prefix)
	  ) ([],[]) offsets_as_list)

  in let rec build_offset_from_list off_list =
    match off_list with
      | [] -> NoOffset
      | (MyField fi) :: tl -> Field (fi, build_offset_from_list tl)
      | (MyIndex e) :: tl -> Index (e, build_offset_from_list tl)
  in let final_array_offsets =
    List.map
      (fun off_list ->
	 build_offset_from_list (List.rev off_list)
      ) all_array_offsets
(*
  in let () =
    debug "Final list of offsets is\n" ;
    List.iter (fun off  -> debug "%a\n" d_offset off) final_array_offsets
*)
  in let final_array_lvals = (List.map (fun off -> (lhost,off)) final_array_offsets)
  in let final_array_terms =
      List.fold_left
	(fun acc lv ->
	   if isFunctionType (typeOfLval lv) then (
	     rte_warn "no predicate available yet to check \
     validity of function pointer dereferencing %a"
	       Cil.d_lval lv
	     ;
	     acc)
           else
	     (translate_C_expr_to_term ~cast:false 
                (mkAddrOf ~loc:(CurrentLoc.get())lv)) 
             :: acc)
        []
        final_array_lvals
  in let final_terms =
      match lv with
	| Mem exp, _ ->
	    if isFunctionType (typeOfLval lv) then (
	      rte_warn "no predicate available yet to check \
validity of function pointer dereferencing %a"
		Cil.d_lval lv
	      ;
	      final_array_terms
	    ) else
	      (translate_C_expr_to_term ~cast:false exp) :: final_array_terms
	| Var _, _ -> final_array_terms
  in List.map (fun t -> (Logic_const.pvalid t, None) ) final_terms

(* compute min/max representable signed integer with bit-length sz (<= 64) *)
let get_signed_min sz =
  assert(sz <= 64);
  let min64 = Int64.min_int
  and shift_value = 64 - sz in
    if shift_value > 0
    then Int64.shift_right min64 shift_value
    else min64

let get_signed_max sz =
  assert(sz <= 64);
  let max64 = Int64.max_int
  and shift_value = 64 - sz in
    if shift_value > 0
    then Int64.shift_right max64 shift_value
    else max64

(* compute max representable unsigned integer
   with bit-length sz (<= 32 for the time being) *)
let get_unsigned_max sz =
  assert(sz <= 32);
  let max32 = Int64.of_string "0xffffffff"
  and shift_value = 32 - sz
  in
    if shift_value > 0
    then Int64.shift_right max32 shift_value
    else max32


(* assertions for bounding a term *)
let assertion_le term bound = Logic_const.prel (Rle, term, Cil.lconstant bound)
let assertion_ge term bound = Logic_const.prel (Rge, term, Cil.lconstant bound)

(* assertion for unary minus signed overflow *)
let get_uminus_assertion ~simplify_constants:simplify_constants ~warning:warning expr =
  (* - expr overflows if exp is TYPE_MIN *)
  let t = Cil.typeOf expr in
  let size = bitsSizeOf t
    in if (size > 64) then (
      (* should never happen *)
      rte_warn "bitsSize of %a > 64: not treated" d_exp expr ;
      []
    )
    else
      let minType = get_signed_min size in
      let assertion () =
	let term = translate_C_expr_to_term expr
	in Logic_const.prel (Rneq, term, Cil.lconstant minType)
      in
	if simplify_constants then (
	  match get_expr_val expr with
	    | Some a64 -> (* constant operand *)
		if Int64.compare a64 minType = 0 then (
		  let assertion = assertion ()
		  in
		    if warning then
		      rte_warn "unary minus assert broken: %a" d_predicate_named assertion
		    ;
		    [ (assertion, Some (make_check_false ())) ]
		)
		else []
	    | None -> [ (assertion (), None) ]
	) else [ (assertion (), None) ]

(* assertions for multiplication/addition/subtraction signed overflow *)
let get_multsubadd_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    full_expr op expr1 expr2 =
  (* signed multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than TYPE_MAX or strictly less than TYPE_MIN *)
  let t = Cil.typeOf full_expr in
  let size = bitsSizeOf t
  in if (size > 64) then (
      (* should never happen *)
      rte_warn "bitsSize of %a > 64: not treated" d_exp full_expr ;
      []
    )
    else
      let (minType,maxType) = (get_signed_min size, get_signed_max size) in
      let full_add_term () =
	(* no cast to int since we check "true result" which is an integer *)
	let term1 = translate_C_expr_to_term ~cast:false expr1
	and term2 = translate_C_expr_to_term ~cast:false expr2
	in Logic_const.term (TBinOp (op, term1,term2)) (Ctype t)
      in
      let assertion_le ()   = assertion_le (full_add_term ()) maxType
      and assertion_ge ()   = assertion_ge (full_add_term ()) minType in
      let full_assertion () = Logic_const.pand (assertion_le (), assertion_ge ())
      in
	if simplify_constants then (
	  match get_expr_val expr1, get_expr_val expr2 with
	    | Some a64, Some b64 -> (* both operands are constant *)
		let big_a64 = Int.of_int64 a64
		and big_b64 = Int.of_int64 b64
		in
		  if op = MinusA then
		    let big_diff = Int.sub big_a64 big_b64
		    in if Int.compare big_diff (Int.of_int64 minType) < 0 then
			let assertion = assertion_ge ()
			in
			  if warning then
			    rte_warn
			      "signed overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  else if op = PlusA then
		    let big_add = Int.add big_a64 big_b64
		    in if Int.compare big_add (Int.of_int64 maxType) > 0 then
			let assertion = assertion_le ()
			in
			  if warning then
			    rte_warn
			      "signed overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  else (
		    assert(op = Mult) ;
		    let big_mult = Int.mul big_a64 big_b64
		    in let b_ov = (Int.compare big_mult (Int.of_int64 maxType) > 0)
		    in if b_ov then
			let assertion = assertion_le ()
			in
			  if warning then
			    rte_warn
			      "signed overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else let b_uv = (Int.compare big_mult (Int.of_int64 minType) < 0)
		      in if b_uv then
			let assertion = assertion_ge ()
			in
			  if warning then
			    rte_warn
			      "signed overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  )
	    | Some a64, None
	    | None, Some a64 ->  (* one operand is constant *)
		if op = MinusA then [ (assertion_ge (), None) ]
		else if op = PlusA then [ (assertion_le (), None) ]
		else (
		  assert(op = Mult);
		  (* multiplying by 1 or 0 if not dangerous *)
		  if (Int64.compare a64 Int64.zero = 0) ||
		    (Int64.compare a64 Int64.one = 0) then []
		  else
		    (* multiplying by -1 is dangerous (albeit seldom) *)
		    if (Int64.compare a64 (Int64.of_int (-1)) = 0) then
		      [ (assertion_le (), None) ]
		    else [ (full_assertion (), None) ]
		)
	    | None,None -> [ (full_assertion (), None) ] (* no operand is a constant *)
	) else [ (full_assertion (), None) ]

(* assertions for multiplication/addition/subtraction unsigned overflow *)
(* this is allowed by C and NOT a runtime-error *)
let get_multsubadd_unsigned_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    full_expr op expr1 expr2 =
  (* unsigned multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than TYPE_MAX or strictly less than TYPE_MIN (=0) *)
  let t = Cil.typeOf full_expr in
  let size = bitsSizeOf t
  in if (size > 32) then (
      (* could happen: but then it's not possible yet to represent the maximum
	 possible value of the domain (2^64 - 1) as a Cil constant
	 (see TODO in cil_types.mli)
      *)
      rte_warn "bitsSize of %a > 32: not treated" d_exp full_expr ;
      []
    )
    else
      let (minType,maxType) = (Int64.zero, get_unsigned_max size) in
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
	if simplify_constants then (
	  match get_expr_val expr1, get_expr_val expr2 with
	    | Some a64, Some b64 -> (* both operands are constant *)
		let big_a64 = Int.of_int64 a64
		and big_b64 = Int.of_int64 b64
		in
		  if op = MinusA then
		    let big_diff = Int.sub big_a64 big_b64
		    in if Int.compare big_diff (Int.of_int64 minType) < 0 then
			let assertion = assertion () in
			  if warning then
			    rte_warn
			      "unsigned overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  else if op = PlusA then
		    let big_add = Int.add big_a64 big_b64
		    in if Int.compare big_add (Int.of_int64 maxType) > 0 then
			let assertion = assertion () in
			  if warning then
			    rte_warn
			      "unsigned overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  else (
		    assert(op = Mult) ;
		    let big_mult = Int.mul big_a64 big_b64
		    in let () = assert(Int.compare big_mult Int.zero >= 0)
		    in let b_ov = (Int.compare big_mult (Int.of_int64 maxType) > 0)
		    in
		      if b_ov then
			let assertion = assertion () in
			  if warning then
			    rte_warn
			      "unsigned overflow assert broken: %a"
			      d_predicate_named assertion
			  ;
			  [ (assertion, Some (make_check_false ())) ]
		      else [ ]
		  )
	    | Some a64, None
	    | None, Some a64 ->  (* one operand is constant *)
		if op = Mult then (
		  (* multiplying by 1 or 0 if not dangerous *)
		  if (Int64.compare a64 Int64.zero = 0) ||
		    (Int64.compare a64 Int64.one = 0)
		  then []
		  else [ (assertion (), None) ]
		) else [ (assertion (), None) ]
	    | None,None -> [ (assertion (), None) ] (* no operand is a constant *)
	) else [ (assertion (), None) ]

(* assertions for division and modulo (divisor is 0) *)
let get_divmod_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    divisor_expr =
  (* division or modulo: overflow occurs when divisor is equal to zero *)
  let badValDivisor = Int64.zero
  in let assertion () =
      let term = translate_C_expr_to_term divisor_expr
      in Logic_const.prel (Rneq, term, Cil.lconstant badValDivisor)
  in if simplify_constants then (
      match get_expr_val divisor_expr with
	| None -> (* divisor is not a constant (or it's value has not been computed) *)
	    [ (assertion (), None) ]
	| Some v64 ->
	    if Int64.compare v64 badValDivisor = 0 then
	      (* divide by 0 *)
	      let assertion = assertion ()
	      in
		if warning then
		  rte_warn "divisor assert broken: %a" d_predicate_named assertion
		;
		[ (assertion, Some (make_check_false ())) ]
	    else
	      (* divide by constant which is not 0 *)
	      (* nothing to assert *)
	      []
    ) else [ (assertion (), None) ]

(* assertion for signed division overflow *)
let get_signed_div_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    dividend_expr divisor_expr =
  (* Signed division: overflow occurs when dividend is equal to the
     the minimum (negative) value for the signed integer type,
     and divisor is equal to -1. Under the hypothesis (cf value analysis) that integers are
     represented in two's completement.
     Nothing done for modulo (the result of TYPE_MIN % -1 is 0, which does not overflow)
     Still it may be dangerous on a number of compilers / architectures
     (modulo may be performed in parallel with divison)
  *)
  let t = Cil.typeOf divisor_expr in
  let size = bitsSizeOf t
  in
    (* check dividend_expr / divisor_expr : if constants ... *)
    if (size > 64) then (
      (* should never happen *)
      rte_warn "bitsSize of %a > 64: not treated" d_exp divisor_expr ;
      []
    )
    else
      let badValDividend =
	(* compute smallest representable "size bits" (signed) integer *)
	get_signed_min size
(*
	let min64 = Int64.min_int
	and shiftright_value = 64 - size
	in if shiftright_value > 0 then Int64.shift_right min64 shiftright_value else min64
*)
      and badValDivisor = Int64.minus_one
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
	if simplify_constants then (
	  let problem_with_divisor () =
	    match get_expr_val divisor_expr with
	      | None -> (false,false)
	      | Some c64 ->
		  if Int64.compare c64 badValDivisor = 0
		  then (true,true)
		  else (true,false)
	  and problem_with_dividend () =
	    match get_expr_val dividend_expr with
	      | None -> (false,false)
	      | Some c64 ->
		  if Int64.compare c64 badValDividend = 0
		  then (true,true)
		  else (true,false)
	  in
	    match problem_with_divisor (), problem_with_dividend () with
	      | (false,_), (false,_) -> (* neither divisor nor dividend is constant *)
		  (* Printf.eprintf "neither divisor nor dividend is constant\n";
		     flush stderr;  *)
		  [ (assert_not_both (), None) ]
	      | (true,true), (true,true) ->
		  (* divisor and dividend are constant and have both bad values *)
		  (* Printf.eprintf
		     "divisor and dividend are constant and have both bad values\n";
		     flush stderr ; *)
		  let assertion = assert_not_both ()
		  in
		    if warning then
		      rte_warn
			"signed overflow assert broken: %a" d_predicate_named assertion
		    ;
		    [ (assertion, Some (make_check_false ())) ]
	      | (true,false), _
	      | _ , (true,false) ->
		  (* one of divisor or dividend is constant and has a good value *)
		  (* Printf.eprintf
		     "one of divisor or dividend is constant and has a good value\n";
		     flush stderr; *)
		  []
	      | (true,true), (false,_) ->
		  (* divisor is constant and has bad value, dividend is not constant *)
		  (* Printf.eprintf
		     "divisor is constant and has bad value, dividend is not constant\n";
		     flush stderr; *)
		  [ (Logic_const.pnot (assert_for_dividend ()), None) ]
	      | (false,_), (true,true) ->
		  (* divisor is not constant, dividend is constant and has bad value *)
		  (* Printf.eprintf
		     "divisor is not constant, dividend is constant and has bad value\n";
		     flush stderr; *)
		  [ (Logic_const.pnot (assert_for_divisor ()), None) ]
	) else [ (assert_not_both (), None) ]

(* assertions for bitwise left shift unsigned overflow *)
(* this is allowed by C and NOT a runtime-error *)
let get_bitwise_lshift_unsigned_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    exp loperand roperand =
  (* result should be representable in result type *)
  let t = Cil.typeOf exp in
  let size = bitsSizeOf t in
  let () =
    if not (size <= 32)
      (* 64 bits size "requires" representing a constant
	 which does not hold in a Cil constant
	 (as long as it remains an int64 and not a big integer) *)
    then rte_warn "problem with bitsSize of %a: not treated" d_exp exp ;
  in let maxValResult = (* compute greatest reprensentable "size bits" unsigned integer *)
      get_unsigned_max size
  in let ov_assertion () =
      let term = translate_C_expr_to_term ~cast:false exp
      in (* unsigned result is representable in result type if loperand times 2^roperand
	    (where loperand and roperand are nonnegative),
	    which should be equal to term (obtained with a shift),
	    is less than the maximal value for the result type *)
	(* no cast to int since we check "true result" which is an integer*)
	Logic_const.prel (Rle, term, Cil.lconstant maxValResult)
  in let problem_with_ov_assertion () =
      if simplify_constants then (
	match get_expr_val loperand, get_expr_val roperand with
	  | None,_
	  | _, None -> (false,false)
	  | Some lval64, Some rval64 ->
	      (* both operands are constant:
		 check result is representable in result type *)
	      let result_true_val =
		Int.shift_left (Int.of_int64 lval64) (Int.of_int64 rval64)
	      in
		if Int.compare result_true_val (Int.of_int64 maxValResult) > 0
		then (true,false) (* constant operators and assertion does not hold *)
		else (true,true) (* constant operators and assertion holds *)
      ) else (false,false)
  in
    match problem_with_ov_assertion () with
      | (true,false) ->
	  let assertion = ov_assertion () in
	    if warning then (
	      rte_warn
		"shift assert broken (unsigned overflow): %a" d_predicate_named assertion
	    )
	    ;
	    [ (assertion, Some (make_check_false())) ]
      | (true,true)  -> [ ]
      | (false,_)    -> [ (ov_assertion (), None) ]


(* generic assertion for bitwise left/right shift on right operand  *)
(* returns (list of assert, true if right operand OK or unknown / false if KO) *)
let get_bitwise_shift_right_operand_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    exp roperand =
  let t = Cil.typeOf exp in
  let size = bitsSizeOf t
  in let size64 = Int64.of_int size
  in let right_operand_assertion () =
      let term = translate_C_expr_to_term roperand
      in
	Logic_const.pand
	  (Logic_const.prel (Rge, term, Cil.lzero ()),
	   Logic_const.prel (Rlt, term, Cil.lconstant size64))
  in let problem_with_operand_assertion () =
      if simplify_constants then (
	match get_expr_val roperand with
	  | None -> (false,false)
	  | Some c64 ->
	      (* right operand is constant:
		 check it is nonnegative and stricly less than size *)
	      if (Int64.compare c64 size64 < 0) && (Int64.compare c64 Int64.zero >= 0)
	      then (true,true)  (* constant operator and assertion holds *)
	      else (true,false) (* constant operator and assertion does not hold *)
      ) else (false,false)
  in match problem_with_operand_assertion () with
    | (true,false) ->
	let assertion = right_operand_assertion () in
	  if warning then (
	    rte_warn
	      "shift assert broken (bad right operand): %a" d_predicate_named assertion
	  )
	  ;
	  ([ (assertion, Some (make_check_false ())) ], false)
    | (true,true) -> ([ ], true)
    | (false,_)  ->  ([ (right_operand_assertion (), None) ], true)


(* assertions for bitwise left/right shift signed overflow *)
let get_bitwise_shift_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    exp shiftop loperand roperand =
  (* - (1) right operand should be nonnegative and
     strictly less than the width of promoted left operand:
     now done by get_bitwise_shift_right_operand_assertion
     - (2) (since signed version) left operand should be nonnegative
     (implementation defined for right shift, undefined for left shift)
     - (3) (for signed left shift) (left operand should be nonnegative: see (2), and)
     result should be representable in result type *)
  let t = Cil.typeOf exp in
  let size = bitsSizeOf t in
  let () =
    if not(size = bitsSizeOf (Cil.typeOf loperand) && size <= 64)
      (* size of result type should be size of left (promoted) operand *)
    then (
      rte_warn "problem with bitsSize of %a: not treated" d_exp exp ;
    )
  in
  let maxValResult = (* compute greatest representable "size bits" (signed) integer *)
    get_signed_max size
(*
    let max64 = Int64.max_int
    and shiftright_value = 64 - size
    in if shiftright_value > 0 then Int64.shift_right max64 shiftright_value else max64
*)
  in let assertion_2 () =
      let term = translate_C_expr_to_term loperand
      in Logic_const.prel (Rge, term, Cil.lzero ())
     and assertion_3 () =
      let term = translate_C_expr_to_term ~cast:false exp
      in (* signed result is representable in result type if loperand times 2^roperand
	    (where loperand and roperand are nonnegative),
	    which should be equal to term (obtained with a shift),
	    is less than the maximal value for the result type *)
	(* no cast to int since we check "true result" which is an integer*)
	Logic_const.prel (Rle, term, Cil.lconstant maxValResult)
  in let problem_with_assertion_2 () =
      if simplify_constants then (
	match get_expr_val loperand with
	  | None -> (false,false)
	  | Some c64 ->
	      (* left operand is constant: check it is nonnegative *)
	      if (Int64.compare c64 Int64.zero >= 0)
	      then (true, true) (* constant operator and assertion holds *)
	      else (true,false) (* constant operator and assertion does not hold *)
      ) else (false,false)
     and problem_with_assertion_3 () =
      if simplify_constants then (
	match get_expr_val loperand, get_expr_val roperand with
	  | None,_
	  | _, None -> (false,false)
	  | Some lval64, Some rval64 ->
	      (* both operands are constant:
		 check result is representable in result type *)
	      if (Int64.compare lval64 Int64.zero <= 0) ||
		(Int64.compare rval64 (Int64.of_int 64) >= 0) then
		(true,false) (* constant operators and assertion does not hold *)
	      else
		let result_true_val =
		  Int.shift_left (Int.of_int64 lval64) (Int.of_int64 rval64)
		in
		  if Int.compare result_true_val (Int.of_int64 maxValResult) > 0
		  then (true,false) (* constant operators and assertion does not hold *)
		  else (true,true) (* constant operators and assertion holds *)
      ) else (false,false)
  in let proceed_with_assertion_3 lassert =
      if (shiftop = Shiftlt) then (
	match problem_with_assertion_3 () with
	  | (true,false) ->
	      let assertion = assertion_3 () in
		if warning then (
		  rte_warn
		    "shift assert broken (signed overflow): %a"
		    d_predicate_named assertion
		)
		;
		((assertion, Some (make_check_false ())))::lassert
	  | (true,true)  -> lassert
	  | (false,_)    -> (assertion_3 (), None)::lassert
      ) else lassert
  in
    match problem_with_assertion_2 () with
      | (true,false) ->
	  let assertion = assertion_2 () in
	    if warning then (
	      rte_warn
		"shift assert broken (left operand should be nonnegative): %a"
		d_predicate_named assertion
	    )
	    ;
	    (* do not proceed with assertion 3: left operand is negative,
	       hence result is implementation defined anyway for left shift *)
	    [ (assertion, Some (make_check_false ())) ]
      | (true,true)  -> proceed_with_assertion_3 [ ]
      | (false,_)    -> proceed_with_assertion_3 [ (assertion_2 (), None) ]


(* assertion for downcasting to a signed integer type *)
(* which is an implementation defined behavior *)
(* pre: isSigned cast_ikind (which also is an integer type) *)
let get_downcast_assertion
    ~simplify_constants:simplify_constants
    ~warning:warning
    cast_typ expr =
  let e_typ = Cil.typeOf expr
  in
    match e_typ with
      | TInt (_,_) ->
	  let szTo = bitsSizeOf cast_typ
	  and szFrom = bitsSizeOf e_typ
	  in
	    if (szTo < szFrom) then
	      (* downcast: the expression result should fit on szTo bits *)
	      let (minType,maxType) = (get_signed_min szTo, get_signed_max szTo)
	      in let term = translate_C_expr_to_term ~cast:false expr in
	      let assertion_le ()   = assertion_le term maxType
	      and assertion_ge ()   = assertion_ge term minType
	      in
	      let ceval =
		if simplify_constants then (
		  match get_expr_val expr with
		    | Some a64 -> (* constant expr *)
			Some (Int64.compare a64 minType >= 0,
			      Int64.compare a64 maxType <= 0)
		    | None -> None
		) else None
	      in match ceval with
		| None ->
		    let full_assertion () =
		      Logic_const.pand (assertion_le (), assertion_ge ())
		    in [ (full_assertion (), None) ]
		| Some (emin,emax) -> (
		    match (emin,emax) with
		      | (true,true) -> []
		      | (true,false) ->
			  let assertion  = assertion_le () in
			    if warning then
			      rte_warn
				"signed downcast assert broken: %a"
				d_predicate_named assertion
			    ;
			    [ (assertion, Some (make_check_false ())) ]
		      | (false,true) ->
			  let assertion = assertion_ge () in
			    if warning then
			      rte_warn
				"signed downcast assert broken: %a"
				d_predicate_named assertion
			    ;
			    [ (assertion_le (), Some (make_check_false ())) ]

		      | (false,false) -> assert false (* should not happen *)

		  )
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

      | TLval _tlv ->

	  self#add_tlval t ;
	  DoChildren

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
                 let add_offset lval = addTermOffsetLval t_offset lval in
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
			       ((let ntlval = addTermOffsetLval t_offset (lhost,off)
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
                         let source = Cil.source nt.term_loc in
                           rte_warn ~source
                             "Cannot substitute a non-lval \
                               parameter under an addrof operation";
                           raise AddrOfFormal
                     | _  -> tlet_or_ident ())
      | _ -> DoChildren


let replacement_visitor fa_terms ret_opt = object
  (* for each term, replace each logic_variable which
     stems from a C variable by the term corresponding
     to the variable at this point iff it is a formal *)

  inherit nopCilVisitor

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

end

let treat_pred pred fa_terms (ret_opt : term_lval option)  =
  let visitor = replacement_visitor fa_terms ret_opt in
    visitCilPredicate (visitor :> cilVisitor) pred

let treat_term trm fa_terms ret_opt =
  let visitor = replacement_visitor fa_terms ret_opt in
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

module H = Hashtbl
module HStmt = Cil_datatype.Stmt.Hashtbl

class rte_annot_visitor kf =
object(self)
  inherit Visitor.frama_c_inplace (* inplace since no Ast transformation:
				     only annotations are added *)

  val mutable skip_set = SkipIdSet.empty
  val mutable index_behavior = 0
  val bhv_index_table  : (Kernel_function.t, int) H.t = H.create 113

  val assertion_table : (predicate list) HStmt.t = HStmt.create 113

  val rte_dep_state = RteAnnotTbl.get_state kf
  val precond_dep_state = PrecondAnnotTbl.get_state kf

  val gen_optionTbl = StateManager.find_current_gen_options kf
  val other_optionTbl = StateManager.find_current_other_options kf

  method private add_to_skip_set eid skip = skip_set <- SkipIdSet.add (eid,skip) skip_set
  method private is_in_skip_set eid skip = SkipIdSet.mem (eid,skip) skip_set

  method private is_DoMemAccess () =
    Parameter_map.is_true DoMemAccess.name gen_optionTbl

  method private is_DoCalledPrecond () =
    Parameter_map.is_true DoCalledPrecond.name gen_optionTbl

  method private is_DoDivMod () =
    Parameter_map.is_true DoDivMod.name gen_optionTbl

  method private is_DoSignedOverflow () =
    Parameter_map.is_true DoSignedOverflow.name gen_optionTbl

  method private is_DoUnsignedOverflow () =
    Parameter_map.is_true DoUnsignedOverflow.name gen_optionTbl

  method private is_DoDownCast () =
    Parameter_map.is_true DoDownCast.name gen_optionTbl

  method private is_Warning () =
    Parameter_map.is_true Warn.name other_optionTbl

  method private is_ConstFold () =
    Parameter_map.is_true ConstFold.name other_optionTbl

  method private queue_stmt_spec _kf spec =
    let stmt = Extlib.the (self#current_stmt)
    in
    Queue.add
      (fun () ->
	let annot =
	  Logic_const.new_code_annotation (AStmtSpec (*stmt_*)spec)
	in
	Annotations.add
	  stmt
	  [ precond_dep_state ]
	  (Db_types.Before (Db_types.User annot))
      ) self#get_filling_actions

  method private queue_assertion ?(pos = Before) assertion_list =
    (* add an assertion (with an optionally given status) in front of current statement *)
    match assertion_list with
      | [] -> ()
      | _ ->
	let stmt = Extlib.the (self#current_stmt) in
	let kf = Extlib.the (self#current_kf) in
	let already_posted_assertions =
	  try
	    HStmt.find assertion_table stmt
	  with Not_found -> []
	in let pruned_assertion_list =
	     (* do not keep an assertion if an equivalent assertion (content)
		is already scheduled *)
	     List.rev (
	       List.fold_left
		 (fun acc (assertion, status_opt) ->
		   if not (List.exists
			     (fun p ->
			       Logic_utils.is_same_predicate
				 p assertion.content)
			     already_posted_assertions)
		   then (assertion, status_opt) :: acc
		   else acc
		 ) [] assertion_list
	     )
	   in let loc_add_assertion assertion assertion_status_opt =
	        let rte_named_assertion =
		  (* give a name to assertion in order to indicate
		     it has been generated by RTE plugin *)
		  { content = assertion.content ;
		    loc = assertion.loc ; name = [ rte_prefix ] }
	        in let annot =
		     Logic_const.new_code_annotation
		       (AAssert ([], rte_named_assertion))
	           in
		   Annotations.add
		     stmt
		     [ rte_dep_state ]
		     (match pos with
		       | Before -> Db_types.Before (Db_types.User annot)
		       | After ->
			 Db_types.After (Db_types.User annot) (* not used *));
		   let astatus = match assertion_status_opt with
		     | None -> Unknown
		     | Some checked_status -> checked_status
		   in
                   let ip = Property.ip_of_code_annot kf stmt annot in
                   List.iter (fun x -> S.set x [] astatus) ip
	      in
	      (* update scheduled assertions *)
	      let () = HStmt.replace assertion_table stmt
	        (List.rev_append
	           (List.rev_map (fun (a,_) -> a.content) pruned_assertion_list)
	           already_posted_assertions)
	      in
	      Queue.add
	        (fun () ->
		  List.iter
		    (fun (assertion, assertion_status_opt) ->
		      loc_add_assertion assertion assertion_status_opt
		    ) pruned_assertion_list
	        ) self#get_filling_actions

  method private get_current_kf () =
    Kernel_function.find_englobing_kf (Extlib.the (self#current_stmt))

  method private mk_new_behavior_name kf_callee =
    let error () =
      (* if index becomes 0 (again): serious problem, but unlikely to happen *)
      error "Generated too many behavior names." ;
      assert false
    in
    let kf = self#get_current_kf () (* get englobing kf *)
    in let known_behaviors = Kernel_function.all_function_behaviors kf
    in let rec aux_new_name i =
	let name = 
	  precond_prefix ^ "_" ^ 
	    (Kernel_function.get_name kf_callee) ^ "_" ^ (string_of_int i)
	in if List.mem name known_behaviors then (
	    let nindex = i + 1
	    in if nindex = 0 then error ()
		(* if index becomes 0 (again): serious problem, but unlikely to happen *)
	      else aux_new_name nindex
	  ) else (name, i)
    in
    let (name, new_index) =
      try
	let i = H.find bhv_index_table kf
	in
	let nindex = i+1
	in if nindex = 0 then error ()
	  else aux_new_name nindex
      with Not_found ->
	aux_new_name 0
    in
      H.replace bhv_index_table kf new_index ;
      name

  method private make_stmt_contract kf formals_actuals_terms ret_opt =
    let tret_opt =
      match ret_opt with
	| None -> None
	| Some lv -> Some (Logic_utils.lval_to_term_lval ~cast:true lv)
    in
    let fun_transform_pred p =
      let p' = Logic_const.pred_of_id_pred p in
      try
	let p_unnamed =
	  Logic_const.unamed
	    (treat_pred
	       p'.content
	       formals_actuals_terms tret_opt)
	in
        Logic_const.new_predicate 
	  { content = p_unnamed.content ;
	    loc = p_unnamed.loc ;
	    name = p'.name 
	  }
      with 
	| AddrOfFormal
	| NoResult ->
          (* A warning has been emitted, we simply ignore the predicate here. *)
          Logic_const.new_predicate Logic_const.ptrue
    and fun_transform_assigns assigns =
      (* substitute terms, then for each from extract lvals and
	 keep those and only those as froms *)
      let treat_from it =
	let nterm =
          treat_term it.it_content formals_actuals_terms tret_opt 
        in
	let visitor = tlval_fetcher_visitor () in
	let _ = visitCilTerm (visitor :> cilVisitor) nterm in
        let list_tlvals = visitor#fetch_lvals () in
	List.map
	  (fun lv -> Logic_const.new_identified_term lv) list_tlvals
      in
      let treat_identified_term_zone_froms z =
	match z with
	  | FromAny -> FromAny
	  | From l -> From (List.flatten (List.map treat_from l))
      in 
      let treat_assign (z,lz) =
	let nt =
	  treat_term
	    z.it_content formals_actuals_terms tret_opt (* should be an lval *)
	in
	match nt.term_node with
	  | TLval _ -> (* if substituted term is not an lval,
			  do not generate an assign *)
	    Some (Logic_const.new_identified_term nt,
		  treat_identified_term_zone_froms lz)
	  | _ -> None
      in 
      let treat_assigns_list acc a =
	try
	  match treat_assign a with
	    | None -> acc
	    | Some e -> e :: acc
	with
	  | AddrOfFormal 
	  | NoResult -> acc
      (* Ignore the location based on the address of a formal parameter. *)

      in 
      let treat_assigns_clause l =
	(* compute list of assigns as (terms, list of terms) ;
	   if empty list of terms => it's a Nothing, else Location ... *)
	(* then process to transform assign on \result *)
        match l with
            WritesAny -> WritesAny
          | Writes l -> Writes (List.fold_left treat_assigns_list [] l)
      in
      let final_assigns_list =
	match ret_opt with
	  | None ->
	       (* no return value: there should be no assign of \result *)
	    treat_assigns_clause assigns
	  | Some ret ->
	    let ret_type = typeOfLval ret in
	    let nlist_assigns =
		 (* if there is a assigns \at(\result,Post) \from x
		    replace by \assigns \result \from x *)
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
                WritesAny -> WritesAny
              | Writes l when
                  List.exists 
                    (fun (a,_) -> Logic_utils.is_result a.it_content) l 
                  ->
                nlist_assigns
              | Writes l -> Writes (add_assigns_result()::l)
      in treat_assigns_clause final_assigns_list
    and behaviors =
      (* calling get_spec on a function with a contract
	 but no code generates default assigns *)
      (Kernel_function.get_spec kf).spec_behavior
    in
      try
	let new_behaviors =
	  List.fold_left
	    (fun acc bhv ->
	       (mk_behavior
		  ~name:(self#mk_new_behavior_name kf)
		  ~post_cond:(List.map
				(fun (k,p) -> (k,fun_transform_pred p)) bhv.b_post_cond)
		  ~assumes:(List.map fun_transform_pred bhv.b_assumes)
		  ~requires:(List.map fun_transform_pred bhv.b_requires)
		  ~assigns:(fun_transform_assigns bhv.b_assigns)
		  ~extended:[] ())::acc
	    ) [] behaviors
	in
	if new_behaviors <> [] then
	  let spec = { spec_behavior = List.rev new_behaviors ;
		       spec_variant = None ;
		       spec_terminates = None ;
		       spec_complete_behaviors = [] ;
		       spec_disjoint_behaviors = []
		     }
	  in Some spec
	else None
      with Exit -> None

  method vinst vi =
    (* assigned left values are checked for valid access *)
    match vi with
      | Set (lval,_,_) ->
	if self#is_DoMemAccess () then
	  self#queue_assertion (get_lval_assertion lval)
	;
	DoChildren
      | Call (ret_opt,funcexp,argl,_) -> (
	if not(self#is_DoCalledPrecond ()) then
	  DoChildren
	else
	  match funcexp.enode with
	    | Lval (Var vinfo,NoOffset) ->
	      let kf =  Globals.Functions.get vinfo in
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
		match self#make_stmt_contract kf formals_actuals_terms ret_opt with
		  | None -> DoChildren
		  | Some contract_stmt ->
		    self#queue_stmt_spec kf contract_stmt
		    ;
		    DoChildren
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
      | BinOp((Div|Mod) as op,dividend,divisor,TInt(kind,_)) ->
	(* add assertion "divisor not zero" *)
	if self#is_DoDivMod () then
	  self#queue_assertion
	    (get_divmod_assertion
	       ~simplify_constants:(self#is_ConstFold ())
	       ~warning:(self#is_Warning ())
	       divisor)
	;
	if (self#is_DoSignedOverflow ()) && (op = Div) && (isSigned kind) then
	  begin
	    (* treat the special case of signed division overflow
	       (no signed modulo overflow) *)
	    self#queue_assertion
	      (get_signed_div_assertion
		 ~simplify_constants:(self#is_ConstFold ())
		 ~warning:(self#is_Warning ())
		 dividend divisor)

	  end
	;
	DoChildren

      | BinOp((Shiftlt|Shiftrt) as shiftop,loperand,roperand,TInt(kind,_)) ->

	if self#is_DoSignedOverflow () || self#is_DoUnsignedOverflow () then (
	  let (a, isOk) =
	    (* generate and check assertion on right operand of shift *)
	    get_bitwise_shift_right_operand_assertion
	      ~simplify_constants:(self#is_ConstFold ())
	      ~warning:(self#is_Warning ())
	      exp roperand
	  in
	  self#queue_assertion a ;
	  if isOk (* right operand is correct:
		     otherwise no need to proceed with other assertions *)
	  then
	    (
	      (* assertions specific to signed shift *)
	      if (self#is_DoSignedOverflow ()) && (isSigned kind) then (

		self#queue_assertion
		  (get_bitwise_shift_assertion
		     ~simplify_constants:(self#is_ConstFold ())
		     ~warning:(self#is_Warning ())
		     exp shiftop loperand roperand)
	      )
	      ;

	      (* assertions specific to unsigned shift *)
	      if self#is_DoUnsignedOverflow () &&
		(shiftop = Shiftlt) && not(isSigned kind) then (
		  self#queue_assertion
		    (get_bitwise_lshift_unsigned_assertion
		       ~simplify_constants:(self#is_ConstFold ())
		       ~warning:(self#is_Warning ())
		       exp loperand roperand)
		 )
	    )
	)
	;
	DoChildren

      | BinOp((PlusA|MinusA|Mult)
		 as op,loperand,roperand,TInt(kind,_)) when (isSigned kind) ->
	(* may be skipped if enclosing expression is a downcast to a signed type *)
	if (self#is_DoSignedOverflow ()) &&
	  not(self#is_in_skip_set exp.eid SkipBounding) then
	  self#queue_assertion
	    (get_multsubadd_assertion
	       ~simplify_constants:(self#is_ConstFold ())
	       ~warning:(self#is_Warning ())
	       exp op loperand roperand)
	;
	DoChildren

      | BinOp((PlusA|MinusA|Mult)
		 as op,loperand,roperand,TInt(kind,_)) when not(isSigned kind) ->
	if self#is_DoUnsignedOverflow () then
	  self#queue_assertion
	    (get_multsubadd_unsigned_assertion
	       ~simplify_constants:(self#is_ConstFold ())
	       ~warning:(self#is_Warning ())
	       exp op loperand roperand)
	;
	DoChildren

      | UnOp(Neg,operand,TInt(kind,_)) when (isSigned kind) ->
	if self#is_DoSignedOverflow () then
	  self#queue_assertion
	    (get_uminus_assertion
	       ~simplify_constants:(self#is_ConstFold ())
	       ~warning:(self#is_Warning ())
	       operand)
	;
	DoChildren

      (* Note: if unary minus on unsigned integer is to be understood as
	 "subtracting the promoted value from the largest value
	 of the promoted type and adding one"
	 the result is always representable so no overflow
       *)

      | Lval lval ->
	(* left values are checked for valid access *)
	if self#is_DoMemAccess () then (
	  debug "exp %a is an lval: validity of potential mem access checked\n"
	    Cil.d_exp exp ;
	  self#queue_assertion (get_lval_assertion lval)
	)
	;
	DoChildren

      | CastE (TInt (kind,_) as typ, e) when (isSigned kind) ->
	if self#is_DoDownCast () then (
	  let downcast_asserts =
	    get_downcast_assertion
	      ~simplify_constants:(self#is_ConstFold ())
	      ~warning:(self#is_Warning ())
	      typ e
	  in match downcast_asserts with
	    | [] -> ()
	    | _ ->
	      self#queue_assertion downcast_asserts
	      ;
	      self#add_to_skip_set e.eid SkipBounding
	(* expression should be skipped w.r.t
	   signed mult/add/sub arithmetic overflow *)
	)
	;
	DoChildren

      | StartOf _lval ->
	if self#is_DoMemAccess () then
	  self#queue_assertion
	    [ (Logic_const.pvalid (translate_C_expr_to_term ~cast:false exp), None) ]
	;
	DoChildren

      | AddrOf _
      | Info _
      | UnOp _
      | Const _
      | CastE _
      | BinOp _ -> DoChildren


      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | AlignOfE _ -> SkipChildren

end

let remove_annotations_kf kf =
  (* remove annotations dependant on function (in fact all dependencies*)
  let s = RteGlobalTbl.get_state kf in
    Project.clear ~selection:(State_selection.Dynamic.only_dependencies s) ()

let is_computed_kf kf =
  (* check whether annotations are computed for function kf
     for the selected set of command-line options *)
  match kf.fundec with
    | Declaration _ -> true
    | Definition _ ->
	(* check whether options have changed for function kf *)
	let old_gen_opts = StateManager.find_current_gen_options kf
	and new_gen_opts = Parameter_map.gen_from_command_line_options ()
	and old_other_opts = StateManager.find_current_other_options kf
	and new_other_opts = Parameter_map.other_from_command_line_options ()
	in
	  (* case 1: generating options have changed *)
	  Parameter_map.compare old_gen_opts new_gen_opts <> 0
	||
	  (* case 2: no generating option has changed, but the user wants to
	     generate the same annotations with const folding, warning
	     enabled ... *)
	  (Parameter_map.is_one_true ~except:None new_gen_opts &&
	     Parameter_map.compare old_other_opts new_other_opts <> 0)

let annotate_kf_with kf new_gen_opts new_other_opts =
  (* generates annotation for function kf
     on the basis of options given as arguments
     (which may differ from command-line options)
  *)
  let do_annotate_kf kf =
    (* annotate function kf with option selected in StateManager *)
    match kf.fundec with
      | Declaration _ -> ()
      | Definition (f,_) ->
	  remove_annotations_kf kf ;
	  let vis = new rte_annot_visitor kf
	    (* (StateManager.find_current_gen_options kf) *)
	  in let _nkf = Visitor.visitFramacFunction vis f in
	    assert(_nkf == f)
  in
  let old_gen_opts = StateManager.find_current_gen_options kf
  and old_other_opts = StateManager.find_current_other_options kf
  in
    if
      (* case 1: generating options have changed *)
      Parameter_map.compare old_gen_opts new_gen_opts <> 0
      ||
	(* case 2: no generating option has changed,
	   but the user wants to generate the same annotations with const
	   folding, warning enabled ... *)
	(Parameter_map.is_one_true ~except:None new_gen_opts &&
	   Parameter_map.compare old_other_opts new_other_opts <> 0)
    then begin
      (* options have changed for function kf: there is some work to do *)
      debug "Options have changed: something to do for function %s"
	(Kernel_function.get_name kf) ;
      (* update analysis options for function kf *)
      StateManager.FuncOptionTbl.replace kf (new_gen_opts, new_other_opts) ;
      if Parameter_map.is_one_true ~except:None new_gen_opts then begin
	(* there may be new annotations to add and remove *)
	feedback "annotating function %s" (Kernel_function.get_name kf) ;
	do_annotate_kf kf ;
	(* set status of rte/precond generation (see properties_status.mli) *)
	List.iter
	  (fun (opt_name, _opt_get, _, property_set) ->
	     property_set kf (Parameter_map.is_true opt_name new_gen_opts))
	  Parameter_map.generating_opts
      end else begin
	(* all annotations should be removed *)
	(* and RTE_Generated / Called_Precond_Generated are reset *)
	feedback "unannotating function %s" (Kernel_function.get_name kf) ;
	(* reset status of rte/precond generation *)
	List.iter
	  (fun (_opt_name, _opt_get, _, property_set) ->
	     property_set kf false)
	  Parameter_map.generating_opts;
	remove_annotations_kf kf
      end
    end

let do_precond kf =
  (* annotate call sites with contracts, for a given function *)
  let current_gen_options = StateManager.find_current_gen_options kf in
  let new_gen_options = Parameter_map.set_precond current_gen_options in
    annotate_kf_with kf new_gen_options (StateManager.find_current_other_options kf)

let do_all_rte kf =
  (* annonate for all rte + unsigned overflows (which are not rte), for a given function *)
  let current_gen_options = StateManager.find_current_gen_options kf in
  let new_gen_options =
    Parameter_map.set_unsignedOv
      (Parameter_map.set_all_rte current_gen_options) in
    annotate_kf_with kf new_gen_options (StateManager.find_current_other_options kf)

let annotate_kf kf =
  (* generates annotation for function kf on the basis of command-line options *)
  debug "launching annotate\n";
  (* add annotations *)
  match kf.fundec with
  | Declaration _ -> ()
  | Definition _ ->
    (* check whether options have changed for function kf *)
      annotate_kf_with kf
	(Parameter_map.gen_from_command_line_options ())
	(Parameter_map.other_from_command_line_options ())

let is_computed () =
  (* check whether annotations are computed for the
     set of functions selected through the command-line option,
     and for the selected set of command-line options *)
  if not (Ast.is_computed ()) then false
  else
    let include_function kf =
      let fsel = FunctionSelection.get () in
      Datatype.String.Set.is_empty fsel
      || let name = Kernel_function.get_name kf in
	 Datatype.String.Set.mem name fsel
    in
      try
	Globals.Functions.iter
	  (fun kf ->
	     if include_function kf then
	       begin
		 match kf.fundec with
		   | Declaration _ -> ()
		   | Definition _ -> if not(is_computed_kf kf) then raise Exit
	       end
	  )
	;
	true
      with Exit -> false

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
    Db.Journalization_not_required
    Db.RteGen.is_computed_kf
    is_computed_kf

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.is_computed
    is_computed

let () =
  Db.register
    (Db.Journalize
       ("RteGen.compute", Datatype.func Datatype.unit Datatype.unit))
    Db.RteGen.compute
    compute

(* dependencies for annotations and annot. status are handled on a per function
   basis (dep. on bindings of RteAnnotTbl/PrecondAnnotTbl) *)

(* but add dependency on properties status using proxy *)
(* removed: RTE_Status_Proxy does not exist anymore *)

(*
let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Properties_status.RTE_Status_Proxy.self
    [ RteAnnotTbl.self ] ;
  State_dependency_graph.Static.add_codependencies
    ~onto:Properties_status.Precond_Status_Proxy.self
    [ PrecondAnnotTbl.self ]
*)

let () =
  Db.register
    (Db.Journalize
       ("RteGen.do_precond", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.do_precond
    do_precond
(*
let do_precond =
  Dynamic.register
    ~plugin:"rte"
    "do_precond"
    (Datatype.func
       Kernel_function.ty
       Datatype.unit)
    ~journalize:true
    do_precond
*)

let () =
  Db.register
    (Db.Journalize
       ("RteGen.do_all_rte", Datatype.func Kernel_function.ty Datatype.unit))
    Db.RteGen.do_all_rte
    do_all_rte
(*
let do_all_rte =
  Dynamic.register
    ~plugin:"rte"
    "do_all_rte"
    (Datatype.func
       Kernel_function.ty
       Datatype.unit)
    ~journalize:true
    do_all_rte
*)

module KF = struct
  include Kernel_function
  let label = None
end

module T =
  Datatype.Triple
    (State)
    (Datatype.Function(KF)(State))
    (Datatype.Function(KF)(Datatype.Bool))

module M1 = RTE_Signed_Generated
module M2 = RTE_MemAccess_Generated
module M3 = RTE_DivMod_Generated
module M4 = RTE_DownCast_Generated
module M5 = Called_Precond_Generated
module M6 = RTE_UnsignedOverflow_Generated

let get_all_status () =
  [ (M1.self, M1.get_state, M1.get);
    (M2.self, M2.get_state, M2.get);
    (M3.self, M3.get_state, M3.get);
    (M4.self, M4.get_state, M4.get);
    (M5.self, M5.get_state, M5.get);
    (M6.self, M6.get_state, M6.get);
  ]

let () =
  Db.register
    Db.Journalization_not_required    
    Db.RteGen.get_all_status
    get_all_status

let get_precond_status () =
  (M5.self, M5.get_state, M5.get)

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_precond_status
    get_precond_status

let get_signedOv_status () =
  (M1.self, M1.get_state, M1.get)

let () =
  Db.register
    Db.Journalization_not_required    
    Db.RteGen.get_signedOv_status
    get_signedOv_status

let get_divMod_status () =
  (M3.self, M3.get_state, M3.get)

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_divMod_status
    get_divMod_status

let get_downCast_status () =
  (M4.self, M4.get_state, M4.get)

let () =
  Db.register
    Db.Journalization_not_required    
    Db.RteGen.get_downCast_status
    get_downCast_status

let get_memAccess_status () =
  (M2.self, M2.get_state, M2.get)

let () =
  Db.register
    Db.Journalization_not_required    
    Db.RteGen.get_memAccess_status
    get_memAccess_status

let get_unsignedOv_status () =
  (M6.self, M6.get_state, M6.get)

let () =
  Db.register
    Db.Journalization_not_required
    Db.RteGen.get_unsignedOv_status
    get_unsignedOv_status


(* dynamic registration *)

(* all rte + precond *)
let get_global_state =
  Dynamic.register
    ~plugin:"rte"
    "get_global_state"
    (Datatype.func
       Kernel_function.ty
       State.ty)
    ~journalize:true
    RteGlobalTbl.get_state

(* only rte (no precond) *)
let get_rte_state =
  Dynamic.register
    ~plugin:"rte"
    "get_rte_state"
    (Datatype.func
       Kernel_function.ty
       State.ty)
    ~journalize:true
    RteAnnotTbl.get_state

(* only precond (no rte) *)
let get_precond_state =
  Dynamic.register
    ~plugin:"rte"
    "get_precond_state"
    (Datatype.func
       Kernel_function.ty
       State.ty)
    ~journalize:true
    PrecondAnnotTbl.get_state

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* Implement Figure 4 of J. Signoles' JFLA'15 paper "Rester statique pour
   devenir plus rapide, plus précis et plus mince". *)

let dkey = Options.dkey_typing

let compute_quantif_guards_ref
    : (predicate -> logic_var list -> predicate ->
       (term * relation * logic_var * relation * term) list) ref
    = Extlib.mk_fun "compute_quantif_guards_ref"

(******************************************************************************)
(** Datatype and constructor *)
(******************************************************************************)

type integer_ty =
  | Gmp
  | C_type of ikind
  | Other

let gmp = Gmp
let c_int = C_type IInt
let ikind ik = C_type ik
let other = Other

include Datatype.Make
(struct
  type t = integer_ty
  let name = "E_ACSL.New_typing.t"
  let reprs = [ Gmp; c_int ]
  include Datatype.Undefined

  let compare ty1 ty2 = match ty1, ty2 with
    | C_type i1, C_type i2 ->
      if i1 = i2 then 0
      else if Cil.intTypeIncluded i1 i2 then -1 else 1
    | (Other | C_type _), Gmp | Other, C_type _ -> -1
    | Gmp, (C_type _ | Other) | C_type _, Other -> 1
    | Gmp, Gmp | Other, Other -> 0

  let equal = Datatype.from_compare

  let pretty fmt = function
    | Gmp -> Format.pp_print_string fmt "GMP"
    | C_type k -> Printer.pp_ikind fmt k
    | Other -> Format.pp_print_string fmt "OTHER"
 end)

(******************************************************************************)
(** Basic operations *)
(******************************************************************************)

let join ty1 ty2 = match ty1, ty2 with
  | Other, Other -> Other
  | Other, (Gmp | C_type _) | (Gmp | C_type _), Other ->
    Options.fatal "[typing] join failure: integer and non integer type"
  | Gmp, _ | _, Gmp -> Gmp
  | C_type i1, C_type i2 ->
    if Options.Gmp_only.get () then Gmp
    else
      let ty = Cil.arithmeticConversion (TInt(i1, [])) (TInt(i2, [])) in
      match ty with
      | TInt(i, _) -> C_type i
      | _ ->
        Options.fatal "[typing] join failure: unexpected result %a"
          Printer.pp_typ ty

exception Not_an_integer
let typ_of_integer_ty = function
  | Gmp -> Gmpz.t ()
  | C_type ik -> TInt(ik, [])
  | Other -> raise Not_an_integer

(******************************************************************************)
(** Memoization *)
(******************************************************************************)

type computed_info =
    { ty: t;  (* type required for the term *)
      op: t; (* type required for the operation *)
      cast: t option; (* if not [None], type of the context which the term
                         must be casted to. If [None], no cast needed. *)
    }

(* Memoization module which retrieves the computed info of some terms. If the
   info is already computed for a term, it is never recomputed *)
module Memo: sig
  val memo: (term -> computed_info) -> term -> computed_info
  val get: term -> computed_info
  val clear: unit -> unit
end = struct

  module H = Hashtbl.Make(struct
    type t = term
    (* the comparison over terms is the physical equality. It cannot be the
       structural one (given by [Cil_datatype.Term.equal]) because the very
       same term can be used in 2 different contexts which lead to different
       casts.

       By construction, there are no physically equal terms in the AST
       built by Cil. Consequently the memoisation should be fully
       useless. However the translation of E-ACSL guarded quantification
       generates new terms (see module {!Quantif}) which must be typed. The term
       corresponding to the bound variable [x] is actually used twice: once in
       the guard and once for encoding [x+1] when incrementing it. The
       memoization is only useful here and indeed prevent the generation of
       one extra variable in some cases. *)
    let equal (t1:term) t2 = t1 == t2
    let hash = Cil_datatype.Term.hash
  end)

  let tbl = H.create 97

  let get t =
    try H.find tbl t
    with Not_found ->
      Options.fatal
        "[typing] type of term '%a' was never computed."
        Printer.pp_term t

  let memo f t =
    try H.find tbl t
    with Not_found ->
      let x = f t in
      H.add tbl t x;
      x

  let clear () = H.clear tbl

end

(******************************************************************************)
(** {2 Coercion rules} *)
(******************************************************************************)

let ty_of_logic_ty = function
  | Linteger -> Gmp
  | Ctype ty -> (match Cil.unrollType ty with
    | TInt(ik, _) -> C_type ik
    | _ -> Other)
  | Lreal | Larrow _ -> Other
  | Ltype _ -> Error.not_yet "user-defined logic type"
  | Lvar _ -> Error.not_yet "type variable"

(* Compute the smallest type (bigger than [int]) which can contain the whole
   interval. It is the \theta operator of the JFLA's paper. *)
let ty_of_interv ?ctx i =
  try
    let itv_kind =
      if Ival.is_bottom i then IInt
      else match Ival.min_and_max i with
        | Some l, Some u ->
          let is_pos = Integer.ge l Integer.zero in
          let lkind = Cil.intKindForValue l is_pos in
          let ukind = Cil.intKindForValue u is_pos in
          (* kind corresponding to the interval *)
          if Cil.intTypeIncluded lkind ukind then ukind else lkind
        | None, None -> raise Cil.Not_representable (* GMP *)
        | None, Some _ | Some _, None ->
          Kernel.fatal ~current:true "ival: %a" Ival.pretty i
    in
    (* convert the kind to [IInt] whenever smaller. *)
    let kind = if Cil.intTypeIncluded itv_kind IInt then IInt else itv_kind in
    (* ctx type whenever possible to prevent superfluous casts in the generated
       code *)
    (match ctx with
     | None | Some (Gmp | Other) -> C_type kind
     | Some (C_type ik as ctx) ->
       if Cil.intTypeIncluded itv_kind ik then ctx else C_type kind)
  with Cil.Not_representable ->
    Gmp

(* compute a new {!computed_info} by coercing the given type [ty] to the given
   context [ctx]. [op] is the type for the operator. *)
let coerce ~arith_operand ~ctx ~op ty =
  if compare ty ctx = 1 then begin
    (* type larger than the expected context,
       so we must introduce an explicit cast *)
    { ty; op; cast = Some ctx }
  end else
    (* only add an explicit cast if the context is [Gmp] and [ty] is not;
       or if the term corresponding to [ty] is an operand of an arithmetic
       operation which must be explicitely coerced in order to force the
       operation to be of the expected type. *)
    if (ctx = Gmp && ty <> Gmp) || arith_operand
    then { ty; op; cast = Some ctx }
    else { ty; op; cast = None }

(* the integer_ty corresponding to [t] whenever use as an offset.
   In that case, it cannot be a GMP, so it must be coerced to an integral type
   in that case *)
let offset_ty t =
  try
    let i = Interval.infer t in
    match ty_of_interv i with
    | Gmp -> C_type ILongLong (* largest possible type *)
    | ty -> ty
  with Interval.Not_an_integer ->
    Options.fatal "expected an integral type for %a" Printer.pp_term t

(******************************************************************************)
(** {2 Type system} *)
(******************************************************************************)

(* generate a context [c]. Take --e-acsl-gmp-only into account iff [use_gmp_opt]
   is true. *)
let mk_ctx ~use_gmp_opt = function
  | Other | Gmp as c -> c
  | C_type _ as c -> if use_gmp_opt && Options.Gmp_only.get () then Gmp else c

(* type the term [t] in a context [ctx] by taking --e-acsl-gmp-only into account
   iff [use_gmp_opt] is true. *)
let rec type_term ~use_gmp_opt ?(arith_operand=false) ?ctx t =
  let ctx = Extlib.opt_map (mk_ctx ~use_gmp_opt) ctx in
  let dup ty = ty, ty in
  let compute_ctx ?ctx i =
    (* in order to get a minimal amount of generated casts for operators, the
       result is typed in the given context [ctx], but not the operands.
       This function returns a tuple (ctx_of_result, ctx_of_operands) *)
    match ctx with
    | None ->
      (* no context: factorize *)
      dup (mk_ctx ~use_gmp_opt:true (ty_of_interv i))
    | Some ctx ->
      mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx i),
      mk_ctx ~use_gmp_opt:true (ty_of_interv i)
  in
  let infer t =
    Cil.CurrentLoc.set t.term_loc;
    (* this pattern matching implements the formal rules of the JFLA's paper
       (and of course also covers the missing cases). Also enforce the invariant
       that every subterm is typed, even if it is not an integer. *)
    match t.term_node with
    | TConst (Integer _ | LChr _ | LEnum _)
    | TSizeOf _
    | TSizeOfStr _
    | TAlignOf _ ->
      let ty =
        try
          let i = Interval.infer t in
          ty_of_interv ?ctx i
        with Interval.Not_an_integer ->
          Other
      in
      dup ty
    | TLval tlv ->
      let ty =
        try
          let i = Interval.infer t in
          ty_of_interv ?ctx i
        with Interval.Not_an_integer ->
          Other
      in
      type_term_lval tlv;
      dup ty

    | Toffset(_, t')
    | Tblock_length(_, t')
    | TSizeOfE t'
    | TAlignOfE t' ->
      let ty =
        try
          let i = Interval.infer t in
          (* [t'] must be typed, but it is a pointer *)
          ignore (type_term ~use_gmp_opt:true ~ctx:Other t');
          ty_of_interv ?ctx i
        with Interval.Not_an_integer ->
          assert false (* this term is an integer *)
      in
      dup ty

    | TBinOp (MinusPP, t1, t2) ->
      let ty =
        try
          let i = Interval.infer t in
          (* [t1] and [t2] must be typed, but they are pointers *)
          ignore (type_term ~use_gmp_opt:true ~ctx:Other t1);
          ignore (type_term ~use_gmp_opt:true ~ctx:Other t2);
          ty_of_interv ?ctx i
        with Interval.Not_an_integer ->
          assert false (* this term is an integer *)
      in
      dup ty

    | TUnOp (unop, t') ->
      let ctx_res, ctx =
        try
          let i = Interval.infer t in
          let i' = Interval.infer t' in
          compute_ctx ?ctx (Ival.join i i')
        with Interval.Not_an_integer ->
          dup Other (* real *)
      in
      ignore (type_term ~use_gmp_opt:true ~arith_operand:true ~ctx t');
      (match unop with
      | LNot -> c_int, ctx_res (* converted into [t == 0] in case of GMP *)
      | Neg | BNot -> dup ctx_res)

    | TBinOp((PlusA | MinusA | Mult | Div | Mod | Shiftlt | Shiftrt), t1, t2) ->
      let ctx_res, ctx =
        try
          let i = Interval.infer t in
          let i1 = Interval.infer t1 in
          let i2 = Interval.infer t2 in
          compute_ctx ?ctx (Ival.join i (Ival.join i1 i2))
        with Interval.Not_an_integer ->
          dup Other (* real *)
      in
      (* it is enough to explicitely coerce when required one operand to [ctx]
         (through [arith_operand]) in order to force the type of the operation.
         Heuristic: coerce the operand which is not a lval in order to lower
         the number of explicit casts *)
      let rec cast_first t1 t2 = match t1.term_node with
        | TLval _ -> false
        | TLogic_coerce(_, t) -> cast_first t t2
        | _ -> true
      in
      let cast_first = cast_first t1 t2 in
      ignore (type_term ~use_gmp_opt:true ~arith_operand:cast_first ~ctx t1);
      ignore
        (type_term ~use_gmp_opt:true ~arith_operand:(not cast_first) ~ctx t2);
      dup ctx_res

    | TBinOp ((Lt | Gt | Le | Ge | Eq | Ne), t1, t2) ->
      assert (match ctx with None -> true | Some c -> compare c c_int >= 0);
      let ctx =
        try
          let i1 = Interval.infer t1 in
          let i2 = Interval.infer t2 in
          mk_ctx ~use_gmp_opt:true (ty_of_interv ?ctx (Ival.join i1 i2))
        with Interval.Not_an_integer ->
          Other
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t1);
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      let ty = match ctx with
        | Other -> c_int
        | Gmp | C_type _ -> ctx
      in
      c_int, ty

    | TBinOp ((LAnd | LOr), t1, t2) ->
      let ty =
        try
          let i1 = Interval.infer t1 in
          let i2 = Interval.infer t2 in
          ty_of_interv ?ctx (Ival.join i1 i2)
        with Interval.Not_an_integer ->
          Other
      in
      (* both operands fit in an int. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int t1);
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int t2);
      dup ty

    | TBinOp (BAnd, _, _) -> Error.not_yet "bitwise and"
    | TBinOp (BXor, _, _) -> Error.not_yet "bitwise xor"
    | TBinOp (BOr, _, _) -> Error.not_yet "bitwise or"

    | TCastE(_, t')
    | TCoerce(t', _) ->
      let ctx =
        try
          (* compute the smallest interval from the whole term [t] *)
          let i = Interval.infer t in
          (* nothing more to do: [i] is already more precise than what we
             could infer from the arguments of the cast. *)
          ty_of_interv ?ctx i
        with Interval.Not_an_integer ->
          Other
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t');
      dup ctx

    | Tif (t1, t2, t3) ->
      let ctx1 =
        mk_ctx ~use_gmp_opt:false c_int (* an int must be generated *)
      in
      ignore (type_term ~use_gmp_opt:false ~ctx:ctx1 t1);
      let i = Interval.infer t in
      let ctx =
        try
          let i2 = Interval.infer t2 in
          let i3 = Interval.infer t3 in
          let ctx = ty_of_interv ?ctx (Ival.join i (Ival.join i2 i3)) in
          mk_ctx ~use_gmp_opt:true ctx
        with Interval.Not_an_integer ->
          Other
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      ignore (type_term ~use_gmp_opt:true ~ctx t3);
      dup ctx

    | Tat (t, _)
    | TLogic_coerce (_, t) ->
      dup (type_term ~use_gmp_opt ~arith_operand ?ctx t).ty

    | TCoerceE (t1, t2) ->
      let ctx =
        try
          let i = Interval.infer t in
          let i1 = Interval.infer t1 in
          let i2 = Interval.infer t2 in
          ty_of_interv ?ctx (Ival.join i (Ival.join i1 i2))
        with Interval.Not_an_integer ->
          Other
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t1);
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      dup ctx

    | TAddrOf tlv
    | TStartOf tlv ->
      (* it is a pointer, but subterms must be typed. *)
      type_term_lval tlv;
      dup Other

    | Tbase_addr (_, t) ->
      (* it is a pointer, but subterms must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Other t);
      dup Other

    | TBinOp ((PlusPI | IndexPI | MinusPI), t1, t2) ->
      (* both [t1] and [t2] must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Other t1);
      let ctx = offset_ty t2 in
      ignore (type_term ~use_gmp_opt:false ~ctx t2);
      dup Other

    | Tapp(li, _, args) ->
      let typ_arg lvi arg =
        let ctx = ty_of_logic_ty lvi.lv_type in
        ignore (type_term ~use_gmp_opt:false ~ctx arg)
      in
      List.iter2 typ_arg li.l_profile args;
      (* [li.l_type is [None] for predicate only: not possible here.
         Thus using [Extlib.the] is fine *)
      dup (ty_of_logic_ty (Extlib.the li.l_type))

    | Tunion _ -> Error.not_yet "tset union"
    | Tinter _ -> Error.not_yet "tset intersection"
    | Tcomprehension (_,_,_) -> Error.not_yet "tset comprehension"
    | Trange (_,_) -> Error.not_yet "trange"
    | Tlet (_,_) -> Error.not_yet "let binding"
    | Tlambda (_,_) -> Error.not_yet "lambda"
    | TDataCons (_,_) -> Error.not_yet "datacons"
    | TUpdate (_,_,_) -> Error.not_yet "update"

    | Tnull
    | TConst (LStr _ | LWStr _ | LReal _)
    | Ttypeof _
    | Ttype _
    | Tempty_set  -> dup Other
  in
  Memo.memo
    (fun t ->
      let ty, op = infer t in
      match ctx with
      | None -> { ty; op; cast = None }
      | Some ctx -> coerce ~arith_operand ~ctx ~op ty)
    t

and type_term_lval (host, offset) =
  type_term_lhost host;
  type_term_offset offset

and type_term_lhost = function
  | TVar _
  | TResult _ -> ()
  | TMem t -> ignore (type_term ~use_gmp_opt:false ~ctx:Other t)

and type_term_offset = function
  | TNoOffset -> ()
  | TField(_, toff)
  | TModel(_, toff) -> type_term_offset toff
  | TIndex(t, toff) ->
    let ctx = offset_ty t in
    ignore (type_term ~use_gmp_opt:false ~ctx t);
    type_term_offset toff

let rec type_predicate p =
  Cil.CurrentLoc.set p.pred_loc;
  (* this pattern matching also follows the formal rules of the JFLA's paper *)
  let op = match p.pred_content with
    | Pfalse | Ptrue -> c_int
    | Papp _ -> Error.not_yet "logic function application"
    | Pseparated _ -> Error.not_yet "\\separated"
    | Pdangling _ -> Error.not_yet "\\dangling"
    | Prel(_, t1, t2) ->
      let ctx =
        try
          let i1 = Interval.infer t1 in
          let i2 = Interval.infer t2 in
          let i = Ival.join i1 i2 in
          mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx:c_int i)
        with Interval.Not_an_integer ->
          Other
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t1);
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      (match ctx with
      | Other -> c_int
      | Gmp | C_type _ -> ctx)
    | Pand(p1, p2)
    | Por(p1, p2)
    | Pxor(p1, p2)
    | Pimplies(p1, p2)
    | Piff(p1, p2) ->
      ignore (type_predicate p1);
      ignore (type_predicate p2);
      c_int
    | Pnot p ->
      ignore (type_predicate p);
      c_int
    | Pif(t, p1, p2) ->
      let ctx = mk_ctx ~use_gmp_opt:false c_int in
      ignore (type_term ~use_gmp_opt:false ~ctx t);
      ignore (type_predicate p1);
      ignore (type_predicate p2);
      c_int
    | Plet _ -> Error.not_yet "let _ = _ in _"

    | Pforall(bounded_vars, { pred_content = Pimplies(hyps, goal) })
    | Pexists(bounded_vars, { pred_content = Pand(hyps, goal) }) ->
      let guards = !compute_quantif_guards_ref p bounded_vars hyps in
      List.iter
        (fun (t1, r1, x, r2, t2) ->
          let i1 = Interval.infer t1 in
          let i1 = match r1 with
            | Rlt -> Ival.add_singleton_int Integer.one i1
            | Rle -> i1
            | _ -> assert false
          in
          let i2 = Interval.infer t2 in
          (* add one to [i2], since we increment the loop counter one more
             time before going outside the loop. *)
          let i2 = match r2 with
            | Rlt -> i2
            | Rle -> Ival.add_singleton_int Integer.one i2
            | _ -> assert false
          in
          let i = Ival.join i1 i2 in
          let ctx = match x.lv_type with
            | Linteger -> mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx:Gmp i)
            | Ctype ty ->
              (match Cil.unrollType ty with
              | TInt(ik, _) -> mk_ctx ~use_gmp_opt:true (C_type ik)
              | ty ->
                Options.fatal "unexpected type %a for quantified variable %a"
                  Printer.pp_typ ty
                  Printer.pp_logic_var x)
            | lty ->
              Options.fatal "unexpected type %a for quantified variable %a"
                Printer.pp_logic_type lty
                Printer.pp_logic_var x
          in
          (* forcing when typing bounds prevents to generate an extra useless
             GMP variable when --e-acsl-gmp-only *)
          ignore (type_term ~use_gmp_opt:false ~ctx t1);
          ignore (type_term ~use_gmp_opt:false ~ctx t2);
          (* if we must generate GMP code, degrade the interval in order to
             guarantee that [x] will be a GMP when typing the goal *)
          let i = match ctx with
            | C_type _ -> i
            | Gmp -> Ival.inject_range None None (* [ -\infty; +\infty ] *)
            | Other -> assert false
          in
          Interval.Env.add x i)
        guards;
      (type_predicate goal).ty

    | Pinitialized(_, t)
    | Pfreeable(_, t)
    | Pallocable(_, t)
    | Pvalid(_, t)
    | Pvalid_read(_, t)
    | Pvalid_function t ->
      ignore (type_term ~use_gmp_opt:false ~ctx:Other t);
      c_int

    | Pforall _ -> Error.not_yet "unguarded \\forall quantification"
    | Pexists _ -> Error.not_yet "unguarded \\exists quantification"
    | Pat(p, _) -> (type_predicate p).ty
    | Pfresh _ -> Error.not_yet "\\fresh"
    | Psubtype _ -> Error.not_yet "subtyping relation" (* Jessie specific *)
  in
  coerce ~arith_operand:false ~ctx:c_int ~op c_int

let type_term ~use_gmp_opt ?ctx t =
  Options.feedback ~dkey ~level:4 "typing term '%a' in ctx '%a'."
    Printer.pp_term t (Pretty_utils.pp_opt pretty) ctx;
  ignore (type_term ~use_gmp_opt ?ctx t)

let type_named_predicate ?(must_clear=true) p =
  Options.feedback ~dkey ~level:3 "typing predicate '%a'."
    Printer.pp_predicate p;
  if must_clear then begin
    Interval.Env.clear ();
    Memo.clear ()
  end;
  ignore (type_predicate p)

let unsafe_set t ?ctx ty =
  let ctx = match ctx with None -> ty | Some ctx -> ctx in
  let mk _ = coerce ~arith_operand:false ~ctx ~op:ty ty in
  ignore (Memo.memo mk t)

(******************************************************************************)
(** {2 Getters} *)
(******************************************************************************)

let get_integer_ty t = (Memo.get t).ty
let get_integer_op t = (Memo.get t).op
let get_integer_op_of_predicate p = (type_predicate p).op

(* {!typ_of_integer}, but handle the not-integer cases. *)
let extract_typ t ty =
  try typ_of_integer_ty ty
  with Not_an_integer ->
    let lty = t.term_type in
    if Cil.isLogicRealType lty then TFloat(FLongDouble, [])
    else if Cil.isLogicFloatType lty then Logic_utils.logicCType lty
    else
      Kernel.fatal "unexpected types %a and %a for term %a"
        Printer.pp_logic_type lty
        pretty ty
        Printer.pp_term t

let get_typ t =
  let info = Memo.get t in
  extract_typ t info.ty

let get_op t =
  let info = Memo.get t in
  extract_typ t info.op

let get_cast t =
  let info = Memo.get t in
  try Extlib.opt_map typ_of_integer_ty info.cast
  with Not_an_integer -> None

let get_cast_of_predicate p =
  let info = type_predicate p in
  try Extlib.opt_map typ_of_integer_ty info.cast
  with Not_an_integer -> assert false

let clear = Memo.clear

(*
Local Variables:
compile-command: "make"
End:
*)

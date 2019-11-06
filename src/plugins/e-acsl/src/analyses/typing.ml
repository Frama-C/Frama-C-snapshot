(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

type number_ty =
  | C_integer of ikind
  | C_float of fkind
  | Gmpz
  | Rational
  | Real
  | Nan

let ikind ik = C_integer ik
let c_int = ikind IInt
let gmpz = Gmpz
let fkind fk = C_float fk
let rational = Rational
let nan = Nan

module D =
  Datatype.Make_with_collections
    (struct
      type t = number_ty
      let name = "E_ACSL.Typing.t"
      let reprs = [ Gmpz; Real; Nan; c_int ]
      include Datatype.Undefined

      let compare ty1 ty2 =
        if ty1 == ty2 then 0
        else
          match ty1, ty2 with
          | C_integer i1, C_integer i2 ->
            if i1 = i2 then 0
            else if Cil.intTypeIncluded i1 i2 then -1 else 1
          | C_float f1, C_float f2 ->
            Transitioning.Stdlib.compare f1 f2
          | (C_integer _ | C_float _ | Gmpz | Rational | Real), Nan
          | (C_integer _ | C_float _ | Gmpz | Rational ), Real
          | (C_integer _ | C_float _ | Gmpz), Rational
          | (C_integer _ | C_float _), Gmpz
          | C_integer _, C_float _ ->
            -1
          | (C_float _ | Gmpz | Rational | Real | Nan), C_integer _
          | (Gmpz | Rational | Real | Nan), C_float _
          | (Rational | Real | Nan), Gmpz
          | (Real | Nan), Rational
          | Nan, Real ->
            1
          | Gmpz, Gmpz
          | Rational, Rational
          | Real, Real
          | Nan, Nan ->
            assert false

      let equal = Datatype.from_compare

      let hash = function
        | C_integer ik -> 7 * Hashtbl.hash ik
        | C_float fk -> 97 * Hashtbl.hash fk
        | Gmpz -> 787
        | Rational -> 907
        | Real -> 1011
        | Nan -> 1277

      let pretty fmt = function
        | C_integer k -> Printer.pp_ikind fmt k
        | C_float k -> Printer.pp_fkind fmt k
        | Gmpz -> Format.pp_print_string fmt "Gmpz"
        | Rational -> Format.pp_print_string fmt "Rational"
        | Real -> Format.pp_print_string fmt "Real"
        | Nan -> Format.pp_print_string fmt "Nan"
    end)

(******************************************************************************)
(** Basic operations *)
(******************************************************************************)

let join_cty ty1 ty2 =
  let ty = Cil.arithmeticConversion ty1 ty2 in
  match ty with
  | TInt(i, _) -> C_integer i
  | TFloat(f, _) -> C_float f
  | _ ->
    Options.fatal "[typing] join failure: unexpected result %a"
      Printer.pp_typ ty

let join ty1 ty2 =
  if ty1 == ty2 then ty1
  else
    match ty1, ty2 with
    | Nan, Nan | Real, Real | Rational, Rational | Gmpz, Gmpz ->
      assert false
    | Nan, (C_integer _ | C_float _ | Gmpz | Rational | Real as ty)
    | (C_integer _ | C_float _ | Gmpz | Rational | Real as ty), Nan ->
      Options.fatal "[typing] join failure: number %a and nan" D.pretty ty
    | Real, (C_integer _ | C_float _ | Gmpz | Rational)
    | (C_integer _ | C_float _ | Rational | Gmpz), Real ->
      Real
    | Rational, (C_integer _ | C_float _ | Gmpz)
    | (C_integer _ | C_float _ | Gmpz), Rational
    | C_float _, Gmpz
    | Gmpz, C_float _ ->
      Rational
    | Gmpz, C_integer _
    | C_integer _, Gmpz ->
      Gmpz
    | C_float f1, C_float f2 ->
      join_cty (TFloat(f1, [])) (TFloat(f2, []))
    | C_float f, C_integer n
    | C_integer n, C_float f ->
      join_cty (TFloat(f, [])) (TInt(n, []))
    | C_integer i1, C_integer i2 ->
      if Options.Gmp_only.get () then Gmpz
      else join_cty (TInt(i1, [])) (TInt(i2, []))

exception Not_a_number
let typ_of_number_ty = function
  | C_integer ik -> TInt(ik, [])
  | C_float fk -> TFloat(fk, [])
  | Gmpz -> Gmp_types.Z.t ()
  (* for the time being, no reals but rationals instead *)
  | Rational -> Gmp_types.Q.t ()
  | Real -> Error.not_yet "real number type"
  | Nan -> raise Not_a_number

let typ_of_lty = function
  | Ctype cty -> cty
  | Linteger -> Gmp_types.Z.t ()
  | Lreal -> Error.not_yet "real type"
  | Ltype _ | Lvar _ | Larrow _ -> Options.fatal "unexpected logic type"

(******************************************************************************)
(** Memoization *)
(******************************************************************************)

type computed_info =
    { ty: D.t;  (* type required for the term *)
      op: D.t; (* type required for the operation *)
      cast: D.t option; (* if not [None], type of the context which the term
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

(* Compute the smallest type (bigger than [int]) which can contain the whole
   interval. It is the \theta operator of the JFLA's paper. *)
let ty_of_interv ?ctx = function
  | Interval.Float(fk, _) -> C_float fk
  | Interval.Rational -> Rational
  | Interval.Real -> Real
  | Interval.Nan -> Nan
  | Interval.Ival iv ->
    try
      let kind = Interval.ikind_of_ival iv in
      (match ctx with
       | None
       | Some (Gmpz | Nan) ->
         C_integer kind
       | Some (C_integer ik as ctx) ->
         (* return [ctx] type for types smaller than int to prevent superfluous
            casts in the generated code *)
         if Cil.intTypeIncluded kind ik then ctx else C_integer kind
       | Some (C_float _ | Rational | Real as ty) ->
         ty)
    with Cil.Not_representable ->
      match ctx with
      | None | Some(C_integer _ | Gmpz | Nan) -> Gmpz
      | Some (C_float _ | Rational) -> Rational
      | Some Real -> Real

(* compute a new {!computed_info} by coercing the given type [ty] to the given
   context [ctx]. [op] is the type for the operator. *)
let coerce ~arith_operand ~ctx ~op ty =
  if D.compare ty ctx = 1 then
    (* type larger than the expected context,
       so we must introduce an explicit cast *)
    { ty; op; cast = Some ctx }
  else
    (* only add an explicit cast if the context is [Gmp] and [ty] is not;
       or if the term corresponding to [ty] is an operand of an arithmetic
       operation which must be explicitly coerced in order to force the
       operation to be of the expected type. *)
    if (ctx = Gmpz && ty <> Gmpz) || arith_operand
    then { ty; op; cast = Some ctx }
    else { ty; op; cast = None }

let number_ty_of_typ ty = match Cil.unrollType ty with
  | TInt(ik, _) | TEnum({ ekind = ik }, _) -> C_integer ik
  | TFloat(fk, _) -> C_float fk
  | TVoid _ | TPtr _ | TArray _ | TFun _ | TComp _ | TBuiltin_va_list _ -> Nan
  | TNamed _ -> assert false

let ty_of_logic_ty ?term lty =
  let get_ty = function
    | Linteger -> Gmpz
    | Ctype ty -> number_ty_of_typ ty
    | Lreal -> Real
    | Larrow _ -> Nan
    | Ltype _ -> Error.not_yet "user-defined logic type"
    | Lvar _ -> Error.not_yet "type variable"
  in
  match term with
  | None -> get_ty lty
  | Some t ->
    if Options.Gmp_only.get () && lty = Linteger then Gmpz
    else
      let i = Interval.infer t in
      ty_of_interv i

(******************************************************************************)
(** {2 Type system} *)
(******************************************************************************)

(* generate a context [c]. Take --e-acsl-gmp-only into account iff [use_gmp_opt]
   is true. *)
let mk_ctx ~use_gmp_opt = function
  | C_integer _ as c ->
    if use_gmp_opt && Options.Gmp_only.get () then Gmpz
    else c
  | C_float _ | Gmpz | Rational | Real | Nan as c -> c

(* the number_ty corresponding to [t] whenever use as an offset.
   In that case, it cannot be a GMP, so it must be coerced to an integral type
   in that case *)
let type_offset t =
  let i = Interval.infer t in
  match ty_of_interv i with
  | Gmpz -> C_integer ILongLong (* largest possible type *)
  | ty -> ty

let type_letin li li_t =
  let i = Interval.infer li_t in
  Interval.Env.add li.l_var_info i

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
    | TConst (Integer _ | LChr _ | LEnum _ | LReal _)
    | TSizeOf _
    | TSizeOfStr _
    | TAlignOf _ ->
      let i = Interval.infer t in
      let ty = ty_of_interv ?ctx i in
      dup ty

    | TLval tlv ->
      let i = Interval.infer t in
      let ty = ty_of_interv ?ctx i in
      type_term_lval tlv;
      dup ty

    | Toffset(_, t')
    | Tblock_length(_, t')
    | TSizeOfE t'
    | TAlignOfE t' ->
      let i = Interval.infer t in
      (* [t'] must be typed, but it is a pointer *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan t');
      let ty = ty_of_interv ?ctx i in
      dup ty

    | TBinOp (MinusPP, t1, t2) ->
      let i = Interval.infer t in
      (* [t1] and [t2] must be typed, but they are pointers *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan t1);
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan t2);
      let ty = ty_of_interv ?ctx i in
      dup ty

    | TUnOp (unop, t') ->
      let i = Interval.infer t in
      let i' = Interval.infer t' in
      let ctx_res, ctx = compute_ctx ?ctx (Interval.join i i') in
      ignore (type_term ~use_gmp_opt:true ~arith_operand:true ~ctx t');
      (match unop with
       | LNot -> c_int, ctx_res (* converted into [t == 0] in case of GMP *)
       | Neg | BNot -> dup ctx_res)

    | TBinOp ((PlusA | MinusA | Mult | Div | Mod | Shiftlt | Shiftrt), t1, t2)
      ->
      let i = Interval.infer t in
      let i1 = Interval.infer t1 in
      let i2 = Interval.infer t2 in
      let ctx_res, ctx =
        compute_ctx ?ctx (Interval.join i (Interval.join i1 i2))
      in
      (* it is enough to explicitly coerce when required one operand to [ctx]
         (through [arith_operand]) in order to force the type of the
         operation.  Heuristic: coerce the operand which is not a lval in
         order to lower the number of explicit casts *)
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
      assert (match ctx with None -> true | Some c -> D.compare c c_int >= 0);
      let i1 = Interval.infer t1 in
      let i2 = Interval.infer t2 in
      let ctx =
        mk_ctx ~use_gmp_opt:true (ty_of_interv ?ctx (Interval.join i1 i2))
      in
      ignore (type_term ~use_gmp_opt:true ~ctx t1);
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      let ty = match ctx with
        | Nan -> c_int
        | Real | Rational | Gmpz | C_float _ | C_integer _ -> ctx
      in
      c_int, ty

    | TBinOp ((LAnd | LOr), t1, t2) ->
      let i1 = Interval.infer t1 in
      let i2 = Interval.infer t2 in
      let ty = ty_of_interv ?ctx (Interval.join i1 i2) in
      (* both operands fit in an int. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int t1);
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int t2);
      dup ty

    | TBinOp (BAnd, _, _) -> Error.not_yet "bitwise and"
    | TBinOp (BXor, _, _) -> Error.not_yet "bitwise xor"
    | TBinOp (BOr, _, _) -> Error.not_yet "bitwise or"

    | TCastE(_, t') ->
      (* compute the smallest interval from the whole term [t] *)
      let i = Interval.infer t in
      (* nothing more to do: [i] is already more precise than what we could
         infer from the arguments of the cast. *)
      let ctx = ty_of_interv ?ctx i in
      ignore (type_term ~use_gmp_opt:true ~ctx t');
      dup ctx

    | Tif (t1, t2, t3) ->
      let ctx1 =
        mk_ctx ~use_gmp_opt:false c_int (* an int must be generated *)
      in
      ignore (type_term ~use_gmp_opt:false ~ctx:ctx1 t1);
      let i = Interval.infer t in
      let i2 = Interval.infer t2 in
      let i3 = Interval.infer t3 in
      let ctx = ty_of_interv ?ctx (Interval.join i (Interval.join i2 i3)) in
      let ctx = mk_ctx ~use_gmp_opt:true ctx in
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      ignore (type_term ~use_gmp_opt:true ~ctx t3);
      dup ctx

    | Tat (t, _)
    | TLogic_coerce (_, t) ->
      dup (type_term ~use_gmp_opt ~arith_operand ?ctx t).ty

    | TAddrOf tlv
    | TStartOf tlv ->
      (* it is a pointer, but subterms must be typed. *)
      type_term_lval tlv;
      dup Nan

    | Tbase_addr (_, t) ->
      (* it is a pointer, but subterms must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan t);
      dup Nan

    | TBinOp ((PlusPI | IndexPI | MinusPI), t1, t2) ->
      (* both [t1] and [t2] must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan t1);
      let ctx = type_offset t2 in
      ignore (type_term ~use_gmp_opt:false ~ctx t2);
      dup Nan

    | Tapp(li, _, args) ->
      if Builtins.mem li.l_var_info.lv_name then
        let typ_arg lvi arg =
          (* a built-in is a C function, so the context is necessarily a C
             type. *)
          let ctx = ty_of_logic_ty lvi.lv_type in
          ignore (type_term ~use_gmp_opt:false ~ctx arg)
        in
        List.iter2 typ_arg li.l_profile args;
        (* [li.l_type is [None] for predicate only: not possible here.
           Thus using [Extlib.the] is fine *)
        dup (ty_of_logic_ty (Extlib.the li.l_type))
      else begin
        (* TODO: what if the type of the parameter is smaller than the infered
           type of the argument? For now, it is silently ignored (both
           statically and at runtime)... *)
        List.iter (fun arg -> ignore (type_term ~use_gmp_opt:true arg)) args;
        (* TODO: recursive call in arguments of function call *)
        match li.l_body with
        | LBpred _ ->
          (* possible to have an [LBpred] here because we transformed
             [Papp] into [Tapp] *)
          dup c_int
        | LBterm _ ->
          begin match li.l_type with
            | None ->
              assert false
            | Some lty ->
              (* TODO: what if the function returns a real? *)
              let ty = ty_of_logic_ty ~term:t lty in
              dup ty
          end
        | LBnone ->
          Error.not_yet "logic functions with no definition nor reads clause"
        | LBreads _ ->
          Error.not_yet "logic functions performing read accesses"
        | LBinductive _ ->
          Error.not_yet "logic functions inductively defined"
      end

    | Tunion _ -> Error.not_yet "tset union"
    | Tinter _ -> Error.not_yet "tset intersection"
    | Tcomprehension (_,_,_) -> Error.not_yet "tset comprehension"

    | Trange(None, _) | Trange(_, None) ->
      Options.abort "unbounded ranges are not part of E-ACSl"
    | Trange(Some n1, Some n2) ->
      ignore (type_term ~use_gmp_opt n1);
      ignore (type_term ~use_gmp_opt n2);
      let i = Interval.infer t in
      let ty = ty_of_interv ?ctx i in
      dup ty

    | Tlet(li, t) ->
      let li_t = Misc.term_of_li li in
      type_letin li li_t;
      ignore (type_term ~use_gmp_opt:true li_t);
      dup (type_term ~use_gmp_opt:true ?ctx t).ty

    | Tlambda (_,_) -> Error.not_yet "lambda"
    | TDataCons (_,_) -> Error.not_yet "datacons"
    | TUpdate (_,_,_) -> Error.not_yet "update"

    | Tnull
    | TConst (LStr _ | LWStr _)
    | Ttypeof _
    | Ttype _
    | Tempty_set  -> dup Nan
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
  | TMem t -> ignore (type_term ~use_gmp_opt:false ~ctx:Nan t)

and type_term_offset = function
  | TNoOffset -> ()
  | TField(_, toff)
  | TModel(_, toff) -> type_term_offset toff
  | TIndex(t, toff) ->
    let ctx = type_offset t in
    ignore (type_term ~use_gmp_opt:false ~ctx t);
    type_term_offset toff

let rec type_predicate p =
  Cil.CurrentLoc.set p.pred_loc;
  (* this pattern matching also follows the formal rules of the JFLA's paper *)
  let op =
    match p.pred_content with
    | Pfalse | Ptrue -> c_int
    | Papp(li, _, _) ->
      begin match li.l_body with
        | LBpred _ ->
          (* No need to type subpredicates
             since Papp will be transformed into Tapp in Translate:
             a retyping is done there *)
          c_int
        | LBnone -> (* Eg: \is_finite *)
          Error.not_yet "predicate with no definition nor reads clause"
        | LBreads _ | LBterm _ | LBinductive _ ->
          Options.fatal "unexpected logic definition"
      end
    | Pseparated _ -> Error.not_yet "\\separated"
    | Pdangling _ -> Error.not_yet "\\dangling"
    | Prel(_, t1, t2) ->
      let i1 = Interval.infer t1 in
      let i2 = Interval.infer t2 in
      let i = Interval.join i1 i2 in
      let ctx = mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx:c_int i) in
      ignore (type_term ~use_gmp_opt:true ~ctx t1);
      ignore (type_term ~use_gmp_opt:true ~ctx t2);
      (match ctx with
       | Nan -> c_int
       | Real | Rational | Gmpz | C_float _ | C_integer _ -> ctx)
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
    | Plet(li, p) ->
      let li_t = Misc.term_of_li li in
      type_letin li li_t;
      ignore (type_term ~use_gmp_opt:true li_t);
      (type_predicate p).ty

    | Pforall(bounded_vars, { pred_content = Pimplies(hyps, goal) })
    | Pexists(bounded_vars, { pred_content = Pand(hyps, goal) }) ->
      let guards = !compute_quantif_guards_ref p bounded_vars hyps in
      let iv_plus_one iv =
        Interval.Ival (Ival.add_singleton_int Integer.one iv)
      in
      List.iter
        (fun (t1, r1, x, r2, t2) ->
           let i1 = Interval.infer t1 in
           let i1 = match r1, i1 with
             | Rlt, Interval.Ival iv -> iv_plus_one iv
             | Rle, _ -> i1
             | _ -> assert false
           in
           let i2 = Interval.infer t2 in
           (* add one to [i2], since we increment the loop counter one more
              time before going outside the loop. *)
           let i2 = match r2, i2 with
             | Rlt, _ -> i2
             | Rle, Interval.Ival iv -> iv_plus_one iv
             | _ -> assert false
           in
           let i = Interval.join i1 i2 in
           let ctx = match x.lv_type with
             | Linteger -> mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx:Gmpz i)
             | Ctype ty ->
               (match Cil.unrollType ty with
                | TInt(ik, _) -> mk_ctx ~use_gmp_opt:true (C_integer ik)
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
             | C_integer _ -> i
             | Gmpz -> Interval.top_ival (* [ -\infty; +\infty ] *)
             | C_float _ | Rational | Real | Nan ->
               Options.fatal "unexpected quantification over %a" D.pretty ctx
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
      ignore (type_term ~use_gmp_opt:false ~ctx:Nan t);
      c_int

    | Pforall _ -> Error.not_yet "unguarded \\forall quantification"
    | Pexists _ -> Error.not_yet "unguarded \\exists quantification"
    | Pat(p, _) -> (type_predicate p).ty
    | Pfresh _ -> Error.not_yet "\\fresh"
  in
  coerce ~arith_operand:false ~ctx:c_int ~op c_int

let type_term ~use_gmp_opt ?ctx t =
  Options.feedback ~dkey ~level:4 "typing term '%a' in ctx '%a'."
    Printer.pp_term t (Pretty_utils.pp_opt D.pretty) ctx;
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

let get_number_ty t = (Memo.get t).ty
let get_integer_op t = (Memo.get t).op
let get_integer_op_of_predicate p = (type_predicate p).op

(* {!typ_of_integer}, but handle the not-integer cases. *)
let extract_typ t ty =
  try typ_of_number_ty ty
  with Not_a_number ->
    match t.term_type with
    | Ctype _ as lty -> Logic_utils.logicCType lty
    | Linteger | Lreal ->
      Options.fatal "unexpected context NaN for term %a" Printer.pp_term t
    | Ltype _ -> Error.not_yet "unsupported logic type: user-defined type"
    | Lvar _ -> Error.not_yet "unsupported logic type: type variable"
    | Larrow _ -> Error.not_yet "unsupported logic type: type arrow"

let get_typ t =
  let info = Memo.get t in
  extract_typ t info.ty

let get_op t =
  let info = Memo.get t in
  extract_typ t info.op

let get_cast t =
  let info = Memo.get t in
  try Extlib.opt_map typ_of_number_ty info.cast
  with Not_a_number -> None

let get_cast_of_predicate p =
  let info = type_predicate p in
  try Extlib.opt_map typ_of_number_ty info.cast
  with Not_a_number -> assert false

let clear = Memo.clear

module Datatype = D

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

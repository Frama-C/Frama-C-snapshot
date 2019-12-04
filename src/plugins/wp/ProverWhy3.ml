(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* Allow type-desambiguation for symbols *)
[@@@ warning "-40-42"]

let dkey = Wp_parameters.register_category "prover"
let dkey_api = Wp_parameters.register_category "why3_api"

let option_file = LogicBuiltins.create_option
    (fun ~driver_dir x -> Filename.concat driver_dir x)
    "why3" "file"

let option_import = LogicBuiltins.create_option
    (fun ~driver_dir:_ x -> x)
    "why3" "import"

let why3_failure msg =
  Pretty_utils.ksfprintf failwith msg

module Env = WpContext.Index(struct
    include Datatype.Unit
    type key = unit
    type data = Why3.Env.env
  end)

let get_why3_env = Env.memoize
    begin fun () ->
      let config = Why3Provers.config () in
      let main = Why3.Whyconf.get_main config in
      let ld =
        (WpContext.directory ())::
        (Wp_parameters.Share.file "why3")::
        (Why3.Whyconf.loadpath main) in
      Why3.Env.create_env ld
    end

type context = {
  mutable th : Why3.Theory.theory_uc;
  env: Why3.Env.env;
}

type convert = {
  th : Why3.Theory.theory_uc;
  env: Why3.Env.env;
  subst: Why3.Term.term Lang.F.Tmap.t;
  pool: Lang.F.pool;
  polarity: Cvalues.polarity;
  in_goal: bool;
  mutable convert_for_export: Lang.F.term Lang.F.Tmap.t;
}

(** The reason for the rebuild *)
let specific_equalities: Lang.For_export.specific_equality list ref =
  ref [Vlist.specialize_eq_list]

let add_specific_equality ~for_tau ~mk_new_eq =
  specific_equalities := { for_tau; mk_new_eq }::!specific_equalities

(** get symbols *)

let get_ls ~cnv ~f ~l ~p =
  let th = Why3.Env.read_theory cnv.env f l in
  let ls =
    try
      Why3.Theory.ns_find_ls th.th_export p
    with Not_found ->
      why3_failure "The symbol %a can't be found in %a.%s"
        Why3.Pp.(print_list dot string) p
        Why3.Pp.(print_list dot string) f l
  in
  ls

let get_ts ~cnv ~f ~l ~p =
  let th = Why3.Env.read_theory cnv.env f l in
  let ls =
    try
      Why3.Theory.ns_find_ts th.th_export p
    with Not_found ->
      why3_failure "The type %a can't be found in %a.%s"
        Why3.Pp.(print_list dot string) p
        Why3.Pp.(print_list dot string) f l
  in
  ls


let t_app ~cnv ~f ~l ~p tl =
  Why3.Term.t_app_infer (get_ls ~cnv ~f ~l ~p) tl

let t_app' ~cnv ~f ~l ~p tl ty =
  Why3.Term.t_app (get_ls ~cnv ~f ~l ~p) tl ty

(** Conversion *)

(** why3 1.3
    let const_int (z:Z.t) =
    Why3.(Term.t_const Number.(int_const (BigInt.of_string (Z.to_string z)))) Why3.Ty.ty_int

    let const_real ~cnv (q:Q.t) =
    let mk_real_int z =
    let c = Why3.Number.real_const (Why3.BigInt.of_string (Z.to_string z)) in
    Why3.(Term.t_const c) Why3.Ty.ty_real
    in
    if Z.equal Z.one q.den
    then mk_real_int q.num
    else
    t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix /"] [mk_real_int q.num;mk_real_int q.den]

*)

let const_int (z:Z.t) =
  Why3.(Term.t_const Number.(const_of_big_int (BigInt.of_string (Z.to_string z)))) Why3.Ty.ty_int

let const_real ~cnv (q:Q.t) =
  let mk_real_int z =
    let rc_negative = Z.sign z < 0 in
    let z = Z.abs z in
    let rc_abs = Why3.Number.real_const_dec (Z.to_string z) "" None in
    let c = Why3.Number.ConstReal { Why3.Number.rc_negative; rc_abs } in
    Why3.(Term.t_const c) Why3.Ty.ty_real
  in
  if Z.equal Z.one q.den
  then mk_real_int q.num
  else
    t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix /"] [mk_real_int q.num;mk_real_int q.den]

(** fold map list of at least one element *)
let fold_map map fold = function
  | [] -> assert false (** absurd: forbidden by qed  *)
  | a::tl ->
      List.fold_left (fun acc a -> fold acc (map a)) (map a) tl

let empty_context name : context = {
  th = Why3.Theory.create_theory (Why3.Ident.id_fresh name);
  env = get_why3_env ();
}

let empty_cnv ?(polarity=`NoPolarity) ?(in_goal=false) (ctx:context) : convert = {
  th = ctx.th;
  subst = Lang.F.Tmap.empty;
  pool = Lang.F.pool ();
  env = ctx.env;
  polarity;
  in_goal;
  convert_for_export = Lang.F.Tmap.empty;
}


let lfun_name (lfun:Lang.lfun) =
  match lfun with
  | ACSL f -> Qed.Engine.F_call (Lang.logic_id f)
  | CTOR c -> Qed.Engine.F_call (Lang.ctor_id c)
  | Model({m_source=Generated(_,n)}) -> Qed.Engine.F_call n
  | Model({m_source=Extern e}) -> e.Lang.ext_link.Lang.why3


let coerce ~cnv sort expected r =
  match sort, expected with
  | Qed.Logic.Bool, Qed.Logic.Prop -> Why3.Term.(t_equ r t_bool_true)
  | Qed.Logic.Int, Qed.Logic.Real ->
      t_app ~cnv ~f:["real"] ~l:"FromInt" ~p:["from_int"] [r]
  | _ -> r

let name_of_adt = function
  | Lang.Mtype a -> a.Lang.ext_link.Lang.why3
  | Mrecord(a,_) -> a.Lang.ext_link.Lang.why3
  | Comp c -> Lang.comp_id c
  | Atype lt -> Lang.type_id lt

let tvar =
  let tvar = Datatype.Int.Hashtbl.create 10 in
  fun i ->
    Datatype.Int.Hashtbl.memo tvar i
      (fun i ->
         let id = Why3.Ident.id_fresh (Printf.sprintf "a%i" i) in
         Why3.Ty.create_tvsymbol id
      )


(** Sharing *)

let shared (_ : Lang.F.term) = false

let shareable e =
  match Lang.F.repr e with
  | Kint _ | Kreal _ | True | False -> false
  | Times _ | Add _ | Mul _ | Div _ | Mod _ -> true
  | Eq _ | Neq _ | Leq _ | Lt _ -> false
  | Aget _ | Aset _ | Rget _ | Rdef _ | Acst _ -> true
  | And _ | Or _ | Not _ | Imply _ | If _ -> false
  | Fun _ -> not (Lang.F.is_prop e)
  | Bvar _ | Fvar _ | Apply _ | Bind _ -> false

let subterms f e =
  match Lang.F.repr e with
  | Rdef fts ->
      begin
        match Lang.F.record_with fts with
        | None -> Lang.F.lc_iter f e
        | Some(a,fts) -> f a ; List.iter (fun (_,e) -> f e) fts
      end
  | _ -> Lang.F.lc_iter f e

(* path splitting *)
let regexp_col = Str.regexp_string ":"
let regexp_com = Str.regexp_string ","
let regexp_dot = Str.regexp_string "."

let cut_path s = Str.split_delim regexp_dot s

(* conversion *)

let rec of_tau ~cnv (t:Lang.F.tau) =
  match t with
  | Prop -> None
  | Bool -> Some Why3.Ty.ty_bool
  | Int -> Some Why3.Ty.ty_int
  | Real -> Some Why3.Ty.ty_real
  | Array(k,v) ->
      let ts = get_ts ~cnv ~f:["map"] ~l:"Map" ~p:["map"] in
      Some (Why3.Ty.ty_app ts [Why3.Opt.get (of_tau ~cnv k); Why3.Opt.get (of_tau ~cnv v)])
  | Data(adt,l) -> begin
      let s = name_of_adt adt in
      match Why3.Theory.(ns_find_ts (get_namespace cnv.th) (cut_path s)) with
      | ts -> Some (Why3.Ty.ty_app ts (List.map (fun e -> Why3.Opt.get (of_tau ~cnv e)) l))
      | exception Not_found ->
          why3_failure "Can't find type '%s' in why3 namespace" s
    end
  | Tvar i -> Some (Why3.Ty.ty_var (tvar i))
  | Record _ ->
      why3_failure "Type %a not (yet) convertible" Lang.F.pp_tau t

let rec full_trigger = function
  | Qed.Engine.TgAny -> false
  | TgVar _ -> true
  | TgGet(a,k) -> full_trigger a && full_trigger k
  | TgSet(a,k,v) -> full_trigger a && full_trigger k && full_trigger v
  | TgFun(_,xs) | TgProp(_,xs) -> List.for_all full_trigger xs

let rec full_triggers = function
  | [] -> []
  | ts :: tgs ->
      match List.filter full_trigger ts with
      | [] -> full_triggers tgs
      | ts -> ts :: full_triggers tgs

let rec of_trigger ~cnv t =
  match t with
  | Qed.Engine.TgAny -> assert false (** absurd: filter by full_triggers *)
  | Qed.Engine.TgVar v -> begin
      try Lang.F.Tmap.find (Lang.F.e_var v) cnv.subst
      with Not_found -> why3_failure "Unbound variable %a" Lang.F.pp_var v
    end
  | Qed.Engine.TgGet(m,k) ->
      t_app ~cnv ~f:["map"] ~l:"Map" ~p:["get"] [of_trigger cnv m;of_trigger cnv k]
  | TgSet(m,k,v) ->
      t_app ~cnv ~f:["map"] ~l:"Map" ~p:["set"] [of_trigger cnv m;of_trigger cnv k;of_trigger cnv v]
  | TgFun (f,l) -> begin
      match lfun_name f with
      | F_call s ->
          let ls = Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) in
          Why3.Term.t_app_infer ls (List.map (fun e -> of_trigger cnv e) l)
      | _ -> why3_failure "can not convert extented calls in triggers"
    end
  | TgProp (f,l) ->
      begin
        match lfun_name f with
        | F_call s ->
            let ls = Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) in
            Why3.Term.t_app_infer ls (List.map (fun e -> of_trigger cnv e) l)
        | _ -> why3_failure "can not convert extented calls in triggers"
      end

let rec of_term ~cnv expected t : Why3.Term.term =
  Wp_parameters.debug ~dkey:dkey_api
    "of_term %a %a@."
    Lang.F.Tau.pretty expected Lang.F.pp_term t;
  let sort = Lang.F.typeof t in
  let ($) f x = f x in
  let r =
    try coerce ~cnv sort expected $ Lang.F.Tmap.find t cnv.subst
    with Not_found ->
    match Lang.F.repr t, sort, expected with
    | (Fvar _, _, _) -> invalid_arg "unbound variable in of_term"
    | (Bvar _, _, _) -> invalid_arg "bound variable in of_term"
    | Bind((Forall|Exists) as q,_,_), _, _ ->
        coerce ~cnv Prop expected $
        let why3_vars, t = successive_binders cnv q t in
        let quant = match q with
          | Qed.Logic.Forall -> Why3.Term.Tforall
          | Qed.Logic.Exists -> Why3.Term.Texists
          | _ -> assert false
        in
        Why3.Term.t_quant quant (Why3.Term.t_close_quant why3_vars [] t)
    | True, _, Prop -> Why3.Term.t_true
    | True, _, Bool -> Why3.Term.t_bool_true
    | False, _, Prop -> Why3.Term.t_false
    | False, _, Bool -> Why3.Term.t_bool_false
    | Kint z, Int, _ -> coerce ~cnv sort expected $ const_int z
    | Kreal q, Real, _ -> coerce ~cnv sort expected $ const_real ~cnv q
    | Times(z,t), Int, _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["int"] ~l:"Int" ~p:["infix *"] [const_int z; of_term cnv sort t]
    | Times(z,t), Real, _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix *"]
          [const_real ~cnv (Q.of_bigint z); of_term cnv sort t]
    | Add l, Int, _ ->
        coerce ~cnv sort expected $
        t_app_fold ~f:["int"] ~l:"Int" ~p:["infix +"] ~cnv sort l
    | Add l, Real, _ ->
        coerce ~cnv sort expected $
        t_app_fold ~f:["real"] ~l:"Real" ~p:["infix +"] ~cnv sort l
    | Mul l, Int, _ ->
        coerce ~cnv sort expected $
        t_app_fold ~f:["int"] ~l:"Int" ~p:["infix *"] ~cnv sort l
    | Mul l, Real, _ ->
        coerce ~cnv sort expected $
        t_app_fold ~f:["real"] ~l:"Real" ~p:["infix *"] ~cnv sort l
    | Leq (a,b), _, Prop ->
        int_or_real ~cnv
          ~fint:["int"] ~lint:"Int" ~pint:["infix <="]
          ~freal:["real"] ~lreal:"Real" ~preal:["infix <="]
          a b
    | Div(a,b), Int, _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["int"] ~l:"ComputerDivision" ~p:["div"]
          [of_term ~cnv sort a; of_term ~cnv sort b]
    | Mod(a,b), Int, _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["int"] ~l:"ComputerDivision" ~p:["mod"]
          [of_term ~cnv sort a; of_term ~cnv sort b]
    | Div(a,b), Real, _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix /"]
          [of_term ~cnv sort a; of_term ~cnv sort b]
    | Lt (a,b), _, Prop ->
        int_or_real ~cnv
          ~fint:["int"] ~lint:"Int" ~pint:["infix <"]
          ~freal:["real"] ~lreal:"Real" ~preal:["infix <"]
          a b
    | Leq (a,b), _, Bool ->
        int_or_real ~cnv
          ~fint:["qed"] ~lint:"Qed" ~pint:["zleq"]
          ~freal:["qed"] ~lreal:"Qed" ~preal:["rleq"]
          a b
    | Lt (a,b), _, Bool ->
        int_or_real ~cnv
          ~fint:["qed"] ~lint:"Qed" ~pint:["zlt"]
          ~freal:["qed"] ~lreal:"Qed" ~preal:["rlt"]
          a b
    | And l, _, Bool ->
        t_app_fold ~f:["bool"] ~l:"Bool" ~p:["andb"] ~cnv expected l
    | And l, _, Prop ->
        fold_map (of_term ~cnv expected) Why3.Term.t_and l
    | Or l, _, Bool ->
        t_app_fold ~f:["bool"] ~l:"Bool" ~p:["orb"] ~cnv expected l
    | Or l, _, Prop ->
        fold_map (of_term ~cnv expected) Why3.Term.t_or l
    | Not e, _, Bool ->
        let cnv = {cnv with polarity = Cvalues.negate cnv.polarity} in
        t_app ~cnv ~f:["bool"] ~l:"Bool" ~p:["notb"] [of_term ~cnv expected e]
    | Not e, _, Prop ->
        let cnv = {cnv with polarity = Cvalues.negate cnv.polarity} in
        Why3.Term.t_not (of_term cnv expected e)
    | Imply (l,e), _, _ ->
        let e = (of_term ~cnv expected) e in
        let cnv' = {cnv with polarity = Cvalues.negate cnv.polarity} in
        let fold acc a =
          let a = of_term ~cnv:cnv' expected a in
          match expected with
          | Prop -> Why3.Term.t_implies a acc
          | _ (* Bool *) ->
              t_app ~cnv:cnv' ~f:["bool"] ~l:"Bool" ~p:["implb"] [a;acc]
        in
        List.fold_left fold e (List.rev l)
    | Eq (a,b), _, Prop -> begin
        match Lang.F.typeof a with
        | Prop | Bool ->
            Why3.Term.t_iff (of_term cnv Prop a) (of_term cnv Prop b)
        | tau ->
            match List.find (fun spe -> spe.Lang.For_export.for_tau tau) !specific_equalities with
            | spe when cnv.polarity = `Positive -> of_term cnv expected (spe.mk_new_eq a b)
            | exception Not_found -> Why3.Term.t_equ (of_term' cnv a) (of_term' cnv b)
            | _                   -> Why3.Term.t_equ (of_term' cnv a) (of_term' cnv b)
      end
    | Neq (a,b), _, Prop ->
        begin
          match Lang.F.typeof a with
          | Prop | Bool ->
              Why3.Term.t_not (Why3.Term.t_iff (of_term cnv Prop a) (of_term cnv Prop b))
          | tau ->
              match List.find (fun spe -> spe.Lang.For_export.for_tau tau) !specific_equalities with
              | spe when cnv.polarity = `Negative ->
                  Why3.Term.t_not (of_term cnv expected (spe.mk_new_eq a b))
              | exception Not_found -> Why3.Term.t_neq (of_term' cnv a) (of_term' cnv b)
              | _                   -> Why3.Term.t_neq (of_term' cnv a) (of_term' cnv b)
        end
    | Eq (a,b), _, Bool ->
        t_app ~cnv ~f:["qed"] ~l:"Qed" ~p:["eqb"] [of_term' cnv a; of_term' cnv b]
    | Neq (a,b), _, Bool ->
        t_app ~cnv ~f:["qed"] ~l:"Qed" ~p:["neqb"] [of_term' cnv a; of_term' cnv b]
    | If(a,b,c), _, _ ->
        let cnv' = {cnv with polarity = `NoPolarity} in
        Why3.Term.t_if (of_term cnv' Prop a) (of_term cnv expected b) (of_term cnv expected c)
    | Aget(m,k), _, _ ->
        coerce ~cnv sort expected $
        let mtau = Lang.F.typeof m in
        let ksort = match mtau with
          | Array(ksort,_) -> ksort
          | _ -> assert false (** absurd: by qed typing *)in
        t_app ~cnv ~f:["map"] ~l:"Map" ~p:["get"] [of_term cnv mtau m;of_term cnv ksort k]
    | Aset(m,k,v), Array(ksort,vsort), _ ->
        coerce ~cnv sort expected $
        t_app ~cnv ~f:["map"] ~l:"Map" ~p:["set"] [of_term cnv sort m;of_term cnv ksort k;of_term cnv vsort v]
    | Acst(_,v), Array(_,vsort), _ ->
        coerce ~cnv sort expected $
        t_app' ~cnv ~f:["map"] ~l:"Const" ~p:["const"] [of_term cnv vsort v] (of_tau cnv sort)
    (* Generic *)
    | Fun (f,l), _, _ -> begin
        let t_app ls l r  =
          Why3.Term.t_app ls l r
        in
        let apply_from_ns s l sort =
          match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)), expected with
          | ls, (Prop | Bool) ->
              coerce ~cnv sort expected $
              t_app ls l (of_tau cnv sort)
          | ls, _ ->
              coerce ~cnv sort expected $
              t_app ls l (of_tau cnv sort)
          | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
        in
        let apply_from_ns' s l =
          apply_from_ns s (List.map (fun e -> of_term' cnv e) l)
        in
        match lfun_name f, expected with
        | F_call s, _ -> apply_from_ns' s l sort
        | Qed.Engine.F_subst _, _ ->
            why3_failure "Driver link with subst not yet implemented"
        | Qed.Engine.F_left s, _ | Qed.Engine.F_assoc s, _ ->
            let rec aux = function
              | [] -> why3_failure "Empty application"
              | [a] -> of_term cnv expected a
              | a::l ->
                  apply_from_ns s [of_term' cnv a; aux l] sort
            in
            aux l
        | Qed.Engine.F_right s, _ ->
            let rec aux = function
              | [] -> why3_failure "Empty application"
              | [a] -> of_term cnv expected a
              | a::l ->
                  apply_from_ns s [aux l;of_term' cnv a] sort
            in
            aux (List.rev l)
        | Qed.Engine.F_list (fcons,fnil), _ ->
            let rec aux = function
              | [] -> apply_from_ns fnil [] sort
              | a::l ->
                  apply_from_ns fcons [of_term' cnv a;aux l] sort
            in
            aux l
        | Qed.Engine.F_bool_prop (s,_), Bool | Qed.Engine.F_bool_prop (_,s), Prop ->
            apply_from_ns' s l expected
        | Qed.Engine.F_bool_prop (_,_), _ ->
            why3_failure "badly expected type %a for term %a"
              Lang.F.pp_tau expected Lang.F.pp_term t
      end
    | Rget(a,f), _ , _ -> begin
        let s = Lang.name_of_field f in
        match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) with
        | ls -> Why3.Term.t_app ls [of_term' cnv a] (of_tau cnv expected)
        | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
      end
    | Rdef(l), Data(Comp c,_) , _ -> begin
        (* l is already sorted by field *)
        let s = Lang.comp_id c in
        match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) with
        | ls ->
            let l = List.map (fun (_,t) -> of_term' cnv t) l in
            Why3.Term.t_app ls l (of_tau cnv expected)
        | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
      end
    | (Rdef _, Data ((Mtype _|Mrecord (_, _)|Atype _), _), _)
    | (Rdef _, (Prop|Bool|Int|Real|Tvar _|Array (_, _)), _)
    | (Aset (_, _, _), (Prop|Bool|Int|Real|Tvar _|Record _|Data (_, _)), _)
    | (Neq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Eq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Not _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Or _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (And _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Lt (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Leq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Div (_, _), (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Mod (_, _), (Prop|Bool|Real|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Mul _, (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Add _, (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Times (_, _), (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Kreal _, (Prop|Bool|Int|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Kint _, (Prop|Bool|Real|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (False, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (True, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Acst (_, _), (Prop|Bool|Int|Real|Tvar _|Record _|Data (_, _)), _)
      -> assert false (** absurd: by typing *)
    | (Bind (Lambda, _, _), _, _)
    | Apply _ , _, _
    | Rdef _, Record _, _ ->
        why3_failure
          "Can't convert to why3 the qed term %a of type %a"
          Lang.F.pp_term t Lang.F.pp_tau sort
  in
  r

and t_app_fold  ~cnv ~f ~l ~p expected lt =
  let fold acc a =
    t_app ~cnv ~f ~l ~p [acc;a]
  in
  fold_map (of_term ~cnv expected) fold lt

and of_term' cnv t =
  of_term cnv (Lang.F.typeof t) t

and share cnv expected t =
  let l = Lang.F.QED.shared ~shareable ~shared ~subterms [t] in
  let cnv,lets = mk_lets cnv l in
  let t = of_term ~cnv expected t in
  let t = List.fold_left (fun t (x,e') ->
      Why3.Term.t_let_close x e' t
    ) t lets
  in
  t

and mk_lets cnv l =
  List.fold_left (fun (cnv,lets) e ->
      let cnv' = {cnv with polarity = `NoPolarity} in
      let e' = of_term cnv' (Lang.F.typeof e) e in
      match e'.t_ty with
      | None -> ({cnv with subst = Lang.F.Tmap.add e e' cnv.subst},lets)
      | Some ty ->
          let x = Why3.Ident.id_fresh (Lang.F.basename e) in
          let x = Why3.Term.create_vsymbol x ty in
          (* Format.printf "lets %a = %a : %a@."
           *   Why3.Pretty.print_vsty x
           *   Why3.Pretty.print_term e'
           *   Why3.Pretty.print_ty (Why3.Term.t_type e'); *)
          let cnv = {cnv with subst = Lang.F.Tmap.add e (Why3.Term.t_var x) cnv.subst } in
          let lets = (x,e')::lets in
          cnv,lets
    ) (cnv,[]) l

and successive_binders cnv q t =
  match Lang.F.repr t with
  | Bind((Forall|Exists) as q',tau,t) when q' = q ->
      let x = Lang.F.fresh cnv.pool tau in
      let x' = Why3.Ident.id_fresh (Lang.F.Tau.basename tau) in
      let x' = Why3.Term.create_vsymbol x' (Why3.Opt.get (of_tau cnv tau)) in
      let cnv = {cnv with subst = Lang.F.Tmap.add (Lang.F.e_var x) (Why3.Term.t_var x') cnv.subst} in
      let t = Lang.F.QED.e_unbind x t in
      let why3_vars, t = successive_binders cnv q t in
      x'::why3_vars, t
  | _ ->
      [], share cnv Prop t

and int_or_real ~cnv ~fint ~lint ~pint ~freal ~lreal ~preal a b =
  match (Lang.F.typeof a), (Lang.F.typeof b) with
  | Int, Int ->
      t_app_fold ~f:fint ~l:lint ~p:pint ~cnv Int [a; b]
  | Real, Int | Real, Real | Int, Real ->
      t_app_fold ~f:freal ~l:lreal ~p:preal ~cnv Real [a; b]
  | _ -> assert false

let convert cnv expected t =
  (** rewrite terms which normal form inside qed are different from the one of the provers *)
  let t, convert_for_export = Lang.For_export.rebuild ~cache:cnv.convert_for_export t in
  cnv.convert_for_export <- convert_for_export;
  Lang.For_export.in_state (share cnv expected) t

let mk_binders cnv l =
  List.fold_left (fun (cnv,lets) v ->
      match of_tau cnv (Lang.F.tau_of_var v) with
      | None -> why3_failure "Quantification on prop"
      | Some ty ->
          let x = Why3.Ident.id_fresh (Lang.F.Var.basename v) in
          let x = Why3.Term.create_vsymbol x ty in
          let e = Lang.F.e_var v in
          let cnv = {cnv with subst = Lang.F.Tmap.add e (Why3.Term.t_var x) cnv.subst } in
          let lets = x::lets in
          cnv,lets
    ) (cnv,[]) (List.rev l)

(** visit definitions and add them in the task *)

module CLUSTERS = WpContext.Index
    (struct
      type key = Definitions.cluster
      type data = int * Why3.Theory.theory
      let name = "ProverWhy3.CLUSTERS"
      let compare = Definitions.cluster_compare
      let pretty = Definitions.pp_cluster
    end)



let filenoext file =
  let basename = Filename.basename file in
  (try Filename.chop_extension basename
   with Invalid_argument _ -> basename)

class visitor (ctx:context) c =
  object(self)

    inherit Definitions.visitor c


    (* --- Files, Theories and Clusters --- *)

    method add_builtin_lib =
      self#add_import_file ["bool"] "Bool" ;
      self#add_import_file ["int"] "Int" ;
      self#add_import_file ["int"] "ComputerDivision" ;
      self#add_import_file ["real"] "RealInfix" ;
      self#on_library "qed";
      self#add_import_file ["map"] "Map"

    method on_cluster c =
      let name = Definitions.cluster_id c in
      Wp_parameters.debug ~dkey:dkey_api "Start on_cluster %s@." name;
      let th_name = String.capitalize_ascii name in
      let thy =
        let age = try fst (CLUSTERS.find c) with Not_found -> (-1) in
        if age < Definitions.cluster_age c then
          let ctx = empty_context th_name in
          let v = new visitor ctx c in
          v#add_builtin_lib;
          v#vself;
          let th = Why3.Theory.close_theory ctx.th in
          if Wp_parameters.has_dkey ProverErgo.dkey_cluster then
            Log.print_on_output
              begin fun fmt ->
                Format.fprintf fmt "---------------------------------------------@\n" ;
                Format.fprintf fmt "--- Context '%s' Cluster '%s' @\n"
                  (WpContext.get_context () |> WpContext.S.id) name;
                Format.fprintf fmt "---------------------------------------------@\n" ;
                Why3.Pretty.print_theory fmt th;
              end ;
          CLUSTERS.update c (Definitions.cluster_age c, th);
          th
        else
          snd (CLUSTERS.find c)
      in
      let th = ctx.th in
      let th = Why3.Theory.open_scope th name in
      let th = Why3.Theory.use_export th thy in
      let th = Why3.Theory.close_scope th true in
      Wp_parameters.debug ~dkey:dkey_api "End  on_cluster %s@." name;
      ctx.th <- th


    method section _ = ()

    method add_import ?was thy =
      match Str.split_delim regexp_dot thy with
      | [] -> why3_failure "[driver] empty import option"
      | l ->
          let file, thy = Why3.Lists.chop_last l in
          self#add_import_use file thy (Why3.Opt.get_def thy was) ~import:true

    method add_import_file file thy =
      self#add_import_use ~import:true file thy thy

    method add_import_file_as file thy name =
      self#add_import_use ~import:false file thy name

    method add_import_use ~import file thy name =
      Wp_parameters.debug ~dkey:dkey_api
        "@[use@ %s@ @[%a.%s@]@ as@ %s@]"
        (if import then "import" else "")
        Why3.Pp.(print_list (Why3.Pp.constant_string ".") string) file
        thy name ;
      let thy = Why3.Env.read_theory ctx.env file thy in
      let th = ctx.th in
      let th = Why3.Theory.open_scope th name in
      let th = Why3.Theory.use_export th thy in
      let th = Why3.Theory.close_scope th import in
      ctx.th <- th

    method on_library thy =
      let copy_file source =
        if Filepath.normalize (Filename.dirname source) <>
           Filepath.normalize (Wp_parameters.Share.dir ())
        then
          let tgtdir = WpContext.directory () in
          let why3src = Filename.basename source in
          let target = Printf.sprintf "%s/%s" tgtdir why3src in
          Command.copy source target
      in
      let iter_file opt =
        match Str.split_delim regexp_col opt with
        | [file] ->
            let filenoext = filenoext file in
            copy_file file;
            self#add_import_file [filenoext]
              (String.capitalize_ascii filenoext);
        | [file;lib] ->
            copy_file file;
            self#add_import_file [filenoext file] lib;
        | [file;lib;name] ->
            copy_file file;
            self#add_import_file_as [filenoext file] lib name;
        | _ -> why3_failure
                 "[driver] incorrect why3.file %S for library '%s'"
                 opt thy
      in
      let iter_import opt =
        List.iter (fun import ->
            match Str.split_delim regexp_col import with
            | [ th ] -> self#add_import th
            | [ th ; was ] -> self#add_import ~was th
            | _ -> why3_failure
                     "[driver] incorrect why3.file %S for library '%s'"
                     opt thy
          ) (Str.split regexp_com opt)
      in
      begin
        List.iter iter_file
          (LogicBuiltins.get_option option_file ~library:thy) ;
        List.iter iter_import
          (LogicBuiltins.get_option option_import ~library:thy) ;
      end

    method on_type lt def =
      match def with
      | Tabs ->
          let id = Why3.Ident.id_fresh (Lang.type_id lt) in
          let map i _ = tvar i in
          let tv_args = List.mapi map lt.lt_params in
          let id = Why3.Ty.create_tysymbol id tv_args NoDef in
          let decl = Why3.Decl.create_ty_decl id in
          ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Tdef t ->
          let id = Why3.Ident.id_fresh (Lang.type_id lt) in
          let map i _ = tvar i in
          let tv_args = List.mapi map lt.lt_params in
          let cnv = empty_cnv ctx in
          let t = Why3.Opt.get (of_tau ~cnv t) in
          let id = Why3.Ty.create_tysymbol id tv_args (Alias t) in
          let decl = Why3.Decl.create_ty_decl id in
          ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Tsum cases ->
          let id = Why3.Ident.id_fresh (Lang.type_id lt) in
          let map i _ = tvar i in
          let tv_args = List.mapi map lt.lt_params in
          let tys = Why3.Ty.create_tysymbol id tv_args NoDef in
          let tv_args = List.map Why3.Ty.ty_var tv_args in
          let return_ty = Why3.Ty.ty_app tys tv_args in
          let cnv = empty_cnv ctx in
          let constr = List.length cases in
          let cases = List.map (fun (c,targs) ->
              let name = match c with | Lang.CTOR c -> Lang.ctor_id c | _ -> assert false in
              let id = Why3.Ident.id_fresh name in
              let targs = List.map (fun t -> Why3.Opt.get (of_tau ~cnv t)) targs in
              let ls = Why3.Term.create_fsymbol ~constr id targs return_ty in
              let proj = List.map (fun _ -> None) targs in
              (ls,proj)
            ) cases in
          let decl = Why3.Decl.create_data_decl [tys,cases] in
          ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Trec fields ->
          let id = Why3.Ident.id_fresh (Lang.type_id lt) in
          let map i _ = tvar i in
          let tv_args = List.mapi map lt.lt_params in
          let tys = Why3.Ty.create_tysymbol id tv_args NoDef in
          let tv_args = List.map Why3.Ty.ty_var tv_args in
          let return_ty = Why3.Ty.ty_app tys tv_args in
          let cnv = empty_cnv ctx in
          let fields,args = List.split @@ List.map (fun (f,ty) ->
              let name = Lang.name_of_field f in
              let id = Why3.Ident.id_fresh name in
              let ty = Why3.Opt.get (of_tau ~cnv ty) in
              let ls = Why3.Term.create_fsymbol id [return_ty] ty in
              Some ls,ty
            ) fields in
          let id = Why3.Ident.id_fresh (Lang.type_id lt) in
          let cstr = Why3.Term.create_fsymbol ~constr:1 id args return_ty in
          let decl = Why3.Decl.create_data_decl [tys,[cstr,fields]] in
          ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;

    method on_comp c (fts:(Lang.field * Lang.tau) list) =
      begin
        let compare_field (f,_) (g,_) =
          let cmp = Lang.Field.compare f g in
          if cmp = 0 then assert false (* by definition *) else cmp
        in
        let fts = List.sort compare_field fts in
        (*TODO:NUPW: manage UNIONS *)
        let id = Why3.Ident.id_fresh (Lang.comp_id c) in
        let ts = Why3.Ty.create_tysymbol id [] Why3.Ty.NoDef in
        let ty = Why3.Ty.ty_app ts [] in
        let id = Why3.Ident.id_fresh (Lang.comp_id c) in
        let cnv = empty_cnv ctx in
        let map (f,tau) =
          let ty_ctr = of_tau ~cnv tau in
          let id = Why3.Ident.id_fresh (Lang.name_of_field f) in
          let ls = Why3.Term.create_lsymbol id [ty] ty_ctr in
          (Some ls,Why3.Opt.get ty_ctr)
        in
        let fields = List.map map fts in
        let constr = Why3.Term.create_fsymbol ~constr:1 id (List.map snd fields) ty in
        let decl = Why3.Decl.create_data_decl [ts,[constr,List.map fst fields]] in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      end

    method on_dlemma l =
      begin
        let kind = Why3.Decl.(if l.l_assumed then Paxiom else Plemma) in
        let id = Why3.Ident.id_fresh (Lang.lemma_id l.l_name) in
        let id = Why3.Decl.create_prsymbol id in
        let cnv = empty_cnv ctx in
        List.iter (Lang.F.add_var cnv.pool) l.l_forall;
        let cnv, vars = Lang.For_export.in_state (mk_binders cnv) l.l_forall in
        let t = convert cnv Prop (Lang.F.e_prop l.l_lemma) in
        let triggers = full_triggers l.l_triggers in
        let triggers = Lang.For_export.in_state (List.map (List.map (of_trigger ~cnv))) triggers in
        let t = Why3.Term.t_forall_close vars triggers t in
        let decl = Why3.Decl.create_prop_decl kind id t in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      end

    method on_dfun d =
      Wp_parameters.debug ~dkey:dkey_api "Define %a@." Lang.Fun.pretty d.d_lfun ;
      let cnv = empty_cnv ctx in
      List.iter (Lang.F.add_var cnv.pool) d.d_params;
      begin
        match d.d_definition with
        | Logic t ->
            let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_name d.d_lfun)) in
            let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
            let ty_args = List.map map d.d_params in
            let id = Why3.Term.create_lsymbol id ty_args (of_tau ~cnv t) in
            let decl = Why3.Decl.create_param_decl id in
            ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
        | Function(t,mu,v) -> begin
            match mu with
            | Rec -> (* transform recursive function into an axioms *)
                let name = Qed.Export.link_name (lfun_name d.d_lfun) in
                let id = Why3.Ident.id_fresh name in
                let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
                let ty_args = List.map map d.d_params in
                let result = of_tau ~cnv t in
                let id = Why3.Term.create_lsymbol id ty_args result in
                let decl = Why3.Decl.create_param_decl id in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
                let cnv = empty_cnv ctx in
                List.iter (Lang.F.add_var cnv.pool) d.d_params;
                let cnv, vars = mk_binders cnv d.d_params in
                let t = share cnv t v in
                let t =
                  Why3.Term.t_forall_close vars []
                    (Why3.Term.t_equ
                       (Why3.Term.t_app id (List.map Why3.Term.t_var vars) result)
                       t)
                in
                let decl =
                  Why3.Decl.create_prop_decl Why3.Decl.Paxiom
                    (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh (name^"_def")))
                    t in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
            | Def ->
                let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_name d.d_lfun)) in
                let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
                let ty_args = List.map map d.d_params in
                let result = of_tau ~cnv t in
                let id = Why3.Term.create_lsymbol id ty_args result in
                let cnv, vars = mk_binders cnv d.d_params in
                let t = share cnv t v in
                let decl = Why3.Decl.make_ls_defn id vars t in
                let decl = Why3.Decl.create_logic_decl [decl] in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl
          end
        | Predicate(mu,p) -> begin
            match mu with
            | Rec ->
                let name = Qed.Export.link_name (lfun_name d.d_lfun) in
                let id = Why3.Ident.id_fresh name in
                let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
                let ty_args = List.map map d.d_params in
                let result = None in
                let id = Why3.Term.create_lsymbol id ty_args result in
                let decl = Why3.Decl.create_param_decl id in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
                let cnv = empty_cnv ctx in
                List.iter (Lang.F.add_var cnv.pool) d.d_params;
                let cnv, vars = mk_binders cnv d.d_params in
                let t = share cnv Prop (Lang.F.e_prop p) in
                let t =
                  Why3.Term.t_forall_close vars []
                    (Why3.Term.t_iff t
                       (Why3.Term.t_app id (List.map Why3.Term.t_var vars) result))
                in
                let decl =
                  Why3.Decl.create_prop_decl Why3.Decl.Paxiom
                    (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh (name^"_def")))
                    t in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
            | Def ->
                let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_name d.d_lfun)) in
                let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
                let ty_args = List.map map d.d_params in
                let id = Why3.Term.create_lsymbol id ty_args None in
                let cnv, vars = mk_binders cnv d.d_params in
                let t = share cnv Prop (Lang.F.e_prop p) in
                let decl = Why3.Decl.make_ls_defn id vars t in
                let decl = Why3.Decl.create_logic_decl [decl] in
                ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl
          end
        | Inductive dl ->
            (* create predicate symbol *)
            let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_name d.d_lfun)) in
            let map e = Why3.Opt.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
            let ty_args = List.map map d.d_params in
            let id = Why3.Term.create_lsymbol id ty_args None in
            let decl = Why3.Decl.create_param_decl id in
            ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl ;
            (* register axioms *)
            List.iter (self#on_dlemma) dl
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Goal Compilation                                                   --- *)
(* -------------------------------------------------------------------------- *)

let goal_id = (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh "wp_goal"))

let prove_goal ~id ~title ~name ?axioms t =
  (* Format.printf "why3_of_qed start@."; *)
  let goal = Definitions.cluster ~id ~title () in
  let ctx = empty_context name in
  let v = new visitor ctx goal in
  Wp_parameters.debug ~dkey:dkey_api "%t"
    begin fun fmt ->
      Format.fprintf fmt "---------------------------------------------@\n" ;
      Format.fprintf fmt "EXPORT GOAL %s@." id ;
      Format.fprintf fmt "PROP @[<hov 2>%a@]@." Lang.F.pp_pred t ;
      Format.fprintf fmt "---------------------------------------------@\n" ;
    end ;
  v#add_builtin_lib;
  v#vgoal axioms t;
  let cnv = empty_cnv ~in_goal:true ~polarity:`Positive ctx in
  let t = convert cnv Prop (Lang.F.e_prop t) in
  let decl = Why3.Decl.create_prop_decl Pgoal goal_id t in
  let th = Why3.Theory.close_theory ctx.th in
  if Wp_parameters.has_print_generated () then begin
    let th_uc_tmp = Why3.Theory.add_decl ~warn:false ctx.th decl in
    let th_tmp    = Why3.Theory.close_theory th_uc_tmp in
    Wp_parameters.debug ~dkey:Wp_parameters.cat_print_generated "%a"
      Why3.Pretty.print_theory th_tmp
  end;
  th, decl

let prove_prop ?axioms ~pid ~prop =
  let id = WpPropId.get_propid pid in
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let name = "WP" in
  let th, decl = prove_goal ?axioms ~id ~title ~name prop in
  let t = None in
  let t = Why3.Task.use_export t th in
  Why3.Task.add_decl t decl

let task_of_wpo wpo =
  let pid = wpo.Wpo.po_pid in
  match wpo.Wpo.po_formula with
  | Wpo.GoalAnnot v ->
      let pid = wpo.Wpo.po_pid in
      let axioms = v.Wpo.VC_Annot.axioms in
      let prop = Wpo.GOAL.compute_proof v.Wpo.VC_Annot.goal in
      (* Format.printf "Goal: %a@." Lang.F.pp_pred prop; *)
      prove_prop ~pid ~prop ?axioms
  | Wpo.GoalLemma v ->
      let lemma = v.Wpo.VC_Lemma.lemma in
      let depends = v.Wpo.VC_Lemma.depends in
      let prop = Lang.F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop ~pid ~prop ?axioms

(* -------------------------------------------------------------------------- *)
(* --- Prover Task                                                        --- *)
(* -------------------------------------------------------------------------- *)

let prover_task prover task =
  let env = get_why3_env () in
  let config = Why3Provers.config () in
  let prover_config = Why3.Whyconf.get_prover_config config prover in
  let drv = Why3.Whyconf.load_driver (Why3.Whyconf.get_main config)
      env prover_config.driver prover_config.extra_drivers in
  let remove_for_prover =
    if prover.prover_name = "Alt-Ergo"
    then Filter_axioms.remove_for_altergo
    else Filter_axioms.remove_for_why3
  in
  let trans = Why3.Trans.seq [
      remove_for_prover;
      Filter_axioms.trans;
      Filter_axioms.def_into_axiom
    ] in
  let task =
    if prover.prover_name = "Coq"
    then task
    else Why3.Trans.apply trans task
  in
  drv , prover_config , Why3.Driver.prepare_task drv task

(* -------------------------------------------------------------------------- *)
(* --- Prover Call                                                        --- *)
(* -------------------------------------------------------------------------- *)

let altergo_step_limit = Str.regexp "^Steps limit reached:"

type prover_call = {
  prover : Why3Provers.t ;
  call : Why3.Call_provers.prover_call ;
  steps : int option ;
  timeover : float option ;
  mutable interrupted : bool ;
  mutable killed : bool ;
}

let ping_prover_call p =
  match Why3.Call_provers.query_call p.call with
  | NoUpdates
  | ProverStarted ->
      let () = match p.timeover with
        | None -> ()
        | Some timeout ->
            let time = Unix.time () in
            if time > timeout then
              begin
                Wp_parameters.debug ~dkey "Hard Kill (late why3server timeout)" ;
                p.interrupted <- true ;
                Why3.Call_provers.interrupt_call p.call ;
              end
      in Task.Wait 100
  | InternalFailure exn ->
      let msg = Format.asprintf "@[<hov 2>%a@]"
          Why3.Exn_printer.exn_printer exn in
      Task.Return (Task.Result (VCS.failed msg))
  | ProverInterrupted -> Task.(Return Canceled)
  | ProverFinished _ when p.killed -> Task.(Return Canceled)
  | ProverFinished pr ->
      let r =
        match pr.pr_answer with
        | Timeout -> VCS.timeout (int_of_float pr.pr_time)
        | Valid -> VCS.result ~time:pr.pr_time ~steps:pr.pr_steps VCS.Valid
        | Invalid -> VCS.result ~time:pr.pr_time ~steps:pr.pr_steps VCS.Invalid
        | OutOfMemory -> VCS.failed "out of memory"
        | StepLimitExceeded -> VCS.result ?steps:p.steps VCS.Stepout
        | Unknown _ -> VCS.unknown
        | _ when p.interrupted -> VCS.timeout (int_of_float pr.pr_time)
        | Failure s -> VCS.failed s
        | HighFailure ->
            let alt_ergo_hack =
              p.prover.prover_name = "Alt-Ergo" &&
              Str.string_match altergo_step_limit pr.pr_output 0
            in
            if alt_ergo_hack then VCS.result ?steps:p.steps VCS.Stepout
            else VCS.failed "Unknown error"
      in
      Wp_parameters.debug ~dkey
        "@[@[Why3 result for %a:@] @[%a@] and @[%a@]@."
        Why3.Whyconf.print_prover p.prover
        (Why3.Call_provers.print_prover_result) pr
        VCS.pp_result r;
      Task.Return (Task.Result r)

let call_prover ~timeout ~steplimit drv prover prover_config task =
  let steps = match steplimit with Some 0 -> None | _ -> steplimit in
  let limit =
    let def = Why3.Call_provers.empty_limit in
    { def with
      Why3.Call_provers.limit_time = Why3.Opt.get_def def.limit_time timeout;
      Why3.Call_provers.limit_steps = Why3.Opt.get_def def.limit_time steps;
    } in
  let command = Why3.Whyconf.get_complete_command prover_config
      ~with_steps:(steps<>None) in
  let call =
    Why3.Driver.prove_task_prepared ~command ~limit drv task in
  Wp_parameters.debug ~dkey "Why3 run prover %a with %i timeout %i steps@."
    Why3.Whyconf.print_prover prover
    (Why3.Opt.get_def (-1) timeout)
    (Why3.Opt.get_def (-1) steps);
  let timeover = match timeout with
    | None -> None | Some tlimit ->
        let started = Unix.time () in
        Some (started +. 2.0 +. float tlimit) in
  let pcall = {
    call ; prover ;
    killed = false ;
    interrupted = false ;
    steps ; timeover ;
  } in
  let ping = function
    | Task.Kill ->
        pcall.killed <- true ;
        Why3.Call_provers.interrupt_call call ;
        Task.Yield
    | Task.Coin -> ping_prover_call pcall
  in
  Task.async ping

(* -------------------------------------------------------------------------- *)
(* --- Cache Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

type mode = NoCache | Update | Replay | Rebuild | Offline | Cleanup

let hits = ref 0
let miss = ref 0
let removed = ref 0
let cleanup = Hashtbl.create 0
(* used entries, never to be reset since cleanup is performed at exit *)

let get_hits () = !hits
let get_miss () = !miss
let get_removed () = !removed

let mark_cache ~mode hash =
  if mode = Cleanup || !Config.is_gui then Hashtbl.replace cleanup hash ()

let cleanup_cache ~mode =
  if mode = Cleanup && (!hits > 0 || !miss > 0) then
    let dir = Wp_parameters.get_session_dir "cache" in
    try
      if Sys.is_directory dir then
        Array.iter
          (fun f ->
             if Filename.check_suffix f ".json" then
               let hash = Filename.chop_suffix f ".json" in
               if not (Hashtbl.mem cleanup hash) then
                 begin
                   incr removed ;
                   Extlib.safe_remove (Printf.sprintf "%s/%s" dir f) ;
                 end
          ) (Sys.readdir dir) ;
    with Unix.Unix_error _ as exn ->
      Wp_parameters.warning ~current:false
        "Can not cleanup cache (%s)" (Printexc.to_string exn)

(* -------------------------------------------------------------------------- *)
(* --- Cache Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse_mode ~origin ~fallback = function
  | "none" -> NoCache
  | "update" -> Update
  | "replay" -> Replay
  | "rebuild" -> Rebuild
  | "offline" -> Offline
  | "cleanup" -> Cleanup
  | "" -> raise Not_found
  | m ->
      Wp_parameters.warning ~current:false
        "Unknown %s mode %S (use %s instead)" origin m fallback ;
      raise Not_found

let mode_name = function
  | NoCache -> "none"
  | Update -> "update"
  | Replay -> "replay"
  | Rebuild -> "rebuild"
  | Offline -> "offline"
  | Cleanup -> "cleanup"

module MODE = WpContext.StaticGenerator(Datatype.Unit)
    (struct
      type key = unit
      type data = mode
      let name = "Wp.Cache.mode"
      let compile () =
        try
          let origin = "FRAMAC_WP_CACHE" in
          parse_mode ~origin ~fallback:"-wp-cache" (Sys.getenv origin)
        with Not_found ->
        try
          parse_mode ~origin:"-wp-cache" ~fallback:"none"
            (Wp_parameters.Cache.get())
        with Not_found ->
          if Wp_parameters.has_session ()
          then Update else NoCache
    end)

let get_mode = MODE.get
let set_mode m = MODE.clear () ; Wp_parameters.Cache.set (mode_name m)

let task_hash wpo drv prover task =
  lazy
    begin
      let file = Wpo.DISK.file_goal
          ~pid:wpo.Wpo.po_pid
          ~model:wpo.Wpo.po_model
          ~prover:(VCS.Why3 prover) in
      let _ = Command.print_file file
          begin fun fmt ->
            Format.fprintf fmt "(* WP Task for Prover %s *)@\n"
              (Why3Provers.print prover) ;
            Why3.Driver.print_task_prepared drv fmt task ;
          end
      in Digest.file file |> Digest.to_hex
    end

let time_fits time = function
  | None | Some 0 -> true
  | Some limit -> time <= float limit

let steps_fits steps = function
  | None | Some 0 -> true
  | Some limit -> steps <= limit

let time_seized time = function
  | None | Some 0 -> false
  | Some limit -> float limit <= time

let steps_seized steps steplimit =
  steps <> 0 &&
  match steplimit with
  | None | Some 0 -> false
  | Some limit -> limit <= steps

let promote ~timeout ~steplimit (res : VCS.result) =
  match res.verdict with
  | VCS.NoResult | VCS.Computing _ | VCS.Checked -> VCS.no_result
  | VCS.Failed -> res
  | VCS.Invalid | VCS.Valid | VCS.Unknown ->
      if not (steps_fits res.prover_steps steplimit) then
        { res with verdict = Stepout }
      else
      if not (time_fits res.prover_time timeout) then
        { res with verdict = Timeout }
      else res
  | VCS.Timeout | VCS.Stepout ->
      if steps_seized res.prover_steps steplimit then
        { res with verdict = Stepout }
      else
      if time_seized res.prover_time timeout then
        { res with verdict = Timeout }
      else (* can be run a longer time or widely *)
        VCS.no_result

let get_cache_result ~mode hash =
  match mode with
  | NoCache | Rebuild -> VCS.no_result
  | Update | Cleanup | Replay | Offline ->
      let dir = Wp_parameters.get_session_dir "cache" in
      let hash = Lazy.force hash in
      let file = Printf.sprintf "%s/%s.json" dir hash in
      if not (Sys.file_exists file) then VCS.no_result
      else
        try
          mark_cache ~mode hash ;
          Json.load_file file |> ProofScript.result_of_json
        with err ->
          Wp_parameters.warning ~current:false ~once:true
            "invalid cache entry (%s)" (Printexc.to_string err) ;
          VCS.no_result

let set_cache_result ~mode hash prover result =
  match mode with
  | NoCache | Replay | Offline -> ()
  | Rebuild | Update | Cleanup ->
      let dir = Wp_parameters.get_session_dir "cache" in
      let hash = Lazy.force hash in
      let file = Printf.sprintf "%s/%s.json" dir hash in
      try
        mark_cache ~mode hash ;
        ProofScript.json_of_result (VCS.Why3 prover) result
        |> Json.save_file file
      with err ->
        Wp_parameters.warning ~current:false ~once:true
          "can not update cache (%s)" (Printexc.to_string err)

let is_trivial (t : Why3.Task.task) =
  let goal = Why3.Task.task_goal_fmla t in
  Why3.Term.t_equal goal Why3.Term.t_true

(* -------------------------------------------------------------------------- *)
(* --- Prove WPO                                                          --- *)
(* -------------------------------------------------------------------------- *)

let build_proof_task ?timeout ?steplimit ~prover wpo () =
  try
    WpContext.on_context (Wpo.get_context wpo)
      begin fun () ->
        (* Always generate common task *)
        let task = task_of_wpo wpo in
        if Wp_parameters.Check.get ()
        then Task.return VCS.checked (* Why3 tasks are type-checked *)
        else
        if Wp_parameters.Generate.get ()
        then Task.return VCS.no_result (* Only generate *)
        else
          let drv , config , task = prover_task prover task in
          if is_trivial task then
            Task.return VCS.valid
          else
            let mode = get_mode () in
            match mode with
            | NoCache ->
                call_prover ~timeout ~steplimit drv prover config task
            | Offline ->
                let hash = task_hash wpo drv prover task in
                let result = get_cache_result ~mode hash |> VCS.cached in
                if VCS.is_verdict result then incr hits else incr miss ;
                Task.return result
            | Update | Replay | Rebuild | Cleanup ->
                let hash = task_hash wpo drv prover task in
                let result =
                  get_cache_result ~mode hash
                  |> promote ~timeout ~steplimit |> VCS.cached in
                if VCS.is_verdict result
                then
                  begin
                    incr hits ;
                    Task.return result
                  end
                else
                  Task.finally
                    (call_prover ~timeout ~steplimit drv prover config task)
                    begin function
                      | Task.Result result when VCS.is_verdict result ->
                          incr miss ;
                          set_cache_result ~mode hash prover result
                      | _ -> ()
                    end
      end ()
  with exn ->
    if Wp_parameters.has_dkey dkey_api then
      Wp_parameters.fatal "[Why3 Error] %a@\n%s"
        Why3.Exn_printer.exn_printer exn
        Printexc.(raw_backtrace_to_string @@ get_raw_backtrace ())
    else
      Task.failed "[Why3 Error] %a" Why3.Exn_printer.exn_printer exn

let prove ?timeout ?steplimit ~prover wpo =
  Task.later (build_proof_task ?timeout ?steplimit ~prover wpo) ()

(* -------------------------------------------------------------------------- *)

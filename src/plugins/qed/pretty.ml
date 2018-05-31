(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printer with sharing                                        --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Plib

module Make(T : Term) =
struct

  open T

  (* -------------------------------------------------------------------------- *)
  (* ---  Types                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let pp_tvarn fmt n = fprintf fmt "?%d" n
  let pp_alpha fmt = function
    | 0 -> pp_print_string fmt "'a"
    | 1 -> pp_print_string fmt "'b"
    | 2 -> pp_print_string fmt "'c"
    | 3 -> pp_print_string fmt "'d"
    | 4 -> pp_print_string fmt "'e"
    | n -> fprintf fmt "?%d" (n-4)

  let pp_tau fmt t =
    let n = Kind.degree_of_tau t in
    if 0<=n && n<5
    then Kind.pp_tau pp_alpha Field.pretty ADT.pretty fmt t
    else Kind.pp_tau pp_tvarn Field.pretty ADT.pretty fmt t

  (* -------------------------------------------------------------------------- *)
  (* --- Shareable                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let shareable e = match T.repr e with
    | And _ | Or _ | Not _ | Imply _ | Eq _ | Neq _ | Leq _ | Lt _ -> false
    | Fun(f,_) -> (Fun.sort f <> Sprop && Fun.sort f <> Sbool)
    | _ -> true

  let subterms f e =
    match T.repr e with
    | Rdef fts ->
        begin
          match T.record_with fts with
          | None -> T.lc_iter f e
          | Some(a,fts) -> f a ; List.iter (fun (_,e) -> f e) fts
        end
    | _ -> T.lc_iter f e

  (* -------------------------------------------------------------------------- *)
  (* --- Variables                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  module Idx = Map.Make(String)
  module Ids = Set.Make(String)

  type env = {
    mutable bound : string Intmap.t ; (* bound var *)
    mutable named : string Tmap.t ; (* named terms *)
    mutable index : int Idx.t ;     (* index names *)
    mutable known : Ids.t ;         (* known names *)
  }

  (* -------------------------------------------------------------------------- *)
  (* --- Environment                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let empty = {
    bound = Intmap.empty ;
    named = Tmap.empty ;
    index = Idx.empty ;
    known = Ids.empty ;
  }

  let copy env = {
    bound = env.bound ;
    named = env.named ;
    index = env.index ;
    known = env.known ;
  }

  (* -------------------------------------------------------------------------- *)
  (* --- Fresh                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let freshname env base =
    let rec scan env base k =
      let a = Printf.sprintf "%s_%d" base k in
      if Ids.mem a env.known
      then scan env base (succ k)
      else (env.index <- Idx.add base (succ k) env.index ; a) in
    scan env base
      (try Idx.find base env.index with Not_found -> 0)

  let known env xs =
    let env = copy env in
    Vars.iter
      (fun x ->
         let x = Plib.to_string Var.pretty x in
         env.known <- Ids.add x env.known
      ) xs ; env

  let marks env =
    T.marks ~shareable ~subterms
      ~shared:(fun t -> Tmap.mem t env.named) ()

  let bind x t env =
    let env = copy env in
    env.named <- Tmap.add t x env.named ;
    env.known <- Ids.add x env.known ;
    env

  let fresh env t =
    let env = copy env in
    let x = freshname env (T.basename t) in
    env.named <- Tmap.add t x env.named ;
    env.known <- Ids.add x env.known ;
    x , env

  let bind_var env k t =
    let x = freshname env (Tau.basename t) in
    env.known <- Ids.add x env.known ;
    env.bound <- Intmap.add k x env.bound ; x

  let find_var env k =
    try Intmap.find k env.bound
    with Not_found -> Printf.sprintf "#%d" k

  (* -------------------------------------------------------------------------- *)
  (* --- Groups of Quantifiers                                              --- *)
  (* -------------------------------------------------------------------------- *)

  module TauMap = Map.Make(T.Tau)

  let group_var t k = TauMap.add t [k] TauMap.empty

  let group_add t k tks =
    let ks = k :: try TauMap.find t tks with Not_found -> [] in
    TauMap.add t ks tks

  let rec group_binders k = function
    | [] -> []
    | (q,t)::qts -> group_collect q (succ k) (group_var t k) qts

  and group_collect q k kts = function
    | [] -> [q,kts]
    | (q0,t) :: qts ->
        if q = q0 && q0 <> Lambda then
          group_collect q (succ k) (group_add t k kts) qts
        else
          (q,kts) :: group_collect q0 (succ k) (group_var t k) qts

  (* -------------------------------------------------------------------------- *)
  (* --- Output Form                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  type out =
    | Sum of term list
    | Atom of string
    | Hbox of string * term list
    | Vbox of string * term list
    | Unop of string * term
    | Binop of ( term * string * term )
    | Cond of ( term * term * term )
    | Call of Fun.t * term list
    | Closure of term * term list
    | Const of term
    | Access of term * term
    | Update of term * term * term
    | Record of field list
    | GetField of term * Field.t
    | Abstraction of (binder * tau) list * term
    | Bind of int

  and field =
    | With  of term
    | Field of Field.t * term
    | Last  of Field.t * term

  let rec fields = function
    | [] -> []
    | [f,v] -> [Last(f,v)]
    | (f,v)::fvs -> Field(f,v)::fields fvs

  let rec abstraction qxs e =
    match T.repr e with
    | Logic.Bind(q,x,t) -> abstraction ((q,x)::qxs) (lc_repr t)
    | _ -> Abstraction( List.rev qxs , e )

  let out e =
    match T.repr e with
    | Bvar(k,_) -> Bind k
    | Fvar x -> Atom( Plib.to_string Var.pretty x )
    | True -> Atom "true"
    | False -> Atom "false"
    | Kint z -> Atom (Z.to_string z)
    | Kreal r -> Atom (Q.to_string r)
    | Times(z,e) when Z.equal z Z.minus_one -> Unop("-",e)
    | Times(z,e) -> Hbox("*",[e_zint z;e])
    | Add es -> Sum es
    | Mul es -> Hbox("*",es)
    | Div(a,b) -> Binop(a,"div",b)
    | Mod(a,b) -> Binop(a,"mod",b)
    | And es -> Vbox("/\\",es)
    | Or  es -> Vbox("\\/",es)
    | Not e -> Unop("not ",e)
    | Imply(hs,p) ->Vbox("->",hs@[p])
    | Eq(a,b) ->
        if T.sort e = Sprop
        then Vbox("<->",[a;b])
        else Hbox("=",[a;b])
    | Lt(a,b) -> Hbox("<",[a;b])
    | Neq(a,b) -> Hbox("!=",[a;b])
    | Leq(a,b) -> Hbox("<=",[a;b])
    | Fun(a,es) -> Call(a,es)
    | Apply(e,es) -> Closure(e,es)
    | If(c,a,b) -> Cond(c,a,b)
    | Acst(_,v) -> Const(v)
    | Aget(a,b) -> Access(a,b)
    | Aset(a,b,c) -> Update(a,b,c)
    | Logic.Bind(q,x,e) -> abstraction [q,x] (lc_repr e)
    | Rget(e,f) -> GetField(e,f)
    | Rdef fvs -> Record
                    begin
                      match T.record_with fvs with
                      | None -> fields fvs
                      | Some(base,fothers) -> With base :: fields fothers
                    end

  let named_out env e =
    try Atom(Tmap.find e env.named)
    with Not_found -> out e

  (* -------------------------------------------------------------------------- *)
  (* --- Atom printer                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec pp_atom (env:env) (fmt:formatter) e =
    pp_atom_out env fmt (named_out env e)

  and pp_atom_out env fmt = function
    | Bind k -> pp_print_string fmt (find_var env k)
    | Atom x -> pp_print_string fmt x
    | Call(f,es) -> pp_call env fmt f es
    | Sum es -> fprintf fmt "@[<hov 1>(%a)@]" (pp_sum false env) es
    | Hbox(op,es) -> fprintf fmt "@[<hov 1>(%a)@]" (pp_hbox env op) es
    | Vbox(op,es) -> fprintf fmt "@[<hov 1>(%a)@]" (pp_vbox env op) es
    | Unop(op,e) -> fprintf fmt "@[<hov 3>(%s%a)@]" op (pp_atom env) e
    | Binop op -> fprintf fmt "@[<hov 3>(%a)@]" (pp_binop env) op
    | Cond c -> fprintf fmt "@[<hv 1>(%a)@]" (pp_cond env) c
    | Closure(e,es) -> pp_closure env fmt e es
    | Abstraction(qts,abs) -> fprintf fmt "@[<v 1>(%t)@]" (pp_abstraction env qts abs)
    | Const(v) -> fprintf fmt "@[<hov 2>[%a..]@]" (pp_free env) v
    | Access(a,b) -> fprintf fmt "@[<hov 2>%a@,[%a]@]"
                       (pp_atom env) a (pp_free env) b
    | Update(a,b,c) -> fprintf fmt "@[<hov 2>%a@,[%a@,->%a]@]"
                         (pp_atom env) a (pp_atom env) b (pp_free env) c
    | GetField(e,f) -> fprintf fmt "%a.%a" (pp_atom env) e Field.pretty f
    | Record fs -> pp_fields env fmt fs

  and pp_free_out env fmt = function
    | Bind k -> pp_print_string fmt (find_var env k)
    | Atom x -> pp_print_string fmt x
    | Call(f,es) -> pp_call env fmt f es
    | Sum es -> fprintf fmt "@[<hov 1>%a@]" (pp_sum true env) es
    | Hbox(op,es) -> fprintf fmt "@[<hov 0>%a@]" (pp_hbox env op) es
    | Vbox(op,es) -> fprintf fmt "@[<hov 0>%a@]" (pp_vbox env op) es
    | Unop(op,e) -> fprintf fmt "@[<hov 2>%s%a@]" op (pp_atom env) e
    | Binop op -> fprintf fmt "@[<hov 2>%a@]" (pp_binop env) op
    | Cond c -> fprintf fmt "@[<hv 0>%a@]" (pp_cond env) c
    | Closure(e,es) -> pp_closure env fmt e es
    | Abstraction(qts,abs) -> fprintf fmt "@[<hv 0>%t@]" (pp_abstraction env qts abs)
    | (Const _ | Access _ | Update _ | Record _ | GetField _) as a -> pp_atom_out env fmt a

  and pp_fields (env:env) (fmt:formatter) fs =
    fprintf fmt "@[<hv 0>{@[<hv 2>" ;
    List.iter
      (function
        | With r ->
            fprintf fmt "@ %a with" (pp_atom env) r
        | Field (f,v) ->
            fprintf fmt "@ @[<hov 2>%a =@ %a ;@]" Field.pretty f (pp_free env) v
        | Last (f,v) ->
            fprintf fmt "@ @[<hov 2>%a =@ %a@]" Field.pretty f (pp_free env) v
      ) fs ;
    fprintf fmt "@]@ }@]"

  (* -------------------------------------------------------------------------- *)
  (* --- Free printer                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_free (env:env) (fmt:formatter) e = pp_free_out env fmt (named_out env e)
  and pp_freedef (env:env) (fmt:formatter) e = pp_free_out env fmt (out e)

  (* -------------------------------------------------------------------------- *)
  (* --- Call printer                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_call (env:env) (fmt:formatter) f = function
    | [] -> Fun.pretty fmt f
    | es ->
        fprintf fmt "@[<hov 2>(%a" Fun.pretty f ;
        List.iter (fun e -> fprintf fmt "@ %a" (pp_atom env) e) es ;
        fprintf fmt ")@]"

  (* -------------------------------------------------------------------------- *)
  (* --- Sum printer                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_sum free (env:env) (fmt:formatter) es =
    let ps,ns = List.fold_right
        (fun e (ps,ns) -> match T.repr e with
           | Times(k,n) when Z.equal k Z.minus_one -> (ps,n::ns)
           | Kint k when Z.lt k Z.zero -> (ps,e_zint (Z.neg k) :: ns)
           | Kreal r when Q.lt r Q.zero -> (ps,e_real (Q.neg r) :: ns)
           | _ -> e::ps , ns)
        es ([],[])
    in match ps , ns with
    | [] , [] -> pp_print_string fmt "0"
    | [] , _ ->
        if free
        then fprintf fmt "(%a)" (pp_factor env "-") ns
        else pp_factor env "-" fmt ns
    | p::ps , ns -> fprintf fmt "%a%a%a"
                      (pp_atom env) p
                      (pp_factor env "+") ps
                      (pp_factor env "-") ns

  and pp_factor env op fmt es =
    List.iter (fun e -> fprintf fmt "%s@,%a" op (pp_atom env) e) es

  (* -------------------------------------------------------------------------- *)
  (* --- Horizontal Boxes                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_hbox (env:env) (sep:string) (fmt:formatter) = function
    | [] -> pp_print_string fmt "()"
    | e::es -> fprintf fmt "%a%a" (pp_atom env) e (pp_factor env sep) es

  (* -------------------------------------------------------------------------- *)
  (* --- Vertical Boxes                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_vbox (env:env) (sep:string) (fmt:formatter) = function
    | [] -> ()
    | e::es ->
        pp_atom env fmt e ;
        List.iter (fun e -> fprintf fmt "@ %s %a" sep (pp_atom env) e) es

  (* -------------------------------------------------------------------------- *)
  (* --- Specific Operators                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_binop (env:env) (fmt:formatter) (a,op,b) =
    fprintf fmt "%a@ %s %a" (pp_atom env) a op (pp_atom env) b

  and pp_cond (env:env) (fmt:formatter) (c,a,b) =
    fprintf fmt "if %a@ then %a@ else %a"
      (pp_atom env) c
      (pp_atom env) a
      (pp_atom env) b

  and pp_closure (env:env) (fmt:formatter) e es =
    fprintf fmt "@[<hov 3>(%a" (pp_atom env) e ;
    List.iter (fun e -> fprintf fmt "@ %a" (pp_atom env) e) es ;
    fprintf fmt ")@]"

  (* -------------------------------------------------------------------------- *)
  (* --- Abstraction                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_abstraction (env:env) qts abs (fmt:formatter) =
    let env = copy env in
    let groups = group_binders 0 qts in
    let size = List.length qts in
    let last = Bvars.order (lc_vars abs) + size - 1 in
    List.iter
      (fun (q,m) ->
         match q with
         | Forall -> fprintf fmt "@[<hov 4>forall %a.@]@ " (pp_group env last) m
         | Exists -> fprintf fmt "@[<hov 4>exists %a.@]@ " (pp_group env last) m
         | Lambda -> fprintf fmt "@[<hov 4>fun %a ->@]@ " (pp_group env last) m
      ) groups ;
    pp_share env fmt abs

  and pp_group (env:env) (last:int) (fmt:formatter) m =
    let sep = ref false in
    TauMap.iter
      (fun t ks ->
         if !sep then fprintf fmt ",@," ;
         Plib.iteri
           (fun idx k ->
              let x = bind_var env (last - k) t in
              match idx with
              | Isingle | Ifirst -> pp_print_string fmt x
              | Imiddle | Ilast -> fprintf fmt ",@,%s" x
           ) (List.rev ks) ;
         fprintf fmt ":%a" pp_tau t ;
         sep := true ;
      ) m

  (* -------------------------------------------------------------------------- *)
  (* --- Sharing                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  and pp_share (env:env) (fmt:formatter) t =
    begin
      fprintf fmt "@[<hv 0>" ;
      let shared t = Tmap.mem t env.named in
      let ts = T.shared ~shareable ~shared ~subterms [t] in
      let env =
        List.fold_left
          (fun env t ->
             let x,env_x = fresh env t in
             fprintf fmt "@[<hov 4>let %s =@ %a in@]@ " x (pp_atom env) t ;
             env_x)
          env ts in
      pp_free env fmt t ;
      fprintf fmt "@]" ;
    end

  (* -------------------------------------------------------------------------- *)
  (* --- Entry Point                                                        --- *)
  (* -------------------------------------------------------------------------- *)


  let pp_term_env (env:env) (fmt:formatter) t = pp_share env fmt t
  let pp_def_env (env:env) (fmt:formatter) t = pp_freedef env fmt t

  let pp_term (env:env) (fmt:formatter) t = pp_term_env (copy env) fmt t
  let pp_def (env:env) (fmt:formatter) t = pp_def_env (copy env) fmt t

end

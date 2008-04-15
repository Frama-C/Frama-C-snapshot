(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* ========================================================================= *)
(* Fourier-Motzkin elimination for integers.                                 *)
(* ========================================================================= *)

(* We use the functions already defined in [qelim.ml] and [cooper.ml]. *)

(* ------------------------------------------------------------------------- *)
(* Linearize the atoms in a formula, and eliminate strict inequalities.      *)
(* ------------------------------------------------------------------------- *)

let mkatom vars p t = Atom(R(p,[Fn("0",[]);lint vars t]));;

let linform vars fm =
  match fm with
    Atom(R("=",[s;t])) -> mkatom vars "=" (Fn("-",[t;s]))
  | Atom(R("<=",[s;t])) -> mkatom vars "<=" (Fn("-",[t;s]))
  | Atom(R(">=",[s;t])) -> mkatom vars "<=" (Fn("-",[s;t]))
  | Atom(R("<",[s;t])) ->
        mkatom vars "<=" (Fn("-",[Fn("-",[t;Fn("1",[])]);s]))
  | Atom(R(">",[s;t])) ->
        mkatom vars "<=" (Fn("-",[Fn("-",[s;Fn("1",[])]);t]))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Post-NNF transformation eliminating negated inequalities.                 *)
(* ------------------------------------------------------------------------- *)

let rec posineq fm =
  match fm with
  | Not(Atom(R("<=",[Fn("0",[]); t]))) ->
        Atom(R("<=",[Fn("0",[]); linear_sub [] (Fn("-1",[])) t]))
  | Not(Atom(R("=",[Fn("0",[]); t]))) ->
        Or(Atom(R("<=",[Fn("0",[]); linear_sub [] (Fn("-1",[])) t])),
	   Atom(R("<=",[Fn("0",[]); linear_add [] (Fn("-1",[])) t])))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Adjust all coefficients of x in formula; fold in reduction to +/- 1.      *)
(* ------------------------------------------------------------------------- *)

let rec adjustcoeff x l fm =
  match fm with
    Atom(R(p,[d; Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
        let m = l // dest_numeral c in
        let n = if p = "<=" then abs_num(m) else m in
        let xtm = Fn("*",[mk_numeral(m // n); x]) in
        Atom(R(p,[linear_cmul (abs_num m) d;
                Fn("+",[xtm; linear_cmul n z])]))
  | Not(p) -> Not(adjustcoeff x l p)
  | And(p,q) -> And(adjustcoeff x l p,adjustcoeff x l q)
  | Or(p,q) -> Or(adjustcoeff x l p,adjustcoeff x l q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Make coefficient of x one in existential formula.                         *)
(* ------------------------------------------------------------------------- *)

let unitycoeff x fm =
  let l = formlcm x fm in
  let fm' = adjustcoeff x l fm in
  if l =/ Int 1 then fm' else
  adjustcoeff x l fm;;

(* ------------------------------------------------------------------------- *)
(* Isolate x on left-hand-side and replace.                                  *)
(* ------------------------------------------------------------------------- *)

let isolate x fm =
  match fm with
    Atom(R(p,[_;Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
      let c = mk_numeral (Int 0 -/ dest_numeral c) in
      Atom(R(p,[Fn("*",[c;y]);z]))
  | Atom(R(p,[zero;Fn("*",[c;y])])) when y = x ->
      let c = mk_numeral (Int 0 -/ dest_numeral c) in
      Atom(R(p,[Fn("*",[c;y]);zero]))
  | _ -> 
      printer fm;
      assert false

let replace vars x t fm =
  match fm with
    Atom(R(p,[Fn("*",[c;y]);z])) when y = x ->
      let t = if dest_numeral c >/ Int 0 then t else linear_neg t in
      linform vars (Atom(R(p,[t;z])))
  | _ -> assert false

(* ------------------------------------------------------------------------- *)
(* This is the base function.                                                *)
(* ------------------------------------------------------------------------- *)

let fourier vars fm =
  match fm with
    Exists(x0,p0) ->
      let x = Var x0 in
      let p = unitycoeff x p0 in
      let cjs = map (isolate x) (conjuncts p) in
      try let eqn = find
            (function (Atom(R("=",_))) -> true | _ -> false) cjs in
          let (Atom(R("=",[s;t]))) = eqn in
	  let (Fn("*",[c;_])) = s in
	  let y = if dest_numeral c =/ Int 1 then t else linear_neg t in 
          list_conj(map (replace vars x y) (subtract cjs [eqn]))
      with Failure _ ->
        let l,r =
          partition 
	    (fun (Atom(R("<=",[Fn("*",[c;_]);t]))) -> 
	      dest_numeral c =/ Int (-1)) cjs 
	in
        let lefts = map (fun (Atom(R("<=",[_;l]))) -> linear_neg l) l
        and rights = map (fun (Atom(R("<=",[_;r]))) -> r) r in
        list_conj(allpairs 
	  (fun l r -> Atom(R("<=",[Fn("0",[]); linear_sub vars r l])))
          lefts rights)
  | _ -> failwith "fourier";;

(* ------------------------------------------------------------------------- *)
(* Overall quelim procedure.                                                 *)
(* ------------------------------------------------------------------------- *)

let fourier_qelim =
  simplify ** evalc **
  lift_qelim linform (dnf ** cnnf posineq ** evalc) fourier;;

(*
Local Variables:
compile-command: "LC_ALL=C ocamlc -I . fourier_motzkin.ml"
End:
*)

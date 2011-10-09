(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau

let dkey = "logic" (* debugging key *)
let simpl = Wp_parameters.Simpl.get

(* [LC] no file LogicRaw.mli to make internal representation visible *)

(* -------------------------------------------------------------------------- *)
(* --- Variables                                                          --- *)
(* -------------------------------------------------------------------------- *)

module VAR =
struct

  type t = {
    var_vid : int ;
    var_base : string ;
    var_tau : tau ;
  }

  type pool = int ref

  let pool () = ref 0

  let basename x = x.var_base
  let tau_of_var x = x.var_tau
  let fresh pool base tau = 
    incr pool ; { var_base=base ; var_tau=tau ; var_vid= !pool }
  let freshen pool x =
    incr pool ; { x with var_vid= !pool }

  let different x y = x.var_vid <> y.var_vid
  let equal x y = (x.var_vid = y.var_vid)
  let compare x y = Pervasives.compare x.var_vid y.var_vid
  let hash x = x.var_vid

  let pretty fmt x = Format.fprintf fmt "%s#%d" x.var_base x.var_vid

end

module VMAP = Map.Make(VAR)
module VSET = Set.Make(VAR)

(* -------------------------------------------------------------------------- *)
(* --- Terms                                                              --- *)
(* -------------------------------------------------------------------------- *)

module TERM =
struct

  type primitive =

    | L_eq | L_neq
      
    (* I operations & comparisons *)
    | I_add | I_sub | I_mul | I_div | I_mod | I_opp
    | I_lt | I_leq
    | I_lsl | I_lsr | I_band | I_bor | I_bxor | I_bnot

    (* R operations & comparisons *)
    | R_add | R_sub | R_mul | R_div | R_opp
    | R_lt | R_leq

    (* R&I conversions *)
    | R_of_I | I_of_R

    (* B operations *)
    | B_and | B_or | B_not

  type t =
    | Ttrue
    | Tfalse
    | Tint of string
    | Treal of string
    | Tprim of primitive * t list
    | Tcall of id * t list
    | Tgetfield of t * field
    | Tsetfield of t * field * t
    | Taccess of t * t
    | Tupdate of t * t * t
    | Tif of t * t * t
    | Tlet of VAR.t * t * t
    | Tvar of VAR.t

  (* Functorial iterator *)
  let iter f = function
    | Ttrue | Tfalse | Tint _ | Treal _ | Tvar _ -> ()
    | Tprim(_,ts) | Tcall(_,ts) -> List.iter f ts
    | Tgetfield(a,_) -> f a
    | Tsetfield(a,_,b) | Taccess(a,b) | Tlet(_,a,b) -> f a ; f b
    | Tupdate(a,b,c) | Tif(a,b,c) -> f a ; f b ; f c
	  
  let rec depend ids = function
    | Ttrue | Tfalse | Tint _ | Treal _ | Tvar _ -> ids
    | Tprim(_,ts) -> List.fold_left depend ids ts
    | Tcall(f,ts) -> List.fold_left depend (Iset.add f ids) ts
    | Tgetfield(a,f) -> depend (Iset.add f.f_record ids) a
    | Tsetfield(a,f,b) ->
	depend (depend (Iset.add f.f_record ids) a) b
    | Taccess(a,b) | Tlet(_,a,b) -> 
	depend (depend ids a) b
    | Tupdate(a,b,c) | Tif(a,b,c) ->
	depend (depend (depend ids a) b) c
    
  let rec equal e1 e2 =
    match e1, e2 with
      | Tint x1, Tint x2 -> x1=x2
      | Treal x1, Treal x2 -> x1=x2
      | Ttrue , Ttrue -> true
      | Tfalse , Tfalse -> false
      | Tcall (f1, args1), Tcall (f2, args2) ->
          (LogicId.equal f1 f2) && 
	    (List.length args1 = List.length args2) &&
	    (List.for_all2 equal args1 args2)
      | Tprim (f1, args1), Tprim (f2, args2) ->
          (f1=f2) && 
	    (List.length args1 = List.length args2) &&
	    (List.for_all2 equal args1 args2)
      | Tif (c1, t1, e1), Tif (c2, t2, e2) ->
          equal c1 c2 && equal t1 t2 && equal e1 e2
      | Tlet (x,v,t),Tlet(x',v',t') ->
          VAR.equal x x' && equal v v' && equal t t'
      | Tgetfield (r,f) , Tgetfield(s,g) ->
          LogicId.equal f.f_name g.f_name && equal r s
      | Tsetfield (r,f,v) , Tsetfield(s,g,w) ->
          LogicId.equal f.f_name g.f_name && equal r s && equal v w
      | Taccess (t,i) , Taccess (u,j) ->
          equal t u && equal i j
      | Tupdate (t,i,v), Tupdate(u,j,w) ->
          equal t u && equal i j && equal v w
      | _ -> false

  let rec different e1 e2 =
    match e1 , e2 with
      | Tint x1 , Tint x2 ->
	  not (My_bigint.equal (My_bigint.of_string x1) (My_bigint.of_string x2))
      | _ -> false

  let i_compute f x y = 
    Tint (My_bigint.to_string (f (My_bigint.of_string x) (My_bigint.of_string y)))

  let i_compare f x y = 
    if f (My_bigint.compare (My_bigint.of_string x) (My_bigint.of_string y)) then Ttrue else Tfalse

  let rec e_not = function
    | Tprim(B_not,[p]) -> p
    | Tprim(B_and,[p;q]) -> Tprim(B_or,[e_not p;e_not q])
    | Tprim(B_or,[p;q]) -> Tprim(B_and,[e_not p;e_not q])
    | Tprim(I_lt,[a;b]) -> Tprim(I_leq,[b;a])
    | Tprim(I_leq,[a;b]) -> Tprim(I_lt,[b;a])
    | p -> Tprim(B_not,[p])

  let e_and a b =
    match a,b with
      | Ttrue,c | c,Ttrue -> c
      | Tfalse,_ | _,Tfalse -> Tfalse
      | _ -> Tprim(B_and,[a;b])

  let e_or a b =
    match a,b with
      | Tfalse,c | c,Tfalse -> c
      | Ttrue,_ | _,Ttrue -> Ttrue
      | _ -> Tprim(B_or,[a;b])

  let e_zero = Tint "0"
  let e_int = function 0 -> e_zero | n -> Tint (string_of_int n)

  let rec e_prim f ts =
    if not (simpl()) then Tprim(f,ts) else
      match f , ts with
	| I_opp , [ Tprim(I_opp,[a]) ] -> a
	| R_opp , [ Tprim(R_opp,[a]) ] -> a
	| I_add , [ Tint a ; Tint b ] -> i_compute My_bigint.add a b
	| I_sub , [ Tint a ; Tint b ] -> i_compute My_bigint.sub a b
	| I_mul , [ Tint a ; Tint b ] -> i_compute My_bigint.mul a b
	| I_add , [ Tint "0" ; x ] -> x
	| I_add , [ x ; Tint "0" ] -> x
	| I_sub , [ Tint "0" ; x ] -> e_prim I_opp [x]
	| I_sub , [ x ; Tint "0" ] -> x
	| I_opp , [ Tint a ] -> i_compute My_bigint.sub "0" a
	| I_mul , [ Tint "1" ; x ] -> x
	| I_mul , [ x ; Tint "1" ] -> x
	| I_mul , [ Tint "0" ; _ ] -> e_zero
	| I_mul , [ _ ; Tint "0" ] -> e_zero
	| I_add , [ b ; Tprim(I_sub,[a;c]) ] when equal b c -> a ;
	| I_add , [ Tprim(I_sub,[a;b]) ; c ] when equal b c -> a
	| I_sub , [ Tprim(I_add,[a;b]) ; c ] when equal b c -> a
	| L_eq ,  [ Tint a ; Tint b ] -> i_compare (fun r -> r=0)  a b
	| L_neq , [ Tint a ; Tint b ] -> i_compare (fun r -> r<>0) a b
	| I_lt ,  [ Tint a ; Tint b ] -> i_compare (fun r -> r<0)  a b
	| L_eq  , [a;b] when equal a b -> Ttrue
	| L_neq , [a;b] when equal a b -> Tfalse
	| I_leq ,  [ Tint a ; Tint b ] -> i_compare (fun r -> r<=0) a b
	| I_of_R , [ Tprim(R_of_I,[a]) ] -> a
	| B_not , [p] -> e_not p
	| B_and , [a;b] -> e_and a b
	| B_or  , [a;b] -> e_or a b
	| _ -> Tprim(f,ts)

  let e_call f ts = Tcall(f,ts)

  let e_cond c a b =
    match c with
      | Ttrue -> a
      | Tfalse -> b
      | Tprim(B_not,[p]) -> Tif(p,b,a)
      | _ -> Tif(c,a,b)

  let rec e_getfield r f =
    match r with
      | Tsetfield(r0,g,v) when simpl () -> if LogicId.equal f.f_name g.f_name then v else e_getfield r0 f
      | _ -> Tgetfield(r,f)

  let e_setfield r f v =
    match r with
      | Tsetfield(r0,g,_) when simpl () && LogicId.equal f.f_name g.f_name -> Tsetfield(r0,f,v)
      | _ -> Tsetfield(r,f,v)

  let rec e_access r k =
    match r with
      | Tupdate(_,k0,v0) when simpl () && equal k k0 -> v0
      | Tupdate(r0,k0,_) when simpl () && different k k0 -> e_access r0 k
      | _ -> Taccess(r,k)

  let e_update r k v =
    match r with
      | Tupdate(r0,k0,_) when simpl () && equal k k0 -> Tupdate(r0,k,v)
      | _ -> Tupdate(r,k,v)

  let rec e_hasvar xs = function
    | Tvar x -> List.exists (VAR.equal x) xs
    | Tcall(_,ts) | Tprim(_,ts) -> List.exists (e_hasvar xs) ts
    | Ttrue | Tfalse | Tint _ | Treal _ -> false
    | Tif(a,b,c) | Tupdate(a,b,c) -> e_hasvar xs a || e_hasvar xs b || e_hasvar xs c
    | Taccess(a,b) | Tsetfield(a,_,b) -> e_hasvar xs a || e_hasvar xs b
    | Tgetfield(a,_) -> e_hasvar xs a
    | Tlet(x,a,b) ->
	e_hasvar xs a ||
	  let ys = List.filter (VAR.different x) xs in
	  ys <> [] && e_hasvar ys b
	    
end

module PRED =
struct

  open TERM

  type relation =
    | L_eq | L_neq
    | I_lt | I_leq
    | R_lt | R_leq
    | B_true | B_false

  type t =
    | Ptrue
    | Pfalse
    | Prel of relation * TERM.t list
    | Pcall of id * TERM.t list
    | Pimplies of t * t
    | Pand of t * t
    | Por of t * t
    | Piff of t * t
    | Pnot of t
    | Pnamed of id * t
    | Pcond of TERM.t * t * t
    | Plet of VAR.t * TERM.t * t
    | Pforall of VAR.t * t
    | Pexists of VAR.t * t

  let iter fp ft = function
    | Ptrue | Pfalse -> ()
    | Prel(_,ts) | Pcall(_,ts) -> List.iter ft ts
    | Pimplies(a,b) | Pand(a,b) | Por(a,b) | Piff(a,b) -> fp a ; fp b
    | Pcond(t,a,b) -> ft t ; fp a ; fp b
    | Plet(_,t,a) -> ft t ; fp a
    | Pnot a | Pnamed(_,a) | Pforall(_,a) | Pexists(_,a) -> fp a

  let rec depend ids = function
    | Ptrue | Pfalse -> ids
    | Prel(_,ts) -> List.fold_left TERM.depend ids ts
    | Pcall(id,ts) -> List.fold_left TERM.depend (Iset.add id ids) ts
    | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) ->
	depend (depend ids p) q
    | Pnot p | Pnamed(_,p) | Pforall(_,p) | Pexists(_,p) -> 
	depend ids p (* names are not declared *)
    | Pcond(t,p,q) -> 
	depend (depend (TERM.depend ids t) p) q
    | Plet(_,a,p) ->
	depend (TERM.depend ids a) p

  let i_compare f x y = 
    let r = My_bigint.compare (My_bigint.of_string x) (My_bigint.of_string y) in
    if f r then Ptrue else Pfalse

  let p_call p ts = Pcall(p,ts)

  let rec val_of = function Pnamed(_,p) -> val_of p | p -> p
  let rec cut pnew = function Pnamed(label,p) -> Pnamed(label,cut pnew p) | _ -> pnew
    
  let rec p_not p = match val_of p with
    | Ptrue -> cut Pfalse p
    | Pfalse -> cut Ptrue p
    | Pnot p -> p
    | Pand(p,q) -> Por(p_not p,p_not q)
    | Por(p,q) -> Pand(p_not p,p_not q)
    | Pimplies(p,q) -> Pand(p,p_not q)
    | Prel( L_eq  , [a;b] ) -> Prel( L_neq , [a;b] )
    | Prel( L_neq , [a;b] ) -> Prel( L_eq ,  [a;b] )
    | Prel( I_lt  , [a;b] ) -> Prel( I_leq ,  [b;a] )
    | Prel( I_leq  , [a;b] ) -> Prel( I_lt ,  [b;a] )
    | Prel( R_lt  , [a;b] ) -> Prel( R_leq ,  [b;a] )
    | Prel( R_leq  , [a;b] ) -> Prel( R_lt ,  [b;a] )
    | Prel( B_true , [a] ) -> Prel( B_false , [a] )
    | Prel( B_false , [a] ) -> Prel( B_true , [a] )
    | Pforall( x , p ) -> Pexists( x , p_not p )
    | Pexists( x , p ) -> Pforall( x , p_not p )
    | _ -> Pnot p
	
  let p_and p1 p2 = match val_of p1, val_of p2 with
    | Ptrue, _ -> p2
    | _, Ptrue  -> p1
    | Pfalse,_-> cut Pfalse p1
    | _,Pfalse -> cut Pfalse p2
    | _ -> Pand (p1, p2)
	
  let p_or p1 p2 = match val_of p1, val_of p2 with
    | Ptrue, _ -> cut Ptrue p1
    | _ , Ptrue -> cut Ptrue p2
    | Pfalse ,_ -> p2
    | _ ,Pfalse -> p1
    | _ -> Por (p1,p2)
	
  let p_xor p1 p2 = match val_of p1, val_of p2 with
    | Ptrue , Ptrue -> cut (cut Pfalse p2) p1
    | Ptrue ,_ -> cut Ptrue p1
    | _,Ptrue -> cut Ptrue p2
    | Pfalse , _ -> p2
    | _ , Pfalse -> p1
    | _ -> Pnot(Piff(p1,p2))
	
  let p_implies p1 p2 =
    match val_of p1, val_of p2 with
      | Ptrue, _ -> p2
      | Pfalse, _ -> cut Ptrue p1
      | _, Ptrue -> cut Ptrue p2
      | _ -> Pimplies (p1, p2)
	
  let p_cond c p1 p2 = match c, val_of p1, val_of p2 with
    | _, Ptrue, Ptrue -> cut (cut Ptrue p2) p1
    | _, Pfalse, Pfalse -> cut (cut Pfalse p2) p1
    | Ttrue , _ , _ -> p1
    | Tfalse , _ , _ -> p2
    | Tprim(B_not,[t]) , _ , _ -> Pcond(t,p2,p1)
    | _ -> Pcond(c,p1,p2)
	
  let p_iff p1 p2 =
    match val_of p1,val_of p2 with
      | Ptrue ,_ -> p2
      | _ ,Ptrue -> p1
      | Pfalse, _ -> p_not p2
      | _ , Pfalse -> p_not p1
      | _ -> Piff (p1,p2)

  let rec p_bool = function
    | Ttrue -> Ptrue
    | Tfalse -> Pfalse
    | Tprim(TERM.L_eq ,[a;b]) -> Prel(L_eq ,[a;b])
    | Tprim(TERM.L_neq,[a;b]) -> Prel(L_neq,[a;b])
    | Tprim(TERM.I_lt ,[a;b]) -> Prel(I_lt ,[a;b])
    | Tprim(TERM.I_leq ,[a;b]) -> Prel(I_leq ,[a;b])
    | Tprim(TERM.R_lt ,[a;b]) -> Prel(R_lt ,[a;b])
    | Tprim(TERM.R_leq ,[a;b]) -> Prel(R_leq ,[a;b])
    | Tprim(B_and,[a;b]) -> p_and (p_bool a) (p_bool b)
    | Tprim(B_or,[a;b]) -> p_or (p_bool a) (p_bool b)
    | Tprim(B_not,[a]) -> p_not (p_bool a)
    | p -> Prel(B_true,[p])

  let p_prim f ts = 
    if not (simpl()) then Prel(f,ts) else
      match f , ts with
	| L_eq  , [a;b] when TERM.equal a b -> Ptrue
	| L_neq , [a;b] when TERM.equal a b -> Pfalse
	| L_eq  , [Tint x;Tint y] -> i_compare (fun r -> r=0)  x y
	| L_neq , [Tint x;Tint y] -> i_compare (fun r -> r<>0) x y
	| I_lt  , [Tint x;Tint y] -> i_compare (fun r -> r<0)  x y
	| I_leq  , [Tint x;Tint y] -> i_compare (fun r -> r<=0) x y
	| B_true , [t] -> p_bool t
	| B_false , [t] -> p_not (p_bool t)
	| _ -> Prel(f,ts)

  let rec p_hasvar xs = function
    | Ptrue | Pfalse -> false
    | Pcall(_,ts) | Prel(_,ts) -> List.exists (e_hasvar xs) ts
    | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) -> p_hasvar xs p || p_hasvar xs q
    | Pnamed(_,p) | Pnot p -> p_hasvar xs p
    | Pcond(a,p,q) -> e_hasvar xs a || p_hasvar xs p || p_hasvar xs q
    | Pforall(x,p) | Pexists(x,p) -> 
	let ys = List.filter (VAR.different x) xs in
	ys <> [] && p_hasvar ys p
    | Plet(x,a,p) ->
	e_hasvar xs a ||
	  let ys = List.filter (VAR.different x) xs in
	  ys <> [] && p_hasvar ys p

  let p_forall x p = if p_hasvar [x] p then Pforall(x,p) else p
  let p_exists x p = if p_hasvar [x] p then Pexists(x,p) else p
  let p_let x a p = if p_hasvar [x] p then Plet(x,a,p) else p

end

(* -------------------------------------------------------------------------- *)
(* --- Substitution & Bindings                                            --- *)
(* -------------------------------------------------------------------------- *)

module SUBST =
struct

  open TERM
  open PRED

  let is_atomic = function 
    | Tint _ | Treal _ | Ttrue | Tfalse | Tcall(_,[]) | Tvar _ -> true
    | _ -> false

  let rec e_bind ?pool x e t = 
    let frec = e_bind ?pool x e in
    match t with
      | Tvar x0 -> if VAR.equal x0 x then e else t
      | Ttrue | Tfalse | Tint _ | Treal _ -> t
      | Tcall(f,ts) -> Tcall(f,List.map frec ts)
      | Tprim(p,ts) -> e_prim p (List.map frec ts)
      | Tif(a,b,c) -> e_cond (frec a) (frec b) (frec c)
      | Tgetfield(r,f) -> e_getfield (frec r) f
      | Tsetfield(r,f,v) -> e_setfield (frec r) f (frec v)
      | Taccess(r,k) -> e_access (frec r) (frec k)
      | Tupdate(r,k,v) -> e_update (frec r) (frec k) (frec v)
      | Tlet(x0,a,b) ->
	  begin
	    if e_hasvar [x0] e then
	      match pool with
		| None -> Tlet(x,e,t)
		| Some thepool ->
		    let y = VAR.freshen thepool x in
		    let b = e_bind ?pool x0 (Tvar y) b in
		    e_let ?pool y (frec a) (frec b)
	    else
	      let b' = if VAR.equal x0 x then b else frec b in
	      e_let ?pool x0 (frec a) b'
	  end

  and e_let ?pool x a b =
    if e_hasvar [x] b then
      if is_atomic a then e_bind ?pool x a b else Tlet(x,a,b)
    else b

  let rec p_bind ?pool x e p =
    let prec = p_bind ?pool x e in
    let erec = e_bind ?pool x e in
    match p with
      | Ptrue | Pfalse -> p
      | Prel(f,ts) -> p_prim f (List.map erec ts)
      | Pcall(p,ts) -> Pcall(p,List.map erec ts)
      | Pimplies(p,q) -> p_implies (prec p) (prec q)
      | Pcond(a,p,q) -> p_cond (erec a) (prec p) (prec q)
      | Pand(p,q) -> p_and (prec p) (prec q)
      | Por(p,q)  -> p_or (prec p) (prec q)
      | Piff(p,q) -> p_iff (prec p) (prec q)
      | Pnot(p)   -> p_not (prec p)
      | Pnamed(a,p) -> Pnamed(a,prec p)
      | Pforall(x0,p) as p0 ->
          begin
            if VAR.equal x x0 then p0
            else if e_hasvar [x0] e then
              match pool with
                | None -> Plet(x,e,p0)
                | Some thepool ->
		    let y = VAR.freshen thepool x in
                    let py = p_bind ?pool x0 (Tvar y) p in
                    PRED.p_forall y (prec py)
            else PRED.p_forall x0 (prec p)
          end
      | Pexists(x0,p) as p0 ->
          begin
            if VAR.equal x x0 then p0
            else if e_hasvar [x0] e then
                match pool with
                  | None -> Plet(x,e,p0)
                  | Some thepool ->
		      let y = VAR.freshen thepool x in
                      let py = p_bind ?pool x0 (Tvar y) p in
                      PRED.p_exists y (prec py)
            else PRED.p_exists x0 (prec p)
          end
      | Plet(x0,t,p) as p0 ->
          begin
            if e_hasvar [x0] e then
              match pool with
                | None -> Plet(x,e,p0)
                | Some thepool ->
		    let y = VAR.freshen thepool x in
                    let py = p_bind ?pool x0 (Tvar y) p in
                    p_let ?pool y (erec t) (prec py)
            else
              let p' = if VAR.equal x0 x then p else prec p in
	      p_let ?pool x0 (erec t) p'
          end

  and p_let ?pool x a p =
    if p_hasvar [x] p then
      if is_atomic a then p_bind ?pool x a p else Plet(x,a,p)
    else p

end

(* -------------------------------------------------------------------------- *)

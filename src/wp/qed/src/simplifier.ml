(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
(* --- Equality-based Simplifier                                          --- *)
(* -------------------------------------------------------------------------- *)

open Logic

type 'a simp =
  | Keep (** Keep the term as it is *)
  | Equal of 'a list (** The term is equal to these others *)
  | Rewrite of 'a (** The term should be replaced by this one *)
  | Condition of 'a * 'a simp * 'a simp (** The action is conditional *)

class type ['a] simplifier =
object
  method copy : 'a simplifier
  method have : 'a -> unit
  method merge : old:'a -> by:'a -> unit
  method simplify : 'a -> 'a simp
end

class timer tm =
object(self)
  val mutable time = 0
  val mutable timeout = tm
  val mutable demon = []

  method private tick =
    if time <= timeout then
      ( if timeout > 0 then time <- succ time ; true )
    else
      ( List.iter (fun d -> d timeout) demon ; false )

  method on_timeout f = demon <- demon @ [f]
  method set_timeout t = time <- 0 ; timeout <- max 0 t
  method timeout = timeout
  method loop = self#tick
  method break = not self#tick
  method check = if not self#tick then raise Exit
  method start = time <- 0
  method stop = time <- succ timeout
  method time = time
  method is_over = time > timeout
  method copy = 
    let t = new timer timeout in
    List.iter t#on_timeout demon ;
    (t :> timer)
      
end

module Make(T : Term) =
struct

  open T

  module P = Pretty.Make(T)
  let pretty = P.pp_term P.closed

  let debug = ref false

  exception Contradiction 

  (* -------------------------------------------------------------------------- *)
  (* --- Congruences                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  type ground = {
    thy : term simplifier list ;
    timer : timer ;
    mutable dag : term Tmap.t ; (* only bind lesser terms *)
    mutable grd : Tset.t ; 
    mutable queue : (term * term) list ;
  }

  let cc_pretty fmt eqs =
    Tmap.iter
      (fun t1 t2 ->
	 Format.fprintf fmt "@[%a ====> %a@]@." pretty t1 pretty t2
      ) eqs.dag

  let rec ground eqs a =
    try
      let b = Tmap.find a eqs.dag in (* b < a *)
      let c = ground eqs b in        (* c <= b < a *)
      if b != c then eqs.dag <- Tmap.add a c eqs.dag ; c
    with Not_found -> a

  let do_have s e =
    match T.repr e with
      | And hs -> List.iter s#have hs
      | _ -> s#have e

  let do_propagate ~old ~by s =
    match T.repr by with
      | True -> do_have s old
      | False -> do_have s (T.e_not old)
      | _ -> s#merge ~old ~by

  (* by << old *)
  let do_replace eqs ~old ~by =
    if !debug then 
      Format.eprintf "@[<hov2>Merge %a@ ====> %a@]@." 
	pretty old pretty by ;
    eqs.dag <- Tmap.add old by eqs.dag ;
    eqs.grd <- Tset.add by (Tset.remove old eqs.grd) ;
    List.iter (do_propagate ~old ~by) eqs.thy

  let do_merge eqs (a,b) =
    let a = ground eqs a in
    let b = ground eqs b in
    match T.are_equal a b with
      | No -> raise Contradiction
      | Yes -> ()
      | Maybe ->
	  if T.compare a b < 0 
	  then do_replace eqs ~old:b ~by:a (* a << b *)
	  else do_replace eqs ~old:a ~by:b (* b << a *)

  module UNFLAT =
  struct 

    let rec add_hyp eqs stk a =
      if not (Tset.mem a stk) then
	match T.repr a with
	  | True -> ()
	  | False -> raise Contradiction
	  | And hs -> List.iter (add_hyp eqs stk) hs
	  | Eq(a,b) -> add_equal eqs stk a b
	  | _ -> 
	      eqs.grd <- Tset.add a eqs.grd ;
	      eqs.queue <- (e_true,a) :: (e_false,e_not a) :: eqs.queue ;
	      if T.flattenable a then
		let stk = Tset.add a stk in
		List.iter (add_hyp eqs stk) (T.flatten a)
		  
    and add_equal eqs stk a b =
    if a != b then
      match T.repr a , T.repr b with
	| True , _ -> add_hyp eqs stk b
	| _ , True -> add_hyp eqs stk a
	| False , _ -> add_hyp eqs stk (e_not b)
	| _ , False -> add_hyp eqs stk (e_not a)
	| _ -> eqs.queue <- (a,b) :: eqs.queue

  end

  let add_hyp eqs a = UNFLAT.add_hyp eqs Tset.empty a
  let add_equal eqs a b = UNFLAT.add_equal eqs Tset.empty a b

  let rec do_simp eqs a = function
    | Keep -> a
    | Equal bs -> 
	List.iter 
	  (fun b -> add_hyp eqs (e_eq a (ground eqs b))) 
	  bs ; a
    | Rewrite b -> 
	let b = ground eqs b in
	if a != b then do_replace eqs ~old:a ~by:b ; b
    | Condition(p,s1,s2) ->
	let p = ground eqs p in
	match T.is_true p with
	  | Yes -> do_simp eqs a s1
	  | No -> do_simp eqs a s2
	  | Maybe -> eqs.grd <- Tset.add p eqs.grd ; a

  let rec do_simplifiers eqs a = function
    | [] -> a
    | s :: thy ->
	let a = do_simp eqs a (s#simplify a) in
	do_simplifiers eqs a thy

  let do_compute eqs a s =
    List.iter 
      (fun b -> add_hyp eqs (e_eq a (ground eqs b)))
      (s#compute a)

  type sigma = {
    bound : Vars.t ;
    mutable memo : term Tmap.t ;
  }
	    
  let rec rebuild eqs sigma e =
    if Vars.intersect sigma.bound (T.vars e) then e else
      try Tmap.find e sigma.memo
      with Not_found ->
	let e' = 
	  match T.repr e with
	    | Bind(q,x,u) ->
		let sigma = { 
		  bound = Vars.add x sigma.bound ;
		  memo = Tmap.empty ;
		} in
		e_bind q x (rebuild eqs sigma u)
	    | _ -> 
		let e' = ground eqs (T.e_map (rebuild eqs sigma) e) in
		do_simplifiers eqs e' eqs.thy
	in sigma.memo <- Tmap.add e e' sigma.memo ; e'

  let do_propagate eqs =
    let sigma = { bound = Vars.empty ; memo = Tmap.empty } in
    Tset.iter
      (fun e -> add_equal eqs e (rebuild eqs sigma e))
      eqs.grd

  let fixpoint eqs = 
    begin
      if !debug then Format.eprintf "------- Fixpoint --------@." ;
      eqs.timer#start ;
      while eqs.queue <> [] && eqs.timer#loop do
	let job = eqs.queue in
	eqs.queue <- [] ;
	List.iter (do_merge eqs) job ;
	do_propagate eqs ;
      done ;
      if !debug then Format.eprintf "------- Done --------@." ;
    end

  let copy eqs = {
    dag = eqs.dag ;
    grd = eqs.grd ;
    thy = List.map (fun s -> s#copy) eqs.thy ;
    queue = eqs.queue ;
    timer = eqs.timer#copy ;
  }

  let cc_make ?(timeout=0) theories = 
    {
      dag = Tmap.empty ;
      grd = Tset.empty ;
      thy = theories ;
      queue = [] ;
      timer = new timer timeout ;
    }

  let cc_assume eqs hs =
    try 
      let eqs = copy eqs in
      let sigma = { bound = Vars.empty ; memo = Tmap.empty } in
      List.iter (fun h -> add_hyp eqs (rebuild eqs sigma h)) hs ;
      fixpoint eqs ;
      Some eqs ;
    with Contradiction ->
      None

  let cc_simplify eqs e = 
    let eqs = copy eqs in
    eqs.grd <- Tset.add e eqs.grd ;
    let sigma = { bound = Vars.empty ; memo = Tmap.empty } in
    let e = rebuild eqs sigma e in
    fixpoint eqs ; ground eqs e

  (* -------------------------------------------------------------------------- *)
  (* --- Basic Simplifiers                                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let rewrites = function
    | [] -> Keep
    | [t] -> Rewrite t
    | ts -> Equal ts

  class cc_record =
  object(self)

    val mutable records = Tmap.empty
    method copy = (Oo.copy self :> term simplifier)

    method private records e = 
      try Tmap.find e records 
      with Not_found -> 
	match T.repr e with
	  | Rdef _ -> [e]
	  | _ -> []
      
    method merge ~old ~by =
      let r = self#records old @ self#records by in
      if r <> [] then records <- Tmap.add by r (Tmap.remove old records)

    method have (_:term) = ()

    method simplify e = match T.repr e with
      | Rget(r,f) ->
	  let rs = self#records r in
	  let fs = List.map (fun r -> T.e_getfield r f) rs in
	  rewrites fs
      | _ -> Keep

  end

  class cc_arrays =
  object(self)

    val mutable arrays = Tmap.empty
    method copy = (Oo.copy self :> term simplifier)

    method private arrays e = 
      try Tmap.find e arrays 
      with Not_found -> 
	match T.repr e with
	  | Aset _ -> [e]
	  | _ -> []
      
    method merge ~old ~by =
      let r = self#arrays old @ self#arrays by in
      if r <> [] then arrays <- Tmap.add by r (Tmap.remove old arrays)

    method have (_:term) = ()

    method simplify e = match T.repr e with
      | Aget(a,k) -> 
	  let rs = self#arrays a in
	  let xs = List.map (fun a -> T.e_get a k) rs in
	  rewrites xs
      | _ -> Keep

  end

end

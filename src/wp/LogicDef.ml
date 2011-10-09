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
(* --- Logic Database                                                     --- *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau
open LogicLang

type item =
  | TYPE of int
  | RECORD of field list
  | FUNCTION of var list * tau * term option
  | PREDICATE of var list * pred option
  | AXIOM of pred

type description = {
  t_source : Lexing.position ;
  t_short : string ;
  t_descr : string ;
}

type declaration = {
  d_name : id ;
  d_item : item ;
  d_descr : description ;
}

type registered = {
  r_declaration : declaration ;
  r_localdeps : Iset.t ;
  r_age : int ;
}

type logic_model = {
  model_name : string ;
  model_pointer : tau ;
  model_index : registered Ihmap.t ;
  mutable model_locked : Iset.t ; (* cofix compiled *)
  mutable model_history : int ; (* age of exportations *)
  mutable model_updated : bool ; (* modified at this age *)
}

module LogicModel : Datatype.S with type t = logic_model =
  Datatype.Make
    (struct
       include Datatype.Undefined
       type t = logic_model
       let name = "Wp.LogicDef.LogicModel"
       let reprs = [{
		      model_name="";
		      model_pointer = Integer;
		      model_index = Ihmap.create 0;
		      model_locked = Iset.empty ;
		      model_history = 0;
		      model_updated = false;
		    }]
     end)

module MODELS = State_builder.Hashtbl(Datatype.String.Hashtbl)(LogicModel)
  (struct
     let name = "Wp.LogicDef.Declarations"
     let dependencies = [Ast.self]
     let kind = `Tuning (* TODO[LC]: to check with JS. *)
     let size = 7
   end)

let register ~name ~pointer =
  if MODELS.mem name then Wp_parameters.fatal "Duplicate logic model {%s}" name ;
  MODELS.add name {
    model_name = name ;
    model_pointer = pointer ;
    model_index = Ihmap.create 231 ;
    model_locked = Iset.empty ;
    model_history = 0 ;
    model_updated = false ;
  }

let current_model = ref None

let on_model model job data =
  match !current_model with
    | Some m -> 
	Wp_parameters.fatal 
	  "Re-entrant logic model {%s,%s}" m.model_name model
    | None ->
	current_model := Some (MODELS.find model) ;
	try let result = job data in current_model := None ; result
	with exn -> current_model := None ; raise exn
	  
let the_model () = 
  match !current_model with Some m -> m | None -> 
    Wp_parameters.fatal "No logic model"

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let lookup id =
  let model = the_model () in
  Ihmap.find model.model_index id

let get_item id = (lookup id).r_declaration.d_item
let get_description id = (lookup id).r_declaration.d_descr
let get_declaration id = (lookup id).r_declaration
let get_local_depends id = 
  try (lookup id).r_localdeps with Not_found -> Iset.empty

let add_depend_var ids x = add_depend_tau ids (tau_of_var x)
let add_depend_field ids f = add_depend_tau ids f.f_type

let dependencies = function
  | TYPE _ -> Iset.empty
  | RECORD fs -> List.fold_left add_depend_field Iset.empty fs
  | FUNCTION (xs,t,None) ->
      List.fold_left add_depend_var (add_depend_tau Iset.empty t) xs
  | FUNCTION (xs,t,Some e) ->
      let core = add_depend_term Iset.empty e in
      List.fold_left add_depend_var (add_depend_tau core t) xs
  | PREDICATE(xs,None) ->
      List.fold_left add_depend_var Iset.empty xs
  | PREDICATE(xs,Some p) ->
      List.fold_left add_depend_var (add_depend_pred Iset.empty p) xs
  | AXIOM p -> add_depend_pred Iset.empty p

let declare d =
  let m = the_model () in
  if Iset.mem d.d_name m.model_locked then
    Wp_parameters.fatal "Locked symbol '%a'" LogicId.pretty d.d_name ;
  m.model_updated <- true ;
  Ihmap.replace m.model_index d.d_name {
    r_declaration = d ;
    r_age = m.model_history ;
    r_localdeps = dependencies d.d_item ;
  }

let lock f =
  let m = the_model () in
  m.model_locked <- Iset.add f m.model_locked

let unlock f =
  let m = the_model () in
  m.model_locked <- Iset.remove f m.model_locked

let mark_history () =
  let m = the_model () in
  if m.model_updated then 
    ( m.model_history <- succ m.model_history ; m.model_updated <- false )
    
let model_age () = (the_model ()).model_history

(* -------------------------------------------------------------------------- *)
(* --- Fixpoint Compilation                                               --- *)
(* -------------------------------------------------------------------------- *)

module Cofix =
struct

  let stack : id list ref = ref []
  let push f = stack := f :: !stack
  let pop f = 
    match !stack with
      | [] -> Wp_parameters.fatal "Logic.cofix: empty stack"
      | f0::stk ->
	  if LogicId.equal f f0 then stack := stk
	  else Wp_parameters.fatal "Logic.cofix: corrupted stack"

  let recursive f =
    List.exists (LogicId.equal f) !stack

  type state =
    | Undefined
    | Defined of item
    | Cyclic of cycle

  and cycle = {
    mutable ccitem : ccitem ;
    mutable stable : bool ; (* stable or not during fixpoint *)
    mutable inners : Iset.t ; (* set of symbols in the cycle, except root *)
  } and ccitem =
    | Cnone
    | Cdefault of item
    | Cupdated of item * description

  let cofix : cycle Ihmap.t = Ihmap.create 31 (* Cycle state only *)

  let lookup f =
    try Defined(get_item f)
    with Not_found ->
      try Cyclic(Ihmap.find cofix f)
      with Not_found -> Undefined

  let is_stable f = match lookup f with
    | Undefined | Defined _ 
    | Cyclic { ccitem=Cupdated _ ; stable=true } -> true
    | Cyclic _ -> false

  let current = function
    | Cnone ->
	Wp_parameters.fatal "logic:undefined value"
    | Cdefault item | Cupdated(item,_) -> item 

  let define f =
    try
      let c = Ihmap.find cofix f in
      Ihmap.remove cofix f ;
      match c.ccitem with
	| Cnone | Cdefault _ ->
	    Wp_parameters.fatal "unstable definition (%a)" LogicId.pretty f ;
	| Cupdated (item,descr) -> 
	    unlock f ;
	    declare { d_name=f ; d_item=item ; d_descr=descr }
    with Not_found -> ()

  let is_stable f =
    match lookup f with
      | Undefined -> false
      | Defined _ -> true
      | Cyclic c -> c.stable

  exception Unstable

  let all_stable fs =
    try Iset.iter 
      (fun f -> 
	 if not (is_stable f) then raise Unstable
      ) fs ; true
    with Unstable -> false

  let set_stable f =
    try let c = Ihmap.find cofix f in c.stable <- true
    with Not_found -> ()

  let rec get_cycle f = function
    | [] -> []
    | g::stk -> if LogicId.equal f g then [] else (g :: get_cycle f stk)

  let add_cycle fs g =
    try
      let c = Ihmap.find cofix g in
      c.inners <- List.fold_right Iset.add fs c.inners
    with Not_found -> ()

  let rec compatible_signature xs ys =
    match xs , ys with
      | [] , [] -> true
      | [] , _ | _ , [] -> false
      | x::xs , y::ys ->
	  (compare_tau (tau_of_var x) (tau_of_var y) = 0) &&
	    compatible_signature xs ys

  let compatible item0 item1 =
    match item0,item1 with
      | FUNCTION(_,_,Some _) , FUNCTION(_,_,None) -> false
      | PREDICATE(_,Some _) , PREDICATE(_,None) -> false
      | FUNCTION(sig0,r0,_) , FUNCTION(sig1,r1,_) ->
	  (compatible_signature sig0 sig1) &&
	    (compare_tau r0 r1 = 0)
      | PREDICATE(sig0,_) , PREDICATE(sig1,_) ->
	  (compatible_signature sig0 sig1)
      | TYPE n , TYPE n' -> n=n'
      | RECORD fs , RECORD fs' -> 
	  (List.length fs = List.length fs') &&
	    List.for_all2 (fun f f' -> compare_field f f'=0) fs fs'
      | AXIOM _ , AXIOM _ -> true
      | _ -> false

  let default f item =
    try
      let c = Ihmap.find cofix f in
      if c.ccitem=Cnone then c.ccitem <- Cdefault item
    with Not_found ->
      Ihmap.add cofix f {
	ccitem = Cdefault item ;
	stable = true ;
	inners = Iset.empty ;
      }

  let update f item descr =
    match lookup f with
      | Undefined | Defined _ -> 
	  declare { d_name=f ; d_item=item ; d_descr=descr }
      | Cyclic c ->
	  if c.stable then
	    begin
	      match c.ccitem with
		| Cnone -> ()
		| Cdefault item0 | Cupdated(item0,_) ->
		    c.stable <- compatible item0 item
	    end ;
	  c.ccitem <- Cupdated(item,descr)

  let compute f cc =
    try push f ; cc f ; pop f ;
    with error -> pop f ; raise error

  let rec fixpoint c f cc =
    compute f cc ;
    if Iset.mem f c.inners then
      (* inside cycle *)
      ( current c.ccitem )
    else
      (* cycle root *)
      if c.stable && all_stable c.inners then 
	begin
	  define f ; 
	  Iset.iter define c.inners ;
	  current c.ccitem
	end
      else
	begin
	  c.stable <- true ; 
	  Iset.iter set_stable c.inners ; 
	  fixpoint c f cc
	end
	  
  let obtain f cc =
    match lookup f with
      | Defined item -> item
      | Undefined ->
	  let c = { ccitem=Cnone ; stable=true ; inners=Iset.empty } in
	  Ihmap.replace cofix f c ;
	  lock f ; fixpoint c f cc
      | Cyclic c ->
	  if recursive f then
	    let fs = get_cycle f !stack in
	    List.iter (add_cycle fs) (f::fs) ;
	    ( current c.ccitem )
	  else
	    ( fixpoint c f cc )

end

let fixpoint = Cofix.obtain
let default = Cofix.default
let update = Cofix.update

(* -------------------------------------------------------------------------- *)
(* --- Exportation                                                        --- *)
(* -------------------------------------------------------------------------- *)


module Components =
struct

  (*[LC] From ocamlgraph/components with added feature *)

  module G = 
  struct
    (* here, G.t = unit and G.V=LogicId *)
    let iter_succ f (*g*) id = Iset.iter f (get_local_depends id)
  end
  module H = Ihmap
  module S = Iset

  (*[LC] Added an iterator for some 'roots' There is no need for
    G.iter_vertex.  Only an iterator over the requested nodes is
    necessary.  The compute hashcomp table is enough to build the
    array of components. *)

  let scc_roots (*g*) iter_roots =
    let root = H.create 997 in
    let hashcomp = H.create 997 in
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    let rec pop x c = function
      | (y, w) :: l when y > x -> 
	  H.add hashcomp w !numcomp; 
	  pop x (S.add w c) l
      | l -> c,l
    in
    let rec visit v = 
      if not (H.mem root v) then
	begin
	  let n = incr numdfs; !numdfs in
	  H.add root v n; 
	  G.iter_succ 
	    (fun w -> 
	       visit w;
	       if not (H.mem hashcomp w) then 
		 H.replace root v (min (H.find root v) (H.find root w)))
	    (*g*) v;
	  if H.find root v = n then 
	    (H.add hashcomp v !numcomp;
	     let _,s = pop n (S.add v S.empty) !stack in 
	     stack:= s;
	     incr numcomp)
	  else stack := (n,v)::!stack;
	end
    in 
    iter_roots visit (*g*) ;
    let t = Array.make !numcomp [] in
    H.iter
      (fun v i -> t.(i) <- v::t.(i))
      hashcomp ; t

end

let declarations ids =
  let section = function
    | TYPE _ | RECORD _ -> 0
    | FUNCTION _ -> 1
    | PREDICATE _ -> 2
    | AXIOM _ -> 3
  in
  let sort d1 d2 = section d1.d_item - section d2.d_item in
  let acc = ref [] in
  List.iter
    (fun f ->
       try acc := get_declaration f :: !acc
       with Not_found -> ())
    ids ;
  List.sort sort !acc

let export f roots_iter =
  Array.iter 
    (fun ids -> f (declarations ids))
    (Components.scc_roots roots_iter)

let export_items f ids = 
  export f (fun visit -> List.iter visit ids)

let export_goal f p = 
  let ids = add_depend_pred Iset.empty p in
  export f (fun visit -> Iset.iter visit ids)

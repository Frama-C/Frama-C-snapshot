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
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes
open Qed
open Qed.Logic

(* -------------------------------------------------------------------------- *)

let basename def name =
  let rec lookup def s k n =
    if k < n then 
      let c = s.[k] in
      if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') 
      then String.sub s k 1
      else lookup def s (succ k) n
    else def
  in lookup def name 0 (String.length name)

(* -------------------------------------------------------------------------- *)
(* Naming Prefixes                                                           
   Names starting with a lower-case character belong to logic language
   or external model(s).

  'pointer' Pointer type
  'S_<s>' Structure <s>
  'U_<u>' Union <u>
  'F_<c>_<f>' Field <f> in compound <c>
  'A_<t>' ACSL Logic type <t> 
  'C_<c>' ACSL Constructor <c>
  'P_<p>' ACSL Predicate <p> (see LogicUsage.get_name)
  'L_<f>' ACSL Logic function <f> (see LogicUsage.get_name)
  'Q_<l>' ACSL Lemma or Axiom
  'S_<n>' Set comprehension predicate
  'Is<phi>' Typing predicate for type <phi>
  'Null<phi>' Null value for type <phi>
*)
let avoid_leading_backlash s = 
  if s.[0]='\\' then 
    let s = String.copy s in
    s.[0]<-'_';
    s
  else s

let comp_id c = 
  if c.cstruct 
  then Printf.sprintf "S_%s" c.cname
  else Printf.sprintf "U_%s" c.cname

let field_id f =
  Printf.sprintf "F_%s_%s" f.fcomp.cname f.fname

let type_id l =
  Printf.sprintf "A_%s" l.lt_name

let logic_id f =
  let name = avoid_leading_backlash (LogicUsage.get_name f) in
  if f.l_type = None 
  then Printf.sprintf "P_%s" name
  else Printf.sprintf "L_%s" name

let ctor_id c = Printf.sprintf "C_%s" (avoid_leading_backlash c.ctor_name)
let lemma_id l = 
  Printf.sprintf "Q_%s" (avoid_leading_backlash l)

(* -------------------------------------------------------------------------- *)

type theory = string

type adt = 
  | Mtype of mdt (* Model type *)
  | Mrecord of mdt * fields (* Model record-type *)
  | Atype of logic_type_info (* Logic Type *)
  | Comp of compinfo (* C-code struct or union *)
and mdt = {
  mdt_link : string ;
  mdt_theory : theory ;
}
and fields = { mutable fields : field list }
and field =
  | Mfield of mdt * fields * string * tau
  | Cfield of fieldinfo
and tau = (field,adt) Logic.datatype

let pointer = Context.create "Lang.pointer"

(* -------------------------------------------------------------------------- *)
(* --- Sorting & Typing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let sort_of_object = function
  | C_int _ -> Logic.Sint
  | C_float _ -> Logic.Sreal
  | C_pointer _ | C_comp _ | C_array _ -> Logic.Sdata

let sort_of_ctype t = sort_of_object (Ctypes.object_of t)

let sort_of_ltype t = match Logic_utils.unroll_type t with
  | Ctype typ -> sort_of_ctype typ
  | Ltype _ | Lvar _ | Larrow _ -> Logic.Sdata
  | Linteger -> Logic.Sint
  | Lreal -> Logic.Sreal

let tau_of_comp c = Logic.Data(Comp c,[])

let array a = Logic.Array(Logic.Int,a)
let farray a b = Logic.Array(a,b)

let rec tau_of_object = function
  | C_int _ -> Logic.Int
  | C_float _ -> Logic.Real
  | C_pointer t -> Context.get pointer t
  | C_comp c -> tau_of_comp c
  | C_array { arr_element = typ } -> array (tau_of_ctype typ)

and tau_of_ctype typ = tau_of_object (Ctypes.object_of typ)

let poly = Context.create "Wp.Lang.poly"

let rec varpoly k x = function
  | [] -> Warning.error "Unbound type parameter <%s>" x
  | y::ys -> if x = y then k else varpoly (succ k) x ys

let builtins = Hashtbl.create 131

let rec tau_of_ltype t = match Logic_utils.unroll_type t with
  | Linteger -> Logic.Int
  | Lreal -> Logic.Real
  | Ctype typ -> tau_of_ctype typ
  | Lvar x -> Logic.Tvar (varpoly 1 x (Context.get poly))
  | Larrow _ ->
      Warning.error "array type non-supported(%a)" 
	!Ast_printer.d_logic_type t
  | Ltype _ as b when Logic_const.is_boolean_type b -> Logic.Bool
  | Ltype(lt,ps) -> 
      try
	let mdt = Hashtbl.find builtins lt.lt_name in
	assert (ps = []) ;
	Logic.Data(Mtype mdt,[])
      with Not_found ->
	Logic.Data(Atype lt,List.map tau_of_ltype ps)

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)
  
module ADT =
struct

  type t = adt

  let basename = function
    | Mtype a -> basename "M" a.mdt_link
    | Mrecord(r,_) -> basename "R" r.mdt_link
    | Comp c -> basename (if c.cstruct then "S" else "U") c.corig_name
    | Atype lt -> basename "A" lt.lt_name

  let id = function
    | Mtype a -> a.mdt_link
    | Mrecord(a,_) -> a.mdt_link
    | Comp c -> comp_id c
    | Atype lt -> type_id lt
  let hash = function
    | Mtype a | Mrecord(a,_) -> Hashtbl.hash a
    | Comp c -> Compinfo.hash c
    | Atype lt -> Logic_type_info.hash lt

  let compare a b =
    if a==b then 0 else
      match a,b with
	| Mtype a , Mtype b -> String.compare a.mdt_link b.mdt_link
	| Mtype _ , _ -> (-1)
	| _ , Mtype _ -> 1
	| Mrecord(a,_) , Mrecord(b,_) -> String.compare a.mdt_link b.mdt_link
	| Mrecord _ , _ -> (-1)
	| _ , Mrecord _ -> 1
	| Comp a , Comp b -> Compinfo.compare a b
	| Comp _ , _ -> (-1)
	| _ , Comp _ -> 1
	| Atype a , Atype b -> Logic_type_info.compare a b

  let equal a b = (compare a b = 0)

  let pretty fmt a = Format.pp_print_string fmt (id a)

end

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)

let atype t = 
  try Mtype(Hashtbl.find builtins t.lt_name)
  with Not_found -> Atype t

let builtin ~name ~link ~theory =
  let m = { mdt_link = link ; mdt_theory = theory } in
  Hashtbl.add builtins name m

let datatype ~link ~theory =
  let m = { mdt_link = link ; mdt_theory = theory } in
  Mtype m

let record ~link ~theory fts =
  let m = { mdt_link = link ; mdt_theory = theory } in
  let r = { fields = [] } in
  let fs = List.map (fun (f,t) -> Mfield(m,r,f,t)) fts in
  r.fields <- fs ; Mrecord(m,r)

let field t f =
  match t with
    | Mrecord(_,r) ->
	begin
	  try List.find (function Mfield(_,_,g,_) -> f = g | _ -> false) r.fields
	  with Not_found -> Wp_parameters.fatal "No field <%s> in record" f
	end
    | _ -> Wp_parameters.fatal "No field <%s> in type '%a'" f ADT.pretty t

let comp c = Comp c

let fields_of_tau = function
  | Record _ -> assert false
  | Data(Mrecord(_,r),_) -> r.fields
  | Data(Comp c,_) -> List.map (fun f -> Cfield f) c.cfields
  | _ -> []

let fields_of_field = function
  | Mfield(_,r,_,_) -> r.fields
  | Cfield f -> List.map (fun f -> Cfield f) f.fcomp.cfields

let tau_of_field = function
  | Mfield(_,_,_,t) -> t
  | Cfield f -> tau_of_ctype f.ftype

module Field =
struct

  type t = field

  let id = function
    | Mfield(_,_,f,_) -> f
    | Cfield f -> field_id f

  let hash = function
    | Mfield(_,_,f,_) -> Hashtbl.hash f
    | Cfield f -> Fieldinfo.hash f

  let compare f g = 
    if f==g then 0 else
      match f , g with
	| Mfield(_,_,f,_) , Mfield(_,_,g,_) -> String.compare f g
	| Mfield _ , Cfield _ -> (-1)
	| Cfield _ , Mfield _ -> 1
	| Cfield f , Cfield g -> Fieldinfo.compare f g
	    
  let equal f g = (compare f g = 0)

  let pretty fmt f = Format.pp_print_string fmt (id f)

  let sort = function
    | Mfield(_,_,_,s) -> Qed.Kind.of_tau s
    | Cfield f -> sort_of_object (Ctypes.object_of f.ftype)

end

(* -------------------------------------------------------------------------- *)
(* --- Functions & Predicates                                             --- *)
(* -------------------------------------------------------------------------- *)

type scope = External of string | Generated
type lfun =
  | Function of scope * Engine.link * lfun category * sort 
  | Predicate of scope * string * string (* prop / bool *)
  | ACSL of logic_info
  | CTOR of logic_ctor_info

let is_associative = function
  | Logic.Operator op -> op.associative
  | _ -> false

let symbolf ~link ?(category=Logic.Function) ?(sort=Logic.Sdata) name =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       let name = Buffer.contents buffer in
       let flink = 
	 if is_associative category 
	 then Engine.F_call2 name 
	 else Engine.F_call name 
       in
       Function(link,flink,category,sort)
    ) (Format.formatter_of_buffer buffer) name

let extern_s ~theory ?category ?sort name = 
  symbolf ~link:(External theory) ?category ?sort "%s" name
let extern_f ~theory ?category ?sort name = 
  symbolf ~link:(External theory) ?category ?sort name
let extern_p ~theory ~prop ~bool = 
  Predicate(External theory,prop,bool)
let extern_fp ~theory phi = 
  Function(External theory,Engine.F_call phi,Logic.Function,Logic.Sprop)
let generated_f ?category ?sort name = 
  symbolf ~link:Generated ?category ?sort name
let generated_p name = 
  Function(Generated,Engine.F_call name,Logic.Function,Logic.Sprop)

module Fun =
struct

  type t = lfun

  let id = function
    | Function(_,f,_,_) -> Export.link_name f
    | Predicate(_,f,_) -> f
    | ACSL f -> logic_id f
    | CTOR c -> ctor_id c

  let link cmode = function
    | Function(_,f,_,_) -> f
    | ACSL f -> Engine.F_call (logic_id f)
    | CTOR c -> Engine.F_call (ctor_id c)
    | Predicate(_,p,b) -> 
	Engine.F_call (match cmode with Engine.Cprop -> p | Engine.Cterm -> b)

  let hash = function
    | Function(_,f,_,_) -> Hashtbl.hash f
    | Predicate(_,f,_) -> Hashtbl.hash f
    | ACSL f -> Logic_info.hash f
    | CTOR c -> Logic_ctor_info.hash c

  let compare f g = 
    if f==g then 0 else
      match f , g with
	| Function(_,f,_,_) , Function(_,g,_,_) -> Export.link_compare f g
	| Function _ , _ -> (-1)
	| _ , Function _ -> 1
	| Predicate(_,f,_) , Predicate(_,g,_) -> String.compare f g
	| Predicate _ , _ -> (-1)
	| _ , Predicate _ -> 1
	| ACSL f , ACSL g -> Logic_info.compare f g
	| ACSL _ , _ -> (-1)
	| _ , ACSL _ -> 1
	| CTOR c , CTOR d -> Logic_ctor_info.compare c d
	    
  let equal f g = (compare f g = 0)

  let pretty fmt f = Format.pp_print_string fmt (id f)

  let category = function
    | Function(_,_,c,_) -> c
    | Predicate _ | ACSL _ -> Logic.Function
    | CTOR _ -> Logic.Constructor

  let sort = function
    | Function(_,_,_,s) -> s
    | Predicate _ | ACSL { l_type=None } -> Logic.Sprop
    | ACSL { l_type=Some t } -> sort_of_ltype t
    | CTOR _ -> Logic.Sdata

end

let link = Fun.link

(* -------------------------------------------------------------------------- *)
(* --- Terms                                                              --- *)
(* -------------------------------------------------------------------------- *)

module F = 
struct

  module T = Qed.Term.Make(ADT)(Field)(Fun)
  module Pretty = Qed.Pretty.Make(T)
  include T

  (* -------------------------------------------------------------------------- *)
  (* --- Term Extensions                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  type unop = term -> term
  type binop = term -> term -> term

  let e_zero = e_zint Z.zero
  let e_one = e_zint Z.one
  let e_minus_one = e_zint Z.minus_one

  let e_int64 z = e_zint (Z.of_string (Int64.to_string z))
  let e_fact k e = e_times (Z.of_string (Int64.to_string k)) e
  let e_bigint z = e_zint (Z.of_string (My_bigint.to_string z))
  let e_float f = e_real (R.of_string (string_of_float f))
  let e_range a b = e_sum [b;e_one;e_opp a]

  let e_setfield r f v =
    (*TODO:NUPW: check for UNIONS *)
    let r = List.map 
      (fun g -> g,if Field.equal f g then v else e_getfield r g)
      (fields_of_field f)
    in e_record r

  (* -------------------------------------------------------------------------- *)
  (* --- Predicates                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  type pred = term
  type cmp = term -> term -> pred

  let p_bool t = t
  let e_prop t = t
  let p_bools xs = xs
  let e_props xs = xs
  let lift f x = f x

  let is_zero e = match T.repr e with
    | Kint z -> Qed.Z.null z
    | _ -> false

  let eqp = equal
  let comparep = compare

  let is_ptrue = is_true
  let is_pfalse = is_false
  let is_equal a b = is_true (e_eq a b)

  let p_equal = e_eq
  let p_neq = e_neq
  let p_leq = e_leq
  let p_lt = e_lt

  let p_positive e = e_leq e_zero e

  let p_true = e_true
  let p_false = e_false

  let p_not = e_not
  let p_forall = e_forall
  let p_exists = e_exists
  let p_subst = e_subst

  let p_and p q = e_and [p;q]
  let p_or p q = e_or [p;q]
  let p_imply h p = e_imply [h] p
  let p_hyps hs p = e_imply hs p
  let p_equiv = e_equiv
  let p_if = e_if

  let p_conj = e_and
  let p_disj = e_or

  let p_all f xs = e_and (List.map f xs)
  let p_any f xs = e_or (List.map f xs)

  let p_call = e_fun
  let p_close p = p_forall (Vars.elements (vars p)) p

  let occurs x t = Vars.mem x (vars t)
  let intersect a b = Vars.intersect (vars a) (vars b)
  let occursp = occurs
  let intersectp = intersect
  let varsp = vars
  let pred = repr
  let idp = id

  let pp_term fmt e = 
    if Wp_parameters.has_dkey "pretty"
    then T.debug fmt e
    else Pretty.pp_term Pretty.closed fmt e 
  let pp_pred fmt p = 
    if Wp_parameters.has_dkey "pretty"
    then T.debug fmt p
    else Pretty.pp_term Pretty.closed fmt p
  let pp_var fmt x = pp_term fmt (e_var x)

  let debugp = T.debug

  module Pmap = Tmap
  module Pset = Tset

end

open F
  
(* -------------------------------------------------------------------------- *)
(* --- Fresh Variables & Local Assumptions                                --- *)
(* -------------------------------------------------------------------------- *)
  
type gamma = { 
  mutable hyps : pred list ; 
  mutable vars : var list ;
}

(* -------------------------------------------------------------------------- *)
    
let cpool = Context.create "Lang.pool"
let cgamma = Context.create "Lang.gamma"
let apool = function None -> F.pool () | Some p -> p
let agamma = function None -> { hyps=[] ; vars=[] } | Some g -> g

let new_pool () = apool None
let new_gamma () = agamma None

let get_pool () = Context.get cpool
let get_gamma () = Context.get cgamma

let _local ?pool f =
  Context.bind cpool (apool pool) (Context.free cgamma f)
    
let freshvar ?basename tau = F.fresh (Context.get cpool) ?basename tau
let freshen x = F.alpha (Context.get cpool) x
  
(* -------------------------------------------------------------------------- *)
(* --- Hypotheses                                                         --- *)
(* -------------------------------------------------------------------------- *)
  
let local ?pool ?gamma f =
  Context.bind cpool (apool pool)
    (Context.bind cgamma (agamma gamma) f)
    
let assume p =
  if p != p_true then
    let d = Context.get cgamma in
    d.hyps <- p :: d.hyps
    
let epsilon ?basename t phi =
  let d = Context.get cgamma in
  let x = freshvar ?basename t in
  let e = e_var x in 
  d.hyps <- phi e :: d.hyps ; 
  d.vars <- x :: d.vars ; 
  e

let hypotheses g = g.hyps
let variables g = List.rev g.vars

let get_hypotheses () = (Context.get cgamma).hyps
let get_variables () = (Context.get cgamma).vars

(* -------------------------------------------------------------------------- *)
(* --- Alpha Conversion                                                   --- *)
(* -------------------------------------------------------------------------- *)

module Alpha =
struct

  module Vmap = Map.Make(Var)

  type t = {
    mutable vars : var Vmap.t ;
    mutable cache : term Tmap.t ;
  }
      
  let create () = { vars = Vmap.empty ; cache = Tmap.empty }

  let get w x =
    try Vmap.find x w.vars
    with Not_found -> 
      let y = freshen x in
      w.vars <- Vmap.add x y w.vars ; y
	
  let iter f w = Vmap.iter f w.vars

  let rec convert w e =
    try Tmap.find e w.cache
    with Not_found ->
      let a =
	match F.repr e with
	  | Logic.Var x -> e_var (get w x)
	  | Logic.Bind(q,x,t) ->
	      let v_temp = w.vars in
	      let c_temp = w.cache in
	      let y = freshen x in
	      w.vars <- Vmap.add x y w.vars ;
	      let b = convert w t in
	      w.vars <- 
		if Vmap.mem x v_temp then
		  Vmap.add x (Vmap.find x v_temp) w.vars
		else
		  Vmap.remove x w.vars ;
	      w.cache <- c_temp ;
	      F.e_bind q y b
	  | _ -> e_map (convert w) e
      in w.cache <- Tmap.add e a w.cache ; a

  let convertp = convert
	
end

(* -------------------------------------------------------------------------- *)

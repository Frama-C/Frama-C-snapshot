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

open Cil_types
open Cil_datatype
open Logic_ptree

module Wp = Wp_parameters

(* -------------------------------------------------------------------------- *)
(* --- L-Path                                                             --- *)
(* -------------------------------------------------------------------------- *)

type region_pattern =
  | FREE
  | PVAR
  | PREF
  | PMEM
  | PVECTOR
  | PMATRIX

type lrange =
  | R_index of term
  | R_range of term option * term option

type lpath = {
  loc : location ;
  lnode : lnode ;
  ltype : typ ;
}
and lnode =
  | L_var of varinfo
  | L_region of string
  | L_addr of lpath
  | L_star of typ * lpath
  | L_shift of lpath * typ * lrange
  | L_index of lpath * typ * lrange
  | L_field of lpath * fieldinfo list
  | L_cast of typ * lpath

type region_spec = {
  region_name: string option ;
  region_pattern: region_pattern ;
  region_lpath: lpath list ;
}

(*
let get_int e =
  match Logic_utils.constFoldTermToInt e with
  | None -> None
  | Some a -> Some (Integer.to_int a)

let get_int_option = function
  | None -> None
  | Some e -> get_int e
*)

module Lpath =
struct

  type t = lpath

  let compare_bound a b =
    match a,b with
    | None , None -> 0
    | Some a , Some b -> Term.compare a b
    | None , Some _ -> (-1)
    | Some _ , None -> 1

  let compare_range a b =
    match a,b with
    | R_index a , R_index b -> Term.compare a b
    | R_index _ , _ -> (-1)
    | _ , R_index _ -> 1
    | R_range(a1,b1) , R_range(a2,b2) ->
        let cmp = compare_bound a1 a2 in
        if cmp <> 0 then cmp else compare_bound b1 b2

  let rec compare a b =
    match a.lnode , b.lnode with
    | L_var x , L_var y -> Varinfo.compare x y
    | L_var _ , _ -> (-1)
    | _ , L_var _ -> 1
    | L_region a , L_region b -> String.compare a b
    | L_region _ , _ -> (-1)
    | _ , L_region _ -> 1
    | L_star(ta,a) , L_star(tb,b) ->
        let cmp = Typ.compare ta tb in
        if cmp <> 0 then cmp else compare a b
    | L_star _ , _ -> (-1)
    | _ , L_star _ -> 1
    | L_addr a , L_addr b -> compare a b
    | L_addr _ , _ -> (-1)
    | _ , L_addr _ -> 1
    | L_shift(a,ta,i) , L_shift(b,tb,j) -> compare_index a ta i b tb j
    | L_shift _ , _ -> (-1)
    | _ , L_shift _ -> 1
    | L_index(a,ta,i) , L_index(b,tb,j) -> compare_index a ta i b tb j
    | L_index _ , _ -> (-1)
    | _ , L_index _ -> 1
    | L_field(a,fs) , L_field(b,gs) ->
        let cmp = compare a b in
        if cmp <> 0 then cmp
        else Qed.Hcons.compare_list Fieldinfo.compare fs gs
    | L_field _ , _ -> (-1)
    | _ , L_field _ -> 1
    | L_cast(ta,a) , L_cast(tb,b) ->
        let cmp = Typ.compare ta tb in
        if cmp <> 0 then cmp else compare a b

  and compare_index a ta i b tb j =
    let cmp = compare a b in
    if cmp <> 0 then cmp else
      let cmp = Typ.compare ta tb in
      if cmp <> 0 then cmp else
        compare_range i j

  let equal a b = (compare a b = 0)

  let pp_bound pp fmt = function None -> () | Some a -> pp fmt a
  let pp_range pp fmt = function
    | R_index a -> pp fmt a
    | R_range(a,b) ->
        begin
          pp_bound pp fmt a ;
          Format.fprintf fmt "@,.." ;
          pp_bound pp fmt b ;
        end

  let first = function [] -> assert false | f::_ -> f
  let rec last = function [] -> assert false | [f] -> f | _::fs -> last fs

  let is_lval = function
    | L_var _ | L_region _ | L_index _ | L_field _ -> true
    | _ -> false

  let rec pp_lpath pp fmt a = match a.lnode with
    | L_var x -> Varinfo.pretty fmt x
    | L_region a -> Format.pp_print_string fmt a
    | L_field( p , [f] ) -> pfield pp p f fmt
    | L_field( p , fs ) ->
        Format.fprintf fmt "@[<hov 2>(%t@,..%t)@]"
          (pfield pp p (first fs)) (pfield pp p (last fs))
    | L_index(a,_,i) ->
        Format.fprintf fmt "@[<hov 2>%a@,[%a]@]"
          (pp_lval pp) a (pp_range pp) i
    | L_shift(a,_,i) ->
        Format.fprintf fmt "@[<hov 2>%a@,+(%a)@]"
          (pp_lpath pp) a (pp_range pp) i
    | L_star(_,a) -> Format.fprintf fmt "*%a" (pp_lval pp) a
    | L_addr a -> Format.fprintf fmt "&%a" (pp_lval pp) a
    | L_cast(t,a) -> Format.fprintf fmt "(%a)@,%a" Typ.pretty t (pp_lval pp) a

  and pfield pp a f fmt =
    Format.fprintf fmt "@[<hov 2>%a%a@]" (panchor pp) a Fieldinfo.pretty f

  and panchor pp fmt a =
    match a.lnode with
    | L_star(_,p) -> Format.fprintf fmt "%a@,->" (pp_lval pp) p
    | _ -> Format.fprintf fmt "%a@,." (pp_lval pp) a

  and pp_lval pp fmt a =
    if is_lval a.lnode then pp_lpath pp fmt a
    else Format.fprintf fmt "(%a)" (pp_lpath pp) a

  let pretty = pp_lpath Term.pretty

end

(* -------------------------------------------------------------------------- *)
(* --- Region Spec Printer                                                --- *)
(* -------------------------------------------------------------------------- *)

let patterns = [
  "PVAR" , PVAR ;
  "PREF" , PREF ;
  "PMEM" , PMEM ;
  "PVECTOR" , PVECTOR ;
  "PMATRIX" , PMATRIX ;
]

let p_name p = fst (List.find (fun (_,q) -> q = p) patterns)

let pp_pattern_spec fmt p =
  try Format.fprintf fmt "\\pattern{%s}" (p_name p) ; true
  with Not_found -> false

let pp_path_spec pp fmt coma lv =
  if coma then Format.fprintf fmt ",@ " ;
  Lpath.pp_lpath pp fmt lv ; true

let pp_region_spec pp fmt coma spec =
  begin
    if coma then Format.fprintf fmt ",@ " ;
    Format.fprintf fmt "@[<hv 2>" ;
    Extlib.may (Format.fprintf fmt "%s:@ ") spec.region_name ;
    let coma = pp_pattern_spec fmt spec.region_pattern in
    let coma = List.fold_left (pp_path_spec pp fmt) coma spec.region_lpath in
    Format.fprintf fmt "@]" ;
    coma
  end

(* -------------------------------------------------------------------------- *)
(* --- Typing Env                                                         --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  context : Logic_typing.typing_context ;
  mutable declared : string list ;
  mutable name : string option ;
  mutable pattern : region_pattern ;
  mutable paths : lpath list ;
  mutable specs : region_spec list ;
}

let error env ~loc msg = env.context.Logic_typing.error loc msg

let flush env =
  let region_name = env.name in env.name <- None ;
  let region_pattern = env.pattern in env.pattern <- FREE ;
  let region_lpath = List.rev env.paths in env.paths <- [] ;
  Extlib.may (fun a -> env.declared <- a::env.declared) region_name ;
  if not (region_name = None && region_lpath = []) then
    let region = { region_name ; region_pattern ; region_lpath } in
    env.specs <- region :: env.specs

(* -------------------------------------------------------------------------- *)
(* --- Type Utils                                                         --- *)
(* -------------------------------------------------------------------------- *)

let isIndexType t =
  match Logic_utils.unroll_type t with
  | Ctype (TInt _) | Linteger -> true
  | _ -> false

let getCompoundType env ~loc typ =
  match Cil.unrollType typ with
  | TComp(comp,_,_) -> comp
  | _ -> error env ~loc "Expected compound type for term"

(* -------------------------------------------------------------------------- *)
(* --- Path Typechecking                                                  --- *)
(* -------------------------------------------------------------------------- *)

let parse_varinfo env ~loc x =
  try
    match env.context.Logic_typing.find_var x with
    | { lv_origin = Some v } -> v
    | _ -> error env ~loc "Variable '%s' is not a C-variable" x
  with Not_found ->
    error env ~loc "Unknown variable (or region) '%s'" x

let parse_fieldinfo env ~loc comp f =
  try List.find (fun fd -> fd.fname = f) comp.cfields
  with Not_found ->
    error env ~loc "No field '%s' in compound type '%s'" f comp.cname

let parse_lindex env e =
  let open Logic_typing in
  let g = env.context in
  let t = g.type_term g g.pre_state e in
  if isIndexType t.term_type then t
  else error env ~loc:t.term_loc "Index term shall have a integer type"

let parse_ltype env ~loc t =
  let open Logic_typing in
  let g = env.context in
  let t = g.logic_type g loc g.pre_state t in
  match Logic_utils.unroll_type t with
  | Ctype typ -> typ
  | _ -> error env ~loc "C-type expected for casting l-values"

let parse_lbound env = function
  | None -> None
  | Some e -> Some (parse_lindex env e)

let parse_lrange env e =
  match e.lexpr_node with
  | PLrange(a,b) -> R_range( parse_lbound env a , parse_lbound env b )
  | _ -> R_index( parse_lindex env e )

let sugar ~loc node = { lexpr_loc = loc ; lexpr_node = node }

let rec field_range ~inside fa fb = function
  | [] -> []
  | f::fs ->
      let bound = Fieldinfo.equal f fa || Fieldinfo.equal f fb in
      if inside then f :: (if bound then [] else field_range ~inside fa fb fs)
      else if bound then f :: (field_range ~inside:true fa fb fs)
      else field_range ~inside fa fb fs

let rec typeof_fields = function
  | [] -> TVoid []
  | [f] -> f.ftype
  | f::fs ->
      let t = typeof_fields fs in
      if Typ.equal f.ftype t then t else TVoid []

let rec parse_lpath env e =
  let loc = e.lexpr_loc in
  match e.lexpr_node with
  | PLvar x ->
      if List.mem x env.declared
      then { loc ; lnode = L_region x ; ltype = TVoid [] }
      else
        let v = parse_varinfo env ~loc x in
        { loc ; lnode = L_var v ; ltype = v.vtype }
  | PLunop( Ustar , p ) ->
      let lv = parse_lpath env p in
      if Cil.isPointerType lv.ltype then
        let te = Cil.typeOf_pointed lv.ltype in
        { loc ; lnode = L_star(te,lv) ; ltype = te }
      else
        error env ~loc "Pointer-type expected for operator '&'"
  | PLunop( Uamp , p ) ->
      let lv = parse_lpath env p in
      let ltype = TPtr( lv.ltype , [] ) in
      { loc ; lnode = L_addr lv ; ltype }
  | PLbinop( p , Badd , r ) ->
      let { ltype } as lv = parse_lpath env p in
      let rg = parse_lrange env r in
      if Cil.isPointerType ltype then
        let te = Cil.typeOf_pointed ltype in
        { loc ; lnode = L_shift(lv,te,rg) ; ltype = ltype }
      else
      if Cil.isArrayType ltype then
        let te = Cil.typeOf_array_elem ltype in
        { loc ; lnode = L_shift(lv,te,rg) ; ltype = TPtr(te,[]) }
      else
        error env ~loc "Pointer-type expected for operator '+'"
  | PLdot( p , f ) ->
      let lv = parse_lpath env p in
      let comp = getCompoundType env ~loc:lv.loc lv.ltype in
      let fd = parse_fieldinfo env ~loc comp f in
      { loc ; lnode = L_field(lv,[fd]) ; ltype = fd.ftype }
  | PLarrow( p , f ) ->
      let sp = sugar ~loc (PLunop(Ustar,p)) in
      let pf = sugar ~loc (PLdot(sp,f)) in
      parse_lpath env pf
  | PLarrget( p , k ) ->
      let { ltype } as lv = parse_lpath env p in
      let rg = parse_lrange env k in
      if Cil.isPointerType ltype then
        let pointed = Cil.typeOf_pointed ltype in
        let ls = { loc ; lnode = L_shift(lv,pointed,rg) ; ltype } in
        { loc ; lnode = L_star(pointed,ls) ; ltype = pointed }
      else
      if Cil.isArrayType ltype then
        let elt = Cil.typeOf_array_elem ltype in
        { loc ; lnode = L_index(lv,elt,rg) ; ltype = elt }
      else
        error env ~loc:lv.loc "Pointer or array type expected"
  | PLcast( t , a ) ->
      let lv = parse_lpath env a in
      let ty = parse_ltype env ~loc t in
      { loc ; lnode = L_cast(ty,lv) ; ltype = ty }
  | PLrange( Some a , Some b ) ->
      let pa,fa = parse_fpath env a in
      let pb,fb = parse_fpath env b in
      let p =
        if Lpath.equal pa pb then pa
        else error env ~loc "Range of fields from different l-values" in
      let comp =
        if Compinfo.equal fa.fcomp fb.fcomp then fa.fcomp
        else error env ~loc "Range of fields from incompatible types" in
      let fields = field_range ~inside:false fa fb comp.cfields in
      let ltype = typeof_fields fields in
      { loc ; lnode = L_field(p,fields) ; ltype }
  | PLrange( Some a , None ) ->
      let p,fd = parse_fpath env a in
      let fields = field_range ~inside:false fd fd fd.fcomp.cfields in
      let ltype = typeof_fields fields in
      { loc ; lnode = L_field(p,fields) ; ltype }
  | PLrange( None , Some a ) ->
      let p,fd = parse_fpath env a in
      let fields = field_range ~inside:true fd fd fd.fcomp.cfields in
      let ltype = typeof_fields fields in
      { loc ; lnode = L_field(p,fields) ; ltype }
  | _ ->
      error env ~loc "Unexpected expression for region spec"

and parse_fpath env p =
  let lv = parse_lpath env p in
  match lv.lnode with
  | L_field( a , [f] ) -> a , f
  | _ -> error env ~loc:lv.loc "Missing field access in range"

(* -------------------------------------------------------------------------- *)
(* --- Spec Typechecking                                                  --- *)
(* -------------------------------------------------------------------------- *)

let kspec = ref 0
let registry = Hashtbl.create 0

let parse_pattern env ~loc names params =
  match names with
  | [name] ->
      let pattern =
        try List.assoc name patterns
        with Not_found -> error env ~loc "Unknown pattern '%s'" name in
      if params <> [] then
        error env ~loc "Unexpected parameters for pattern '%s'" name ;
      pattern
  | [] -> error env ~loc "Missing pattern name"
  | _ -> error env ~loc "Duplicate pattern names"

let rec parse_region env p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLnamed( name , p ) ->
      flush env ;
      env.name <- Some name ;
      parse_region env p
  | PLapp("\\pattern",names,params) ->
      let pattern = parse_pattern env ~loc names params in
      if env.pattern <> FREE && env.pattern <> pattern then
        error env ~loc "Duplicate pattern definition in region"
      else
        env.pattern <- pattern
  | _ ->
      let path = parse_lpath env p in
      env.paths <- path :: env.paths

let typecheck ~typing_context ~loc:_loc ps =
  let env = {
    name = None ;
    declared = [] ;
    context = typing_context ;
    pattern = FREE ;
    paths = [] ; specs = [] ;
  } in
  List.iter (parse_region env) ps ;
  let id = !kspec in incr kspec ;
  let specs = flush env ; env.specs in
  Hashtbl.add registry id specs ; Ext_id id

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let of_extid = Hashtbl.find registry
let of_extrev = function
  | { ext_name="region" ; ext_kind = Ext_id k } -> of_extid k
  | _ -> raise Not_found
let of_extension e = List.rev (of_extrev e)
let of_behavior bhv =
  List.fold_left
    (fun acc e -> List.rev_append (try of_extrev e with Not_found -> []) acc)
    [] bhv.Cil_types.b_extended

let pp_extension printer fmt = function
  | Ext_id k ->
      let spec = try Hashtbl.find registry k with Not_found -> [] in
      ignore (List.fold_left (pp_region_spec printer#term fmt) false spec)
  | _ -> ()

let specified =
  let re = Str.regexp_case_fold "region" in
  fun model ->
    try
      ignore (Str.search_forward re model 0) ; true
    with Not_found -> false

let register () =
  if Wp.Region.get () || Wp.Region_annot.get () ||
     List.exists specified (Wp.Model.get ())
  then
    begin
      Logic_typing.register_behavior_extension "region" true typecheck ;
      Cil_printer.register_behavior_extension "region" pp_extension ;
    end

let () = Cmdline.run_after_configuring_stage register

(* -------------------------------------------------------------------------- *)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(* --- L-Value Indexed Memory Model                                       --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Lang
open Lang.F
open Memory

module Logic = Qed.Logic

let datatype = "MemZeroAlias"

let configure () =
  begin
    Context.set Lang.pointer (fun _typ -> Logic.Int) ;
    Context.set Cvalues.null F.(p_equal e_zero) ;
  end

(* TODO: compute actual separation hypotheses *)
let separation () = []

(* -------------------------------------------------------------------------- *)
(* --- Chunks                                                             --- *)
(* -------------------------------------------------------------------------- *)

type chunk = varinfo * path list (* from left to right *)
and path = S | I | F of fieldinfo

let hash_path = function S -> 1 | I -> 2 | F fd -> Fieldinfo.hash fd

let equal_path p q = match p,q with
  | S , S -> true
  | I , I -> true
  | F f , F g -> Fieldinfo.equal f g
  | _ -> false

let compare_path p q = match p,q with
  | S , S -> 0
  | S , _ -> (-1)
  | _ , S -> 1
  | I , I -> 0
  | I , _ -> (-1)
  | _ , I -> 1
  | F f , F g -> Fieldinfo.compare f g

[@@@ warning "-32"]
let pp_path fmt = function
  | S -> Format.pp_print_char fmt '*'
  | I -> Format.pp_print_string fmt "[]"
  | F f -> Format.pp_print_char fmt '.' ; Fieldinfo.pretty fmt f
[@@@ warning "+32"]

let rec object_of_rpath x = function
  | [] -> Ctypes.object_of x.vtype
  | S :: p -> Ctypes.object_of_pointed (object_of_rpath x p)
  | I :: p -> Ctypes.object_of_array_elem (object_of_rpath x p)
  | F f :: _ -> Ctypes.object_of f.ftype

let rec dim_of_path t = function
  | [] -> t
  | S :: p | F _ :: p -> dim_of_path t p
  | I :: p -> dim_of_path Qed.Logic.(Array(Int,t)) p

module Chunk =
struct
  type t = chunk
  let self = "mtree"
  let hash (x,p) = Qed.Hcons.hash_list hash_path (Varinfo.hash x) p
  let equal (x,p) (y,q) = Varinfo.equal x y && Qed.Hcons.equal_list equal_path p q
  let compare (x,p) (y,q) =
    let cmp = Varinfo.compare x y in
    if cmp <> 0 then cmp else Qed.Hcons.compare_list compare_path p q

  let rec pp x fmt = function
    | [] -> Varinfo.pretty fmt x
    | [S] -> Format.fprintf fmt "*%a" Varinfo.pretty x
    | S::ps -> Format.fprintf fmt "*(%a)" (pp x) ps
    | I::ps -> Format.fprintf fmt "%a[]" (pp x) ps
    | F f::S::ps -> Format.fprintf fmt "%a->%a" (pp x) ps Fieldinfo.pretty f
    | F f::ps -> Format.fprintf fmt "%a.%a" (pp x) ps Fieldinfo.pretty f
  
  let pretty fmt (x,p) = Format.fprintf fmt "@[<hov 2>%a@]" (pp x) (List.rev p)
  let tau_of_chunk (x,p) =
    let te = Lang.tau_of_object (object_of_rpath x (List.rev p)) in
    dim_of_path te p
  let basename_of_chunk (x,_) = LogicUsage.basename x
  let is_framed (x,p) = not x.vglob && p = []
end

module Heap = Qed.Collection.Make(Chunk)
module Sigma = Sigma.Make(Chunk)(Heap)

type loc =
  | Null
  | Var of varinfo
  | Star of loc
  | Array of loc * F.term
  | Field of loc * fieldinfo

type sigma = Sigma.t
type segment = loc rloc

let rec pretty fmt = function
  | Null -> Format.pp_print_string fmt "null"
  | Var x -> Varinfo.pretty fmt x
  | Star(Var x) -> Format.fprintf fmt "*%a" Varinfo.pretty x
  | Star p -> Format.fprintf fmt "*(%a)" pretty p
  | Array(p,k) -> Format.fprintf fmt "%a[%a]" pretty p Lang.F.pp_term k
  | Field(Star p,f) -> Format.fprintf fmt "%a->%a" pretty p Fieldinfo.pretty f
  | Field(p,f) -> Format.fprintf fmt "%a.%a" pretty p Fieldinfo.pretty f

let rec vars = function
  | Var _ | Null -> Vars.empty
  | Star p | Field(p,_) -> vars p
  | Array(p,k) -> Vars.union (vars p) (F.vars k)

let rec occurs x = function
  | Null | Var _ -> false
  | Star p | Field(p,_) -> occurs x p
  | Array(p,k) -> F.occurs x k || occurs x p

let source = "Tree Model"

let null = Null
let literal ~eid:_ _ = Warning.error ~source "No Literal"
let pointer_loc _t = Warning.error ~source "No Pointer Loc"
let pointer_val _v = Warning.error ~source "No Pointer Val"

let cvar x = Var x
let field l f = Field(l,f)
let shift l _obj k = Array(l,k)

let base_addr _l = Warning.error ~source "No Base Addr"
let block_length _s _obj _l = Warning.error ~source "No Block Length"

let cast _ _l = Warning.error ~source "No Cast"
let loc_of_int _ _ = Warning.error ~source "No Hardware Address"
let int_of_loc _ _ = Warning.error ~source "No Hardware Address"

let rec walk ps ks = function
  | Null -> Warning.error ~source "No Null Walk"
  | Var x -> (x,ps),ks
  | Star l -> walk (S::ps) ks l
  | Array(l,k) -> walk (I::ps) (k::ks) l
  | Field(l,f) -> walk (F f::ps) ks l

let access l = walk [] [] l

let domain _obj l =
  try Heap.Set.singleton (fst (access l))
  with _ -> Heap.Set.empty

let value sigma l =
  let m,ks = access l in
  let x = Sigma.get sigma m in
  List.fold_left F.e_get (e_var x) ks

let rec update a ks v =
  match ks with
  | [] -> v
  | k::ks -> F.e_set a k (update (F.e_get a k) ks v)

let set s m ks v = if ks = [] then v else update (e_var (Sigma.get s m)) ks v

let load sigma obj l =
  if Ctypes.is_pointer obj then Loc (Star l) else Val(value sigma l)

let stored seq _obj l e =
  let m,ks = access l in
  let x = F.e_var (Sigma.get seq.post m) in
  [ F.p_equal x (set seq.pre m ks e) ]

let copied seq obj a b =
  stored seq obj a (value seq.pre b)

let assigned _s _obj _sloc = []

type state = Chunk.t Tmap.t
let state (s:sigma) =
  let m = ref Tmap.empty in
  Sigma.iter (fun c x -> m := Tmap.add (F.e_var x) c !m) s ; !m

let imval c = Memory.Mchunk (Pretty_utils.to_string Chunk.pretty c)
let iter f s = Tmap.iter (fun v c -> f (imval c) v) s
let lookup (s : state) (e : Lang.F.term) = imval (F.Tmap.find e s)
let apply f s =
  let m = ref Tmap.empty in
  Tmap.iter (fun e c -> m := Tmap.add (f e) c !m) s ; !m

let rec ipath lv = function
  | [] -> lv
  | S::w -> ipath (Mval lv,[]) w
  | I::_ -> raise Not_found
  | F f::w ->
      let (host,path) = lv in
      ipath (host, path @ [Mfield f]) w
let ilval (x,p) = ipath (Mvar x,[]) p

let heap domain state =
  Tmap.fold
    (fun m c w ->
       if Vars.intersect (F.vars m) domain
       then Heap.Map.add c m w else w
    ) state Heap.Map.empty

let updates seq domain =
  let pre = heap domain seq.pre in
  let post = heap domain seq.post in
  let pool = ref Bag.empty in
  Heap.Map.iter2
    (fun c v1 v2 ->
       try
         match v1,v2 with
         | _,None -> ()
         | None,Some v ->
             pool := Bag.add (Mstore(ilval c,v)) !pool
         | Some v1,Some v2 ->
             if v2 != v1 then
               pool := Bag.add (Mstore (ilval c,v2)) !pool
       with Not_found -> ()
    ) pre post ;
  !pool

let no_pointer () = Warning.error ~source "No Pointer"

let is_null = function Null -> F.p_true | _ -> no_pointer ()
let loc_eq _ _ = no_pointer ()
let loc_lt _ _ = no_pointer ()
let loc_leq _ _ = no_pointer ()
let loc_neq _ _ = no_pointer ()
let loc_diff _ _ _ = no_pointer ()

let valid _sigma _l = Warning.error ~source "No validity"
let scope sigma _s _xs = sigma , []
let global _sigma _p = p_true

let included _s1 _s2 = no_pointer ()
let separated _s1 _s2 = no_pointer ()

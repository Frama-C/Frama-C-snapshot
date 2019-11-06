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

(* TODO DEVEL MODE *)
[@@@ warning "-32-37-60"]

(* -------------------------------------------------------------------------- *)
(* --- Region Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Sigs
open Definitions

module Wp = Wp_parameters
module F = Lang.F
module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Why-3 Region Theory                                                --- *)
(* -------------------------------------------------------------------------- *)

let library = "region"

let cluster_region () =
  Definitions.cluster ~id:"Region" ~title:"Region Index Constructors" ()

(* Index *)
let t_addr = MemMemory.t_addr
let t_index = L.Data( Lang.datatype ~library "index" ,[] )
let f_addrof = Lang.extern_f ~library ~result:t_addr "addrof"
let f_consistent = Lang.extern_fp ~library "consistent"
let f_consistent_range = Lang.extern_fp ~library "consistent_range"

let a_addrof l = F.e_fun f_addrof [l]
let p_consistent l = F.p_call f_consistent [l]
let p_consistent_range l n = F.p_call f_consistent_range [l;n]
let p_range k n ps = F.(p_leq e_zero k :: p_lt k n :: ps)

(* Null *)
let f_inull = Lang.extern_f ~library ~result:t_index "inull"
let l_inull = F.e_fun f_inull []
let a_null = MemMemory.a_null
let p_inull l = F.p_equal a_null (a_addrof l)

(* Address *)

let p_separated p n q m = F.p_call MemMemory.p_separated [p;n;q;m]

(* Constructors *)
let region_ctor ~result =
  Lang.extern_f ~library ~category:L.Constructor ~result "%s"

let f_addr_var = region_ctor ~result:t_addr "addr_var"
let f_addr_ref = region_ctor ~result:t_addr "addr_ref"
let f_base_var = region_ctor ~result:L.Int "base_var"
let f_base_ref = region_ctor ~result:L.Int "base_ref"
let f_index_var = region_ctor ~result:t_index "index_var"
let f_index_ref = region_ctor ~result:t_index "index_ref"
let f_index_mem = region_ctor ~result:t_index "index_mem"

let a_addr_var x = F.e_fun f_addr_var [x]
let a_addr_ref p = F.e_fun f_addr_ref [p]
let l_index_var x = F.e_fun f_index_var [F.e_int x]
let l_index_mem l k n = F.e_fun f_index_ref [l;k;n]
let l_index_ref l = F.e_fun f_index_ref [l]

(* Shifts *)

let a_shift = MemMemory.a_shift
let f_shift_index = Lang.extern_f ~library ~result:t_index "shift_index"
let l_shift_index l p = F.e_fun f_shift_index [l;p]

(* Bits *)
let t_bits = L.Int

(* -------------------------------------------------------------------------- *)
(* --- Index Simplifiers                                                  --- *)
(* -------------------------------------------------------------------------- *)

type index_builtin = {
  index: (Lang.lfun -> F.term list -> F.term -> F.term) ;
  addrof : (F.term list -> F.term) ;
  consistent : (F.term list -> F.pred) ;
}

module IndexBuiltin = WpContext.Static
    (struct
      type key = Lang.lfun
      type data = index_builtin
      let name = "MemRegion.INDEXER"
      include Lang.Fun
    end)

(* f enjoys shifting props:
   -  f(l,p,...)+k == f(l,p+k,...)
   - &f(l,p,...) = &l+p
*)
let is_shiftable f =
  ( f == f_shift_index ) || ( f == f_index_mem)

let phi_addrof index =
  match F.repr index with
  | L.Fun(f,[]) when f == f_inull -> a_null
  | L.Fun(f,[x]) when f == f_index_var -> a_addr_var x
  | L.Fun(f,[l]) when f == f_index_ref -> a_addr_ref (a_addrof l)
  | L.Fun(f,l::p::_) when is_shiftable f -> a_shift (a_addrof l) p
  | L.Fun(f,es) -> (IndexBuiltin.find f).addrof es
  | _ -> raise Not_found

let phi_shift_index l p =
  if p == F.e_zero then l else
    match F.repr l with
    | L.Fun(f,l::q::w) when is_shiftable f -> F.e_fun f (l::(F.e_add p q)::w)
    | L.Fun(f,es) -> (IndexBuiltin.find f).index f es p
    | _ -> raise Not_found

let phi_consistent index =
  match F.repr index with
  | L.Fun(f,[]) when f == f_inull -> F.e_false
  | L.Fun(f,[x]) when f == f_index_var ->
      F.e_neq x F.e_zero
  | L.Fun(f,[l]) when f == f_index_ref ->
      F.e_prop @@ p_consistent l
  | L.Fun(f,[l;k;n]) when f == f_index_mem ->
      F.e_prop @@ F.p_conj @@ p_range k n [p_consistent l]
  | L.Fun(f,es) ->
      F.e_prop @@ (IndexBuiltin.find f).consistent es
  | _ -> raise Not_found

let phi_consistent_range index sizeof =
  match F.repr index with
  | L.Fun(f,[l;k;n]) when f == f_index_mem ->
      F.e_prop @@ F.p_conj @@ F.[
          p_leq e_zero sizeof ;
          p_leq e_zero k ;
          p_leq (e_add k sizeof) n ;
          p_consistent l ;
        ]
  | _ -> raise Not_found

let () = Context.register
    begin fun () ->
      MemMemory.register f_addr_var
        ~base:(F.e_fun f_base_var) ~offset:(fun _ -> F.e_zero) ;
      MemMemory.register f_addr_ref
        ~base:(F.e_fun f_base_ref) ;
      F.set_builtin_1 f_addrof phi_addrof ;
      F.set_builtin_1 f_consistent phi_consistent ;
      F.set_builtin_2 f_shift_index phi_shift_index ;
      F.set_builtin_2 f_consistent_range phi_consistent_range ;
    end

let cid = ref 0 (* TODO: projectified *)

let constructor ~basename ~params ~index ~addrof ~consistent =
  let id = incr cid ; !cid in
  let lfun = Lang.generated_f ~result:t_index "%s_%d" basename id in
  let ps = List.map F.e_var params in
  let l = F.e_fun lfun ps in
  let k = Lang.freshvar ~basename:"k" L.Int in
  let ofs = F.e_var k in
  (* Must compute properties before registering simplifiers *)
  let p_addrof = F.p_equal (a_addrof l) (addrof ps) in
  let p_consistent = F.p_equiv (p_consistent l) (consistent ps) in
  let p_index = F.p_equal (l_shift_index l ofs) (index lfun ps ofs) in
  IndexBuiltin.define lfun { index ; addrof ; consistent } ;
  fun cluster ->
    begin
      Definitions.define_symbol {
        d_cluster = cluster ;
        d_lfun = lfun ; d_params = params ; d_types = 0 ;
        d_definition = Logic t_index ;
      } ;
      Definitions.define_lemma {
        l_cluster = cluster ;
        l_assumed = true ;
        l_name = Printf.sprintf "addrof_%s_%d" basename id ;
        l_forall = params ; l_types = 0 ; l_triggers = [] ;
        l_lemma = p_addrof ;
      } ;
      Definitions.define_lemma {
        l_cluster = cluster ;
        l_assumed = true ;
        l_name = Printf.sprintf "consistent_%s_%d" basename id ;
        l_forall = params ; l_types = 0 ; l_triggers = [] ;
        l_lemma = p_consistent ;
      } ;
      if p_index != F.p_true then
        Definitions.define_lemma {
          l_cluster = cluster ;
          l_assumed = true ;
          l_name = Printf.sprintf "index_%s_%d" basename id ;
          l_forall = params @ [k] ; l_types = 0 ; l_triggers = [] ;
          l_lemma = p_index ;
        } ;
      lfun
    end

(* -------------------------------------------------------------------------- *)
(* --- Field Index Constructors                                           --- *)
(* -------------------------------------------------------------------------- *)

module FIELD =
struct

  type t = int list (* Overlay offsets *)

  let pretty fmt = function
    | [] -> Format.fprintf fmt "{}"
    | p::ps ->
        begin
          Format.fprintf fmt "@[<hov 2>{%d" p ;
          List.iter (fun p -> Format.fprintf fmt ",@,%d" p) ps ;
          Format.fprintf fmt "}@]" ;
        end

  let compare = Pervasives.compare

  (* Extract constant offset *)
  let offset k =
    let rec walk s a =
      match F.repr a with
      | L.Add es -> List.fold_left walk s es
      | L.Kint z ->
          (try s + Integer.to_int z
           with Z.Overflow -> s)
      | _ -> s
    in walk 0 k

  let builtin_index f es q = match es with
    | [l;p] -> F.e_fun f [l;F.e_add q p]
    | _ -> raise Not_found

  let builtin_addrof = function
    | [l;p] -> a_shift (a_addrof l) p
    | _ -> raise Not_found

  let builtin_consistent fds = function
    | [l;p] -> F.p_and (p_consistent l)
                 (F.p_any (fun fd -> F.p_equal (F.e_int fd) p) fds)
    | _ -> raise Not_found

end

(* Model Independant Generators *)
module FIELD_GEN = WpContext.StaticGenerator(FIELD)
    (struct
      type key = FIELD.t
      type data = cluster -> Lang.lfun
      let name = "MemRegion.FIELD_GEN"
      let compile fds =
        let l = Lang.freshvar ~basename:"l" t_index in
        let p = Lang.freshvar ~basename:"p" L.Int in
        constructor
          ~basename:"field"
          ~params:[l;p]
          ~index:FIELD.builtin_index
          ~addrof:FIELD.builtin_addrof
          ~consistent:(FIELD.builtin_consistent fds)
    end)

(* Model Dependent Definitions *)
module FIELD_MODEL = WpContext.Generator(FIELD)
    (struct
      type key = FIELD.t
      type data = Lang.lfun
      let name = "MemRegion.FIELD_MODEL"
      let compile fds = FIELD_GEN.get fds @@ cluster_region ()
    end)

let l_field ovl l k =
  let fds = List.map (fun rg -> rg.Layout.ofs) ovl in
  F.e_fun (FIELD_MODEL.get fds) [l;k]

(* -------------------------------------------------------------------------- *)
(* --- Array Index Constructors                                           --- *)
(* -------------------------------------------------------------------------- *)

module ARRAY =
struct

  type t = int * int list
  let compare = Pervasives.compare
  let pretty fmt (s,ds) = Format.fprintf fmt "%d%a" s Layout.Matrix.pretty ds

  (* Coefficient from Matrix dimensions: c_i = \Pi_{i<j} d_j *)
  let coefs s ds =
    let rec walk cs s = function
      | d::ds -> walk (s::cs) (d*s) ds
      | [] -> cs
    in walk [] s ds

  (* All zeroes *)
  let zeroes = List.map (fun _ -> F.e_zero)

  (* Address shift with coefficient c_i for each index k_i *)
  let rec shift a cs ks =
    match cs , ks with
    | c::cs , k::ks -> shift (a_shift a (F.e_fact c k)) cs ks
    | _ -> a

  (* Address of an array index *)
  let builtin_addrof cs = function
    | l::ks -> shift (a_addrof l) cs ks
    | _ -> raise Not_found

  (* Add conditions (0 <= ki < ni) to [ps].
     WARNING: ns = rev ds *)
  let rec add_range_dims ps ks ns =
    match ks , ns with
    | k::ks , n::ns ->
        add_range_dims F.(p_range k (e_int n) ps) ks ns
    | k::ks , [] ->
        add_range_dims F.(p_equal e_zero k :: ps) ks []
    | [] , _ -> ps

  (* Consistent index.
     WARNING: ns = rev ds *)
  let builtin_consistent ns = function
    | l::ks -> F.p_conj (add_range_dims [p_consistent l] ks ns)
    | _ -> raise Not_found

  (* Extract linear forms *)
  let rec get_linear poly a =
    match F.repr a with
    | L.Add es -> List.fold_left get_linear poly es
    | L.Kint z ->
        (try (Integer.to_int z,F.e_one)::poly
         with Z.Overflow -> (1,a)::poly)
    | L.Times(c,e) ->
        (try (Integer.to_int c,e)::poly
         with Z.Overflow -> (1,a)::poly)
    | _ -> (1,a)::poly

  (* Some of linear form *)
  let rec add_linear s = function
    | (k,e)::poly -> add_linear (F.e_add s (F.e_fact k e)) poly
    | [] -> s

  (* Euclidian division *)
  (* euclid q r ci p = q',r' <-> p + ci.q + r = ci.q' + r' *)
  let rec euclid q r ci = function
    | [] -> q,r
    | (c,k)::poly ->
        let q0 = c / ci in
        let r0 = c mod ci in
        euclid (F.e_add q (F.e_fact q0 k)) ((r0,k)::r) ci poly

  (* Linear offset decomposed on each coefficient *)
  let rec add_linear_index cs ks ks' p =
    match cs , ks with
    | c :: cs , k :: ks ->
        let k' , r = euclid k [] c p in
        add_linear_index cs ks (k'::ks') r
    | _ -> List.rev_append ks' ks , p

  (* Linear offset and remainder delta *)
  let offset cs ks p =
    let ks',r = add_linear_index cs ks [] (get_linear [] p) in
    ks' , add_linear F.e_zero r

  (* Builtin simplifier *)
  let builtin_index cs f es p = match es with
    | l::ks ->
        let ks' , r = offset cs ks p in
        if Qed.Hcons.equal_list F.equal ks ks' then
          raise Not_found
        else
          let l' = F.e_fun f (l :: ks) in
          l_shift_index l' r
    | _ -> raise Not_found

end

module ARRAY_GEN = WpContext.StaticGenerator(ARRAY)
    (struct
      type key = ARRAY.t
      type data = (cluster -> Lang.lfun)
      let name = "MemRegion.ARRAY_GEN"
      let compile (s,ds) =
        let l = Lang.freshvar ~basename:"l" t_index in
        let ks = List.map (fun _ -> Lang.freshvar ~basename:"k" L.Int) ds in
        let cs = ARRAY.coefs s ds in
        let ns = List.rev ds in
        constructor
          ~basename:"array"
          ~params:(l::ks)
          ~index:(ARRAY.builtin_index cs)
          ~addrof:(ARRAY.builtin_addrof cs)
          ~consistent:(ARRAY.builtin_consistent ns)
    end)

module ARRAY_MODEL = WpContext.Generator(ARRAY)
    (struct
      type key = ARRAY.t
      type data = Lang.lfun
      let name = "MemRegion.ARRAY_MODEL"
      let compile dim = ARRAY_GEN.get dim @@ cluster_region ()
    end)

let l_array s ds l ks = F.e_fun (ARRAY_MODEL.get (s,ds)) (l::ks)

(* -------------------------------------------------------------------------- *)
(* --- Model Context                                                      --- *)
(* -------------------------------------------------------------------------- *)

let datatype = "MemRegion"

let configure () =
  begin
    Context.set Lang.pointer (fun _ -> t_index) ;
    Context.set Cvalues.null p_inull ;
  end

let configure_ia =
  let no_binder = { bind = fun _ f v -> f v } in
  fun _vertex -> no_binder

let hypotheses () = []

let error msg = Warning.error ~source:"Region Model" msg

(* -------------------------------------------------------------------------- *)
(* --- Region Maps                                                        --- *)
(* -------------------------------------------------------------------------- *)

let map () =
  RegionAnalysis.get
    begin match WpContext.get_scope () with
      | WpContext.Global -> None
      | WpContext.Kf kf -> Some kf
    end

(* -------------------------------------------------------------------------- *)
(* --- Locations                                                          --- *)
(* -------------------------------------------------------------------------- *)

open Layout

type region = Region.region
type index = F.term

let pp_index = F.pp_term
let pp_region = Region.R.pretty
let pp_value = Value.pretty pp_region
let pp_args fmt = function
  | [] -> ()
  | k::ks ->
      F.pp_term fmt k ;
      List.iter (fun k -> Format.fprintf fmt "@,,%a" F.pp_term k) ks

let pp_field fmt k =
  if F.is_atomic k then
    Format.fprintf fmt "@,+%a" F.pp_term k
  else
    Format.fprintf fmt "@,+(%a)" F.pp_term k

let pp_delta fmt k =
  if k != F.e_zero then pp_field fmt k

type loc =
  | GarbledMix (* any possible location *)
  | Index of index (* unqualified address *)
  | Lref of region * index * region
  | Lmem of region * index * root * region value
  | Lraw of region * index * root * region option
  | Lfld of region * index * F.term * region overlay
  | Larr of region * index * F.term * F.term list * int * int list
  (* For Lxxx locations:
     - index: start index inside the chunk
     - term: additional shift index
     - term list: array index from start *)

(* -------------------------------------------------------------------------- *)
(* --- Loc Basics                                                         --- *)
(* -------------------------------------------------------------------------- *)

let null = Index l_inull

let vars = function
  | GarbledMix -> F.Vars.empty
  | Index l | Lref(_,l,_) | Lmem(_,l,_,_) | Lraw(_,l,_,_) -> F.vars l
  | Lfld(_,l,k,_) ->
      F.Vars.union (F.vars l) (F.vars k)
  | Larr(_,l,k,ks,_,_) ->
      Qed.Hcons.fold_list F.Vars.union F.vars F.Vars.empty (l::k::ks)

let occurs x = function
  | GarbledMix -> false
  | Index l | Lref(_,l,_) | Lmem(_,l,_,_) | Lraw(_,l,_,_) -> F.occurs x l
  | Lfld(_,l,k,_) ->
      F.occurs x l || F.occurs x k
  | Larr(_,l,k,ks,_,_) ->
      List.exists (F.occurs x) (l::k::ks)

let pretty fmt = function
  | GarbledMix -> Format.pp_print_string fmt "garbled-mix"
  | Index l ->
      Format.fprintf fmt "@[<hov 2>Index(%a)@]" pp_index l
  | Lref(r,l,r') ->
      Format.fprintf fmt "@[<hov 2>Ref@,{%a->%a}@,(%a)@]"
        pp_region r pp_region r' pp_index l
  | Lmem(r,l,_,v) ->
      Format.fprintf fmt "@[<hov 2>Mem@,{%a:@,%a}@,(%a)@]"
        pp_region r pp_value v pp_index l
  | Lraw(r,l,_,None) ->
      Format.fprintf fmt "@[<hov 2>Raw@,{%a}@,(%a)"
        pp_region r pp_index l
  | Lraw(r,l,_,Some r') ->
      Format.fprintf fmt "@[<hov 2>Raw@,{%a->%a}@,(%a)"
        pp_region r pp_region r' pp_index l
  | Lfld(r,l,k,_) ->
      Format.fprintf fmt "@[<hov 2>Field@,{%a}@,(%a%a)@]"
        pp_region r pp_index l pp_field k
  | Larr(r,l,k,ks,_,_) ->
      Format.fprintf fmt "@[<hov 2>Index@,{%a}@,@[<hov 2>(%a[%a]%a)@]@]"
        pp_region r pp_index l pp_args ks pp_delta k

(* -------------------------------------------------------------------------- *)
(* --- Loc Constructors                                                   --- *)
(* -------------------------------------------------------------------------- *)

let rec index map (r:region) (l:index)  (ofs:F.term) (len:int) =
  index_chunk map r l ofs len (Region.chunk map r)

and index_chunk map (r:region) l ofs len = function
  | Mref r' -> Lref(r,l_shift_index l ofs,r')
  | Mraw(m,p) -> Lraw(r,l_shift_index l ofs,m,p)
  | Mmem(m,v) -> Lmem(r,l_shift_index l ofs,m,v)
  | Mcomp(_,[{ofs=0;reg;dim}]) -> index_dim map reg l ofs len dim
  | Mcomp(_,overlay) -> index_field map r l ofs len overlay

and index_field map r l ofs len overlay =
  try
    let k = FIELD.offset ofs in
    let rg = List.find (Layout.Range.included k len) overlay in
    let fd = F.e_int k in
    let l' = l_field overlay l fd in
    index_dim map rg.reg l' (F.e_sub ofs fd) len rg.dim
  with Not_found ->
    Lfld(r,l,ofs,overlay)

and index_dim map r l ofs len = function
  | Raw s | Dim(s,[]) ->
      index map r (l_index_mem l F.e_zero (F.e_int s)) ofs len
  | Dim(s,ds) ->
      index_array map r l (ARRAY.zeroes ds) ofs len s ds

and index_array map r l ks ofs len s ds =
  let cs = ARRAY.coefs s ds in
  let ks,ofs = ARRAY.offset cs ks ofs in
  if len <= s then
    let l' = l_array s ds l ks in
    index map r l' ofs len
  else
    Larr(r,l,ofs,ks,s,ds)

and shift_index_loc map loc ofs len =
  match loc with
  | GarbledMix -> GarbledMix
  | Index l -> Index (l_shift_index l ofs)
  | Lref(r,l,r') -> Lref(r,l_shift_index l ofs,r')
  | Lmem(r,l,m,v) -> Lmem(r,l_shift_index l ofs,m,v)
  | Lraw(r,l,m,p) -> Lraw(r,l_shift_index l ofs,m,p)
  | Lfld(r,l,k,overlay) -> index_field map r l (F.e_add k ofs) len overlay
  | Larr(r,l,k,ks,s,ds) -> index_array map r l ks (F.e_add k ofs) len s ds

let cvar x =
  let map = map () in
  let region = Region.of_cvar map x in
  let id = if Cil.isConstType x.vtype then - x.vid else x.vid in
  index map region (l_index_var id) F.e_zero (Cil.bitsSizeOf x.vtype)

let field loc fd =
  let map = map () in
  let ofs,len = Region.field_offset map fd in
  shift_index_loc map loc (F.e_int ofs) len

let shift loc obj n =
  let map = map () in
  let s = Ctypes.bits_sizeof_object obj in
  shift_index_loc map loc (F.e_fact s n) s

let pointer_loc l = Index l
let pointer_val = function
  | GarbledMix -> error "Can not obtain address of Garbled-Mix location"
  | Index l | Lref(_,l,_) | Lmem(_,l,_,_) | Lraw(_,l,_,_) -> l
  | Lfld(_,l,k,overlay) -> l_field overlay l k
  | Larr(_,l,k,ks,s,ds) -> l_shift_index (l_array s ds l ks) k

let loc_of_index re ty l =
  index (map()) re l F.e_zero (Cil.bitsSizeOf ty)

(* -------------------------------------------------------------------------- *)
(* --- Chunks                                                             --- *)
(* -------------------------------------------------------------------------- *)

type chunk =
  | Mu_alloc
  | Mu_raw of region * root
  | Mu_mem of region * root * region value

module Chunk =
struct

  type t = chunk
  let self = "region"

  let id = function
    | Mu_raw(r,_) | Mu_mem(r,_,_) -> Region.id r
    | Mu_alloc -> Region.noid

  let hash m = id m
  let compare m m' = if m==m then 0 else Pervasives.compare (id m) (id m')
  let equal m m' = m==m' || (id m = id m')

  let tau_of_value = function
    | Int _ -> L.Int
    | Float _ -> L.Real
    | Pointer _ -> t_index

  let tau_of_chunk = function
    | Mu_alloc -> MemMemory.t_malloc
    | Mu_raw _ -> t_bits
    | Mu_mem(_,root,v) ->
        let value = tau_of_value v in
        if Root.indexed root then L.Array(t_addr,value) else value

  let basename_of_chunk = function
    | Mu_raw _ -> "B"
    | Mu_mem(_,root,Int _) -> if Root.indexed root then "M" else "V"
    | Mu_mem(_,root,Float _) -> if Root.indexed root then "Mf" else "F"
    | Mu_mem(_,root,Pointer _) -> if Root.indexed root then "Mp" else "M"
    | Mu_alloc -> "A"

  let is_framed = function
    | Mu_raw(_,root) | Mu_mem(_,root,_) -> Root.framed root
    | Mu_alloc -> false

  let pretty fmt mu = Format.pp_print_string fmt (basename_of_chunk mu)

end

module Heap =
struct
  include Qed.Collection.Make(Chunk)
  let empty = Set.empty
  let of_raw r rt = Set.singleton (Mu_raw(r,rt))
  let of_mem r rt v = Set.singleton (Mu_mem(r,rt,v))

  let rec of_region map r =
    match Region.chunk map r with
    | Mref _ -> Set.empty
    | Mraw(rt,_) -> of_raw r rt
    | Mmem(rt,v) -> of_mem r rt v
    | Mcomp(_,overlay) -> of_overlay map overlay

  and of_range map { reg } = of_region map reg

  and of_overlay map ovl =
    Qed.Hcons.fold_list Set.union (of_range map) empty ovl

end
module Sigma = Sigma.Make(Chunk)(Heap)

type sigma = Sigma.t
type domain = Sigma.domain

let domain _obj = function
  | GarbledMix | Index _ -> error "Can not compute Garbled-mix domain"
  | Lref _ -> Heap.empty
  | Lraw(r,_,rt,_) -> Heap.of_raw r rt
  | Lmem(r,_,rt,v) -> Heap.of_mem r rt v
  | Lfld(_,_,_,ovl) -> Heap.of_overlay (map()) ovl
  | Larr(r,_,_,_,_,_) -> Heap.of_region (map()) r

let region_of_loc = function
  | (GarbledMix | Index _) as l -> error "Can not find region of %a" pretty l
  | Lref(r,_,_) | Lraw(r,_,_,_) | Lmem(r,_,_,_)
  | Lfld(r,_,_,_) | Larr(r,_,_,_,_,_) -> r

(* -------------------------------------------------------------------------- *)
(* ---  Loader                                                            --- *)
(* -------------------------------------------------------------------------- *)

module MODEL =
struct

  module Chunk = Chunk
  module Sigma = Sigma
  let name = "MemRegion.LOADER"
  type nonrec loc = loc
  let field = field
  let shift = shift
  let sizeof = Ctypes.bits_sizeof_object
  let domain = domain
  let frames _ _ _ = []

  let to_addr l = a_addrof (pointer_val l)
  let to_region_pointer l = Region.id (region_of_loc l) , pointer_val l
  let of_region_pointer r obj l =
    let map = map () in
    index map (Region.region map r) l F.e_zero (Ctypes.bits_sizeof_object obj)

  let load_mem sigma r rt v l =
    let m = Sigma.value sigma (Mu_mem(r,rt,v)) in
    if Root.indexed rt then F.e_get m (a_addrof l) else m

  let load_int sigma i = function
    | Lmem(r,l,rt,(Int i0 as v)) when i = i0 -> load_mem sigma r rt v l
    | l -> error "Can not load %a value from %a" Ctypes.pp_int i pretty l

  let load_float sigma f = function
    | Lmem(r,l,rt,(Float f0 as v)) when f = f0 -> load_mem sigma r rt v l
    | l -> error "Can not load %a value from %a" Ctypes.pp_float f pretty l

  let load_pointer sigma ty = function
    | Lmem(r,l,rt,(Pointer r' as v)) ->
        loc_of_index r' ty (load_mem sigma r rt v l)
    | Lref(_,l,r') ->
        loc_of_index r' ty (l_index_ref l)
    | l -> error "Can not load pointer value from %a" pretty l

  let havoc obj loc ~length (chunk:chunk) ~fresh ~current =
    match chunk with
    | Mu_alloc -> fresh
    | Mu_raw _ -> error "Can not havoc raw memories"
    | Mu_mem(_,root,_) ->
        if Layout.Root.indexed root then
          let addr = to_addr loc in
          let offset = F.e_fact (Ctypes.bits_sizeof_object obj) length in
          F.e_fun MemMemory.f_havoc [fresh;current;addr;offset]
        else
          fresh

  let eqmem obj loc chunk m1 m2 =
    match chunk with
    | Mu_alloc -> error "Can not compare allocation tables"
    | Mu_raw _ -> error "Can not compare raw memories"
    | Mu_mem(_,root,_) ->
        if Layout.Root.indexed root then
          let addr = to_addr loc in
          let offset = F.e_int (Ctypes.bits_sizeof_object obj) in
          F.p_call MemMemory.p_eqmem [m1;m2;addr;offset]
        else F.p_equal m1 m2

  let eqmem_forall obj loc chunk m1 m2 =
    match chunk with
    | Mu_alloc -> error "Can not compare allocation tables"
    | Mu_raw _ -> error "Can not compare raw memories"
    | Mu_mem(_,root,_) ->
        if Layout.Root.indexed root then
          let xp = Lang.freshvar ~basename:"p" t_addr in
          let p = F.e_var xp in
          let a = to_addr loc in
          let n = F.e_int (Ctypes.bits_sizeof_object obj) in
          let separated = p_separated p F.e_one a n in
          let equal = F.p_equal (F.e_get m1 p) (F.e_get m2 p) in
          [xp],separated,equal
        else [],F.p_true,F.p_equal m1 m2

  let last _ = error "Can not compute last valid index"

  let store_mem sigma r rt v l value =
    let c = Mu_mem(r,rt,v) in
    if Root.indexed rt then
      c , F.e_set (Sigma.value sigma c) (a_addrof l) value
    else c , value

  let store_int sigma i loc value =
    match loc with
    | Lmem(r,l,rt,(Int i0 as v)) when i = i0 -> store_mem sigma r rt v l value
    | _ -> error "Can not store %a value into %a" Ctypes.pp_int i pretty loc

  let store_float sigma f loc value =
    match loc with
    | Lmem(r,l,rt,(Float f0 as v)) when f = f0 -> store_mem sigma r rt v l value
    | _ -> error "Can not store %a value into %a" Ctypes.pp_float f pretty loc

  let store_pointer sigma _ty loc value =
    match loc with
    | Lmem(r,l,rt,(Pointer _ as v)) -> store_mem sigma r rt v l value
    | _ -> error "Can not store pointer values into %a" pretty loc

end

module LOADER = MemLoader.Make(MODEL)

let load = LOADER.load
let loadvalue = LOADER.loadvalue

let stored = LOADER.stored
let copied = LOADER.copied
let assigned = LOADER.assigned

(* -------------------------------------------------------------------------- *)
(* ---  Loc Segments                                                      --- *)
(* -------------------------------------------------------------------------- *)

type segment = loc rloc

let region_of_sloc = function Rloc(_,l) | Rrange(l,_,_,_) -> region_of_loc l

let disjoint_region s1 s2 =
  let map = map () in
  let c1 = Region.chunks map (region_of_sloc s1) in
  let c2 = Region.chunks map (region_of_sloc s2) in
  not (Qed.Intset.intersect c1 c2)


let addrof = MODEL.to_addr
let sizeof = Ctypes.bits_sizeof_object

let included s1 s2 =
  if disjoint_region s1 s2 then F.p_false else
    MemMemory.included ~shift ~addrof ~sizeof s1 s2

let separated s1 s2 =
  if disjoint_region s1 s2 then F.p_true else
    MemMemory.separated ~shift ~addrof ~sizeof s1 s2

(* -------------------------------------------------------------------------- *)
(* ---  TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO  --- *)
(* -------------------------------------------------------------------------- *)

type state = unit

let state _ = ()
let iter _ _ = ()
let lookup _ _ = Mterm
let updates _ _ = Bag.empty
let apply _ _ = ()

let literal ~eid _ = ignore eid ; GarbledMix

let base_addr _l = GarbledMix
let base_offset l = MemMemory.a_offset (addrof l)
let block_length _s _obj _l = F.e_zero

let cast _ _l = GarbledMix
let loc_of_int _ _ = GarbledMix
let int_of_loc _ _ = F.e_zero

let not_yet_pointer () = error "Pointer comparison not yet implemented"

let is_null _ = not_yet_pointer ()
let loc_eq _ _ = not_yet_pointer ()
let loc_lt _ _ = not_yet_pointer ()
let loc_leq _ _ = not_yet_pointer ()
let loc_neq _ _ = not_yet_pointer ()
let loc_diff _ _ _ = not_yet_pointer ()

let frame _sigma = []
let alloc sigma _xs = sigma
let scope _seq _s _xs = []
let valid _sigma _acs _l = error "Validity not yet implemented"
let invalid _sigma _l = error "Validity not yet implemented"
let global _sigma _p = F.p_true

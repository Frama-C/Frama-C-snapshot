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

open Cil_datatype
open Layout

module Wp = Wp_parameters

(* -------------------------------------------------------------------------- *)
(* --- Access Maps                                                        --- *)
(* -------------------------------------------------------------------------- *)

module Vmap = Varinfo.Map
module Smap = Datatype.String.Map
module Rmap = Qed.Intmap
module Rset = Qed.Intset
module Dmap = Qed.Listmap.Make(Offset)
module Dset = Qed.Listset.Make(Deref)
module Acs = Qed.Listset.Make(Lvalue)
module Class = Qed.Listset.Make(Datatype.String)
module Ranks = Qed.Listset.Make(Datatype.Int)

type region = {
  id : int ;
  mutable garbled : bool ;
  mutable rw : bool ;
  mutable pack : bool ;
  mutable flat : bool ;
  mutable names : Class.t ;
  mutable alias : alias ;
  mutable delta : int Dmap.t ;
  mutable deref : Dset.t ;
  mutable read : Acs.t ;
  mutable written : Acs.t ;
  mutable shifted : Acs.t ;
  mutable copiedTo : Rset.t ; (* copies to *)
  mutable pointsTo : int option ;
}

type map = {
  cache : Offset.cache ;
  queue : int Queue.t ;
  mutable rid : int ;
  mutable vars : int Vmap.t ;
  mutable return : int ; (* -1 when undefined *)
  mutable strings : (int * string) Rmap.t ; (* eid -> rid *)
  mutable index : int Smap.t ;
  mutable region : region Rmap.t ;
  mutable aliasing : int Rmap.t ;
  mutable cluster : region cluster Rmap.t ;
  mutable roots : root Rmap.t ;
  mutable froms : region from list Rmap.t ;
  mutable mranks : Ranks.t Rmap.t ; (* set of sizeof(ds) accessed by shifting *)
  mutable mdims : int list Rmap.t ; (* common dim prefix accessed from cluster *)
  mutable domain : Rset.t ; (* reachable regions via clusters *)
  mutable chunk : region chunk Rmap.t ; (* memory chunks *)
}

let create () = {
  rid = 0 ;
  return = (-1) ;
  cache = Offset.cache () ;
  vars = Vmap.empty ;
  strings = Rmap.empty ;
  index = Smap.empty ;
  region = Rmap.empty ;
  aliasing = Rmap.empty ;
  queue = Queue.create () ;
  cluster = Rmap.empty ;
  roots = Rmap.empty ;
  froms = Rmap.empty ;
  mranks = Rmap.empty ;
  mdims = Rmap.empty ;
  domain = Rset.empty ;
  chunk = Rmap.empty ;
}

let noid = 0
let is_empty map = (map.rid = 0)

let fresh map =
  let id = map.rid in
  map.rid <- succ id ;
  let region = {
    id ;
    garbled = false ;
    rw = RW.default () ;
    flat = Flat.default () ;
    pack = Pack.default () ;
    names = [] ;
    alias = NotUsed ;
    delta = Dmap.empty ;
    deref = Dset.empty ;
    read = Acs.empty ;
    written = Acs.empty ;
    shifted = Acs.empty ;
    copiedTo = Rset.empty ;
    pointsTo = None ;
  } in
  map.region <- Rmap.add id region map.region ;
  region

(* -------------------------------------------------------------------------- *)
(* --- Datatype                                                           --- *)
(* -------------------------------------------------------------------------- *)

module R =
struct
  type t = region
  let id a = a.id
  let equal a b = (a.id = b.id)
  let compare a b = Pervasives.compare a.id b.id
  let pp_rid fmt id = Format.fprintf fmt "R%03d" id
  let pretty fmt r = pp_rid fmt r.id
end

module Map = Qed.Idxmap.Make(R)
module Set = Qed.Idxset.Make(R)

(* -------------------------------------------------------------------------- *)
(* --- Union Find                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec aliasing map i =
  try
    let j = aliasing map (Rmap.find i map.aliasing) in
    if j <> i then map.aliasing <- Rmap.add i j map.aliasing ; j
  with Not_found -> i

let linkto map i k =
  if i <> k then
    begin
      map.aliasing <- Rmap.add i k map.aliasing ;
      Queue.add i map.queue ;
    end

let region map r =
  try Rmap.find (aliasing map r) map.region
  with Not_found -> failwith "Wp.Region: Undefined Region"

let join_classes map i j =
  let k = min i j in (linkto map i k ; linkto map j k ; k)

let join_id map i j =
  let i = aliasing map i in
  let j = aliasing map j in
  if i = j then i else join_classes map i j

let join_region map ra rb =
  let i = aliasing map ra.id in
  let j = aliasing map rb.id in
  let k = join_classes map i j in
  if k = i then ra else
  if k = j then rb else
    (* defensive *) region map k

(* -------------------------------------------------------------------------- *)
(* --- Aliasing                                                           --- *)
(* -------------------------------------------------------------------------- *)

let alias map a b =
  let k = join_id map a.id b.id in
  let r = region map k in
  r.alias <- Aliased ; r

let do_alias map a b = ignore (alias map a b)

let add_alias map ~into:a b =
  let i = aliasing map a.id in
  let j = aliasing map b.id in
  let wa = (region map i).alias in
  let wb = (region map j).alias in
  let k = join_classes map i j in
  (* Aliasing has changed *)
  (region map k).alias <- Alias.alias wa (Alias.use wb)

let get_merged map r =
  let i = aliasing map r.id in
  if i <> r.id then Some (region map i) else None

let get_alias map r =
  let i = aliasing map r.id in
  if i <> r.id then region map i else r

let eq_alias map a b = (aliasing map a.id = aliasing map b.id)

(* -------------------------------------------------------------------------- *)
(* --- General Iterator                                                   --- *)
(* -------------------------------------------------------------------------- *)

let once mark r =
  if Rset.mem r.id !mark then false
  else ( mark := Rset.add r.id !mark ; true )

let iter map f =
  let do_once marks f r = if once marks r then f r else () in
  Rmap.iter (do_once (ref Rset.empty) f) map.region

(* -------------------------------------------------------------------------- *)
(* --- Region Accessor                                                    --- *)
(* -------------------------------------------------------------------------- *)

let id reg = reg.id
let is_garbled reg = reg.garbled
let has_pointed reg = reg.pointsTo <> None
let has_deref reg = not (Dset.is_empty reg.deref)
let has_layout reg = not (Dmap.is_empty reg.delta)
let has_offset reg d = Dmap.mem d reg.delta
let iter_offset map f reg =
  Dmap.iter (fun ofs r -> f ofs (region map r)) reg.delta

let has_copies reg = not (Rset.is_empty reg.copiedTo)
let iter_copies map f reg =
  Rset.iter (fun r -> f (region map r)) reg.copiedTo

let add_offset map reg d =
  try region map (Dmap.find d reg.delta)
  with Not_found ->
    let rd = fresh map in
    reg.delta <- Dmap.add d rd.id reg.delta ; rd

let add_pointed map reg =
  match reg.pointsTo with
  | Some k -> region map k
  | None ->
      let r = fresh map in
      reg.pointsTo <- Some r.id ; r

let get_addrof map reg =
  let addr = fresh map in
  addr.pointsTo <- Some reg.id ; addr

let get_pointed map reg =
  match reg.pointsTo with
  | None -> None
  | Some r -> Some (region map r)

let get_offset map reg d =
  try Some (region map (Dmap.find d reg.delta))
  with Not_found -> None

let get_copies map reg =
  List.map (region map) (Rset.elements reg.copiedTo)

(* -------------------------------------------------------------------------- *)
(* --- Access                                                             --- *)
(* -------------------------------------------------------------------------- *)

let acs_read rg lvalue = rg.read <- Acs.add lvalue rg.read
let acs_write rg lvalue = rg.written <- Acs.add lvalue rg.written
let acs_shift rg lvalue = rg.shifted <- Acs.add lvalue rg.shifted
let acs_deref rg deref = rg.deref <- Dset.add deref rg.deref
let acs_copy ~src ~tgt =
  if tgt.id <> src.id then src.copiedTo <- Rset.add tgt.id src.copiedTo

let iter_read f rg = Acs.iter f rg.read
let iter_write f rg = Acs.iter f rg.written
let iter_shift f rg = Acs.iter f rg.shifted
let iter_deref f rg = Dset.iter f rg.deref

let is_read rg = not (Acs.is_empty rg.read)
let is_written rg = not (Acs.is_empty rg.written)
let is_shifted rg = not (Acs.is_empty rg.shifted)
let is_aliased rg = Alias.is_aliased rg.alias

(* -------------------------------------------------------------------------- *)
(* --- Varinfo Index                                                      --- *)
(* -------------------------------------------------------------------------- *)

let rvar map x r =
  let reg = region map r in
  if reg.id <> r then map.vars <- Vmap.add x reg.id map.vars ; reg

let of_null map = fresh map (* A fresh region each time: polymorphic *)

let of_cvar map x =
  try rvar map x (Vmap.find x map.vars)
  with Not_found ->
    let reg = fresh map in
    map.vars <- Vmap.add x reg.id map.vars ; reg

let of_return map =
  if map.return < 0 then
    let reg = fresh map in
    map.return <- reg.id ; reg
  else
    region map map.return

let has_return map = 0 <= map.return

let iter_vars map f = Vmap.iter (fun x r -> f x (rvar map x r)) map.vars

(* -------------------------------------------------------------------------- *)
(* --- Field Info Index                                                   --- *)
(* -------------------------------------------------------------------------- *)

let field_offset map fd = Offset.field_offset map.cache fd

(* -------------------------------------------------------------------------- *)
(* --- String Literal Index                                               --- *)
(* -------------------------------------------------------------------------- *)

let of_cstring map ~eid ~cst =
  try region map (fst @@ Rmap.find eid map.strings)
  with Not_found ->
    let reg = fresh map in
    map.strings <- Rmap.add eid (reg.id,cst) map.strings ; reg

let iter_strings map f =
  Rmap.iter (fun (rid,cst) -> f (region map rid) cst) map.strings

(* -------------------------------------------------------------------------- *)
(* --- Region Index                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rindex map a r =
  let reg = region map r in
  if reg.id <> r then map.index <- Smap.add a reg.id map.index ; reg

let of_name map a =
  try rindex map a (Smap.find a map.index)
  with Not_found ->
    let reg = fresh map in
    reg.names <- [a] ;
    map.index <- Smap.add a reg.id map.index ; reg

let of_class map = function
  | None -> fresh map
  | Some a -> of_name map a

let has_names reg = not (Class.is_empty reg.names)
let iter_names map f = Smap.iter (fun a r -> f a (rindex map a r)) map.index

(* -------------------------------------------------------------------------- *)
(* --- Fusion                                                             --- *)
(* -------------------------------------------------------------------------- *)

let merge_pointed map u v =
  match u,v with
  | None , w | w , None -> w
  | Some i , Some j -> Some (join_id map i j)

let merge_delta map _d a b = join_id map a b

let merge_region map ~id a b =
  {
    id ;
    garbled = a.garbled || b.garbled ;
    rw = RW.merge a.rw b.rw ;
    flat = Flat.merge a.flat b.flat ;
    pack = Pack.merge a.pack b.pack ;
    alias = Alias.merge a.alias b.alias ;
    names = Class.union a.names b.names ;
    read = Acs.union a.read b.read ;
    written = Acs.union a.written b.written ;
    shifted = Acs.union a.shifted b.shifted ;
    copiedTo = Rset.union a.copiedTo b.copiedTo ;
    pointsTo = merge_pointed map a.pointsTo b.pointsTo ;
    delta = Dmap.union (merge_delta map) a.delta b.delta ;
    deref = Dset.union a.deref b.deref ;
  }

let fusion map =
  while not (Queue.is_empty map.queue) do
    let i = Queue.pop map.queue in
    let j = aliasing map i in
    if i <> j then
      begin
        if not (Wp.Region_fixpoint.get ()) then
          Wp.debug "Region %a -> %a" R.pp_rid i R.pp_rid j ;
        let a = try Rmap.find i map.region with Not_found -> assert false in
        let b = try Rmap.find j map.region with Not_found -> assert false in
        assert (i = a.id) ;
        assert (j = b.id ) ;
        let c = merge_region map ~id:j a b in
        map.region <- Rmap.add j c (Rmap.remove i map.region) ;
      end
  done

let fusionned map = not (Queue.is_empty map.queue)
let iter_fusion map f = Queue.iter (fun i -> f i (region map i)) map.queue

(* -------------------------------------------------------------------------- *)
(* --- Garbling                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec garblify map reg =
  if not reg.garbled then
    begin
      reg.garbled <- true ;
      Dmap.iter
        (fun _delta r ->
           garblify map (region map r) ;
           ignore (join_id map reg.id r) ;
        ) reg.delta ;
      reg.delta <- Dmap.empty ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Clustering                                                         --- *)
(* -------------------------------------------------------------------------- *)

let cluster map reg =
  try Rmap.find reg.id map.cluster
  with Not_found -> Layout.Empty

module Cluster =
struct
  open Layout

  let rec from_region map reg =
    try Rmap.find reg.id map.cluster
    with Not_found ->
      if reg.garbled then Garbled else
      if not (Wp.Region_cluster.get ()) then Empty else
        begin
          map.cluster <- Rmap.add reg.id Empty map.cluster ;
          let mu ~raw ra rb =
            if raw then
              begin
                garblify map ra ;
                garblify map rb ;
              end ;
            join_region map ra rb
          in
          let cluster =
            if has_layout reg then
              Cluster.reshape ~eq:R.equal ~flat:reg.flat ~pack:reg.pack @@
              from_layout map mu reg
            else
              from_deref map mu reg
          in
          if cluster = Garbled then garblify map reg ;
          map.cluster <- Rmap.add reg.id cluster map.cluster ;
          cluster
        end

  and from_deref map mu reg =
    let pointed = lazy (add_pointed map reg) in
    List.fold_left
      (fun chunk deref ->
         Cluster.merge R.pretty mu chunk (Cluster.deref ~pointed deref)
      ) Empty reg.deref

  and from_layout map mu reg =
    Dmap.fold
      (fun offset tgt acc ->
         let layout = shift map offset (region map tgt) in
         Cluster.merge R.pretty mu (Layout layout) acc
      ) reg.delta Empty

  and shift map offset target =
    let inline = Wp.Region_inline.get () || not (is_aliased target) in
    let cluster = from_region map target in
    Cluster.shift map.cache R.pretty offset target ~inline cluster

  let compute map reg =
    begin
      if has_layout reg && has_deref reg then
        begin
          Dset.iter
            (fun deref ->
               let target = add_offset map reg (Index(snd deref,1)) in
               target.read <- Acs.union reg.read target.read ;
               target.written <- Acs.union reg.written target.written ;
               acs_deref target deref
            ) reg.deref ;
          reg.deref <- Dset.empty ;
          reg.read <- Acs.empty ;
          reg.written <- Acs.empty ;
          Queue.add reg.id map.queue ;
        end ;
      ignore (from_region map reg) ;
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Froms Analysis                                                     --- *)
(* -------------------------------------------------------------------------- *)

let get_froms map reg =
  try Rmap.find reg.id map.froms
  with Not_found -> []

let add_from map ~from ~target =
  let rs = get_froms map target in
  map.froms <- Rmap.add target.id (from :: rs) map.froms

module Froms =
struct
  open Layout

  let rec forward map marks ~source ~from ~target =
    map.domain <- Rset.add source.id map.domain ;
    add_from map ~from ~target ;
    if once marks target then add_region map marks target

  and add_region map marks reg =
    begin
      add_points_to map marks ~source:reg reg.pointsTo ;
      add_cluster map marks ~source:reg (cluster map reg) ;
    end

  and add_points_to map marks ~source = function
    | None -> ()
    | Some p -> add_deref map marks ~source ~target:(region map p)

  and add_deref map marks ~source ~target =
    let from = if is_shifted target then Farray source else Fderef source in
    forward map marks ~source ~from ~target

  and add_cluster map marks ~source = function
    | Empty | Garbled | Chunk (Int _ | Float _) -> ()
    | Chunk (Pointer target) -> add_deref map marks ~source ~target
    | Layout { layout } -> List.iter (add_range map marks ~source) layout

  and add_range map marks ~source = function
    | { ofs ; reg = target ; dim = Dim(_,[]) } ->
        forward map marks ~source ~from:(Ffield(source,ofs)) ~target
    | { reg = target } ->
        forward map marks ~source ~from:(Findex source) ~target

end

(* -------------------------------------------------------------------------- *)
(* --- Roots Analysis                                                     --- *)
(* -------------------------------------------------------------------------- *)

let get_roots map reg =
  try Rmap.find reg.id map.roots
  with Not_found -> Rnone

let has_roots map reg = get_roots map reg <> Rnone

module Roots =
struct

  let rec of_region map region =
    try Rmap.find region.id map.roots
    with Not_found ->
      let froms = get_froms map region in
      let roots =
        List.fold_left
          (fun roots from ->
             Root.merge roots (Root.from ~root:(of_region map) from)
          ) Rnone froms
      in map.roots <- Rmap.add region.id roots map.roots ; roots

  let compute map reg = ignore (of_region map reg)

end

(* -------------------------------------------------------------------------- *)
(* --- Forward & Backward Propagation                                     --- *)
(* -------------------------------------------------------------------------- *)

let forward map =
  begin
    let marks = ref Rset.empty in
    map.domain <- Rset.empty ;
    Vmap.iter
      (fun x r ->
         let reg = region map r in
         let open Cil_types in
         if x.vglob || x.vformal then
           add_from map ~from:(Fvar x) ~target:(region map r) ;
         Froms.add_region map marks reg ;
      ) map.vars ;
  end

let backward map =
  begin
    Rmap.iter (Roots.compute map) map.region ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Chunk Analysis                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec chunk map region =
  try Rmap.find region.id map.chunk
  with Not_found ->
    let roots = get_roots map region in
    let chunk =
      match cluster map region with
      | Empty | Garbled -> Mraw (roots,get_pointed map region)
      | Chunk v ->
          if is_read region || is_written region then
            Mmem(roots,v)
          else
            begin match v with
              | Pointer r -> Mref r
              | _ -> Mraw (roots,get_pointed map region)
            end
      | Layout { layout } ->
          let chunks = Chunk.union_map (fun { reg } -> chunks map reg) layout
          in Mcomp(chunks,layout)

    in map.chunk <- Rmap.add region.id chunk map.chunk ; chunk

and chunks map region =
  match chunk map region with
  | Mcomp(rs,_) -> rs
  | _ -> Chunk.singleton region.id

(* -------------------------------------------------------------------------- *)
(* --- Fixpoint                                                           --- *)
(* -------------------------------------------------------------------------- *)

let fixpoint map =
  begin
    let turn = ref 0 in
    let loop = ref true in
    while !loop do
      incr turn ;
      Wp.feedback ~ontty:`Transient "Region clustering (loop #%d)" !turn ;
      fusion map ;
      map.cluster <- Rmap.empty ;
      iter map (Cluster.compute map) ;
      loop := fusionned map ;
    done ;
    Wp.feedback ~ontty:`Transient "Region forward analysis" ;
    forward map ;
    Wp.feedback ~ontty:`Transient "Region backward analysis" ;
    backward map ;
    Wp.feedback ~ontty:`Transient "Region fixpoint reached" ;
  end

(* -------------------------------------------------------------------------- *)

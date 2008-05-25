(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** *)

open Cil_types

(** this one shouldn't occur, but... *)
exception Pdg_Internal_Error of string

module Elem = struct
  type key = PdgIndex.Key.t

  type t = { id : int; key : key }

  let make num key =
    { id = num; key = key }

  let compare e1 e2 = Pervasives.compare e1.id e2.id
  let hash e = e.id
  let equal e1 e2 = e1.id = e2.id

  let key e = e.key

  let print_id fmt e =
     Format.fprintf fmt "%d" e.id

  (* for DataState : only print the id. *)
  let pretty = print_id

end

(** Edges label for the Program Dependence Graph.
  *)
module Dpd : sig
  type t

  (** used to speak about the different kinds of dependencies *)
  type td =  Ctrl | Addr | Data

  val make_adc : bool -> bool -> bool -> t
  val make : td -> t
  val make_addr : t
  val make_ctrl : t
  val make_data : t
  val bottom : t
  val top : t

  (** = bottom but needed to build the Graph *)
  val default : t

  val adc_value : t -> bool * bool * bool
  val is_addr : t -> bool
  val is_ctrl : t -> bool
  val is_data : t -> bool
  val is_dpd : td -> t -> bool
  val is_bottom : t -> bool

  (** total order. Used only to sort...*)
  val compare : t -> t -> int

  val is_included : t -> t -> bool
  val combine : t -> t -> t
  val add : t -> td -> t
  val inter : t -> t -> t
  val intersect : t -> t -> bool

  (** remove the flags that are in m2 for m1 *)
  val minus : t -> t -> t

  val pretty : Format.formatter -> t -> unit
  end
= struct
  type t = {addr : bool; data: bool; ctrl:bool }

  type td = Ctrl | Addr | Data

  let make_adc a d c = { addr = a; data = d; ctrl = c }
  let bottom   = make_adc false false false
  let top   = make_adc true true true
  let make_addr = make_adc true false false
  let make_ctrl = make_adc false false true
  let make_data = make_adc false true false
  let make kind = match kind with
  | Ctrl -> make_ctrl | Addr -> make_addr | Data -> make_data

  let default = bottom

  let is_addr d = d.addr
  let is_ctrl d = d.ctrl
  let is_data d = d.data
  let is_dpd tdpd d = match tdpd with
  | Addr -> d.addr
  | Ctrl -> d.ctrl
  | Data -> d.data
  let is_bottom d = (d = bottom)


  let adc_value d = (is_addr d, is_data d, is_ctrl d)

  let compare l1 l2 = Pervasives.compare l1 l2

  let combine d1 d2 = { addr = d1.addr || d2.addr;
                        ctrl = d1.ctrl || d2.ctrl;
                        data = d1.data || d2.data }
  let inter d1 d2 = { addr = d1.addr && d2.addr;
                        ctrl = d1.ctrl && d2.ctrl;
                        data = d1.data && d2.data }

  let is_included d1 d2 = let d = combine d1 d2 in d = d2
  let intersect d1 d2 = let d = inter d1 d2 in not (is_bottom d)

  let add d kind = combine d (make kind)

  let minus adc1 adc2 =
    let a1, d1, c1 = adc_value adc1 in
    let a2, d2, c2 = adc_value adc2 in
    let a1 = if a2 then false else a1 in
    let d1 = if d2 then false else d1 in
    let c1 = if c2 then false else c1 in
      make_adc a1 d1 c1

  let pretty fmt d =  Format.fprintf fmt "[%c%c%c]"
                        (if is_addr d then 'a' else '-')
                        (if is_ctrl d then 'c' else '-')
                        (if is_data d then 'd' else '-')

end


(** The graph itself.
* It uses ocamlgraph
* {{:http://ocamlgraph.lri.fr/doc/Imperative.S.AbstractLabeled.html}Graph.Imperative.Digraph}.
* @see <http://www.lri.fr/~filliatr/ocamlgraph/> ocamlgraph web site
*)
module G = struct
  include Graph.Imperative.Digraph.AbstractLabeled(Elem)(Dpd)

  (* added for compatibility between ocamlgraph 0.98 and 0.99 in which
     create has an optional argument
   *)
  let create () = create ()

  let find_dpd graph v1 v2 =
    let edge = find_edge graph v1 v2  in (edge, E.label edge)

  let add_elem graph key =
    let elem = Elem.make (nb_vertex graph) key in
    let new_vertex = V.create elem in
    let _ = add_vertex graph new_vertex in
      new_vertex

  let simple_add_dpd graph v1 dpd v2 =
    add_edge_e graph (E.create v1 dpd v2)

  let replace_dpd graph edge new_dpd =
    let v1 = E.src edge in let v2 = E.dst edge in
    remove_edge_e graph edge; simple_add_dpd graph v1 new_dpd v2

  let add_dpd graph v1 dpd_kind v2 =
  try
    let edge, old_dpd = find_dpd graph v1 v2 in
    let new_dpd = Dpd.add old_dpd dpd_kind in
      if (Dpd.compare old_dpd new_dpd) <> 0 then
        replace_dpd graph edge new_dpd
  with Not_found ->
    let new_dpd = Dpd.make dpd_kind in simple_add_dpd graph v1 new_dpd v2

  let remove_elem graph v =
    iter_pred_e ( fun e -> remove_edge_e graph e ) graph v ;
    iter_succ_e ( fun e -> remove_edge_e graph e ) graph v ;
    remove_vertex graph v


end

(** Node.t is the type of the PDG vertex.
   [compare] and [pretty] are needed by [Abstract_interp.Make_Lattice_Set]. *)
module Node = struct
  type t = G.V.t
  let add_simple_node graph key =
    let elem = Elem.make (G.nb_vertex graph) key in
    let new_vertex = G.V.create elem in
      new_vertex
  let compare = G.V.compare
  let elem n = G.V.label n
  let equal n1 n2 = Elem.equal (elem n1) (elem n2)
  let hash = G.V.hash
  let elem_id n = (elem n).Elem.id
  let elem_key n = Elem.key (elem n)
  let stmt n = PdgIndex.Key.stmt (elem_key n)

  (** tells if the node represent the same thing that the given key. *)
  let equivalent n key = (elem_key n) = key

  let print_id fmt n = Elem.print_id fmt (elem n)

  (** needed by NodeSetLattice in DataState, but impossible to write here for
 * dependence problem. See PrintPdg.pretty_node if needed. *)
  let pretty = print_id

  module Datatype = 
    Project.Datatype.Imperative
      (struct type t = G.V.t let copy _ = assert false (* TODO *) end)

  let pretty_list fmt l = 
    List.iter (fun n -> Format.fprintf fmt " %a" pretty n) l

end

module NodeSet = struct
  include Set.Make (Node)

  let add_list ?(set=empty) l = 
    List.fold_left (fun acc n -> add n acc) set l
end

(** set of nodes of the graph *)
module NodeSetLattice = struct
  include Abstract_interp.Make_Lattice_Set (Node)
  exception NoNodeForZone of Locations.Zone.t
  type t_elt = O.elt
  let tag = hash
  let default _v _a _b : t = empty
    (* raise (NoNodeForZone (Locations.Zone.default v a b)) *)
  let defaultall _v : t = empty
    (* raise (NoNodeForZone (Locations.Zone.defaultall v)) *)
end

module LocInfo = Lmap_bitwise.Make_bitwise (NodeSetLattice)

(** DataState is associated with a program point
    and provide the dependancies for the data,
    ie. it stores for each location the nodes of the pdg where its value
    was last defined.
    Managed in src/pdg/state.ml
  *)
type t_data_state =
  { loc_info : LocInfo.t ; under_outputs : Locations.Zone.t }


(** Dynamic dependencies *)
module DynDpds : sig
  type t
  type t_node = Node.t
  type t_dpds_list = t_node list
  type t_dpds_lists

  val empty : t
  val add_x_dpds : t -> t_node ->
                   data:t_dpds_list -> addr:t_dpds_list -> ctrl:t_dpds_list ->
                   unit
  val clear : t -> unit

  val find_dpds : t -> t_node -> t_dpds_lists
  val find_co_dpds : t -> t_node -> t_dpds_lists
  val get_all_dpds : t_dpds_lists -> t_dpds_list
  val get_x_dpds : t_dpds_lists -> Dpd.td -> t_dpds_list

  val iter_dpds : (G.E.t -> unit) -> t -> unit

end = struct
  type t_node = Node.t
  type t_dpds_list = t_node list

  (** [DAC] order ie. data + addr + ctrl *)
  type t_dpds_lists = t_dpds_list * t_dpds_list * t_dpds_list
  (** the node and its dependencies and codependencies lists *)
  type t = (t_node * t_dpds_lists * t_dpds_lists) Inthash.t

  let empty = Inthash.create 100

  let is_empty dd = (Inthash.length dd) = 0

  let clear dd = Inthash.clear dd

  let find_lists dd node =
    let _n, dpds, codpds = Inthash.find dd (Node.elem_id node) in dpds, codpds

  let find_dpds dd node = let (dpds, _) = find_lists dd node in dpds

  let find_co_dpds dd node = let (_, codpds) = find_lists dd node in codpds

  let iter_dpds f dd =
    let rec iter n tdpd ldpds = match ldpds with [] -> ()
      | d :: ldpds -> f (G.E.create n tdpd d); iter n tdpd ldpds
    in
    let do_f _n_id (n, (ldata, laddr, lctrl), _codpds) =
      iter n Dpd.make_data ldata;
      iter n Dpd.make_addr laddr;
      iter n Dpd.make_ctrl lctrl
    in Inthash.iter do_f dd

  let get_all_dpds (ldata, laddr, lctrl) = ldata @ laddr @ lctrl

  let get_x_dpds (ldata, laddr, lctrl) td =
    match td with
      | Dpd.Data -> ldata | Dpd.Addr -> laddr | Dpd.Ctrl -> lctrl

  (** keeps the list ordered from the smallest to the largest elem_id *)
      (* TODO : add_node_to_list for several nodes at a time *)
  let add_node_to_list node_list node =
    let n_id = Node.elem_id node in
    let rec add node_list =
      match node_list with
        | [] -> [node]
        | n :: tail ->
            if (Node.elem_id n) < n_id then n :: (add tail)
            else if n_id < (Node.elem_id n) then node :: node_list
            else (* already in *) node_list
    in add node_list

  let add_node_to_lists td lists node =
    let (ldata, laddr, lctrl) = lists in
    let lists = match td with
      | Dpd.Data -> add_node_to_list ldata node, laddr, lctrl
      | Dpd.Addr -> ldata, add_node_to_list laddr node, lctrl
      | Dpd.Ctrl -> ldata, laddr, add_node_to_list lctrl node
    in lists

  (** add the nodes in [data] [addr] [ctrl] to the [node] dependancies.
  * If [x] is a new dependency of [node], we also have to add [node] in [x]
  * codependencies *)
  let add_x_dpds dd node ~data ~addr ~ctrl =
    let add_codpd t x = (* add [node] in [x] codpds *)
      let x_dpds, x_codpds =
        try find_lists dd x with Not_found -> ([], [], []), ([], [], []) in
      let x_codpds = add_node_to_lists t x_codpds node in
        Inthash.replace dd (Node.elem_id x) (x, x_dpds, x_codpds)
    in
    let add t old_node_dpds new_dpd =
      add_codpd t new_dpd;
      add_node_to_lists t old_node_dpds new_dpd
    in
    let node_dpds, node_codpds =
      try find_lists dd node with Not_found -> ([], [], []), ([], [], []) in
    let node_dpds = List.fold_left (add Dpd.Data) node_dpds data in
    let node_dpds = List.fold_left (add Dpd.Addr) node_dpds addr in
    let node_dpds = List.fold_left (add Dpd.Ctrl) node_dpds ctrl in
      Inthash.replace dd (Node.elem_id node) (node, node_dpds, node_codpds)
end

(** PDG for a function *)
module Pdg = struct
  exception Top
  exception Bottom

  type t_index = (Node.t, unit) PdgIndex.FctIndex.t
                   (** The nodes which are associated the each element.
                     * There is only one node for simple statements,
                     * but there are several for a call for instance. *)
  type t_def = { var_fct : Cil_types.varinfo;
             graph : G.t ;
             states : t_data_state Inthash.t ;
             index : t_index ;
             dynamic_links : DynDpds.t ;
           }

  type t = PdgDef of t_def | PdgTop of string | PdgBottom of string

  (* [JS 2008/02/28] quite strange that it has ever worked because it used
     hashconsed values (at least Locations.Zone.t) *)
  module Datatype =
    Project.Datatype.Imperative
      (struct
	 type tt = t
	 type t = tt
	 let copy _ = assert false (* TODO *)
       end)

  let make vf graph states index =
    let pdg = { var_fct = vf; graph = graph; 
                states = states; index = index ;
                dynamic_links = DynDpds.empty ;
    } in PdgDef pdg

  let top fct_name = PdgTop fct_name
  let bottom fct_name = PdgBottom fct_name

  let is_top pdg = match pdg with PdgTop _ -> true | _ -> false
  let is_bottom pdg = match pdg with PdgBottom _ -> true | _ -> false

  let get_pdg pdg = match pdg with
    | PdgDef pdg -> pdg 
    | PdgTop _ -> raise Top 
    | PdgBottom _ -> raise Bottom

  let get_fct_name pdg = match pdg with
    | PdgTop name | PdgBottom name -> name
    | PdgDef pdg -> pdg.var_fct.vname
  let get_var_fct pdg = let pdg = get_pdg pdg in  pdg.var_fct
  let get_graph pdg = let pdg = get_pdg pdg in  pdg.graph
  let get_states pdg = let pdg = get_pdg pdg in  pdg.states
  let get_index pdg = let pdg = get_pdg pdg in  pdg.index

  let iter_nodes f pdg = G.iter_vertex f (get_graph pdg)

  let fold_call_nodes f acc pdg call =
    let _, call_pdg = PdgIndex.FctIndex.find_call (get_index pdg) call in
    let do_it acc (_k, n) = f acc n in
      PdgIndex.Signature.fold do_it acc call_pdg


  let add_dynamic_dpds pdg ?(data=[]) ?(addr=[]) ?(ctrl=[]) node =
    let pdg = get_pdg pdg in
    if G.mem_vertex pdg.graph node then
      let dd = pdg.dynamic_links in
        DynDpds.add_x_dpds dd node ~data ~addr ~ctrl
    else raise Not_found

  let iter_dynamic_dpds f pdg =
    let pdg = get_pdg pdg in
    DynDpds.iter_dpds f pdg.dynamic_links

  let clear_dynamic_dpds pdg =
    let pdg = get_pdg pdg in
    DynDpds.clear pdg.dynamic_links

  (* let remove_node pdg node = 
    G.remove_elem (get_graph pdg) node;
    PdgIndex.FctIndex.remove (get_index pdg) (Node.elem_key node) *)

  let get_all_direct ~co pdg node =
    let pdg = get_pdg pdg in
    let nodes = (if co then G.pred else G.succ) pdg.graph node in
    let dynamic_dpds =
      try
        let dyn_find = if co then DynDpds.find_co_dpds else DynDpds.find_dpds in
        let dpds = dyn_find pdg.dynamic_links node in
          DynDpds.get_all_dpds dpds
      with Not_found -> []
    in dynamic_dpds @ nodes

  let get_all_direct_dpds pdg node = get_all_direct ~co:false pdg node
  let get_all_direct_codpds pdg node = get_all_direct ~co:true pdg node

  (** gives the list of nodes that depend to the given node
      with a given kind of dependency.
    *)
  let get_x_direct ~co dpd_type pdg node =
    let pdg = get_pdg pdg in
    let filter edge_end edge nodes =
      if Dpd.is_dpd dpd_type (G.E.label edge)
      then (edge_end edge) :: nodes else nodes
    in
    let fold = if co then G.fold_pred_e else G.fold_succ_e  in
    let edge_end = if co then G.E.src else G.E.dst in
    let nodes = fold (filter edge_end) pdg.graph node [] in
    let dynamic_dpds =
      try
        let dyn_find = if co then DynDpds.find_co_dpds else DynDpds.find_dpds in
        let lists = dyn_find pdg.dynamic_links node in
        DynDpds.get_x_dpds lists dpd_type
      with Not_found -> []
    in dynamic_dpds @ nodes

  let get_x_direct_dpds = get_x_direct ~co:false
  let get_x_direct_codpds = get_x_direct ~co:true
end

module InternalPdg = Pdg

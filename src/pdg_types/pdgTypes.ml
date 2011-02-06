(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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

  type tt = { id : int; key : key }

  let make counter key =
    {id = (incr counter;
           if !counter = -1 then
             failwith "Internal limit reached in PDG counter";
           !counter);
     key = key}

  let key e = e.key
  let print_id fmt e = Format.fprintf fmt "%d" e.id

  include Datatype.Make
      (struct
	type t = tt
	let name = "PdgTypes.Elem"
	let structural_descr =
	  Structural_descr.t_record
	    [| Structural_descr.p_int; PdgIndex.Key.packed_descr |]
	let reprs = [ { id = -1; key = PdgIndex.Key.top_input } ]
	let compare e1 e2 = Datatype.Int.compare e1.id e2.id
	let hash e = e.id
	let equal e1 e2 = e1.id = e2.id
	let pretty = print_id
	let rehash = Datatype.identity
	let copy = Datatype.undefined
	let internal_pretty_code = Datatype.undefined
	let varname = Datatype.undefined
	let mem_project = Datatype.never_any_project
       end)

end

(** Edges label for the Program Dependence Graph.
  *)
module Dpd : sig
  type t

  (** used to speak about the different kinds of dependencies *)
  type td =  Ctrl | Addr | Data

  val make : ?a:bool -> ?d:bool -> ?c:bool -> unit -> t
  val make_simple : td -> t
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
  val equal : t -> t -> bool

  val is_included : t -> t -> bool
  val combine : t -> t -> t
  val add : t -> td -> t
  val inter : t -> t -> t
  val intersect : t -> t -> bool

  (** remove the flags that are in m2 for m1 *)
  val minus : t -> t -> t

  val pretty : Format.formatter -> t -> unit
  end
  =
struct

  type t = {addr : bool; data: bool; ctrl:bool }
  type td = Ctrl | Addr | Data


  (* internal constructor *)
  let create ?(a=false) ?(d=false) ?(c=false) _ =
    { addr = a; data = d; ctrl = c }

  (* all possible value for [t] *)
  let bottom = create ()
  let top    = create ~a:true ~d:true ~c:true ()
  let a_dpd  = create ~a:true ()
  let d_dpd  = create ~d:true ()
  let c_dpd  = create ~c:true ()
  let ad_dpd = create ~a:true ~d:true ()
  let ac_dpd = create ~a:true ~c:true ()
  let dc_dpd = create ~d:true ~c:true ()

  (* external constructor sharing identical [t] values *)
  let make ?(a=false) ?(d=false) ?(c=false) _ =
    match a,d,c with
      | false, false, false -> bottom
      | true,  false, false -> a_dpd
      | false, true,  false -> d_dpd
      | false, false, true  -> c_dpd
      | true,  true,  false -> ad_dpd
      | true,  false, true  -> ac_dpd
      | false, true,  true  -> dc_dpd
      | true,  true,  true  -> top

  (* the use the external constructor ensures [==] can be used instead of [=] *)
  let equal d1 d2 = d1 == d2

  let make_simple kind = match kind with
    | Ctrl -> c_dpd
    | Addr -> a_dpd
    | Data -> d_dpd

  let default = bottom

  let is_addr d = d.addr
  let is_ctrl d = d.ctrl
  let is_data d = d.data
  let is_dpd tdpd d = match tdpd with
    | Addr -> d.addr
    | Ctrl -> d.ctrl
    | Data -> d.data

  let is_bottom d = equal d bottom

  let adc_value d = (is_addr d, is_data d, is_ctrl d)

  let compare : t -> t -> int = Extlib.compare_basic

  let combine d1 d2 =
    if (d1 == d2) then d1
    else make
      ~a:(d1.addr || d2.addr)
      ~c:(d1.ctrl || d2.ctrl)
      ~d:(d1.data || d2.data) ()

  let inter d1 d2 =
    if (d1 == d2) then d1
    else make
      ~a:(d1.addr && d2.addr)
      ~c:(d1.ctrl && d2.ctrl)
      ~d:(d1.data && d2.data) ()

  let is_included d1 d2 = let d = combine d1 d2 in equal d d2
  let intersect d1 d2 = let d = inter d1 d2 in not (is_bottom d)

  let add d kind = combine d (make_simple kind)

  let minus adc1 adc2 =
    let a1, d1, c1 = adc_value adc1 in
    let a2, d2, c2 = adc_value adc2 in
    let a = if a2 then false else a1 in
    let d = if d2 then false else d1 in
    let c = if c2 then false else c1 in
      make ~a ~d ~c ()

  let pretty fmt d =  Format.fprintf fmt "[%c%c%c]"
                        (if is_addr d then 'a' else '-')
                        (if is_ctrl d then 'c' else '-')
                        (if is_data d then 'd' else '-')

end

module DpdZone : sig
  type t
  val is_dpd : Dpd.td -> t -> bool
  val make : Dpd.td -> Locations.Zone.t option -> t
  val add : t -> Dpd.td -> Locations.Zone.t option -> t
  val kind_and_zone : t -> Dpd.t * Locations.Zone.t option
  val dpd_zone : t -> Locations.Zone.t option

  (** total order. Used only to sort...*)
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val default : t
  val pretty : Format.formatter -> t -> unit

end = struct

  type t = Dpd.t * Locations.Zone.t option (* None == Locations.Zone.Top *)

  let dpd_kind dpd = fst dpd
  let dpd_zone dpd = snd dpd
  let kind_and_zone dpd = dpd
  let make k z = (Dpd.make_simple k), z

  let default = Dpd.default, None

  let is_dpd k dpd = Dpd.is_dpd k (dpd_kind dpd)

  let equal dpd1 dpd2 =
    let cmp = Dpd.equal (dpd_kind dpd1) (dpd_kind dpd2) in
      if cmp then
        match (dpd_zone dpd1), (dpd_zone dpd2) with
          | None, None -> true
          | Some z1, Some z2 -> Locations.Zone.equal z1 z2
          | _, _ -> false
      else false


  let compare dpd1 dpd2 =
    if equal dpd1 dpd2 then 0
    else assert false (* is this useful ? TODO ? *)
                            (*
    let cmp = Dpd.compare (dpd_kind dpd1) (dpd_kind dpd2) in
      if cmp = 0 then
        match (dpd_zone dpd1), (dpd_zone dpd2) with
          | None, None -> 0
          | _, None -> -1
          | None, _ -> 1
          | Some z1, Some z2 ->
              if Locations.Zone.equal z1 z2 then 0
              else assert false (* is this useful ? TODO ? *)
      else cmp
      *)

  let add ((d1,z1) as dpd) k z =
    let d = Dpd.add d1 k in
    let z = match z1, z with
      | None, _ -> z1
      | _, None -> z
      | Some zz1, Some zz2  ->
          (* we are loosing some precision here because for instance :
           * (zz1, addr) + (zz2, data) = (zz1 U zz2, data+addr) *)
          let zz = Locations.Zone.join zz1 zz2 in
            match zz with
              | Locations.Zone.Top(_p, _o) -> None
              | _ -> (* To share values as much as possible *)
                  if (zz == zz1)      then z1
                  else if (zz == zz2) then z
                  else Some zz
    in if (d == d1) && (z == z1) then dpd else d, z

  let pretty fmt dpd =
    Dpd.pretty fmt (dpd_kind dpd);
    match (dpd_zone dpd) with None -> ()
      | Some z ->
          Format.fprintf fmt "@[<h 1>(%a)@]" Locations.Zone.pretty z
end

(** The graph itself.
* It uses ocamlgraph
* {{:http://ocamlgraph.lri.fr/doc/Imperative.S.concreteLabeled.html}Graph.Imperative.Digraph}.
* @see <http://ocamlgraph.lri.fr/> ocamlgraph web site
*)
module G = struct

  module IGraph = Graph.Imperative.Digraph.AbstractLabeled(Elem)(DpdZone)
  module E = IGraph.E
  module V = IGraph.V

  (* Could be declared private out of G if the sig was explicit. *)
  type t = { counter : int ref; graph : IGraph.t }

  let create =
    fun () ->
    let counter = ref (-1) in (*BUG: Should be ok but slicing fails if
                                it is not shared among all graphs?????? *)
    { counter = counter;
      graph = IGraph.create ()}

  let find_dpd g v1 v2 =
    let edge = IGraph.find_edge g.graph v1 v2 in
    (edge, IGraph.E.label edge)

  let add_elem g key =
    let elem = Elem.make g.counter key in
    let new_vertex = V.create elem in
    IGraph.add_vertex g.graph new_vertex;
    new_vertex

  let simple_add_dpd g v1 dpd v2 =
    IGraph.add_edge_e g.graph (IGraph.E.create v1 dpd v2)

  let replace_dpd g edge new_dpd =
    let v1 = IGraph.E.src edge in
    let v2 = IGraph.E.dst edge in
    IGraph.remove_edge_e g.graph edge;
    simple_add_dpd g v1 new_dpd v2

  let add_dpd graph v1 dpd_kind opt_zone v2 =
    try
      let edge, old_dpd = find_dpd graph v1 v2 in
      let new_dpd = DpdZone.add old_dpd dpd_kind opt_zone in
      if not (DpdZone.equal old_dpd new_dpd) then
        replace_dpd graph edge new_dpd
    with Not_found ->
      let new_dpd = DpdZone.make dpd_kind opt_zone in
      simple_add_dpd graph v1 new_dpd v2

  let iter_vertex x g = IGraph.iter_vertex x g.graph
  let iter_edges_e x g = IGraph.iter_edges_e x g.graph
  let iter_succ_e x g y = IGraph.iter_succ_e x g.graph y
  let fold_succ_e x g y z = IGraph.fold_succ_e x g.graph y z
  let fold_succ x g y z = IGraph.fold_succ x g.graph y z
  let iter_pred_e x g y = IGraph.iter_pred_e x g.graph y
  let fold_pred x g y z = IGraph.fold_pred x g.graph y z
  let fold_pred_e x g y z = IGraph.fold_pred_e x g.graph y z
  let pred g x = IGraph.pred g.graph x
  let succ g x = IGraph.succ g.graph x

  let edge_dpd e = DpdZone.kind_and_zone (IGraph.E.label e)
  let pretty_edge_label = DpdZone.pretty

end

(** Node.t is the type of the PDG vertex.
   [compare] and [pretty] are needed by [Abstract_interp.Make_Lattice_Set]. *)
module Node = struct

  let elem n = G.V.label n
  let elem_id n = (elem n).Elem.id
  let elem_key n = Elem.key (elem n)
  let stmt n = PdgIndex.Key.stmt (elem_key n)

  (** tells if the node represent the same thing that the given key. *)
  let equivalent n key = (elem_key n) = key

  let print_id fmt n = Elem.print_id fmt (elem n)

  include Datatype.Make_with_collections
    (struct
      include G.V
      let name = "PdgTypes.Node"
      let structural_descr =
	(* ocamlgraph abstract vertex descriptor;
	   see ocamlgraph/src/imperative.ml *)
	Structural_descr.t_record
	  [| Structural_descr.p_int;
	     Elem.packed_descr;
	     Structural_descr.p_int |]
      let reprs = List.map G.V.create Elem.reprs
      let rehash = Datatype.identity
      let copy = Datatype.undefined
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
      let pretty = print_id
      let internal_pretty_code = Datatype.undefined
     end)

(*
  let add_simple_node g key =
    let elem = Elem.make g key in
    let new_vertex = G.V.create elem in
    new_vertex
*)

  let pretty_list fmt l =
    List.iter (fun n -> Format.fprintf fmt " %a" pretty n) l

  let pretty_with_part fmt (n, z_part) =
    Format.fprintf fmt "n%a" pretty n;
    match z_part with None -> ()
      | Some z -> Format.fprintf fmt "(restrict to @[<h 1>%a@])"
                    Locations.Zone.pretty z

end

module NodeSet = struct
  include Set.Make(Node)
  let add_list ?(set=empty) l =
    List.fold_left (fun acc n -> add n acc) set l
end

(** set of nodes of the graph *)
module NodeSetLattice = struct
  include Abstract_interp.Make_Lattice_Set(Node)
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

module Data_state =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = t_data_state
      let name = "PdgTypes.Data_state"
      let reprs =
	List.fold_left
	  (fun acc l ->
	    List.fold_left
	      (fun acc z -> { loc_info = l; under_outputs = z } :: acc)
	      acc
	      Locations.Zone.reprs)
	  []
	  LocInfo.reprs
      let structural_descr =
	Structural_descr.t_record
	  [| LocInfo.packed_descr; Locations.Zone.packed_descr |]
      let mem_project = Datatype.never_any_project
     end)


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
  val get_x_dpds : t_dpds_lists -> Dpd.td option -> t_dpds_list

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
      iter n (DpdZone.make Dpd.Data None) ldata;
      iter n (DpdZone.make Dpd.Addr None) laddr;
      iter n (DpdZone.make Dpd.Ctrl None) lctrl
    in Inthash.iter do_f dd

  let get_x_dpds (ldata, laddr, lctrl) td =
    match td with
      | None ->  ldata @ laddr @ lctrl
      | Some Dpd.Data -> ldata | Some Dpd.Addr -> laddr | Some Dpd.Ctrl -> lctrl

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
  type t_def = {
    graph : G.t ;
    states : t_data_state Inthash.t ;
    index : t_index ;
  }

  type t_body = PdgDef of t_def | PdgTop | PdgBottom

  module Body_datatype =
    Datatype.Make
      (struct
	include Datatype.Undefined(*Serializable_undefined*)
	type t = t_body
	let reprs = [ PdgTop; PdgBottom ]
(*
	(* [JS 2010/09/27] this descr is incorrect since its internal zones are
	   not rehashconsed (do not use Structural_descr.Abstract in all
	   positions containing hashconsed values) *)
	let structural_descr =
	  Structural_descr.Structure
	    (Structural_descr.Sum [| [|
	      Structural_descr.pack
		(Structural_descr.t_record [|
		  Structural_descr.pack Structural_descr.Abstract (* TODO *);
		  (let module H = Cil_datatype.Int_hashtbl.Make(Data_state) in
		   H.packed_descr);
		  Structural_descr.pack Structural_descr.Abstract (* TODO *);
					   |])
				     |] |])
 *)
	let name = "t_body"
	let mem_project = Datatype.never_any_project
       end)
  let () = Type.set_ml_name Body_datatype.ty None

  include Datatype.Pair(Kernel_function)(Body_datatype)

  let make kf graph states index =
    let body = { graph = graph; states = states; index = index ; } in
      (kf, PdgDef body)

  let top kf = (kf, PdgTop)
  let bottom kf = (kf, PdgBottom)

  let is_top pdg = match snd pdg with PdgTop -> true | _ -> false
  let is_bottom pdg = match snd pdg with PdgBottom -> true | _ -> false

  let get_pdg_body pdg = match snd pdg with
    | PdgDef pdg -> pdg
    | PdgTop -> raise Top
    | PdgBottom -> raise Bottom

  let get_kf pdg = fst pdg
  let get_graph pdg = let pdg = get_pdg_body pdg in  pdg.graph
  let get_states pdg = let pdg = get_pdg_body pdg in  pdg.states
  let get_index pdg = let pdg = get_pdg_body pdg in  pdg.index

  let iter_nodes f pdg = G.iter_vertex f (get_graph pdg)

  let fold_call_nodes f acc pdg call =
    let _, call_pdg = PdgIndex.FctIndex.find_call (get_index pdg) call in
    let do_it acc (_k, n) = f acc n in
      PdgIndex.Signature.fold do_it acc call_pdg


  (* let remove_node pdg node =
    G.remove_elem (get_graph pdg) node;
    PdgIndex.FctIndex.remove (get_index pdg) (Node.elem_key node) *)

    type dpd_info = (Node.t * Locations.Zone.t option)

  (** gives the list of nodes that depend to the given node
      with a given kind of dependency.
    *)
  let get_x_direct_edges ~co dpd_type_opt pdg node =
    let pdg = get_pdg_body pdg in
    let is_dpd_ok e = match dpd_type_opt with None -> true
      | Some k -> DpdZone.is_dpd k (G.E.label e)
    in
    let filter edge nodes =
      if is_dpd_ok edge then edge :: nodes else nodes
    in
    let fold = if co then G.fold_pred_e else G.fold_succ_e  in
    let nodes = fold filter pdg.graph node [] in
      nodes

  let edge_end co = if co then G.E.src else G.E.dst

  let get_x_direct ~co dpd_type pdg node =
    let edges = get_x_direct_edges ~co (Some dpd_type) pdg node in
    List.map
      (fun e -> (edge_end co e, DpdZone.dpd_zone (G.E.label e)))
      edges

  let get_x_direct_dpds k = get_x_direct ~co:false k
  let get_x_direct_codpds k = get_x_direct ~co:true k

  let get_all_direct ~co pdg node =
    let edges = get_x_direct_edges ~co None pdg node in
    List.map
      (fun e -> (edge_end co e, DpdZone.dpd_zone (G.E.label e)))
      edges

  let get_all_direct_dpds pdg node = get_all_direct ~co:false pdg node
  let get_all_direct_codpds pdg node = get_all_direct ~co:true pdg node

  let get_all_direct_edges pdg node =
    let co = false in
    let edges = get_x_direct_edges ~co None pdg node in
    let get_info e =
      let k, z = G.edge_dpd e in
      let n = edge_end co e in
        (k, z, n)
    in
      List.map get_info edges

  let pretty_node fmt n =
    let id = Node.elem_id n in
    Format.fprintf fmt "[Elem] %d : " id;
    let key = Node.elem_key n in
    PdgIndex.Key.pretty fmt key

  let pretty_nodes fmt nodes =
    let pretty_node n = Format.fprintf fmt "%a@." pretty_node n in
    List.iter pretty_node nodes

  let pretty_graph ?(bw=false) fmt graph =
    let iter = if bw then G.iter_pred_e else G.iter_succ_e in
    let print_node n =  Format.fprintf fmt "%a@." pretty_node n in
    let print_dpd d =
      let dpd_kind = G.E.label d in
      if bw then Format.fprintf fmt "  <-%a- %d@." G.pretty_edge_label dpd_kind
        (Node.elem_id (G.E.src d))
      else Format.fprintf fmt "  -%a-> %d@." G.pretty_edge_label dpd_kind
        (Node.elem_id (G.E.dst d))
    in
    let print_node_and_dpds n =
      print_node n;
      iter print_dpd graph n
    in
    G.iter_vertex print_node_and_dpds graph

  let pretty_bw ?(bw=false) fmt pdg =
    try
      let graph = get_graph pdg in
      pretty_graph ~bw fmt graph;
    with
    | Top -> Format.fprintf fmt "Top PDG@."
    | Bottom -> Format.fprintf fmt "Bottom PDG@."



  (*-----------------------------------------------------------------------*)
  module Printer = struct

    type parent_t = t
    type t = parent_t
    module V = G.V
    module E = struct
      type t = G.E.t * bool (** boolean to say that the edge is dynamic *)
      let src (e, _dyn) = G.E.src e
      let dst (e, _dyn) = G.E.dst e
    end

    let iter_vertex f pdg =
      try
	let graph = get_graph pdg in
        G.iter_vertex f graph
      with Top | Bottom -> ()

    let iter_edges_e f pdg =
      try
	let graph = get_graph pdg in
	let f_static e = f (e, false) in
        G.iter_edges_e f_static graph;
      with Top | Bottom -> ()

    let graph_attributes _ = [`Rankdir `TopToBottom ]

    let default_vertex_attributes _ = [`Style `Filled]
    let vertex_name v = string_of_int (Node.elem_id v)

    let vertex_attributes v =
      let color_in = (`Fillcolor 0x6495ED) in
      let color_out = (`Fillcolor 0x90EE90) in
      let color_decl = (`Fillcolor 0xFFEFD5) in
      let color_stmt = (`Fillcolor 0xCCCCCC) in
    (* let color_annot = (`Fillcolor 0x999999) in *)
      let color_call = (`Fillcolor 0xFF8A0F) in
      let color_elem_call = (`Fillcolor 0xFFCA6E) in
      let sh_box = (`Shape `Box) in
      let key = Node.elem_key v in
      let sh, col, txt = match key with
        | PdgIndex.Key.VarDecl v ->
          let txt =
            Pretty_utils.sfprintf "@[Decl %a@]" !Ast_printer.d_ident v.vname
          in
          (`Shape `Box) , color_decl , txt
        | PdgIndex.Key.SigKey k ->
          let txt = Pretty_utils.sfprintf "%a"
            PdgIndex.Signature.pretty_key k
          in
          let color = match k with | PdgIndex.Signature.Out _ -> color_out | _ ->  color_in in
          (`Shape `Box), color, txt
        | PdgIndex.Key.Stmt s ->
          let sh, txt = match s.skind with
            | Switch (exp,_,_,_) | If (exp,_,_,_) ->
              let txt = Pretty_utils.sfprintf "%a" !Ast_printer.d_exp exp in
              (`Shape `Diamond), txt
            | Loop _ ->
              (`Shape `Doublecircle), "while"
            | Block _ | UnspecifiedSequence _ ->
              (`Shape `Doublecircle), "{}"
            | Goto _ | Break _ | Continue _ ->
              let txt = Pretty_utils.sfprintf "%a"
                (Cil.defaultCilPrinter#pStmtKind s) s.skind
              in (`Shape `Doublecircle), txt
            | Return _ | Instr _ ->
              let txt = Pretty_utils.sfprintf "%a"
                (Cil.defaultCilPrinter#pStmtKind s) s.skind in
              sh_box, txt
            | _ -> sh_box, "???"
          in sh, color_stmt, txt
        (* | PdgIndex.Key.Annot _ ->
           let txt = Pretty_utils.sfprintf "%a" PdgIndex.Key.pretty key in
           (`Shape `Doublecircle), color_annot, txt *)
        | PdgIndex.Key.CallStmt call ->
          let call_stmt = PdgIndex.Key.call_from_id call in
          let txt = Pretty_utils.sfprintf "%a"
            (Cil.defaultCilPrinter#pStmtKind call_stmt)
            call_stmt.skind
          in sh_box, color_call, txt
        | PdgIndex.Key.SigCallKey (_call, sgn) ->
          let txt =
	    Pretty_utils.sfprintf "%a" PdgIndex.Signature.pretty_key sgn
	  in
          sh_box, color_elem_call, txt
        | PdgIndex.Key.Label _ ->
          let txt = Pretty_utils.sfprintf "%a" PdgIndex.Key.pretty key in
          sh_box, color_stmt, txt
      in sh :: col :: [`Label ( String.escaped txt)]

    let default_edge_attributes _ = []

    let edge_attributes (e, dynamic) =
      let d, z = G.edge_dpd e in
      let attrib = [] in
      let attrib = match z with
	| None -> attrib
	| Some z ->
          let txt =
            Pretty_utils.sfprintf "@[<h 1>%a@]" Locations.Zone.pretty z in
          (`Label txt) :: attrib
      in
      let attrib =
	let color =
          if Dpd.is_data d then (if dynamic then 0xFF00FF else 0x0000FF)
          else  (if dynamic then 0xFF0000 else 0x000000)
	in (`Color color) :: attrib
      in
      let attrib =
	if Dpd.is_ctrl d then (`Arrowhead `Odot)::attrib else attrib
      in
      let attrib =
	if Dpd.is_addr d then (`Style `Dotted)::attrib else attrib
      in attrib

    let get_subgraph v =
      let mk_subgraph name attrib =
	let attrib = (`Style `Filled) :: attrib in
        Some { Graph.Graphviz.DotAttributes.sg_name= name;
               Graph.Graphviz.DotAttributes.sg_attributes = attrib }
      in
      match Node.elem_key v with
      | PdgIndex.Key.CallStmt call | PdgIndex.Key.SigCallKey (call, _) ->
        let call_stmt = PdgIndex.Key.call_from_id call in
        let name = "Call"^(string_of_int call_stmt.sid) in
        let call_txt =
          Pretty_utils.sfprintf "%a"
            (fun fmt ->
              Cil.defaultCilPrinter#pStmtKind call_stmt fmt
            )
            call_stmt.skind  in
        let call_txt = String.escaped call_txt in
        let attrib = [(`Label (name^" : "^call_txt))] in
        let attrib = (`Fillcolor 0xB38B4D) :: attrib in
        mk_subgraph name attrib
      | PdgIndex.Key.SigKey k ->
        let pack_inputs_outputs = false in
        if pack_inputs_outputs then
          begin
            let is_in =  match k with PdgIndex.Signature.In _ -> true | _ -> false in
            let name = if is_in then "Inputs" else "Outputs" in
            let color = if is_in then 0x90EE90 else 0x6495ED in
            let attrib = [] in
            let attrib = (`Fillcolor color) :: attrib in
            mk_subgraph name attrib
          end
        else
          None
      | _ -> None
  end

  (** @see <http://www.lri.fr/~filliatr/ocamlgraph/doc/Graphviz.html>
    *        Graph.Graphviz *)
  module PrintG = Graph.Graphviz.Dot(Printer)

  (*-----------------------------------------------------------------------*)
  (** build the PDG .dot file and put it in [filename].  *)
  let build_dot filename pdg =
    let file = open_out filename in
    PrintG.output_graph file pdg;
    close_out file

  (** build the .dot file and put it in [pdg function name.dot]  *)
  let build_dot_file pdg =
    let kf = get_kf pdg in
    let fct_name = Kernel_function.get_name kf in
    let filename = (fct_name ^ ".dot") in
    build_dot filename pdg;
    filename

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

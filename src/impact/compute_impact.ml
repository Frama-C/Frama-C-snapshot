(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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
open PdgIndex
open Reason_graph

(** Computation of the PDG nodes that are impacted by the "execution"
    of some initial PDG nodes. This is implemented as a forward
    inter-procedural analysis on top of the PDG plugin. *)

module NS = Pdg_aux.NS
type nodes = NS.t
module NM = PdgTypes.Node.Map
module KFS = Kernel_function.Hptset
module KFM = Kernel_function.Map
let kfmns_find_default key m =
  try KFM.find key m
  with Not_found -> NS.empty

(* Data associated to PDG nodes that are impacted, and that have not been
   treated yet. *)
type todo = {
  kf: kernel_function (* kernel_function in which the node can be found *);
  pdg: PdgTypes.Pdg.t (* pdg of this kernel_function *);
  zone: Locations.Zone.t (* fragment of the node that is impacted *);
  init: bool (* is this node in the worklist only because it is part of the
                nodes initially selected as source? The initial nodes are not
                in the final result, but must be present in intermediate
                results for technical reasons *);
}
(* Nodes that are impacted, and that will have to be considered at some point.*)
and todolist = todo NM.t

(* All nodes that have been found to be impacted. Presented as a map from
   Kf, because this information cannot be recovered from the PDG nodes.
   (Also, this speeds up somes operations *)
type result = nodes KFM.t

(* Modelization of a call. The first function (the caller) calls the second
   (the callee) at the given statement. *)
module KfKfCall = Datatype.Triple_with_collections
  (Kernel_function)(Kernel_function)(Cil_datatype.Stmt)
  (struct let module_name = "Impact.Compute.KfKfCall" end)

(** Worklist maintained by the plugin to build its results *)
type worklist = {
  mutable todo: todolist (** nodes that are impacted, but that have not been
       propagated yet. *);

  mutable result: result (** impacted nodes. This field only grows.
       An invariant is that nodes in [todolist] are not already in [result],
       except with differing [init] fields. *);

  mutable downward_calls: Pdg_aux.call_interface KfKfCall.Map.t
    (** calls for which an input may be impacted. If so, we must compute the
       impact within the called function. For each call, we associate to each
       PDG input of the callee the nodes that define the input in the caller.
       The contents of this field grow. *);

  mutable callers: KFS.t (** all the callers of the functions in which the
       initial nodes are located. Constant after initialization, used to
       initialize [upward_calls] below. *);

  mutable upward_calls:  Pdg_aux.call_interface Lazy.t KfKfCall.Map.t
    (** calls for which an output may be impacted. If so, we must compute the
       impact after the call in the caller (which is part of the [callers]
       field by construction). For each output node at the call point in the
       caller, associate all the nodes of the callee that define this output.
       The field is lazy: if the impact "dies" before before reaching the call,
       we may avoid a costly computation. Constant once initialized. *);

  mutable fun_changed_downward: KFS.t (** Functions in which a new pdg node has
       been found since the last iteration. The impact on downward calls with
       those callers will have to be computed again. *);

  mutable fun_changed_upward: KFS.t  (** Functions in which a new pdg node has
       been found. The impact on upward calls to those callees
       will have to be computed again. *);

  mutable skip: Locations.Zone.t (** Locations for which the impact is
       dismissed. Nodes that involve only those zones are skipped. Constant
       after initialization *);

  mutable initial_nodes: nodes KFM.t
       (** Nodes that are part of the initial impact query, or directly
           equivalent to those (corresponding nodes in a caller). *);

  mutable unimpacted_initial: nodes KFM.t
       (** Initial nodes (as defined above) that are not "self-impacting"
           so far. Those nodes will not be part of the final results. *);

  mutable reason: reason_graph
    (** Reasons why nodes in [result] are marked as impacted. *);

  compute_reason: bool (** compute the field [reason]; may be costly *);
}

(** Extract the node of the kf that are only part of the initial impact *)
let unimpacted_initial_by_kf wl kf =
  kfmns_find_default kf wl.unimpacted_initial

(** Extract the current results for a given function *)
let result_by_kf wl kf =
  kfmns_find_default kf wl.result

let result_to_node_origin (r: result) : Reason_graph.nodes_origin =
  KFM.fold
    (fun kf ns acc ->
      NS.fold (fun (n, _) acc -> PdgTypes.Node.Map.add n kf acc) ns acc)
    r PdgTypes.Node.Map.empty

let initial_to_node_set (init: nodes KFM.t) : NS.t =
  KFM.fold (fun _  -> NS.union) init NS.empty

(* -------------------------------------------------------------------------- *)
(* --- Adding nodes to the worklist, or to the results                    --- *)
(* -------------------------------------------------------------------------- *)

(** Mark that [n] comes from an indirect impact, ie. remove it from the
    set of initial nodes that are not impacted. *)
let remove_from_unimpacted_initial wl kf (n, z) =
  let unimpacted = unimpacted_initial_by_kf wl kf in
  if NS.mem' (n, z) unimpacted then begin
    Options.debug ~level:2 "node of initial impact %a is indirectly impacted"
      PdgTypes.Node.pretty n;
    wl.unimpacted_initial <-
      KFM.add kf (NS.remove n unimpacted) wl.unimpacted_initial;
  end
;;

(** Add a node to the sets of impacted nodes. Update the various fields
   of the worklist that need it. [init] indicates that the node
   is added only because it belongs to the set of initial nodes. *)
let add_to_result wl n kf init =
  if init = false then remove_from_unimpacted_initial wl kf n;
  (* if useful, mark that a new node was found in [kf] *)
  if not (KFS.mem kf wl.fun_changed_downward) then
    (* wl.fun_changed_upward is not updated, because we merge
       wl.fun_changed_downward with wl.fun_changed_upward when needed *)
    wl.fun_changed_downward <- KFS.add kf wl.fun_changed_downward;
  let set = result_by_kf wl kf in
  let s' = NS.add' n set in
  wl.result <- KFM.add kf s' wl.result

(** return [true] if the location in [n] is contained in [skip], in which
    case the node should be skipped entirely *)
let node_to_skip skip n =
  match !Db.Pdg.node_key n with
    | Key.SigKey (Signature.In (Signature.InImpl z))
    | Key.SigKey (Signature.Out (Signature.OutLoc z))
    | Key.SigCallKey (_, Signature.In (Signature.InImpl z))
    | Key.SigCallKey (_, Signature.Out (Signature.OutLoc z)) ->
        Locations.Zone.equal Locations.Zone.bottom
          (Locations.Zone.diff z skip)
    | _ -> false

(** Auxiliary function, used to refuse some nodes that should not go in
    the results *)
let filter wl (n, z) =
  not (Locations.Zone.is_bottom z) &&
  match !Db.Pdg.node_key n with
    | Key.SigKey (Signature.In Signature.InCtrl) -> false
        (* do not consider node [InCtrl]. YYY: find when this may happen *)
    | Key.VarDecl _ -> false
        (* do not consider variable declarations. This is probably impossible
           in a forward analysis anyway. *)
    | _ ->
      if node_to_skip wl.skip n then (
        Options.debug ~once:true ~level:2 "skipping node %a as required"
          PdgTypes.Node.pretty n;
        false)
      else true

(** Add a new edge in the graph explaining the results *)
let add_to_reason wl ~nsrc ~ndst rt =
  if wl.compute_reason && filter wl ndst then
    let reason = Reason.Set.add (fst nsrc, fst ndst, rt) wl.reason in
    Options.debug ~level:2 "@[<hov 4>Adding %a@ because of@ %a/%a@]"
      Pdg_aux.pretty_node ndst Reason_graph.ReasonType.pretty rt
      Pdg_aux.pretty_node nsrc;
    wl.reason <- reason
;;

(** Add some nodes to the [todo] field of the worklist, while enforcing some
    invariants. Some kind of pdg nodes must not appear in it, plus the nodes
    must not be in result already. *)
let add_to_do_aux ~init wl kf pdg (pn, zone as n) =
  if filter wl n then
    let pp fmt =
      Format.fprintf fmt "node %a (in %a)"
        Pdg_aux.pretty_node n Kernel_function.pretty kf;
    in
    let add () =
      let todo = { kf; pdg; init; zone } in
      wl.todo <- NM.add pn todo wl.todo
    in
    try
      let cur = NM.find pn wl.todo in
      (* Node is already in the todo list. Check init field and zone *)
      if (cur.init = true && init = false) ||
         (not (Locations.Zone.is_included zone cur.zone))
      then begin
        (* overwrite the existing binding in the todo list *)
        Options.debug ~level:2 "todo list node %t is now init=false" pp;
        add ();
      end
    with Not_found ->
      (* Node is not in todo list. Check if it is already in results *)
      if NS.mem' n (result_by_kf wl kf) then begin
        (* Already in results. Check if [init] flag matches. *)
        if init = false && NS.mem' n (unimpacted_initial_by_kf wl kf) then begin
          (* TODO: check above *)
          (* Node was already there with [init=true] or with smaller .
             Compute impact again
             with [init=false] *)
          Options.debug ~level:2 "adding again node %t, with init=false" pp;
          add ()
        end
      end
      else begin (* General case *)
        Options.debug ~level:2 "adding %t" pp;
        add ()
      end
;;

(** Build the initial value of the [todo] field, from a list of initial nodes *)
let initial_to_do_list wl kf pdg nodes =
  List.iter (fun n -> add_to_do_aux true wl kf pdg n) nodes

(** Mark a new node as impacted, and simultaneouly mark that it is equivalent
    to nodes that are all initial nodes *)
let add_to_do_part_of_initial wl kf pdg n =
  add_to_do_aux ~init:true wl kf pdg n;
  let initial_nodes = kfmns_find_default kf wl.initial_nodes in
  if not (NS.mem' n initial_nodes) then begin
    (* n has never been marked as initial. Mark it in both initial and
       unimpacted_initial fields (it may leave the second later) *)
    Options.debug ~level:2 "node %a is a part of the initial impact"
      Pdg_aux.pretty_node n;
   let unimpacted_kf = unimpacted_initial_by_kf wl kf in
   let new_unimpacted = NS.add' n unimpacted_kf in
   let new_initial = NS.add' n initial_nodes in
   wl.unimpacted_initial <- KFM.add kf new_unimpacted wl.unimpacted_initial;
   wl.initial_nodes <- KFM.add kf new_initial wl.initial_nodes;
  end
;;

(** From now on, most functions will pass [init = false] to [add_to_do_aux]. We
   define an alias instead *)
let add_to_do = add_to_do_aux ~init:false


(* -------------------------------------------------------------------------- *)
(* --- Basic propagation                                                  --- *)
(* -------------------------------------------------------------------------- *)

(** Purely intra-procedural propagation from one impacted node. Just follow
    the PDG once, for all kind of dependencies. *)
let intraprocedural_one_node wl (node, z as nsrc) kf pdg =
  Options.debug ~level:3 "intraprocedural part";
  PdgTypes.Pdg.fold_direct_codpds pdg
    (fun () (dpd, zopt) n ->
       (* Filter edge according to the subzone of the node that is impacted *)
       let follow = match zopt with
         | None -> true
         | Some z' -> Locations.Zone.intersects z z'
       in
       if follow then begin
         (* YYY: is it possible to compute a refinement on this zone? *)
         let ndst = (n, Locations.Zone.top) in
         add_to_reason wl ~nsrc ~ndst (Intraprocedural dpd);
         add_to_do wl kf pdg ndst;
       end
    ) () node;
  Options.debug ~level:3 "intraprocedural part done"


(* -------------------------------------------------------------------------- *)
(* --- Downward call propagation                                          --- *)
(* -------------------------------------------------------------------------- *)

(** Add a downward call to the worklist the first time it is encountered. This
    functions implicitly caches the mapping from the PDG nodes of the caller to
    the ones of the callee, as this information is expensive to compute *)
let add_downward_call wl (caller_kf, pdg) (called_kf, called_pdg) stmt =
  Options.debug ~level:3 "downward part";
  if not (KfKfCall.Map.mem (caller_kf, called_kf, stmt) wl.downward_calls) then
    let callee = (called_kf, called_pdg) in
    let deps = Pdg_aux.all_call_input_nodes ~caller:pdg ~callee stmt in
    wl.downward_calls <-
      KfKfCall.Map.add (caller_kf, called_kf, stmt) deps wl.downward_calls;
    Options.debug ~level:3 "downard part done"
  else
    Options.debug ~level:3 "empty downward part"
;;

(** Propagate impact from node [node] if it corresponds to a call statement.
    This is a partially inter-procedural propagation: some nodes of the
    callee are directly in the worklist, and the call is registered in the
    field [downward_calls]. *)
let downward_one_call_node wl (pnode, _ as node) caller_kf pdg =
  match !Db.Pdg.node_key pnode with
    | Key.SigKey (Signature.In Signature.InCtrl) (* never in the worklist *)
    | Key.VarDecl _ (* never in the worklist *)
    | Key.CallStmt _ (* pdg returns a SigCallKey instead *)
        -> assert false

    | Key.SigKey _ | Key.Stmt _ | Key.Label _ ->
        (* Only intraprocedural part needed, done by
           [intraprocedural_one_node] *) ()

    | Key.SigCallKey(id, key) ->
      let stmt = Key.call_from_id id in
      let called_kfs = Db.Value.call_to_kernel_function stmt in
      KFS.iter
        (fun called_kf ->
           let called_pdg = !Db.Pdg.get called_kf in
           let nodes_callee, pdg_ok =
             Options.debug ~level:3 "%a: considering call to %a"
               Pdg_aux.pretty_node node Kernel_function.pretty called_kf;
             try
               (match key with
                 | Signature.In (Signature.InNum n) ->
                     (try [!Db.Pdg.find_input_node called_pdg n,
                           Locations.Zone.top]
                      with Not_found -> [])
                 | Signature.In Signature.InCtrl ->
                     (try [!Db.Pdg.find_entry_point_node called_pdg,
                           Locations.Zone.top]
                      with Not_found -> [])
                 | Signature.In (Signature.InImpl _) -> assert false
                 | Signature.Out _ -> []
               ), true
             with
               | Db.Pdg.Top ->
                   Options.warning
                     "no precise pdg for function %s. \n\
Ignoring this function in the analysis (potentially incorrect results)."
                     (Kernel_function.get_name called_kf);
                   [], false
               | Db.Pdg.Bottom ->
                   (*Function that fails or never returns immediately *)
                   [], false
               | Not_found -> assert false
           in
           Options.debug ~level:4 "Direct call nodes %a"
             (Pretty_utils.pp_list ~sep:" " Pdg_aux.pretty_node) nodes_callee;
           List.iter
             (fun n ->
                add_to_reason wl ~nsrc:node ~ndst:n InterproceduralDownward;
                add_to_do wl called_kf called_pdg n
             ) nodes_callee;
           if pdg_ok then
             add_downward_call wl (caller_kf, pdg) (called_kf, called_pdg) stmt
        ) called_kfs;
      Options.debug ~level:3 "propagation of call  %a done"
        Pdg_aux.pretty_node node



(* TODO: document *)
let zone_restrict set_src_impact =
  let aux (_, z) acc = Locations.Zone.join z acc in
  NS.fold aux set_src_impact Locations.Zone.bottom


(** Propagate impact for one call registered in [downward_calls].  If the set
    of impacted nodes in the caller intersect the nodes [deps] that define the
    input [node] of the call, add [node] to the impacted nodes. *)
let downward_one_call_inputs wl kf_caller kf_callee (node, deps)  =
  let results_for_kf_caller = result_by_kf wl kf_caller in
  if NS.intersects deps results_for_kf_caller then
    let inter = NS.inter deps results_for_kf_caller in
    let z = zone_restrict inter in
    let node' = (node, z) in
    NS.iter'
      (fun nsrc ->
        add_to_reason wl ~nsrc ~ndst:node' InterproceduralDownward)
      inter;
    add_to_do wl kf_callee (!Db.Pdg.get kf_callee) node';
;;

(** Propagate impact for all calls registered in [downward_calls]. For each
    caller, if new impacted nodes have been found, try to propagate the call.
    Then, zero out the list of functions that must be considered again. *)
let downward_calls_inputs wl =
  let aux (kf_caller, kf_callee, _stmt) ldeps =
    if KFS.mem kf_caller wl.fun_changed_downward then begin
      Options.debug ~level:3 "Inputs from call %a -> %a"
        Kernel_function.pretty kf_caller Kernel_function.pretty kf_callee;
      List.iter (downward_one_call_inputs wl kf_caller kf_callee) ldeps;
      Options.debug ~level:3 "call done"
    end
  in
  KfKfCall.Map.iter aux wl.downward_calls;
  wl.fun_changed_downward <- KFS.empty


(* -------------------------------------------------------------------------- *)
(* --- Upward call propagation                                            --- *)
(* -------------------------------------------------------------------------- *)

(** Fill out the field [upward_calls] of the worklist. This is done by
    visiting (transitively) all the callers of functions in [kfs], and
    registering all the calls found this way. The callers found are added
    to the field [callers]. For each find, we find the nodes of the callee
    that define a given output in the caller using [Pdg_aux.all_call_out_nodes].
    [kfs] must be all the functions containing the initial nodes of the
    analysis. *)
let all_upward_callers wl kfs =
  let aux_call (caller, pdg_caller) (callee, pdg_callee) callsite =
    Options.debug ~level:2 ~source:(fst (Cil_datatype.Stmt.loc callsite))
      "Found call %a -> %a"
      Kernel_function.pretty caller Kernel_function.pretty callee;
    let nodes =
      lazy (Pdg_aux.all_call_out_nodes pdg_callee pdg_caller callsite)
    in
    wl.upward_calls <-
      KfKfCall.Map.add (caller, callee, callsite) nodes wl.upward_calls
  in
  let rec fixpoint todo =
    try
      let kf = KFS.choose todo in
      let todo = KFS.remove kf todo in
      let todo =
        if not (KFS.mem kf wl.callers) then (
          Options.debug "Found caller %a" Kernel_function.pretty kf;
          let pdg_kf = !Db.Pdg.get kf in
          List.fold_left
            (fun todo (caller, callsites) ->
               let pdg_caller = !Db.Pdg.get caller in
               List.iter (aux_call (caller, pdg_caller) (kf, pdg_kf)) callsites;
               KFS.add caller todo
            ) todo (!Db.Value.callers kf);
        )
        else todo
      in
      wl.callers <- KFS.add kf wl.callers;
      fixpoint todo
    with Not_found -> ()
  in
  fixpoint kfs

(** Upward propagation in all the callers. For all upward-registered calls,
    find if new impacted nodes have been found in the callee. If so, check if
    they intersect with the nodes of the callee defining the output. Then, mark
    the (caller) output node as impacted. At the end, zero out the list of
    function that must be examined again. *)
let upward_in_callers wl =
  let aux (caller, callee, _callsite) l =
    if KFS.mem callee wl.fun_changed_upward then
      List.iter
        (fun (n, nodes) ->
           let results_for_callee = result_by_kf wl callee in 
           if NS.intersects nodes results_for_callee then
             let inter = NS.inter nodes results_for_callee in
             let unimpacted_callee = unimpacted_initial_by_kf wl callee in
             let init =
               NS.for_all' (fun n -> NS.mem' n unimpacted_callee) inter
             in
             let z = zone_restrict inter in
             let n = (n, z) in
             NS.iter' (fun nsrc ->
                        add_to_reason wl ~nsrc ~ndst:n InterproceduralUpward
                     ) inter;
             if init then
               add_to_do_part_of_initial wl caller (!Db.Pdg.get caller) n
             else
               add_to_do wl caller (!Db.Pdg.get caller) n
        ) (Lazy.force l)
  in
  KfKfCall.Map.iter aux wl.upward_calls;
  wl.fun_changed_upward <- KFS.empty


(* -------------------------------------------------------------------------- *)
(* --- Initialization                                                     --- *)
(* -------------------------------------------------------------------------- *)

(** Compute the initial state of the worklist. *)
let initial_worklist ?(skip=Locations.Zone.bottom) ?(reason=false) nodes kf =
  let initial = 
      KFM.add kf
        (List.fold_left (fun s n -> NS.add' n s) NS.empty nodes)
        KFM.empty;
  in
  let wl = {
    todo = NM.empty;
    result = KFM.empty;
    downward_calls = KfKfCall.Map.empty;
    callers = KFS.empty;
    upward_calls = KfKfCall.Map.empty;
    initial_nodes = initial;
    unimpacted_initial = initial;
    fun_changed_downward = KFS.empty;
    fun_changed_upward = KFS.empty;
    skip = skip;
    reason = Reason.Set.empty;
    compute_reason = reason;
  }
  in
  (* Fill the [todo] field *)
  initial_to_do_list wl kf (!Db.Pdg.get kf) nodes;
  let initial_callers =
    if Options.Upward.get () then KFS.singleton kf else KFS.empty
  in
  (* Fill the [callers] and [upward_calls] fields *)
  all_upward_callers wl initial_callers;  
  wl

(** To compute the impact of a statement, find the initial PDG nodes that must
    be put in the worklist. The only subtlety consists in skipping input nodes
    on statements that are calls; otherwise, we would get an impact in the
    callees of the call. *)
let initial_nodes ~skip kf stmt =
  Options.debug ~level:3 "computing initial nodes for %d" stmt.sid;
  let pdg = !Db.Pdg.get kf in
  if Db.Value.is_reachable_stmt stmt then
    try
      let all = !Db.Pdg.find_simple_stmt_nodes pdg stmt in
      let filter n = match PdgTypes.Node.elem_key n with
        | Key.SigCallKey (_, Signature.In _) -> false
        | _ -> not (node_to_skip skip n)
      in
      List.filter filter all
    with
      | PdgTypes.Pdg.Top ->
          Options.warning
            "analysis of %a is too imprecise, impact cannot be computed@."
            Kernel_function.pretty kf;
          []
      | Not_found -> assert false
  else begin
    Options.debug ~level:3 "stmt %d is dead. skipping." stmt.sid;
    []
  end

(* -------------------------------------------------------------------------- *)
(* --- Fixpoint                                                           --- *)
(* -------------------------------------------------------------------------- *)

(** Choose one node to process in the todo list, if one remains *)
let pick wl =
  try
    let (n, _ as r) = NM.choose wl.todo in
    wl.todo <- NM.remove n wl.todo;
    Some r
  with Not_found -> None


(** Empty the [todo] field of the worklist by applying as many basic steps as
    possible: intra-procedural steps, plus basic inter-procedural steps on
    downward calls. *)
let rec intraprocedural wl = match pick wl with
  | None -> ()
  | Some (pnode, { kf; pdg; init; zone }) ->
      let node = pnode, zone in
      add_to_result wl node kf init;
      !Db.progress ();
      Options.debug ~level:2 "considering new node %a in %a:@ <%a>%t"
        PdgTypes.Node.pretty pnode Kernel_function.pretty kf
        Pdg_aux.pretty_node node
        (fun fmt -> if init then Format.pp_print_string fmt " (init)");
      intraprocedural_one_node wl node kf pdg;
      downward_one_call_node wl node kf pdg;
      intraprocedural wl

let something_to_do wl = not (NM.is_empty wl.todo)

(** Make the worklist reach a fixpoint, by propagating all possible source of
    impact as much as possible. Due to the way calls are treated (by
    intersecting new impacted nodes with constant sets of nodes), it is more
    efficient to saturate the field [result] before calling
    [downward_calls_inputs] and [upward_in_callers]. We also make sure all
    downward propagation is done before starting upward propagation. *)
let rec fixpoint wl =
  if something_to_do wl then begin
    intraprocedural wl;
    (* Save functions on which the results have changed, as
    [downward_calls_inputs] clears the field [fun_changed_downward] *)
    wl.fun_changed_upward <-
      KFS.union wl.fun_changed_downward wl.fun_changed_upward;
    downward_calls_inputs wl;
    if something_to_do wl then
      fixpoint wl
    else (
      upward_in_callers wl;
      fixpoint wl
    )
  end

let remove_unimpacted _kf impact initial =
  match impact, initial with
    | None, None | Some _, None | None, Some _ (* impossible *) -> impact
    | Some impact, Some initial -> Some (NS.diff impact initial)

(** Impact of a set of nodes. Once the worklist has reached its fixpoint,
    remove the initial nodes that are not self-impacting from the result,
    and return this result. *)
let impact ?skip ?reason nodes kf =
  let wl = initial_worklist ?skip ?reason nodes kf in
  fixpoint wl;
  let without_init =
    KFM.merge remove_unimpacted wl.result wl.unimpacted_initial
  in
  without_init, wl.unimpacted_initial, wl.initial_nodes, wl.reason


(* -------------------------------------------------------------------------- *)
(* --- High-level API                                                     --- *)
(* -------------------------------------------------------------------------- *)

(** Impact of a list of PDG nodes coming from the same function *)
let nodes_impacted_by_nodes ?(skip=Locations.Zone.bottom) ?(restrict=Locations.Zone.top) ?(reason=false) kf nodes =
  let nodes = List.map (fun n -> n, restrict) nodes in
  let r, unimpacted, initial, reason_graph = impact ~skip ~reason nodes kf in
  let pp_kf fmt (kf, ns) =
    Format.fprintf fmt "@[%a: %a@]@ " Kernel_function.pretty kf
      (Pretty_utils.pp_iter ~sep:",@ " ~pre:"" ~suf:""
         NS.iter' Pdg_aux.pretty_node) ns
  in
  let iter f = KFM.iter (fun kf ns -> f (kf, ns)) in
  Options.debug ~level:1 "@[<v>Results:@ %a@]"
    (Pretty_utils.pp_iter ~sep:"@ " ~pre:"" ~suf:"" iter pp_kf) r;
  let reason_full = {
    Reason_graph.reason_graph;
    nodes_origin = result_to_node_origin r;
    initial_nodes = initial_to_node_set initial;
  } in
  if reason then Reason_graph.print_dot_graph reason_full;
  r, unimpacted, reason_full

(** Impact of a list stmts coming from the same function *)
let nodes_impacted_by_stmts ?(skip=Locations.Zone.bottom) ?(restrict=Locations.Zone.top) ?(reason=false) kf stmts =
  let nodes = List.map (initial_nodes ~skip kf) stmts in
  let nodes = List.concat nodes in
  Options.debug
    "about to compute impact for stmt(s) %a, %d initial nodes"
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) stmts
    (List.length nodes);
  nodes_impacted_by_nodes ~skip ~restrict ~reason kf nodes


(** Transform the result of an analysis into a set of PDG nodes *)
let result_to_nodes (res: result) : nodes =
  KFM.fold (fun _ s acc -> NS.union s acc) res NS.empty

(** Transform a set of PDG nodes into a set of statements *)
let nodes_to_stmts ns =
  let get_stmt node = Key.stmt (!Db.Pdg.node_key node) in
  let set =
    (* Do not generate a list immediately, some nodes would be duplicated *)
    NS.fold
      (fun (n, _z) acc ->
        Extlib.may_map ~dft:acc (fun s -> Stmt.Set.add s acc) (get_stmt n)
      ) ns  Stmt.Set.empty
  in
  Stmt.Set.elements set


(** Impact of a list of statements as a set of statements *)
let stmts_impacted ?(skip=Locations.Zone.bottom) ~reason kf stmts =
  let r, _, _ = nodes_impacted_by_stmts ~skip ~reason kf stmts in
  nodes_to_stmts (result_to_nodes r)

(** Impact of a list of PDG nodes as a set of nodes *)
let nodes_impacted ?(skip=Locations.Zone.bottom) ~reason kf nodes =
  let r, _, _ = nodes_impacted_by_nodes ~skip ~reason kf nodes in
  result_to_nodes r


(** Nodes impacted in a given function *)
let impact_in_kf (res: result) kf = kfmns_find_default kf res

(** Computation of the [skip] field from a list of variables *)
let skip_bases vars =
  let aux acc v = Locations.Zone.join acc (Locations.Zone.defaultall v) in
  List.fold_left aux Locations.Zone.bottom vars

(** Computation of the [skip] field from the [-impact-skip] option *)
let skip () =
  let bases = Options.Skip.fold
    (fun name l ->
      let vi = 
        try
          Base.of_varinfo (Globals.Vars.find_from_astinfo name VGlobal)
        with Not_found ->
          if name = "NULL" then Base.null
          else
            Options.abort "cannot skip unknown variable %s" name
      in
      vi :: l) []
  in
  skip_bases bases  

  

(* TODO: dynamically register more high-level functions *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

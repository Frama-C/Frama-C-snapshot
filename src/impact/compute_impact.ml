(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Db
open PdgIndex

(** Computation of the PDG nodes that are impacted by the "execution"
    of some initial PDG nodes. This is implemented as a forward
    inter-procedural analysis on top of the PDG plugin. *)

module NS = PdgTypes.NodeSet
type nodes = NS.t
module NM = PdgTypes.Node.Map
module KFS = Kernel_function.Hptset
module KFM = Kernel_function.Map

(* Data associated to PDG nodes that are impacted, and that have not been
   treated yet. *)
type todo = {
  kf: kernel_function (* kernel_function in which the node can be found *);
  pdg: Pdg.t (* pdg of this kernel_function *);
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
       An invariant is that nodes in [todolist] are not already in [result]. *);

  mutable downward_calls: (PdgTypes.Node.t * nodes) list KfKfCall.Map.t
    (** calls for which an input may be impacted. If so, we must compute the
       impact within the called function. For each call, we associate to each
       PDG input of the callee the nodes that define the input in the caller.
       The contents of this field grow. *);

  mutable callers: KFS.t (** all the callers of the functions in which the
       initial nodes are located. Constant after initialization, used to
       initialize [upward_calls] below. *);

  mutable upward_calls: (PdgTypes.Node.t * nodes) list Lazy.t KfKfCall.Map.t
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

  mutable unimpacted_initial: nodes (** Subset of the initial nodes that are
       not self-impacting so far. Will not be part of the final results. *);
}

(** If [init] is false, mark that [n] has been impacted by an indirect
   impact, and remove it from the set of not self-impacting initial nodes. *)
let init_impact wl n init =
  if not init then
    if NS.mem n wl.unimpacted_initial then
      wl.unimpacted_initial <- NS.remove n wl.unimpacted_initial

(** Extract the current results for a given function *)
let result_by_kf wl kf =
  try KFM.find kf wl.result with Not_found -> NS.empty


(* -------------------------------------------------------------------------- *)
(* --- Adding nodes to the worklist, or to the results                    --- *)
(* -------------------------------------------------------------------------- *)

(** Add a node to the sets of impacted nodes. Update the various fields
   of the worklist that need it. [init] indicates that the node
   is added only because it belongs to the set of initial nodes. *)
let add_to_result wl n kf init =
  init_impact wl n init;
  (* if useful, mark that a new node was found in [kf] *)
  if not (KFS.mem kf wl.fun_changed_downward) then
    (* wl.fun_changed_upward is not updated, because we merge
       wl.fun_changed_downward with wl.fun_changed_upward when needed *)
    wl.fun_changed_downward <- KFS.add kf wl.fun_changed_downward;
  let set = result_by_kf wl kf in
  assert (not (NS.mem n set));
  let s' = NS.add n set in
  wl.result <- KFM.add kf s' wl.result

(** return [true] if the location in [n] is contained in [skip], in which
    case the node should be skipped entirely *)
(* TODO: For nodes InNum, we should evaluate the corresponding formal and see
   if its value matches [skip]. *)
let node_to_skip skip n =
  match !Pdg.node_key n with
    | Key.SigKey (Signature.In (Signature.InImpl z))
    | Key.SigKey (Signature.Out (Signature.OutLoc z))
    | Key.SigCallKey (_, Signature.In (Signature.InImpl z))
    | Key.SigCallKey (_, Signature.Out (Signature.OutLoc z)) ->
        Locations.Zone.equal Locations.Zone.bottom
          (Locations.Zone.diff z skip)
    | _ -> false

(** Add some nodes to the [todo] field of the worklist, while enforcing some
    invariants. Some kind of pdg nodes must not appear in it, plus the nodes
    must not be in result already. *)
let add_to_do_aux init wl kf pdg n =
  match !Pdg.node_key n with
    | Key.SigKey (Signature.In Signature.InCtrl) -> ()
        (* do not consider node [InCtrl] TODO: find when this may happen *)
    | Key.VarDecl _ -> ()
        (* do not consider variable declarations. This is probably impossible
           in a forward analysis anyway. *)
    | _ ->
      if node_to_skip wl.skip n then
        Options.debug ~level:2 "skipping node %a as required"
          PdgTypes.Node.pretty n
      else
        let add () =
          let todo = { kf = kf; pdg = pdg; init = init } in
          wl.todo <- NM.add n todo wl.todo
        in
        if NS.mem n (result_by_kf wl kf) then
          (* Node has already been processed earlier. We only mark that it is
             impacted indirectly (not as an initial node) when needed *)
          init_impact wl n init
        else
          if NM.mem n wl.todo then
            (* Node is not in the results, but is already in the todo list *)
            (if init = false then add () (* overwrite the existing binding
               in the todo list, which may contain [init = true] *))
          else (
            Options.debug ~level:2 "adding node %a (in %a)"
              PdgTypes.Node.pretty n Kernel_function.pretty kf;
            add ()
          )

(** Build the initial value of the [todo] field, from a list of initial nodes *)
let initial_to_do_list wl kf pdg nodes =
  List.iter (fun n -> add_to_do_aux true wl kf pdg n) nodes

(** From now on, all functions will pass [init = false] to [add_to_do_aux]. We
   define an alias instead *)
let add_to_do = add_to_do_aux false

(** Add many nodes to the todo list in one pass, using a generic iterator *)
let add_many_to_do wl kf pdg iter v : unit =
  iter (add_to_do wl kf pdg) v


(* -------------------------------------------------------------------------- *)
(* --- Basic propagation                                                  --- *)
(* -------------------------------------------------------------------------- *)

(** Purely intra-procedural propagation from one impacted node. Just follow
    the PDG once, for all kind of dependencies. *)
let intraprocedural_one_node wl node kf pdg =
  add_many_to_do wl kf pdg (PdgTypes.Pdg.iter_direct_codpds pdg) node;
  Options.debug ~level:3 "intraprocedural part done"


(* -------------------------------------------------------------------------- *)
(* --- Downward call propagation                                          --- *)
(* -------------------------------------------------------------------------- *)

(** Add a downward call to the worklist the first time it is encountered. This
    functions implicitly caches the mapping from the PDG nodes of the caller to
    the ones of the callee, as this information is expensive to compute *)
let add_downward_call wl (caller_kf, pdg) (called_kf, called_pdg) stmt =
  if not (KfKfCall.Map.mem (caller_kf, called_kf, stmt) wl.downward_calls) then
    let deps = Pdg_aux.all_call_input_nodes pdg (called_kf, called_pdg) stmt in
    wl.downward_calls <-
      KfKfCall.Map.add (caller_kf, called_kf, stmt) deps wl.downward_calls

(** Propagate impact from node [node] if it corresponds to a call statement.
    This is a partially inter-procedural propagation: some nodes of the
    callee are directly in the worklist, and the call is registered in the
    field [downward_calls]. *)
let downward_one_call_node wl node caller_kf pdg =
  match !Pdg.node_key node with
    | Key.SigKey (Signature.In Signature.InCtrl) (* never in the worklist *)
    | Key.VarDecl _ (* never in the worklist *)
    | Key.CallStmt _ (* pdg returns a SigCallKey instead *)
        -> assert false

    | Key.SigKey _ | Key.Stmt _ | Key.Label _ ->
        (* Only intraprocedural part needed, done by
           [intraprocedural_one_node] *) ()

    | Key.SigCallKey(id, key) ->
      let stmt = Key.call_from_id id in
      let called_kfs = Value.call_to_kernel_function stmt in
      KFS.iter
        (fun called_kf ->
           let called_pdg = !Pdg.get called_kf in
           let nodes_callee, pdg_ok =
             Options.debug ~level:3 "%a: considering call to %a"
               PdgTypes.Node.pretty node Kernel_function.pretty called_kf;
             try
               (match key with
                 | Signature.In (Signature.InNum n) ->
                     (try [!Pdg.find_input_node called_pdg n]
                      with Not_found -> [])
                 | Signature.In Signature.InCtrl ->
                     (try [!Pdg.find_entry_point_node called_pdg]
                      with Not_found -> [])
                 | Signature.In (Signature.InImpl _) -> assert false
                 | Signature.Out _ -> []
               ), true
             with
               | Pdg.Top ->
                   Options.warning
                     "no precise pdg for function %s. \n\
Ignoring this function in the analysis (potentially incorrect results)."
                     (Kernel_function.get_name called_kf);
                   [], false
               | Pdg.Bottom | Not_found -> assert false
           in
           Options.debug ~level:4 "Direct call nodes %a"
             (Pretty_utils.pp_list ~sep:" " PdgTypes.Node.pretty) nodes_callee;
           add_many_to_do wl called_kf called_pdg List.iter nodes_callee;
           if pdg_ok then
             add_downward_call wl (caller_kf, pdg) (called_kf, called_pdg) stmt
        ) called_kfs;
      Options.debug ~level:3 "propagation of call  %a done"
        PdgTypes.Node.pretty node

(** Propagate impact for one call registered in [downward_calls].  If the set
    of impacted nodes in the caller intersect the nodes [deps] that define the
    input [node] of the call, add [node] to the impacted nodes. *)
let downward_one_call_inputs wl kf_caller kf_callee (node, deps)  =
  Options.debug ~level:3 "Inputs from call %a -> %a"
    Kernel_function.pretty kf_caller Kernel_function.pretty kf_callee;
  if NS.intersects deps (result_by_kf wl kf_caller) then
    add_to_do wl kf_callee (!Db.Pdg.get kf_callee) node;
  Options.debug ~level:3 "call done"

(** Propagate impact for all calls registered in [downward_calls]. For each
    caller, if new impacted nodes have been found, try to propagate the call.
    Then, zero out the list of functions that must be considered again. *)
let downward_calls_inputs wl =
  let aux (kf_caller, kf_callee, _stmt) ldeps =
    if KFS.mem kf_caller wl.fun_changed_downward then
      List.iter (downward_one_call_inputs wl kf_caller kf_callee) ldeps
  in
  KfKfCall.Map.iter aux wl.downward_calls;
  wl.fun_changed_downward <- KFS.empty


(* -------------------------------------------------------------------------- *)
(* --- Upward call propagation                                            --- *)
(* -------------------------------------------------------------------------- *)

(** Fill out the field [upward_calls] of the worklist. This is done by
    visiting (transitively) all the callers of functions in [kfs], and
    registerting all the calls found this way. The callers found are added
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
          let pdg_kf = !Pdg.get kf in
          List.fold_left
            (fun todo (caller, callsites) ->
               let pdg_caller = !Pdg.get caller in
               List.iter (aux_call (caller, pdg_caller) (kf, pdg_kf)) callsites;
               KFS.add caller todo
            ) todo (!Value.callers kf);
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
           if NS.intersects nodes (result_by_kf wl callee) then
             add_to_do wl caller (!Pdg.get caller) n
        ) (Lazy.force l)
  in
  KfKfCall.Map.iter aux wl.upward_calls;
  wl.fun_changed_upward <- KFS.empty


(* -------------------------------------------------------------------------- *)
(* --- Initialization                                                     --- *)
(* -------------------------------------------------------------------------- *)

(** Compute the initial state of the worklist. *)
let initial_worklist ?(skip=Locations.Zone.bottom) nodes kf =
  let wl = {
    todo = NM.empty;
    result = KFM.empty;
    downward_calls = KfKfCall.Map.empty;
    callers = KFS.empty;
    upward_calls = KfKfCall.Map.empty;
    unimpacted_initial = List.fold_left (fun s n -> NS.add n s) NS.empty nodes;
    fun_changed_downward = KFS.empty;
    fun_changed_upward = KFS.empty;
    skip = skip;
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
  let pdg = !Pdg.get kf in
  if Db.Value.is_reachable_stmt stmt then
    try
      let all = !Pdg.find_simple_stmt_nodes pdg stmt in
      let filter n = match PdgTypes.Node.elem_key n with
        | Key.SigCallKey (_, Signature.In _) -> false
        | _ -> not (node_to_skip skip n)
      in
      List.filter filter all
    with Not_found -> assert false
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
  | Some (node, { kf = kf; pdg = pdg; init = init }) ->
      add_to_result wl node kf init;
      !Db.progress ();
      Options.debug ~level:2 "considering new node %a in %a: %a"
        PdgTypes.Node.pretty node Kernel_function.pretty kf
        PdgTypes.Node.pretty_node node;
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

(** Impact of a set of nodes. Once the worklist has reached its fixpoint,
    remove the initial nodes that are not self-impacting from the result,
    and return this result. *)
let impact ?skip nodes kf =
  let wl = initial_worklist ?skip nodes kf in
  fixpoint wl;
  let impact_kf = result_by_kf wl kf in
  let not_init = NS.diff impact_kf wl.unimpacted_initial in
  KFM.add kf not_init wl.result


(* -------------------------------------------------------------------------- *)
(* --- High-level API                                                     --- *)
(* -------------------------------------------------------------------------- *)

(** Impact of a list of statements coming from the same function *)
let impacted_nodes ?(skip=Locations.Zone.bottom) kf stmts =
  let nodes = List.map (initial_nodes ~skip kf) stmts in
  let nodes = List.concat nodes in
  Options.debug
    "about to compute impact for stmt(s) %a, %d initial nodes"
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) stmts
    (List.length nodes);
  let r = impact ~skip nodes kf in
  let pp_kf fmt (kf, ns) =
    Format.fprintf fmt "@[%a: %a@]@ " Kernel_function.pretty kf
      (Pretty_utils.pp_iter ~sep:",@ " ~pre:"" ~suf:""
         NS.iter PdgTypes.Node.pretty) ns
  in
  let iter f = KFM.iter (fun kf ns -> f (kf, ns)) in
  Options.debug ~level:1 "@[<v>Results:@ %a@]"
    (Pretty_utils.pp_iter ~sep:"@ " ~pre:"" ~suf:"" iter pp_kf) r;
  r

(** Transform the result of an analysis into a set of PDG nodes *)
let result_to_nodes (res: result) : nodes =
  KFM.fold (fun _ s acc -> NS.union s acc) res NS.empty

(** Transform a set of PDG nodes into a set of statements *)
let nodes_to_stmts ns =
  let get_stmt node = Key.stmt (!Pdg.node_key node) in
  let set =
    (* Do not generate a list immediately, some nodes would be duplicated *)
    NS.fold
      (fun n acc ->
        Extlib.may_map ~dft:acc (fun s -> Stmt.Set.add s acc) (get_stmt n)
      ) ns  Stmt.Set.empty
  in
  Stmt.Set.elements set

(** Impact of a list of statements as a set of statements *)
let impacted_stmts ?(skip=Locations.Zone.bottom) kf stmts =
  nodes_to_stmts (result_to_nodes (impacted_nodes ~skip kf stmts))

(** Nodes impacted in a given function *)
let impact_in_kf (res: result) kf =
  try KFM.find kf res
  with Not_found -> NS.empty

(** Computation of the [skip] field from a list of variables *)
let skip_vars vars =
  let aux vi =
    let b = Base.create_varinfo vi in
    Locations.Zone.defaultall b
  in
  List.fold_left (fun acc v -> Locations.Zone.join acc (aux v))
    Locations.Zone.bottom vars

(** Computation of the [skip] field from the [-impact-skip] option *)
let skip () =
  let vars = Options.Skip.fold
    (fun name l -> Globals.Vars.find_from_astinfo name VGlobal :: l) []
  in
  skip_vars vars


(* TODO: dynamically register more high-level functions *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

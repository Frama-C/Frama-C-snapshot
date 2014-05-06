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

(* ************************************************************************* *)
(** {2 Searching security annotations} *)
(* ************************************************************************* *)
(*
(** The state of statement for which a security verification should occur. *)
module Security_Annotations =
  Cil_computation.StmtSetRef
    (struct
       let name = "Components.Annotations"
       let dependencies = [ Ast.self ]
     end)

let rec is_security_predicate p = match p.content with
  | Pand(p1, p2) -> is_security_predicate p1 || is_security_predicate p2
  | (* [state(lval) op term] *)
      Prel(_,
           { term_node = Tapp(f1, _ , ([ _ ])) },
           { term_node = TLval(TVar _,_) })
        when f1.l_var_info.lv_name = Model.state_name ->
      true
  | (* [state(lval) op term] *)
      Prel(_,
           { term_node = Tapp(f1, _, [ _ ]) },
           { term_node = _ })
        when f1.l_var_info.lv_name = Model.state_name ->
      assert false
  | _ ->
      false

let has_security_requirement kf =
  List.exists (is_security_predicate $ Logic_const.pred_of_id_pred)
    (Kernel_function.get_spec kf).spec_requires

(* Do not called twice. *)
let search_security_requirements () =
  if Security_Annotations.is_empty () then begin
    Security_slicing_parameters.feedback
      ~level:3 "searching security annotations";
    (* TODO: chercher dans les GlobalAnnotations *)
    let is_security_annotation a =
         (match a.annot_content with
           | AAssert (_behav,p,_) -> is_security_predicate p
           | AStmtSpec { spec_requires = l } ->
               List.exists
                 (is_security_predicate $ Logic_const.pred_of_id_pred) l
           | APragma _
           | AInvariant _ (* | ALoopBehavior _ *)
               (* [JS 2008/02/26] may contain a security predicate *)
           | AVariant _ | AAssigns _
               -> false)
    in
    Annotations.iter
      (fun s annotations ->
         if
           Value.is_reachable_stmt s
           && List.exists
             (function Before a | After a -> is_security_annotation a)
             !annotations
         then
           Security_Annotations.add s);
    Globals.Functions.iter
      (fun kf ->
         if has_security_requirement kf  then
           List.iter
             (fun (_, callsites) ->
                List.iter Security_Annotations.add callsites)
             (!Value.callers kf));
  end
*)
(* ************************************************************************* *)
(** {2 Computing security components} *)
(* ************************************************************************* *)

open PdgIndex

let get_node_stmt node = Key.stmt (!Db.Pdg.node_key node)

module NodeKf = Datatype.Pair(PdgTypes.Node)(Kernel_function)

(* type bwd_kind = Direct | Indirect
type fwd_kind = Impact | Security
type kind =
  | Backward of bwd_kind
  | Forward of fwd_kind

(** Debugging purpose only *)
let pretty_kind fmt = function
  | Backward Direct -> Format.fprintf fmt "backward direct"
  | Backward Indirect -> Format.fprintf fmt "backward indirect"
  | Forward Security -> Format.fprintf fmt "forward"
  | Forward Impact -> Format.fprintf fmt "impact"
*)

(* Never plugged in. To be tested.
module Memo : sig
  val init: kind -> kernel_function -> unit
  val push_function: stmt -> kernel_function -> unit
  val pop_function: unit -> unit
  val memo:
    Pdg.t_node ->
    (unit -> (Pdg.t_node * kernel_function) list) ->
    (Pdg.t_node * kernel_function) list
end = struct

  module Callstack = struct

    type t =
        { mutable stack: (stmt * kernel_function) list;
          mutable current_kf: kernel_function }

    let init kf callstack = callstack.stack <- []; callstack.current_kf <- kf

    let push stmt kf stack =
      stack.stack <- (stmt, stack.current_kf) :: stack.stack;
      stack.current_kf <- kf

    let pop stack =
      let kf = match stack.stack with [] -> assert false | (_, k) :: _ -> k in
      stack.current_kf <- kf

    let equal s1 s2 =
      Kernel_function.equal s1.current_kf s2.current_kf
      && try
        List.iter2
          (fun (s1, kf1) (s2, kf2) ->
             if not (s1.sid = s2.sid && Kernel_function.equal kf1 kf2) then
               raise Exit)
          s1.stack s2.stack;
        true
      with Exit ->
        false

    let hash = Hashtbl.hash

  end

  (* *********************************************************************** *)
  (* state: kind -> callstack -> (node * kf) -> (node * kf) list *)

  module Nodekfs = Hashtbl.Make(NodeKf) (* (node * kf) -> (node * kf) list *)

  module Callstacks = struct
    include Hashtbl.Make(Callstack) (* callstack -> nodekfs *)
    let memo tbl c =
      try find tbl c
      with Not_found -> let t = Nodekfs.create 7 in replace tbl c t; t
  end

  module Memo = struct
    include Hashtbl
    let memo tbl k callstack =
      try
        let callstacks = find tbl k in
        Callstacks.memo callstacks callstack
      with Not_found ->
        let callstacks = Callstacks.create 7 in
        let t = Nodekfs.create 7 in
        Callstacks.replace callstacks callstack t;
        replace tbl k callstacks;
        t
  end

  type local_tbl = (Pdg.t_node * kernel_function) list Nodekfs.t

  type state =
      { mutable kind: kind;
        mutable callstack: Callstack.t;
        mutable local_tbl: local_tbl;
        memo_tbl: (kind, local_tbl Callstacks.t) Memo.t; }
  (* *********************************************************************** *)

  let state =
    let spec = Cil.empty_funspec () in
    { kind = Backward Direct;
      callstack =
        { Callstack.stack = [];
          current_kf =
            { fundec =
                (* do not use Cil.emptyFunction here since it changes the
                   numerotation of variables *)
                Declaration
                  (spec,
                   Cil_datatype.Varinfo.dummy,
                   None,
                   Cil_datatype.Location.unknown);
              return_stmt = None;
              spec = Cil.empty_funspec () } };
      local_tbl = Nodekfs.create 0;
      memo_tbl = Hashtbl.create 5 }

  let update () =
    state.local_tbl <- Memo.memo state.memo_tbl state.kind state.callstack

  let init k kf =
    state.kind <- k;
    Callstack.init kf state.callstack;
    update ()

  let push_function stmt kf =
    Callstack.push stmt kf state.callstack;
    update ()

  let pop_function () =
    Callstack.pop state.callstack;
    update ()

  let memo node f =
    let key = node, state.callstack.Callstack.current_kf in
    try
      Nodekfs.find state.local_tbl key
    with Not_found ->
      let value = f () in
      Nodekfs.replace state.local_tbl key value;
      value

end
*)

(* used to enforce an invariant on [add] *)
module Todolist : sig
  type todo = private
      { node: PdgTypes.Node.t;
        kf: kernel_function;
        pdg: Db.Pdg.t;
        callstack_length: int;
        from_deep: bool }
  type t = todo list
  val mk_init: kernel_function -> Db.Pdg.t -> PdgTypes.Node.t list -> todo list
  val add: 
    PdgTypes.Node.t -> kernel_function -> Db.Pdg.t -> int -> bool -> t -> t
end = struct

  type todo =
      { node: PdgTypes.Node.t;
        kf: kernel_function;
        pdg: Db.Pdg.t;
        callstack_length: int;
        from_deep: bool }

  type t = todo list

  let add n kf pdg len fd list =
    match !Db.Pdg.node_key n with
    | Key.SigKey (Signature.In Signature.InCtrl) ->
        (* do not consider node [InCtrl]  *)
        list
    | Key.VarDecl vi when not (Kernel.LibEntry.get () && vi.vglob) ->
        (* do not consider variable declaration,
           except if libEntry is set and they are globals
           (i.e. we could have no further info about them) *)
        list
    | _ ->
        Security_slicing_parameters.debug ~level:2 "adding node %a (in %s)"
          (!Db.Pdg.pretty_node false) n
          (Kernel_function.get_name kf);
        { node = n; kf = kf; pdg = pdg;
          callstack_length = len; from_deep = fd }
        :: list

  let mk_init kf pdg =
    List.fold_left (fun acc n -> add n kf pdg 0 false acc) []

end

module Component = struct

  (* not optimal implementation: no memoization (bts#006) *)

  module M = FCMap.Make(NodeKf)

  type fwd_kind = Impact | Security

  type kind =
    | Direct
    | Indirect_Backward
    | Forward of fwd_kind

  type value =
      { pdg: Db.Pdg.t;
        mutable callstack_length: int;
        mutable direct: bool;
        mutable indirect_backward: bool;
        mutable forward: bool }

  type t = value M.t

  let is_direct v = v.direct
  let is_indirect_backward v = v.indirect_backward && not v.direct
  let is_forward v = not (v.direct || v.indirect_backward)

  (** Returns [found, new_already] with:
      - [found] is [true] iff [elt] was previously added for [kind]
      - [new_already] is [already] updated with [elt] and its (new) associated
      value. *)
  let check_and_add first elt kind pdg len (already: t) =
    try
      (*        Format.printf "[security] check node %a (in %s, kind %a)@."
                (!Pdg.pretty_node true) (fst elt)
                (Kernel_function.get_name (snd elt))
                pretty_kind kind;*)
      let v = M.find elt already in
      let found, dir, up, down = match kind with
        | Direct -> true, true, false, false
        | Indirect_Backward -> v.indirect_backward, v.direct, true, false
        | Forward _ -> v.forward, v.direct, v.indirect_backward, true
      in
      v.callstack_length <- min v.callstack_length len;
      v.direct <- dir;
      v.indirect_backward <- up;
      v.forward <- down;
      found, already
    with Not_found ->
      let dir, up, down = match kind with
        | Direct -> true, false, false
        | Indirect_Backward -> false, true, false
        | Forward _ -> false, false, true
      in
      let v =
        { pdg = pdg; callstack_length = len;
          direct = dir; indirect_backward = up; forward = down }
      in
      false,
      if first && kind = Forward Impact then
        (* do not add the initial selected stmt for an impact analysis.
           fixed FS#411 *)
        already
      else
        M.add elt v already

  let one_step_related_nodes kind pdg node =
    (* do not consider address dependencies now (except for impact analysis):
       just consider them during the last slicing pass
       (for semantic preservation of pointers) *)
    let direct node = !Db.Pdg.direct_data_dpds pdg node in
    match kind with
    | Direct -> direct node
    | Indirect_Backward -> direct node @ !Db.Pdg.direct_ctrl_dpds pdg node
    | Forward Security ->
        !Db.Pdg.direct_data_uses pdg node @ !Db.Pdg.direct_ctrl_uses pdg node
    | Forward Impact ->
        !Db.Pdg.direct_data_uses pdg node @ !Db.Pdg.direct_ctrl_uses pdg node
        @ !Db.Pdg.direct_addr_uses pdg node

  let search_input kind kf lazy_l =
    try
      match kind with
      | Forward _ -> Lazy.force lazy_l
      | Direct | Indirect_Backward ->
          if !Db.Value.use_spec_instead_of_definition kf
          then Lazy.force lazy_l
          else []
    with Not_found ->
      []

  let add_from_deep caller todo n =
    Todolist.add n caller (!Db.Pdg.get caller) 0 true todo

  let forward_caller kf node todolist =
    let pdg = !Db.Pdg.get kf in
    List.fold_left
      (fun todolist (caller, callsites) ->
         (* foreach caller *)
         List.fold_left
           (fun todolist callsite ->
              let nodes =
                !Db.Pdg.find_call_out_nodes_to_select
                  pdg (PdgTypes.NodeSet.singleton node) (!Db.Pdg.get caller) callsite
              in
              List.fold_left
                (add_from_deep caller)
                todolist
                nodes)
           todolist
           callsites)
      todolist
      (!Db.Value.callers kf)

  let related_nodes_of_nodes kind result nodes =
    let initial_nodes =
      List.map (fun n -> n.Todolist.node, n.Todolist.kf) nodes
    in
    let rec aux first result = function
      | [] -> result
      | { Todolist.node = node; kf = kf; pdg = pdg;
          callstack_length = callstack_length; from_deep = from_deep }
        :: todolist
        ->
          let elt = node, kf in
          let found, result =
            check_and_add first elt kind pdg callstack_length result
          in
          let todolist =
            if found then begin
              todolist
            end else begin
              Security_slicing_parameters.debug
                ~level:2 "considering node %a (in %s)"
                (!Db.Pdg.pretty_node false) node
                (Kernel_function.get_name kf);
              (* intraprocedural related_nodes *)
              let related_nodes = one_step_related_nodes kind pdg node in
              Security_slicing_parameters.debug ~level:3
                "intraprocedural part done";
              let todolist =
                List.fold_left
                  (fun todo n ->
                     Todolist.add n kf pdg callstack_length false todo)
                  todolist
                  related_nodes
              in
              (* interprocedural part *)
              let backward_from_deep compute_nodes =
                (* [TODO optimisation:]
                   en fait, regarder from_deep:
                   si vrai, faire pour chaque caller
                   sinon, faire uniquement pour le caller d'o� on vient *)
                match kind, callstack_length with
                | (Direct | Indirect_Backward), 0 ->
                    (* input of a deep security annotation: foreach call
                       to [kf], compute its related nodes *)
                    let do_caller todolist (caller, callsites) =
                      (* Format.printf "[security of %s] search callers in %s
                         for zone %a@."  (Kernel_function.get_name kf)
                         (Kernel_function.get_name caller)
                         Locations.Zone.pretty zone;*)
                      let pdg_caller = !Db.Pdg.get caller in
                      let do_call todolist callsite =
                        match kind with
                        | Direct | Indirect_Backward ->
                            let nodes = compute_nodes pdg_caller callsite in
                            List.fold_left
                              (add_from_deep caller) todolist nodes
                        | Forward _ ->
                            todolist (* not considered here, see at end *)
                      in
                      List.fold_left do_call todolist callsites
                    in
                    List.fold_left do_caller todolist (!Db.Value.callers kf)
                | _ ->
                    todolist
              in
              let todolist =
                match !Db.Pdg.node_key node with
                | Key.SigKey (Signature.In Signature.InCtrl) ->
                    assert false
                | Key.SigKey (Signature.In (Signature.InImpl zone)) ->
                    let compute_nodes pdg_caller callsite =
                      let nodes, _undef_zone =
                        !Db.Pdg.find_location_nodes_at_stmt
                          pdg_caller callsite ~before:true zone
                          (* TODO : use undef_zone (see FS#201)? *)
                      in
                      let nodes = List.map (fun (n, _z_part) -> n) nodes in
                      (* TODO : use _z_part ? *)
                      nodes
                    in
                    backward_from_deep compute_nodes
                | Key.SigKey key ->
                    let compute_nodes pdg_caller callsite =
                      [ match key with
                        | Signature.In (Signature.InNum n) ->
                            !Db.Pdg.find_call_input_node pdg_caller callsite n
                        | Signature.Out Signature.OutRet  ->
                            !Db.Pdg.find_call_output_node pdg_caller callsite
                        | Signature.In
                            (Signature.InCtrl | Signature.InImpl _)
                        | Signature.Out _ ->
                            assert false ]
                    in
                    backward_from_deep compute_nodes
                | Key.SigCallKey(id, key) ->
                  (* the node is a call: search the related nodes inside the
                     called function (see FS#155) *)
                  if from_deep then
                    (* already come from a deeper annotation:
                       do not go again inside it *)
                    todolist
                  else
                    let stmt = Key.call_from_id id in
                    let called_kfs =
                      Kernel_function.Hptset.elements
                        (try Db.Value.call_to_kernel_function stmt
                         with Db.Value.Not_a_call -> assert false)
                    in
                    let todolist =
                      List.fold_left
                        (fun todolist called_kf ->
                          (* foreach called kf *)
                          (*Format.printf
                            "[security] search inside %s (from %s)@."
                            (Kernel_function.get_name called_kf)
                            (Kernel_function.get_name kf);*)
                          let called_pdg = !Db.Pdg.get called_kf in
                          let nodes =
                            try
                              match kind, key with
                              | (Direct | Indirect_Backward),
                            Signature.Out out_key  ->
                                let nodes, _undef_zone =
                                  !Db.Pdg.find_output_nodes called_pdg out_key
                                (* TODO: use undef_zone (see FS#201) *)
                                in
                                let nodes =
                                  List.map (fun (n, _z_part) -> n) nodes in
                                   (* TODO : use _z_part ? *)
                                nodes
                              | _, Signature.In (Signature.InNum n) ->
                                search_input kind called_kf
                                  (lazy [!Db.Pdg.find_input_node called_pdg n])
                              | _, Signature.In Signature.InCtrl ->
                                search_input kind called_kf
                                  (lazy
                                     [!Db.Pdg.find_entry_point_node called_pdg])
                              | _, Signature.In (Signature.InImpl _) ->
                                assert false
                              | Forward _, Signature.Out _ ->
                                []
                            with
                            | Db.Pdg.Top ->
                              Security_slicing_parameters.warning
                                "no precise pdg for function %s. \n\
Ignoring this function in the analysis (potentially incorrect results)."
                                (Kernel_function.get_name called_kf);
                              []
                            | Db.Pdg.Bottom | Not_found -> assert false
                          in
                          List.fold_left
                            (fun todo n ->
                                  (*Format.printf "node %a inside %s@."
                                    (!Db.Pdg.pretty_node false) n
                                    (Kernel_function.get_name called_kf);*)
                              Todolist.add
                                n called_kf called_pdg
                                (callstack_length + 1) false todo)
                            todolist
                            nodes)
                        todolist
                        called_kfs
                    in
                    (match kind with
                    | Direct | Indirect_Backward ->
                      todolist
                    | Forward _ ->
                      List.fold_left
                        (fun todolist called_kf ->
                          let compute_from_stmt fold =
                            fold
                              (fun (n, kfn) _ acc ->
                                if Kernel_function.equal kfn kf then n :: acc
                                else acc)
                          in
                          let from_stmt =
                            compute_from_stmt M.fold result [] in
                          let from_stmt =
                            (* initial nodes may be not in results *)
                            compute_from_stmt
                              (fun f e acc ->
                                List.fold_left
                                  (fun acc e -> f e [] acc) acc e)
                              initial_nodes
                              from_stmt
                          in
                          let from_stmt = List.fold_left
                            (fun s n -> PdgTypes.NodeSet.add n s)
                            PdgTypes.NodeSet.empty from_stmt in
                          let called_pdg = !Db.Pdg.get called_kf in
                          let nodes =
                            try
                              !Db.Pdg.find_in_nodes_to_select_for_this_call
                                pdg from_stmt stmt called_pdg
                            with
                            | Db.Pdg.Top ->
                              (* warning already emited in the previous fold *)
                              []
                            | Db.Pdg.Bottom | Not_found -> assert false
                          in
                          List.fold_left
                            (fun todo n ->
                              Todolist.add
                                n called_kf called_pdg
                                (callstack_length + 1) false todo)
                            todolist
                            nodes)
                        todolist
                        called_kfs)
                | Key.CallStmt _ | Key.VarDecl _ ->
                  assert false
                | Key.Stmt _ | Key.Label _ ->
                  todolist
              in
              (* [TODO optimisation:] voir commentaire plus haut *)
              match kind with
              | (Direct | Indirect_Backward) -> todolist
              | Forward _ -> forward_caller kf node todolist
            end
          in
          (* recursive call *)
          aux false result todolist
    in
    aux true result nodes

  let initial_nodes kf stmt =
    Security_slicing_parameters.debug
      ~level:3 "computing initial nodes for %d" stmt.sid;
    let pdg = !Db.Pdg.get kf in
    let nodes =
      if Db.Value.is_reachable_stmt stmt then
        try !Db.Pdg.find_simple_stmt_nodes pdg stmt
        with Not_found -> assert false
      else begin
        Security_slicing_parameters.debug
          ~level:3 "stmt %d is dead. skipping." stmt.sid;
        []
      end
    in
    Todolist.mk_init kf pdg nodes

  let direct kf stmt =
    try
      let nodes = initial_nodes kf stmt in
      Security_slicing_parameters.debug
        "computing direct component %d" stmt.sid;
      let res = related_nodes_of_nodes Direct M.empty nodes in
      (* add the initial node, fix FS#180 *)
      let mk p =
        { pdg = p; callstack_length = 0;
          direct = true; indirect_backward = false; forward = false }
      in
      let res =
        List.fold_left
          (fun acc { Todolist.node=n; kf=f; pdg=p } -> M.add (n,f) (mk p) acc)
          res
          nodes
      in
      res
    with Db.Pdg.Top | Db.Pdg.Bottom ->
      Security_slicing_parameters.warning "PDG is not manageable. skipping.";
      M.empty

  let backward kf stmt =
    try
      let nodes = initial_nodes kf stmt in
      let res = direct kf stmt in
      Security_slicing_parameters.debug
        "computing backward indirect component for %d" stmt.sid;
      related_nodes_of_nodes Indirect_Backward res nodes
    with Db.Pdg.Top | Db.Pdg.Bottom ->
      Security_slicing_parameters.warning "PDG is not manageable. skipping.";
      M.empty

  let whole kf stmt =
    let res = backward kf stmt in
    let from =
      M.fold
        (fun (n,kf) v acc ->
           Todolist.add n kf v.pdg v.callstack_length false(*?*) acc)
        res
        []
    in
    Security_slicing_parameters.debug
      "computing forward component for stmt %d" stmt.sid;
    related_nodes_of_nodes (Forward Security) res from

  (* is exactly an impact analysis iff [fwd_kind = Impact] *)
  let forward fwd_kind kf stmt =
    let nodes = initial_nodes kf stmt in
    Security_slicing_parameters.debug
      "computing forward component for stmt %d" stmt.sid;
    let res = related_nodes_of_nodes (Forward fwd_kind) M.empty nodes in
    let set =
      M.fold
        (fun (n,_) _ acc ->
           Extlib.may_map
             ~dft:acc
             (fun s -> Stmt.Set.add s acc)
             (get_node_stmt n))
        res
        Stmt.Set.empty
    in
    Stmt.Set.elements set

  let get_component kind stmt =
    let kf = Kernel_function.find_englobing_kf stmt in
    let action, check = match kind with
      | Direct -> direct, is_direct
      | Indirect_Backward -> backward, is_indirect_backward
      | Forward _ -> whole, is_forward
    in
    let set =
      M.fold
        (fun (n,_) v acc ->
           if check v then
             Extlib.may_map
               ~dft:acc
               (fun s -> Stmt.Set.add s acc)
               (get_node_stmt n)
           else
             acc)
        (action kf stmt)
        Stmt.Set.empty
    in
    Stmt.Set.elements set

(*  let iter use_ctrl_dpds f kf stmt =
    let action = if use_ctrl_dpds then whole else direct in
    M.iter (fun elt _ -> f elt) (action kf stmt)
*)
end

(* ************************************************************************ *)
(* Dynamic registration *)
(* ************************************************************************ *)

let register name arg =
  Dynamic.register
    ~journalize:true
    ~plugin:"Security_slicing"
    name
    (Datatype.func Stmt.ty (Datatype.list Stmt.ty))
    (Component.get_component arg)

let get_direct_component = register "get_direct_component" Component.Direct

let get_indirect_backward_component =
  register "get_indirect_backward_component" Component.Indirect_Backward

let get_forward_component = register "get_forward_component"
  (Component.Forward Component.Security)

let impact_analysis =
  Dynamic.register
    ~plugin:"Security_slicing"
    "impact_analysis"
    ~journalize:true
    (Datatype.func2 Kernel_function.ty Stmt.ty (Datatype.list Stmt.ty))
    (Component.forward Component.Impact)

(* ************************************************************************ *)

(* type t = stmt *)

(** Security component table: a security component is represented by the
    statement at which a security verification should occur.  It is associated
    with the list of its statements. *)
module Components : sig
  (*val add: t -> stmt -> unit
  val find: t -> stmt list
  val self: State.t
  val fold_fold:
    ('b -> t -> 'a -> 'b) -> ('a -> Cil_types.stmt -> 'a) -> 'b -> 'a -> 'b
   *)
end = struct

  module S =
    State_builder.Hashtbl
      (Stmt.Hashtbl)
      (Datatype.Ref(Datatype.List(Stmt)))
      (struct
         let name = "Components"
         let size = 7
         let dependencies = [ Ast.self; Db.Value.self ]
       end)

  let () =
    Cmdline.run_after_extended_stage
      (fun () ->
        State_dependency_graph.add_codependencies ~onto:S.self [ !Db.Pdg.self ])
(*
  let add c =
    let l = S.memo (fun _ -> ref []) c in
    fun s -> l := s :: !l

  let find s = !(S.find s)

  let self = S.self

  let fold_fold f g init_f init_g =
    S.fold (fun c l acc -> f acc c (List.fold_left g init_g !l)) init_f
*)
end
(*
module Nodes =
  State_builder.SetRef
    (struct include NodeKf.Datatype let compare = NodeKf.compare end)
    (struct
       let name = "Components.Nodes"
       let dependencies = [ Security_Annotations.self ]
     end)

let use_ctrl_dependencies = ref false

(** Set tables [Components] and [Stmts]. *)
let compute, self =
  State_builder.apply_once
    "Components.compute"
    [ Security_Annotations.self ]
    (fun () ->
       search_security_requirements ();
       let add_component stmt =
         Security_slicing_parameters.debug
           "computing security component %d" stmt.sid;
         let add_one = Components.add stmt in
         let kf = Kernel_function.find_englobing_kf stmt in
         Component.iter
           !use_ctrl_dependencies
           (fun (n, _ as elt) ->
              Nodes.add elt;
              Extlib.may add_one (get_node_stmt n))
           kf
           stmt
       in
       Security_Annotations.iter add_component)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       Project.State_builder.add_dependency self !Pdg.self;
       Project.State_builder.add_dependency Nodes.self self;
       Project.State_builder.add_dependency Components.self self)

let get_component =
  Dynamic.register
  ~journalize:true
  "Security.get_component"
  (Datatype.func Kernel_type.stmt (Datatype.list Kernel_type.stmt))
  (fun s -> compute (); Components.find s)

(* ************************************************************************ *)
(** {2 Security slicing} *)
(* ************************************************************************ *)

let slice ctrl =
  use_ctrl_dependencies := ctrl;
  Security_slicing_parameters.feedback ~level:2 "beginning slicing";
  compute ();
  let name = "security slicing" in
  let slicing = !Slicing.Project.mk_project name in
  let select (n, kf) sel =
    Security_slicing_parameters.debug ~level:2 "selecting %a (of %s)"
      (!Db.Pdg.pretty_node false) n
      (Kernel_function.get_name kf);
    !Slicing.Select.select_pdg_nodes
      sel
      (!Slicing.Mark.make ~data:true ~addr:true ~ctrl)
      [ n ]
      kf
  in
  let sel = Nodes.fold select Slicing.Select.empty_selects in
  Security_slicing_parameters.debug "adding selection";
  !Slicing.Request.add_persistent_selection slicing sel;
  Security_slicing_parameters.debug "applying slicing request";
  !Slicing.Request.apply_all_internal slicing;
  !Slicing.Slice.remove_uncalled slicing;
  let p = !Slicing.Project.extract name slicing in
(*  Project.copy ~only:(Options.get_selection_after_slicing ()) p;*)
  Security_slicing_parameters.feedback ~level:2 "slicing done";
  p

let slice =
  Dynamic.register
    "Security_slicing.slice"
    ~journalize:true
    (Datatype.func Datatype.bool Project.ty)
    slice
*)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

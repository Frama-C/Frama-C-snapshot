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

(** Build graphs (PDG) for the function
    (see module {!module: Build.BuildPdg})
    to represente the dependencies between instructions
    in order to use it for slicing purposes.

    A function is processed using a forward dataflow analysis
    (see module {{: ../html/Dataflow2.html}Dataflow2}
     which is instanciated with the module
    {!module: Build.Computer} below).
 *)

let dkey = Pdg_parameters.register_category "build"
let debug fmt = Pdg_parameters.debug ~dkey fmt
let debug2 fmt = Pdg_parameters.debug ~dkey fmt ~level:2

open Cil_types
open Cil_datatype
open PdgTypes
open PdgIndex

(* exception Err_Top of string *)
exception Err_Bot of string

(** set of nodes of the graph *)
module BoolNodeSet =
  FCSet.Make(Datatype.Pair(Datatype.Bool)(PdgTypes.Node))

let pretty_node ?(key=false) fmt n = 
  PdgTypes.Node.pretty fmt n;
    if key then 
      Format.fprintf fmt ": %a" PdgIndex.Key.pretty (PdgTypes.Node.elem_key n)

let is_variadic kf =
  let varf = Kernel_function.get_vi kf in
    match varf.vtype with
      | TFun (_, _, is_variadic, _) -> is_variadic
      | _ -> Pdg_parameters.fatal 
               "The variable of a kernel_function has to be a function !"

(* -------------------------------------------------------------------------- *)
(* --- Auxiliary functions                                                --- *)
(* -------------------------------------------------------------------------- *)


  type arg_nodes = Node.t list

  (** type of the whole PDG representation during its building process *)
  type pdg_build = {
             fct : kernel_function;
             mutable topinput : PdgTypes.Node.t option;
             mutable other_inputs :
               (PdgTypes.Node.t * Dpd.td * Locations.Zone.t) list;
             graph : G.t;
             states : Pdg_state.states;
             index : PdgTypes.Pdg.fi;
             ctrl_dpds : BoolNodeSet.t Stmt.Hashtbl.t ;
                       (** The nodes to which each stmt control-depend on.
                         * The links will be added in the graph at the end. *)
             decl_nodes : Node.t Varinfo.Hashtbl.t ;
                       (** map between declaration nodes and the variables
                           to build the dependencies. *)
            }

  (** create an empty build pdg for the function*)
  let create_pdg_build kf =
    let nb_stmts =
      if !Db.Value.use_spec_instead_of_definition kf then 17
      else List.length (Kernel_function.get_definition kf).sallstmts
    in
    let index = FctIndex.create nb_stmts in
    let states = Stmt.Hashtbl.create nb_stmts in
    let graph = G.create () in
    { fct = kf; graph = graph; states = states; index = index;
      topinput = None; other_inputs = [];
      ctrl_dpds  = Stmt.Hashtbl.create nb_stmts ;
      decl_nodes = Varinfo.Hashtbl.create 10 ;
    }

  let _pretty fmt pdg = PdgTypes.Pdg.pretty_graph fmt pdg.graph

  (** add a node to the PDG, but if it is associated with a stmt,
      check before if it doesn't exist already (useful for loops).
      @return the (new or old) node. *)
  let add_elem pdg key =
    match key with
    | Key.CallStmt _ -> assert false
    | _ ->
      try
        FctIndex.find_info pdg.index key
      with Not_found ->
        let new_node = G.add_elem pdg.graph key in
        debug "add_new_node %a@." (pretty_node ~key:true) new_node;
        FctIndex.add pdg.index key new_node;
        new_node

  let decl_var pdg var =
    let new_node = add_elem pdg (Key.decl_var_key var) in
      Varinfo.Hashtbl.add pdg.decl_nodes var new_node;
      new_node

  let get_var_base zone =
    try
      let base, _ = Locations.Zone.find_lonely_key zone in
        match base with
          | Base.Var (var,_) -> Some var
          | _ -> None
    with Not_found -> None

  (** add a dependency with the given label between the two nodes.
      Pre : the nodes have to be already in pdg. *)
  let add_dpd_in_g graph v1 dpd_kind part_opt v2 =
    debug "add_dpd : %a -%a-> %a@." 
      PdgTypes.Node.pretty v1 Dpd.pretty_td dpd_kind 
      PdgTypes.Node.pretty v2;
    G.add_dpd graph v1 dpd_kind part_opt v2

  let add_z_dpd pdg n1 k z_part n2 =
    add_dpd_in_g pdg.graph n1 k z_part n2

  let add_ctrl_dpd pdg n1 n2 =
    add_dpd_in_g pdg.graph n1 Dpd.Ctrl None n2

  let add_decl_dpd pdg n1 k n2 =
    add_dpd_in_g pdg.graph n1 k None n2

  (** add a dependency on the variable declaration.
      The kind of the dependency is address if the variable appears
      in a lvalue, data otherwise.
  *)
  let add_decl_dpds pdg node dpd_kind varset =
    let add_dpd var =
      try
        let var_decl_node = Varinfo.Hashtbl.find pdg.decl_nodes var in
        add_decl_dpd pdg node dpd_kind var_decl_node
      with Not_found ->
        ()
    in
    Varinfo.Set.iter add_dpd varset

  (** [add_dpds pdg v dpd_kind state loc]
  * add 'dpd_kind' dependencies from node n to each element
  * which are stored for loc in state
  *)
  let add_dpds pdg n dpd_kind state loc =
    let add (node,z_part) =
      (* we only use [z_part] for dependencies to OutCall.
      * Would it be interesting to have it on other cases ? *)
      let z_part = match PdgTypes.Node.elem_key node with
        | PdgIndex.Key.SigCallKey
            (_, PdgIndex.Signature.Out (PdgIndex.Signature.OutLoc _)) ->
            z_part
        | _ -> None
      in add_z_dpd pdg n dpd_kind z_part node in
    let nodes, undef_zone = Pdg_state.get_loc_nodes state loc in
    List.iter add nodes;
    match undef_zone with
    | None -> ()
    | Some undef_zone ->
      pdg.other_inputs <- (n, dpd_kind, undef_zone) :: pdg.other_inputs

  (** Process and clear [pdg.ctrl_dpds] which contains a mapping between the
  * statements and the control dependencies that have to be added to the
  * statement nodes. 
  * Because some jump nodes can vanish due to optimisations using the value
  * analysis, we can not rely on the transitivity of the dependencies.
  * So let's compute a transitive closure of the control dependencies. 
  * The table gives : stmt -> ctrl dependency nodes of the statement.
  * So for each stmt, we have to find if some of its ctrl nodes
  * also have dependencies that have to be added to the stmt.
  * *)
  let add_ctrl_dpds pdg =
    let add_indirect ctrl_node_set =
      (* Also add the ctrl_node dependencies to the set.
      * TODOopt: probably a better way to do that if it happens to work ! *)
      let rec add_node (real, n) (acc, seen) =
        if BoolNodeSet.mem (real, n) seen then (acc, seen)
        else 
          let seen = BoolNodeSet.add (real, n) seen in
          let acc = if real then BoolNodeSet.add (true, n) acc else acc in
            add_rec n (acc, seen)
      and add_rec ctrl_node acc = 
        match PdgTypes.Node.elem_key ctrl_node with
          | Key.Stmt ctrl_stmt ->
              (try
                 let stmt_dpds = Stmt.Hashtbl.find pdg.ctrl_dpds ctrl_stmt in
                   BoolNodeSet.fold add_node stmt_dpds acc
               with Not_found -> acc)
          | _ -> (* strange control dependency ! Ignore. *) acc
      in
      let acc = BoolNodeSet.empty, BoolNodeSet.empty in
      let acc, _ = BoolNodeSet.fold add_node ctrl_node_set acc in
        acc
    in
    let add_stmt_ctrl_dpd stmt ctrl_node_set =
      let stmt_nodes =
        try FctIndex.find_all pdg.index (Key.stmt_key stmt)
        with Not_found -> []
             (* some stmts have no node if they are dead code for instance*)
      in
      let label_nodes acc label =
        try acc @ FctIndex.find_all pdg.index (Key.label_key stmt label)
        with Not_found -> acc
      in
      let stmt_nodes = List.fold_left label_nodes stmt_nodes stmt.labels in
      let ctrl_node_set = add_indirect ctrl_node_set in
      let add_node_ctrl_dpds stmt_node =
        BoolNodeSet.iter 
          (fun (_, n) -> add_ctrl_dpd pdg stmt_node n) ctrl_node_set 
      in List.iter add_node_ctrl_dpds stmt_nodes
    in
      Stmt.Hashtbl.iter add_stmt_ctrl_dpd pdg.ctrl_dpds;
      Stmt.Hashtbl.clear pdg.ctrl_dpds


  let process_declarations pdg ~formals ~locals =
    (** 2 new nodes for each formal parameters :
       one for its declaration, and one for its values.
       This is because it might be the case that we only need the declaration
       whatever the value is.
       Might allow us to do a better slicing of the callers.
       TODO: normally, the value should depend on the the declaration,
             but because we don't know how to select a declaration
             without selecting the value at the moment,
             we do the dependence the other way round.
     *)
    let do_param (n, state) v =
      let decl_node = decl_var pdg v in
      let new_node = add_elem pdg (Key.param_key n) in
      add_decl_dpd pdg new_node Dpd.Addr decl_node ;
      add_decl_dpd pdg decl_node Dpd.Addr new_node ;
      let new_state =
        Pdg_state.add_loc_node
          state  ~exact:true (Locations.zone_of_varinfo v) new_node in
        (n+1, new_state)
    in
    let _next_in_num, new_state =
      List.fold_left do_param (1, Pdg_state.empty) formals in
    List.iter (fun v -> ignore (decl_var pdg v)) locals;
    new_state

  let ctrl_call_node pdg call_stmt =
    try FctIndex.find_info pdg.index (Key.call_ctrl_key call_stmt)
    with Not_found -> assert false

  let process_call_args pdg d_state stmt args_dpds : arg_nodes =
    let num = ref 1 in
    let process_arg (dpds, decl_dpds) =
      let new_node = add_elem pdg (Key.call_input_key stmt !num) in
      add_dpds pdg new_node Dpd.Data d_state dpds;
      add_decl_dpds pdg new_node Dpd.Data decl_dpds;
        incr num; new_node
    in List.map process_arg args_dpds

  (** Add a PDG node for each formal argument,
  * and add its dependencies to the corresponding argument node.
  *)
  let process_call_params pdg d_state stmt called_kf (arg_nodes:arg_nodes) =
    let ctrl_node = ctrl_call_node pdg stmt in
    let param_list = Kernel_function.get_formals called_kf in
    let process_param state param arg =
      let new_node = arg in
      add_ctrl_dpd pdg new_node ctrl_node;
        Pdg_state.add_loc_node
          state (Locations.zone_of_varinfo param) new_node ~exact:true
    in
    let rec do_param_arg state param_list (arg_nodes: arg_nodes) =
      match param_list, arg_nodes with
        | [], [] -> state
        | p :: param_list, a :: arg_nodes ->
            let state = process_param state p a in
              do_param_arg state param_list arg_nodes
        | [], _ -> (* call to a variadic function *)
            (* warning already sent during 'from' computation. *)
            state
        | _, [] -> Pdg_parameters.fatal
                     "call to a function with to few arguments"
    in do_param_arg d_state param_list arg_nodes

  let create_call_output_node pdg state stmt out_key out_from fct_dpds =
    let new_node = add_elem pdg out_key in
    add_dpds pdg new_node Dpd.Data state out_from;
    add_dpds pdg new_node Dpd.Ctrl state fct_dpds;
    let ctrl_node = ctrl_call_node pdg stmt in
    add_ctrl_dpd pdg new_node ctrl_node;
    new_node

  (** creates a node for lval : caller has to add dpds about the right part *)
  let create_lval_node pdg state key  ~l_loc ~exact ~l_dpds ~l_decl =
    let new_node = add_elem pdg key in
    add_dpds pdg new_node Dpd.Addr state l_dpds;
    add_decl_dpds pdg new_node Dpd.Addr l_decl;
    let new_state = Pdg_state.add_loc_node state exact l_loc new_node in
     (new_node, new_state)

  let add_from pdg state_before state lval (default, deps) =
    let new_node = add_elem pdg (Key.out_from_key lval) in
    let exact = (not default) in
    let state = Pdg_state.add_loc_node state exact lval new_node in
    add_dpds pdg new_node Dpd.Data state_before deps;
      state

  let process_call_ouput pdg state_before_call state stmt numout out default from_out fct_dpds =
    let exact =
      (* TODO : Check this with Pascal !
      * (Locations.Zone.cardinal_zero_or_one out) && *)
      (not default) in
    debug "call-%d Out%d : %a From %a (%sexact)@."
      stmt.sid numout
      Locations.Zone.pretty out Locations.Zone.pretty from_out
      (if exact then "" else "not ");

    let key = Key.call_output_key stmt (* numout *) out in
    let new_node = create_call_output_node pdg state_before_call stmt
                                          key from_out fct_dpds in
    let state = Pdg_state.add_loc_node state exact out new_node
    in state

  (** mix between process_call_ouput and process_asgn *)
  let process_call_return pdg state_before_call state_with_inputs stmt
                          ~l_loc ~exact ~l_dpds ~l_decl ~r_dpds fct_dpds =
    let out_key = Key.call_outret_key stmt in
    let new_node =
      create_call_output_node pdg state_with_inputs stmt out_key r_dpds fct_dpds
    in
    add_dpds pdg new_node Dpd.Addr state_before_call l_dpds;
    add_decl_dpds pdg new_node Dpd.Addr l_decl;
    let new_state = 
      Pdg_state.add_loc_node state_before_call exact l_loc new_node in
    new_state

  (** for skip statement : we want to add a node in the PDG in ordrer to be able
   * to store information (like marks) about this statement later on *)
  let process_skip pdg _state stmt =
    ignore (add_elem pdg (Key.stmt_key stmt));
      None (* keep previous state *)

  (** for asm: similar to [process_skip], except that we emit a warning *)
  let process_asm pdg _state stmt =
    Pdg_parameters.warning ~once:true ~current:true
      "Ignoring inline assembly code";
    ignore (add_elem pdg (Key.stmt_key stmt));
    None (* keep previous state *)


  let add_label pdg label label_stmt =
    let key = Key.label_key label_stmt label in
      try FctIndex.find_info pdg.index key
      with Not_found -> add_elem pdg key

  let process_stmt_labels pdg stmt =
    let add label = match label with 
      | Label _ -> ignore (add_label pdg label stmt)
      | _ -> (* see [add_dpd_switch_cases] *) ()
    in List.iter add stmt.labels

  let add_label_and_dpd pdg label label_stmt jump_node =
    let label_node = add_label pdg label label_stmt in
      add_ctrl_dpd pdg jump_node label_node

  let add_dpd_goto_label pdg goto_node dest_goto =
    let rec pickLabel = function
      | [] -> None
      | Label _ as lab :: _ -> Some lab
      | _ :: rest -> pickLabel rest
    in
    let label = match pickLabel dest_goto.labels with
    | Some label -> label
    | None -> 
        (* break and continue might not jump to a stmt with label : create one*)
        let lname = Printf.sprintf "fc_stmt_%d" dest_goto.sid in
        let label = Label (lname, Cil_datatype.Stmt.loc dest_goto, false) in
          dest_goto.labels <- label::dest_goto.labels;
          label
    in add_label_and_dpd pdg label dest_goto goto_node

  let add_dpd_switch_cases pdg switch_node case_stmts =
    let add_case stmt =
      let rec pickLabel = function
        | [] -> None
        | Case _ as lab :: _    -> Some lab
        | Default _ as lab :: _ -> Some lab
        | _ :: rest -> pickLabel rest
      in
      match pickLabel stmt.labels with
      | Some label -> add_label_and_dpd pdg label stmt switch_node
      | None -> assert false (* switch sans case ou default ??? *)
    in List.iter add_case case_stmts

  (** The control dependencies are stored : they will be added at the end
     by [finalize_pdg] *)
  let store_ctrl_dpds pdg node iterator (real_dpd, controled_stmt) =
     debug2 "store_ctrl_dpds on %a (real = %b)@."
       (pretty_node ~key:true) node real_dpd ;
    let add_ctrl_dpd stmt =
      let new_dpds =
        try
          let old_dpds = Stmt.Hashtbl.find pdg.ctrl_dpds stmt in
          BoolNodeSet.add (real_dpd, node) old_dpds
        with Not_found -> BoolNodeSet.singleton (real_dpd, node)
      in
      Stmt.Hashtbl.replace pdg.ctrl_dpds stmt new_dpds
    in iterator add_ctrl_dpd controled_stmt

  let mk_jump_node pdg stmt controled_stmts =
    let new_node = add_elem pdg (Key.stmt_key stmt) in
    begin match stmt.skind with
      | If _ | Loop _ | Return _ -> ()
      | Break _ | Continue _ ->
          (* can use : add_dpd_goto_label pdg new_node s
          * if we want later to change break and continue to goto...
          *) ()
      | Goto (sref,_) -> add_dpd_goto_label pdg new_node !sref
      | Switch (_,_,stmts,_) -> add_dpd_switch_cases pdg new_node stmts
      | _ -> assert false
    end;
    store_ctrl_dpds pdg new_node Stmt.Hptset.iter controled_stmts;
       new_node


  (** Add a node for a stmt that is a jump.
      Add control dependencies from this node to the nodes which correspond to
      the stmt list.
      Also add dependencies for the jump to the label.
      Don't use for jumps with data dependencies : use [process_jump_with_exp]
      instead !
   *)
  let process_jump pdg stmt controled_stmts =
    ignore (mk_jump_node pdg stmt controled_stmts)

  (** like [process_jump] but also add data dependencies on the datas and their
      declarations. Use for conditional jumps and returns.
   *)
  let process_jump_with_exp pdg stmt controled_stmts state loc_cond decls_cond =
    let jump_node = mk_jump_node pdg stmt controled_stmts in
    add_dpds pdg jump_node Dpd.Data state loc_cond;
    add_decl_dpds pdg jump_node Dpd.Data decls_cond

  let add_blk_ctrl_dpds pdg key bstmts =
    let new_node = add_elem pdg key in
      store_ctrl_dpds pdg new_node List.iter (true, bstmts)

  let process_block pdg stmt blk =
    add_blk_ctrl_dpds pdg (Key.stmt_key stmt) blk.bstmts

  let process_entry_point pdg bstmts =
      add_blk_ctrl_dpds pdg Key.entry_point bstmts

  let create_fun_output_node pdg state dpds =
    let new_node = add_elem pdg Key.output_key in
    match state with
      | Some state -> add_dpds pdg new_node Dpd.Data state dpds
      | None -> (* return is unreachable *) ()

  (** add a node corresponding to the returned value. *)
  let add_retres pdg state ret_stmt retres_loc_dpds retres_decls =
    let key_return = Key.stmt_key ret_stmt in
    let return_node = add_elem pdg key_return in
    let retres_loc = Db.Value.find_return_loc pdg.fct in
    let retres =
      Locations.enumerate_valid_bits ~for_writing:false
        retres_loc
    in
    add_dpds pdg return_node  Dpd.Data state retres_loc_dpds;
    add_decl_dpds pdg return_node Dpd.Data retres_decls;
    let new_state = Pdg_state.add_loc_node state true retres return_node in
    create_fun_output_node pdg (Some new_state) retres;
      new_state

  (** part of [finalize_pdg] : add missing inputs
  * and build a state with the new nodes to find them back when searching for
  * undefined zones.
  * (notice that now, they can overlap, for example we can have G and G.a)
  * And also deals with warning for uninitialized local variables. *)
  let process_other_inputs pdg =
    debug2 "process_other_inputs@.";
    let rec add n dpd_kind (state, zones) z_or_top =
      (* be careful because [z] can intersect several elements in [zones] *)
      match zones with
        | [] ->
            let key = Key.implicit_in_key z_or_top in
            let nz = add_elem pdg key in
              debug "add_implicit_input : %a@."
                  Locations.Zone.pretty z_or_top ;
            let state = Pdg_state.add_init_state_input state z_or_top nz in
            add_z_dpd pdg n dpd_kind None nz;
              state, [(z_or_top, nz)]
        | (zone, nz)::tl_zones ->
            match z_or_top, zone with
              | (Locations.Zone.Top (_,_), Locations.Zone.Top (_,_)) ->
                  add_z_dpd  pdg n dpd_kind None nz;
                    (state, zones)
              | (z, _) when (Locations.Zone.equal zone z) ->
                  add_z_dpd  pdg n dpd_kind None nz;
                    (* don't add z : already in *)
                    (state, zones)
              | _ -> (* rec : look for z in tail *)
                  let state, tl_zones =
                    add n dpd_kind (state, tl_zones) z_or_top in
                  state, (zone, nz)::tl_zones
    in
    let add_zone acc (n, dpd_kind, z) =
      let do_add = match get_var_base z with
        | Some v -> if Kernel_function.is_local v pdg.fct then false else true
        | None -> true
      in if do_add then
        let acc = match z with
          | Locations.Zone.Top (_,_) ->  add n dpd_kind acc z
          | _ ->
              let aux b intervs acc =
                let z = Locations.Zone.inject b intervs in
                add n dpd_kind acc z
              in
              Locations.Zone.fold_i aux z acc
        in acc
      else begin
        debug2 "might use uninitialized : %a" Locations.Zone.pretty z;
        acc
      end
    in
    let (state, _) =
      List.fold_left add_zone (Pdg_state.empty, []) pdg.other_inputs
    in state

  (** to call then the building process is over :
      add the control dependencies in the graph.
      @return the real PDG that will be used later on.
      @param from_opt for undefined functions  (declarations) *)
  let finalize_pdg pdg from_opt =
    debug2 "try to finalize_pdg";
    let last_state =
      try Some (Pdg_state.get_last_state pdg.states)
      with Not_found ->
        let ret =
          try Kernel_function.find_return pdg.fct
          with Kernel_function.No_Statement ->
            Pdg_parameters.abort "No return in a declaration"
        in
        Pdg_parameters.warning ~once:true ~source:(fst (Stmt.loc ret))
          "no final state. Probably unreachable...";
        None
    in
    (match from_opt with
    | None -> () (* defined function : retres already processed. *)
    | Some froms -> (* undefined function : add output 0 *)
      (* TODO : also add the nodes for the other from ! *)
      let state = match last_state with Some s -> s | None -> assert false in
      let process_out out  (default, deps) s =
        let from_out = Function_Froms.Deps.to_zone deps in
        add_from pdg state s out (default, from_out)
      in
      let from_table = froms.Function_Froms.deps_table in
      let new_state =
        if Function_Froms.Memory.is_bottom from_table then
          Pdg_state.bottom
        else
          let new_state =
            try Function_Froms.Memory.fold_fuse_same
                  process_out from_table state
            with Function_Froms.Memory.Cannot_fold -> (* TOP in from_table *)
              process_out 
		Locations.Zone.top
		(false, Function_Froms.Deps.top)
		state
          in
          if not (Kernel_function.returns_void pdg.fct) then begin
            let from0 = froms.Function_Froms.deps_return in
            let deps_ret = Function_Froms.Memory.LOffset.collapse from0 in
            let deps_ret = Function_Froms.Deps.to_zone deps_ret in
            ignore
              (create_fun_output_node pdg (Some new_state) deps_ret)
          end;
          new_state
      in
      Pdg_state.store_last_state pdg.states new_state);
    let init_state = process_other_inputs pdg in
    Pdg_state.store_init_state pdg.states init_state;
    add_ctrl_dpds pdg ;
    debug2 "finalize_pdg ok";
    PdgTypes.Pdg.make pdg.fct pdg.graph pdg.states pdg.index

(*-----------------------------------------------------------------------*)

(** gives needed informations about [lval] :
  = location + exact + dependencies + declarations *)
let get_lval_infos lval stmt =
  let decl = Cil.extract_varinfos_from_lval lval in
  let state = Db.Value.get_stmt_state stmt in
  let dpds, z_loc, exact =
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Locations.Zone.bottom) ~for_writing:true lval
  in
  (z_loc, exact, dpds, decl)

(** process assignment {v lval = exp; v}
    Use the state at ki (before assign)
    and returns the new state (after assign). *)
let process_asgn pdg state stmt lval exp =
  let r_dpds = !Db.From.find_deps_no_transitivity stmt exp in
  let r_decl = Cil.extract_varinfos_from_exp exp in
  let (l_loc, exact, l_dpds, l_decl) = get_lval_infos lval stmt in
  let key = Key.stmt_key stmt in
  let new_node, new_state =
    create_lval_node pdg state key ~l_loc ~exact ~l_dpds ~l_decl
  in
  add_dpds pdg new_node Dpd.Data state r_dpds;
  add_decl_dpds pdg new_node Dpd.Data r_decl;
  Some new_state


(** Add a PDG node and its dependencies for each explicit call argument. *)
let process_args pdg st stmt argl =
  let process_one_arg arg =
    let dpds = !Db.From.find_deps_no_transitivity stmt arg in
    let decl_dpds = Cil.extract_varinfos_from_exp arg in
    (dpds, decl_dpds)
  in let arg_dpds = List.map process_one_arg argl in
    process_call_args pdg st stmt arg_dpds

(** Add nodes for the call outputs,
   and add the dependencies according to from_table.
   To avoid mixing inputs and outputs, [in_state] is the input state
   and [new_state] the state to modify.
* Process call outputs (including returned value) *)
let call_ouputs  pdg state_before_call state_with_inputs stmt
    lvaloption froms fct_dpds =
  (* obtain inputs from state_with_inputs
     to avoid mixing in and out *)
  let froms_deps_return = froms.Function_Froms.deps_return in
  let from_table = froms.Function_Froms.deps_table in
  let print_outputs fmt =
    Format.fprintf fmt "call outputs  : %a"
      Function_Froms.Memory.pretty from_table;
    if not (lvaloption = None) then
      Format.fprintf fmt "\t and \\result %a@."
        Function_Froms.Memory.LOffset.pretty froms_deps_return
  in
  debug "%t" print_outputs;
  let process_out out (default, deps) (state, numout) =
    let from_out = Function_Froms.Deps.to_zone deps in
    let new_state =
      process_call_ouput pdg state_with_inputs state stmt
                                  numout out default from_out fct_dpds in
      (new_state, numout+1)
  in
  if Function_Froms.Memory.is_bottom from_table then
    Pdg_state.bottom
  else
  let (state_with_outputs, _num) =
    try 
      Function_Froms.Memory.fold_fuse_same process_out from_table (state_before_call, 1)
    with  Function_Froms.Memory.Cannot_fold -> (* TOP in from_table *)
      process_out Locations.Zone.top (false, Function_Froms.Deps.top) 
                                                   (state_before_call, 1)
  in
  let new_state =
    match lvaloption with
      | None -> state_with_outputs
      | Some lval ->
          let r_dpds =
            Function_Froms.Memory.LOffset.collapse froms_deps_return
          in
          let r_dpds = Function_Froms.Deps.to_zone r_dpds in
          let (l_loc, exact, l_dpds, l_decl) = get_lval_infos lval stmt in
          process_call_return
            pdg
            state_with_outputs
            state_with_inputs stmt
            ~l_loc ~exact ~l_dpds ~l_decl
            ~r_dpds fct_dpds
  in new_state

(** process call : {v lvaloption = funcexp (argl); v}
    Use the state at ki (before the call)
    and returns the new state (after the call).
  *)
let process_call pdg state stmt lvaloption funcexp argl =
  let state_before_call = state in
  (** add a simple node for each call in order to have something in the PDG
      for this statement even if there are no input/output *)
  ignore (add_elem pdg (Key.call_ctrl_key stmt));
  let arg_nodes = process_args pdg state_before_call stmt argl in
  let state_with_args = state in
  let funcexp_dpds, called_functions =
    !Db.Value.expr_to_kernel_function
       ~with_alarms:CilE.warn_none_mode
      (Kstmt stmt) ~deps:(Some Locations.Zone.bottom) funcexp
  in
  let mixed_froms =
    try let froms = !Db.From.Callwise.find (Kstmt stmt) in Some froms
    with Not_found -> None (* don't have callwise analysis (-calldeps option) *)
  in
  let process_simple_call called_kf acc =
    let state_with_inputs =
      process_call_params pdg state_with_args stmt called_kf arg_nodes
    in
    let r =
      match mixed_froms with
        | Some _ -> state_with_inputs (* process outputs later *)
        | None -> (* don't have callwise analysis (-calldeps option) *)
            let froms = !Db.From.get called_kf in
            let state_for_this_call =
              call_ouputs pdg state_before_call state_with_inputs
                stmt lvaloption froms funcexp_dpds
            in state_for_this_call
    in r :: acc
  in
  let state_for_each_call =
    Kernel_function.Hptset.fold process_simple_call called_functions []
  in
  let new_state =
    match state_for_each_call with
    | [] ->
       let stmt_str = Pretty_utils.sfprintf "%a" Printer.pp_stmt stmt in
       Pdg_parameters.not_yet_implemented
         "pdg with an unknown function call: %s" stmt_str
    | st :: [] -> st
    | st :: other_states ->
        let merge s1 s2 =
          let _,s = Pdg_state.test_and_merge ~old:s1 s2 in s
        in List.fold_left merge st other_states
  in
  let new_state = match mixed_froms with
    | None -> new_state
    | Some froms ->
          call_ouputs pdg state_before_call new_state
            stmt lvaloption froms funcexp_dpds
  in
  Some new_state

(** Add a node in the PDG for the conditional statement,
 * and register the statements that are control-dependent on it.
 *)
let process_condition ctrl_dpds_infos pdg state stmt condition =
  let loc_cond = !Db.From.find_deps_no_transitivity stmt condition in
  let decls_cond = Cil.extract_varinfos_from_exp condition in

  let controled_stmts = CtrlDpds.get_if_controled_stmts ctrl_dpds_infos stmt in
  let go_then, go_else = Db.Value.condition_truth_value stmt in
  let real = go_then && go_else (* real dpd if we can go in both branches *) in
    if not real then
      debug 
        "[process_condition] stmt %d is not a real cond (never goes in '%s')@." 
        stmt.sid (if go_then then "else" else "then");
   (* build a node for the condition and store de control dependencies *)
   process_jump_with_exp pdg stmt (real, controled_stmts)
                                  state loc_cond decls_cond

(** let's add a node for e jump statement (goto, break, continue)
   and find the statements which are depending on it.
   Returns are not handled here, but in {!Build.process_return}.
*)
let process_jump_stmt pdg ctrl_dpds_infos jump =
  let controled_stmts =
    CtrlDpds.get_jump_controled_stmts ctrl_dpds_infos jump
  in
  let real = Db.Value.is_reachable_stmt jump in
    if not real then
      debug "[process_jump_stmt] stmt %d is not a real jump@." jump.sid;
    process_jump pdg jump (real, controled_stmts)

(** Loop are processed like gotos because CIL transforms them into
* {v while(true) body; v} which is equivalent to {v L : body ; goto L; v}
* There is a small difference because we have to detect the case where
* the [goto L;] would be unreachable (no real loop).
* This is important because it might lead to infinite loop (see bst#787)
*)
let process_loop_stmt pdg ctrl_dpds_infos loop =
  let _entry, back_edges = Stmts_graph.loop_preds loop in
    debug2 "[process_loop_stmt] for loop %d : back edges = {%a}@."
      loop.sid (Pretty_utils.pp_list Stmt.pretty_sid) back_edges;
  let controled_stmts = 
    CtrlDpds.get_loop_controled_stmts ctrl_dpds_infos loop
  in
  let real_loop = List.exists (Db.Value.is_reachable_stmt) back_edges in
    if not real_loop then
      debug "[process_loop_stmt] stmt %d is not a real loop@." loop.sid;
    process_jump pdg loop (real_loop, controled_stmts)

(** [return ret_exp;] is equivalent to [out0 = ret_exp; goto END;]
  * while a simple [return;] is only a [goto END;].
  * Here, we assume that the {{:../html/Oneret.html}Oneret} analysis
  * was used, ie. that it is the only return of the function
  * and that it is the last statement. So, the [goto] is not useful,
  * and the final state is stored to be used later on to compute the outputs.
  *)
let process_return _current_function pdg state stmt ret_exp =
  let last_state =
      match ret_exp with
        | Some exp ->
            let loc_exp = !Db.From.find_deps_no_transitivity stmt exp in
            let decls_exp =  Cil.extract_varinfos_from_exp exp in
            add_retres pdg state stmt loc_exp decls_exp
        | None ->
            let controled_stmt = Cil_datatype.Stmt.Hptset.empty in
            let real = Db.Value.is_reachable_stmt stmt in
              process_jump pdg stmt (real, controled_stmt);
            state
  in
    if Db.Value.is_reachable_stmt stmt then
      Pdg_state.store_last_state pdg.states last_state

module Computer
  (Initial:sig val initial: (stmt * PdgTypes.data_state) list end)
  (Fenv:Dataflows.FUNCTION_ENV)
  (Param:sig val current_pdg : pdg_build
    	     val ctrl_dpds_infos : CtrlDpds.t
  end) = struct
  let pdg_debug fmt = debug fmt

  type t = PdgTypes.data_state

  let current_pdg = Param.current_pdg
  let current_function = Fenv.kf;;
  assert (current_function == current_pdg.fct);;

  let ctrl_dpds_infos = Param.ctrl_dpds_infos

  let init = Initial.initial;;
  let bottom = Pdg_state.bottom

  let pretty fmt (v: t) =
    Format.fprintf fmt "<STATE>@\n%a@\n<\\STATE>@." Pdg_state.pretty v

  let join_and_is_included smaller larger =
    pdg_debug "smaller (new): %a larger (old) %a" pretty smaller pretty larger;
    let is_new, new_state = Pdg_state.test_and_merge larger smaller in
    pdg_debug "new_state: %a is_new: %b" pretty new_state is_new;
    (new_state, not is_new)
  ;;

  let join a b = fst (join_and_is_included a b)

  (** Compute the new state after 'instr' starting from state before 'state'.
    *)
  let doInstr stmt instr state =
    !Db.progress ();
    pdg_debug "doInstr sid:%d : %a" stmt.sid Printer.pp_instr instr;
    let state' = match instr with
      | _ when not (Db.Value.is_reachable_stmt stmt) ->
          pdg_debug "stmt sid:%d is unreachable : skip.@." stmt.sid ;
          Some Pdg_state.bottom 
      | Set (lv, exp, _) -> process_asgn current_pdg state stmt lv exp
      | Call (lvaloption,funcexp,argl,_) ->
          !Db.progress ();
          process_call current_pdg state stmt lvaloption funcexp argl
      | Code_annot _
      | Skip _ -> process_skip current_pdg state stmt
      | Asm  _ -> process_asm current_pdg state stmt
    in 
    (* BY: simplify this code. No need to return an option in the functions
       above *)
    match state' with
    | None -> state
    | Some state -> state

  (** Called before processing the successors of the statements.
   *)
  let transfer_stmt (stmt: Cil_types.stmt) (state: t) =
      pdg_debug "doStmt %d @." stmt.sid ;
    let map_on_all_succs newstate =
      List.map (fun x -> (x,newstate)) stmt.succs
    in
    (* Notice that the stmt labels are processed while processing the jumps. *)
    process_stmt_labels current_pdg stmt;
    match stmt.skind with
      | Instr i
        -> map_on_all_succs (doInstr stmt i state)

      | Block blk ->
          process_block current_pdg stmt blk;
	  map_on_all_succs state
      | UnspecifiedSequence seq ->
          process_block current_pdg stmt
            (Cil.block_from_unspecified_sequence seq);
	  map_on_all_succs state

      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          process_condition ctrl_dpds_infos current_pdg state stmt exp;
	map_on_all_succs state

      | Return (exp,_) ->
          process_return current_function current_pdg state stmt exp;
	[]

      | Continue _
      | Break _
      | Goto _ ->
          process_jump_stmt current_pdg ctrl_dpds_infos stmt;
	map_on_all_succs state

      | Loop _ ->
          process_loop_stmt current_pdg ctrl_dpds_infos stmt;
	map_on_all_succs state

      | TryExcept (_, _, _, _)
      | TryFinally (_, _, _) ->
	map_on_all_succs state

end

exception Value_State_Top

(** Compute and return the PDG for the given function *)
let compute_pdg_for_f kf =
  let pdg = create_pdg_build kf in
  let f_locals, f_stmts =
    if !Db.Value.use_spec_instead_of_definition kf then [], []
    else
      let f = Kernel_function.get_definition kf in
      if !Db.Value.no_results f then
        raise Value_State_Top
      else
        f.slocals, f.sbody.bstmts
  in
  let init_state =
    process_entry_point pdg f_stmts;
    let formals = Kernel_function.get_formals kf in
    process_declarations pdg formals f_locals
  in
  let froms = match f_stmts with
  | [] ->
      Pdg_state.store_last_state pdg.states init_state;
      let froms = !Db.From.get kf in
        Some (froms)
  | start :: _ ->
      let ctrl_dpds_infos = CtrlDpds.compute kf in

      (* Put all statements in initial, so that they are processed and
	 are in the worklist (even if they are dead).  *)
      let allstmts =  (Kernel_function.get_definition kf).sallstmts in
      let allstmts_no_start =
	List.filter (fun s -> s.sid != start.sid) allstmts
      in
      let initial_list =
	List.map (fun s -> (s, Pdg_state.bottom)) allstmts_no_start
      in
      let module Initial = struct
	let initial = (start, init_state)::initial_list end
      in
      let module Fenv =
            (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
      in
      let module Computer = Computer(Initial)(Fenv)(struct
        let current_pdg = pdg
        let ctrl_dpds_infos = ctrl_dpds_infos
      end)
      in
      if Db.Value.is_reachable_stmt start then
        begin
	  let module Compute = Dataflows.Simple_forward(Fenv)(Computer) in
	  Array.iteri (fun ord value ->
	    let stmt = Fenv.to_stmt ord in
	    Stmt.Hashtbl.replace pdg.states stmt value) Compute.before;
          None
        end
      else
        raise
          (Err_Bot
             (Printf.sprintf "unreachable entry point (sid:%d, function %s)"
                start.sid (Kernel_function.get_name kf)))
  in
  let pdg = finalize_pdg pdg froms in
    pdg

let degenerated top kf =
  Pdg_parameters.feedback "%s for function %a" (if top then "Top" else "Bottom")
    Kernel_function.pretty kf;
  if top then PdgTypes.Pdg.top kf else PdgTypes.Pdg.bottom kf

let compute_pdg kf =
  if not (Db.Value.is_computed ()) then !Db.Value.compute ();
  Pdg_parameters.feedback "computing for function %a" Kernel_function.pretty kf;
  try
    if is_variadic kf then
      Pdg_parameters.not_yet_implemented "variadic function";
    let pdg = compute_pdg_for_f kf in
    Pdg_parameters.feedback "done for function %a" Kernel_function.pretty kf;
    pdg
  with
    | Err_Bot what ->
        Pdg_parameters.warning "%s" what ;
        degenerated false kf
    | Value_State_Top -> degenerated true kf
    | Log.AbortFatal what ->
	(* [JS 2012/08/24] nobody should catch this exception *)
        Pdg_parameters.warning "internal error: %s" what ;
        degenerated true kf
    | Log.AbortError what ->
	(* [JS 2012/08/24] nobody should catch this exception *)
        Pdg_parameters.warning "user error: %s" what ;
        degenerated true kf
    | Pdg_state.Cannot_fold ->
        Pdg_parameters.warning "too imprecise value analysis : abort" ;
        degenerated true kf
    | Log.FeatureRequest (who, what) ->
	(* [JS 2012/08/24] nobody should catch this exception *)
        Pdg_parameters.warning "not implemented by %s yet: %s" who what ;
        degenerated true kf

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

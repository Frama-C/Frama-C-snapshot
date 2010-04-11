(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: build.ml,v 1.121 2009-02-23 12:52:19 uid562 Exp $ *)

(** Build graphs (PDG) for the function
    (see module {!module: Build.BuildPdg})
    to represente the dependencies between instructions
    in order to use it for slicing purposes.

    A function is processed using a forward dataflow analysis
    (see module {{: ../html/Dataflow.html}Dataflow}
     which is instanciated with the module
    {!module: Build.Computer} below).
 *)

open Cilutil
open Cil_types
open Cilutil

module M = Macros
module P = Pdg_parameters
module G = PdgTypes.G
module Dpd = PdgTypes.Dpd
module FI = PdgIndex.FctIndex
module Key = PdgIndex.Key

(* exception Err_Top of string *)
exception Err_Bot of string

(** set of nodes of the graph *)
module SimpleNodeSet = Set.Make(PdgTypes.Node)

(* for mk_list_zones see Locations.Zone.fold_enum_by_base *)

let var_to_loc var =
  Locations.valid_enumerate_bits (Locations.loc_of_varinfo var)

let is_variadic kf =
  let varf = Kernel_function.get_vi kf in
    match varf.vtype with
        TFun (_, _, is_variadic, _) -> is_variadic
      | _ -> (Macros.bug
                "The variable of a kernel_function has to be a function !")


(** add a dependency with the given label between the two nodes.
    Pre : the nodes have to be already in pdg.
    *)
let add_dpd_in_g graph v1 dpd_kind part_opt v2 =
  (* let part_opt = match part_opt with Some _ | None -> None in *)
  P.debug "add_dpd : %a -> %a@." Macros.pretty_node v1 Macros.pretty_node v2;
  G.add_dpd graph v1 dpd_kind part_opt v2

(** Module to build the PDG. *)
module BuildPdg : sig
  (** type of the whole PDG representation during its building process *)
  type t

  (** create an empty pdg for the function*)
  val create : Db_types.kernel_function -> t

  val get_kf : t -> Db_types.kernel_function

  val pretty : Format.formatter -> t -> unit

  (** type of the state that is propagated by the forward dataflow analysis *)
  type t_state

  type t_arg_nodes

  val get_states : t -> t_state Inthash.t

  val print_state : Format.formatter -> t_state -> unit

  (** type to describe the data locations (to store information in [t_state]) *)
  type t_loc = Locations.Zone.t

  (** gives the first state of a function with declaration information
   * and begin to build the pdg.
   * [process_declarations pdg formals locals]
   *)
  val process_declarations : t -> formals:Cil_types.varinfo list ->
                             locals:Cil_types.varinfo list ->
                             t_state

  (** for assign statement.
   * @param l_loc assigned location(s)
   * @param l_dpds dependencies of the left hand side of the statement,
   * @param r_dpds dependencies of the right hand side of the statement,
   * @param exact true if the location is surely modified.
  *)
  val process_asgn : t -> t_state -> Cil_types.stmt ->
              l_loc:t_loc -> exact:bool ->
              l_dpds:t_loc -> l_decl: VarinfoSet.t ->
              r_dpds:t_loc -> r_decl: VarinfoSet.t ->
              t_state

  (** for skip statement : we want to add a node in the PDG in ordrer to be able
   * to store information (like marks) about this statement later on *)
  val process_skip : t ->  Cil_types.stmt -> unit

  (** Add a node for the stmt which is a jump.
      Add control dependencies from this node
        to the nodes which correspond to the stmt list.
      Also add dependencies for the jump to the label.
      Don't use for jumps with data dependencies :
        use [process_jump_with_exp] instead !
   *)
  val process_jump : t -> Cil_types.stmt -> Cil_types.stmt list -> unit

  val process_block : t -> Cil_types.stmt -> Cil_types.block -> unit

  val process_entry_point : t -> Cil_types.stmt list -> unit

  (** like [process_jump] but also
      add data dependencies on the datas and their declarations.
      For conditional jumps and returns.
   *)
  val process_jump_with_exp :
      t -> Cil_types.stmt -> Cil_types.stmt list ->
                          t_state -> t_loc -> VarinfoSet.t ->
                          unit

  (** Kind of 'join' of hte two states
   *  but test before if the new state is included in ~old.
   *  @return (true, old U new) if the result is a new state,
   *                     (false, old) if new is included in old.
   *)
  val test_and_merge_states : old:t_state -> t_state -> bool * t_state

  (** add a simple node for each call in order to have something in the PDG
   * for this statement even if there are no input/output *)
  val process_call_node : t ->  Cil_types.stmt -> unit

  val process_call_args : t -> t_state -> Cil_types.stmt ->
                          (t_loc * VarinfoSet.t) list ->
                          (int * t_arg_nodes)
  val process_call_params : t -> t_state -> Cil_types.stmt ->
                            Db_types.kernel_function -> t_arg_nodes ->
                            t_state

  val process_call_ouput : t -> t_state -> t_state -> Cil_types.stmt ->
                            int -> t_loc -> bool -> t_loc -> t_loc -> t_state

  val process_call_return : t -> t_state -> t_state -> Cil_types.stmt ->
                            l_loc:t_loc -> exact:bool ->
                            l_dpds:t_loc -> l_decl:VarinfoSet.t ->
                            r_dpds:t_loc -> t_loc ->
                            t_state

  (** add a node corresponding to the returned value. *)
  val add_retres :  t -> t_state ->  Cil_types.stmt ->
                       t_loc -> VarinfoSet.t -> t_state

  (** store the state as the final state. Will be used in finalize_pdg to add
    * the output nodes. *)
  val store_last_state : t -> t_state -> unit

  (** to call then the building process is over :
      add the control dependencies in the graph.
      @return the real PDG that will be used later on.
   *)
  val finalize_pdg : t -> Function_Froms.t option -> PdgTypes.Pdg.t

  end
= struct

  type t_node = PdgTypes.Node.t
  (** mapping between the statements and the PDG nodes *)
  type t_state = State.t
  type t_loc = Locations.Zone.t
  type t_arg_nodes = t_node list

  (** The PDG used during its computation.
    *)
  type t = { fct : Db_types.kernel_function;
             mutable topinput : PdgTypes.Node.t option;
             mutable other_inputs :
               (PdgTypes.Node.t * Dpd.td * Locations.Zone.t) list;
             graph : G.t;
             states : State.t_states;
             index : PdgTypes.Pdg.t_index;

             ctrl_dpds : (SimpleNodeSet.t) InstrHashtbl.t ;
                       (** The nodes to which each stmt control-depend on.
                         * The links will be added in the graph at the end. *)
             decl_nodes : t_node VarinfoHashtbl.t ;
                       (** map between declaration nodes and the variables
                           to build the dependencies. *)
            }

  let create kf =
    let nb_stmts =
      try
        let fundec = Kernel_function.get_definition kf in
        List.length fundec.sallstmts
      with Kernel_function.No_Definition ->
	42
    in
    let index = FI.create nb_stmts in
    let states = Inthash.create nb_stmts in
    let graph = G.create () in
    { fct = kf; graph = graph; states = states; index = index;
      topinput = None; other_inputs = [];
      ctrl_dpds  = InstrHashtbl.create nb_stmts ;
      decl_nodes = VarinfoHashtbl.create 10 ;
    }

  let get_kf pdg = pdg.fct
  let graph pdg = pdg.graph
  let nodes_index pdg = pdg.index
  let get_states pdg = pdg.states

  let add_to_inputs pdg n dk zone =
    pdg.other_inputs <- (n, dk, zone) :: pdg.other_inputs

  let pretty fmt pdg = PdgTypes.Pdg.pretty_graph fmt pdg.graph

    (** add a node to the PDG, but if it is associated with a stmt,
        check before if it doesn't exist already (useful for loops).
        @return the (new or old) node.
      *)
  let add_elem pdg key =
    let add_new_node key =
      let new_node = G.add_elem (graph pdg) key in
      P.debug "add_new_node %a @." PdgTypes.Node.pretty new_node;
      new_node
    in
    let index = nodes_index pdg in
      try
        match key with
          | Key.CallStmt _ -> assert false
              (*FI.find_info_call index (Key.call_from_id call_id)*)
          | _ -> FI.find_info index key
      with PdgIndex.NotFound ->
        let new_node = add_new_node key in
        let _ = match key with
          | Key.CallStmt _call_id -> assert false
                                      (*
              FI.add_info_call index (Key.call_from_id call_id) new_node
              ~replace:false
              *)
          | _ -> FI.add index key new_node in
          new_node


  let topinput pdg = match pdg.topinput with
    | None ->
        let key = Key.top_input in
        let topinput = add_elem pdg key in
          pdg.topinput <- Some topinput;
          topinput
    | Some top -> top

  let decl_var pdg var =
    let key = Key.decl_var_key var in
    let new_node = add_elem pdg key in
      VarinfoHashtbl.add pdg.decl_nodes var new_node;
      new_node

  let get_var_base zone =
    try
      let base, _ = Locations.Zone.find_lonely_key zone in
        match base with
          | Base.Var (var,_) -> Some var
          | _ -> None
    with Not_found -> None

  let add_z_dpd pdg n1 k z_part n2 =
    add_dpd_in_g (graph pdg) n1 k z_part n2

  let add_ctrl_dpd pdg n1 n2 =
    add_dpd_in_g (graph pdg) n1 Dpd.Ctrl None n2

  let add_decl_dpd pdg n1 k n2 =
    add_dpd_in_g (graph pdg) n1 k None n2

  (** add a dependency on the variable declaration.
      The kind of the dependency is address if the variable appears
      in a lvalue, data otherwise.
  *)
  let add_decl_dpds pdg node dpd_kind varset =
    let add_dpd var =
      try
        let var_decl_node = VarinfoHashtbl.find pdg.decl_nodes var in
        add_decl_dpd pdg node dpd_kind var_decl_node
      with Not_found -> ()
    in VarinfoSet.iter add_dpd varset

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
    let nodes, undef_zone = State.get_loc_nodes state loc in
    List.iter add nodes;
      match undef_zone with None -> ()
        | Some undef_zone -> add_to_inputs pdg n dpd_kind undef_zone

  (** Process and clear [pdg.ctrl_dpds] which contains a mapping between the
  * statements and the control dependencies that have to be added to the
  * statement nodes. *)
  let add_ctrl_dpds pdg =
    let add_node_ctrl_dpd n ctrl_node = add_ctrl_dpd pdg n ctrl_node in
    let add_node_ctrl_dpds n ctrl_node_set =
      SimpleNodeSet.iter (add_node_ctrl_dpd n) ctrl_node_set in
    let add_stmt_ctrl_dpd ki ctrl_node_set =
      let index = nodes_index pdg in
      let label_nodes stmt labels label =
        try labels @ FI.find_all index (Key.label_key stmt label)
        with PdgIndex.NotFound -> labels
      in
        match ki with
          | Kstmt stmt ->
              let lab_nodes =
                List.fold_left (label_nodes stmt) [] stmt.labels
              in
              let stmt_nodes =
                try FI.find_all index (Key.stmt_key stmt)
                with PdgIndex.NotFound -> []
                  (* some stmts have no node if they are dead code for
                   * instance*)
              in
              let nodes = lab_nodes @ stmt_nodes in
                List.iter (fun n -> add_node_ctrl_dpds n ctrl_node_set) nodes
          | _ -> assert false
    in
    InstrHashtbl.iter add_stmt_ctrl_dpd pdg.ctrl_dpds;
    InstrHashtbl.clear pdg.ctrl_dpds

  let test_and_merge_states = State.test_and_merge
  let print_state = State.pretty

  let process_declarations pdg ~formals ~locals =
    let empty_state = State.empty in

    (** 2 new nodes for each formal parameters :
       one for its declaration, and one for its values.
       This is because it might be the case that we only need the declaration
       whatever the value is.
       Might allow us to do a better slicing of the callers.
     *)
    let do_param (n, state) v =
      let decl_node = decl_var pdg v in
      let key = Key.param_key n v in
      let new_node = add_elem pdg key in
      add_decl_dpd pdg new_node Dpd.Addr decl_node ;
      let new_state =
        State.add_loc_node state  ~exact:true (var_to_loc v) new_node in
        (n+1, new_state)
    in
    let _next_in_num, new_state =
      List.fold_left do_param (1, empty_state) formals in
      (* set_max_in pdg (next_in_num - 1); *)

    (* local variables *)
    List.iter (fun v -> ignore (decl_var pdg v)) locals;

    new_state

  let process_call_node pdg call_stmt =
    let key = Key.call_ctrl_key call_stmt in
    let _new_node = add_elem pdg key in ()

  let ctrl_call_node pdg call_stmt =
    try FI.find_info (nodes_index pdg) (Key.call_ctrl_key call_stmt)
    with PdgIndex.NotFound -> assert false

  let process_call_args pdg d_state stmt args_dpds =
    let num = ref 1 in
    let process_arg (dpds, decl_dpds) =
      let key = Key.call_input_key stmt !num in
      let new_node = add_elem pdg key in
      let _ = add_dpds pdg new_node Dpd.Data d_state dpds in
      let _ = add_decl_dpds pdg new_node Dpd.Data decl_dpds in
      num := !num+1; new_node
    in let arg_nodes = List.map process_arg args_dpds in
    !num, arg_nodes

  (** Add a PDG node for each formal argument,
  * and add its dependencies to the corresponding argument node.
  *)
  let process_call_params pdg d_state stmt called_kf arg_nodes =
    let ctrl_node = ctrl_call_node pdg stmt in
    let param_list = Kernel_function.get_formals called_kf in
    let process_param state param arg =
      let new_node = arg in
      let _ = add_ctrl_dpd pdg new_node ctrl_node in
        State.add_loc_node state (var_to_loc param) new_node ~exact:true
    in
    let rec do_param_arg state param_list arg_nodes =
      match param_list, arg_nodes with
        | [], [] -> state
        | p :: param_list, a :: arg_nodes ->
            let state = process_param state p a in
              do_param_arg state param_list arg_nodes
        | [], _ -> (* call to a variadic function *)
            (* warning already sent during 'from' computation. *)
            state
        | _, [] -> Macros.bug "call to a function with to few arguments"
    in do_param_arg d_state param_list arg_nodes

  let create_call_output_node pdg state stmt out_key out_from fct_dpds =
    let new_node = add_elem pdg out_key in
    let _ = add_dpds pdg new_node Dpd.Data state out_from in
    let _ = add_dpds pdg new_node Dpd.Ctrl state fct_dpds in
    let ctrl_node = ctrl_call_node pdg stmt in
    let _ = add_ctrl_dpd pdg new_node ctrl_node in
    new_node

  (** creates a node for lval : caller has to add dpds about the right part *)
  let create_lval_node pdg state key  ~l_loc ~exact ~l_dpds ~l_decl =
    let new_node = add_elem pdg key in
    let _ = add_dpds pdg new_node Dpd.Addr state l_dpds in
    let _ = add_decl_dpds pdg new_node Dpd.Addr l_decl in
    let new_state = State.add_loc_node state exact l_loc new_node in
     (new_node, new_state)

  let add_from pdg state_before state lval (default, deps) =
    let key = Key.out_from_key lval in
    let new_node = add_elem pdg key in
    let exact = (not default) in
    let state = State.add_loc_node state exact lval new_node in
    let _ = add_dpds pdg new_node Dpd.Data state_before deps in
      state

  let process_call_ouput pdg state_before_call state
                         stmt numout out default from_out fct_dpds =
    let exact =
      (* TODO : Check this with Pascal !
      * (Locations.Zone.cardinal_zero_or_one out) && *)
      (not default) in
    P.debug "call-%d Out%d : %a From %a (%sexact)@."
      stmt.sid numout
      Locations.Zone.pretty out Locations.Zone.pretty from_out
      (if exact then "" else "not ");

    let key = Key.call_output_key stmt (* numout *) out in
    let new_node = create_call_output_node pdg state_before_call stmt
                                          key from_out fct_dpds in
    let state = State.add_loc_node state exact out new_node
    in state

  (** mix between process_call_ouput and process_asgn *)
  let process_call_return pdg state_before_call state_with_inputs stmt
                          ~l_loc ~exact ~l_dpds ~l_decl ~r_dpds fct_dpds =
    let out_key = Key.call_outret_key stmt in
    let new_node =
      create_call_output_node pdg state_with_inputs stmt out_key r_dpds fct_dpds
    in
    let _ = add_dpds pdg new_node Dpd.Addr state_before_call l_dpds in
    let _ = add_decl_dpds pdg new_node Dpd.Addr l_decl in
    let new_state = State.add_loc_node state_before_call exact l_loc new_node in
    new_state


  let process_asgn pdg d_state stmt ~l_loc ~exact ~l_dpds ~l_decl
                                    ~r_dpds ~r_decl =
    let key = Key.stmt_key stmt in
    let new_node, new_state =
      create_lval_node pdg d_state key ~l_loc ~exact ~l_dpds ~l_decl in
    let _ = add_dpds pdg new_node Dpd.Data d_state r_dpds in
    let _ = add_decl_dpds pdg new_node Dpd.Data r_decl in
     new_state

  let process_skip pdg stmt =
    let key = Key.stmt_key stmt in
    let _new_node = add_elem pdg key in
      ()


  let add_label pdg label label_stmt jump_node =
    let key = Key.label_key label_stmt label in
    let label_node = add_elem pdg key in
    add_ctrl_dpd pdg jump_node label_node

  let add_dpd_goto_label pdg goto_node dest_goto =
    let rec pickLabel = function
      | [] -> None
      | Label _ as lab :: _ -> Some lab
      | _ :: rest -> pickLabel rest
    in
    match pickLabel dest_goto.labels with
    | Some label -> add_label pdg label dest_goto goto_node
    | None -> assert false (* goto sans label ??? *)

  let add_dpd_switch_cases pdg switch_node case_stmts =
    let add_case stmt =
      let rec pickLabel = function
        | [] -> None
        | Case _ as lab :: _    -> Some lab
        | Default _ as lab :: _ -> Some lab
        | _ :: rest -> pickLabel rest
      in
      match pickLabel stmt.labels with
      | Some label -> add_label pdg label stmt switch_node
      | None -> assert false (* switch sans case ou default ??? *)
    in List.iter add_case case_stmts

  (* The control dependencies are stored : they will be added at the end
     by [finalize_pdg] *)
  let store_ctrl_dpds pdg node controled_stmt =
    let add_ctrl_dpd stmt =
      let kinstr = (Kstmt stmt) in
      let new_dpds =
        try
          let old_dpds = InstrHashtbl.find pdg.ctrl_dpds kinstr in
          SimpleNodeSet.add node old_dpds
        with Not_found -> SimpleNodeSet.singleton node
      in
      InstrHashtbl.replace pdg.ctrl_dpds kinstr new_dpds
    in List.iter add_ctrl_dpd controled_stmt

  let mk_jump_node pdg stmt controled_stmts =
    let key = Key.stmt_key stmt in
    let new_node = add_elem pdg key in
    let _ = match stmt.skind with
      | If _ | Continue _ | Break _ | Loop _ | Return _ -> ()
      | Switch (_,_,stmts,_) -> add_dpd_switch_cases pdg new_node stmts
      | Goto (sref,_) -> add_dpd_goto_label pdg new_node !sref
      | _ -> assert false
    in store_ctrl_dpds pdg new_node controled_stmts;
       new_node

  let process_jump pdg stmt controled_stmts =
    ignore (mk_jump_node pdg stmt controled_stmts)

  let process_jump_with_exp pdg stmt controled_stmts state loc_cond decls_cond =
    let jump_node = mk_jump_node pdg stmt controled_stmts in
    add_dpds pdg jump_node Dpd.Data state loc_cond;
    add_decl_dpds pdg jump_node Dpd.Data decls_cond

  let add_blk_ctrl_dpds pdg key bstmts =
    let new_node = add_elem pdg key in
      store_ctrl_dpds pdg new_node bstmts

  let process_block pdg stmt blk =
    let key = Key.stmt_key stmt in
    add_blk_ctrl_dpds pdg key blk.bstmts

  let process_entry_point pdg bstmts =
    let key = Key.entry_point in
      add_blk_ctrl_dpds pdg key bstmts

  let create_fun_output_node pdg state dpds =
    let key = Key.output_key in
    let new_node = add_elem pdg key in
    match state with
      | Some state -> add_dpds pdg new_node Dpd.Data state dpds
      | None -> (* return is unreachable *) ()

  let add_retres pdg state ret_stmt retres_loc_dpds retres_decls =
    let key_return = Key.stmt_key ret_stmt in
    let return_node = add_elem pdg key_return in
    let retres_loc = Db.Value.find_return_loc (get_kf pdg) in
    let retres = Locations.valid_enumerate_bits retres_loc in
    let _ = add_dpds pdg return_node  Dpd.Data state retres_loc_dpds in
    let _ = add_decl_dpds pdg return_node Dpd.Data retres_decls in
    let new_state = State.add_loc_node state true retres return_node in
    let _ = create_fun_output_node pdg (Some new_state) retres in
      new_state

  let store_last_state pdg state =
    State.store_last_state (get_states pdg) state

  let store_init_state pdg state =
    State.store_init_state (get_states pdg) state

  (** part of [finalize_pdg] : add missing inputs
  * and build a state with the new nodes to find them back when searching for
  * undefined zones.
  * (notice that now, they can overlap, for example we can have G and G.a)
  * And also deals with warning for uninitialized local variables. *)
  let process_other_inputs pdg =
    P.debug ~level:2 "process_other_inputs@.";
    let rec add n dpd_kind (state, zones) z_or_top =
      (* be careful because [z] can intersect several elements in [zones] *)
      match zones with
        | [] ->
            let key = Key.implicit_in_key z_or_top in
            let nz = add_elem pdg key in
              P.debug "add_implicit_input : %a@."
                  Locations.Zone.pretty z_or_top ;
            let state = State.add_init_state_input state z_or_top nz in
            let _ = add_z_dpd pdg n dpd_kind None nz in
              state, [(z_or_top, nz)]
        | (zone, nz)::tl_zones ->
            match z_or_top, zone with
              | (Locations.Zone.Top (_,_), Locations.Zone.Top (_,_)) ->
                  let _ = add_z_dpd  pdg n dpd_kind None nz in
                    (state, zones)
              | (z, _) when (Locations.Zone.equal zone z) ->
                  let _ = add_z_dpd  pdg n dpd_kind None nz in
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
              let do_z z acc = add n dpd_kind acc z in
                Locations.Zone.fold_enum_by_base do_z z acc
        in acc
      else
        begin
          Cil.log "[pdg warning] might use uninitialized : %a"
            Locations.Zone.pretty z ;
          acc
        end
    in
    let (state, _) =
      List.fold_left add_zone (State.empty, []) pdg.other_inputs
    in state

  (** @param from_opt for undefined functions  (declarations) *)
  let finalize_pdg pdg from_opt =
    P.debug ~level:2 "try to finalize_pdg";
    let last_state =
      try Some (State.get_last_state (get_states pdg))
      with Not_found ->
        CilE.warn_once "no final state. Probably unreachable..."; None
    in
    let _ = match from_opt with
      | None -> () (* defined function : retres already processed. *)
      | Some froms -> (* undefined function : add output 0 *)
          (* TODO : also add the nodes for the other from ! *)
          let in_state =
            match last_state with Some s -> s | None -> assert false in
          let state = in_state in
          let process_out out  (default, from_out) state =
            add_from pdg in_state state out (default, from_out)
          in
          let from_table = froms.Function_Froms.deps_table in
          let new_state =
            try Lmap_bitwise.From_Model.fold process_out from_table state
            with Lmap_bitwise.From_Model.Cannot_fold -> (* TOP in from_table *)
              process_out Locations.Zone.top (false, Locations.Zone.top) state
          in
          let new_state =
            if (not (Kernel_function.returns_void pdg.fct)) then
              let from0 = froms.Function_Froms.deps_return in
              let _ = create_fun_output_node pdg (Some new_state)
                        (Lmap_bitwise.From_Model.LOffset.collapse from0)
              in new_state
            else new_state
          in
          store_last_state pdg new_state
    in
    let init_state = process_other_inputs pdg in
      store_init_state pdg init_state;
    add_ctrl_dpds pdg ;
    P.debug ~level:2 "finalize_pdg ok";
    let states = get_states pdg in
    let pdg = PdgTypes.Pdg.make pdg.fct pdg.graph states pdg.index 
    in
    pdg

end

(*-----------------------------------------------------------------------*)


(** gives needed informations about [lval] :
  = location + exact + dependencies + declarations
  *)
let get_lval_infos lval stmt =
  let decl = Cil.extract_varinfos_from_lval lval in
  let dpds, loc = !Db.Value.lval_to_loc_with_deps
                    ~with_alarms:CilE.warn_none_mode
                    (Kstmt stmt)
                    ~deps:Locations.Zone.bottom lval
  in
  let l_loc = Locations.valid_enumerate_bits loc in
  let exact =  Locations.valid_cardinal_zero_or_one loc in
    (l_loc, exact, dpds, decl)

(** process assignment {v lval = exp; v}
    Use the state at ki (before assign)
    and returns the new state (after assign).
  *)
let process_asgn pdg state stmt lval exp =
  let r_dpds = !Db.From.find_deps_no_transitivity (Kstmt stmt) exp in
  let r_decl = Cil.extract_varinfos_from_exp exp in
  let (l_loc, exact, l_dpds, l_decl) = get_lval_infos lval stmt in
  BuildPdg.process_asgn pdg state stmt ~l_loc ~exact ~l_dpds ~l_decl
                                       ~r_dpds ~r_decl

let process_code_annot pdg stmt _annot =
  (* TODO : we could add dependencies to some data if we know how to extract
   * then from the annotation... *)
  BuildPdg.process_skip  pdg stmt

let process_skip pdg stmt = BuildPdg.process_skip  pdg stmt

(** Add a PDG node and its dependencies for each explicit call argument. *)
let process_args pdg st stmt argl =
  let process_one_arg arg =
    let dpds = !Db.From.find_deps_no_transitivity (Kstmt stmt) arg in
    let decl_dpds = Cil.extract_varinfos_from_exp arg in
    (dpds, decl_dpds)
  in let arg_dpds = List.map process_one_arg argl in
    BuildPdg.process_call_args pdg st stmt arg_dpds


(** Add nodes for the call outputs,
   and add the dependencies according to from_table.
   To avoid mixing inputs and outputs, [in_state] is the input state
   and [new_state] the state to modify.
* Process call outputs (including returned value) *)
let call_ouputs  pdg state_before_call state_with_inputs stmt
    lvaloption froms fct_dpds =
  (* be carefull to get every inputs from state_with_inputs
   * to avoid mixing in and out *)
  let froms_deps_return = froms.Function_Froms.deps_return in
  let from_table = froms.Function_Froms.deps_table in
  let print_outputs fmt =
    Format.fprintf fmt "call outputs  : %a"
      Lmap_bitwise.From_Model.pretty from_table;
    if not (lvaloption = None) then
      Format.fprintf fmt "\t and \\result %a@."
        Lmap_bitwise.From_Model.LOffset.pretty froms_deps_return
  in
  Pdg_parameters.debug "%t" print_outputs;
  let new_state =
    match lvaloption with
      | None -> state_before_call
      | Some lval ->
          let r_dpds =
            Lmap_bitwise.From_Model.LOffset.collapse froms_deps_return
          in
          let (l_loc, exact, l_dpds, l_decl) = get_lval_infos lval stmt in
            BuildPdg.process_call_return pdg state_before_call
                                         state_with_inputs stmt
                                         ~l_loc ~exact ~l_dpds ~l_decl
                                         ~r_dpds fct_dpds
  in
  let process_out out (default, from_out) (state, numout) =
    let new_state =
      BuildPdg.process_call_ouput pdg state_with_inputs state stmt
                                  numout out default from_out fct_dpds in
      (new_state, numout+1)
  in
  let (new_state, _num) =
    try Lmap_bitwise.From_Model.fold process_out from_table (new_state, 1)
    with  Lmap_bitwise.From_Model.Cannot_fold -> (* TOP in from_table *)
      process_out Locations.Zone.top (false, Locations.Zone.top) (new_state, 1)
  in new_state

(** process call : {v lvaloption = funcexp (argl); v}
    Use the state at ki (before the call)
    and returns the new state (after the call).
  *)
let process_call pdg state stmt lvaloption funcexp argl =
  let state_before_call = state in
  let _ = BuildPdg.process_call_node pdg stmt in
  let _nb_arg, arg_nodes = process_args pdg state_before_call stmt argl in
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
      BuildPdg.process_call_params pdg state_with_args stmt called_kf arg_nodes
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
  let state_for_each_call = Kernel_function.Set.fold process_simple_call called_functions [] in
  let new_state =
    match state_for_each_call with
    | [] ->
       let stmt_str = Pretty_utils.sfprintf "%a" !Ast_printer.d_stmt stmt in
       Extlib.not_yet_implemented
	 ("pdg with an unknown function call : " ^ stmt_str)
    | st :: [] -> st
    | st :: other_states ->
        let merge s1 s2 =
          let _,s = BuildPdg.test_and_merge_states ~old:s1 s2 in s
        in List.fold_left merge st other_states
  in
  let new_state = match mixed_froms with
    | None -> new_state
    | Some froms ->
          call_ouputs pdg state_before_call new_state
            stmt lvaloption froms funcexp_dpds
  in
  new_state



(** Add a node in the PDG for the conditional statement,
 * and register the statements that are control-dependent on it.
 *)
let process_condition ctrl_dpds_infos pdg state stmt condition =
  (* TODO : test if we met this stmt already to avoid recomputing
             the control dependencies. *)

   (* let's find the locations used in the condition *)
   let loc_cond = !Db.From.find_deps_no_transitivity (Kstmt stmt) condition in
   let decls_cond = Cil.extract_varinfos_from_exp condition in

   (*let cond_val = !Db.Value.access_expr (Kstmt stmt) condition in*)
   let controled_stmts =
     CtrlDpds.get_if_controled_stmts ctrl_dpds_infos stmt
   in
     (*
   let real_dpd =
     let always_false = Locations.Location_Bytes.is_zero cond_val in
       if always_false then false
       else
         let always_true =
           not (Locations.Location_Bytes.intersects
                  cond_val Locations.Location_Bytes.singleton_zero) in
           if always_true then false
           else true
   in
  * We cannot ignore de dependencies, even if [real_dpd = false]
  * because we lose indirect dependencies... (see BTS#181)
  *)
   (* build a node for the condition and store de control dependencies *)
   BuildPdg.process_jump_with_exp pdg stmt controled_stmts
                                  state loc_cond decls_cond

(** let's add a node for e jump statement (goto, break, continue)
   and find the statements which are depending on it.

   Loop are processed like gotos because CIL transformations
   make them {v while(true) body; v} which is equivalent to
   {v L : body ; goto L; v}

   Returns are not handled here, but in {!Build.process_return}.
*)
let process_jump_or_loop_stmt pdg ctrl_dpds_infos jump =
  let controled_stmt_list =
    CtrlDpds.get_jump_controled_stmts ctrl_dpds_infos jump
  in
  BuildPdg.process_jump pdg jump controled_stmt_list

(** [return ret_exp;] is equivalent to [out0 = ret_exp; goto END;]
  * while a simple [return;] is only a [goto END;].
  * Here, we assume that the {{:../html/Oneret.html}Oneret} analysis
  * was used, ie. that it is the only return of the function
  * and that it is the last statement. So, the [goto] is not usefull,
  * and the final state is stored to be used later on to compute the outputs.
  *)
let process_return _current_function pdg state stmt ret_exp =
  let last_state =
      match ret_exp with
        | Some exp ->
            let loc_exp = !Db.From.find_deps_no_transitivity (Kstmt stmt) exp in
            let decls_exp =  Cil.extract_varinfos_from_exp exp in
            BuildPdg.add_retres pdg state stmt loc_exp decls_exp
        | None ->
            let controled_stmt = [] in
              BuildPdg.process_jump pdg stmt controled_stmt;
            state
  in
    BuildPdg.store_last_state pdg last_state

(*
let rec process_labels pdg labels =
    match stmt.labels with
    | [] -> ()
    | label :: tail -> Pdg.process_label pdg label;
                       process_labels pdg labels
*)


(** Computer is a ForwardsTransfer to use ForwardsDataFlow *)
module Computer (Param:sig
                   val current_pdg : BuildPdg.t
                   val ctrl_dpds_infos : CtrlDpds.t
                 end) = struct
  let name = "slicingflow"
  let debug = ref false

  type t = BuildPdg.t_state

  let current_pdg = Param.current_pdg
  let current_function = BuildPdg.get_kf current_pdg

  let ctrl_dpds_infos = Param.ctrl_dpds_infos

  module StmtStartData = struct
    type data = BuildPdg.t_state
    let states = BuildPdg.get_states current_pdg
    let clear () = Inthash.clear states
    let mem = Inthash.mem states
    let find = Inthash.find states
    let replace = Inthash.replace states
    let add = Inthash.add states
    let iter f = Inthash.iter f states
    let length () = Inthash.length states
  end

(*
  (** place to store information at each point of the program during analysis *)
  let stmtStartData: IH.t = BuildPdg.get_states current_pdg
*)
  let stmt_can_reach = Stmts_graph.stmt_can_reach current_function

  let copy (d: t) = d

  let pretty fmt (v: t) =
    Format.fprintf fmt "<STATE>@\n%a@\n<\\STATE>@." BuildPdg.print_state v

  (** Transforme the state before storing it at the point before 'stmt'
      when there is nothing stored yet.
   *)
  let computeFirstPredecessor _stmt state = state

  (** Combine an old state with a new one at the point before 's'.
      Simply 'join' the two states.
      Return None if the new state is already included in the old one
      to stop processing (fix point reached).
   *)
  let combinePredecessors stmt ~old (new_:t) =
    let new_state = computeFirstPredecessor stmt new_ in
    let is_new, new_state = BuildPdg.test_and_merge_states old new_state in
    if is_new then Some new_state
    else
      begin
        (if !debug
         then P.debug "fix point reached for stmt %d" stmt.sid);
        None
      end

  (** Compute the new state after 'instr' starting from state before 'state'.
    *)
  let doInstr stmt instr state =
    !Db.progress ();
    P.debug "doInstr : %a" !Ast_printer.d_instr instr;
    match instr with
    | Set (lv, exp, _) ->
        let new_state = process_asgn current_pdg state stmt lv exp in
        Dataflow.Done new_state
    | Call (lvaloption,funcexp,argl,_) ->
        !Db.progress ();
        let new_state = process_call current_pdg state stmt
                                     lvaloption funcexp argl in
        Dataflow.Done new_state
    | Code_annot (annot, _) ->
        process_code_annot current_pdg stmt annot; Dataflow.Default
    | Skip _ -> process_skip current_pdg stmt ; Dataflow.Default
    | Asm  _ -> P.fatal ~current:true "inline assembly instruction"

  (** Called before processing the successors of the statements.
   *)
  let doStmt (stmt: Cil_types.stmt) (state: t) =
      P.debug "doStmt %d @." stmt.sid ;

    (* labels are processed while processing the jumps.
       process_labels current_pdg labels ;
     *)

    match stmt.skind with
      | Instr _
        -> Dataflow.SDefault

      | Block blk
        -> BuildPdg.process_block current_pdg stmt blk;
           Dataflow.SDefault
      | UnspecifiedSequence seq ->
          BuildPdg.process_block current_pdg stmt
            (Cil.block_from_unspecified_sequence seq);
          Dataflow.SDefault

      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          process_condition ctrl_dpds_infos current_pdg state stmt exp;
          Dataflow.SDefault

      | Return (exp,_) ->
          process_return current_function current_pdg state stmt exp;
          Dataflow.SDefault

      | Continue _
      | Break _
      | Goto _
      | Loop _ ->
          process_jump_or_loop_stmt current_pdg ctrl_dpds_infos stmt;
          Dataflow.SDefault

      | TryExcept (_, _, _, _)
      | TryFinally (_, _, _)
          -> Dataflow.SDefault

  (** Whether to put this statement in the worklist. *)
  let filterStmt stmt = Db.Value.is_accessible (Kstmt stmt)

  let doGuard _ _ _ = Dataflow.GDefault

  let doEdge _ _ d = d

end

(** Compute and return the PDG for the given function *)
let compute_pdg_for_f kf =
  let pdg = BuildPdg.create kf in

  let f_locals, f_stmts =
    try
      let f = Kernel_function.get_definition kf in
        f.slocals, f.sbody.bstmts
    with Kernel_function.No_Definition -> [], []
  in
  let init_state =
    let _ = BuildPdg.process_entry_point pdg f_stmts in
    let formals = Kernel_function.get_formals kf in
    BuildPdg.process_declarations pdg formals f_locals
  in
  let froms = match f_stmts with
  | [] ->
      let state = init_state in
      BuildPdg.store_last_state pdg state ;
      let froms = !Db.From.get kf in
        Some (froms)
  | start :: _ ->
      let ctrl_dpds_infos = CtrlDpds.compute kf in
      let module Computer = Computer (struct
                                        let current_pdg = pdg
                                        let ctrl_dpds_infos = ctrl_dpds_infos
                                      end)
      in
      let module Compute = Dataflow.ForwardsDataFlow(Computer) in
      if Computer.filterStmt start then
        begin
          let init_state = Computer.computeFirstPredecessor start init_state in
          Computer.StmtStartData.add start.sid init_state ;
          Compute.compute [start] ;
          None
        end
      else
        raise
	  (Err_Bot
             (Printf.sprintf "unreachable entry point (sid %d, function %s)"
		start.sid (Kernel_function.get_name kf)))
  in
  let pdg = BuildPdg.finalize_pdg pdg froms in
    pdg

let degenerated top kf =
  P.feedback "%s for function %a"
    (if top then "Top" else "Bottom")
    Kernel_function.pretty_name kf;
  if top then PdgTypes.Pdg.top kf else PdgTypes.Pdg.bottom kf

let compute_pdg kf =
  if not (Db.Value.is_computed ()) then !Db.Value.compute ();

  P.feedback "computing for function %a"
    Kernel_function.pretty_name kf;

  try

    if is_variadic kf then
      Extlib.not_yet_implemented "PDG for a variadic function";

    let pdg = compute_pdg_for_f kf in

    P.feedback "done for function %a"
      Kernel_function.pretty_name kf;

    (* Datascope.compute kf; *)
    pdg

  with
    | Err_Bot what ->
	P.warning "%s" what ;
	degenerated false kf

    | PdgTypes.Pdg_Internal_Error what ->
	P.failure "%s" what ;
	degenerated true kf

    | State.Cannot_fold ->
	P.failure "too imprecise value analysis : abort" ;
	degenerated true kf

    | Extlib.NotYetImplemented why_nyi ->
	P.failure "%s not implemented yet" why_nyi ;
	degenerated true kf

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

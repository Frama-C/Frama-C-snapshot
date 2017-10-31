(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** The aim here is to select the statements where a data D
* has the same value then a given starting program point L. *)

open Cil_types

let cat_rm_asserts_name = "rm_asserts"
let () = Plugin.default_msg_keys [cat_rm_asserts_name]

module R =
  Plugin.Register
    (struct
       let name = "scope"
       let shortname = "scope"
       let help = "data dependencies higher level functions"
     end)

let cat_rm_asserts = R.register_category cat_rm_asserts_name

(** {2 Computing a mapping between zones and modifying statements}
We first go through all the function statements in other to build
a mapping between each zone and the statements that are modifying it.
**)

(** Statement identifier *)
module StmtDefault = struct
  include Cil_datatype.Stmt
  let id s = s.sid
end

(** set of values to store for each data *)
module StmtSetLattice = struct

  include Abstract_interp.Make_Hashconsed_Lattice_Set(StmtDefault)(Cil_datatype.Stmt.Hptset)

  let default: t = empty

  let single s = inject_singleton s

end

(** A place to map each data to the state of statements that modify it. *)
module InitSid = struct
  module LM = Lmap_bitwise.Make_bitwise (StmtSetLattice)
  (* Clear the (non-project compliant) internal caches each time the ast
     changes, which includes every time we switch project. *)
  let () = Ast.add_hook_on_update LM.clear_caches

  let empty = LM.empty
  let find = LM.find

  let add_zone lmap zone sid =
    let new_val = StmtSetLattice.single sid in
    LM.add_binding ~exact:false lmap zone new_val

  let pretty fmt lmap =
    Format.fprintf fmt "Lmap = %a@\n" LM.pretty lmap
end

let get_lval_zones ~for_writing stmt lval =
  let state = Db.Value.get_stmt_state stmt in
  let dpds, zone, exact =
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Locations.Zone.bottom) ~for_writing lval
  in
  dpds, exact, zone

(** Add to [stmt] to [lmap] for all the locations modified by the statement.
* Something to do only for calls and assignments.
* *)
let register_modified_zones lmap stmt =
  let register lmap zone = InitSid.add_zone lmap zone stmt in
  let aux_out kf out =
    let inout= !Db.Operational_inputs.get_internal_precise ~stmt kf in
    Locations.Zone.join out inout.Inout_type.over_outputs
  in
  match stmt.skind with
      | Instr (Set (lval, _, _)) ->
          let _dpds, _, zone =
            get_lval_zones ~for_writing:true  stmt lval
          in
          register lmap zone
      | Instr (Local_init(v, i, _)) ->
        let _, _, zone = get_lval_zones ~for_writing:true stmt (Cil.var v) in
        let lmap_init = register lmap zone in
        (match i with
         | AssignInit _ -> lmap_init
         | ConsInit(f,_,_) ->
           let kf = Globals.Functions.get f in
           let out = aux_out kf Locations.Zone.bottom in
           register lmap_init out)
      | Instr (Call (dst,funcexp,_args,_)) ->
          begin
            let lmap = match dst with
              | None -> lmap
              | Some lval ->
                let _dpds, _, zone =
                  get_lval_zones ~for_writing:true stmt lval
                in
                register lmap zone
            in
            let _, kfs =
              !Db.Value.expr_to_kernel_function ~deps:None (Kstmt stmt) funcexp
            in
            let out =
              Kernel_function.Hptset.fold aux_out kfs Locations.Zone.bottom
            in
            register lmap out
          end
      | _ -> lmap


(** compute the mapping for the function
 * @raise Kernel_function.No_Definition if [kf] has no definition
 *)
let compute kf =
   R.debug ~level:1 "computing for function %a" Kernel_function.pretty kf;
  let f = Kernel_function.get_definition kf in
  let do_stmt lmap s =
    Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s);
    if Db.Value.is_reachable_stmt s
    then register_modified_zones lmap s
    else lmap
  in
  let f_datas = List.fold_left do_stmt InitSid.empty f.sallstmts in
  R.debug ~level:2 "data init stmts : %a" InitSid.pretty f_datas;
    f.sallstmts, f_datas (* TODO : store it ! *)

(** {2 Computing Scopes} *)

module State = struct

  (* The algorithm starts by defining the "modified" function, that
     tells for each statement if it changes the lvalue under
     consideration. We want to add a "temporal" information on top of
     modified, i.e. we want to know for each statement s', whether for
     each path from the starting statement s to s', the lvalue has
     been modified. To make this computable, we overapproximate, and
     the dataflow computes if the statement may have been modified
     (Modif) or has not been modified in any case (SameVal).

     The simple boolean lattice with Modif and SameVal does not
     suffice: if we initialized the dataflow with "SameVal" for all
     statements, "join_and_is_included" would return true and the
     dataflow could stop before having visited all statements. This
     explains why a value of Bottom is needed, to distinguish
     statements not yet visited (or unreachable) from the others.

     Now another problem in the dataflow is the representation of
     loop. In a program such has:

     while(1) {
     s1;
     s2;
     s3;
     s4;
     }

     Where "modified" is false except for s4. We start the forward
     dataflow on s2. We would compute that s2 is not modified, then s3
     is not modified, then s4 is modified, then s1 is modified... but
     then we would compute that s3 and s4 are modified (and indeed,
     they are in further iterations of the loop). To cope with this
     problem, s2 is initialized to the Start state. The Start state is
     not propagated (transfer Start = SameVal), and cannot be removed
     from s2 (Start = Top). Thus the Hasse diagram of the lattice is simply:

     :  Start = Top
     :    |
     :  Modif
     :    |
     :  SameVal
     :    |
     :  NotSeen = Bottom
  *)

  type t = Start | NotSeen | Modif | SameVal

  let pretty fmt b = Format.fprintf fmt "%s" (match b with
                                                | Start -> "Start"
                                                | NotSeen -> "NotSeen"
                                                | Modif -> "Modif"
                                                | SameVal -> "SameVal")

  let bottom = NotSeen

  (* Just compute the "max" between elements of the lattice. *)
  let merge b1 b2 =
    let b = match b1, b2 with
    | Start, _ | _, Start -> Start
    | NotSeen, b | b, NotSeen -> b
    | Modif, _ | _, Modif -> Modif
    | SameVal, SameVal -> SameVal
    in b
  let join = merge;;

  let equal (b1 : t) (b2: t) = (b1 = b2)

  let join_and_is_included a b =
    let j = join a b in
    (j, equal j b)

  let is_included a b = snd (join_and_is_included a b)

  (* Note: the transfer function "if m = Start then SameVal else if
     modif then Modif else m" suits better visualisation by scope,
     since it does not consider the "current statement" as
     "modifying". But this gives incorrect results for
     remove-redundant-alarms. *)
  let transfer modif m =
    if modif then Modif else if m = Start then SameVal else m

end

module BackwardScope (X : sig val modified : stmt -> bool end ) = struct

  let transfer_stmt stmt state = match stmt.skind  with
    | Instr _ -> State.transfer (X.modified stmt) state
    | _ -> state

  include State

end

let backward_data_scope modif_stmts s kf =
  let modified s = StmtSetLattice.mem s modif_stmts in
  let module Fenv = (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV) in
  let module Arg = struct
      include BackwardScope(struct let modified = modified end)
      let init = [(s,State.Start)];;
  end in
  let module Compute = Dataflows.Simple_backward(Fenv)(Arg) in
  Compute.pre_state
;;

module ForwardScope (X : sig
    (* Effects of the statement itself *)
    val modified : stmt -> bool
    (* Effects of scope change *)
    val modified_by_edge: stmt -> stmt -> bool
  end) =
struct
  include State;;

  let transfer_stmt s state =
    let map_on_all_succs new_state =
      let do_succ s' =
        (s', State.transfer (X.modified_by_edge s s') new_state)
      in
      List.map do_succ s.succs
    in
    match s.skind with
    | Instr _ -> map_on_all_succs (State.transfer (X.modified s) state)
    | If _ | Switch _ -> map_on_all_succs (State.transfer false state)

    | Return _ | Throw _ -> []

    | UnspecifiedSequence _ | Loop _ | Block _
    | Goto _ | Break _ | Continue _
    | TryExcept _ | TryFinally _ | TryCatch _
      -> map_on_all_succs state
  ;;

end

let forward_data_scope modif_stmts modif_edge s kf =
  let modified s = StmtSetLattice.mem s modif_stmts in
  let module Fenv = (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV) in
  let module Arg = struct
    include ForwardScope(struct
        let modified = modified
        let modified_by_edge = modif_edge
      end)
    let init = [(s,State.Start)];;
  end in
  let module Compute = Dataflows.Simple_forward(Fenv)(Arg) in
  Compute.pre_state, Compute.post_state
;;

(* Add only 'simple' statements. *)
let add_s s acc =
  match s.skind with
    | Instr _ | Return _ | Continue _ | Break _ | Goto _ | Throw _
        -> Cil_datatype.Stmt.Hptset.add s acc
    | Block _ | Switch _ | If _ | UnspecifiedSequence _ | Loop _
    | TryExcept _ | TryFinally _ | TryCatch _
        -> acc

(** Do backward and then forward propagations and compute the 3 statement sets :
* - forward only,
* - forward and backward,
* - backward only.
*)
let find_scope allstmts modif_stmts modif_edge s kf =
  (* Add only statements for which the lvalue certainly did not change. *)
  let add get_state acc s =
    match get_state s with
    | State.Start | State.SameVal -> add_s s acc
    | _ -> acc
  in
  let _, fw_post = forward_data_scope modif_stmts modif_edge s kf in
  let fw = List.fold_left (add fw_post) Cil_datatype.Stmt.Hptset.empty allstmts in
  let bw_pre = backward_data_scope modif_stmts s kf in
  let bw = List.fold_left (add bw_pre)  Cil_datatype.Stmt.Hptset.empty allstmts in
  let fb = Cil_datatype.Stmt.Hptset.inter bw fw in
  let fw = Cil_datatype.Stmt.Hptset.diff fw fb in
  let bw = Cil_datatype.Stmt.Hptset.diff bw fb in
  fw, fb, bw

(* Computes the memory zones that points to a base in [escaping] in a state. *)
let gather_escaping_zones escaping = function
  | Cvalue.Model.Top -> Locations.Zone.top
  | Cvalue.Model.Bottom -> Locations.Zone.bottom
  | Cvalue.Model.Map m ->
    let aux base offsm zone =
      let test b = Base.Hptset.mem b escaping in
      let gather (_, _ as itv) (v, _, _) acc =
        let v = Cvalue.V_Or_Uninitialized.get_v v in
        if Cvalue.V.contains_addresses_of_locals test v
        then
          let z = Locations.Zone.inject base (Int_Intervals.inject_itv itv) in
          Locations.Zone.join acc z
        else acc
      in
      Cvalue.V_Offsetmap.fold gather offsm zone
    in
    Cvalue.Model.fold aux m Locations.Zone.bottom

(* compute the memory zones that are changed into ESCAPING ADDRESS
   when taking the cfg edge s1->s2 *)
let compute_escaping_zones s1 s2 =
  let closed_blocks = Kernel_function.blocks_closed_by_edge s1 s2 in
  let locals = List.flatten (List.map (fun b -> b.blocals) closed_blocks) in
  let filter acc v =
    if v.vtemp || not v.vreferenced
    then acc else Base.Hptset.add (Base.of_varinfo v) acc
  in
  let bases = List.fold_left filter Base.Hptset.empty locals in
  if Base.Hptset.is_empty bases
  then Locations.Zone.bottom
  else gather_escaping_zones bases (Db.Value.get_stmt_state s1)

(* type pair_stmts = stmt * stmt *)
module PairStmts =
  Datatype.Pair_with_collections
    (Cil_datatype.Stmt)(Cil_datatype.Stmt)
    (struct let module_name = "Scope.Datascope.PairStmts" end)

(* Hashtbl from pairs of stmts to zone. Used as maps from Cfg edges to the
   memory zones that are 'modified' by thescope change. *)
module HashPairStmtsZone =
  PairStmts.Hashtbl.Make(Locations.Zone)
type modified_by_edge = HashPairStmtsZone.t

(* compute the {!modified_by_edge} hashtbl for the fundec [fdec] *)
let compute_modif_edge fdec : modified_by_edge =
  let modifs_edge = PairStmts.Hashtbl.create 17 in
  let do_stmt stmt =
    let do_succ stmt' =
      let z = compute_escaping_zones stmt stmt' in
      PairStmts.Hashtbl.add modifs_edge (stmt, stmt') z
    in
    List.iter do_succ stmt.succs
  in
  List.iter do_stmt fdec.sallstmts;
  modifs_edge

module ModifEdge =
  Cil_state_builder.Kernel_function_hashtbl(HashPairStmtsZone)
    (struct
      let name = "Scope.Datatscope.ModifsEdge"
      let dependencies = [Db.Value.self]
      let size = 16
    end)

let modified_by_edge_kf =
  ModifEdge.memo
    (fun kf -> compute_modif_edge (Kernel_function.get_definition kf))

(* Does the Cfg edge [s1->s2] has an effect on [z]? *)
let is_modified_by_edge kf z s1 s2 =
  let modifs_edge = modified_by_edge_kf kf in
  Locations.Zone.intersects z (PairStmts.Hashtbl.find modifs_edge (s1, s2))

(** Try to find the statement set where [data] has the same value than
* before [stmt].
 * @raise Kernel_function.No_Definition if [kf] has no definition
 *)
let get_data_scope_at_stmt kf stmt lval =
  let dpds, _, zone = get_lval_zones ~for_writing:false stmt lval in
  (* TODO : is there something to do with 'exact' ? *)
  let zone = Locations.Zone.join dpds zone in
  let allstmts, info = compute kf in
  let modif_stmts = InitSid.find info zone in
  let modifs_edge = is_modified_by_edge kf zone in
  let (f_scope, fb_scope, b_scope) =
    find_scope allstmts modif_stmts modifs_edge stmt kf
  in
  R.debug
    "@[<hv 4>get_data_scope_at_stmt %a at %d @\n\
                   modified by = %a@\n\
                   f = %a@\nfb = %a@\nb = %a@]"
      (* stmt at *)
    Locations.Zone.pretty zone stmt.sid
      (* modified by *)
    (Pretty_utils.pp_iter
       StmtSetLattice.iter ~sep:",@ " Cil_datatype.Stmt.pretty_sid)
    modif_stmts
      (* scope *)
    Cil_datatype.Stmt.Hptset.pretty f_scope
    Cil_datatype.Stmt.Hptset.pretty fb_scope
    Cil_datatype.Stmt.Hptset.pretty b_scope;
  (f_scope, (fb_scope, b_scope))

exception ToDo

let get_annot_zone kf stmt annot =
    let add_zone z info =
      let s = info.Db.Properties.Interp.To_zone.ki in
      let before = info.Db.Properties.Interp.To_zone.before in
      let zone = info.Db.Properties.Interp.To_zone.zone in
        R.debug ~level:2 "[forward_prop_scope] need %a %s stmt %d@."
          Locations.Zone.pretty zone
          (if before then "before" else "after") s.sid;
        if before && stmt.sid = s.sid then
          Locations.Zone.join zone z
        else (* TODO *)
          raise ToDo
    in
    let (info, _), _ =
      !Db.Properties.Interp.To_zone.from_stmt_annot annot (stmt, kf)
    in
    match info with
      | None -> raise ToDo
      | Some info ->
          let zone = List.fold_left add_zone Locations.Zone.bottom info in
            R.debug "[get_annot_zone] need %a" Locations.Zone.pretty zone ;
            zone


module CA_Map = Cil_datatype.Code_annotation.Map

type proven = (stmt * code_annotation * stmt) CA_Map.t
(** Type of the properties proven so far. A binding
    [ca -> (stmt_ca, ca_because, stmt_because)] must be read as "[ca] at
    statement [stmt_ca] is a logical consequence of [ca_because] at statement
    [stmt_because]".
    Currently, [ca] and [ca_because] are always exactly the same ACSL assertion,
    although this may be extended in the future. *)

(** Assertions proven so far, as a list *)
let list_proven (m:proven) =
  CA_Map.fold (fun ca _ acc -> ca :: acc) m []

(** [add_proven_annot proven because] add the fact that [proven] is proven
    thanks to [because]. This function also returns a boolean indicating
    that [proven] was not already proven. *)
let add_proven_annot (ca, stmt_ca) (ca_because, stmt_because) acc =
  if CA_Map.mem ca acc then
    (* already proven *)
    acc, false
  else
    CA_Map.add ca (stmt_ca, ca_because, stmt_because) acc, true

(** Check if an assertion at [stmt] is identical to [ca] (itself emitted
    at [stmt_ca]). Add them to acc if any *)
let check_stmt_annots (ca, stmt_ca) stmt acc =
  let check _ annot acc =
    match ca.annot_content, annot.annot_content with
    | AAssert (_, p'), AAssert (_, p) ->
        if Logic_utils.is_same_predicate_node p.pred_content p'.pred_content then
          let acc, added = add_proven_annot (annot, stmt) (ca, stmt_ca) acc in
          if added then
            R.debug "annot at stmt %d could be removed: %a"
              stmt.sid Printer.pp_code_annotation annot;
          acc
        else
          acc
    | _ -> acc
  in
  Annotations.fold_code_annot check stmt acc

exception VolatileFound

(* This visitor detects the presence of a volatile logic l-value. Such a
   l-value may evaluate differently at different program point. *)
class containsVolatile = object
  inherit Visitor.frama_c_inplace

  method! vterm t =
    match t.term_node with
    | TLval tlv -> begin
        match Logic_utils.unroll_type (Cil.typeOfTermLval tlv) with
        | Ctype typ ->
          if Cil.typeHasQualifier "volatile" typ then raise VolatileFound
        | _ -> ()
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren

end

let code_annot_is_volatile ca =
  let vis = new containsVolatile in
  try ignore (Visitor.visitFramacCodeAnnotation vis ca); false
  with VolatileFound -> true

(** Return the set of stmts ([scope]) where [annot] has the same value
   as at [stmt], and adds to [proven] the annotations that are identical to
   [annot] at statements that are both in [scope] and dominated by [stmt].
    [stmt] is not added to the set, and [annot] is not added to [proven]. *)
let get_prop_scope_at_stmt ~warn kf stmt ?(proven=CA_Map.empty) annot =
  R.debug "[get_prop_scope_at_stmt] at stmt %d in %a : %a"
    stmt.sid Kernel_function.pretty kf
    Printer.pp_code_annotation annot;
  let acc = (Cil_datatype.Stmt.Hptset.empty, proven) in
  if code_annot_is_volatile annot then acc
  else
  try
    let zone =  get_annot_zone kf stmt annot in
    let allstmts, info = compute kf in
    let modif_stmts = InitSid.find info zone in
    let modifs_edge = is_modified_by_edge kf zone in
    let pre_state, _ = forward_data_scope modif_stmts modifs_edge stmt kf in
    begin match annot.annot_content with
      | AAssert _ -> ()
      | _ -> R.abort "only 'assert' are handled by get_prop_scope_at_stmt"
    end;
    let add ((acc_scope, acc_to_be_rm) as acc) s = match pre_state s with
      | State.SameVal ->
        if Dominators.dominates stmt s && not (Cil_datatype.Stmt.equal stmt s)
        then
          let acc_scope = add_s s acc_scope in
          let acc_to_be_rm = check_stmt_annots (annot, stmt) s acc_to_be_rm in
          (acc_scope, acc_to_be_rm)
        else acc
      | _ -> acc
    in
    List.fold_left add acc allstmts
  with ToDo ->
    if warn then
      R.warning ~current:true ~once:true
        "[get_annot_zone] don't know how to compute zone: skip this annotation";
    acc

(** Collect the annotations that can be removed because they are redundant. *)
class check_annot_visitor = object(self)

  inherit Visitor.frama_c_inplace

  val mutable proven = CA_Map.empty

  method proven () = proven

  method! vcode_annot annot =
    let kf = Extlib.the self#current_kf in
    let stmt =
      Cil.get_original_stmt self#behavior (Extlib.the self#current_stmt)
    in
    begin match annot.annot_content with
      | AAssert (_, _) ->
        R.debug ~level:2 "[check] annot %d at stmt %d in %a : %a@."
          annot.annot_id stmt.sid Kernel_function.pretty kf
          Printer.pp_code_annotation annot;
        let _scope, proven' =
          get_prop_scope_at_stmt ~warn:false kf stmt ~proven annot
        in
        proven <- proven'
      | _ -> ()
    end;
    Cil.SkipChildren

  method! vglob_aux g = match g with
    | GFun (fdec, _loc) when
        !Db.Value.is_called (Extlib.the self#current_kf) &&
        not (!Db.Value.no_results fdec)
        ->
      Cil.DoChildren
    | _ -> Cil.SkipChildren

  method! vexpr _ = Cil.SkipChildren

end (* class check_annot_visitor *)

let redundant_assertions () =
  let visitor = new check_annot_visitor in
  ignore (Visitor.visitFramacFile
            (visitor:>Visitor.frama_c_visitor)
            (Ast.get ()));
  visitor#proven ()

let check_asserts () =
  R.feedback "check if there are some redundant assertions...";
  let to_be_removed = redundant_assertions () in
  let n = CA_Map.cardinal to_be_removed in
    R.result "[check_asserts] %d assertion(s) could be removed@." n;
    (list_proven to_be_removed)

(* erasing optional arguments, plus return a list*)
let get_prop_scope_at_stmt kf stmt annot =
  let s, m = get_prop_scope_at_stmt ~warn:true kf stmt annot in
  s, list_proven m

(* Currently lazy, because we need to define it after Value as been registered
   in Db *)
let emitter = lazy (
  let conv = List.map Typed_parameter.get in
  let correctness = conv (Emitter.correctness_parameters !Db.Value.emitter) in
  let tuning = conv (Emitter.tuning_parameters !Db.Value.emitter) in
  Emitter.create "RedundantAlarms" [Emitter.Property_status]
    ~correctness ~tuning)

(** Mark as proved the annotations collected by [check_asserts]. *)
let rm_asserts () =
  let to_be_removed = redundant_assertions () in
  let n = CA_Map.cardinal to_be_removed in
  if n > 0 then begin
    R.feedback ~dkey:cat_rm_asserts "removing %d assertion(s)@." n;
    let aux ca (stmt_ca, ca_because, stmt_because) =
      let loc = Cil_datatype.Stmt.loc stmt_ca in
      R.result ~source:(fst loc) ~dkey:cat_rm_asserts ~level:2
        "@[removing redundant@ %a@]" Printer.pp_code_annotation ca;
      let kf = Kernel_function.find_englobing_kf stmt_ca in
      let ip_ca = Property.ip_of_code_annot_single kf stmt_ca ca in
      let ip_because =
        Property.ip_of_code_annot_single kf stmt_because ca_because
      in
      let e = Lazy.force emitter in
      Property_status.emit e ~hyps:[ip_because] ip_ca Property_status.True
    in
    CA_Map.iter aux to_be_removed
  end

let get_data_scope_at_stmt =
  Journal.register
    "Scope.Datascope.get_data_scope_at_stmt"
    (Datatype.func3
          Kernel_function.ty
          Cil_datatype.Stmt.ty
          Cil_datatype.Lval.ty
          (Datatype.pair
             Cil_datatype.Stmt.Hptset.ty
             (Datatype.pair Cil_datatype.Stmt.Hptset.ty
                            Cil_datatype.Stmt.Hptset.ty)))
    get_data_scope_at_stmt

let get_prop_scope_at_stmt =
  Journal.register
    "Scope.Datascope.get_prop_scope_at_stmt"
    (Datatype.func3
       Kernel_function.ty
       Cil_datatype.Stmt.ty
       Cil_datatype.Code_annotation.ty
       (Datatype.pair
          (Cil_datatype.Stmt.Hptset.ty)
          (Datatype.list Cil_datatype.Code_annotation.ty)))
    get_prop_scope_at_stmt

let check_asserts =
  Journal.register
    "Scope.Datascope.check_asserts"
    (Datatype.func Datatype.unit (Datatype.list Cil_datatype.Code_annotation.ty))
    check_asserts

let rm_asserts =
  Journal.register
    "Scope.Datascope.rm_asserts"
    (Datatype.func Datatype.unit Datatype.unit)
    rm_asserts

let () =
  Db.register
    (Db.Journalize
       ("Value.rm_asserts", Datatype.func Datatype.unit Datatype.unit))
    Db.Value.rm_asserts rm_asserts

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

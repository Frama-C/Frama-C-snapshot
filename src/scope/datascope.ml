(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(** The aim here is to select the statements where a data D
* has the same value then a given starting program point L. *)

open Cil_types
open Db_types

module R : Plugin.S =  
  Plugin.Register
    (struct
       let name = "scope"
       let shortname = "scope"
       let descr = "data dependencies higher level functions"
     end)

(** {2 Computing a mapping between zones and modifying statements}
We first go through all the function statements in other to build
a mapping between each zone and the statements that are modifying it.
**)

(** Statement identifier *)
module Sid = struct
  type t = int
  let compare = compare
  let hash x = x
  let id x = x
  let default = -1
  let pretty fmt v = Format.fprintf fmt "sid:%d" v
  module Datatype = Datatype.Int
end

(** set of values to store for each data *)
module SidSet = struct

  include Abstract_interp.Make_Lattice_Set (Sid)

  let default _v _a _b : t = inject_singleton Sid.default
  let defaultall _v : t = inject_singleton Sid.default

  let empty = bottom
  let cardinal set = fold (fun _ n -> n+1) set 0
  let single sid = inject_singleton sid

  let to_list ~keep_default set =
    fold (fun n l -> if (n = Sid.default) && not keep_default then l else n::l)
      set []

  let add sid set = join set (single sid)
end

(** A place to map each data to the state of statements that modify it. *)
module InitSid = struct
  module LM = Lmap_bitwise.Make_bitwise (SidSet)

  type t = LM.t

  let empty = LM.empty
  let find = LM.find

  let add_zone ~exact lmap zone sid =
    let new_val = SidSet.single sid in
    let lmap = LM.add_binding exact lmap zone new_val in
      lmap

  let test_and_merge old_lmap new_lmap =
    let new_lmap = LM.join old_lmap new_lmap in
      if LM.is_included new_lmap old_lmap then None
      else Some new_lmap

  let pretty fmt lmap =
    Format.fprintf fmt "Lmap = %a@\n" LM.pretty lmap
end

let get_lval_zones stmt lval =
  let dpds, loc = !Db.Value.lval_to_loc_with_deps
                    ~with_alarms:CilE.warn_none_mode
                    (Kstmt stmt)
                    ~deps:Locations.Zone.bottom lval
  in
  let zone = Locations.valid_enumerate_bits loc in
  let exact =  Locations.valid_cardinal_zero_or_one loc in
    dpds, exact, zone

(** Add to [stmt] to [lmap] for all the locations modified by the statement.
* Something to do only for calls and assignments.
* *)
let register_modified_zones lmap stmt inst =
  let register lmap zone =
    (* [exact] should always be false because we want to store all the stmts *)
    InitSid.add_zone ~exact:false lmap zone stmt.sid
  in
  let process_froms lmap froms =
    let from_table = froms.Function_Froms.deps_table in
      try Lmap_bitwise.From_Model.fold
            (fun out _ lmap -> register lmap out) from_table lmap
      with Lmap_bitwise.From_Model.Cannot_fold ->
        (R.debug ~level:1 "register_modified_zones : top on stmt(%d) : %a@."
          stmt.sid !Ast_printer.d_stmt stmt;
        register lmap Locations.Zone.top)
  in
    match inst with
      | Set (lval, _, _) ->
          let _dpds, _exact, zone = get_lval_zones stmt lval in
            register lmap zone
      | Call (lvaloption,funcexp,_args,_) ->
          begin
            let lmap = match lvaloption with None -> lmap
              | Some lval ->
                  let _dpds, _exact, zone = get_lval_zones stmt lval in
                    register lmap zone
            in
              try
                let froms = !Db.From.Callwise.find (Kstmt stmt) in
                  process_froms lmap froms
              with Not_found -> (* don't have callwise (-calldeps option) *)
                let _funcexp_dpds, called_functions =
                  !Db.Value.expr_to_kernel_function
                    ~with_alarms:CilE.warn_none_mode
                    (Kstmt stmt) ~deps:(Some Locations.Zone.bottom) funcexp
                in
                  List.fold_left
                    (fun lmap kf -> process_froms lmap (!Db.From.get kf))
                    lmap called_functions
          end
      | _ -> lmap


(** compute the mapping for the function
 * @raise Kernel_function.No_Definition if [kf] has no definition
 *)
let compute kf =
   R.debug ~level:1 "computing for function %a" Kernel_function.pretty_name kf;
  let f = Kernel_function.get_definition kf in
  let do_stmt lmap s =
    if Db.Value.is_accessible (Kstmt s) then
      match s.skind with
        | Instr i -> register_modified_zones lmap s i
        | _ -> lmap
    else lmap
  in
  let f_datas = List.fold_left do_stmt InitSid.empty f.sallstmts in
  R.debug ~level:2 "data init stmts : %a" InitSid.pretty f_datas;
    f.sallstmts, f_datas (* TODO : store it ! *)

(** {2 Computing Scopes} *)

module State = struct
  type t = Start | NotSeen | Modif | SameVal

  let pretty fmt b = Format.fprintf fmt "%s" (match b with
                                                | Start -> "Start"
                                                | NotSeen -> "NotSeen"
                                                | Modif -> "Modif"
                                                | SameVal -> "SameVal")
  let merge b1 b2 =
    let b = match b1, b2 with
    | Start, _ | _, Start -> Start
    | NotSeen, b | b, NotSeen -> b
    | Modif, _ | _, Modif -> Modif
    | SameVal, SameVal -> SameVal
    in b

  let equal b1 b2 = (b1 = b2)

  let test_and_merge ~old new_ =
    let result = merge new_ old in
    if equal result old then None else Some result

  let transfer modif m =
    if modif then Modif else if m = Start then SameVal else m

end

(** Place to store the dataflow analyses results *)
module GenStates (S : sig
                    type t
                    val pretty : Format.formatter -> t -> unit
                  end)
  = struct
  type data = S.t
  type t = data Inthash.t

  let states:t = Inthash.create 50
  let clear () = Inthash.clear states

  let add = Inthash.add states
  let find = Inthash.find states
  let mem = Inthash.mem states
  let find = Inthash.find states
  let replace = Inthash.replace states
  let add = Inthash.add states
  let iter f = Inthash.iter f states
  let fold f = Inthash.fold f states

  let pretty fmt infos =
    Inthash.iter
      (fun k v -> Format.fprintf fmt "Stmt:%d\n%a\n======" k S.pretty v)
      infos
end

module States = GenStates (State)

module BackwardScope (X : sig val modified : stmt -> bool end ) = struct
  let name = "scope(back)"
  let debug = ref false

  module StmtStartData = States

  type t = StmtStartData.data
  let pretty = State.pretty

  let combineStmtStartData _stmt ~old new_ = State.test_and_merge ~old new_

  let combineSuccessors s1 s2 = State.merge s1 s2

  let doStmt _stmt = Dataflow.Default

  let doInstr stmt _instr m_after =
    Dataflow.Done (State.transfer (X.modified stmt) m_after)

  let filterStmt _stmt _next = true

  let funcExitData = State.NotSeen
end

let backward_data_scope allstmts modif_stmts s =
  States.clear ();
  List.iter (fun s -> States.add s.sid State.NotSeen) allstmts;
  let modified s = SidSet.mem s.sid modif_stmts in
  States.replace s.sid State.Start;
  let stmts = s.preds in
  let module Computer = BackwardScope (struct let modified = modified end) in
  let module Compute = Dataflow.BackwardsDataFlow(Computer) in
  Compute.compute stmts

module ForwardScope (X : sig val modified : stmt -> bool end ) = struct
  let name = "scope(forward)"
  let debug = ref false

  module StmtStartData = States

  type t = StmtStartData.data
  let pretty = State.pretty
  let copy (s:t) = s

  let computeFirstPredecessor _stmt state =
    if state = State.Start then State.SameVal else state

  let combinePredecessors _stmt ~old new_ =
    assert (R.verify (new_ <> State.Start) 
              "forward traversal shouldn't go through Start !");
    State.test_and_merge ~old new_

  let doStmt _stmt _state = Dataflow.SDefault

  let doInstr stmt _ m_before =
    Dataflow.Done (State.transfer (X.modified stmt) m_before)

  let stmt_can_reach _ _ = true
  let filterStmt _stmt = true

  let doGuard _ _ _ = Dataflow.GDefault

  let doEdge _ _ d = d
end

let forward_data_scope modif_stmts s =
  States.clear ();
  let modified s = SidSet.mem s.sid modif_stmts in
  let module Computer = ForwardScope (struct let modified = modified end) in
  let module Compute = Dataflow.ForwardsDataFlow(Computer) in
    States.replace s.sid State.Start;
    Compute.compute [s]

let add_s sid acc =
  let s, _ = Kernel_function.find_from_sid sid in
    (* we add only 'simple' statements *)
    match s.skind with
      | Instr _ | Return _ | Continue _ | Break _ | Goto _
        -> Cilutil.StmtSet.add s acc
      | Block _ | Switch _ | If _ | UnspecifiedSequence _ | Loop _
      | TryExcept _ | TryFinally _
        -> acc

(** Do backward and then forward propagations and compute the 3 statement sets :
* - forward only,
* - forward and backward,
* - backward only.
*)
let find_scope allstmts modif_stmts s =
  let add fw sid x acc =
    match x with
      | State.Start ->
          if fw then add_s sid acc
          else
            let x =
              List.fold_left (fun x s -> State.merge x (States.find s.sid))
                State.NotSeen s.succs
            in let x = State.transfer (SidSet.mem sid modif_stmts) x in
              if x = State.SameVal then add_s sid acc else acc
      | State.SameVal -> add_s sid acc
      | _ -> acc
  in
  let _ = backward_data_scope allstmts modif_stmts s in
  let bw = States.fold (add false) Cilutil.StmtSet.empty in

  let _ = forward_data_scope modif_stmts s in
  let fw = States.fold (add true) Cilutil.StmtSet.empty in

  let fb = Cilutil.StmtSet.inter bw fw in
  let fw = Cilutil.StmtSet.diff fw fb in
  let bw = Cilutil.StmtSet.diff bw fb in
    fw, fb, bw

(** Try to find the statement set where [data] has the same value than
* before [stmt].
 * @raise Kernel_function.No_Definition if [kf] has no definition
 *)
let get_data_scope_at_stmt kf stmt lval =
  let dpds, _exact, zone = get_lval_zones stmt lval in
  (* TODO : is there something to do with 'exact' ? *)
  let zone = Locations.Zone.join dpds zone in
  let allstmts, info = compute kf in
  let modif_stmts = InitSid.find info zone in
  let (f_scope, fb_scope, b_scope) as all = 
    find_scope allstmts modif_stmts stmt 
  in
    R.debug
      "@[<hv 4>get_data_scope_at_stmt %a at %d @\n\
                   modified by = %a@\n\
                   f = %a@\nfb = %a@\nb = %a@]"
      (* stmt at *)
      Locations.Zone.pretty zone stmt.sid
      (* modified by *)
      (Cilutil.print_list Cilutil.space Sid.pretty) 
      (SidSet.to_list ~keep_default:false modif_stmts)
      (* scope *)
      Cilutil.StmtSet.pretty f_scope
      Cilutil.StmtSet.pretty fb_scope
      Cilutil.StmtSet.pretty b_scope;
  all

exception ToDo

let get_annot_zone kf stmt annot =
  try
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
      !Db.Properties.Interp.To_zone.from_stmt_annot annot
        ~before:true (stmt, kf)
    in
    let zone = List.fold_left add_zone Locations.Zone.bottom info in
      R.debug "[get_annot_zone] need %a" Locations.Zone.pretty zone ;
      zone
  with Extlib.NotYetImplemented _ | ToDo ->
      begin
        R.warning
	  "[get_annot_zone] don't know how to compute zone: skip this annotation";
        raise ToDo
      end

(** add [annot] to [acc] if it is not already in.
  * Return [true] if it has been added.
  * [acc] is supposed to be sorted according to [annot_id].
  * *)
let rec add_annot annot acc =
    match acc with
      | [] -> [annot], true
      | a::tl ->
          if  annot.annot_id < a.annot_id then annot::acc, true
          else if annot.annot_id = a.annot_id then acc, false
          else
            let tl, added = add_annot annot tl in
              a::tl, added

(** Check if some assertions before [s] are identical to [pred].
  * Add them to acc if any *)
let check_stmt_annots pred s acc =
  let check acc annot =
    match annot with
      | Before (AI (_, ({annot_content= AAssert (_, p, _) } as annot))) ->
          if Logic_utils.is_same_named_predicate p pred
          then begin
            let acc, added = add_annot annot acc in
              if added then
                R.debug "annot at stmt %d could be removed: %a"
                  s.sid !Ast_printer.d_code_annotation annot;
              acc
          end
          else acc
      | _ -> acc
  in
  List.fold_left check acc (Annotations.get_filter Logic_utils.is_assert s)

(** Return the set of stmts (scope) where [annot] has the same value
  * than in [stmt]
  * and add to [to_be_removed] the annotations that are identical to [annot]
  * in the statements that are both the scope and that are dominated by stmt.
  * *)
let get_prop_scope_at_stmt kf stmt ?(to_be_removed=[]) annot =
  R.debug "[get_prop_scope_at_stmt] at stmt %d in %a : %a"
    stmt.sid Kernel_function.pretty_name kf
    !Ast_printer.d_code_annotation annot;

  let sets = (Cilutil.StmtSet.empty, to_be_removed) in
    try
      let zone =  get_annot_zone kf stmt annot in

      let _allstmts, info = compute kf in
      let modif_stmts = InitSid.find info zone in
      let _ = forward_data_scope modif_stmts stmt in
      let pred = match annot.annot_content with
        | AAssert (_, p, _) -> p
        | _ -> R.abort "only 'assert' are handeled here"
      in
      let add sid x ((acc_scope, acc_to_be_rm) as acc) =
        match x with
          | State.Start -> (add_s sid acc_scope, acc_to_be_rm)
          | State.SameVal ->
              let s, _ = Kernel_function.find_from_sid sid in
                if !Db.Dominators.is_dominator kf ~opening:stmt ~closing:s
                then begin
                  let acc_scope = add_s sid acc_scope in
                  let acc_to_be_rm = check_stmt_annots pred s acc_to_be_rm in
                    (acc_scope, acc_to_be_rm)
                end
                else acc
          | _ -> acc
      in
      let sets = States.fold add sets in
        sets
    with ToDo -> sets

(** Collect the annotations that can be removed because they are redondant. *)
class check_annot_visitor = object(self)

  inherit Visitor.generic_frama_c_visitor
            (Project.current ()) (Cil.inplace_visit ()) as super

  val mutable to_be_removed = []

  method get_to_be_removed () = to_be_removed
  method to_be_removed = to_be_removed

  method vcode_annot annot =
    let kf = Extlib.the self#current_kf in
    let stmt =
      Cil.get_original_stmt self#behavior (Cilutil.valOf self#current_stmt)
    in
    let before = self#is_annot_before in
    let _ = match annot.annot_content with
        | AAssert (_, _, _) ->
            if before then begin
              R.debug ~level:2 "[check] annot %d at stmt %d in %a : %a@."
                annot.annot_id stmt.sid Kernel_function.pretty_name kf
                !Ast_printer.d_code_annotation annot;
              let _, added = add_annot annot to_be_removed in
                (* just check if [annot] is in [to_be_removed] :
                 * don't add it... *)
                if added then (* annot is not already removed *)
                  let _scope, rem =
                    get_prop_scope_at_stmt kf stmt ~to_be_removed annot
                  in to_be_removed <- rem
            end
        | _ -> ()
    in Cil.SkipChildren

  method vglob_aux g = match g with
    | GFun (_, _loc) when !Db.Value.is_called (Extlib.the self#current_kf) ->
        Cil.DoChildren
    | _ -> Cil.SkipChildren


end (* class check_annot_visitor *)

let f_check_asserts () =
  let visitor = new check_annot_visitor in
  ignore (Visitor.visitFramacFile 
            (visitor:>Visitor.frama_c_visitor)
            (Ast.get ()));
  visitor#get_to_be_removed () 

let check_asserts () =
  R.feedback "check if there are some redondant assertions...";
  let to_be_removed = f_check_asserts () in
  let n = List.length to_be_removed in
    R.result "[check_asserts] %d assertion(s) could be removed@." n;
    to_be_removed

(* erasing optional arguments *)
let get_prop_scope_at_stmt kf stmt annot = get_prop_scope_at_stmt kf stmt annot

(** Visitor to remove the annotations collected by [check_asserts].
  * In fact, it changes them to [assert true;]
  * *)
class rm_annot_visitor to_be_removed = object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vcode_annot annot =
    let _, not_in = add_annot annot to_be_removed in
    if not_in then (* not to be removed *)  Cil.SkipChildren
    else (* is to be removed *)
      match annot.annot_content with
      | AAssert (_, p, _) ->
          R.debug ~level:2 "[rm_asserts] removing redundant %a@." Cil.d_code_annotation annot;
          let status = Checked {emitter="scope"; valid = Maybe} in
          let p = { p with content = Ptrue } in
          let aassert = AAssert ([], p, {status = status}) in
          let annot = { annot with annot_content = aassert } in
          Cil.ChangeTo annot
      | _ -> Cil.SkipChildren

end

(** Remove  the annotations collected by [check_asserts]. *)
let rm_asserts () =
  let to_be_removed = f_check_asserts () in
  let n = List.length to_be_removed in
    (if n > 0 then
      R.feedback "[rm_asserts] removing %d assertion(s)@." n);
  let visitor = new rm_annot_visitor to_be_removed in
  ignore (Visitor.visitFramacFile (visitor:>Visitor.frama_c_visitor)
            (Ast.get ()))


(** Register external functions into Db. *)
let () =
  Db.Scope.get_data_scope_at_stmt := get_data_scope_at_stmt;
  Db.Scope.get_prop_scope_at_stmt := get_prop_scope_at_stmt;
  Db.Scope.check_asserts := check_asserts;
  Db.Scope.rm_asserts := rm_asserts;


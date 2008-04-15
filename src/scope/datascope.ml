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

(** The aim here is to select the statements where a data D
* has the same value then a given starting program point L. *)

open Cil_types 
open Db_types 

let debug n format =
  if Cmdline.Debug.get () >= n
  then Format.printf format
  else Format.ifprintf Format.std_formatter format

let debug1 format = debug 1 format
let debug2 format = debug 2 format


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
      Lmap_bitwise.From_Model.fold 
        (fun out _ lmap -> register lmap out) from_table lmap
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
   debug2 "[scope] computing for function %a@." Kernel_function.pretty_name kf;
  let f = Kernel_function.get_definition kf in 
  let do_stmt lmap s =
    if Db.Value.is_accessible (Kstmt s) then
      match s.skind with
        | Instr i -> register_modified_zones lmap s i
        | _ -> lmap
    else lmap
  in
  let f_datas = List.fold_left do_stmt InitSid.empty f.sallstmts in
  debug2 "[scope:compute] data init stmts : %a@\n" InitSid.pretty f_datas;
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
    assert (new_ <> State.Start);
    State.test_and_merge ~old new_

  let doStmt _stmt _state = Dataflow.SDefault

  let doInstr stmt _ m_before = 
    Dataflow.Done (State.transfer (X.modified stmt) m_before)

  let stmt_can_reach _ _ = true
  let filterStmt _stmt = true

  let doGuard _ _ _ = Dataflow.GDefault
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
  let (f_scope, fb_scope, b_scope) as all = find_scope allstmts modif_stmts stmt in
  if Cmdline.Debug.get () >= 1 then 
    begin
      let print_list fmt l = 
        List.iter (fun s -> Format.fprintf fmt "%d " s.sid) l in
      Format.printf "[scope] get_data_scope_at_stmt %a at %d @\n"
        Locations.Zone.pretty zone stmt.sid;
      let stmts = SidSet.to_list ~keep_default:false modif_stmts in
      Format.printf "\tmodified by = %a@\n" 
        (fun fmt -> List.iter (fun s -> Format.fprintf fmt "%d " s)) stmts;
      Format.printf "\tf = %a@\n\tfb = %a@\n\tb = %a@\n" 
        print_list (Cilutil.StmtSet.elements f_scope)
        print_list (Cilutil.StmtSet.elements fb_scope)
        print_list (Cilutil.StmtSet.elements b_scope)
    end;
  all

exception ToDo

let get_annot_zone kf stmt annot =
  try
    let add_zone z info =
      let s = info.Db.Properties.Interp.To_zone.ki in
      let before = info.Db.Properties.Interp.To_zone.before in
      let zone = info.Db.Properties.Interp.To_zone.zone in
        debug2 "[forward_prop_scope] need %a %s stmt %d@."
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
      debug1 "[scope:get_annot_zone] need %a @." Locations.Zone.pretty zone ;
      zone
  with Extlib.NotYetImplemented _ | ToDo ->
      begin
        Format.printf "[get_annot_zone] don't know how to compute zone@.";
        Format.printf "[get_annot_zone] skip this annotation@.";
        raise ToDo
      end


let check_stmt_annots pred s =
  let check res annot =
    match annot with 
      | Before (AI (_, annot)) ->
          begin
          match annot.annot_content with 
            | AAssert (_, p, _) -> 
                if Logic_const.is_same_named_predicate p pred then true else res
            | _ -> res
          end
      | _ -> res
  in
  List.fold_left check false (Annotations.get_filter Logic_const.is_assert s)

let get_prop_scope_at_stmt kf stmt annot =
  debug1 "[get_prop_scope_at_stmt] at stmt %d in %a : %a@."
    stmt.sid Kernel_function.pretty_name kf
    !Ast_printer.d_code_annotation annot;

  let sets = (Cilutil.StmtSet.empty, Cilutil.StmtSet.empty) in
  try
  let zone =  get_annot_zone kf stmt annot in

  let _allstmts, info = compute kf in
  let modif_stmts = InitSid.find info zone in
  let _ = forward_data_scope modif_stmts stmt in
  let pred = 
    match annot.annot_content with AAssert (_, p, _) -> p | _ -> assert false in
  let add sid x acc =
    let (acc_scope, acc_check) = acc in
    match x with
      | State.Start -> 
          (add_s sid acc_scope, acc_check)
      | State.SameVal -> 
          let s, _ = Kernel_function.find_from_sid sid in
          if !Db.Dominators.is_dominator kf ~opening:stmt ~closing:s 
          then begin
            let acc_scope = add_s sid acc_scope in
            let acc_check =
              if check_stmt_annots pred s then add_s sid acc_check
              else acc_check
            in (acc_scope, acc_check)
          end
          else acc
      | _ -> acc
  in
  let sets = States.fold add sets in
    sets
  with ToDo -> sets

class annot_visitor = object(self)

  inherit Visitor.generic_frama_c_visitor
            (Project.current ()) (Cil.inplace_visit ()) as super

  val mutable kf = None
  val mutable n = 0;

  method get_n () = n

  method vcode_annot annot =
    let kf = Extlib.the kf in
    let stmt = Cil.get_original_stmt self#behavior 
                 (Cilutil.valOf self#current_stmt) in
    let before = self#is_annot_before in
    let _ = match annot.annot_content with 
        | AAssert (_, _, _) -> 
            if before then begin
              Debug.debug 2 "[check] at stmt %d in %a : %a@."
                stmt.sid Kernel_function.pretty_name kf 
                !Ast_printer.d_code_annotation annot;
              let _scope, equiv = get_prop_scope_at_stmt kf stmt annot in
              let warn s =
                n <- n + 1;
                Debug.debug 1 "\t-> annot at stmt %d can be removed@." s.sid
              in Cilutil.StmtSet.iter warn equiv
            end
        | _ -> ()
    in Cil.SkipChildren

  method vglob_aux g = match g with
    | GFun (f, _loc) ->
        kf <- Some (Globals.Functions.get (f.Cil_types.svar));
        Cil.DoChildren
    | _ -> Cil.SkipChildren


end (* class annot_visitor *)

let check_asserts () =
  let visitor = new annot_visitor in
  ignore (Visitor.visitFramacFile (visitor:>Visitor.frama_c_visitor) 
            (Cil_state.file ()));
  let n = visitor#get_n() in
    if n > 0 then
      Format.printf "[check_asserts] %d assertions could be removed@." n


(** Register external functions into Db. *)
let () =
  Db.Scope.get_data_scope_at_stmt := get_data_scope_at_stmt;
  Db.Scope.get_prop_scope_at_stmt := get_prop_scope_at_stmt;
  Db.Scope.check_asserts := check_asserts;


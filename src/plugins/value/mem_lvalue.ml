(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

module KernelFile = File

open Cil_types
open Cil_datatype
open Locations

let debug = false

(* ------------------------------------------------------------------------- *)
(* --- Constant or non-constant l-values                                 --- *)
(* ------------------------------------------------------------------------- *)

let lv_is_precise (lv: lval) = match lv with
  | (Mem _, _) -> false
  | (Var _, off) -> Cil.isConstantOffset off

let lv_has_exact_loc stmt lv =
  if Db.Value.is_computed () then
    let exact state =
      if Cvalue.Model.(equal bottom state) then
        true
      else
        let loc = !Db.Value.lval_to_loc_state state lv in
        Locations.cardinal_zero_or_one loc
    in
    match Db.Value.get_stmt_state_callstack ~after:false stmt with
    | None -> exact (Db.Value.get_stmt_state stmt)
    | Some by_stack ->
      try
        Value_types.Callstack.Hashtbl.iter
          (fun _ state -> if not (exact state) then raise Exit)
          by_stack;
        true
      with Exit -> false
  else false

let lv_imprecise_loc stmt lv =
  not (lv_is_precise lv) && not (lv_has_exact_loc stmt lv)


(* ------------------------------------------------------------------------- *)
(* --- Use analysis                                                      --- *)
(* ------------------------------------------------------------------------- *)
type found = LV | Addr | Write

module LatticeDirty = struct
  type t = Bottom | Dirty | Known

  let join d1 d2 = match d1, d2 with
    | Bottom, d | d, Bottom -> d
    | Dirty, _ | _, Dirty -> Dirty
    | Known, Known -> Known

  let bottom = Bottom
  let is_included d1 d2 = match d1, d2 with
    | _, Dirty |  (Bottom | Known), Known | Bottom, Bottom -> true
    | _ -> false

  let join_and_is_included d1 d2 = (join d1 d2, is_included d1 d2)
  let pretty fmt = function
    | Bottom -> Format.fprintf fmt "Bot"
    | Dirty -> Format.fprintf fmt "dirty"
    | Known -> Format.fprintf fmt "known"
end

module Compute(X: sig
  val kf: kernel_function
  val lv: lval
  val locs: (found * stmt) list
end) = struct

  include LatticeDirty

  (** Does the evaluation of [stmt] modifies the eventual value of [X.lv]? *)
  let dirties stmt = match stmt.skind with
    | Instr (Set _ | Call _) ->
      let out = !Db.Outputs.statement stmt in
      let state = Db.Value.get_stmt_state stmt in
      let deps = Some Zone.bottom in
      let for_writing = false in
      let deps, z, _ =
        !Db.Value.lval_to_zone_with_deps_state ~for_writing state ~deps X.lv
      in
      let r = Zone.intersects deps out || Zone.intersects z out in
      if debug then Format.printf "S %d, %b, %a/%a|%a@." stmt.sid r
        Zone.pretty out Zone.pretty deps Zone.pretty z;
      r
    | _ -> false

  let reads s =
    let read (found, s') = Cil_datatype.Stmt.equal s s' && found = LV in
    List.exists read X.locs

  let writes s =
    let write (found, s') = Cil_datatype.Stmt.equal s s' && found = Write in
    List.exists write X.locs

  let transfer_stmt stmt v =
    if Db.Value.is_reachable_stmt stmt then
      let v' =
        if dirties stmt then
          if writes stmt then Known else Dirty
        else
          if reads stmt then Known else v
      in
      List.map (fun s -> s, v') stmt.succs
    else []

    let init = [Kernel_function.find_first_stmt X.kf, Dirty]
end

let compute kf lv locs =
  let fenv = Dataflows.function_env kf in
  let module Fenv = (val fenv: Dataflows.FUNCTION_ENV) in
  let module Arg = struct
    let kf = kf
    let lv = lv
    let locs = locs
  end in
  let module Arg2 = Compute(Arg) in
  let module Dataflow = Dataflows.Simple_forward(Fenv)(Arg2) in
  List.map
    (fun (found, stmt) ->
      let state = Dataflow.pre_state stmt in
      found, stmt, state
    )
    locs



(* ------------------------------------------------------------------------- *)
(* --- Re-emitting statuses                                              --- *)
(* ------------------------------------------------------------------------- *)

let reemit = function
  | Property.IPBehavior _ | Property.IPAxiom _ | Property.IPAxiomatic _
  | Property.IPPredicate (Property.PKAssumes _, _, _, _) -> false
  | _ -> true

let good_emitter emitter =
  Emitter.Usable_emitter.get_name emitter = "Inferred annotations"

let copy_ppt old_prj new_prj old_ppt new_ppt =
  let module P = Property_status in
  let emit s l =
    Project.on new_prj
      (fun s ->
        let aux e =
          let emitter = e.P.emitter in
          if good_emitter emitter then
            let emitter = Emitter.Usable_emitter.get emitter in
            match s with
            | P.True | P.False_and_reachable | P.Dont_know ->
              let hyps = [] in (* TODO: sharing bug, we must convert the
                                  property in the new project *)
              P.emit emitter ~hyps new_ppt s
            | P.False_if_reachable ->
              (* in that case, the only hypothesis is "Reachable new_ppt" which
                 must be automatically added by the kernel *)
              P.emit emitter ~hyps:[] new_ppt P.False_if_reachable
        in
        List.iter aux l
      ) s
  in
  if reemit old_ppt then
    match Project.on old_prj P.get old_ppt with
    | P.Never_tried -> ()
    | P.Best(s, l) -> emit s l
    | P.Inconsistent i ->
      emit P.True i.P.valid;
      emit P.False_and_reachable i.P.invalid

(** Assumes the current project is old_prj *)
let dup_spec_status new_prj kf =
  let kf_name = Kernel_function.get_name kf in
  let new_kf = Project.on new_prj Globals.Functions.find_by_name kf_name in
  let old_spec = Annotations.funspec ~populate:false kf in
  let new_spec =
    Project.on new_prj (Annotations.funspec ~populate:false) new_kf
  in
  let old_ppts = Property.ip_of_spec kf Kglobal old_spec in
  let new_ppts = Property.ip_of_spec new_kf Kglobal new_spec in
  let old_prj = Project.current () in
(*  Project.set_current new_prj; *)
  List.iter2 (copy_ppt old_prj new_prj) old_ppts new_ppts;
(*  Project.set_current old_prj *)


(* ------------------------------------------------------------------------- *)
(* --- Results                                                           --- *)
(* ------------------------------------------------------------------------- *)

module HLV = Cil_datatype.LvalStructEq.Hashtbl

type action =
| Read of varinfo * lval
| Invalidate of varinfo

let lv_name lv =
  (* Cabs2cil.fresh_global "tmplv" (* too fresh *) *)
  let s = Pretty_utils.to_string Printer.pp_lval lv in
  let regexp = Str.regexp "[^a-zA-Z0-9_]+" in
  Str.global_replace regexp "_" s

let instrument fundec lv locs hstmt hlv =
  let typ = Cil.typeOfLval lv in
  let size = Bit_utils.sizeof_lval lv in
  match size with
  | Int_Base.Top -> ()
  | Int_Base.Value _size ->
    let typ = Cil.typeRemoveAttributes ["const"; "restrict"] typ in
    let name = "__lv_" ^ lv_name lv  in
    (* TODOBUG: this local is registered in the AST of the original project *)
    let vi = Cil.makeLocalVar fundec ~temp:false name typ in
    HLV.add hlv lv vi;
    let txt = Pretty_utils.to_string Printer.pp_lval lv in
    vi.vdescr <- Some txt;
(*
    let vtop = Cvalue.V_Or_Uninitialized.initialized Cvalue.V.top in
    let default = Cvalue.V_Offsetmap.create_isotropic ~size vtop in
    ignore (Cvalue.Default_offsetmap.create_initialized_var
	      vi (Base.Known (Integer.zero, Integer.pred size)) default);
*)
    let h = Cil_datatype.Stmt.Hashtbl.create 16 in
    let aux_occur (found, stmt, state) =
      let i = try Stmt.Hashtbl.find h stmt with Not_found -> (false, false) in
      match found with
      | Addr -> ()
      | LV ->
        if state = LatticeDirty.Dirty then
          Stmt.Hashtbl.replace h stmt (true, snd i)
      | Write ->
        Stmt.Hashtbl.replace h stmt (fst i, true)
    in
    List.iter aux_occur locs;
    Stmt.Hashtbl.iter
      (fun stmt (before, after) ->
        if before then Stmt.Hashtbl.add hstmt stmt (`Read lv);
        if after  then Stmt.Hashtbl.add hstmt stmt (`Write lv);
        begin match stmt.skind with
        | If (_, _, _, _) | Switch (_, _, _, _) ->
          List.iter
            (fun succ -> Stmt.Hashtbl.add hstmt succ (`Assert lv)) stmt.succs
        | _ -> ()
        end;
      ) h;
;;

type skip_change = SkipChangeDeep | SkipOneChange

module GraphDeps = struct
  module V = struct
    type t = varinfo * (varinfo list * stmt)
    let compare (vi1, _) (vi2, _) = Cil_datatype.Varinfo.compare vi1 vi2
    let equal (vi1, _) (vi2, _) = Cil_datatype.Varinfo.equal vi1 vi2
    let hash (vi, _) = Cil_datatype.Varinfo.hash vi
  end

  type t = V.t list

  let iter_vertex f (g:t) = List.iter f g

  let iter_succ f (g:t) ((_, (l, _)):V.t) =
    let aux vi =
      try
        let assvi = List.assq vi g in
        f (vi, assvi)
      with Not_found -> ()
    in
    List.iter aux l

end

module TopologicalBefore = Graph.Topological.Make(GraphDeps)

class instrument project hstmt hlv = object (self)
  inherit Visitor.frama_c_copy project

  val mutable change_lv = []
  val mutable is_alarm = false
  val mutable changed = []

  method private self = (self :> Visitor.frama_c_visitor)

  method! vlval lv =
    match change_lv with
    | SkipChangeDeep :: _ -> Cil.DoChildren
    | SkipOneChange :: q -> begin
      change_lv <- q;
      Cil.DoChildren
    end
    | [] -> begin
      try
        let vi = HLV.find hlv lv in
        let vi' = Visitor.visitFramacVarDecl self#self vi in
        changed <- vi' :: changed;
        Cil.ChangeTo (Var vi', NoOffset)
      with Not_found -> Cil.DoChildren
    end

  method! vexpr e =
    match e.enode with
    | StartOf _ | AddrOf _ ->
      (* Skip to avoid unsoundness in codes such  as
         [int *p = t[i]; p++; y = *p; // does not read t[i] ] *)
      change_lv <- SkipOneChange :: change_lv;
      Cil.DoChildren
    | SizeOfE _ | AlignOfE _ -> (* Skip for pretty-printing reasons *)
      change_lv <- SkipChangeDeep :: change_lv;
      Cil.DoChildrenPost (fun r -> change_lv <- List.tl change_lv; r)
    | _ ->
      Cil.DoChildren

  method! vpredicate_named named =
    if is_alarm then
      Cil.ChangeTo { named with content = Ptrue }
    else Cil.DoChildren

  method! vcode_annot ca =
    match Alarms.find ca with
    | None -> Cil.DoChildren
    | Some _ ->
      is_alarm <- true;
      Cil.DoChildrenPost (fun r -> is_alarm <- false; r)

  method private read_lv lv loc =
    let vi = HLV.find hlv lv in
    let vi' = Visitor.visitFramacVarDecl self#self vi in
    change_lv <- SkipOneChange :: change_lv;
    changed <- [];
    let lv' = Visitor.visitFramacLval self#self lv in
    let i = Set ((Var vi', NoOffset), Cil.new_exp ~loc (Lval lv'), loc) in
    (vi', (changed, Cil.mkStmtOneInstr ~valid_sid:true i))

  method private write_lv lv loc =
    let vi = HLV.find hlv lv in
    let vi' = Visitor.visitFramacVarDecl self#self vi in
    change_lv <- SkipOneChange :: change_lv;
    let lv' = Visitor.visitFramacLval self#self lv in
    let i = Set (lv', Cil.new_exp ~loc (Lval (Var vi', NoOffset)), loc) in
    Cil.mkStmtOneInstr ~valid_sid:true i

  method! vstmt_aux stmt =
    let loc = Stmt.loc stmt in
    let actions = Stmt.Hashtbl.find_all hstmt stmt in
    let before = ref [] in
    let after = ref [] in
    let aux action =
      match action with
      | `Read lv -> before := self#read_lv lv loc :: !before
      | `Write lv -> after := self#write_lv lv loc :: !after
      | `Assert _lv -> ()
    in
    List.iter aux actions;
    match !before, !after with
    | [], [] -> Cil.DoChildren
    | _, _ ->
      let aux (_, (_, stmt)) acc = stmt :: acc in
      let before = TopologicalBefore.fold aux !before [] in
      Cil.DoChildrenPost
        (fun stmt' ->
          let stmt'' = Cil.mkStmt stmt'.skind in
          let stmts = before @ [stmt''] @ !after in
          stmt'.skind <- Block (Cil.mkBlock stmts);
          stmt')
end

let print_lv lv locs =
  match locs with
  | [] -> []
  | _ :: _ ->
    let stmts = List.map snd locs in
    let dom = Dominators.nearest_common_ancestor stmts in
    let loc = Cil_datatype.Stmt.loc dom in
    let kf = Kernel_function.find_englobing_kf dom in
    let locs' = compute kf lv locs in
    let one_known =
      List.exists (fun (found, _, state) ->
        found = LV && state = LatticeDirty.Known) locs'
    in
    if one_known then
      let aux fmt (k, stmt, state) =
        let loc = Cil_datatype.Stmt.loc stmt in
        Format.fprintf fmt
          "function %a (%a), %s, %t"
          Kernel_function.pretty kf Cil_datatype.Location.pretty loc
          (match k with LV -> "read" | Addr -> "address" | Write -> "write")
          (fun fmt -> if k <> Write then LatticeDirty.pretty fmt state)
      in
      Value_parameters.warning ~source:(fst loc)
        "@[<v 2>%a@ %a@]" Printer.pp_lval lv
        (Pretty_utils.pp_list ~pre:"" ~suf:"" aux) locs';
      locs'
    else []


class reused_lval = object (self)
  inherit Visitor.frama_c_inplace

  val found = HLV.create 17

  val hstmt = Stmt.Hashtbl.create 17
  val hlv = HLV.create 17

  method instrument =
    KernelFile.create_project_from_visitor "lvalue"
      (fun p -> new instrument p hstmt hlv)

  method private add lv v =
    (* TODO: check that no sub-lvalue is volatile *)
    let typ = Cil.typeOfLval lv in
    if not (Cil.typeHasQualifier "volatile" typ) then
      try
        let prev = HLV.find found lv in
        HLV.replace found lv (v :: prev)
      with Not_found ->
        HLV.add found lv [v]

  method! vstmt_aux stmt =
    match stmt.skind with
    | UnspecifiedSequence seq ->
      List.iter
        (fun (stmt,_,_,_,_) ->
          ignore (Visitor.visitFramacStmt (self:>Visitor.frama_c_visitor) stmt))
        seq;
      Cil.SkipChildren (* do not visit the additional lvals *)
    | _ -> Cil.DoChildren

  method private is_scalar lv =
    match Cil.unrollType (Cil.typeOfLval lv) with
    | TEnum _ | TInt _ | TFloat _ | TPtr _ -> true
    | _ -> false

  method! vinst i =
    let stmt = Extlib.the self#current_stmt in
    match i with
    | Set (lv, _, _) | Call (Some lv, _, _, _) when self#is_scalar lv ->
      if lv_imprecise_loc stmt lv then self#add lv (Write, stmt);
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method! vexpr e =
    match self#current_stmt with
    | None -> Cil.SkipChildren (* initializers *)
    | Some stmt ->
      match e.enode with
      | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
        Cil.SkipChildren (* skip things such as 'sizeof( *p )' *)
      | Lval lv when self#is_scalar lv ->
        if lv_imprecise_loc stmt lv then self#add lv (LV, stmt);
        Cil.DoChildren
      | StartOf lv | AddrOf lv when self#is_scalar lv ->        
        if false && lv_imprecise_loc stmt lv then self#add lv (Addr, stmt);
        Cil.DoChildren
      | _ -> Cil.DoChildren

  method! vglob_aux g =
    match g with
    | GFun (fdec, _) ->
      Cil.DoChildrenPost
        (fun g' ->
          HLV.iter_sorted
            (fun lv locs ->
              let sort_loc (_, s1) (_, s2) =
                Cil_datatype.(Location.compare (Stmt.loc s1) (Stmt.loc s2))
              in
              let locs = List.sort sort_loc locs in
              let locs' = print_lv lv locs in
              if locs' <> [] then
                instrument fdec lv locs' hstmt hlv
            ) found;
          HLV.clear found;
          g')
          
    | _ -> Cil.SkipChildren

end

let compute () =
  let vis = new reused_lval in
  Visitor.visitFramacFileSameGlobals
    (vis:> Visitor.frama_c_visitor) (Ast.get ());
  let new_prj = vis#instrument in
  Project.on new_prj
    (fun () ->
      Cfg.clearFileCFG ~clear_id:false (Ast.get());
      Cfg.computeFileCFG (Ast.get())
    ) ();
  Globals.Functions.iter (dup_spec_status new_prj)

let compute, _ =
  State_builder.apply_once "Value.Mem_lvalue" [Db.Value.self] compute


let () = Db.Main.extend
  (fun () ->
    if Value_parameters.(not (ForceValues.get ()) && ReusedExprs.get ()) then
      (* Analysis done by Value itsefl otherwise *)
      compute ()
  )



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

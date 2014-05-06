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

(** The aim here is to select the statements where a data D
* has the same value then a given starting program point L. *)

open Cil_types
(*open Cil_datatype*)

module R =
  Plugin.Register
    (struct
       let name = "scope"
       let shortname = "scope"
       let help = "data dependencies higher level functions"
     end)

let cat_rm_asserts_name = "rm_asserts"
let cat_rm_asserts = R.register_category cat_rm_asserts_name
let () = R.add_debug_keys (R.get_category cat_rm_asserts_name)


(** {2 Computing a mapping between zones and modifying statements}
We first go through all the function statements in other to build
a mapping between each zone and the statements that are modifying it.
**)

(** Statement identifier *)
module StmtDefault = struct
  include Cil_datatype.Stmt
  let default = Cil.dummyStmt
  let id s = s.sid
end

(** set of values to store for each data *)
module StmtSetLattice = struct

  include Abstract_interp.Make_Hashconsed_Lattice_Set(StmtDefault)(Cil_datatype.Stmt.Hptset)

  let default _v _a _b : t = empty
  let defaultall _v : t = empty

  let empty = bottom
  let cardinal set = fold (fun _ n -> n+1) set 0
  let single s = inject_singleton s

  let add s set = join set (single s)
end

(** A place to map each data to the state of statements that modify it. *)
module InitSid = struct
  module LM = Lmap_bitwise.Make_bitwise (StmtSetLattice)
  (* Clear the (non-project compliant) internal caches each time the ast
     changes, which includes every time we switch project. *)
  let () = Ast.add_hook_on_update LM.clear_caches

  type t = LM.t

  let empty = LM.empty
  let find = LM.find

  let add_zone ~exact lmap zone sid =
    let new_val = StmtSetLattice.single sid in
    let lmap = LM.add_binding exact lmap zone new_val in
      lmap

  let test_and_merge old_lmap new_lmap =
    let new_lmap = LM.join old_lmap new_lmap in
      if LM.is_included new_lmap old_lmap then None
      else Some new_lmap

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
  let register lmap zone =
    (* [exact] should always be false because we want to store all the stmts *)
    InitSid.add_zone ~exact:false lmap zone stmt
  in
    match stmt.skind with
      | Instr (Set (lval, _, _)) ->
          let _dpds, _, zone =
            get_lval_zones ~for_writing:true  stmt lval
          in
          register lmap zone
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
              !Db.Value.expr_to_kernel_function
                ~with_alarms:CilE.warn_none_mode ~deps:None (Kstmt stmt) funcexp
            in
            let aux_out kf out =
              let inout= !Db.Operational_inputs.get_internal_precise ~stmt kf in
              Locations.Zone.join out inout.Inout_type.over_outputs
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

  let equal (b1 : t) b2 = (b1 = b2)

  let test_and_merge ~old new_ =
    let result = merge new_ old in
    if equal result old then None else Some result

  let join_and_is_included smaller larger =
    let result = join smaller larger in
    (result, equal result larger)
  let is_included smaller larger = snd (join_and_is_included smaller larger)


  let transfer modif m =
    if modif then Modif else if m = Start then SameVal else m

end

(** Place to store the dataflow analyses results *)
module GenStates (S : sig
                    type t
                    val pretty : Format.formatter -> t -> unit
                  end)
  = struct
  type key = stmt
  type data = S.t
  type t = data Cil_datatype.Stmt.Hashtbl.t

  let states:t = Cil_datatype.Stmt.Hashtbl.create 50
  let clear () = Cil_datatype.Stmt.Hashtbl.clear states

  let mem = Cil_datatype.Stmt.Hashtbl.mem states
  let find = Cil_datatype.Stmt.Hashtbl.find states
  let replace = Cil_datatype.Stmt.Hashtbl.replace states
  let add = Cil_datatype.Stmt.Hashtbl.add states
  let iter f = Cil_datatype.Stmt.Hashtbl.iter f states
  let fold f = Cil_datatype.Stmt.Hashtbl.fold f states
  let length () = Cil_datatype.Stmt.Hashtbl.length states

  let pretty fmt infos =
    Cil_datatype.Stmt.Hashtbl.iter
      (fun k v -> Format.fprintf fmt "Stmt:%d\n%a\n======" k.sid S.pretty v)
      infos
end

module States = GenStates (State)

module BackwardScope (X : sig val modified : stmt -> bool end ) = struct
  let name = "scope(back)"
  let debug = false

  module StmtStartData = States

  type t = StmtStartData.data
  let pretty = State.pretty

  let combineStmtStartData _stmt ~old new_ =
    if State.equal new_ old then None else Some new_

  let combineSuccessors s1 s2 = State.merge s1 s2

  let doStmt _stmt = Dataflow2.Default

  let doInstr stmt _instr m_after =
    Dataflow2.Done (State.transfer (X.modified stmt) m_after)

  let filterStmt _stmt _next = true

  let funcExitData = State.NotSeen
end

let backward_data_scope allstmts modif_stmts s =
  States.clear ();
  List.iter (fun s -> States.add s State.NotSeen) allstmts;
  let modified s = StmtSetLattice.mem s modif_stmts in
  States.replace s State.Start;
  let stmts = s.preds in
  let module Computer = BackwardScope (struct let modified = modified end) in
  let module Compute = Dataflow2.Backwards(Computer) in
  Compute.compute stmts

module ForwardScope (X : sig val modified : stmt -> bool end ) = struct
  include State;;

  let transfer_stmt s state =
    let map_on_all_succs new_state = List.map (fun x -> (x,new_state)) s.succs in
    match s.skind with
    | Instr _ -> map_on_all_succs (State.transfer (X.modified s) state)
    | If _ | Switch _ -> map_on_all_succs (State.transfer false state)

    | Return _ -> []

    | UnspecifiedSequence _ | Loop _ | Block _
    | Goto _ | Break _ | Continue _
    | TryExcept _ | TryFinally _
      -> map_on_all_succs state
  ;;

end

let forward_data_scope modif_stmts s kf =
  let _modified s = (* the undescore is for ocaml3.12.1. Otherwise:
File "src/scope/datascope.ml", line 326, characters 6-14:
Warning 26: unused variable modified.
Error: Error-enabled warnings (1 occurrences) *) 
    StmtSetLattice.mem s modif_stmts in 
  let module Fenv = (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV) in
  let module Arg = struct
      include ForwardScope(struct let modified = _modified end)
      let init = [(s,State.Start)];;
  end in
  let module Compute = Dataflows.Simple_forward(Fenv)(Arg) in
  Compute.fold_on_result
;;


(* XXX *)
let add_s s acc =
  (* we add only 'simple' statements *)
  match s.skind with
    | Instr _ | Return _ | Continue _ | Break _ | Goto _
        -> Cil_datatype.Stmt.Hptset.add s acc
    | Block _ | Switch _ | If _ | UnspecifiedSequence _ | Loop _
    | TryExcept _ | TryFinally _
        -> acc

(** Do backward and then forward propagations and compute the 3 statement sets :
* - forward only,
* - forward and backward,
* - backward only.
*)
let find_scope allstmts modif_stmts s kf =
  let add fw s' x acc =
    match x with
      | State.Start ->
          if fw then add_s s' acc
          else
            let x =
              List.fold_left (fun x s -> State.merge x (States.find s))
                State.NotSeen s.succs
            in let x = State.transfer (StmtSetLattice.mem s' modif_stmts) x in
              if x = State.SameVal then add_s s' acc else acc
      | State.SameVal -> add_s s' acc
      | _ -> acc
  in
  let _ = backward_data_scope allstmts modif_stmts s in
  let bw = States.fold (add false) Cil_datatype.Stmt.Hptset.empty in

  let fold = forward_data_scope modif_stmts s kf in
  let fw = fold (fun acc s' x -> add true s' x acc) Cil_datatype.Stmt.Hptset.empty in

  let fb = Cil_datatype.Stmt.Hptset.inter bw fw in
  let fw = Cil_datatype.Stmt.Hptset.diff fw fb in
  let bw = Cil_datatype.Stmt.Hptset.diff bw fb in
    fw, fb, bw

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
  let (f_scope, fb_scope, b_scope) = find_scope allstmts modif_stmts stmt kf in
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

(** add [annot] to [acc] if it is not already in.
    [acc] is supposed to be sorted according to [annot_id].
    @return true if it has been added. *)
let rec add_annot annot acc = match acc with
  | [] -> [ annot ], true
  | a :: tl ->
    if  annot.annot_id < a.annot_id then annot::acc, true
    else if annot.annot_id = a.annot_id then acc, false
    else
      let tl, added = add_annot annot tl in
      a::tl, added

(** Check if some assertions before [s] are identical to [pred].
  * Add them to acc if any *)
let check_stmt_annots pred stmt acc =
  let check _ annot acc =
    match annot.annot_content with
    | AAssert (_, p) ->
        if Logic_utils.is_same_predicate p.content pred.content then
          let acc, added =add_annot annot acc in
          if added then
            R.debug "annot at stmt %d could be removed: %a"
              stmt.sid Printer.pp_code_annotation annot;
          acc
        else 
	  acc
    | _ -> acc
  in
  Annotations.fold_code_annot check stmt acc

(** Return the set of stmts (scope) where [annot] has the same value
  * than in [stmt]
  * and add to [to_be_removed] the annotations that are identical to [annot]
  * in the statements that are both the scope and that are dominated by stmt.
  * *)
let get_prop_scope_at_stmt kf stmt ?(to_be_removed=[]) annot =
  R.debug "[get_prop_scope_at_stmt] at stmt %d in %a : %a"
    stmt.sid Kernel_function.pretty kf
    Printer.pp_code_annotation annot;

  let sets = (Cil_datatype.Stmt.Hptset.empty, to_be_removed) in
    try
      let zone =  get_annot_zone kf stmt annot in

      let _allstmts, info = compute kf in
      let modif_stmts = InitSid.find info zone in
      let fold = forward_data_scope modif_stmts stmt kf in
      let pred = match annot.annot_content with
        | AAssert (_, p) -> p
        | _ -> R.abort "only 'assert' are handled here"
      in
      let add ((acc_scope, acc_to_be_rm) as acc) s x  = match x with
        | State.Start -> (add_s s acc_scope, acc_to_be_rm)
        | State.SameVal ->
          if Dominators.dominates stmt s
          then begin
            let acc_scope = add_s s acc_scope in
            let acc_to_be_rm = check_stmt_annots pred s acc_to_be_rm in
            (acc_scope, acc_to_be_rm)
          end
          else acc
        | _ -> acc
      in
      let sets = fold add sets in
      sets
    with ToDo ->
      R.warning
        "[get_annot_zone] don't know how to compute zone: skip this annotation";
      sets

(** Collect the annotations that can be removed because they are redondant. *)
class check_annot_visitor = object(self)

  inherit Visitor.frama_c_inplace

  val mutable to_be_removed = []

  method get_to_be_removed () = to_be_removed

  method! vcode_annot annot =
    let kf = Extlib.the self#current_kf in
    let stmt =
      Cil.get_original_stmt self#behavior (Extlib.the self#current_stmt)
    in
    let _ = match annot.annot_content with
        | AAssert (_, _) ->
              R.debug ~level:2 "[check] annot %d at stmt %d in %a : %a@."
                annot.annot_id stmt.sid Kernel_function.pretty kf
                Printer.pp_code_annotation annot;
              let _, added = add_annot annot to_be_removed in
                (* just check if [annot] is in [to_be_removed] :
                 * don't add it... *)
                if added then (* annot is not already removed *)
                  let _scope, rem =
                    get_prop_scope_at_stmt kf stmt ~to_be_removed annot
                  in 
		  to_be_removed <- rem
        | _ -> ()
    in Cil.SkipChildren

  method! vglob_aux g = match g with
    | GFun (fdec, _loc) when
        !Db.Value.is_called (Extlib.the self#current_kf) &&
        not (!Db.Value.no_results fdec)
        ->
      Cil.DoChildren
    | _ -> Cil.SkipChildren

  method! vexpr _ = Cil.SkipChildren

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

  inherit Visitor.frama_c_inplace

  method! vcode_annot annot =
    let _, not_in = add_annot annot to_be_removed in
    if not_in then (* not to be removed *)  Cil.SkipChildren
    else (* is to be removed *)
      match annot.annot_content with
      | AAssert (_, p) ->
          R.result ~dkey:cat_rm_asserts ~level:2
            "removing redundant %a@." Printer.pp_code_annotation annot;
          let p = { p with content = Ptrue } in
          let aassert = AAssert ([], p) in
          let annot = { annot with annot_content = aassert } in
          Cil.ChangeTo annot
      | _ -> Cil.SkipChildren

end

(** Remove  the annotations collected by [check_asserts]. *)
let rm_asserts () =
  let to_be_removed = f_check_asserts () in
  let n = List.length to_be_removed in
    (if n > 0 then
      R.feedback ~dkey:cat_rm_asserts "removing %d assertion(s)@." n);
  let visitor = new rm_annot_visitor to_be_removed in
  Visitor.visitFramacFileSameGlobals visitor (Ast.get ())

(* let code_annotation_type = ??? TODO *)

(** Register external functions into Db. *)
let () =
  Db.register (* kernel_function -> stmt -> lval ->
       Cil_datatype.Stmt.Set.t * 
       (Cil_datatype.Stmt.Set.t * 
        Cil_datatype.Stmt.Set.t) *)
    (Db.Journalize
       ("Scope.get_data_scope_at_stmt",
        Datatype.func3
          Kernel_function.ty
          Cil_datatype.Stmt.ty
          Cil_datatype.Lval.ty
          (Datatype.pair 
             Cil_datatype.Stmt.Hptset.ty 
             (Datatype.pair Cil_datatype.Stmt.Hptset.ty 
                            Cil_datatype.Stmt.Hptset.ty))))
  Db.Scope.get_data_scope_at_stmt get_data_scope_at_stmt;

   Db.register (* (kernel_function -> stmt -> code_annotation ->
       Cil_datatype.Stmt.Hptset.t * code_annotation list *)
      Db.Journalization_not_required (* TODO *)
     (* (Db.Journalize("Scope.get_prop_scope_at_stmt",
                    Datatype.func Kernel_type.kernel_function
                     (Datatype.func Kernel_type.stmt
                        (Datatype.func code_annotation_type
                           (Datatype.couple  Kernel_type.stmt_set
                              (Datatype.list code_annotation_type)))))) *)
     Db.Scope.get_prop_scope_at_stmt  get_prop_scope_at_stmt;

   Db.register (* unit -> code_annotation list *)
      Db.Journalization_not_required (* TODO *)
     (* (Db.Journalize("Scope.check_asserts",
                    Datatype.func Datatype.unit  (Datatype.list code_annotation_type))) *)
     Db.Scope.check_asserts check_asserts;

  Db.register
    (Db.Journalize
       ("Scope.rm_asserts", Datatype.func Datatype.unit Datatype.unit))
    Db.Scope.rm_asserts rm_asserts

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

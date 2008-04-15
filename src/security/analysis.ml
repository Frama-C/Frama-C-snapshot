(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: analysis.ml,v 1.82 2008/08/21 15:02:25 uid528 Exp $ *)

open Cil
open Cil_types
open Locations
open Db
open Db_types
open Extlib

module type REACH = sig val stmt_can_reach : stmt -> stmt -> bool end

module Make(S : Lattice.S) = struct

  module Components = Components.Make(S)

  module Model = Model.Make(S)

  let global_state : Model.State.t Inthash.t = Inthash.create 107

  let debug = ref false

  let pretty _ = log "[security] status_tbl:@.%a" Model.State.pretty

  module rec Computer :
    functor(Reach : REACH) ->
      Dataflow.ForwardsTransfer with type t = Model.State.t
    =
    functor(Reach : REACH) -> struct

      let name = "security analysis"

      let debug = debug

      type t = Model.State.t

      module StmtStartData =
	Dataflow.StmtStartData
	  (struct type t = Model.State.t let size = 107 end)
	  (*	Computation.IntHashtbl(struct (* 21/11/2007: doesn't work *)
			 let dependencies = [ File_types.self ] (* TODO *)
			 type t = Model.State.t
			 let size = 107
			 let name = name
			 let copy x = assert false (* TODO *)
		       end)
*)
      open Model

      let pretty = pretty

      let copy s = s

      let computeFirstPredecessor _ state = state

      let combinePredecessors _ ~old state =
	if State.is_included state old then
	  None
	else
	  Some (State.combine ~old state)

      let doInstr stmt instr state =
(*	if Components.is_concerned_by_security stmt then*)
	  let ki = Kstmt stmt in
	  match instr with
	  | Set(lv, exp, _) ->
	      Dataflow.Done (Logic.affect (Value.get_state ki) state lv exp)
	  | Call(ret, func, args, _) ->
	      let _, funcs =
		!Value.expr_to_kernel_function
		  ~with_alarms:CilE.warn_none_mode
		  ~deps:None
		  ki
		  func
	      in
	      let do_on state kf = Function.call ki kf ret args state in
	      let fold_no_neutral state = function
		| [] -> assert false
		| hd :: tl ->
		    let acc = do_on state hd in
		    List.fold_left
		      (fun acc kf ->
			 let state = do_on (State.clear_leaks state) kf in
			 State.combine ~old:acc state)
		      acc
		      tl
	      in
	      Dataflow.Done (fold_no_neutral state funcs)
	  | Code_annot (_) ->
 	      Dataflow.Default
	  | Asm _ ->
	      CilE.warn_once "[security] Assembler code is ignored in this version. Please upgraded.";
	      Dataflow.Default
	  | Skip _ ->
	      Dataflow.Default
(*	else
	  Dataflow.Done state*)

      let doGuard s e _t =
	let ki = Kstmt s in
	if Cvalue_type.V.contains_non_zero (!Value.access_expr ki e)
(*	  && Components.is_concerned_by_security s*)
	then
	  Dataflow.GDefault
	else
	  Dataflow.GUnreachable

      let doStmt stmt state =
	assert (Value.is_reachable_stmt stmt);
        !Db.progress ();
	State.last_stmt := stmt;
        let ki = Kstmt stmt in
        let before,after,contract = Db.Properties.predicates_on_stmt stmt in
        (match contract with
             None -> ()
           | Some _s -> CilE.warn_once "ignoring statement contract");
        let state =
          List.fold_left
            (fun state (p,_) -> Logic.requires (Value.get_state ki) ki state p)
            state
            before
        in
        if after <> [] then Logic.warn_todo ();
	Dataflow.SUse state

      let filterStmt = Value.is_reachable_stmt

      let stmt_can_reach = Reach.stmt_can_reach

    end

and Function : sig
  val call :
    kinstr ->
    kernel_function ->
    lval option ->
    exp list ->
    Model.State.t ->
    Model.State.t
end =
  struct

    (* La memoisation est fausse.
       En fait, c'est une fonction des inputs vers les outputs.
       Hors, pour l'instant, c'est une fonction des args + dep add -> state.
       C'est donc faut aussi bien en entrée qu'en sortie :-(.

       ==> Mémoisation débranchée (cf. fonction memo) ! *)

    open Model

    module H =
      Hashtbl.Make
	(struct
	   type t = S.t list
	   let hash = Hashtbl.hash
	   let equal = List.for_all2 (fun x y -> S.equal x y )
	 end)

    let table : State.t H.t Inthash.t = Inthash.create 97

    let memo _h arg f =
(*      try
	H.find h arg
      with Not_found ->*)
	let res = f arg in
(*	H.add h arg res;*)
	res

    let analyse_stmt kf state stmt =
      let module Computer =
	Computer(struct let stmt_can_reach = Stmts_graph.stmt_can_reach kf end)
      in
      let module Analysis = Dataflow.ForwardsDataFlow(Computer) in
      Computer.StmtStartData.add stmt.sid state;
      Analysis.compute [ stmt ];
      Computer.StmtStartData.iter (Inthash.replace global_state);
      try
	Computer.StmtStartData.find !State.last_stmt.sid
      with Not_found ->
	Format.eprintf "%d:%a@." !State.last_stmt.sid
          !Ast_printer.d_stmt !State.last_stmt;
	assert false

    let analyse_definition kf values state _args =
      match kf.fundec with
      | Declaration _ -> assert false
      | Definition(fd, _) ->
	  let state = Model.Register.locals values fd state in
	  match fd.sbody.bstmts with
	  | [] -> assert false
	  | start :: _ -> analyse_stmt kf state start

    let apply ki kf ret args values state =
      match kf.fundec with
      | Definition(fd, _) ->
	  let fstate(*, ctx*) = State.push_call values kf args state in
	  let _args_tbl =
	    Inthash.memoize table fd.svar.vid (fun _ -> H.create 17)
	  in
	  let fstate =
(*	    memo
	      args_tbl
	      ctx
	    (fun _ ->*) analyse_definition kf values fstate args(* ) *)
	  in
	  let fstate, values =
	    match Kernel_function.find_return kf with
	    | { skind = Return (None, _) } as s ->
		fstate, Value.get_state (Kstmt s)
	    | { skind = Return (Some e, _) } as s ->
		let values = Value.get_state (Kstmt s) in
		Logic.return values fstate ret e, values
            | _ -> assert false
	  in
	  let s = State.pop_call ki fd args ~old:state fstate in
	  s, Some values
      | Declaration(_,_,_,_) ->
	  state, None
(*
    let get_values_just_before stmt =
      let ki = Kstmt stmt in
      match stmt.preds with
      | [] -> Value.get_state ki
      | hd :: tl ->
	  List.fold_left
	    (fun v s ->
	       Relations_type.Model.join v (Value.get_state (Kstmt s)))
	    (Value.get_state (Kstmt hd))
	    tl
*)
    let call ki kf ret args state =
      let spec = kf.spec in
      match kf.fundec with
      | Declaration(_, v, _,_)
      | Definition({ svar = v}, _) ->
	  (*let may f x dft = Extlib.may_map ~dft (f x dft) in*)
	  let formals = Kernel_function.get_formals kf in
	  let state, values =
	    match ki with
	    | Kglobal -> state, Value.get_state ki
	    | Kstmt _stmt ->
		Model.Register.formals
		  formals
		  args
		  (*TODO: incorrect(?)	  (get_values_just_before stmt)*)
		  (Value.get_state ki)
		  state
	  in
	  if Cmdline.Debug.get () > 0 then
	    log "[security] function call on %a@.%a@.values %a"
	      Ast_info.pretty_vname v pretty state Value.pretty_state values;
	  let state =
	    Logic.requires values ki state
	      ((Logic_const.pands $ (List.map Logic_const.pred_of_id_pred))
                 spec.spec_requires)
	  in
	  let state, ret_values = apply ki kf ret args values state in
	  let values =
	    match ret_values with
	    | None -> values (* TODO: declaration *)
	    | Some v -> v
	  in
	  let state =
	    Logic.ensures values state (Kernel_function.postcondition kf)
	  in
	  if Cmdline.Debug.get () > 0 then
	    log "[security] end of call of %a@.%a@. values %a"
	      Ast_info.pretty_vname v
	      pretty state Value.pretty_state values;
	  Model.Register.clean kf state

  end

let slicing () =
  if Cmdline.Security.Slicing.get () then begin
    log "====== Run security slicing analysis ======";
    let prj = Components.slice () in
    if Cmdline.Slicing.Print.get () then
      Project.on
	prj (fun () -> Format.printf "%t@." (fun fmt -> File.pretty fmt)) ();
    prj
  end else
    Project.current ()

let ai () =
  if Cmdline.Security.Analysis.get () then begin
    log "====== Run security analysis ======";
    !Db.Value.compute ();
    let main = fst (Globals.entry_point ()) in
    let state = Model.State.reset () in
    let state =
      Function.call Kglobal main None [] (Model.Register.globals state)
    in
    (* TODO: compute indirect dependencies *)
    (* appeler une fonction d'ordre-sup genre fold dans component dont
       la fonction f prend un stmt et un acc (ici, un statut) en entrée
       et calcule join acc (statut correspondant à stmt dans global_state)
       (pour précision, ne pas se prendre en compte soi-même).
       l'acc retourné par la fonction est le statut indirect de la composante.
       mettre à jour l'état global avec ce statut indirect.
       met à jour cet état avec le statut indirect.
    *)
    (* essai non compile: *)
    (*
      let indirect_state =
      Components.fold_fold
      (fun indirect_state c result -> add indirect_state c result)
      (fun acc s -> join acc s (* ne prend pas en compte l'optim quand c = s *))
      empty_indirect_state
      bottom
    *)
    Model.State.print_results state
  end

let whole () =
  let prj = slicing () in
  Project.on prj ai ()

let init () =
  Components.init ();
  Db.Security.run_whole_analysis := whole;
  Db.Security.run_ai_analysis := ai;
  Db.Security.run_slicing_analysis := slicing;

end

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)

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

open Cil_types

let add_destructor (_, l as acc) var =
  let loc = var.vdecl in
  match Cil.findAttribute Cabs2cil.frama_c_destructor var.vattr with
  | [] -> acc
  | [ attr ] ->
    let mk_call f e args =
      let kf = Globals.Functions.find_by_name f in
      let e =
        match Globals.Functions.get_params kf with
        | vi :: _ ->
          if Cil.need_cast (Cil.typeOf e) vi.vtype then Cil.mkCast e vi.vtype
          else e
        | [] ->
          Kernel.fatal
            "Destructor function %a should take at least one argument"
            Kernel_function.pretty kf
      in
      let vf = Kernel_function.get_vi kf in
      vf.vreferenced <- true;
      let s =
        Cil.mkStmtOneInstr ~valid_sid:true
          (Call(None,Cil.evar ~loc vf,e::args,loc))
      in (true, s :: l)
    in
    let rec aux e a =
      match a with
      | AAddrOf a -> aux (Cil.mkAddrOfVi var) a
      | AStr f -> mk_call f e []
      | ACons (f, [n]) ->
        (match Cil.intOfAttrparam n with
         | Some n ->
           mk_call f e [Cil.kinteger ~loc Cil.(theMachine.kindOfSizeOf) n]
         | None ->
           Kernel.fatal
             "unexpected argument of attribute %s: %a"
             Cabs2cil.frama_c_destructor
             Printer.pp_attrparam a)
      | _ ->
        Kernel.fatal
          "unexpected argument of attribute %s: %a"
          Cabs2cil.frama_c_destructor
          Printer.pp_attrparam a
    in aux (Cil.evar ~loc var) attr
  | _ ->
    Kernel.fatal
      "attribute %s expects exactly one argument" Cabs2cil.frama_c_destructor

(* we expect the variables from oldest to newest. Hence the fold_left will
   call the destructors in the reverse order, starting with the newest ones. *)
let add_destructors vars = List.fold_left add_destructor (false,[]) vars

(* insert the destructors before the given jump statement. *)
let insert_destructors destructors s stmts =
  let rec aux previous tl =
    match tl with
    | [] -> false, []
    | hd :: _ when Cil_datatype.Stmt.equal hd s ->
      true, List.rev_append previous destructors @ tl
    | { skind = UnspecifiedSequence l } as s :: tl ->
      let (has_inserted, res) = aux_seq [] l in
      if has_inserted then
        true,
        List.rev_append previous ({s with skind = UnspecifiedSequence res}:: tl)
      else
        aux (s::previous) tl
    | hd :: tl -> aux (hd :: previous) tl
  and aux_seq previous tl =
    match tl with
    | [] -> false, []
    | (s', _, _, _, _) :: _ when Cil_datatype.Stmt.equal s s' ->
      let destructors = List.map (fun s -> s,[],[],[],[]) destructors in
      true, List.rev_append previous destructors @ tl
    (* There can't be a block here, since we are by definition in the
       innermost block containing the statement. *)
    | hd :: tl -> aux_seq (hd :: previous) tl
  in
  let (has_inserted, res) = aux [] stmts in
  if  has_inserted then res
  else
    Kernel.fatal ~current:true
      "Statement %a not found in the current block" Printer.pp_stmt s

class vis flag = object(self)
  inherit Visitor.frama_c_inplace

  val blocks = Stack.create ()

  (* We sometimes move labels between statements. This table maps the old
     statements to the new ones. *)
  val moved_labels = Cil_datatype.Stmt.Hashtbl.create 17

  (* List of goto statements encountered in a function. If their target label
     has been moved, they need to be updated afterwards. *)
  val mutable gotos = []

  (* Updates the goto statements whose target has been changed after the
     introduction of the vla destructors. No destructor can have been added
     between a switch statement and its cases, so no need to update switches. *)
  method! vfunc _fundec =
    let update_target sref =
      try
        let new_target = Cil_datatype.Stmt.Hashtbl.find moved_labels !sref in
        sref := new_target
      with Not_found -> ()
    in
    let update_goto stmt = match stmt.skind with
      | Goto (sref, _loc) -> update_target sref
      | _ -> assert false
    in
    let post_goto_updater id =
      List.iter update_goto gotos;
      gotos <- [];
      Cil_datatype.Stmt.Hashtbl.clear moved_labels;
      id
    in
    Cil.DoChildrenPost post_goto_updater

  method! vblock b =
    Stack.push b.bstmts blocks;
    let post b =
      let stmts = Stack.pop blocks in
      let has_destructors, my_destructors = add_destructors b.blocals in
      let stmts =
        if has_destructors then begin
          flag := true;
          if stmts = [] then my_destructors
          else begin
            let stmt = Extlib.last stmts in
            if Cabs2cil.stmtFallsThrough stmt then stmts @ my_destructors
            else stmts
          end
        end
        else stmts
      in
      if stmts != b.bstmts then b.bstmts <- stmts;
      b
    in
    Cil.DoChildrenPost post

  method! vstmt_aux s =
    let inspect_closed_blocks b =
      (* blocks are sorted from innermost to outermost. The fold_left
         will give us the list in appropriate order for add_destructors
         which expects variable from oldest to newest.
      *)
      let vars = List.fold_left (fun acc b -> b.blocals @ acc) [] b in
      let has_destructors, stmts = add_destructors vars in
      if has_destructors then begin
        flag:=true;
        let curr_block = Stack.pop blocks in
        (* Moves the labels of [s] into the first destructor, as any goto
           jumping to [s] must also apply the destructors. *)
        let first_destructor = List.hd stmts in
        first_destructor.labels <- s.labels;
        s.labels <- [];
        (* Retains the move of labels to update later the gotos jumping to s. *)
        Cil_datatype.Stmt.Hashtbl.add moved_labels s first_destructor;
        let curr_block = insert_destructors stmts s curr_block in
        Stack.push curr_block blocks;
      end;
      Cil.SkipChildren
    in
    let abort_if_non_trivial_type kind v =
      if Cil.hasAttribute Cabs2cil.frama_c_destructor v.vattr then
        Kernel.abort
          "%a, cannot jump from %s statement \
           bypassing initialization of variable %a, declared at %a"
          Printer.pp_location (Cil_datatype.Stmt.loc s) kind
          Printer.pp_varinfo v Printer.pp_location v.vdecl
    in
    let check_def_domination kind b s v =
      if v.vdefined then begin
        let def = Cil.find_def_stmt b v in
        if not (Dominators.dominates s def) then
          (* if the jump's target [s] dominates the definition [def],
             jumping to it from outside the block will not prevent the
             initialization, hence is permitted: technically, the scope only
             begins at the end of the declaration part of [def].
             See C11, 6.2.1§7 and C++11, stmt.dcl§3.
          *)
          abort_if_non_trivial_type kind v
      end else abort_if_non_trivial_type kind v
    in
    let inspect_local_vars kind b s lv =
      List.iter (check_def_domination kind b s) lv
    in
    let treat_jump_close s =
      match s.succs with
      | [ succ ] ->
        inspect_closed_blocks (Kernel_function.blocks_closed_by_edge s succ)
      | _ ->
        Kernel.fatal ~current:true
          "%a in function %a is expected to have a single successor"
          Printer.pp_stmt s
          Kernel_function.pretty (Extlib.the self#current_kf)
    in
    let treat_succ_open kind s succ =
      let blocks = Kernel_function.blocks_opened_by_edge s succ in
      List.iter (fun b -> inspect_local_vars kind b succ b.blocals) blocks
    in
    let treat_jump_open k s = List.iter (treat_succ_open k s) s.succs in
    match s.skind with
    (* jump to a statement inside the function.
    *)
    | Break _ | Continue _ -> treat_jump_close s
    (* For goto, we must verify in addition that we do not enter the scope
       of VLA or similar non trivial types (C++). See 6.8.6.1§1 of C11 and
       stmt.dcl§3 of C++11
    *)
    | Goto _ ->
      gotos <- s :: gotos;
      treat_jump_open "goto" s; treat_jump_close s
    (* Ensures that there's no VLA declared between the switch and the case
       label. See 6.8.4§2 of C11 and stmt.dcl§3 and footnote 88 of C++11. *)
    | Switch _ -> treat_jump_open "switch" s; Cil.DoChildren
    (* jump outside of the function: all currently opened blocks are closed. *)
    | Return _ | Throw _ ->
      inspect_closed_blocks (Kernel_function.find_all_enclosing_blocks s)
    (* no jump yet, visit children *)
    | _ -> Cil.DoChildren

end

let treat_one_function flag kf =
  let my_flag = ref false in
  let vis = new vis my_flag in
  ignore (Visitor.visitFramacKf vis kf);
  if !my_flag then begin
    flag := true;
    File.must_recompute_cfg (Kernel_function.get_definition kf)
  end

let add_destructor _ast =
  let has_grown = ref false in
  Globals.Functions.iter (treat_one_function has_grown);
  if !has_grown then Ast.mark_as_grown ()

let transform_category =
  File.register_code_transformation_category "expand_destructors"

let () =
  let after = [Exn_flow.transform_category] in
  File.add_code_transformation_after_cleanup
    ~after transform_category add_destructor

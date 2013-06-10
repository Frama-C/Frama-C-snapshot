(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Cil
open Cil_types
open Cil_datatype
open Db
open Visitor
open Options

let print_results fmt a =
  List.iter
    (fun s -> 
      Format.fprintf fmt "@\nsid %d: %a" 
	s.sid (Printer.without_annot Printer.pp_stmt) s) 
    a

let compute_from_stmt stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  Compute_impact.impacted_stmts ~skip ~reason kf [stmt]

let compute_multiple_stmts skip kf ls =
  debug "computing impact of statement(s) %a" 
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls;
  let reason = Options.Reason.get () in
  let res, _, _ = Compute_impact.impacted_nodes ~skip ~reason kf ls in
  let res_nodes = Compute_impact.result_to_nodes res in
  let res_stmts = Compute_impact.nodes_to_stmts res_nodes in
  if Print.get () then begin
    result "impacted statements of stmt(s) %a are:%a"
      (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls
      print_results res_stmts
  end;
  res_nodes

let slice (stmts:stmt list) =
  feedback ~level:2 "beginning slicing";
  let name = "impact slicing" in
  let slicing = !Db.Slicing.Project.mk_project name in
  let select sel ({ sid = id } as stmt) =
    let kf = Kernel_function.find_englobing_kf stmt in
    debug ~level:3 "selecting sid %d (of %s)" id (Kernel_function.get_name kf);
    !Db.Slicing.Select.select_stmt sel ~spare:false stmt kf
  in
  let sel = List.fold_left select Db.Slicing.Select.empty_selects stmts in
  debug ~level:2 "applying slicing request";
  !Db.Slicing.Request.add_persistent_selection slicing sel;
  !Db.Slicing.Request.apply_all_internal slicing;
  !Db.Slicing.Slice.remove_uncalled slicing;
  let extracted_prj = !Db.Slicing.Project.extract name slicing in
  !Db.Slicing.Project.print_extracted_project ?fmt:None ~extracted_prj ;
  feedback ~level:2 "slicing done"

(* TODO: change function to generate on-the-fly the relevant pdg nodes *)
let all_pragmas_kf _kf l =
  List.fold_left
    (fun acc (s, a) ->
      match a.annot_content with
        | APragma (Impact_pragma IPstmt) -> s :: acc
        | APragma (Impact_pragma (IPexpr _)) ->
            Options.not_yet_implemented "impact pragmas: expr"
        | _ -> assert false)
    [] l

let compute_pragmas () =
  Ast.compute ();
  let pragmas = ref [] in
  let visitor = object
    inherit Visitor.frama_c_inplace as super

    method vfunc f =
      pragmas := [];
      super#vfunc f

    method vstmt_aux s =
      pragmas :=
        List.map
          (fun a -> s, a)
        (Annotations.code_annot ~filter:Logic_utils.is_impact_pragma s)
      @ !pragmas;
      DoChildren
  end
  in
  (* fill [pragmas] with all the pragmas of all the selected functions *)
  let pragmas = Pragma.fold
    (fun f acc ->
       try
         let kf = Globals.Functions.find_def_by_name f in
         match kf.fundec with
         | Definition(f, _) ->
             ignore (visitFramacFunction visitor f);
             if !pragmas != [] then (kf, !pragmas) :: acc else acc
         | Declaration _ -> assert false
       with Not_found ->
         abort "function %s not found." f
    ) []
  in
  let skip = Compute_impact.skip () in
  (* compute impact analyses on each kf *)
  let nodes = List.fold_left
    (fun nodes (kf, pragmas) ->
       let pragmas_stmts = all_pragmas_kf kf pragmas in
       PdgTypes.NodeSet.union nodes
         (compute_multiple_stmts skip kf pragmas_stmts)
    ) PdgTypes.NodeSet.empty pragmas
  in
  let stmts = Compute_impact.nodes_to_stmts nodes in
  if Options.Slicing.get () then ignore (slice stmts);
  stmts;
;;

let main () =
  if is_on () then begin
    feedback "beginning analysis";
    assert (not (Pragma.is_empty ()));
    ignore (!Impact.compute_pragmas ());
    feedback "analysis done"
  end
let () = Db.Main.extend main

let () =
  (* compute_pragmas *)
  Db.register
    (Db.Journalize
       ("Impact.compute_pragmas",
        Datatype.func Datatype.unit (Datatype.list Stmt.ty)))
    Impact.compute_pragmas
    compute_pragmas;
  (* from_stmt *)
  Db.register
    (Db.Journalize
       ("Impact.from_stmt", Datatype.func Stmt.ty (Datatype.list Stmt.ty)))
    Impact.from_stmt
    compute_from_stmt;
  (* slice *)
  Db.register
    (Db.Journalize
       ("Impact.slice", Datatype.func (Datatype.list Stmt.ty) Datatype.unit))
    Impact.slice
    slice

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

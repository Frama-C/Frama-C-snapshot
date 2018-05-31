(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open Cil_datatype

let rec pp_stmt fmt s = match s.skind with
  | Instr _ | Return _ | Goto _ | Break _ | Continue _ | TryFinally _
  | TryExcept _ | Throw _ | TryCatch _ -> 
    Printer.without_annot Printer.pp_stmt fmt s
  | If (e, _, _, _) ->
    Format.fprintf fmt "if(%a) <..>" Printer.pp_exp e
  | Switch (e, _, _, _) ->
    Format.fprintf fmt "switch(%a)<..>" Printer.pp_exp e
  | Loop _ -> Format.fprintf fmt "while (...)"
  | Block b ->
    begin match b.bstmts with
    | [] -> Format.fprintf fmt "<Block {}>"
    | s :: _ -> Format.fprintf fmt "<Block { %a }>" pp_stmt s
    end
  | UnspecifiedSequence _ -> Format.fprintf fmt "TODO"

let print_results fmt a =
  Pretty_utils.pp_list
    (fun fmt s -> 
      Format.fprintf fmt "@[<hov 2>%a (sid %d): %a@]" 
	Printer.pp_location (Stmt.loc s) s.sid pp_stmt s
    ) fmt a

let compute_from_stmt stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  Compute_impact.stmts_impacted ~skip ~reason kf [stmt]

let compute_from_nodes kf nodes =
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  let r = Compute_impact.nodes_impacted ~skip ~reason kf nodes in
  Pdg_aux.NS.fold
    (fun (n, _z) acc -> PdgTypes.NodeSet.add n acc)
    r PdgTypes.NodeSet.empty


let compute_multiple_stmts skip kf ls =
  Options.debug "computing impact of statement(s) %a"
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls;
  let reason = Options.Reason.get () in
  let res, _, _ = Compute_impact.nodes_impacted_by_stmts ~skip ~reason kf ls in
  let res_nodes = Compute_impact.result_to_nodes res in
  let res_stmts = Compute_impact.nodes_to_stmts res_nodes in
  if Options.Print.get () then begin
    Options.result "@[<v 2>@[impacted statements of stmt(s) %a are:@]@ %a@]"
      (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls
      print_results res_stmts
  end;
  res_nodes

(* Slice on the given list of stmts *)
let slice (stmts:stmt list) =
  Options.feedback ~level:2 "beginning slicing";
  let name = "impact slicing" in
  Slicing.Api.Project.reset_slicing ();
  let select sel ({ sid = id } as stmt) =
    let kf = Kernel_function.find_englobing_kf stmt in
    Options.debug ~level:3 "selecting sid %d (of %s)"
      id
      (Kernel_function.get_name kf);
    Slicing.Api.Select.select_stmt sel ~spare:false stmt kf
  in
  let sel = List.fold_left select Slicing.Api.Select.empty_selects stmts in
  Options.debug ~level:2 "applying slicing request";
  Slicing.Api.Request.add_persistent_selection sel;
  Slicing.Api.Request.apply_all_internal ();
  Slicing.Api.Slice.remove_uncalled ();
  let extracted_prj = Slicing.Api.Project.extract name in
  Options.feedback ~level:2 "slicing done";
  extracted_prj

let all_pragmas_kf l =
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

    method! vfunc f =
      pragmas := [];
      super#vfunc f

    method! vstmt_aux s =
      pragmas :=
        List.map
          (fun a -> s, a)
        (Annotations.code_annot ~filter:Logic_utils.is_impact_pragma s)
      @ !pragmas;
      Cil.DoChildren
  end
  in
  (* fill [pragmas] with all the pragmas of all the selected functions *)
  let pragmas =
    Options.Pragma.fold
      (fun kf acc ->
        (* Pragma option only accept defined functions. *)
        let f = Kernel_function.get_definition kf in
        ignore (Visitor.visitFramacFunction visitor f);
        if !pragmas != [] then (kf, !pragmas) :: acc else acc)
      []
  in
  let skip = Compute_impact.skip () in
  (* compute impact analyses on each kf *)
  let nodes = List.fold_left
    (fun nodes (kf, pragmas) ->
       let pragmas_stmts = all_pragmas_kf pragmas in
       Pdg_aux.NS.union nodes (compute_multiple_stmts skip kf pragmas_stmts)
    ) Pdg_aux.NS.empty pragmas
  in
  let stmts = Compute_impact.nodes_to_stmts nodes in
  if Options.Slicing.get () then ignore (slice stmts);
  stmts;
;;

let compute_pragmas =
  Journal.register
    "Impact.compute_pragmas"
    (Datatype.func Datatype.unit (Datatype.list Stmt.ty))
    compute_pragmas

let from_stmt =
  Journal.register
    "Impact.from_stmt"
    (Datatype.func Stmt.ty (Datatype.list Stmt.ty))
    compute_from_stmt

let from_nodes =
  Journal.register
    "Impact.from_nodes"
    (Datatype.func2 Kernel_function.ty
       (Datatype.list PdgTypes.Node.ty)
       (PdgTypes.NodeSet.ty))
    compute_from_nodes

let main () =
  if Options.is_on () then begin
    Options.feedback "beginning analysis";
    assert (not (Options.Pragma.is_empty ()));
    ignore (compute_pragmas ());
    Options.feedback "analysis done"
  end
let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

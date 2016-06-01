(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types
open Cil_datatype
open Visitor
open Options

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
  debug "computing impact of statement(s) %a" 
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls;
  let reason = Options.Reason.get () in
  let res, _, _ = Compute_impact.nodes_impacted_by_stmts ~skip ~reason kf ls in
  let res_nodes = Compute_impact.result_to_nodes res in
  let res_stmts = Compute_impact.nodes_to_stmts res_nodes in
  if Print.get () then begin
    result "@[<v 2>@[impacted statements of stmt(s) %a are:@]@ %a@]"
      (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls
      print_results res_stmts
  end;
  res_nodes

(* Slice on the given list of stmts *)
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
      DoChildren
  end
  in
  (* fill [pragmas] with all the pragmas of all the selected functions *)
  let pragmas = Pragma.fold
    (fun kf acc ->
      (* Pragma option only accept defined functions. *)
      let f = Kernel_function.get_definition kf in
      ignore (visitFramacFunction visitor f);
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

let main () =
  if is_on () then begin
    feedback "beginning analysis";
    assert (not (Pragma.is_empty ()));
    ignore (!Db.Impact.compute_pragmas ());
    feedback "analysis done"
  end
let () = Db.Main.extend main

let () =
  (* compute_pragmas *)
  Db.register
    (Db.Journalize
       ("Impact.compute_pragmas",
        Datatype.func Datatype.unit (Datatype.list Stmt.ty)))
    Db.Impact.compute_pragmas
    compute_pragmas;
  (* from_stmt *)
  Db.register
    (Db.Journalize
       ("Impact.from_stmt", Datatype.func Stmt.ty (Datatype.list Stmt.ty)))
    Db.Impact.from_stmt
    compute_from_stmt;
  (* from_nodes *)
  Db.register
    (Db.Journalize
       ("Impact.from_nodes", Datatype.func2 Kernel_function.ty (Datatype.list PdgTypes.Node.ty) (PdgTypes.NodeSet.ty)))
    Db.Impact.from_nodes
    compute_from_nodes;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

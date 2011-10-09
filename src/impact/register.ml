(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
  List.iter (fun s -> Format.fprintf fmt "@\nsid %d: %a" s.sid Cil.d_stmt s) a

let from_stmt s =
  let kf = Kernel_function.find_englobing_kf s in
  try
    Dynamic.get
      ~plugin:"Security_slicing"
      "impact_analysis"
      (Datatype.func2 Kernel_function.ty Stmt.ty (Datatype.list Stmt.ty))
      kf s
  with
  | Dynamic.Incompatible_type _ ->
    error "versions of plug-ins `impact' and `Security_slicing' seem \
incompatible.\nCheck the environement variable FRAMAC_PLUGIN.\n\
Analysis discarded.";
    []
  | Dynamic.Unbound_value _ ->
    error "cannot access to plug-in `Security_slicing'.\n\
Are you sure that it is loaded? Check the environement variable \
FRAMAC_PLUGIN.\n\
Analysis discarded.";
    []

let compute_one_stmt s =
  debug "computing impact of statement %d" s.sid;
  let res = from_stmt s in
  if Print.get () then begin
    result "impacted statements of stmt %d are:%a" s.sid print_results res
  end;
  res

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

let on_pragma f =
  List.fold_left
    (fun acc (s, a) ->
       match a with
       | User a ->
           (match a.annot_content with
            | APragma (Impact_pragma IPstmt) -> f acc s
            | APragma (Impact_pragma (IPexpr _)) ->
                raise (Extlib.NotYetImplemented "impact pragmas: expr")
            | _ -> assert false)
       | _ -> assert false)

let compute_pragmas () =
  Ast.compute ();
  let pragmas = ref [] in
  let visitor = object
    inherit Visitor.generic_frama_c_visitor
      (Project.current ()) (inplace_visit ())
    method vstmt_aux s =
      pragmas :=
        List.map
          (fun a -> s, a)
          (Annotations.get_filter Logic_utils.is_impact_pragma s)
      @ !pragmas;
      DoChildren
  end in
  (* fill [pragmas] with all the pragmas of all the selected functions *)
  Pragma.iter
    (fun s ->
       try
         match (Globals.Functions.find_def_by_name s).fundec with
         | Definition(f, _) -> ignore (visitFramacFunction visitor f)
         | Declaration _ -> assert false
       with Not_found ->
         abort "function %s not found." s);
  (* compute impact analyses on [!pragmas] *)
  let res = on_pragma (fun acc s -> compute_one_stmt s @ acc) [] !pragmas in
  if Options.Slicing.get () then ignore (slice res)

let main _fmt =
  if is_on () then begin
    feedback "beginning analysis";
    assert (not (Pragma.is_empty ()));
    !Impact.compute_pragmas ();
    feedback "analysis done"
  end
let () = Db.Main.extend main

let () =
  (* compute_pragmas *)
  Db.register
    (Db.Journalize
       ("Impact.compute_pragmas", Datatype.func Datatype.unit Datatype.unit))
    Impact.compute_pragmas
    compute_pragmas;
  (* from_stmt *)
  if not !Config.is_gui then
    Db.register
      (Db.Journalize
         ("Impact.from_stmt", Datatype.func Stmt.ty (Datatype.list Stmt.ty)))
      Impact.from_stmt
      from_stmt;
  (* slice *)
  Db.register
    (Db.Journalize
       ("Impact.slice", Datatype.func (Datatype.list Stmt.ty) Datatype.unit))
    Impact.slice
    slice

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

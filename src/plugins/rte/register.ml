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

let journal_register ?comment is_dyn name ty_arg fctref fct = 
  let ty = Datatype.func ty_arg Datatype.unit in
  Db.register (Db.Journalize("RteGen." ^ name, ty)) fctref fct;
  if is_dyn then
    let _ =
      Dynamic.register ?comment ~plugin:"RteGen" name ty ~journalize:true fct
    in
    ()

let nojournal_register fctref fct = 
  Db.register Db.Journalization_not_required fctref (fun () -> fct)

let () = 
  journal_register false
    "annotate_kf" Kernel_function.ty Db.RteGen.annotate_kf Visit.annotate_kf;
  journal_register false "compute" Datatype.unit Db.RteGen.compute 
    Visit.compute;
  journal_register true
    ~comment:"Generate all RTE annotations in the \
  given function."
    "do_all_rte" Kernel_function.ty Db.RteGen.do_all_rte Visit.do_all_rte;
  journal_register false
    ~comment:"Generate all RTE annotations except pre-conditions \
    in the given function."
    "do_rte" Kernel_function.ty Db.RteGen.do_rte Visit.do_rte;
  let open Generator in
  let open Db.RteGen in
  nojournal_register get_signedOv_status Signed_overflow.accessor;
  nojournal_register get_divMod_status Div_mod.accessor;
  nojournal_register get_initialized_status Initialized.accessor;
  nojournal_register get_signed_downCast_status Signed_downcast.accessor;
  nojournal_register get_memAccess_status Mem_access.accessor;
  nojournal_register get_pointerCall_status Pointer_call.accessor;
  nojournal_register get_unsignedOv_status Unsigned_overflow.accessor;
  nojournal_register get_unsignedDownCast_status Unsigned_downcast.accessor;
  nojournal_register get_float_to_int_status Float_to_int.accessor;
  nojournal_register get_finite_float_status Finite_float.accessor;
  nojournal_register get_all_status all_statuses;
;;

(* dynamic registration *)

let _ =
  Dynamic.register
    ~comment:"The emitter used for generating RTE annotations"
    ~plugin:"RteGen"
    "emitter"
    Emitter.ty
    ~journalize:false
    Generator.emitter

(* retrieve list of generated rte annotations (not precond) for
   a given stmt *)
let _ =
  Dynamic.register
    ~comment:"Get the list of annotations previously emitted by RTE for the \
given statement."
    ~plugin:"RteGen"
    "get_rte_annotations"
    (Datatype.func
       Cil_datatype.Stmt.ty
       (let module L = Datatype.List(Cil_datatype.Code_annotation) in L.ty))
    ~journalize:true
    Visit.rte_annotations

let _ =
  Dynamic.register
    ~comment:"Generate RTE annotations corresponding to the given stmt of \
the given function."
    ~plugin:"RteGen"
    "stmt_annotations"
    (Datatype.func2 Kernel_function.ty Cil_datatype.Stmt.ty
       (let module L = Datatype.List(Cil_datatype.Code_annotation) in L.ty))
    ~journalize:false
    Visit.do_stmt_annotations

let _ =
  Dynamic.register
    ~comment:"Generate RTE annotations corresponding to the given exp \
of the given stmt in the given function."
    ~plugin:"RteGen"
    "exp_annotations"
    (Datatype.func3 Kernel_function.ty Cil_datatype.Stmt.ty Cil_datatype.Exp.ty 
       (let module L = Datatype.List(Cil_datatype.Code_annotation) in L.ty))
    ~journalize:false
    Visit.do_exp_annotations

let main () =
  (* reset "rte generated"/"called precond generated" properties for all
     functions *)
  if Options.Enabled.get () then begin
    Options.feedback ~level:2 "generating annotations";
    !Db.RteGen.compute ();
    Options.feedback ~level:2 "annotations computed"
  end

let () = Db.Main.extend main

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)

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

(** Slicing  module public macros that should be used to avoid  using the type
* concrete definition from other modules.
*)

(**/**)

open Cil_types

(**/**)
  
(** {2 Options} *)

let str_level_option opt = match opt with
  |  SlicingInternals.DontSlice -> "DontSlice"
  |  SlicingInternals.DontSliceButComputeMarks -> "DontSliceButComputeMarks"
  |  SlicingInternals.MinNbSlice -> "MinNbSlice"
  |  SlicingInternals.MaxNbSlice -> "MaxNbSlice"

let translate_num_to_slicing_level n =
  match n with
      | 0 -> SlicingInternals.DontSlice
      | 1 -> SlicingInternals.DontSliceButComputeMarks
      | 2 -> SlicingInternals.MinNbSlice
      | 3 -> SlicingInternals.MaxNbSlice
      | _ -> raise SlicingTypes.WrongSlicingLevel

let get_default_level_option defined_function =
  if defined_function || (SlicingParameters.Mode.SliceUndef.get ()) then
    translate_num_to_slicing_level (SlicingParameters.Mode.Calls.get ())
  else SlicingInternals.DontSlice


(** {2 Getting [fct_info] and others } *)

(** {4 getting [svar]} *)

let fi_svar fi = Kernel_function.get_vi fi.SlicingInternals.fi_kf

let ff_svar ff = fi_svar (ff.SlicingInternals.ff_fct)

(** {4 getting [fct_info]} *)

(** Get the fct_info if it exists or build a new fct_info. *)
let get_kf_fi proj kf =
  let fct_var = Kernel_function.get_vi kf in
  try Cil_datatype.Varinfo.Hashtbl.find proj.SlicingInternals.functions fct_var
  with Not_found ->
    let fi_def, is_def =
      match kf.fundec with
        | Declaration _ -> None, false
        | Definition _ when !Db.Value.use_spec_instead_of_definition kf ->
            None, false
        | Definition (def, _) -> Some def, true
    in
    let new_fi = {
      SlicingInternals.fi_kf = kf;
      SlicingInternals.fi_def = fi_def;
      SlicingInternals.fi_project = proj;
      SlicingInternals.fi_top = None;
      SlicingInternals.fi_level_option = get_default_level_option is_def;
      SlicingInternals.fi_init_marks = None ;
      SlicingInternals.fi_slices = [] ;
      SlicingInternals.fi_next_ff_num = 1;
      SlicingInternals.f_called_by = [] }
    in
    Cil_datatype.Varinfo.Hashtbl.add proj.SlicingInternals.functions fct_var new_fi;
    new_fi

let fold_fi f acc proj =
  Cil_datatype.Varinfo.Hashtbl.fold
    (fun _v fi acc -> f acc fi)
    proj.SlicingInternals.functions
    acc



(** {4 getting num id} *)
let get_ff_id ff = ff.SlicingInternals.ff_id

(** {4 getting names} *)

let fi_name fi = let svar = fi_svar fi in svar.Cil_types.vname

(** get the name of the function corresponding to that slice. *)
let ff_name ff =
  let fi = ff.SlicingInternals.ff_fct in
  let ff_id = get_ff_id ff in
  let fct_name = fi_name fi in
    (fct_name ^ "_slice_" ^ (string_of_int (ff_id)))

let f_name f = match f with
  | SlicingInternals.FctSrc fct -> fi_name fct
  | SlicingInternals.FctSliced ff -> ff_name ff

let ff_src_name ff = fi_name ff.SlicingInternals.ff_fct

(** {4 getting [kernel_function]} *)

let get_fi_kf fi = fi.SlicingInternals.fi_kf

let get_ff_kf ff =  let fi = ff.SlicingInternals.ff_fct in get_fi_kf fi

let get_pdg_kf pdg = PdgTypes.Pdg.get_kf pdg

(** {4 getting PDG} *)

let get_fi_pdg fi = let kf = get_fi_kf fi in  !Db.Pdg.get kf

let get_ff_pdg ff = get_fi_pdg ff.SlicingInternals.ff_fct

(** {4 getting the slicing level} *)


let ff_slicing_level ff = ff.SlicingInternals.ff_fct.SlicingInternals.fi_level_option

let change_fi_slicing_level fi slicing_level =
    fi.SlicingInternals.fi_level_option <- slicing_level

(** @raise SlicingTypes.WrongSlicingLevel if [n] is not valid.
* *)
let change_slicing_level proj kf n =
  let slicing_level = translate_num_to_slicing_level n in
  let fi = get_kf_fi proj kf in (* build if if it doesn't exist *)
    change_fi_slicing_level fi slicing_level

(** {2 functions and slices} *)

let fi_slices fi = fi.SlicingInternals.fi_slices

(** {4 Comparisons} *)

let equal_fi fi1 fi2 =
  let v1 = fi_svar fi1 in
  let v2 = fi_svar fi2 in
    Cil_datatype.Varinfo.equal v1 v2

let equal_ff ff1 ff2 = (equal_fi ff1.SlicingInternals.ff_fct ff2.SlicingInternals.ff_fct) &&
                       ((get_ff_id ff1) = (get_ff_id ff2))


(** {2 Calls} *)

let same_call c1 c2 = (c1.sid = c2.sid)

let same_ff_call (f1,c1) (f2,c2) =
  equal_ff f1 f2 && same_call c1 c2

let is_call_stmt stmt =
  match stmt.skind with Instr (Call _) -> true | _ -> false

let get_called_kf call_stmt = match call_stmt.skind with
  | Instr (Call (_, funcexp,_,_)) ->
    let _funcexp_dpds, called_functions =
      !Db.Value.expr_to_kernel_function
        ~with_alarms:CilE.warn_none_mode
        ~deps:(Some Locations.Zone.bottom)
        (Kstmt call_stmt)
        funcexp
    in
    (match Kernel_function.Hptset.contains_single_elt called_functions with
     | Some kf -> kf
     | _ -> raise SlicingTypes.PtrCallExpr)
  | _ -> invalid_arg "Not a call statement !"

let is_variadic kf =
  let varf = Kernel_function.get_vi kf in
  match varf.vtype with
  | TFun (_, _, is_variadic, _) -> is_variadic
  | _ -> assert false

(** get the [fct_info] of the called function, if we know it *)
let get_fi_call proj call =
  try
    let kf = get_called_kf call in
      if is_variadic kf then None
      else
        let fct_info = get_kf_fi proj kf in
          Some fct_info
  with SlicingTypes.PtrCallExpr -> None

let is_src_fun_called proj kf =
  let fi = get_kf_fi proj kf in
  match fi.SlicingInternals.f_called_by with [] -> false | _ -> true

let is_src_fun_visible proj kf =
  let is_fi_top fi = match fi.SlicingInternals.fi_top with None -> false | Some _ -> true
  in is_src_fun_called proj kf || is_fi_top (get_kf_fi proj kf)

let fi_has_persistent_selection fi =
        (match fi.SlicingInternals.fi_init_marks with None -> false | _ -> true)

let has_persistent_selection proj kf =
  let fi = get_kf_fi proj kf in
    fi_has_persistent_selection fi


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

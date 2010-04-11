(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Slicing module types. *)

exception Slicing_Internal_Error of string
exception ChangeCallErr of string
exception PtrCallExpr
exception CantRemoveCalledFf
exception WrongSlicingLevel

(** raised when someone tries to build more than one slice for the entry point.
* *)
exception OnlyOneEntryPointSlice

(** raised when one triesy to select something in a function where we are not
* able to compute the Pdg. *)
exception NoPdg

(** {2 Public types}
  * These types are the only one that should be used by the API functions.
  * Public type definitions should be hidden to the outside world,
  * but it is not really possible to have abstract types since Slicing has to
  * use Db.Slicing functions...  So, it is up to the user of this module to use
  * only this public part.
 *)

(** contains global things that has been computed so far
  for the slicing project.
  This includes :
  - the slices of the functions,
  - and the queue of actions to be applied.
  *)
type sl_project   = SlicingInternals.t_project

(** Type of the selections
* (we store the varinfo because we cannot use the kernel_function in this file)
* *)
type sl_select  = (Cil_types.varinfo * SlicingInternals.t_fct_user_crit)

(** A set of selections, grouped by function *)
type sl_selects = (SlicingInternals.t_fct_user_crit Cilutil.VarinfoMap.t)

(** Function slice *)
type sl_fct_slice = SlicingInternals.t_fct_slice

(** Marks : used to put 'colors' in the result *)
type sl_mark = SlicingInternals.t_pdg_mark

(** {3 For the journalization of values of these types} *)

let pp_sl_project p_caller fmt p =
  let pp fmt =
    Format.fprintf fmt "@[<hv 2>!Db.Slicing.Project.from_unique_name@;%S@]"
      p.SlicingInternals.name
  in
  Type.par p_caller Type.Call fmt pp

let dyn_sl_project =
  Type.register
    ~name:"Db.Slicing.Project.t"
    ~value_name:(Some "SlicingTypes.dyn_sl_project")
    ~pp:pp_sl_project
    ~varname:(fun s -> "sl_project_" ^ s.SlicingInternals.name)
    [ SlicingInternals.dummy_t_project ]


let dummy_sl_select =
  Kernel_type.varinfo_dummy, SlicingInternals.dummy_t_fct_user_crit

let dyn_sl_select =
  Type.register
    ~name:"Db.Slicing.Selection.t"
    ~value_name:(Some "SlicingTypes.dyn_sl_select")
    ~varname:(fun _ -> "sl_select")
    [ dummy_sl_select ]

let dyn_sl_selects =
  Type.register
    ~name:"Db.Slicing.Selection.t_set"
    ~value_name:(Some "SlicingTypes.dyn_sl_selects")
    ~varname:(fun _ -> "sl_select_set")
    [ (Cilutil.VarinfoMap.empty : sl_selects) ]

let pp_sl_fct_slice p_caller fmt ff =
  let pp fmt =
    Format.fprintf fmt
      "@[<hv 2>!Db.Slicing.Slice.from_num_id@;%a@;%a@;%d@]"
      (Type.pp dyn_sl_project Type.Call)
      ff.SlicingInternals.ff_fct.SlicingInternals.fi_project
      (Type.pp Kernel_type.kernel_function Type.Call)
      ff.SlicingInternals.ff_fct.SlicingInternals.fi_kf
      ff.SlicingInternals.ff_id
  in
  Type.par p_caller Type.Call fmt pp

let dyn_sl_fct_slice =
  Type.register
    ~name:"Db.Slicing.Slice.t"
    ~value_name:(Some "SlicingTypes.dyn_sl_fct_slice")
    ~pp:pp_sl_fct_slice
    ~varname:(fun _ -> "sl_slice")
    [ SlicingInternals.dummy_t_fct_slice ]

let pp_sl_mark p fmt m =
  let pp = match m.SlicingInternals.m1, m.SlicingInternals.m2 with
    | SlicingInternals.Spare, _ -> None
    | _, SlicingInternals.Spare -> None
    | SlicingInternals.Cav mark1, SlicingInternals.Cav mark2 ->
        if (PdgTypes.Dpd.is_bottom mark2) then
	  (* use [!Db.Slicing.Mark.make] contructor *)
          Some (fun fmt ->
                  Format.fprintf fmt "@[<hv 2>!Db.Slicing.Mark.make@;~addr:%b@;~data:%b@;~ctrl:%b@]"
                    (PdgTypes.Dpd.is_addr mark1)
		    (PdgTypes.Dpd.is_data mark1)
		    (PdgTypes.Dpd.is_ctrl mark1))
        else
	  None
  in
  let pp = match pp with
    | Some pp -> pp
    | None ->
	let pp fmt sub_m = match sub_m with
	    (* use internals constructors *)
          | SlicingInternals.Spare -> Format.fprintf fmt "SlicingInternals.Spare"
          | SlicingInternals.Cav pdg_m -> Format.fprintf fmt
              "@[<hv 2>(SlicingInternals.Cav@;@[<hv 2>(PdgTypes.Dpd.make@;~a:%b@;~d:%b@;~c:%b@;())@])@]"
		(PdgTypes.Dpd.is_addr pdg_m)
		(PdgTypes.Dpd.is_data pdg_m)
		(PdgTypes.Dpd.is_ctrl pdg_m)
	in
        fun fmt ->
          Format.fprintf fmt "@[<hv 2>SlicingInternals.create_sl_mark@;~m1:%a@;~m2:%a@]"
            pp m.SlicingInternals.m1 pp m.SlicingInternals.m2
  in Type.par p Type.Call fmt pp

let dyn_sl_mark =
  Type.register
    ~name:"Db.Slicing.Mark.t"
    ~value_name:(Some "SlicingTypes.dyn_sl_mark")
    ~pp:pp_sl_mark
    ~varname:(fun _ -> "sl_mark")
    [ SlicingInternals.dummy_t_pdg_mark ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

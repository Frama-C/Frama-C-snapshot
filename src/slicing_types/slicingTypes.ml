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
type sl_project   = SlicingInternals.project

(** Type of the selections
* (we store the varinfo because we cannot use the kernel_function in this file)
* *)
type sl_select = Cil_types.varinfo * SlicingInternals.fct_user_crit

module Fct_user_crit =
  Datatype.Make
    (struct
      include Datatype.Undefined (* TODO: unmarshal *)
      type t = SlicingInternals.fct_user_crit
      let reprs = [ SlicingInternals.dummy_fct_user_crit ]
      let name = "SlicingTypes.Fct_user_crit"
      let mem_project = Datatype.never_any_project
      let varname _ = "user_criteria"
     end)

(** Function slice *)
type sl_fct_slice = SlicingInternals.fct_slice

(** Marks : used to put 'colors' in the result *)
type sl_mark = SlicingInternals.pdg_mark

(** {3 For the journalization of values of these types} *)

let pp_sl_project p_caller fmt p =
  let pp fmt =
    Format.fprintf fmt "@[<hv 2>!Db.Slicing.Project.from_unique_name@;%S@]"
      p.SlicingInternals.name
  in
  Type.par p_caller Type.Call fmt pp

module Sl_project =
  Datatype.Make
    (struct
      include Datatype.Undefined (* TODO: unmarshal *)
      type t = sl_project
      let reprs = [ SlicingInternals.dummy_project ]
      let name = "SlicingTypes.Sl_project"
      let internal_pretty_code = pp_sl_project
      let varname s = "sl_project_" ^ s.SlicingInternals.name
      let mem_project f s = f s.SlicingInternals.application
     end)

module Sl_select =
  Datatype.Make
    (struct
      include Datatype.Undefined (* TODO: unmarshal *)
      type t = sl_select
      let reprs =
        List.map
          (fun v -> v, SlicingInternals.dummy_fct_user_crit)
          Cil_datatype.Varinfo.reprs
      let name = "SlicingTypes.Sl_select"
      let varname _s = "sl_select"
      let mem_project = Datatype.never_any_project
     end)

let pp_sl_fct_slice p_caller fmt ff =
  let pp fmt =
    Format.fprintf fmt
      "@[<hv 2>!Db.Slicing.Slice.from_num_id@;%a@;%a@;%d@]"
      (Sl_project.internal_pretty_code Type.Call)
      ff.SlicingInternals.ff_fct.SlicingInternals.fi_project
      (Kernel_function.internal_pretty_code Type.Call)
      ff.SlicingInternals.ff_fct.SlicingInternals.fi_kf
      ff.SlicingInternals.ff_id
  in
  Type.par p_caller Type.Call fmt pp

module Sl_fct_slice =
  Datatype.Make
    (struct
      include Datatype.Undefined (* TODO: unmarshal *)
      open SlicingInternals
      type t = fct_slice
      let name = "SlicingTypes.Sl_fct_slice"
      let reprs = [ dummy_fct_slice ]
      let internal_pretty_code = pp_sl_fct_slice
      let mem_project f x = f x.ff_fct.fi_project.application
     end)

let dyn_sl_fct_slice = Sl_fct_slice.ty

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

module Sl_mark =
  Datatype.Make_with_collections
    (struct
      type t = SlicingInternals.pdg_mark
      let name = "SlicingTypes.Sl_mark"
      let structural_descr = Structural_descr.t_unknown
      let reprs = [ SlicingInternals.dummy_pdg_mark ]
      let compare = SlicingInternals.compare_pdg_mark
      let equal : t -> t -> bool = ( = )
      let hash = Hashtbl.hash
      let copy = Datatype.undefined
      let rehash = Datatype.undefined
      let internal_pretty_code = pp_sl_mark
      let pretty = Datatype.from_pretty_code
      let mem_project = Datatype.never_any_project
      let varname = Datatype.undefined
     end)

let dyn_sl_mark = Sl_mark.ty

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

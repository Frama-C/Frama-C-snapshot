(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** {2 Internals types}
  * Internals type definitions should be hidden to the outside world,
  * but it is not really possible to have abstract types since Slicing has to
  * use Db.Slicing functions...
 *)

open Cil_datatype

(** {3 About the PDG}
* As the PDG is not defined here anymore, look at
* {{:../pdg/PdgTypes.html}PdgTypes} for more information about it.
* *)

(** Nodes of the PDG *)
type t_pdg_node = PdgTypes.Node.t

(** {3 About options} *)

(** associate a level to each function in order to control how it will be
* specialized. This is only a hint used when the tool has to make a choice,
* but it doesn't forbid to the user to do whatever he wants
* (like building slices for a [DontSlice] function). *)
type t_level_option =
  | DontSlice (** don't build slice for the function :
                  ie. always call the source function. *)
  | DontSliceButComputeMarks
              (** don't slice the called functions,
              *   but compute the marks for them *)
  | MinNbSlice (** try to use existing slices, create at most one *)
  | MaxNbSlice (** most precise slices
                   (but merge slices with the same visibility,
                    even if they don't have the same marks) *)

(** {3 About function slice} *)

(** Kinds of elementary marks. *)
type t_mark = Cav of PdgTypes.Dpd.t
            | Spare

let compare_mark m1 m2 =
  if m1 == m2 then 0
  else match m1, m2 with
    | Spare, Spare -> 0
    | Cav d1, Cav d2 -> PdgTypes.Dpd.compare d1 d2
    | Cav _, Spare -> -1
    | Spare, Cav _ -> 1


(** Each PDG element has 2 marks to deal with interprocedural propagation *)
type t_pdg_mark = {m1 : t_mark ; m2 : t_mark }

let t_pdg_mark_packed_descr = Structural_descr.p_abstract
  (* Ok: Dpd.t is in fact int *)

let compare_pdg_mark p1 p2 =
  if p1 == p2 then 0
  else
    let r = compare_mark p1.m1 p2.m1 in
    if r = 0 then compare_mark p1.m2 p2.m2 else r


type t_call_id =  Cil_types.stmt

(** Type for all the informations related to any function,
* even if we don't have its definition.  *)
and t_fct_info = {
  fi_kf : Cil_types.kernel_function;
  fi_def : Cil_types.fundec option;
  fi_project : t_project;
  mutable fi_top : t_pdg_mark option;
          (** indicates if the function is maked top (=> src visible) *)
  mutable fi_level_option : t_level_option;
          (** level of specialisation for this function *)
  mutable fi_init_marks : t_ff_marks option;
          (** the marks that must be in every slices of that function *)
  mutable fi_slices : t_fct_slice list ;
          (** the list of the slices already computed for this function. *)
  mutable fi_next_ff_num : int;
          (** the number to assign to the next slice. *)
  mutable f_called_by : t_called_by;
          (** calls in slices that call source fct *)
}

and
  (** to represent where a function is called. *)
  t_called_by = (t_fct_slice * t_call_id) list

and
(** Function slice :
    created as soon as there is a criterion to compute it,
    even if the slice itself hasn't been computed yet.
  *)
 t_fct_slice  = {
    ff_fct : t_fct_info ;
    ff_id : int ;
    mutable ff_marks : t_ff_marks;
    mutable ff_called_by : t_called_by
    }

and
(** [t_fct_id] is used to identify either a source function or a sliced one.*)
  t_fct_id =
  | FctSrc of t_fct_info  (** source function *)
  | FctSliced of t_fct_slice (** sliced function *)

and
  t_called_fct =
  | CallSrc of t_fct_info option
    (** call the source function (might be unknown if the call uses pointer) *)
  | CallSlice of t_fct_slice

and
  (** information about a call in a slice which gives the function to call *)
  t_call_info = t_called_fct option

and
(** main part of a slice = mapping between the function elements
  * and information about them in the slice. *)
  t_marks_index = (t_pdg_mark, t_call_info) PdgIndex.FctIndex.t

and
  t_ff_marks = PdgTypes.Pdg.t * t_marks_index

and
  t_project = { name : string ;
                application : Project.t ;
                functions : t_fct_info Varinfo.Hashtbl.t;
                mutable actions : t_criterion list;
              }

and
(** Slicing criterion at the application level.
    When applied, they are translated into [t_fct_criterion]
*)
 t_appli_criterion =
  | CaGlobalData of Locations.Zone.t
    (** select all that is necessary to compute the given location. *)
  | CaCall of t_fct_info
    (** select all that is necessary to call the given function.
    * Its application generates requests to add persistent selection
    * to all the function callers. *)
  | CaOther

and
(** Base criterion for the functions. These are the only one that can
    really generate function slices. All the other criterions are
    translated in more basic ones.
    Note that to build such a base criterion, the PDG has to be already
    computed.
*)
  t_fct_base_criterion = t_pdg_mark PdgMarks.t_select

and
  (** Used to identify a location (zone) at a given program point.
      * The boolean tell if the point is before (true) or after the statement *)
  t_loc_point = Cil_types.stmt * Locations.Zone.t * bool

(** List of pdg nodes to be selected (see {!t_fct_user_crit})*)
(*type t_nodes = t_pdg_node list*)

and
  (** [t_node_or_dpds] tells how we want to select nodes,
      * or some of their dependencies (see {!t_fct_user_crit}). *)
  t_node_or_dpds = CwNode | CwAddrDpds | CwDataDpds | CwCtrlDpds

and
(** Tells which marks we want to put in the slice of a function *)
 t_fct_user_crit =
  (* | CuNodes of (t_pdg_node list * (t_node_or_dpds * t_pdg_mark) list) list *)
  | CuSelect of t_pdg_mark PdgMarks.t_select
  | CuTop of t_pdg_mark (** the function has probably no PDG,
                            but we nonetheless give a mark to propagate *)
and
(** kinds of actions that can be apply to a function *)
  t_fct_crit =
  | CcUserMark of t_fct_user_crit
      (** add marks to a slice *)
  | CcChooseCall of t_call_id
      (** have to choose what function to call here. *)
  | CcChangeCall of t_call_id * t_called_fct
      (** call the [t_called_fct] for the given call [t_call_id] *)
  | CcMissingOutputs of t_call_id * (t_pdg_mark PdgMarks.t_select) * bool
      (** this call is affected to a function that doesn't compute enough
      * outputs : we will have to choose between adding outputs to that slice,
      * or call another one. The boolean tells if the modifications would
      * change the visibility of some outputs. *)
  | CcMissingInputs of t_call_id * (t_pdg_mark PdgMarks.t_select) * bool
      (** the function calls a slice that has been modified :
      * and doesn't compute not enough inputs.
      * We will have to choose between adding marks to this function,
      * and call another slice.
      * The boolean tells if the modifications would
      * change the visibility of some inputs. *)
  | CcPropagate of (t_pdg_mark PdgMarks.t_select)
     (** simply propagate the given marks *)
  | CcExamineCalls of t_pdg_mark PdgMarks.t_info_called_outputs
and
(** Slicing criterion for a function.  *)
  t_fct_criterion =  {
  cf_fct : t_fct_id ;
    (** Identification of the {b RESULT} of this filter.
     * When it a a slice, it might be an existing slice that will be modified,
      * or a new one will be created during application.
      * When it is the source function, it means what the criterion has to be
      * applied on each existing slice, and stored into the inititial marks of
      * the function.
      *)
  cf_info : t_fct_crit
}
and
(** A slicing criterion is either an application level criterion,
  * or a function level one.  *)
  t_criterion =
  CrAppli of t_appli_criterion | CrFct of t_fct_criterion

(** {2 Internals values} *)

(** Internal function allowing creation of slicing marks
    which can break their type invariant ! *)
let create_sl_mark ~m1 ~m2 = { m1 = m1; m2 = m2 }

(** {3 For the journalization of these internals types} *)
let dummy_t_pdg_mark = {m1 = Spare ; m2 = Spare }

(** The whole project. *)
let dummy_t_project =
  { name = "";
    application = Project_skeleton.dummy;
    functions = Varinfo.Hashtbl.create 0;
    actions = [] }

let dummy_t_fct_info = {
  fi_kf = Kernel_function.dummy () ;
  fi_def = None;
  fi_project = dummy_t_project;
  fi_top = None;
  fi_level_option = DontSlice;
  fi_init_marks = None;
  fi_slices = [] ;
  fi_next_ff_num =0;
  f_called_by = [];
}

let dummy_t_marks_index = PdgIndex.FctIndex.create 0

let dummy_t_ff_marks = (PdgTypes.Pdg.top (Kernel_function.dummy ()),
                        dummy_t_marks_index)

let dummy_t_fct_slice = {
  ff_fct = dummy_t_fct_info ;
  ff_id = 0 ;
  ff_marks = dummy_t_ff_marks ;
  ff_called_by = []
}

let dummy_t_fct_user_crit = CuTop dummy_t_pdg_mark

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

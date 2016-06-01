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

(** All these functions find the nodes needed for various kind of annotations.
*
* @raise Kernel_function.No_Definition on annotations for function declarations.
*
* *)

(** [data_info] is composed of [(node,z_part) list, undef_loc)]
*             and correspond to data dependencies nodes.
*             Can be None if we don't know how to compute them.
*)
type data_info =
  ((PdgTypes.Node.t * Locations.Zone.t option) list
  * Locations.Zone.t option) option

(** [ctrl_info] correspond to control dependancies nodes *)
type ctrl_info = PdgTypes.Node.t list

(** [decl_info] correspond to the declarations nodes of the variables needed to
* parse the annotation *)
type decl_info =  PdgTypes.Node.t list

(** @raise Not_found when the statement is unreachable. *)
val find_code_annot_nodes :
  PdgTypes.Pdg.t -> Cil_types.stmt -> Cil_types.code_annotation ->
  ctrl_info * decl_info * data_info

val find_fun_precond_nodes : PdgTypes.Pdg.t -> Cil_types.predicate ->
  decl_info * data_info

val find_fun_postcond_nodes : PdgTypes.Pdg.t -> Cil_types.predicate ->
  decl_info * data_info

val find_fun_variant_nodes : PdgTypes.Pdg.t -> Cil_types.term ->
  decl_info * data_info

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

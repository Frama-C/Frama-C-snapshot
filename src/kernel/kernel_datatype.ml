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

open Db_types
open Cil_datatype

module Rooted_code_annotation =
  Datatype.Make
    (struct
       include Datatype.Serializable_undefined
       type t = Db_types.rooted_code_annotation
       let name = "rooted_code_annotation"
       let reprs =
	 List.map (fun c -> User c) Code_annotation.reprs
       let compare x y = match x, y with
	 | User a, User b
	 | AI(_, a), AI(_, b) -> Code_annotation.compare a b
	 | User _, AI _ -> -1
	 | AI _, User _ -> 1
       let equal = Datatype.from_compare
       let mem_project = Datatype.never_any_project
     end)

module Before_after_poly =
  Datatype.Polymorphic
    (struct
      type 'a t = 'a before_after
      let name ty = Type.name ty ^ " before_after"
      let module_name = "kernel_datatype.Before_after"
      let structural_descr _ = Structural_descr.Abstract
      let reprs r = [ Before r; After r ]
      let mk_compare f x y = match x, y with
	| Before a, Before b -> f a b
	| After a, After b -> f a b
	| Before _, After _ -> -1
	| After _, Before _ -> 1
      let mk_equal = Datatype.undefined
      let mk_hash = Datatype.undefined
      let map f =
	if f == Datatype.identity then Datatype.identity
	else function Before a -> Before (f a) | After a -> After (f a)
      let mk_internal_pretty_code = Datatype.undefined
      let mk_pretty = Datatype.undefined
      let mk_mem_project _ = Datatype.never_any_project
      let mk_varname = Datatype.undefined
     end)

module Before_after = Before_after_poly.Make
module Rooted_code_annotation_before_after =
  Before_after(Rooted_code_annotation)

module Kernel_function = struct
  let id kf = Ast_info.Function.get_id kf.fundec
  include Datatype.Make_with_collections
    (struct
      type t = kernel_function
      let name = "Kernel_function"
      let structural_descr = Structural_descr.Abstract
      let reprs =
	[ { fundec =
	    Definition
	      (Cil.emptyFunction "@dummy@", Location.unknown);
	    return_stmt = None;
	    spec = Cil.empty_funspec ();
	    stmts_graph = None } ]
      let compare k1 k2 = Datatype.Int.compare (id k1) (id k2)
      let equal = (==)
      let hash = id
      let copy = Datatype.undefined
      let rehash x = match x.fundec with
        | Definition _ | Declaration (_, _, None, _)-> x
        | Declaration (_, v, Some args, _) ->
          Cil.unsafeSetFormalsDecl v args;
	  x
      let get_name_kf kf = (Ast_info.Function.get_vi kf.fundec).Cil_types.vname
      let internal_pretty_code p_caller fmt kf =
	Type.par p_caller Type.Call fmt
	  (fun fmt ->
	    Format.fprintf fmt "@[<hv 2>Globals.Functions.find_by_name@;%S@]"
	      (get_name_kf kf))
      let pretty fmt kf =
	Ast_info.pretty_vname fmt (Ast_info.Function.get_vi kf.fundec)
      let mem_project = Datatype.never_any_project
      let varname kf = "kf_" ^ (get_name_kf kf)
     end)

end

(* ------------------------------------------------------------------------- *)
(* localisation *)
(* ------------------------------------------------------------------------- *)

module Localisation =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = localisation
      let name = "kernel_datatype.Localisation"
      let reprs = [ VGlobal ]
      let internal_pretty_code p_caller fmt loc =
	let pp s kf =
	  Type.par p_caller Type.Call fmt
	    (fun fmt ->
	      Format.fprintf fmt "@[<hv 2>%s@;%a@]"
		s
		(Kernel_function.internal_pretty_code Type.Call)
		kf)
	in
	match loc with
	| VGlobal -> Format.fprintf fmt "Db_types.VGlobal"
	| VLocal kf -> pp "Db_types.VLocal" kf
	| VFormal kf -> pp "Db_types.VFormal" kf
      let mem_project = Datatype.never_any_project
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

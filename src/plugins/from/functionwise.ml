(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

open Locations


module Tbl =
  Kernel_function.Make_Table
    (Function_Froms)
    (struct
       let name = "Functionwise dependencies"
       let size = 17
       let dependencies = [ Db.Value.self ]
     end)
let () = From_parameters.ForceDeps.set_output_dependencies [Tbl.self]

(* Forward reference to a function computing the from for a given function *)
let force_compute = ref (fun _ -> assert false)

module To_Use = struct
  let get_value_state = Db.Value.get_stmt_state

  let memo kf =
    Tbl.memo
      (fun kf ->
         !force_compute kf;
         try Tbl.find kf
         with Not_found -> invalid_arg "could not compute dependencies")
      kf

  let get_from_call kf _ = memo kf

  let keep_base kf = (* Eta-expansion required *)
    Callgraph.Uses.accept_base ~with_formals:false ~with_locals:false kf

  let cleanup kf froms =
    if Function_Froms.Memory.is_bottom froms.Function_Froms.deps_table
    then froms
    else
    let f b intervs =
      if Callgraph.Uses.accept_base ~with_formals:true ~with_locals:false kf b
      then Zone.inject b intervs
      else Zone.bottom
    in
    let joiner = Zone.join in
    let projection _ = Int_Intervals.top in
    let zone_substitution =
      Zone.cached_fold ~cache_name:"from cleanup" ~temporary:true
        ~f ~joiner ~empty:Zone.bottom ~projection
    in
    let zone_substitution x =
      try
        zone_substitution x
      with Zone.Error_Top -> Zone.top
    in
    let map_zone = Function_Froms.Deps.map zone_substitution in
    let subst = Function_Froms.DepsOrUnassigned.subst map_zone  in
    let open Function_Froms in
    { deps_table = Memory.map subst froms.deps_table;
      deps_return = Deps.map zone_substitution froms.deps_return;
    }

  let cleanup_and_save kf froms =
    let froms = cleanup kf froms in
    Tbl.add kf froms;
    froms
end

module From = From_compute.Make(To_Use)

let () =
  force_compute := From.compute


let force_compute_all () =
  !Db.Value.compute ();
  Callgraph.Uses.iter_in_rev_order
    (fun kf ->
       if Kernel_function.is_definition kf && !Db.Value.is_called kf
       then !Db.From.compute kf)

(* Db Registration for function-wise from *)
let () =
  Db.From.self := Tbl.self;
  Db.From.is_computed := Tbl.mem;
  Db.From.compute :=
    (fun kf -> ignore (To_Use.memo kf));
  Db.From.get := To_Use.memo;
  Db.From.pretty :=
    (fun fmt v ->
      let deps = To_Use.memo v in
      Function_Froms.pretty_with_type (Kernel_function.get_type v) fmt deps);
  Db.From.find_deps_no_transitivity :=
    (fun stmt lv ->
       let state = Db.Value.get_stmt_state stmt in
       let deps = From_compute.find_deps_no_transitivity state lv in
       Function_Froms.Deps.to_zone deps);
  Db.From.find_deps_no_transitivity_state :=
    (fun s e ->
      let deps = From_compute.find_deps_no_transitivity s e in
      Function_Froms.Deps.to_zone deps);

  ignore (
    Db.register_compute "From.compute_all"
      [Tbl.self]
      Db.From.compute_all
      force_compute_all);


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

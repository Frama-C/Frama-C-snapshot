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

open Cil_types

module Initial_datatype = Datatype

include
  State_builder.Option_ref
    (Cil_datatype.File)
    (struct
       let name = "AST"
       
       (* Kernel.UnrollingLevel.self is not a real dependency: the AST will
          get recomputed whenever this parameter changes. See unroll.ml *)
       let dependencies =
         [ Cil.selfMachine;
           Kernel.SimplifyCfg.self;
           Kernel.KeepSwitch.self;
           Kernel.Constfold.self;
           Kernel.ReadAnnot.self;
           Kernel.PreprocessAnnot.self;
           Kernel.Files.self;
           Cil.selfFormalsDecl ]
     end)

let mark_as_computed () = mark_as_computed () (* eta-expansion required *)

let linked_states = 
  ref
    [ Logic_env.Logic_info.self;
      Logic_env.Logic_type_info.self;
      Logic_env.Logic_ctor_info.self;
      Logic_env.Model_info.self;
      Logic_env.Lemmas.self;
      Cil.selfFormalsDecl ]

let add_linked_state state = linked_states := state :: !linked_states

let monotonic_states = ref []
let add_monotonic_state state = monotonic_states := state :: !monotonic_states

module After_building = Hook.Build(struct type t = Cil_types.file end)
let apply_after_computed = After_building.extend
let () = Parameter_customize.set_ast_hook apply_after_computed
let () = List.iter apply_after_computed !Parameter_customize.init_ast_hooks

let mark_as_changed () =
  let depends = State_selection.only_dependencies self in
  let no_remove = State_selection.list_state_union !linked_states in
  let selection = State_selection.diff depends no_remove in
  Project.clear ~selection ();
  After_building.apply (get())

let mark_as_grown () =
  let depends = State_selection.only_dependencies self in
  let no_remove = State_selection.list_state_union !linked_states in
  let no_remove =
    State_selection.union no_remove
      (State_selection.list_state_union !monotonic_states)
  in
  let selection = State_selection.diff depends no_remove in
  Project.clear ~selection ()

let () =
  State_dependency_graph.add_dependencies
    ~from:self [ Cil_datatype.Stmt.Hptset.self;
                 Cil_datatype.Varinfo.Hptset.self ];
  add_monotonic_state Cil_datatype.Stmt.Hptset.self;
  add_monotonic_state Cil_datatype.Varinfo.Hptset.self;
  Cil.register_ast_dependencies self;
  Logic_env.init_dependencies self;

exception Bad_Initialization of string
exception NoUntypedAst

let default_initialization =
  ref (fun () -> raise (Bad_Initialization "Cil file not initialized"))

let set_default_initialization f = default_initialization := f

let syntactic_constant_folding ast =
  Cil.visitCilFileSameGlobals (Cil.constFoldVisitor true) ast

module Computing =
  State_builder.False_ref(
    struct let name = "Ast.computing" let dependencies = [] end)

let force_compute () =
  if Computing.get () then
    Kernel.fatal "attempting to get the AST during its initialization";
  Computing.set true;
  Kernel.feedback ~level:2 "computing the AST";
  !default_initialization ();
  Computing.set false;
  let s = get () in
  (* Syntactic constant folding before analysing files if required *)
  if Kernel.Constfold.get () then syntactic_constant_folding s;
  After_building.apply s;
  s

let get () = memo (fun () -> force_compute ())

let is_computed () = is_computed ()

let compute () = if not (is_computed ()) then ignore (force_compute ())

let set_file file =
  let change old_file =
    if old_file == file then old_file
    else raise (Bad_Initialization "Too many AST initializations")
  in
  ignore 
    (memo ~change
       (fun () -> mark_as_computed (); After_building.apply file; file))

module UntypedFiles = struct

  let compute_untyped () =
    if not (is_computed()) then ignore (force_compute())
    else raise NoUntypedAst

  include State_builder.Option_ref
    (Initial_datatype.List(Cil_datatype.Cabs_file))
    (struct
       let name = "Untyped AST"
       let dependencies = (* the others delayed until file.ml *)
         [ Cil.selfMachine;
           self (* can't be computed without the AST *) ]
     end)

  let get () = memo (fun () -> compute_untyped (); get ())

end

module LastDecl =
  State_builder.Hashtbl
    (Cil_datatype.Varinfo.Hashtbl)
    (Cil_datatype.Global)
    (struct
      let name = "Ast.LastDecl"
      let dependencies = [ self ]
      let size = 47
     end)

let compute_last_decl () =
  (* Only meaningful when we have definitely computed the AST. *)
  if is_computed () && not (LastDecl.is_computed ()) then begin
    let globs = (get ()).globals in
    let update_one_global g =
      match g with
        | GVarDecl(_,v,_) when Cil.isFunctionType v.vtype ->
          LastDecl.replace v g
        | GFun (f,_) -> LastDecl.replace f.svar g
        | _ -> ()
    in
    List.iter update_one_global globs;
    LastDecl.mark_as_computed ()
  end

let is_last_decl g =
  (* Not_found mainly means that the information is irrelevant at this stage,
     not that there is a dangling varinfo.
   *)
  let is_eq v =
    compute_last_decl ();
    try (LastDecl.find v == g) with Not_found -> false
  in
  match g with
    | GVarDecl(_,v,_) -> is_eq v
    | GFun(f,_) -> is_eq f.svar
    | _ -> false

let clear_last_decl () =
  let selection = State_selection.Static.with_dependencies LastDecl.self in
  Project.clear ~selection ()

let add_hook_on_update f =
  add_hook_on_update (fun _ -> f ())
let () = add_hook_on_update Cil_datatype.clear_caches

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

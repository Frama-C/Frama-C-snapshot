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

(* ************************************************************************** *)
(** {2 Project skeleton} *)
(* ************************************************************************** *)

open Project_skeleton
open Output

(* re-exporting record fields *)
type project = t = private
    { pid : int;
      mutable name : string;
      mutable unique_name : string }

let rehash_ref = ref (fun _ -> assert false)

module D =
  Datatype.Make
    (struct
      type t = project
      let name = "Project"
      let structural_descr =
        Structural_descr.t_record
          [| Structural_descr.p_int;
             Structural_descr.p_string;
             Structural_descr.p_string |]
      let reprs = [ dummy ]
      let equal = (==)
      let compare p1 p2 = Datatype.Int.compare p1.pid p2.pid
      let hash p = p.pid
      let rehash x = !rehash_ref x
      let copy = Datatype.undefined
      let internal_pretty_code p_caller fmt p =
        let pp f =
          Format.fprintf
            f "@[<hv 2>Project.from_unique_name@;%S@]" p.unique_name
        in
        Type.par p_caller Type.Call fmt pp
      let pretty fmt p = Format.fprintf fmt "project %S" p.unique_name
      let varname p = "p_" ^ p.name
      let mem_project f x = f x
     end)
include D

module Project_tbl = Hashtbl.Make(D)

(* ************************************************************************** *)
(** {2 States operations} *)
(* ************************************************************************** *)

let current_selection = ref State_selection.empty
let get_current_selection () = !current_selection

module States_operations = struct

  module H = Hashtbl
  open State
  module Hashtbl = H

  let iter f x =
    current_selection := State_selection.full;
    State_dependency_graph.G.iter_vertex
      (fun s -> f s x)
      State_dependency_graph.graph

  let iter_on_selection ?(selection=State_selection.full) f x =
    current_selection := selection;
    State_selection.iter (fun s -> f s x) selection

  let fold_on_selection ?(selection=State_selection.full) f x =
    current_selection := selection;
    State_selection.fold (fun s -> f s x) selection

  let create = iter (fun s -> (private_ops s).create)
  let remove = iter (fun s -> (private_ops s).remove)
  let clean = iter (fun s -> (private_ops s).clean)

  let commit ?selection =
    iter_on_selection ?selection (fun s -> (private_ops s).commit)

  let update ?selection =
    iter_on_selection ?selection (fun s -> (private_ops s).update)

  let clear ?(selection=State_selection.full) p =
    let clear s = (private_ops s).clear in
    if State_selection.is_full selection then
      iter clear p (* clearing the static states also clears the dynamic ones *)
    else begin
      current_selection := selection;
      State_selection.iter (fun s -> clear s p) selection
    end

  let clear_some_projects ?selection f p =
    let states_to_clear =
      fold_on_selection
        ?selection
        (fun s p acc ->
           let is_cleared = (private_ops s).clear_some_projects f p in
           if is_cleared then
             State_selection.union
               (State_selection.with_dependencies s)
               acc
           else
             acc)
        p
        State_selection.empty
    in
    if not (State_selection.is_empty states_to_clear) then begin
      warning "clearing dangling project pointers in project %S" p.unique_name;
      debug ~dkey ~once:true ~append:(fun fmt -> Format.fprintf fmt "@]")
        "@[the involved states are:%t"
        (fun fmt ->
           iter_on_selection
             ~selection:states_to_clear
             (fun s () -> Format.fprintf fmt "@ %S" (get_name s))
             ())
    end

  let copy ?selection src =
    iter_on_selection ?selection (fun s -> (private_ops s).copy src)

  let serialize ?selection p =
    fold_on_selection
      ?selection
      (fun s p acc -> (get_unique_name s, (private_ops s).serialize p) :: acc)
      p
      []

  let unserialize ?selection dst loaded_states =
    let pp_err fmt n msg_sing msg_plural =
      if n > 0 then begin
        warning ~once:true
          fmt
	  n
          (if n = 1 then "" else "s")
          (if n = 1 then msg_sing else msg_plural)
      end
    in
    let tbl = Hashtbl.create 97 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) loaded_states;
    let invalid_on_disk = State.Hashtbl.create 7 in
    iter_on_selection
      ?selection
      (fun s () ->
         try
           let n = get_unique_name s in
           let d = Hashtbl.find tbl n in
           (try
	      (private_ops s).unserialize dst d;
	      (* do not remove if [State.Incompatible_datatype] occurs *)
              Hashtbl.remove tbl n
            with 
	    | Not_found -> assert false
	    | State.Incompatible_datatype _ ->
	      (* datatype of [s] on disk is incompatible with the one in RAM: as
		 [dst] is a new project, [s] is already equal to its default
		 value. However must clear the dependencies for consistency, but
		 it is doable only when all states are loaded. *)
	      State.Hashtbl.add invalid_on_disk s ())
         with Not_found ->
           (* [s] is in RAM but not on disk: silently ignore it!
	      Furthermore, all the dependencies of [s] are consistent 
              with this default value. So no need to clear them. Whenever
              the value of [s] in [dst] changes, the dependencies will be
              cleared (if required by the user). *)
           ())
      ();
    (* warns for the saved states that cannot be loaded
       (either they are not in RAM or they are incompatible). *)
    let nb_ignored =
      Hashtbl.fold (fun _ s n -> if s.on_disk_saved then succ n else n) tbl 0
    in
    pp_err
      "%d state%s in saved file ignored. \
%s this Frama-C configuration."
      nb_ignored
      "It is invalid in"
      "They are invalid in";
    if debug_atleast 1 then
      Hashtbl.iter
        (fun k s -> if s.on_disk_saved then debug ~dkey "ignoring state %s" k)
        tbl;
    (* after loading, reset dependencies of incompatible states *)
    let to_be_cleared =
      State.Hashtbl.fold
	(fun s () -> 
	  State_selection.union
	    (State_selection.only_dependencies s))
	invalid_on_disk
	State_selection.empty
    in
    let nb_cleared = State_selection.cardinal to_be_cleared in
    if nb_cleared > 0 then begin
      pp_err "%d state%s in memory reset to their default value. \
%s this Frama_C configuration."
	nb_cleared
	"It is inconsistent in"
	"They are inconsistent in";
      clear ~selection:to_be_cleared dst
    end

end

let guarded_feedback selection level fmt_msg =
  if verbose_atleast level then
    if State_selection.is_full selection then
      feedback ~dkey ~level fmt_msg
    else
      let n = State_selection.cardinal selection in
      if n = 0 then
        Log.nullprintf fmt_msg
      else
        let states fmt =
          if n > 1 then Format.fprintf fmt " (for %d states)" n
          else Format.fprintf fmt " (for 1 state)"
	in
        feedback ~dkey ~level ~append:states fmt_msg;
  else
    Log.nullprintf fmt_msg

let dft_sel () = State_selection.full

module Q = Qstack.Make(struct type t = project let equal = equal end)

let projects = Q.create ()
  (* The stack of projects. *)

let current () = Q.top projects
let is_current p = equal p (current ())

let iter_on_projects f = Q.iter f projects
let fold_on_projects f acc = Q.fold f acc projects

let find_all name = Q.filter (fun p -> p.name = name) projects
let from_unique_name uname = Q.find (fun p -> p.unique_name = uname) projects
module Mem = struct
  let mem s = try ignore (from_unique_name s); true with Not_found -> false
end
module Setter = Make_setter(Mem)

let unjournalized_set_name p s =
  feedback ~dkey ~level:2 "renaming project %S to %S" p.unique_name s;
  Setter.set_name p s

let set_name =
  Journal.register
    "Project.set_name"
    (Datatype.func2 ty Datatype.string Datatype.unit)
    unjournalized_set_name

module Create_Hook = Hook.Build(struct type t = project end)
let register_create_hook = Create_Hook.extend

let force_create name =
  feedback ~dkey ~level:2 "creating project %S" name;
  let p = Setter.make name in
  feedback ~dkey ~level:3 "its unique name is %S" p.unique_name;
  Q.add_at_end p projects;
  States_operations.create p;
  Create_Hook.apply p;
  p

let journalized_create =
  Journal.register
    "Project.create" (Datatype.func Datatype.string ty) force_create

(* do not journalise the first call to [create] *)
let create =
  let first = ref true in
  fun name ->
    let p = if !first then force_create name else journalized_create name in
    first := false;
    p

let get_name p = p.name
let get_unique_name p = p.unique_name

exception NoProject = Q.Empty

module Set_Current_Hook_User = Hook.Build (struct type t = project end)
module Set_Current_Hook = Hook.Build(struct type t = project end)

let register_after_set_current_hook ~user_only =
  if user_only then Set_Current_Hook_User.extend else Set_Current_Hook.extend

let unjournalized_set_current =
  let apply_hook = ref false in
  fun on selection p ->
    if not (Q.mem p projects) then
      invalid_arg ("Project.set_current: " ^ p.unique_name ^ " does not exist");
    let old = current () in
    States_operations.commit ~selection old;
    (try Q.move_at_top p projects with Invalid_argument _ -> assert false);
    let level = if on then 3 else 2 in
    guarded_feedback selection level
      "%S is now the current project"
      p.unique_name;
    assert (equal p (current ()));
    States_operations.update ~selection p;
    (* do not apply hook if an hook calls [set_current] *)
    if not !apply_hook then begin
      apply_hook := true;
      if not on then Set_Current_Hook_User.apply old;
      Set_Current_Hook.apply old;
      apply_hook := false
    end

let journalized_set_current =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.set_current"
    (lbl "on" (fun () -> false) Datatype.bool
       (lbl "selection" dft_sel State_selection.ty
          (Datatype.func ty Datatype.unit)))
    unjournalized_set_current

let set_current ?(on=false) ?(selection=State_selection.full) p =
  if not (equal p (current ())) then journalized_set_current on selection p

let on ?selection p f x =
  let old_current = current () in
  let set p = set_current ~on:true ?selection p in
  let go () =
    set p;
    let r = f x in
    set old_current;
    r
  in
  if debug_atleast 1 then go ()
  else begin try go () with e -> set old_current; raise e end

(* [set_current] must never be called internally. *)
module Hide_set_current = struct let set_current () = assert false end
open Hide_set_current
(* Silence warning on unused and unexported functions *)
let () = if false then set_current ()

exception Cannot_remove of string

module Before_remove = Hook.Build(struct type t = project end)
let register_before_remove_hook = Before_remove.extend

let unjournalized_remove project =
  feedback ~dkey ~level:2 "removing project %S" project.unique_name;
  if Q.length projects = 1 then raise (Cannot_remove project.unique_name);
  Before_remove.apply project;
  States_operations.remove project;
  let old_current = current () in
  Q.remove project projects;
  if equal project old_current then begin
    (* we removed the current project. So there is a new current project
       and we have to update the local states according to it. *)
    let c = current () in
    States_operations.update c;
    Set_Current_Hook_User.apply c
  end;
  (* clear all the states of other projects referring to the delete project *)
  Q.iter (States_operations.clear_some_projects (equal project)) projects
(*  Gc.major ()*)

let journalized_remove =
  Journal.register "Project.remove"
    (Datatype.optlabel_func
       "project" current ty (Datatype.func Datatype.unit Datatype.unit))
    (fun project () -> unjournalized_remove project)

let remove ?(project=current()) () = journalized_remove project ()

let remove_all () =
  feedback ~dkey ~level:2 "removing all existing projects";
  try
    iter_on_projects Before_remove.apply;
    States_operations.clean ();
    Q.clear projects;
    Gc.full_major ()
  with NoProject ->
    ()

let journalized_copy =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.copy"
    (lbl "selection" dft_sel State_selection.ty
       (lbl "src" current ty (Datatype.func ty Datatype.unit)))
    (fun selection src dst ->
       guarded_feedback selection 2 "copying project from %S to %S"
         src.unique_name dst.unique_name;
       States_operations.commit ~selection src;
       States_operations.copy ~selection src dst)

let copy ?(selection=State_selection.full) ?(src=current()) dst =
  journalized_copy selection src dst

module Before_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_before_clear = Before_Clear_Hook.extend

module After_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_after_clear = After_Clear_Hook.extend

let journalized_clear =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.clear"
    (lbl "selection" dft_sel State_selection.ty
       (lbl "project" current ty (Datatype.func Datatype.unit Datatype.unit)))
    (fun selection project () ->
      guarded_feedback selection 2 "clearing project %S" project.unique_name;
      Before_Clear_Hook.apply project;
      States_operations.clear ~selection project;
      After_Clear_Hook.apply project;
       (*Gc.major ()*))

let clear ?(selection=State_selection.full) ?(project=current()) () =
  journalized_clear selection project ()

let unjournalized_clear_all () =
  Q.iter States_operations.clear projects;
  Gc.full_major ()

let clear_all =
  Journal.register
    "Project.clear_all"
    (Datatype.func Datatype.unit Datatype.unit)
    unjournalized_clear_all

(* ************************************************************************** *)
(* Save/load *)
(* ************************************************************************** *)

exception IOError = Sys_error

module Before_load = Hook.Make(struct end)
let register_before_load_hook = Before_load.extend

module After_load = Hook.Make(struct end)
let register_after_load_hook = After_load.extend

module After_global_load = Hook.Make(struct end)
let register_after_global_load_hook = After_global_load.extend

let magic = 9 (* magic number *)

let save_projects selection projects filename =
  if Cmdline.use_obj then begin
    let cout = open_out_bin filename in
    output_value cout Config.version;
    output_value cout magic;
    output_value cout !Graph.Blocks.cpt_vertex;
    let states : (t * (string * State.state_on_disk) list) list =
      Q.fold
        (fun acc p ->
           (* project + serialized version of all its states *)
           (p, States_operations.serialize ~selection p) :: acc)
        []
        projects
    in
    (* projects are stored on disk from the current one to the last project *)
    output_value cout (List.rev states);
    close_out cout;
  end else
    abort "saving a file is not supported in the 'no obj' mode"

let unjournalized_save selection project filename =
  guarded_feedback selection 2 "saving project %S into file %S"
    project.unique_name filename;
  save_projects selection (Q.singleton project) filename

let journalized_save =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.save"
    (lbl "selection" dft_sel State_selection.ty
       (lbl "project" current ty (Datatype.func Datatype.string Datatype.unit)))
    unjournalized_save

let save ?(selection=State_selection.full) ?(project=current()) filename =
  journalized_save selection project filename

let unjournalized_save_all selection filename =
  guarded_feedback selection 2 "saving the current session into file %S"
    filename;
  save_projects selection projects filename

let journalized_save_all =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.save_all"
    (lbl "selection" dft_sel State_selection.ty
       (Datatype.func Datatype.string Datatype.unit))
    unjournalized_save_all

let save_all ?(selection=State_selection.full) filename =
  journalized_save_all selection filename

module Descr = struct

  let project_under_copy_ref: project option ref = ref None
    (* The project which is currently copying. Only set by [create_by_copy].
       In this case, there is no possible dangling project pointers (projects
       at saving time and at loading time are the same).
       Furthermore, we have to merge pre-existing projects and loaded
       projects, except the project under copy. *)

  module Rehash =
    Hashtbl.Make
      (struct
         type t = project
         let hash p = Hashtbl.hash p.pid
         let equal x y =
           match !project_under_copy_ref with
           | Some p when p.pid <> x.pid && p.pid <> y.pid ->
               (* Merge projects on disk with pre-existing projects, except the
                  project under copy; so don't use (==) in this context. *)
               x.pid = y.pid
           | None | Some _ ->
               (* In all other cases, don't merge.
                  (==) ensures that there is no sharing between a pre-existing
                  project and a project on disk. Great! *)
               x == y
       end)

  let rehash_cache : project Rehash.t = Rehash.create 7
  let existing_projects : unit Project_tbl.t = Project_tbl.create 7

  let rehash p =
(*    Format.printf "REHASHING %S (%d;%x)@." p.unique_name p.pid (Extlib.address_of_value p);*)
    try
      Rehash.find rehash_cache p
    with Not_found ->
      let v = create p.name (* real name set when loading the key project *) in
      Rehash.add rehash_cache p v;
      v
  let () = rehash_ref := rehash

  let init project_under_copy =
    assert (Rehash.length rehash_cache = 0
           && Project_tbl.length existing_projects = 0);
    project_under_copy_ref := project_under_copy;
    Q.fold
      (fun acc p -> Project_tbl.add existing_projects p (); p :: acc)
      []
      projects

  let finalize loaded_states selection =
    (match !project_under_copy_ref with
    | None ->
      List.iter
        (fun ( (p, _)) ->
          States_operations.clear_some_projects
            ~selection
            (fun p -> not (Project_tbl.mem existing_projects p))
            p)
        loaded_states
    | Some _ ->
      ());
    Rehash.clear rehash_cache;
    Project_tbl.clear existing_projects

  let global_state name selection =
    let state_on_disk s =
(*      Format.printf "State %S@." s;*)
      let descr =
        try State.get_descr (State.get s)
        with State.Unknown -> Structural_descr.p_unit (* dummy value *)
      in
      Descr.t_record
        [| descr;
           Structural_descr.p_bool;
           Structural_descr.p_bool;
           Structural_descr.p_string |]
        State.dummy_state_on_disk
    in
    let tbl_on_disk = Descr.dependent_pair Descr.t_string state_on_disk in
    let one_state =
      let unmarshal_states p =
        Descr.dynamic
          (fun () ->
            (* Local states must be up-to-date according to [p] when
	       unmarshalling states of [p] *)
            unjournalized_set_current true selection p;
            Before_load.apply ();
            Descr.t_list tbl_on_disk)
      in
      Descr.dependent_pair descr unmarshal_states
    in
    let final_one_state =
      Descr.transform
        one_state
        (fun (p, s as c) ->
           (match name with None -> () | Some s -> set_name p s);
           Project_tbl.add existing_projects p ();
           (* At this point, the local states are always up-to-date according
              to the current project, since we load first the old current
              project *)
           States_operations.unserialize ~selection p s;
           After_load.apply ();
           c)
    in
    Descr.t_list final_one_state

  let input_val = Descr.input_val

end

let load_projects ~project_under_copy selection ?name filename =
  if Cmdline.use_obj then begin
    let cin = open_in_bin filename in
    let gen_read f cin =
      try f cin with Failure s -> close_in cin; raise (IOError s)
    in
    let read = gen_read input_value in
    let check_magic cin to_string current =
      let old = read cin in
      if old <> current then begin
        close_in cin;
        let s =
          Format.sprintf
            "project saved with an incompatible version (old: %S,current: %S)"
            (to_string old)
            (to_string current)
        in
        raise (IOError s)
      end
    in
    check_magic cin (fun x -> x) Config.version;
    check_magic cin (fun n -> "magic number " ^ string_of_int n) magic;
    let ocamlgraph_counter = read cin in
    let pre_existing_projects = Descr.init project_under_copy in
    let loaded_states =
      gen_read
        (fun c -> Descr.input_val c (Descr.global_state name selection))
        cin
    in
    close_in cin;
    Descr.finalize loaded_states selection;
    Graph.Blocks.after_unserialization ocamlgraph_counter;
    (* [set_current] done when unmarshalling and hooks may reorder
       projects: rebuild it in the good order *)
    let last = current () in
    Q.clear projects;
    let loaded_projects =
      List.fold_right
        (fun (p, _) acc -> Q.add p projects; p :: acc) loaded_states []
    in
    List.iter (fun p -> Q.add p projects) pre_existing_projects;
    (* We have to restore all the local states if the last loaded project is
       not the good current one. The trick is to call [set_current] on [current
       ()], but we ensure that this operation **does** something (that is not
       the case by default) by putting [last] as current project
       temporarily. *)
    let true_current = current () in
    Q.add last projects;
    unjournalized_set_current true selection true_current;
    Q.remove last projects;
    After_global_load.apply ();
    loaded_projects
  end else
    abort "loading a file is not supported in the 'no obj' mode"

let unjournalized_load ~project_under_copy selection name filename =
  guarded_feedback selection 2 "loading the project saved in file %S"
    filename;
  match load_projects ~project_under_copy selection ?name filename with
  | [ p ] -> p
  | [] | _ :: _ :: _ -> assert false

let journalized_load =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.load"
    (lbl "selection" dft_sel State_selection.ty
       (lbl "name" (fun () -> None)
          (Datatype.option Datatype.string) (Datatype.func Datatype.string ty)))
    (unjournalized_load ~project_under_copy:None)

let load ?(selection=State_selection.full) ?name filename =
  journalized_load selection name filename

let unjournalized_load_all selection filename =
  remove_all ();
  guarded_feedback selection 2 "loading the session saved in file %S" filename;
  try
    ignore (load_projects ~project_under_copy:None selection filename)
  with IOError _ as e ->
    unjournalized_set_current false selection (create "default");
    raise e

let journalized_load_all =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.load_all"
    (lbl "selection" dft_sel State_selection.ty
       (Datatype.func Datatype.string Datatype.unit))
    unjournalized_load_all

let load_all ?(selection=State_selection.full) filename =
  journalized_load_all selection filename

module Create_by_copy_hook = Hook.Build(struct type t = project * project end)
let create_by_copy_hook f =
  Create_by_copy_hook.extend (fun (src, dst) -> f src dst)

let unjournalized_create_by_copy selection src name =
  guarded_feedback selection 2 "creating project %S by copying project %S"
    name (src.unique_name);
  let filename =
    try Extlib.temp_file_cleanup_at_exit "frama_c_create_by_copy" ".sav"
    with Extlib.Temp_file_error s -> abort "cannot create temporary file: %s" s
  in
  save ~selection ~project:src filename;
  try
    let prj =
      unjournalized_load
        ~project_under_copy:(Some src) selection (Some name) filename
    in
    Extlib.safe_remove filename;
    Create_by_copy_hook.apply (src, prj);
    prj
  with e ->
    Extlib.safe_remove filename;
    raise e

let journalized_create_by_copy =
  let lbl = Datatype.optlabel_func in
  Journal.register "Project.create_by_copy"
    (lbl "selection" dft_sel State_selection.ty
       (lbl "src" current ty (Datatype.func Datatype.string ty)))
    unjournalized_create_by_copy

let create_by_copy ?(selection=State_selection.full) ?(src=current()) name =
  journalized_create_by_copy selection src name

(* ************************************************************************** *)
(** {2 Undoing} *)
(* ************************************************************************** *)

module Undo = struct

  let short_filename = "frama_c_undo_restore"
  let filename = ref ""

  let clear_breakpoint () = Extlib.safe_remove !filename

  let restore () =
    if Cmdline.use_obj then begin
      try
        Journal.prevent load_all !filename;
        Journal.restore ();
        clear_breakpoint ()
      with IOError s ->
        feedback ~dkey "cannot restore the last breakpoint: %S" s;
        clear_breakpoint ()
    end

  let breakpoint () =
    if Cmdline.use_obj then begin
      clear_breakpoint ();
      filename :=
        (try Extlib.temp_file_cleanup_at_exit short_filename ".sav"
         with Extlib.Temp_file_error s ->
           abort "cannot create temporary file: %s" s);
      Journal.prevent save_all !filename;
      Journal.save ()
    end

end

(* Exporting Datatype for an easy external use *)
module Datatype = D

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

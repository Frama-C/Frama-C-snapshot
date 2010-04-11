(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
(** {2 Debugging} *)
(* ************************************************************************** *)

include Log.Register
  (struct
     let channel = Log.kernel_channel_name
     let label = Log.kernel_label_name
     let verbose_atleast n = !Cmdline.kernel_verbose_atleast_ref n
     let debug_atleast n = !Cmdline.kernel_debug_atleast_ref n
   end)

(* ************************************************************************** *)
(** {2 Project} *)
(* ************************************************************************** *)

type t = { pid: int; mutable name: string; mutable unique_name: string }
type project = t

let dummy = { pid = 0; name = ""; unique_name = ""}

let pp p_caller fmt p =
  let pp f =
    Format.fprintf f "@[<hv 2>Project.from_unique_name@;%S@]" p.unique_name
  in
  Type.par p_caller Type.Call fmt pp

let varname p = "p_" ^ p.name

let ty =
  Type.register
    ~name:"Project.t" ~value_name:(Some "Project.ty") ~pp ~varname:varname
    [ dummy ]

(* ************************************************************************* *)
(** Projects are comparable *)

let equal = (==)
let compare p1 p2 = Pervasives.compare p1.pid p2.pid
let hash p = p.pid

module Project_Tbl =
  Hashtbl.Make(struct type t = project let hash = hash let equal = equal end)
(* ************************************************************************* *)

type state_on_disk =
    { s_value: Obj.t;
      s_computed: bool;
      s_saved: bool;
      s_is_current: bool;
      s_digest: Digest.t }

let global_pid = ref 0

(* Operations on states visible by operations on projects *)
type computation_operations =
    { cname: string;
      create: t -> unit;
      remove: t -> unit;
      mutable clear: t -> unit;
      clear_some_projects: (t -> bool) -> t -> bool;
      copy: t -> t -> unit;
      commit: t -> unit;
      update: t -> unit;
      clean: unit -> unit;
      serialize: t -> state_on_disk;
      unserialize: t -> state_on_disk -> unit }

let dummy_name = "dummy"

(* Operations on computations. *)
module Computations = struct

  include Kind.Make
    (struct
       type t = computation_operations
       let name = "Computations"
       let kind_name x = x.cname
       let dummy =
	 let never_called _ = assert false in
	 { cname = dummy_name;
	   create = never_called;
	   remove = never_called;
	   clear = never_called;
	   clear_some_projects = never_called;
	   copy = never_called;
	   commit = never_called;
	   update = never_called;
	   serialize = never_called;
	   unserialize = never_called;
	   clean = never_called }
     end)

  let create_kind = create

  let create = iter (fun s -> s.create)
  let remove = iter (fun s -> s.remove)
  let clean = iter (fun s -> s.clean)

  let commit ?(only=Selection.empty) ?(except=Selection.empty) =
    iter_in_order only except (fun s -> s.commit)

  let update ?(only=Selection.empty) ?(except=Selection.empty) =
    iter_in_order only except (fun s -> s.update)

  let clear only except =
    full_iter_in_order only except (fun s -> s.clear)

  let clear_some_projects only except f p =
    let cleared =
      apply_in_order only except
	(fun s acc ->
	   let v = value s in
	   if v.clear_some_projects f p then begin
	     let sel = Selection.singleton s Kind.Select_Dependencies in
             clear sel Selection.empty p;
	     Selection.add s Kind.Select_Dependencies acc
	   end else
	     acc)
	Selection.empty
    in
    if not (Selection.is_empty cleared) then begin
      warning "clearing dangling project pointers in project %S" p.unique_name;
      debug ~once:true ~append:(fun fmt -> Format.fprintf fmt "@]")
	"@[the involved states are:%t"
	(fun fmt ->
	   iter_in_order cleared Selection.empty
	     (fun v () -> Format.fprintf fmt "@ %S" v.cname)
	     ())
    end

  let copy only except src =
    iter_in_order only except (fun s -> s.copy src)

  let serialize only except p =
    fold_in_order only except (fun s acc -> (s.cname, s.serialize p) :: acc) []

  let unserialize only except dst l =
    let pp_err fmt n =
      if n > 0 then begin
	warning ~once:true
	  fmt
	  n
	  (if n = 1 then "" else "s") (if n = 1 then "is" else "are")
	  (if n = 1 then "It does not exist in" else "They do not exist in")
      end
    in
    let tbl = Hashtbl.create 97 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) l;
    iter_in_order only except
      (fun s () ->
	 try
	   let d = Hashtbl.find tbl s.cname in
	   s.unserialize dst d;
	   Hashtbl.remove tbl s.cname;
	 with Not_found ->
	   (* [s] is in RAM but not on disk: silently ignore it!
	      As [dst] is a new project], [s] is already equal to its default
	      value. Furthermore, all the dependencies of [s] are consistent
	      with this default value. So no need to clear them. Whenever
	      the value of [s] in [dst] changes, the dependencies will be
	      cleared (if required by the user of Project.clear). *)
	   ())
      ();
    (* warns for the saved states that cannot be loaded. *)
    let nb_ignored =
      Hashtbl.fold (fun _ s n -> if s.s_saved then succ n else n) tbl 0
    in
    pp_err
      "%n state%s of the saved file %s ignored. \
%s your Frama-C configuration"
      nb_ignored;
    Hashtbl.iter
      (fun k s -> if s.s_saved then debug "ignoring state %s" k)
      tbl

end

module Selection = Computations.Selection

let guarded_feedback ?(full=false) only except level fmt_msg =
  if verbose_atleast level then
    let f =
      if full then Computations.full_number_of_applicants
      else Computations.number_of_applicants
    in
    match f only except with
    | None -> feedback ~level fmt_msg
    | Some n when n = 0 -> Log.nullprintf fmt_msg
    | Some n ->
	let states fmt =
	  if n > 1 then Format.fprintf fmt " (for %d states)" n
	  else Format.fprintf fmt " (for 1 state)"
	in
	feedback ~level ~append:states fmt_msg
  else
    Log.nullprintf fmt_msg

let dft_sel () = Selection.empty

module Q = Qstack.Make(struct type u = t type t = u let equal = equal end)

let projects = Q.create ()
  (* The stack of projects. *)

let current () = Q.top projects
let is_current p = equal p (current ())

let iter_on_projects f = Q.iter f projects
let fold_on_projects f acc = Q.fold f acc projects

let find_all name = Q.filter (fun p -> p.name = name) projects
let from_unique_name uname = Q.find (fun p -> p.unique_name = uname) projects

(* Build a name different of all the others project names and based on
   [from]. *)
let mk_unique from =
  let exists s =
    try ignore (from_unique_name s); true with Not_found -> false
  in
  let rec build base id =
    let fullname = base ^ " " ^ string_of_int id in
    if exists fullname then build base (succ id) else fullname
  in
  if exists from then build from 2 else from

let unjournalized_set_name p s =
  feedback ~level:2 "Renaming project %S to %S" p.unique_name s;
  p.unique_name <- mk_unique s;
  p.name <- s

let set_name =
  Journal.register
    "Project.set_name"
    (Type.func2 ty Type.string Type.unit)
    unjournalized_set_name

module Create_Hook = Hook.Build(struct type t = project end)
let register_create_hook = Create_Hook.extend

let force_create name =
  feedback ~level:2 "creating project %S" name;
  incr global_pid;
  let p = { pid = !global_pid; name = name; unique_name = mk_unique name } in
  Q.add_at_end p projects;
  Computations.create p;
  Create_Hook.apply p;
  p

let journalized_create =
  Journal.register "Project.create" (Type.func Type.string ty) force_create

(* do not journalise the first call to [create] *)
let create =
  let first = ref true in
  fun name ->
    let p = if !first then force_create name else journalized_create name in
    first := false;
    p

let name p = p.name
let unique_name p = p.unique_name

exception NoProject = Q.Empty

module Set_Current_Hook_User = Hook.Build (struct type t = project end)
module Set_Current_Hook = Hook.Build(struct type t = project end)

let register_after_set_current_hook ~user_only =
  if user_only then Set_Current_Hook_User.extend else Set_Current_Hook.extend

let unjournalized_set_current =
  let apply_hook = ref false in
  fun on only except p ->
    if not (Q.mem p projects) then
      invalid_arg ("Project.set_current: " ^ p.unique_name ^ " does not exist");
    let old = current () in
    Computations.commit ~only ~except old;
    (try Q.move_at_top p projects with Invalid_argument _ -> assert false);
    let level = if on then 3 else 2 in
    guarded_feedback only except level
      "%S is now the current project"
      p.unique_name;
    assert (equal p (current ()));
    Computations.update ~only ~except p;
    (* do not apply hook if an hook calls [set_current] *)
    if not !apply_hook then begin
      apply_hook := true;
      if not on then Set_Current_Hook_User.apply old;
      Set_Current_Hook.apply old;
      apply_hook := false
    end

let journalized_set_current =
  let lbl = Type.optlabel_func in
  Journal.register "Project.set_current"
    (lbl "on" (fun () -> false) Type.bool
       (lbl "only" dft_sel Selection.ty
	  (lbl "except" dft_sel Selection.ty (Type.func ty Type.unit))))
    unjournalized_set_current

let set_current
    ?(on=false) ?(only=Selection.empty) ?(except=Selection.empty) p =
  if not (equal p (current ())) then journalized_set_current on only except p

exception Cannot_remove of string

module Before_remove = Hook.Build(struct type t = project end)
let register_before_remove_hook = Before_remove.extend

let unjournalized_remove project =
  feedback ~level:2 "removing project %S" project.name;
  if Q.length projects = 1 then raise (Cannot_remove project.name);
  Before_remove.apply project;
  Computations.remove project;
  let old_current = current () in
  Q.remove project projects;
  if equal project old_current then begin
    (* we removed the current project. So there is a new current project
       and we have to update the local states according to it. *)
    let c = current () in
    Computations.update c;
    Set_Current_Hook_User.apply c
  end;
  (* clear all the states of other projects referring to the delete project *)
  Q.iter
    (Computations.clear_some_projects Selection.empty Selection.empty
       (equal project))
    projects;
  Gc.major ()

let journalized_remove =
  Journal.register "Project.remove"
    (Type.optlabel_func "project" current ty (Type.func Type.unit Type.unit))
    (fun project () -> unjournalized_remove project)

let remove ?(project=current()) () = journalized_remove project ()

let remove_all () =
  feedback ~level:2 "removing all existing projects";
  try
    iter_on_projects Before_remove.apply;
    Computations.clean ();
    Q.clear projects;
    Gc.full_major ()
  with NoProject ->
    ()

let journalized_copy =
  let lbl = Type.optlabel_func in
  Journal.register "Project.copy"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "src" current ty (Type.func ty Type.unit))))
    (fun only except src  dst ->
       guarded_feedback only except 2 "copying project from %S to %S"
	 src.name dst.name;
       Computations.commit ~only ~except src;
       Computations.copy only except src dst)

let copy
    ?(only=Selection.empty) ?(except=Selection.empty) ?(src=current()) dst =
  journalized_copy only except src dst

module Before_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_before_clear = Before_Clear_Hook.extend
let register_todo_on_clear =
  deprecated
    "Project.register_todo_on_clear" ~now:"Project.register_todo_before_clear"
    register_todo_before_clear

module After_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_after_clear = After_Clear_Hook.extend

let journalized_clear =
  let lbl = Type.optlabel_func in
  Journal.register "Project.clear"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "project" current ty (Type.func Type.unit Type.unit))))
    (fun only except project () ->
       guarded_feedback ~full:true only except 2 "clearing project %S"
	 project.name;
       Before_Clear_Hook.apply project;
       Computations.clear only except project;
       After_Clear_Hook.apply project;
       Gc.major ())

let clear
    ?(only=Selection.empty) ?(except=Selection.empty) ?(project=current()) () =
  journalized_clear only except project ()

let unjournalized_clear_all () =
  Q.iter
    (Computations.clear
       Selection.empty
       Selection.empty)
    projects;
  Gc.full_major ()

let clear_all =
  Journal.register "Project.clear_all" (Type.func Type.unit Type.unit)
    unjournalized_clear_all

let on ?only ?except p f x =
  let old_current = current () in
  let set p = set_current ~on:true ?only ?except p in
  try
    set p;
    let r = f x in
    set old_current;
    r
  with e ->
    set old_current;
    raise e

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

let save_projects only except projects filename =
  if Cmdline.use_obj then begin
    let cout = open_out_bin filename in
    output_value cout Config.version;
    output_value cout magic;
    output_value cout !Graph.Blocks.cpt_vertex;
    let states : (t * (string * state_on_disk) list) list =
      Q.fold
	(fun acc p ->
	   (* project + serialized version of all its states *)
	    (p, Computations.serialize only except p) :: acc)
	[]
	projects
    in
    (* projects are stored on disk from current one to the last project *)
    output_value cout (List.rev states);
    close_out cout
  end else
    abort "saving a file is not supported in the 'no obj' mode"

let unjournalized_save only except project filename =
  guarded_feedback only except 2 "saving project %S into file %S"
    project.name filename;
  save_projects only except (Q.singleton project) filename

let journalized_save =
  let lbl = Type.optlabel_func in
  Journal.register "Project.save"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "project" current ty (Type.func Type.string Type.unit))))
    unjournalized_save

let save
    ?(only=Selection.empty) ?(except=Selection.empty)
    ?(project=current())
    filename
    =
  journalized_save only except project filename

let unjournalized_save_all only except filename =
  guarded_feedback only except 2 "saving the current session into file %S"
    filename;
  save_projects only except projects filename

let journalized_save_all =
  let lbl = Type.optlabel_func in
  Journal.register "Project.save_all"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (Type.func Type.string Type.unit)))
    unjournalized_save_all

let save_all ?(only=Selection.empty) ?(except=Selection.empty) filename =
  journalized_save_all only except filename

module Descr = struct

  let states_tbl : (string, Unmarshal.t) Hashtbl.t = Hashtbl.create 97

  let project_under_copy_ref: project option ref = ref None
    (* The project which is currently copying. Only set by [create_by_copy].
       In this case, there is no possible dangling project pointers (projects
       at saving time and at loading time are the same).
       In this case, we have to merge pre-existing projects and loaded
       projects, except the project under copy. *)

  open Unmarshal

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
  let existing_projects : unit Project_Tbl.t = Project_Tbl.create 7

  let rehash p =
(*    Format.printf "REHASHING %S (%d;%x)@." p.unique_name p.pid (Extlib.address_of_value p);*)
    try
      Rehash.find rehash_cache p
    with Not_found ->
      let v = create p.name (* real name set when loading the key project *) in
      Rehash.add rehash_cache p v;
      v

  let init project_under_copy =
    assert (Rehash.length rehash_cache = 0
	&& Project_Tbl.length existing_projects = 0);
    project_under_copy_ref := project_under_copy;
    Q.fold
      (fun acc p -> Project_Tbl.add existing_projects p (); p :: acc)
      []
      projects

  let finalize loaded_states only except =
    (match !project_under_copy_ref with
     | None ->
	 List.iter
	   (fun ( (p, _)) ->
	      Computations.clear_some_projects
		only except
		(fun p -> not (Project_Tbl.mem existing_projects p))
		p)
	   loaded_states
     | Some _ ->
	 ());
    Rehash.clear rehash_cache;
    Project_Tbl.clear existing_projects

  let project =
    Transform
      (Structure (Sum [| [| Abstract; Abstract; Abstract |] |]),
       fun o ->
	 let p : project = Obj.obj o in
	 Obj.repr (rehash p))

  let global_state name only except =
    let state_on_disk s =
      let descr = try Hashtbl.find states_tbl s with Not_found -> Abstract in
      Structure (Sum [| [| descr; Abstract; Abstract; Abstract; Abstract |] |])
    in
    let tbl_on_disk =
      Structure
	(Dependent_pair
	   (Abstract,
	    (fun o -> let s : string = Obj.obj o in state_on_disk s)))
    in
    let one_state =
      Structure
	(Dependent_pair
	   (project,
	    (fun o ->
	       let p : project = Obj.obj o in
	       Dynamic
		 (fun () ->
(*		    Format.printf "before loading project %S@." p.unique_name;*)
		    on p Before_load.apply (); t_list tbl_on_disk))))
    in
    let final_one_state =
      Transform
	(one_state,
	 fun o ->
	   let (((p,s):(project * (string * state_on_disk) list))) = Obj.obj o in
	   (match name with None -> () | Some s -> set_name p s);
	   Project_Tbl.add existing_projects p ();
	   Computations.unserialize only except p s;
	   (* At this point, the local state are always up-to-date according to
	      the current project,
	      since we load first the old current project *)
	   on p After_load.apply ();
	   o)
    in
    t_list final_one_state

end

let load_projects ~project_under_copy only except ?name filename =
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
    let loaded_states : (t * (string * state_on_disk) list)
	list=
      gen_read
	(fun c -> Unmarshal.input_val c (Descr.global_state name only except))
	cin
    in
    close_in cin;
    Descr.finalize loaded_states only except;
    Graph.Blocks.after_unserialization ocamlgraph_counter;
    Computations.Dynamic.after_load ();
    After_global_load.apply ();
    (* [on] above and hooks may reorder projects: rebuild it in the good order
       at end *)
    Q.clear projects;
    let loaded_projects =
      List.fold_right
	(fun ( (p, _)) acc -> Q.add p projects; p :: acc) loaded_states []
    in
    List.iter (fun p -> Q.add p projects) pre_existing_projects;
    loaded_projects
  end else
    abort "loading a file is not supported in the 'no obj' mode"

let unjournalized_load ~project_under_copy only except name filename =
  guarded_feedback only except 2 "loading the project saved in file %S"
    filename;
  match load_projects ~project_under_copy only except ?name filename with
  | [ p ] -> p
  | [] | _ :: _ :: _ -> assert false

let journalized_load =
  let lbl = Type.optlabel_func in
  Journal.register "Project.load"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "name" (fun () -> None)
	     (Type.option Type.string) (Type.func Type.string ty))))
    (unjournalized_load ~project_under_copy:None)

let load ?(only=Selection.empty) ?(except=Selection.empty) ?name filename =
  journalized_load only except name filename

let unjournalized_load_all only except filename =
  remove_all ();
  guarded_feedback only except 2 "loading the session saved in file %S"
    filename;
  try
    ignore (load_projects ~project_under_copy:None only except filename);
    (* the current project changed: apply corresponding hooks *)
    let p = current () in
    Set_Current_Hook_User.apply p;
    Set_Current_Hook.apply p
  with IOError _ as e ->
    set_current (create "default");
    raise e

let journalized_load_all =
  let lbl = Type.optlabel_func in
  Journal.register "Project.load_all"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty (Type.func Type.string Type.unit)))
    unjournalized_load_all

let load_all ?(only=Selection.empty) ?(except=Selection.empty) filename =
  journalized_load_all only except filename

module Create_by_copy_hook = Hook.Build(struct type t = project * project end)
let create_by_copy_hook f =
  Create_by_copy_hook.extend (fun (src, dst) -> f src dst)

let unjournalized_create_by_copy only except src name =
  guarded_feedback only except 2 "creating project %S by copying project %S"
    name (src.unique_name);
  let filename = Filename.temp_file "frama_c_create_by_copy" ".sav" in
  save ~only ~except ~project:src filename;
  try
    let prj =
      unjournalized_load
	~project_under_copy:(Some src) only except (Some name) filename
    in
    Extlib.safe_remove filename;
    Create_by_copy_hook.apply (src, prj);
    prj
  with e ->
    Extlib.safe_remove filename;
    raise e

let journalized_create_by_copy =
  let lbl = Type.optlabel_func in
  Journal.register "Project.create_by_copy"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "src" current ty (Type.func Type.string ty))))
    unjournalized_create_by_copy

let create_by_copy
    ?(only=Selection.empty) ?(except=Selection.empty) ?(src=current()) name =
  journalized_create_by_copy only except src name

(* ************************************************************************** *)
(** {2 State dealing with Project} *)
(* ************************************************************************** *)

module type KIND = sig
  type t
  val dummy : t
  val name: t -> string
  val get_from_name: string -> t
  val add_dependency: t -> t -> unit
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
end

let identity = fun x -> x
let is_identity x = x == identity

let no_project _ _ = false

let no_descr = Unmarshal.Abstract

module Datatype = struct

  module type INPUT = sig
    type t
    val descr: Unmarshal.t
    val copy: t -> t
    val name: string
  end

  module type S = sig
    include INPUT
    val register_comparable:
      ?compare:(t -> t -> int) ->
      ?equal:(t -> t -> bool) ->
      ?hash:(t -> int) ->
      unit -> unit
    val is_comparable_set: unit -> bool
    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val mem_project: ((project -> bool) -> t -> bool) option ref
  end

  let default_copy = identity

  module Name = Namespace.Make(struct end)
  let extend_name x y = Name.get (Name.extend x y)
  let extend_name2 x y z = Name.get (Name.extend2 x y z)
  let extend_name3 x y z t = Name.get (Name.extend3 x y z t)

  module Register(Datatype: INPUT) = struct

    include Datatype
    let name = Name.get (Name.make Datatype.name)

    let mem_project: ((project -> bool) -> t -> bool) option ref = ref None

    type comparable =
	{ mutable hash: t -> int;
	  mutable equal: t -> t -> bool;
	  mutable compare: t -> t -> int;
	  mutable is_set: bool }

    let default_equal _ _ = fatal "no equality defined for datatype %S" name
    let default_compare _ _ =
      fatal "no comparison defined for datatype %S" name

    let comparable =
      { hash = Hashtbl.hash;
	equal = default_equal;
	compare = default_compare;
	is_set = false }

    (* eta-expansion is required *)
    let hash x = comparable.hash x
    let equal x y = comparable.equal x y
    let compare x y = comparable.compare x y
    let is_comparable_set () = comparable.is_set

    let register_comparable ?compare ?equal ?hash () =
      (* the test below if required in order to assign the right value to
	 [comparable.is_set]. *)
      match compare, equal, hash with
      | None, None, None -> ()
      | _ ->
	  comparable.is_set <- true;
	  let cmp, eq = match compare, equal with
	    | None, None -> default_compare, default_equal
	    | None, Some eq -> default_compare, eq
	    | Some cmp, None -> cmp, (fun x y -> cmp x y = 0)
	    | Some cmp, Some eq -> cmp, eq
	  in
	  comparable.compare <- cmp;
	  comparable.equal <- eq;
	  comparable.hash <- match hash with None -> Hashtbl.hash | Some h -> h

  end

  module Imperative(D: sig type t val copy: t -> t val name: string end) =
    Register
      (struct
	 include D
	 let descr = Unmarshal.Abstract
       end)

  module Persistent(D:sig type t val name: string end) =
    Imperative(struct include D let copy x = x end)

  module Own =
    Register
      (struct
	 type t = project
	 let copy _ = abort "never perform a project copy"
	 let name = "project"
	 let descr = Descr.project
       end)
  let () =
    Own.register_comparable ~hash ~equal ~compare ();
    Own.mem_project := Some (fun f x -> f x)


end

module Computation = struct

  type t = Computations.t
  type selection = Selection.t

  let name k = (Computations.value k).cname
  let get_from_name = Computations.get_from_name
  let add_dependency = Computations.add_dependency

  module Name = Namespace.Make(struct end)

  module type INPUT = sig
    type t
    val create: unit -> t
    val clear: t -> unit
    val get: unit -> t
    val set: t -> unit
    val clear_some_projects: (project -> bool) -> t -> bool
  end

  module type INFO = sig
    val name: string
    val dependencies : t list
  end

  module type MINIMAL_OUTPUT = sig
    val self: t
    val select: Kind.how -> selection -> selection
    val depend: t -> unit
    val name: string
  end

  module type OUTPUT = sig
    include MINIMAL_OUTPUT
    val mark_as_computed: ?project:project -> unit -> unit
    val is_computed: ?project:project -> unit -> bool
    val do_not_save: unit -> unit
    module Datatype: Datatype.S
    val howto_marshal: (Datatype.t -> 'a) -> ('a -> Datatype.t) -> unit
  end

  module Register
    (Datatype: Datatype.S)
    (State: INPUT with type t = Datatype.t)
    (Info: INFO) =
  struct

    let () = Hashtbl.add Descr.states_tbl Info.name Datatype.descr

    module Datatype = Datatype

    include Info

    type t =
	{ mutable state: State.t;
	  mutable computed: bool }

    (* Project --> plugin state. *)
    let tbl : t Project_Tbl.t = Project_Tbl.create 7

    let find p = Project_Tbl.find tbl p
    let mem p = Project_Tbl.mem tbl p

    let add p s = Project_Tbl.replace tbl p { state = s; computed = false }

    let remove p =
      assert (mem p);
      Project_Tbl.remove tbl p

    let commit p =
      if is_current p then
	try
	  let v = find p in
	  v.state <- State.get ()
	with Not_found ->
	  fatal
	    "state %S not associated with project %S; program will fail"
	    name
	    p.name

    let update_with ~force p s =
      if is_current p || force then begin
	feedback ~level:4 "update state %S of project %S" Info.name p.name;
	State.set s
      end

    let update p = update_with ~force:false p (find p).state

    let change ~update ~force p x =
      let v = find p in
      v.state <- x.state;
      v.computed <- x.computed;
      if update then update_with ~force p v.state

    let clean () =
      State.set (State.create ());
      Project_Tbl.clear tbl

    let create =
      let first = ref true in
      fun p ->
	assert (not (mem p));
	(* For efficiency purpose, do not create the initial project twice:
	   directly get it *)
	let mk () =
	  if !first then begin
	    first := false;
	    State.get ()
	  end else begin
            debug ~level:4 "creating state %S for project %S"
	      Info.name p.name;
	    State.create ()
          end
	in
	let s = mk () in
	add p s;
	update_with ~force:false p s

    let clear p =
      debug ~level:4 "clearing state %S for project %S" Info.name p.name;
      let v = find p in
      State.clear v.state;
      v.computed <- false;
      update_with ~force:false p v.state

    let clear_some_projects f p =
      debug ~level:4
	"clearing dangling project pointers in state %S for project %S"
	Info.name
	p.name;
      assert (not (f p));
      State.clear_some_projects f (find p).state

    let copy src dst =
      debug ~level:4 "copying state %S from %S to %S"
	Info.name src.name dst.name;
      let v = find src in
      change ~update:true ~force:false
	dst { v with state = Datatype.copy v.state }

    let must_save = ref true
    let do_not_save () = must_save := false

    (* ******* TOUCH THE FOLLOWING AT YOUR OWN RISK: DANGEROUS CODE ******** *)
    let marshal : (Datatype.t -> Obj.t) ref = ref Obj.repr
    let unmarshal : (Obj.t -> Datatype.t) ref = ref Obj.obj

    let howto_marshal (go_in:Datatype.t -> 'a) (go_out:'a -> Datatype.t) =
      marshal := (fun x -> Obj.repr (go_in x));
      unmarshal := fun x -> go_out (Obj.obj x)

    let serialize p =
      assert Cmdline.use_obj;
      debug ~level:4 "serializing state %S for project %S" Info.name p.name;
      commit p;
      let v = find p in
      let d = Digest.string Datatype.name in
      let obj = if !must_save then !marshal v.state else Obj.repr () in
      { s_value = obj;
	s_computed = v.computed;
	s_saved = !must_save;
	s_is_current = equal p (current ());
	s_digest = d }

    let unserialize p new_s =
      assert Cmdline.use_obj;
      if Digest.string Datatype.name = new_s.s_digest then begin
	debug ~level:4 "unserializing state %S for project %S"
	  Info.name p.name;
	let s, computed =
	  if !must_save && new_s.s_saved then
	    !unmarshal new_s.s_value, new_s.s_computed
	  else
	    (* invariant: the found state is equal to the default one since it
	       has been just created.
	       Do not call State.create to don't break sharing *)
	    (find p).state, false
	in
	(* update the local state if we are unserializing the current project
	   and we want to restore it *)
	change
	  ~update:(new_s.s_is_current && !Descr.project_under_copy_ref = None)
	  ~force:true
	  p
	  { state = s; computed = computed };
      end else
	raise
	  (IOError
	     ("project saved with incompatibles datatypes for state "
	      ^ Info.name))
    (* ********************************************************************* *)

    let mark_as_computed ?(project=(current ())) () =
      (find project).computed <- true

    let is_computed ?(project=(current ())) () = (find project).computed

    let self, depend =
      let me =
	Computations.create_kind
	  { cname = Info.name;
	    create = create;
	    remove = remove;
	    clear = clear;
	    clear_some_projects = clear_some_projects;
	    copy = copy;
	    commit = commit;
	    update = update;
	    serialize = serialize;
	    unserialize = unserialize;
	    clean = clean }
	  dependencies
      in
      me, add_dependency me

    let () =
      (* dynamically extend the existing projects with this state *)
      iter_on_projects create

    let select = Selection.add self

    let name = Name.get (Name.make Info.name)

  end

  let restore_list = ref []
  let clear_list = ref []
  let () =
    register_todo_before_clear (fun _ -> clear_list := []);
    register_todo_after_clear
      (fun p -> on p (fun () -> List.iter (fun f -> f ()) !clear_list) ())

  module Dynamic
    (Local: sig val restore: t -> (project -> unit) end)
    (Info: INFO) =
  struct

    let nop _ = assert false
    let nop2 _ _ = assert false

    let new_kind name clear =
      let clear p =
	debug ~level:4 "clearing dynamic state %S for project %S"
	  Info.name p.name;
	clear p
      in
      { cname = name;
	create = nop;
	remove = nop;
	clear = clear;
	clear_some_projects = (fun _ _ -> false);
	copy = nop2;
	commit = nop;
	update = nop;
	serialize = nop;
	unserialize = nop2;
	clean = nop }

    let state = Computations.Dynamic.create ()

    module State =
      Register
	(Datatype.Imperative
	   (struct
	      type t = Computations.Dynamic.graph
	      let copy _ = assert false
	      let name = Info.name
	    end))
	(struct
	   type t = Computations.Dynamic.graph
	   let create = Computations.Dynamic.create_graph
	   let get () = !state
	   let set x = state := x
	   let clear x =
	     (* Clearing the graph erases dependencies which matters for the
		topological iteration. Thus delay these operations. *)
	     let clear () = Computations.Dynamic.clear_graph x in
	     clear_list := clear :: !clear_list
	   let clear_some_projects _ _ = false
	 end)
	(Info)

    let () =
      let kind_from_name s = new_kind s nop in
      let update v =
	restore_list :=
	  (fun () ->  (Computations.value v).clear <- Local.restore v)
	:: !restore_list
      in
      State.howto_marshal
	Computations.Dynamic.marshal
	(Computations.Dynamic.unmarshal kind_from_name update)

    let self = State.self

    let add_dependency = Computations.Dynamic.add_dependency state

    let remove_computation ~reset s =
(*      Format.printf "removing computation %S of %S@." (name s) Info.name;*)
      if reset then begin
	let only = Selection.singleton s Kind.Only_Select_Dependencies in
	clear ~only ()
      end;
      Computations.Dynamic.remove_kind state s

    module Register(State: sig val clear: project -> unit end)(Info: INFO) =
    struct

      let self, depend =
	let me =
	  Computations.Dynamic.add_kind
	    state
	    (new_kind Info.name State.clear)
	    Info.dependencies
	in
	me, Computations.Dynamic.add_dependency state me

      let select = Selection.add self
      let name = Name.get (Name.make Info.name)

    end

  end

  let () =
    let restore () =
      List.iter (fun f -> f ()) !restore_list;
      restore_list := []
    in
    register_after_load_hook restore

  let dummy = Computations.dummy
  let dump_dependencies = Computations.dump_dependencies
  let dump_dynamic_dependencies = Computations.dump_dynamic_dependencies

  let compare = Computations.compare
  let equal = Computations.equal
  let hash = Computations.hash

end

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
	feedback "cannot restore the last breakpoint: %S" s;
	clear_breakpoint ()
    end

  let breakpoint () =
    if Cmdline.use_obj then begin
      clear_breakpoint ();
      filename := Filename.temp_file short_filename ".sav";
      Journal.prevent save_all !filename;
      Journal.save ()
    end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

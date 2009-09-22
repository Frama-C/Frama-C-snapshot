(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: project.ml,v 1.60 2009-02-05 12:35:06 uid568 Exp $ *)

(* ************************************************************************** *)
(** {2 Debugging} *)
(* ************************************************************************** *)

include Log.Register
  (struct
     let channel = Log.kernel_channel_name
     let label = Log.kernel_label_name
     let verbose_atleast n = Cmdline.kernel_verbose_level >= n
     let debug_atleast n = Cmdline.kernel_debug_level >= n
   end)

(* ************************************************************************** *)
(** {2 Project} *)
(* ************************************************************************** *)

type t = { pid: int; name: string; unique_name: string }
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
    dummy

type state_ondisk = { s_value: Obj.t; s_computed: bool; s_digest: Digest.t }

let global_pid = ref 0

(** Projects are comparable *)

(* Be careful: it is incorrect to compare projects using (==) while loading
   projects from disk whom states refer to another project.
   The function [load_all] ensures that comparing pid are conservative, so
   correct. *)

let equal p1 p2 = p1.pid = p2.pid
let compare p1 p2 = Pervasives.compare p1.pid p2.pid
let hash p = p.pid

(* Operations on states visible by operations on projects *)
type computation_operations =
    { cname: string;
      create: t -> unit;
      remove: t -> unit;
      clear: t -> unit;
      clear_if_project: t -> t -> bool;
      contain_any_project: bool;
      copy: t -> t -> unit;
      commit: t -> unit;
      update: t -> unit;
      clean: unit -> unit;
      serialize: t -> state_ondisk;
      unserialize: t -> state_ondisk -> unit;
      rehash: t -> t -> unit }

let dummy_name = "dummy"

(** Operations on computations. *)
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
	   clear_if_project = never_called;
	   contain_any_project = false;
	   copy = never_called;
	   commit = never_called;
	   update = never_called;
	   serialize = never_called;
	   unserialize = never_called;
	   clean = never_called;
	   rehash = never_called }
     end)

  let create_kind = create

  let create = iter (fun s -> s.create)
  let remove = iter (fun s -> s.remove)
  let clean = iter (fun s -> s.clean)

  let commit ?(only=Selection.empty) ?(except=Selection.empty) =
    iter_in_order only except (fun s -> s.commit)

  let update ?(only=Selection.empty) ?(except=Selection.empty) =
    iter_in_order only except (fun s -> s.update)

  let rehash p rehashed_p =
    iter_in_order Selection.empty Selection.empty
      (fun s p -> s.rehash p rehashed_p) p

  let clear only except =
    iter_in_order only except (fun s -> s.clear)

  let clear_if_project tested_p p =
    let cleared =
      apply_in_order Selection.empty Selection.empty
	(fun s acc ->
	   let v = value s in
	   if v.clear_if_project tested_p p then begin
	     let sel = Selection.singleton s Kind.Select_Dependencies in
             clear sel Selection.empty p;
	     Selection.add s Kind.Select_Dependencies acc
	   end else
	     acc)
	Selection.empty
    in
    if not (Selection.is_empty cleared) then begin
      warning
	"forcing to clear all values equal to project %S in project %S"
	tested_p.name
	p.name;
      debug ~append:(fun fmt -> Format.fprintf fmt "@]")
	"@[the involved states are:%t"
	(fun fmt ->
	   iter_in_order cleared Selection.empty
	     (fun v () -> Format.fprintf fmt "@ %S" v.cname)
	     ())
    end

  let clear_when_project only except p =
    let cleared =
      apply_in_order only except
	(fun s acc ->
	   let v = value s in
	   if v.contain_any_project then begin
	     let sel = Selection.singleton s Kind.Select_Dependencies in
             clear sel Selection.empty p;
	     Selection.add s Kind.Select_Dependencies acc
	   end else
	     acc)
	Selection.empty
    in
    if not (Selection.is_empty cleared) then begin
      warning "forcing to clear states containing projects in project %S"
	p.name;
      debug ~append:(fun fmt -> Format.fprintf fmt "@]")
	"@[the involved states are:%t"
	(fun fmt ->
	   iter_in_order cleared Selection.empty
	     (fun v () -> Format.fprintf fmt "@ %S" v.cname)
	     ())
    end

  let copy only except src =
    iter_in_order only except (fun s -> s.copy src)

  let serialize only except p =
    let tbl = Hashtbl.create 97 in
    iter_in_order only except
      (fun s () ->
	 assert (not (Hashtbl.mem tbl s.cname));
	 Hashtbl.add tbl s.cname (s.serialize p))
      ();
    tbl

  let unserialize only except dst tbl =
    let pp_err fmt n =
      if n > 0 then begin
	warning
	  fmt
	  n
	  (if n = 1 then "" else "s") (if n = 1 then "is" else "are")
	  (if n = 1 then "It does not exist in" else "They do not exist in")
      end
    in
    let ignored () =
      pp_err
	"%n state%s of the saved file %s ignored. %s your Frama-C configuration"
	(Hashtbl.length tbl);
      Hashtbl.iter (fun k _ -> debug "ignoring state %s" k) tbl
    in
    let to_default l =
      pp_err
	"%d state%s %s set to default. \
%s the Frama-C configuration of the saved file"
	(List.length l);
      List.iter
	(fun s -> debug "setting to default state %s" s.cname)
	(List.rev l);
      List.iter (fun s -> s.clear dst) l
    in
    let dft_list =
      apply_in_order only except
	(fun v acc ->
	   let s = value v in
	   try
	     let d = Hashtbl.find tbl s.cname in
	     s.unserialize dst d;
	     Hashtbl.remove tbl s.cname;
	     acc
	   with Not_found ->
	     (* set to default the unserializable dependencies of [v]. *)
	     fold_in_order
	       (Selection.singleton v Kind.Only_Select_Dependencies)
	       except
	       (fun s' acc ->
		  if Hashtbl.mem tbl s'.cname then s' :: acc else acc)
	       acc)
	[]
    in
    to_default (List.rev dft_list);
    ignored ()

end

module Selection = Computations.Selection

let guarded_feedback only except level fmt_msg =
  if verbose_atleast level then
    match Computations.number_of_applicants only except with
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
  (** The stack of projects. *)

let current () = Q.top projects

let is_current p = equal p (current ())

let iter_on_projects f = Q.iter f projects

let find_all name = Q.filter (fun p -> p.name = name) projects

(** Build a name different of all the others project names and based on
    [from]. *)
let mk_unique from =
  let rec build name =
    match List.length (find_all name) with
    | 0 -> name
    | n when n > 0 -> build (name ^ " (number " ^ string_of_int (n+1) ^ ")")
    | _ -> assert false
  in
  build from

let from_unique_name uname =
  Q.find (fun p -> p.unique_name = uname) projects

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
  Q.iter (Computations.clear_if_project project) projects

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

module Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_on_clear = Clear_Hook.extend

let journalized_clear =
  let lbl = Type.optlabel_func in
  Journal.register "Project.clear"
    (lbl "only" dft_sel Selection.ty
       (lbl "except" dft_sel Selection.ty
	  (lbl "src" current ty (Type.func Type.unit Type.unit))))
    (fun only except project () ->
       guarded_feedback only except 2 "clearing project %S" project.name;
       Clear_Hook.apply project;
       Computations.clear only except project)

let clear
    ?(only=Selection.empty) ?(except=Selection.empty) ?(project=current()) () =
  journalized_clear only except project ()

let unjournalized_clear_all () =
  Q.iter
    (Computations.clear
       Selection.empty
       Selection.empty)
    projects

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

exception IOError = Sys_error

module Before_load = Hook.Make(struct end)
let register_before_load_hook = Before_load.extend

module After_load = Hook.Make(struct end)
let register_after_load_hook = After_load.extend

(* Rehashing of projects is required while loading *)
module Rehash_Cache =
  Hashtbl.Make
    (struct
       type t = project
       let hash = hash
       let equal = equal
     end)
let rehash_cache = Rehash_Cache.create 17
let rehash p =
  try
    Rehash_Cache.find rehash_cache p
  with Not_found ->
    let v = create p.name in
    Rehash_Cache.add rehash_cache p v;
    v

(* magic numbers *)
let magic = 5
let magic_one () = 100 + magic (* TO(re)DO *)
let magic_all () = 10000 + try magic_one () with NoProject -> 0

(* Generic saving function on disk. *)
let gen_save f (n:int) filename =
  let cout = open_out_bin filename in
  output_value cout !Kind.version;
  output_value cout n;
  output_value cout !Graph.Blocks.cpt_vertex;
  output_value cout (f ());
  close_out cout

(* Generic loading function from disk. *)
let gen_load =
  let error old current =
    raise
      (IOError
	 (Format.sprintf
	    "project saved with an incompatible version (old:%S,current:%S)"
	    old
	    current))
  in
  fun f n filename ->
    let current = !Kind.version in
    let cin =
      try open_in_bin filename
      with Sys_error s -> raise (IOError s)
    in
    let read cin =
      try input_value cin with Failure s -> close_in cin; raise (IOError s)
    in
    let old : string = read cin in
    let m : int = read cin in
    if current <> old || m <> n then begin close_in cin; error old current end;
    let c = read cin in
    let state = read cin in
    close_in cin;
    Graph.Blocks.after_unserialization c;
    f state

let write, read =
  (fun only except p ->
     (Computations.serialize only except p :
	(string, state_ondisk) Hashtbl.t)),
  (fun only except dst (data: (string, state_ondisk) Hashtbl.t) ->
     Computations.unserialize only except dst data)

let unjournalized_save only except project filename =
  if Cmdline.use_obj then begin
    guarded_feedback only except 2
      "saving project %S into file %S"
      project.name filename;
    gen_save (fun () -> write only except project) (magic_one ()) filename
  end else
    raise (IOError "saving a file is not supported in the 'no obj' mode")

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

(* loading one single project [p] from disk is incorrect if one state of [p]
   refers to a project [p'] which does not exist in the current session. *)
let unjournalized_load safe only except name filename =
  if Cmdline.use_obj then begin
    guarded_feedback only except 2
      "loading project %S from file %S"
      name filename;
    let p = create name in
    on p
      (fun () ->
	 if safe then Before_load.apply ();
	 gen_load (read only except p) (magic_one ()) filename;
	 if safe then begin
	   Computations.clear_when_project only except p;
	   After_load.apply ()
	 end)
      ();
    p
  end else
    raise (IOError "loading a file is not supported in the 'no obj' mode")

let journalized_load =
  let lbl = Type.optlabel_func in
  Journal.register "Project.load"
    (lbl "safe" (fun () -> true) Type.bool
       (lbl "only" dft_sel Selection.ty
	  (lbl "except" dft_sel Selection.ty
	     (Type.func ~label:("name", None) Type.string
		(Type.func Type.string ty)))))
    unjournalized_load

let unsafe_load
    ?(safe=true) ?(only=Selection.empty) ?(except=Selection.empty) ~name
    filename =
  journalized_load safe only except name filename

(* mask the optional parameter [save] *)
let load ?only ?except ~name filename =
  unsafe_load ?only ?except ~name filename

let write_all, read_all =
  (fun () ->
     (Q.fold
	(fun acc p ->
	   (p, write Selection.empty Selection.empty p) :: acc)
	[]
	projects
	:'a)),
  (fun (data:'a) ->
     let load_one (p, s) =
       let p =
	 try
	   let p = Rehash_Cache.find rehash_cache p in
	   Q.move_at_end p projects;
	   p
	 with Not_found ->
	   create p.name
       in
       on p
	 (fun () ->
	    Before_load.apply ();
	    read Selection.empty Selection.empty p s;
	    After_load.apply ())
	 ()
     in
     List.iter load_one (List.rev data))

let save_all =
  Journal.register "Project.save_all" (Type.func Type.string Type.unit)
    (fun f ->
       if Cmdline.use_obj then begin
	 feedback ~level:2 "saving all projects into file %S" f;
	 gen_save write_all (magic_all ()) f
       end else
	 raise (IOError "saving a file is not supported in the 'no obj' mode"))

let unjournalized_load_all f =
  if Cmdline.use_obj then begin
    remove_all ();
    feedback ~level:2 "loading all projects from file %S" f;
    try
      gen_load read_all (magic_all ()) f;
      Rehash_Cache.clear rehash_cache;
    with
    | IOError _ as e ->
	set_current (create "default");
        Rehash_Cache.clear rehash_cache;
	raise e
    | _ -> assert false
  end else
    raise (IOError "loading a file is not supported in the 'no obj' mode")

let load_all =
  Journal.register
    "Project.load_all"
    (Type.func Type.string Type.unit)
    unjournalized_load_all

module Create_by_copy_hook = Hook.Build(struct type t = project * project end)
let create_by_copy_hook f =
  Create_by_copy_hook.extend (fun (src, dst) -> f src dst)

let unjournalized_create_by_copy only except src name =
  let filename = Filename.temp_file "frama_c_create_by_copy" ".sav" in
  save ~only ~except ~project:src filename;
  try
    let prj = unsafe_load ~safe:false ~only ~except ~name filename in
    Sys.remove filename;
    Create_by_copy_hook.apply (src, prj);
    prj
  with
  | IOError _ as e ->
    Sys.remove filename;
    raise e
  | _ ->
      Sys.remove filename;
      assert false

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
  val add_dependency: t -> t -> unit
end

let identity = fun x -> x
let is_identity x = x == identity

let no_project _ _ = false

module Datatype = struct

  module type INPUT = sig
    type t
    val rehash: t -> t
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
      ?physical_hash:(t -> int) ->
      unit -> unit
    val is_comparable_set: unit -> bool
    val hash: t -> int
    val physical_hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val contain_project: (project -> t -> bool) option ref
  end

  let default_rehash = identity
  let default_copy = identity

  module Name = Namespace.Make(struct end)
  let extend_name x y = Name.get (Name.extend x y)
  let extend_name2 x y z = Name.get (Name.extend2 x y z)
  let extend_name3 x y z t = Name.get (Name.extend3 x y z t)

  module Register(Datatype: INPUT) = struct

    include Datatype
    let name = Name.get (Name.make Datatype.name)

    let contain_project : (project -> t -> bool) option ref = ref None

    type comparable =
	{ mutable hash: t -> int;
	  mutable physical_hash: t -> int;
	  mutable equal: t -> t -> bool;
	  mutable compare: t -> t -> int;
	  mutable is_set: bool }

    let default_equal _ _ = fatal "no equality defined for datatype %S" name
    let default_compare _ _ =
      fatal "no comparison defined for datatype %S" name

    let comparable =
      { hash = Hashtbl.hash;
	physical_hash = Hashtbl.hash;
	equal = default_equal;
	compare = default_compare;
	is_set = false }

    (* eta-expansion is required *)
    let hash x = comparable.hash x
    let physical_hash x = comparable.physical_hash x
    let equal x y = comparable.equal x y
    let compare x y = comparable.compare x y
    let is_comparable_set () = comparable.is_set

    let register_comparable ?compare ?equal ?hash ?physical_hash () =
      (* the test below if required in order to assign the right value to
	 [comparable.is_set]. *)
      match compare, equal, hash, physical_hash with
      | None, None, None, None -> ()
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
	  let h, ph = match hash, physical_hash with
	    | None, None -> Hashtbl.hash, Hashtbl.hash
	    | None, Some ph -> Hashtbl.hash, ph
	    | Some h, None -> h, h
	    | Some h, Some ph -> h, ph
	  in
	  comparable.hash <- h;
	  comparable.physical_hash <- ph

    module Rehash_Cache =
      Hashtbl.Make
	(struct
	  type t = Datatype.t
(* [pc 2009/07] need a better physical hash function because there may
   be some unsharing in the original data that cause the hashtable to
   degenerate with a semantical hash function
let hash x = comparable.physical_hash x (* eta-expansion required *) *)

	  let hash = Extlib.address_of_value
	  let equal = (==)
	 end)

    let cache = ref (Rehash_Cache.create 17)
    let rehash =
      if is_identity Datatype.rehash then identity
      else begin
        register_after_load_hook (fun () -> cache := Rehash_Cache.create 5);
	fun x ->
	  try
	    Rehash_Cache.find !cache x
	  with Not_found ->
            let v = Datatype.rehash x in
            Rehash_Cache.add !cache x v;
            v
      end

  end

  module Imperative(D: sig type t val copy: t -> t val name: string end) =
    Register
      (struct
	 include D
	 let rehash = identity
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
	 let rehash = rehash
	 let descr = Unmarshal.Abstract (* TODO: Ask Julien *)
       end)
  let () =
    Own.register_comparable ~hash ~equal ~compare ();
    Own.contain_project := Some equal

end

module Computation = struct

  type t = Computations.t
  type selection = Selection.t

  let name k = (Computations.value k).cname

  module Name = Namespace.Make(struct end)

  let add_dependency = Computations.add_dependency

  module type INPUT = sig
    type t
    val create: unit -> t
    val clear: t -> unit
    val get: unit -> t
    val set: t -> unit
    val clear_if_project: project -> t -> bool
  end

  module type INFO = sig
    val name: string
    val dependencies : t list
  end

  module type OUTPUT = sig
    val self: t
    val select: Kind.how -> Selection.t  -> Selection.t
    val depend: t -> unit
    val mark_as_computed: ?project:project -> unit -> unit
    val is_computed: ?project:project -> unit -> bool
    val do_not_save: unit -> unit
    module Datatype: Datatype.S
    val name: string
  end

  module Register
    (Datatype: Datatype.S)
    (State: INPUT with type t = Datatype.t)
    (Info: INFO) =
  struct

    module Datatype = Datatype

    include Info

    type t = { mutable state: State.t; mutable computed: bool }

    (** Project --> plugin state. *)
    let tbl: (int, t) Hashtbl.t = Hashtbl.create 17
      (* Do not use Inthash: project must not depend of Cil. *)

    let find p = Hashtbl.find tbl p.pid
    let mem p = Hashtbl.mem tbl p.pid

    let add p s = Hashtbl.replace tbl p.pid { state = s; computed = false }

    let remove p =
      assert (mem p);
      Hashtbl.remove tbl p.pid

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

    let update_with p s = if is_current p then begin
      feedback ~level:4 "update state %S of project %S" Info.name p.name;
      State.set s
    end

    let update p = update_with p (find p).state

    let change_and_update p x =
      let v = find p in
      v.state <- x.state;
      v.computed <- x.computed;
      update_with p v.state

    let clean () =
      State.set (State.create ());
      Hashtbl.clear tbl

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
	update_with p s

    let clear p =
      debug ~level:4 "clearing state %S for project %S" Info.name p.name;
      let v = find p in
      State.clear v.state;
      v.computed <- false;
      update_with p v.state

    let clear_if_project tested_p p =
      debug ~level:4
	"clearing values equal to project %S in state %S for project %S"
	tested_p.name Info.name p.name;
      assert (p <> tested_p);
      State.clear_if_project tested_p (find p).state

    let copy src dst =
      debug ~level:4 "copying state %S from %S to %S"
	Info.name src.name dst.name;
      let v = find src in
      change_and_update dst { v with state = Datatype.copy v.state }

    let rehash p rehashed_p =
      debug ~level:4 "rehashing state %S for project %S" Info.name p.name;
      let v = find p in
      Hashtbl.remove tbl p.pid;
      Hashtbl.add tbl rehashed_p.pid v

    let must_save = ref true
    let do_not_save () = must_save := false

    (* ******* TOUCH THE FOLLOWING AT YOUR OWN RISK: DANGEROUS CODE ******** *)
    let serialize p =
      if Cmdline.use_obj then begin
	debug ~level:4 "serializing state %S for project %S" Info.name p.name;
	commit p;
	let v = find p in
	let d = Digest.string Datatype.name in
	let obj = if !must_save then Obj.repr v.state else Obj.repr () in
	{ s_value = obj; s_computed = v.computed; s_digest = d }
      end else
	assert false (* aborting already occured *)

    let unserialize p new_s =
      if Cmdline.use_obj then
	if Digest.string Datatype.name = new_s.s_digest then begin
	  debug ~level:4 "unserializing state %S for project %S"
	    Info.name p.name;
	  let s, computed =
	    if !must_save then
	      Datatype.rehash (Obj.obj new_s.s_value), new_s.s_computed
	    else
	      State.create (), false
	  in
	  change_and_update p { state = s; computed = computed }
	end else
	  raise
	    (IOError
	       ("project saved with incompatibles datatypes for state "
		^ Info.name))
      else
	assert false (* aborting already occured *)
    (* ********************************************************************* *)

    let self, depend =
      let me =
	Computations.create_kind
	  { cname = Info.name;
	    create = create;
	    remove = remove;
	    clear = clear;
	    clear_if_project = clear_if_project;
	    contain_any_project = !Datatype.contain_project <> None;
	    copy = copy;
	    commit = commit;
	    update = update;
	    serialize = serialize;
	    unserialize = unserialize;
	    clean = clean;
	    rehash = rehash }
	  dependencies
      in
      me, add_dependency me

    let () =
      (* dynamically extend the existing projects with this state *)
      iter_on_projects create

    let select = Selection.add self

    let mark_as_computed ?(project=(current ())) () =
      (find project).computed <- true

    let is_computed ?(project=(current ())) () = (find project).computed

    let name = Name.get (Name.make Info.name)

  end

  let dummy = Computations.dummy
  let dump_dependencies = Computations.dump_dependencies

end

(* ************************************************************************** *)
(** {2 Undoing} *)
(* ************************************************************************** *)

module Undo = struct

  let short_filename = "frama_c_undo_restore"
  let filename = ref ""

  let clear_breakpoint () = try Sys.remove !filename with _ -> ()

  let restore () =
    try
      Journal.prevent load_all !filename;
      Journal.restore ();
      clear_breakpoint ()
    with IOError s ->
      feedback "cannot restore the last breakpoint: %S" s;
      clear_breakpoint ()

  let breakpoint () =
    clear_breakpoint ();
    filename := Filename.temp_file short_filename ".sav";
    Journal.prevent save_all !filename;
    Journal.save ()

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

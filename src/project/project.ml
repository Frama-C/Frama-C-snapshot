(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* $Id: project.ml,v 1.50 2008/11/18 16:37:29 uid562 Exp $ *)

(* ************************************************************************** *)
(** {2 Debugging} *)
(* ************************************************************************** *)

let debug_level = ref 0
let set_debug_level n = debug_level := n

let debug0 n fmt = if !debug_level >= n  then Format.eprintf fmt
let debug n fmt s = if !debug_level >= n then Format.eprintf fmt s
let debug2 n fmt s1 s2 = if !debug_level >= n then Format.eprintf fmt s1 s2
let debug3 n fmt s1 s2 s3 =
  if !debug_level >= n then Format.eprintf fmt s1 s2 s3

(* ************************************************************************** *)
(** {2 Project} *)
(* ************************************************************************** *)

type t = { pid: int; name: string; unique_name: string }
type project = t

let dummy = { pid = 0; name = ""; unique_name = ""}
let repr = Type.make "Project.t" dummy

let () =
  Journal.register_printer
    repr
    (fun fmt p ->
       Format.fprintf fmt "(Project.from_unique_name %S)" p.unique_name)

type state_ondisk = { s_value: Obj.t; s_computed: bool; s_digest: Digest.t }

(** Projects are comparable *)

let compare p1 p2 = Pervasives.compare p1.pid p2.pid
let equal = (==)
let hash p = p.pid

(** Roughly first-class-value type for computation kinds. *)
type computation_operations =
    { cname: string;
      create: t -> unit;
      remove: t -> unit;
      clear: t -> unit;
      copy: t -> t -> unit;
      commit: t -> unit;
      update: t -> unit;
      clean: unit -> unit;
      serialize: t -> state_ondisk;
      unserialize: t -> state_ondisk -> unit }

let dummy_name = "dummy"

(** Operations on computations. *)
module Computations = struct

  include Kind.Make
    (struct
       type t = computation_operations
       let name = "computations"
       let kind_name x = x.cname
       let dummy =
	 { cname = dummy_name;
	   create = Extlib.nop;
	   remove = Extlib.nop;
	   clear = Extlib.nop;
	   copy = (fun _ _ -> ());
	   commit = Extlib.nop;
	   update = Extlib.nop;
	   serialize = (* not called *)
	     (fun _ -> { s_value = Obj.repr ();
			 s_computed = false;
			 s_digest = Digest.string "" });
	   unserialize = (fun _ s -> assert (() = Obj.obj s.s_value));
	   clean = Extlib.nop }
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
(*    Format.printf "Project.clear@."; (* project.clear happening too often
can cause slowness of regression tests and be otherwise undetectable.
Uncomment to check how often it happens *) *)
    iter_in_order only except (fun s -> s.clear)

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
    let use_info = ref false in
    let pp_err fmt n pp_debug =
      if n > 0 then begin
	Format.eprintf ("Warning: " ^^ fmt ^^ "@.")
	  n
	  (if n = 1 then "" else "s") (if n = 1 then "is" else "are")
	  (if n = 1 then "It does not exist in" else "They do not exist in");
	if !debug_level >= 1 then pp_debug () else use_info := true
      end
    in
    let ignored () =
      pp_err
	"%n state%s of the saved file %s ignored. \
%s your Frama-C configuration."
	(Hashtbl.length tbl)
	(fun () ->
	   Hashtbl.iter
	     (fun k _ -> Format.eprintf "Ignoring state %s.@." k)
	     tbl)
    in
    let to_default l =
      pp_err
	"%d state%s %s set to default. \
%s the Frama-C configuration of the saved file."
	(List.length l)
	(fun () ->
	   List.iter
	     (fun s -> Format.eprintf "Setting to default state %s.@." s.cname)
	     (List.rev l));
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
    ignored ();
    if !use_info then
      Format.eprintf "Use -project-debug \"-debug 1\" for some details.@."

end

module Selection = Computations.Selection

(* [Julien] TODO: change this datastructure for a queue with promotion? *)
module Q = Qstack.Make(struct type u = t type t = u let equal = equal end)

let projects = Q.create ()
  (** The stack of projects. *)

let current () = Q.top projects

let is_current p = p == (current ())

let iter_on_projects f = Q.iter f projects

let nb_projects = ref 0

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

let create name =
  debug 1 "Creating project %s.@." name;
  incr nb_projects;
  let p = { pid = !nb_projects; name = name; unique_name = mk_unique name } in
  Q.add_at_end p projects;
  Computations.create p;
  Create_Hook.apply p;
  p

let name p = p.name
let unique_name p = p.unique_name

exception NoProject = Q.Empty

module Set_Current_Hook_User = Hook.Make(struct end)
module Set_Current_Hook = Hook.Make(struct end)
let register_after_set_current_hook ~user_only =
  if user_only then Set_Current_Hook_User.extend else Set_Current_Hook.extend

let force_set_current ?(on=false) ?only ?except p =
  if not (Q.mem p projects) then
    invalid_arg ("Project.set_current: " ^ p.name ^ " does not exist");
  let old = current () in
  Computations.commit ?only ?except old;
  (try Q.move_at_top p projects with Invalid_argument _ -> assert false);
  debug3 1 "Project %s (id %d) is now the current one  (was project %s).@."
    p.name p.pid old.name;
  assert (p == current ());
  Computations.update ?only ?except p;
  if not on then Set_Current_Hook_User.apply ();
  Set_Current_Hook.apply ()

let j_force_set_current =
  Journal.register "Project.set_current" (Type.func repr Type.unit)
    (fun s -> force_set_current s (* etat-expansion required: the function
                                     must be registered once.
                                   *))

let set_current ?(on=false) ?only ?except p =
  if not (is_current p) then
    match on,only,except with
    | false, None, None -> j_force_set_current p
    | _ -> force_set_current ~on ?only ?except p

let remove project =
  debug 1 "Removing project %s.@." project.name;
  Computations.remove project;
  let old_current = current () in
  Q.remove project projects;
  if project == old_current then Computations.update (current ())

let remove =
  Journal.register "Project.remove" (Type.func repr Type.unit ) remove

let remove ?(project=current()) () = remove project

let remove_all () =
  debug0 1 "Removing all the projects.@.";
  try
    Computations.clean ();
    Q.clear projects;
    nb_projects := 0
  with NoProject ->
    ()

let copy
    ?(only=Selection.empty)
    ?(except=Selection.empty)
    ?(src=current())
    dst =
  debug2 1 "Copy project from %s to %s.@." src.name dst.name;
  Computations.commit ~only ~except src;
  Computations.copy only except src dst

module Clear_Hook = Hook.Make(struct end)
let register_todo_on_clear = Clear_Hook.extend

let clear
    ?(only=Selection.empty)
    ?(except=Selection.empty)
    ?(project=current()) () =
  debug 1 "Cleaning project %s.@." project.name;
  Clear_Hook.apply ();
  Computations.clear only except project

let clear_all () =
  Q.iter
    (Computations.clear
       Selection.empty
       Selection.empty)
    projects

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

let set_current ?only ?except p =  set_current ?only ?except p (* hide ?on *)

exception IOError = Sys_error

module Before_load = Hook.Make(struct end)
let register_before_load_hook = Before_load.extend

module After_load = Hook.Make(struct end)
let register_after_load_hook = After_load.extend

(* magic numbers *)
let magic = 5
let magic_one () = (*Dependencies.nb_kind () * *)(100 + magic) (* TO(re)DO *)
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
	    "project saved with an incompatible version (old:%s,current:%s)"
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

let save
    ?(only=Selection.empty)
    ?(except=Selection.empty)
    ?(project=current())
    filename =
  debug2 1 "Saving project %s into file %s.@." project.name filename;
  gen_save (fun () -> write only except project) (magic_one ()) filename

let load
    ?(only=Selection.empty)
    ?(except=Selection.empty)
    ~name
    filename =
  debug2 1 "Loading project %s from file %s.@." name filename;
  let p = create name in
  on p
    (fun () ->
       Before_load.apply ();
       gen_load (read only except p) (magic_one ()) filename;
       After_load.apply ())
    ();
  p

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
       let p = create p.name in
       on p
	 (fun () ->
	    Before_load.apply ();
	    read Selection.empty Selection.empty p s;
	    After_load.apply ())
	 ()
     in
     List.iter load_one (List.rev data))

let save_all f =
  debug 1 "Saving all projects into file %s.@." f;
  gen_save write_all (magic_all ()) f
let save_all =
  Journal.register "Project.save_all" (Type.func Type.string Type.unit)
    save_all

let load_all f =
  remove_all ();
  debug 1 "Loading all projects from file %s.@." f;
  try gen_load read_all (magic_all ()) f
  with IOError _ as e ->
    set_current (create "default");
    raise e

let load_all =
  Journal.register "Project.load_all" (Type.func Type.string Type.unit)
    load_all

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

module Datatype = struct

  module type INPUT = sig
    type t
    val rehash: t -> t
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

    type comparable =
	{ mutable hash: t -> int;
	  mutable physical_hash: t -> int;
	  mutable equal: t -> t -> bool;
	  mutable compare: t -> t -> int;
	  mutable is_set: bool }

    let comparable =
      { hash = Hashtbl.hash;
	physical_hash = Hashtbl.hash;
	equal = (=);
	compare = Pervasives.compare;
	is_set = false }

    let hash = comparable.hash
    let physical_hash = comparable.physical_hash
    let equal = comparable.equal
    let compare = comparable.compare
    let is_comparable_set () = comparable.is_set

    let register_comparable ?compare ?equal ?hash ?physical_hash () =
      (* the test below if required in order to assign the right value to
	 [comparable.is_set]. *)
      match compare, equal, hash, physical_hash with
      | None, None, None, None -> ()
      | _ ->
	  comparable.is_set <- true;
	  let cmp, eq = match compare, equal with
	    | None, None -> Pervasives.compare, (=)
	    | None, Some eq -> Pervasives.compare, eq
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
	   let hash x = comparable.physical_hash x (* eta-expansion required *)
	   let equal = (==)
	 end)

    let cache = ref (Rehash_Cache.create 17)
    let rehash =
      if is_identity Datatype.rehash then
	identity
      else begin
	register_after_load_hook (fun () -> cache := Rehash_Cache.create 5);
	fun x ->
	  try
	    Rehash_Cache.find !cache x;
	  with Not_found ->
            let v = Datatype.rehash x in
            Rehash_Cache.add !cache x v;
            v
      end

  end

  module Imperative(D: sig type t val copy: t -> t val name: string end) =
    Register(struct include D let rehash = identity end)

  module Persistent(D:sig type t val name: string end) =
    Imperative(struct include D let copy x = x end)

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
	with Not_found -> begin
	  debug 2 "State %s not found@. Program will fail" name ;
	  assert false
	end

    let update_with p s = if is_current p then begin
      debug2 2 "update state %s of project %s@." Info.name p.name;
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
            debug2 2 "creating state %s for project %s@." Info.name p.name;
	    State.create ()
          end
	in
	let s = mk () in
	add p s;
	update_with p s

    let clear p =
      debug2 2 "clearing state %s for project %s@." Info.name p.name;
      let v = find p in
      State.clear v.state;
      v.computed <- false;
      update_with p v.state

    let copy src dst =
      debug3 2 "copying state %s from %s to %s@." Info.name src.name dst.name;
      let v = find src in
      change_and_update dst { v with state = Datatype.copy v.state }

    let must_save = ref true
    let do_not_save () = must_save := false

    (* ******* TOUCH THE FOLLOWING AT YOUR OWN RISK: DANGEROUS CODE ******** *)
    let serialize p =
      debug2 2 "serializing state %s for project %s.@." Info.name p.name;
      commit p;
      let v = find p in
      let d = Digest.string Datatype.name in
      let obj = if !must_save then Obj.repr v.state else Obj.repr () in
      { s_value = obj; s_computed = v.computed; s_digest = d }

    let unserialize p new_s =
      if Digest.string Datatype.name = new_s.s_digest then begin
	debug2 2 "unserializing state %s for project %s@." Info.name p.name;
	let s =
	  if !must_save then Datatype.rehash (Obj.obj new_s.s_value)
	  else State.create ()
	in
	change_and_update p { state = s; computed = new_s.s_computed }
      end else
	raise
	  (IOError
	     ("project saved with incompatibles datatypes for state "
	      ^ Info.name))
    (* ********************************************************************* *)

    let self, depend =
      let me =
	Computations.create_kind
	  { cname = Info.name;
	    create = create;
	    remove = remove;
	    clear = clear;
	    copy = copy;
	    commit = commit;
	    update = update;
	    serialize = serialize;
	    unserialize = unserialize;
	    clean = clean }
	  dependencies
      in
      me, add_dependency me

    let select = Selection.add self

    let mark_as_computed ?(project=(current ())) () =
      (find project).computed <- true

    let is_computed ?(project=(current ())) () = (find project).computed

    let name = Name.get (Name.make Info.name)

  end

  let dummy = Computations.dummy
  let dump_dependencies = Computations.dump_dependencies

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

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

(* $Id: project.ml,v 1.24 2008/07/11 09:18:50 uid568 Exp $ *)

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
let debug4 n fmt s1 s2 s3 s4 =
  if !debug_level >= n then Format.eprintf fmt s1 s2 s3 s4

(* ************************************************************************** *)
(** {2 Project} *)
(* ************************************************************************** *)

type t = { pid: int; name: string; unique_name: string }
type project = t

type state_ondisk = { s_value: Obj.t; s_computed: bool }

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

(** Roughly first-class-value type for datatype kinds. *)
type datatype_operations =
    { dname: string;
      before_load: unit -> unit;
      after_load: unit -> unit }      

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
	     (fun _ -> { s_value = Obj.repr (); s_computed = false });
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
      (fun s () -> Hashtbl.add tbl s.cname (s.serialize p))
      ();
    tbl

  let unserialize only except dst tbl =
    let ignored () =
      if Hashtbl.length tbl > 0 then begin
	Format.eprintf "Warning: Some states in the saved file are ignored.@.";
	if !debug_level >= 1 then
	  Hashtbl.iter 
	    (fun k _ -> 
	       Format.eprintf "Ignoring state %s@." k)
	    tbl
      end
    in
    let to_default l =
      if l <> [] then begin
	Format.eprintf "Warning: Some states in the saved file are set to \
default.@.";
      	if !debug_level >= 1 then
	  List.iter 
	    (Format.eprintf "Setting to default state %s@.") 
	    (List.rev l)
      end
    in
    let dft_list =
      fold_in_order only except 
      (fun s acc -> 
	 try
	   let d = Hashtbl.find tbl s.cname in
	   s.unserialize dst d; 
	   Hashtbl.remove tbl s.cname;
	   acc
	 with Not_found ->
	   (* When the project is created states were already 
	      filled by default values *)
	   s.cname :: acc)    
      []
    in
    to_default dft_list;
    ignored ()

end

module Selection = Computations.Selection

(** Operations on datatypes *)
module Datatypes = struct

  include Kind.Make
    (struct 
       type t = datatype_operations 
       let name = "datatypes"
       let kind_name x = x.dname 
       let dummy =
	 { dname = dummy_name; 
	   before_load = Extlib.nop; 
	   after_load = Extlib.nop }
     end)

  let before_load = 
    iter_in_order Selection.empty Selection.empty 
      (fun x () -> (*Format.printf "bl (%s)@." x.dname ;*) x.before_load ())

  let after_load = iter (fun x -> x.after_load)

end

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

let create name =
  debug 1 "Creating project %s.@." name;
  incr nb_projects;
  let p = { pid = !nb_projects; name = name; unique_name = mk_unique name } in
  Computations.create p;
  Q.add_at_end p projects;
  p

let name p = p.name
let unique_name p = p.unique_name

exception NoProject = Q.Empty

let set_current ?only ?except p =
  if not (is_current p) then begin
    if not (Q.mem p projects) then
      invalid_arg ("Project.set_current: " ^ p.name ^ " does not exist");
    let old = current () in
    Computations.commit ?only ?except old;
    (try Q.move_at_top p projects with Invalid_argument _ -> assert false);
    debug3 1 "Project %s (id %d) is now the current one  (was project %s).@." 
      p.name p.pid old.name;
    assert (p == current ());
    Computations.update ?only ?except p
  end

let remove ?(project=current()) () =
  debug 1 "Removing project %s.@." project.name;
  Computations.remove project;
  let old_current = current () in
  Q.remove project projects;
  if project == old_current then Computations.update (current ())

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
  let set p = set_current ?only ?except p in
  try
    set p;
    let r = f x in
    set old_current;
    r
  with e -> 
    set old_current;
    raise e

exception IOError = Sys_error

(* magic numbers *)
let magic = 3
let magic_one () = (*Dependencies.nb_kind () * *)(100 + magic) (* TO(re)DO *)
let magic_all () = 10000 + try magic_one () with NoProject -> 0

(* Generic saving function on disk. *)
let gen_save f (n:int) filename =
  let cout = open_out_bin filename in
  output_value cout n;
  output_value cout (f ());
  close_out cout

(* Generic loading function from disk. *)
let gen_load =
  let error () = 
    raise (IOError "project saved with an incompatible version") 
  in
  fun f n filename ->
    let cin = open_in_bin filename in
    let m : int = input_value cin in
    if m <> n then error ();
    let state = input_value cin in
    close_in cin;
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
  Datatypes.before_load ();
  gen_load (read only except p) (magic_one ()) filename;
  Datatypes.after_load ();
  p

let write_all, read_all =
  (fun () ->
     (Q.fold 
	(fun acc p -> (p, write Selection.empty Selection.empty p) :: acc)
	[]
	projects
	:'a)),
  (fun (data:'a) ->
     let load_one (p, s) =
       let p = create p.name in
       Datatypes.before_load ();
       read Selection.empty Selection.empty p s;
       Datatypes.after_load ()
     in
     List.iter load_one data)

let save_all f = 
  debug 1 "Saving all projects into file %s.@." f;
  gen_save write_all (magic_all ()) f

let load_all f = 
  remove_all ();
  debug 1 "Loading all projects from file %s.@." f;
  gen_load read_all (magic_all ()) f

(* ************************************************************************** *)
(** {2 State dealing with Project} *)
(* ************************************************************************** *)

module type KIND = sig
  type t
  val dummy : t
  val name: t -> string
  exception Circular_Dependency of t * t
  val add_dependency: t -> t -> unit
end

module type NAME = sig
  type t
  exception AlreadyExists of string
  val make: string -> t
  val extend: string -> t -> t
  val extend2: string -> t -> t -> t
  val get: t -> string
end

module Make_Name(X:sig end) = struct

  type t = string
      
  module S = Set.Make(String)

  exception AlreadyExists of string

  let make =
    let names = ref (S.singleton dummy_name) in
    fun s -> 
      assert ( (* do not check with -noassert *)
	if S.mem s !names then raise (AlreadyExists s);
	names := S.add s !names;
	true);
      s

  let extend s name = s ^ "(" ^ name ^ ")"
  let extend2 s name1 name2 = s ^ "(" ^ name1 ^ "," ^ name2 ^ ")"

  let get s = s

end

module Datatype = struct

  type t = Datatypes.t

  let name k = (Datatypes.value k).dname

  module Name = Make_Name(struct end)

  exception Circular_Dependency = Datatypes.Circular
  let add_dependency = Datatypes.add_dependency

  module type INPUT = sig
    val dependencies : t list
    type t
    val before_load: unit -> unit
    val after_load: unit -> unit
    val rehash: t -> t
    val copy: t -> t
    val name: Name.t
  end

  module type OUTPUT = sig
    val self: t
    val depend: t -> unit
    include INPUT
  end

  module Register(Datatype: INPUT) = struct

    include Datatype

    let self, depend =
      let me =
	Datatypes.create
	  { dname = name;
	    before_load = before_load;
	    after_load = after_load }
	  dependencies
      in
      me, add_dependency me

  end

  let dummy = Datatypes.dummy
  let depend_of_dummy = add_dependency dummy

  let dump_dependencies f = Datatypes.dump_dependencies f

  module Imperative(D: sig type t val copy: t -> t end) = struct
    include D
    let before_load = Extlib.nop
    let after_load = Extlib.nop
    let rehash x = x
    let copy = D.copy
    let self = dummy
    let depend = depend_of_dummy
    let name = dummy_name
    let dependencies = []
  end

  module Persistent(D:sig type t end) = 
    Imperative(struct include D let copy x = x end)

end

module Computation = struct

  type t = Computations.t
  type selection = Selection.t

  let name k = (Computations.value k).cname

  module Name = Make_Name(struct end)

  exception Circular_Dependency = Computations.Circular
  let add_dependency = Computations.add_dependency

  module type INPUT = sig
    type t
    val create: unit -> t
    val clear: t -> unit
    val get: unit -> t
    val set: t -> unit
  end

  module type INFO = sig
    val name: Name.t
    val dependencies : t list
  end

  module type OUTPUT = sig 
    val self: t
    val select: Kind.how -> Selection.t  -> Selection.t
    val depend: t -> unit
    val mark_as_computed: ?project:project -> unit -> unit
    val is_computed: ?project:project -> unit -> bool
    module Datatype: Datatype.OUTPUT
    val name: Name.t
  end

  module Register
    (Datatype: Datatype.OUTPUT)
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
	add p (mk ())

    let clear p = 
      let v = find p in
      State.clear v.state;
      v.computed <- false;
      update_with p (State.create ())

    let copy src dst = 
      debug3 2 "copying state %s from %s to %s@." Info.name src.name dst.name;
      let v = find src in
      change_and_update dst { v with state = Datatype.copy v.state }

    (* ******* TOUCH THE FOLLOWING AT YOUR OWN RISK: DANGEROUS CODE ******** *)
    let serialize p =
      debug2 2 "serializing state %s for project %s.@." Info.name p.name;
      commit p;
      let v = find p in
      { s_value = Obj.repr v.state; s_computed = v.computed }

    let unserialize p new_s = 
      debug2 2 "unserializing state %s for project %s@." Info.name p.name;
      let s = Datatype.rehash (Obj.obj new_s.s_value) in
      change_and_update p { state = s; computed = new_s.s_computed }
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

    let name = Info.name

  end

  let dummy = Computations.dummy

  let dump_dependencies = Computations.dump_dependencies

end
  
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

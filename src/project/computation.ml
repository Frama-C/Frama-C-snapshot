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

(* ************************************************************************* *)
(** {3 References} *)
(* ************************************************************************* *)

module type REF_INPUT = sig
  include Project.Datatype.S
  val default: unit -> t
end

module type REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type data
  val set: data -> unit
  val get: unit -> data
  val clear: unit -> unit
end

module Ref(Data:REF_INPUT)(Info:Signature.NAME_DPDS) = struct

  type data = Data.t

  let create () = ref (Data.default ())
  let state = ref (create ())

  include Project.Computation.Register
  (Datatype.Ref(Data))
  (struct
     type t = data ref
     let create = create
     let clear tbl = tbl := Data.default ()
     let get () = !state
     let set x = state := x
     let clear_some_projects f x = match !Data.mem_project with
       | None -> false
       | Some g -> if g f !x then begin clear x; true end else false
   end)
  (Info)

  let set v = !state := v
  let get () = !(!state)
  let clear () = !state := Data.default ()

end

module type OPTION_REF_OUTPUT = sig
  include REF_OUTPUT
  val memo: ?change:(data -> data) -> (unit -> data) -> data
  val map: (data -> data) -> data option
  val may: (data -> unit) -> unit
  val get_option : unit -> data option
end

module OptionRef
  (Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct

  type data = Data.t

  let create () = ref None
  let state = ref (create ())

  include Project.Computation.Register
  (Datatype.OptionRef(Data))
  (struct
     type t = data option ref
     let create = create
     let clear tbl = tbl := None
     let get () = !state
     let set x = state := x
     let clear_some_projects f x = match !Data.mem_project, !x with
       | None, _ | _, None -> false
       | Some g, Some v -> if g f v then begin clear x; true end else false
   end)
  (Info)

  let set v = !state := Some v
  let get () = match !(!state) with None -> raise Not_found | Some v -> v
  let get_option () = !(!state)
  let clear () = !state := None

  let memo ?change f =
    try
      let old = get () in
      Extlib.may_map
	~dft:old (fun f -> let v = f old in set v; v) change
    with Not_found ->
      let data = f () in
      set data;
      data

  let map f = Extlib.opt_map f !(!state)
  let may f = Extlib.may f !(!state)

end

module type LIST_REF_OUTPUT = sig
  type data_in_list
  include REF_OUTPUT
  val iter: (data_in_list -> unit) -> unit
  val fold_left: ('a -> data_in_list -> 'a) -> 'a -> 'a
end

module ListRef(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct

  type data_in_list = Data.t

  module Data = struct
    include Datatype.List(Data)
    let default () = []
  end

  include Ref(Data)(Info)

  let iter f = List.iter f (get ())
  let fold_left f acc = List.fold_left f acc (get ())

end

(* ************************************************************************* *)
(** {3 References on a set} *)
(* ************************************************************************* *)

module type SET = sig
  type elt
  type t
  val descr: Unmarshal.t
  val empty: t
  val singleton: elt -> t
  val is_empty: t -> bool
  val add: elt -> t -> t
  val mem: elt -> t -> bool
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (elt -> unit) -> t -> unit
end

module type SET_REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type elt
  val add: elt -> unit
  val mem: elt -> bool
  val is_empty: unit -> bool
  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  val iter: (elt -> unit) -> unit
end

module Make_SetRef
  (Set:SET)
  (Data:Project.Datatype.S with type t = Set.elt)
  (Info:Signature.NAME_DPDS) =
struct
  include Ref
    (struct
       include Datatype.Make_Set(Set)(Data)
       let default () = Set.empty
     end)
    (Info)
  type elt = Set.elt
  let apply f = f (get ())
  let is_empty () = apply Set.is_empty
  let add x = set (apply (Set.add x))
  let mem x = apply (Set.mem x)
  let fold f = apply (Set.fold f)
  let iter f = apply (Set.iter f)
end

module SetRef(Data:Project.Datatype.S) =
  Make_SetRef(Unmarshal.SetWithDescr(Data))(Data)

module Caml_hashtbl = Hashtbl

(* ************************************************************************* *)
(** {3 Hashtbl} *)
(* ************************************************************************* *)

module type HASHTBL = sig
  include Datatype.HASHTBL
  val clear: 'a t -> unit
  val find: 'a t -> key -> 'a
  val remove: 'a t -> key -> unit
  val mem: 'a t -> key -> bool
end

module type HASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
  type key
  type data
  val replace: key -> data -> unit
  val add: key -> data -> unit
  val clear: unit -> unit
  val length: unit -> int
  val iter: (key -> data -> unit) -> unit
  val fold: (key -> data -> 'a -> 'a) -> 'a -> 'a
  val memo: ?change:(data -> data) -> (key -> data) -> key -> data
  val find: key -> data
  val find_all: key -> data list
  val unsafe_find: key -> data
  val mem: key -> bool
  val remove: key -> unit
end

module Make_Hashtbl
  (H:HASHTBL)(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS) =
struct

  type key = H.key
  type data = Data.t

  let create () = H.create Info.size

  let state = ref (create ())

  include Project.Computation.Register
  (Datatype.Make_Hashtbl(H)(Data))
  (struct
     type t = data H.t
     let create = create
     let clear = H.clear
     let get () = !state
     let set x = state := x
     let clear_some_projects f h = match !Data.mem_project with
       | None -> false
       | Some g ->
	   let found = H.fold (fun k v l -> if g f v then k::l else l) h [] in
	   List.iter (H.remove h) found;
	   found <> []
   end)
  (Info)

  let clear () = H.clear !state
  let length () = H.length !state
  let replace key v = H.replace !state key v
  let add key v = H.add !state key v
  let find key = H.find !state key
  let find_all key = H.find_all !state key
  let unsafe_find key = try find key with Not_found -> assert false
  let mem key = H.mem !state key
  let remove key = H.remove !state key
  let iter f = H.iter f !state
  let fold f acc = H.fold f !state acc

  let memo ?change f key =
    try
      let old = find key in
      Extlib.may_map
	~dft:old (fun f -> let v = f old in replace key v; v) change
    with Not_found ->
      let data = f key in
      replace key data;
      data

end

module Hashtbl(Key:Hashtbl.HashedType) = Make_Hashtbl(Hashtbl.Make(Key))

(* ************************************************************************* *)
(** {3 Weak Hashtbl} *)
(* ************************************************************************* *)

module type WEAK_HASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
  type data
  val merge: data -> data
  val add: data -> unit
  val clear: unit -> unit
  val count: unit -> int
  val iter: (data -> unit) -> unit
  val fold: (data -> 'a -> 'a) -> 'a -> 'a
  val find: data -> data
  val find_all: data -> data list
  val mem: data -> bool
  val remove: data -> unit
end

module Make_WeakHashtbl
  (W:Weak.S)
  (Data: Project.Datatype.S with type t = W.data)
  (Info: Signature.NAME_SIZE_DPDS) =
struct

  type data = W.data

  let create () = W.create Info.size

  let state = ref (create ())

  include Project.Computation.Register
  (Project.Datatype.Imperative (* [JS] TODO: implement a dedicated datatype *)
     (struct
	type t = W.t
	let copy _ = assert false
	let name = Project.Datatype.extend_name "weak hashtable" Data.name
      end))
  (struct
     type t = W.t
     let create = create
     let clear = W.clear
     let get () = !state
     let set x = state := x
     let clear_some_projects f h = match !Data.mem_project with
       | None -> false
       | Some g ->
	   let found = W.fold (fun k l -> if g f k then k::l else l) h [] in
	   List.iter (W.remove h) found;
	   found <> []
   end)
  (Info)

  let merge k = W.merge !state k
  let add k = W.add !state k
  let clear () = W.clear !state
  let count () = W.count !state
  let iter f = W.iter f !state
  let fold f acc = W.fold f !state acc
  let find k = W.find !state k
  let find_all k = W.find_all !state k
  let mem k = W.mem !state k
  let remove k = W.remove !state k

end

module WeakHashtbl(Data: Project.Datatype.S)(Info: Signature.NAME_SIZE_DPDS) =
struct
  include Make_WeakHashtbl(Weak.Make(Data))(Data)(Info)
  let () = do_not_save ()
end

module HashconsTbl
  (Data: 
    sig
      type t
      val name: string
      val equal_internal: t -> t -> bool
      val hash_internal: t -> int
      val initial_values: t list
   end)
  (Info: Signature.NAME_SIZE_DPDS)
  =
struct

  (* OCaml module typing requires to name this module. Too bad :-( *)
  module W = struct
    include Weak.Make
      (struct
	 type t = Data.t
	 let equal = Data.equal_internal
	 let hash = Data.hash_internal
       end)
    let add_initial_values h =
      List.iter
	(fun vi ->
	   let _r = merge h vi in
	 (*  (* Check that we do not add the value twice, which is probably a bug
	      in the calling interface *)
	   assert (r == vi) *) ()) 
	Data.initial_values
    let create size = let h = create size in add_initial_values h; h
    let clear t = clear t; add_initial_values t
  end
  module Datatype_internal =
    Project.Datatype.Imperative
      (struct include Data let copy _ = assert false end)
  include Make_WeakHashtbl(W)(Datatype_internal)(Info)
  let pb _ = failwith Datatype.name
  let pb2 _ = pb
  let () = Datatype.register_comparable ~equal:pb2 ~hash:pb ~compare:pb2 ()
  let () = do_not_save ()

end

(* ************************************************************************* *)
(** {3 Dashtbl} *)
(* ************************************************************************* *)

module type DASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
  type key
  type data
  val add: key -> Project.Computation.t list -> data -> unit
  val replace: reset:bool -> key -> Project.Computation.t list -> data -> unit
  val memo:
    reset:bool -> (data list -> data) -> key -> Project.Computation.t list ->
    data
  val clear: reset:bool -> unit -> unit
  val remove: reset:bool -> key -> Project.Computation.t -> unit
  val remove_all: reset:bool -> key -> unit
  val filter:
    reset:bool -> (key -> Project.Computation.t -> data -> bool) -> key -> unit
  val mem: key -> bool
  val find:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    data * Project.Computation.t
  val find_data:
    ?who:Project.Computation.t list -> key -> Project.Computation.t -> data
  val find_state:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    Project.Computation.t
  val find_all_local:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    (data * Project.Computation.t) list
  val find_all_local_data:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    data list
  val find_all_local_state:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    Project.Computation.t list
  val find_all:
    ?who:Project.Computation.t list -> key ->
    (data * Project.Computation.t) list
  val find_all_data:
    ?who:Project.Computation.t list -> key -> data list
  val find_all_states:
    ?who:Project.Computation.t list -> key -> Project.Computation.t list
  val iter: (key -> Project.Computation.t -> data -> unit) -> unit
  val iter_key: (Project.Computation.t -> data -> unit) -> key -> unit
  val fold: (key -> Project.Computation.t -> data -> 'a -> 'a) -> 'a -> 'a
  val fold_key: (Project.Computation.t -> data -> 'a -> 'a) -> key -> 'a -> 'a
  val length: unit -> int
  val add_dependency: Project.Computation.t -> Project.Computation.t -> unit
end

(* Share a same reference among projects.
   The projectification is only required for good marshalling *)
let cpt = ref 0
module Cpt =
  Project.Computation.Register
    (struct
       include Datatype.Int
       let default () = 0
       let descr =
	 Unmarshal.Transform
	   (Unmarshal.Abstract,
	    fun o ->
	      let n : int = Obj.obj o in
	      cpt := Extlib.max_cpt n !cpt;
	      Obj.repr !cpt)
     end)
    (struct
       type t = int
       let create () = !cpt
       let clear _ = ()
       let get () = !cpt
       let set _ = ()
       let clear_some_projects _ _ = false
     end)
    (struct
       let name = "Computation.Cpt"
       let dependencies = []
     end)

module Make_Dashtbl
  (H:HASHTBL)(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS) =
struct

  type key = H.key

  module DataTbl = struct

    module H = Caml_hashtbl.Make(Project.Computation)

    let create = H.create

    let add tbl s v =
      try
	let l = H.find tbl s in
	H.replace tbl s (v :: l)
      with Not_found ->
	H.add tbl s [ v ]

    let replace reset tbl s v =
      begin try
	let old = H.find tbl s in
	List.iter (fun (_, l) -> reset l) old
      with Not_found ->
	()
      end;
      H.replace tbl s [ v ]
    let find tbl s = match H.find tbl s with [] -> assert false | x :: _ -> x
    let find_all = H.find
    let iter f tbl = H.iter (fun k -> List.iter (f k)) tbl
    let fold f tbl =
      H.fold (fun k l acc -> List.fold_left (fun acc v -> f k v acc) acc l) tbl
    let clear = H.clear
    let length tbl = H.fold (fun _k l acc -> List.length l + acc) tbl 0
    let remove = H.remove

  end

  type data = Data.t
  type bound_value = (Data.t * Project.Computation.t) list DataTbl.H.t

  let create () = H.create Info.size

  let state = ref (create ())

  let datatype_name = Project.Datatype.extend_name Data.name "Dashtbl.DataTbl"

  include Project.Computation.Register
  (Datatype.Make_Hashtbl
     (H)
     (Project.Datatype.Imperative
	(struct
	   let name = datatype_name
	   type t = bound_value
	   let copy _ = assert  false
	 end)))
  (struct
     type t = bound_value H.t
	 (* all operations assume that they will be also applied on the dynamic
	    computation *)
     let create = create
     let clear = H.clear
     let get () = !state
     let set x = state := x
     let clear_some_projects _ _ =
       (* TODO: not able to handle project in dashtbl yet *)
       assert (!Data.mem_project = None);
       false
   end)
  (Info)

  (* Global invariant: only one binding by key.
     So never required to call [H.find_all] *)
  let memo_tbl state key =
    try H.find state key
    with Not_found ->
      let tbl = DataTbl.create 7 in
      H.add state key tbl;
      tbl

  let unmarshal_tbl = ref (H.create 7)
  let revert_bindings = ref (Caml_hashtbl.create 7)
  let () =
    Project.register_after_global_load_hook
      (fun () ->
	 unmarshal_tbl := H.create 7;
	 revert_bindings := Caml_hashtbl.create 7)

  let marshal tbl =
    let revert_tbl = Caml_hashtbl.create (H.length tbl) in
    let fill k =
      DataTbl.iter
	(fun s (v, local) ->
	   let name s = Project.Computation.name s in
	   let localname = name local in
	   let keyname = name s in
	   try
	     let _, _, keys = Caml_hashtbl.find revert_tbl localname in
	     keys := keyname :: !keys
	   with Not_found ->
	     Caml_hashtbl.add revert_tbl localname (k, v, ref [ keyname ]))
    in
    H.iter fill tbl;
    revert_tbl

  let unmarshal revert_tbl =
    let tbl = H.create (Caml_hashtbl.length revert_tbl) in
    revert_bindings := revert_tbl;
    unmarshal_tbl := tbl;
    tbl

  let () = howto_marshal marshal unmarshal

  (* breaking mutually recursive modules which cannot be statically computed *)
  let clear_state_ref = ref (fun _ _ -> assert false)

  module Local_computation =
    Project.Computation.Dynamic
      (struct
	 let restore local =
	   try
	     let s = Project.Computation.name local in
	     let k, v, l = Caml_hashtbl.find !revert_bindings s in
	     let l = List.map Project.Computation.get_from_name !l in
	     let tbl = memo_tbl !unmarshal_tbl k in
	     List.iter (fun s -> DataTbl.add tbl s (v, local)) l;
	     (fun p -> Project.on p (fun () -> !clear_state_ref k local) ())
	   with Not_found ->
	     let s = Project.Computation.name local in
	     Format.printf "State %S should exist in project %S: %b@."
	       s
	       (Project.unique_name (Project.current ()))
	       (Caml_hashtbl.mem !revert_bindings s);
	     assert false
       end)
      (struct
	 let name = Info.name ^ "; local state"
	 let dependencies = [ self ]
       end)

  let add_dependency = Local_computation.add_dependency

  let remove_dynamic_vertices = ref []
  let () =
    Project.register_todo_before_clear
      (fun _ -> remove_dynamic_vertices := []);
    Project.register_todo_after_clear
      (fun p ->
	 let del () = List.iter (fun f -> f ()) !remove_dynamic_vertices in
	 Project.on p del ())

  let clear_state key local =
    (* Clearing the vertex erases dependencies which matters for the
       topological iteration. Thus delay these operations. *)
    remove_dynamic_vertices :=
      (fun () -> Local_computation.remove_computation ~reset:false local) ::
	!remove_dynamic_vertices;
    try
      let tbl = H.find !state key in
      DataTbl.H.iter
	(fun s l -> match l with
	 | [] ->
	     assert false
	 | _ :: _ ->
	     match
	     List.filter
	       (fun (_, l) -> not (Project.Computation.equal local l)) l
	     with
	     | [] ->
		 DataTbl.remove tbl s;
		 if DataTbl.length tbl = 0 then H.remove !state key
	     | l ->
		 DataTbl.H.replace tbl s l)
	tbl
    with Not_found ->
      (* binding already removed in this iteration *)
      ()

  let () = clear_state_ref := clear_state

  let remove ~reset key s =
    try
      let tbl = H.find !state key in
      let reset () =
	try
	  List.iter
	    (fun (_, c) -> Local_computation.remove_computation ~reset c)
	    (DataTbl.find_all tbl s)
	with Not_found ->
	  ()
      in
      match DataTbl.length tbl with
      | n when n < 1 -> assert false
      | 1 -> reset (); H.remove !state key
      | _ -> reset (); DataTbl.remove tbl s
    with Not_found ->
      ()

  let remove_all ~reset key =
    try
      let tbl = H.find !state key in
      DataTbl.iter
	(fun _ (_, s) -> Local_computation.remove_computation ~reset s)
	tbl;
      H.remove !state key
    with Not_found ->
      ()

  let clear ~reset () =
    if reset then begin
      let only =
	Project.Selection.singleton self Kind.Only_Select_Dependencies
      in
      Project.clear ~only ()
    end;
    H.clear !state

  let length () = H.length !state

  let create_local_computation key deps =
    let clear = ref (fun _ -> assert false) in
    let module L =
      Local_computation.Register
	(struct let clear p = !clear p end)
	(struct
	   let name =
	     let n = !cpt in
	     incr cpt;
	     Info.name ^ "; binding " ^ string_of_int n
	   let dependencies = deps
	 end)
    in
    clear := (fun p -> Project.on p (fun () -> clear_state key L.self) ());
    L.self

  let add_binding f key deps v =
    let full_deps = Local_computation.self :: deps in
(*    Format.printf "add_binding in %S (%t)@." Info.name
      (fun fmt -> List.iter (fun s -> Format.fprintf fmt "%S, "
			       (Project.Computation.name s)) full_deps);*)
    let tbl = memo_tbl !state key in
    let local = create_local_computation key full_deps in
    let value = v, local in
    List.iter (fun s -> f tbl s value) full_deps

  let add = add_binding DataTbl.add

  let replace ~reset key deps v =
    add_binding
      (DataTbl.replace (Local_computation.remove_computation ~reset))
      key
      deps
      v

  let find ?(who=[]) key s =
    let _, local as res = DataTbl.find (H.find !state key) s in
    List.iter (fun s -> Project.Computation.add_dependency s local) who;
    res

  let find_data ?who key s = fst (find ?who key s)
  let find_state ?who key s = snd (find ?who key s)

  let find_all_local ?(who=[]) key s =
    try
      let l = DataTbl.find_all (H.find !state key) s in
      List.iter
	(fun (_, local) ->
	   List.iter (fun s -> Project.Computation.add_dependency s local) who)
	l;
      l
    with Not_found ->
      []

  let find_all_local_data ?who key s = List.map fst (find_all_local ?who key s)
  let find_all_local_state ?who key s = List.map snd (find_all_local ?who key s)

  let tbl_iter f key =
    try DataTbl.iter f (H.find !state key) with Not_found -> ()

  let tbl_fold f key acc =
    try DataTbl.fold f (H.find !state key) acc with Not_found -> acc

  let find_all =
    (* do not get the same value twice *)
    let module S =
      Set.Make
	(struct
	   type t = Data.t * Project.Computation.t
	   let equal = (==)
	     (* cannot compare the first component but the second one is a
		valid key *)
	   let compare (_, x) (_, y) = Project.Computation.compare x y
	 end)
    in
    fun ?(who=[]) key ->
      let values = tbl_fold (fun _ -> S.add) key S.empty in
      S.fold
	(fun (_, local as res) acc ->
	   List.iter (fun s -> Project.Computation.add_dependency s local) who;
	   res :: acc)
	values
	[]

  (* optimized *)
  let find_all_data =
    (* do not get the same value twice *)
    let module S =
      Set.Make
	(struct
	   type t = Data.t * Project.Computation.t
	   let equal = (==)
	     (* cannot compare the first component but the second one is a
		valid key *)
	   let compare (_, x) (_, y) = Project.Computation.compare x y
	 end)
    in
    fun ?(who=[]) key ->
      let values = tbl_fold (fun _ -> S.add) key S.empty in
      S.fold
	(fun (v, local) acc ->
	   List.iter (fun s -> Project.Computation.add_dependency s local) who;
	   v :: acc)
	values
	[]

  (* optimized *)
  let find_all_states =
    (* do not get the same state twice *)
    let module S =
      Set.Make
	(struct
	   type t = Project.Computation.t
	   let equal = (==)
	   let compare x y = Project.Computation.compare x y
	 end)
    in
    fun ?(who=[]) key ->
      let selfs = tbl_fold (fun _ (_, s) -> S.add s) key S.empty in
      S.fold
	(fun local acc ->
	   List.iter (fun s -> Project.Computation.add_dependency s local) who;
	   local :: acc)
	selfs
	[]

  let mem key = H.mem !state key

  let iter_key f = tbl_iter (fun s (v, _) -> f s v)
  let fold_key f = tbl_fold (fun s (v, _) -> f s v)

  let iter f = H.iter (fun k -> DataTbl.iter (fun s (v, _) -> f k s v)) !state

  let fold f acc =
    H.fold (fun k -> DataTbl.fold (fun s (v, _) -> f k s v)) !state acc

  let memo ~reset f key deps =
    let olds =
      List.fold_left
	(fun acc s -> try find_data key s :: acc with Not_found -> acc) [] deps
    in
    let data = f olds in
    replace ~reset key deps data;
    data

  let filter ~reset f stmt =
    let fold f acc key = DataTbl.fold f (H.find !state key) acc in
    try
      let tbl = H.find !state stmt in
      let keep =
	fold
	  (fun s (v, state as x) acc ->
	     if f stmt s v then (s, x) :: acc
	     else begin
	       Local_computation.remove_computation ~reset state;
	       acc
	     end)
	  []
	  stmt
      in
      DataTbl.clear tbl;
      List.iter (fun (s, v) -> DataTbl.add tbl s v) keep
    with Not_found ->
      ()

end

module Dashtbl(Key:Caml_hashtbl.HashedType) =
  Make_Dashtbl(Caml_hashtbl.Make(Key))

(* ************************************************************************* *)
(** {3 Queue} *)
(* ************************************************************************* *)

module type QUEUE = sig
  type elt
  val add: elt -> unit
  val iter: (elt -> unit) -> unit
  val is_empty: unit -> bool
end

module Queue(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct

  type elt = Data.t

  let state = ref (Queue.create ())

  include Project.Computation.Register
  (Datatype.Queue(Data))
  (struct
     type t = elt Queue.t
     let create = Queue.create
     let clear = Queue.clear
     let get () = !state
     let set x = state := x
     let clear_some_projects f q = match !Data.mem_project with
       | None -> false
       | Some g ->
	   (* cannot remove a single element from a queue *)
	   try Queue.iter (fun x -> if g f x then raise Exit) q; false
	   with Exit -> clear q; true
   end)
  (Info)

  let add x = Queue.add x !state
  let iter f = Queue.iter f !state
  let is_empty () = Queue.is_empty !state

end

(* ************************************************************************* *)
(** {3 Apply Once} *)
(* ************************************************************************* *)

let apply_once name dep f =
  let module First =
    Ref
      (struct include Datatype.Bool let default () = true end)
      (struct let dependencies = dep let name = name end)
  in
  (fun () ->
     if First.get () then begin
       First.set false;
       try
	 f ();
	 assert (First.get () = false)
       with exn ->
	 First.set true;
	 raise exn
     end),
  First.self

(* ************************************************************************* *)
(** {3 Project itself} *)
(* ************************************************************************* *)

module Project(Info:Signature.NAME_DPDS) =
  Ref
    (struct include Datatype.Project let default () = Project.dummy end)
    (Info)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

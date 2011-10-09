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

module type S = sig
  include Datatype.S
  type key
  type data
  val create: int -> t
  val add: t -> string -> key -> State.t list -> data -> unit
  val replace: reset:bool -> t -> string -> key -> State.t list -> data -> unit
  val memo:
    reset:bool -> (data list -> data) -> t -> string -> key -> State.t list ->
    data
  val clear: reset:bool -> t -> unit
  val remove: reset:bool -> t -> key -> State.t -> unit
  val remove_all: reset:bool -> t -> key -> unit
  val filter:
    reset:bool -> (key -> State.t option -> data -> bool) -> t -> key -> unit
  val mem: t -> key -> bool
  val is_local: t -> State.t -> bool
  val find: ?who:State.t list -> t -> key -> State.t -> data * State.t
  val find_key: t -> State.t -> (key * State.t) list
  val find_data: ?who:State.t list -> t -> key -> State.t -> data
  val find_state: ?who:State.t list -> t -> key -> State.t -> State.t
  val find_all_local:
    ?who:State.t list -> t -> key -> State.t -> (data * State.t) list
  val find_all_local_data:
    ?who:State.t list -> t -> key -> State.t -> data list
  val find_all_local_states:
    ?who:State.t list -> t -> key -> State.t -> State.t list
  val find_all: ?who:State.t list -> t -> key -> (data * State.t) list
  val find_all_data: ?who:State.t list -> t -> key -> data list
  val find_all_states: ?who:State.t list -> t -> key -> State.t list
  val iter: (key -> State.t option -> data * State.t -> unit) -> t -> unit
  val iter_key: (State.t option -> data * State.t  -> unit) -> t -> key -> unit
  val fold:
    (key -> State.t option -> data * State.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_key:
    (State.t option -> data * State.t -> 'a -> 'a) -> t -> key -> 'a -> 'a
  val length: t -> int
  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)
end

module type Graph = sig
  val create_and_add_state:
    clear:(Project.t -> unit) -> name:string -> deps:State.t list -> State.t
  val add_state: State.t -> unit
  val remove_state: reset:bool -> State.t -> unit
  val self: State.t ref
  val internal_kind: State.kind
end

module type Key = sig
  include Datatype.S_with_collections
  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)
  val equal_marshaled: marshaled -> marshaled -> bool
  val hash_marshaled: marshaled -> int
end

module Default_key_marshaler(K: Datatype.S_with_collections) = struct
  include K
  type marshaled = t
  let marshaler = Datatype.identity, Datatype.identity
  let equal_marshaled = equal
  let hash_marshaled = hash
end

module type Data = sig
  include Datatype.S
  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)
end

module Default_data_marshaler(D: Datatype.S) = struct
  include D
  type marshaled = D.t
  let marshaler = Datatype.identity, Datatype.identity
end

module Make(G:Graph)(K:Key)(D:Data)(Info: sig val name: string end) = struct

  module State_tbl = Hashtbl.Make(State)

  module Internal_tbl: sig
    type t
    val create: int -> t
    val clear: t -> unit
    val is_empty: t -> bool
    val is_singleton: t -> bool
    val add: t -> State.t -> (D.t * State.t) -> unit
    val replace:
      on_clear:(State.t -> unit) -> t -> State.t -> (D.t * State.t) -> unit
    val remove: t -> State.t -> unit
    val remove_single: t -> State.t -> State.t -> unit
    val find: t -> State.t -> D.t * State.t
    val find_all: t -> State.t -> (D.t * State.t) list
    val iter: (State.t -> D.t * State.t -> unit) -> t -> unit
    val fold: (State.t -> D.t * State.t -> 'a -> 'a) -> t -> 'a -> 'a
  end = struct

    type t = (D.t * State.t) list State_tbl.t

    let create x = State_tbl.create x
    let clear = State_tbl.clear

    let add tbl s v =
      try
        let l = State_tbl.find tbl s in
        State_tbl.replace tbl s (v :: l)
      with Not_found ->
        State_tbl.add tbl s [ v ]

    let replace ~on_clear tbl s v =
      begin try
        let old = State_tbl.find tbl s in
        List.iter (fun (_, s') -> on_clear s') old
      with Not_found ->
        ()
      end;
      State_tbl.replace tbl s [ v ]

    let remove = State_tbl.remove

    let remove_single tbl key s =
      try
        match State_tbl.find tbl key with
        | [] ->
          assert false
        | _ :: _ as l ->
          match List.filter (fun (_, s') -> not (State.equal s s')) l with
          | [] -> State_tbl.remove tbl key;
          | _ :: _ as l -> State_tbl.replace tbl key l
      with Not_found ->
        ()

    let find tbl s =
      match State_tbl.find tbl s with
      | [] -> assert false
      | v :: _ -> v

    let find_all = State_tbl.find

    let iter f = State_tbl.iter (fun s -> List.iter (f s))

    let fold f tbl acc =
      State_tbl.fold
        (fun s l acc -> List.fold_left (fun acc v -> f s v acc) acc l)
        tbl
        acc

    let is_empty tbl =
      try
        State_tbl.iter (fun _ _ -> raise Exit) tbl;
        true;
      with Exit ->
        false

    let is_singleton tbl =
      try
        State_tbl.fold
          (fun _ _ second -> if second then raise Exit else true) tbl false;
      with Exit ->
        false

  end

  type key = K.t
  type data = D.t
  type tbl =
      { h: Internal_tbl.t K.Hashtbl.t;
        inverse: (key * State.t) list State_tbl.t }

  include Datatype.Make
        (struct
          include Datatype.Undefined
          type t = tbl
          let name = Info.name
          let reprs =
            [ { h = K.Hashtbl.create 0; inverse = State_tbl.create 0 } ]
         end)

  (* Global invariant: only one binding by key.
     So never required to call [K.Hashtbl.find_all] *)

  let create n = { h = K.Hashtbl.create n; inverse = State_tbl.create n }

  let memoize_internal_tbl t key =
    let tbl = t.h in
    try K.Hashtbl.find tbl key
    with Not_found ->
      let internal = Internal_tbl.create 7 in
      K.Hashtbl.add tbl key internal;
      internal

  (* Clearing a state erases dependencies which matters for the
     topological iteration. Thus postpone these operations. *)
  module States_to_be_removed : sig val add: (unit -> unit) -> unit end =
  struct

    let todo_list = ref []

    let () = Project.register_todo_before_clear (fun _ -> todo_list := [])
    let () =
      Project.register_todo_after_clear
        (fun p ->
          let del () = List.iter (fun f -> f ()) !todo_list in
          Project.on p del ())

    let add f = todo_list := f :: !todo_list

  end

  let clear_state t s =
    States_to_be_removed.add (fun () -> G.remove_state ~reset:false s);
    try
      let inverse = t.inverse in
      let h = t.h in
      let keys = State_tbl.find inverse s in
      State_tbl.remove inverse s;
      let clear key s' =
        try
          let internal = K.Hashtbl.find h key in
          Internal_tbl.remove_single internal s' s;
          if Internal_tbl.is_empty internal then K.Hashtbl.remove h key
        with Not_found ->
          assert false
      in
      List.iter (fun (key, s') -> clear key s') keys
    with Not_found ->
      (* binding already removed in this iteration *)
      ()

  let clear_state_on t local p =
    Project.on p (fun () -> clear_state t local) ()

  let single_remove ~reset t s =
    G.remove_state ~reset s;
    State_tbl.remove t.inverse s

  let remove ~reset t key s =
    try
      let h = t.h in
      let tbl = K.Hashtbl.find h key in
      (try
         let bindings = Internal_tbl.find_all tbl s in
         let del = single_remove ~reset t in
         List.iter (fun (_, c) -> del c) bindings
       with Not_found ->
         ());
      if Internal_tbl.is_singleton tbl then K.Hashtbl.remove h key
      else Internal_tbl.remove tbl s
    with Not_found ->
      ()

  let remove_all ~reset t key =
    try
      let h = t.h in
      let tbl = K.Hashtbl.find h key in
      let del = single_remove ~reset t in
      Internal_tbl.iter (fun _ (_, s) -> del s) tbl;
      K.Hashtbl.remove h key
    with Not_found ->
      ()

  let clear ~reset t =
    if reset then
      Project.clear
        ~selection:(State_selection.Dynamic.with_dependencies !G.self) ();
    K.Hashtbl.clear t.h;
    State_tbl.clear t.inverse

  let length t = K.Hashtbl.length t.h

  let create_local_state t name deps =
    let clear = ref (fun _ -> assert false) in
    let state = G.create_and_add_state ~clear:(fun p -> !clear p) ~name ~deps in
    clear := clear_state_on t state;
    state

  let add_binding f t name key deps v =
    let tbl = memoize_internal_tbl t key in
    let local = create_local_state t name deps in
(*    Format.printf "add binding %S (%S) in %S@\ndependencies: %t@."
      (State.get_name local)
      (State.get_unique_name local)
      (State.get_name !G.self)
      (fun fmt ->
        List.iter (fun s -> Format.fprintf fmt "%S, " (State.get_name s)) deps);*)
    let value = v, local in
    let full_deps = match deps with [] -> [ State.dummy ] | _ :: _ -> deps in
    let inverse_binders =
      List.fold_left (fun acc s -> f tbl s value; (key, s) :: acc) [] full_deps
    in
    State_tbl.add t.inverse local inverse_binders

  let add = add_binding Internal_tbl.add

  let replace ~reset t name key deps v =
    let on_clear s =
      G.remove_state ~reset s;
      State_tbl.remove t.inverse s
    in
    add_binding (Internal_tbl.replace ~on_clear) t name key deps v

  let find ?(who=[]) t key s =
    let _, local as res = Internal_tbl.find (K.Hashtbl.find t.h key) s in
    State_dependency_graph.Dynamic.add_codependencies ~onto:local who;
    res

  let find_key t = State_tbl.find t.inverse
  let find_data ?who t key s = fst (find ?who t key s)
  let find_state ?who t key s = snd (find ?who t key s)

  let find_all_local ?(who=[]) t key s =
    try
      let l = Internal_tbl.find_all (K.Hashtbl.find t.h key) s in
      List.iter
        (fun (_, local) ->
           State_dependency_graph.Dynamic.add_codependencies ~onto:local who)
        l;
      l
    with Not_found ->
      []

  let find_all_local_data ?who t key s =
    List.map fst (find_all_local ?who t key s)

  let find_all_local_states ?who t key s =
    List.map snd (find_all_local ?who t key s)

  let tbl_iter f t key =
    try Internal_tbl.iter f (K.Hashtbl.find t.h key) with Not_found -> ()

  let tbl_fold f t key acc =
    try Internal_tbl.fold f (K.Hashtbl.find t.h key) acc with Not_found -> acc

  let find_all =
    (* do not get the same value twice *)
    let module S =
      Set.Make
        (struct
           type t = D.t * State.t
           let equal = (==)
             (* cannot compare the first component but the second one is a
                valid key *)
           let compare (_, x) (_, y) = State.compare x y
         end)
    in
    fun ?(who=[]) t key ->
      let values = tbl_fold (fun _ -> S.add) t key S.empty in
      S.fold
        (fun (_, local as res) acc ->
           State_dependency_graph.Dynamic.add_codependencies ~onto:local who;
           res :: acc)
        values
        []

  (* optimized *)
  let find_all_data =
    (* do not get the same value twice *)
    let module S =
      Set.Make
        (struct
           type t = D.t * State.t
           let equal = (==)
             (* cannot compare the first component but the second one is a
                valid key *)
           let compare (_, x) (_, y) = State.compare x y
         end)
    in
    fun ?(who=[]) t key ->
      let values = tbl_fold (fun _ -> S.add) t key S.empty in
      S.fold
        (fun (v, local) acc ->
           State_dependency_graph.Dynamic.add_codependencies ~onto:local who;
           v :: acc)
        values
        []

  (* optimized *)
  let find_all_states =
    (* do not get the same state twice *)
    let module S = Set.Make(State) in
    fun ?(who=[]) t key ->
      let selfs = tbl_fold (fun _ (_, s) -> S.add s) t key S.empty in
      S.fold
        (fun local acc ->
           State_dependency_graph.Dynamic.add_codependencies ~onto:local who;
           local :: acc)
        selfs
        []

  let mem t key = K.Hashtbl.mem t.h key
  let is_local t s = State_tbl.mem t.inverse s

  let iter_key f =
    tbl_iter
      (fun s -> let s = if State.is_dummy s then None else Some s in f s)

  let fold_key f =
    tbl_fold
      (fun s -> let s = if State.is_dummy s then None else Some s in f s)

  let iter f t =
    K.Hashtbl.iter
      (fun k ->
        Internal_tbl.iter
          (fun s v ->
            let s = if State.is_dummy s then None else Some s in
            f k s v))
      t.h

  let fold f t acc =
    K.Hashtbl.fold
      (fun k ->
        Internal_tbl.fold
          (fun s v ->
            let s = if State.is_dummy s then None else Some s in
            f k s v))
      t.h acc

  let memo ~reset f t name key deps =
    let olds =
      List.fold_left
        (fun acc s -> try find_data t key s :: acc with Not_found -> acc)
        []
        deps
    in
    let data = f olds in
    replace ~reset t name key deps data;
    data

  let filter =
    let module S = Set.Make(State) in
    fun ~reset f t key ->
      try
        let h = t.h in
        let tbl = K.Hashtbl.find h key in
        let keep, to_be_delete =
          Internal_tbl.fold
            (fun s (v, state as x) (keep, delete) ->
              let s = if State.is_dummy s then None else Some s in
              if f key s v then (s, x) :: keep, delete
              else begin
(*              Format.printf "FILTERING %S (key is %S)@."
                  (State.get_name state)
                  (State.get_name s);*)
                keep, S.add state delete
              end)
            tbl
            ([], S.empty)
        in
        S.iter (single_remove ~reset t) to_be_delete;
        match keep with
        | [] -> K.Hashtbl.remove h key
        | _ :: _ ->
          Internal_tbl.clear tbl;
          List.iter
            (fun (s, v) ->
              let s = match s with None -> State.dummy | Some s -> s in
              Internal_tbl.add tbl s v)
            keep;
      with Not_found ->
        ()

  module H =
    Hashtbl.Make
      (struct
        type t = K.marshaled
        let equal = K.equal_marshaled
        let hash = K.hash_marshaled
       end)

  type marshaled_value =
      { data: D.marshaled;
        key_name: string;
        key_cluster: string option;
        data_uname: string;
        data_name: string;
        data_cluster: string option }

  type marshaled =
      (string, marshaled_value) Hashtbl.t H.t

  let key_marshal, key_unmarshal = K.marshaler
  let data_marshal, data_unmarshal = D.marshaler

  let marshal (dash: t) =
    let h : marshaled = H.create 97 in
    K.Hashtbl.iter
      (fun key tbl ->
        let tbl' = Hashtbl.create 7 in
        Internal_tbl.iter
          (fun s (d, s') ->
            Hashtbl.add
              tbl'
              (State.get_unique_name s)
              { data = data_marshal d;
                key_name = State.get_name s;
                key_cluster = State.Cluster.name s;
                data_uname = State.get_unique_name s';
                data_name = State.get_name s';
                data_cluster = State.Cluster.name s' })
          tbl;
        H.add h (key_marshal key) tbl')
      dash.h;
    h

  let unmarshal (h: marshaled) =
    let dash : t = create 7 in
    let dash_h = dash.h in
    let inverse = dash.inverse in
    H.iter
      (fun key tbl ->
        let tbl' = Internal_tbl.create 7 in
        let k = key_unmarshal key in
        let update s k =
          (*      Format.printf "updating %S@." (State.get_unique_name s);*)
          State.update_unusable s k (clear_state_on dash s);
          G.add_state s;
        in
        Hashtbl.iter
          (fun
            unique_name
            { data = d;
              key_name = name;
              key_cluster = c;
              data_uname = unique_name';
              data_name = name';
              data_cluster = c' }
          ->
            let s =
              if unique_name = State.dummy_unique_name then State.dummy
              else
                try State.get unique_name
                with State.Unknown -> State.unusable ~name unique_name
            in
            State.Cluster.unmarshal c s;
            let s' =
              assert (unique_name' <> State.dummy_unique_name);
              try
                let s' = State.get unique_name' in
                if not (State.is_usable s') then update s' G.internal_kind;
                s'
              with State.Unknown ->
                let s' = State.unusable ~name:name' unique_name' in
                update s' G.internal_kind;
                s'
            in
            State.Cluster.unmarshal c' s';
            let d = data_unmarshal d in
            Internal_tbl.add tbl' s (d, s');
            try
              let l = State_tbl.find inverse s' in
              State_tbl.replace inverse s' ((k, s) :: l)
            with Not_found ->
              State_tbl.add inverse s' [ k, s ])
          tbl;
        K.Hashtbl.add dash_h k tbl')
      h;
    dash

  let marshaler = marshal, unmarshal

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

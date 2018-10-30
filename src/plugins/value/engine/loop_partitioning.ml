(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open Bottom.Type
open State_partitioning

let rec list_merge_two_last f = function
  | ([] | [_]) as l -> l
  | [ a ; b ] -> [ f a b ]
  | x :: l -> x :: list_merge_two_last f l


module Make (Domain : Domain) (Param : Param) =
struct
  include Param
  module Partition = Partitioning.Make (Domain)

  (* Partitioning tree for set of states.

     Each internal node [Loop] corresponds to a partitioning over the iterations
     of a loop: states corresponding to different iterations of a loop are
     stored separately in a list. The list is sorted by increasing iterations:
     the first element gathers states for the first iteration, and so on. When
     reaching the unrolling limit of a loop, the last element gathers states for
     all higher iterations.

     The root of the tree is the innermost loop. As we descend towards the
     leaves, we move to the outer loops. The [Empty] constructor can be put in
     place of any empty subtree.

     Functions (such as [map2] or [update]) that iterate simultaneously over two
     trees assume that they have the same shape, i.e. that they partition the
     same loops in the same order. However, an [Empty] node can match any shape:
     when iterating over an empty subtree, the shape assertion will always hold.
     Moreover, the lists for a same [Loop] node may have different lengths;
     [Empty] nodes are then added to the shorter list to match the other one. *)
  type 'a tree =
    | Empty
    | Leaf of 'a
    | Loop of loop * ('a tree) list

  module Tree =
  struct
    let rec iter (f : 'a -> unit) (t : 'a tree) : unit =
      match t with
      | Empty -> ()
      | Leaf p -> f p
      | Loop (_i,l) -> List.iter (iter f) l

    let rec map (f : 'a -> 'b) (t : 'a tree) : 'b tree =
      match t with
      | Empty -> Empty
      | Leaf p -> Leaf (f p)
      | Loop (i,l) -> Loop (i, List.map (map f) l)

    let rec fold (f : 'a -> 'b -> 'b) (t : 'a tree) (x : 'b) : 'b =
      match t with
      | Empty -> x
      | Leaf p -> f p x
      | Loop (_i,l) -> List.fold_left (fun x t -> fold f t x) x l

    let exists (f : 'a -> bool) (t : 'a tree) : bool =
      try
        iter (fun x -> if f x then raise Exit) t;
        false
      with Exit ->
        true

    let list_decons : 'a tree list -> 'a tree * 'a tree list = function
      | [] -> Empty, []
      | h :: t -> h, t

    let rec list_map2 (f : 'a tree -> 'b tree -> 'c tree)
        (l1 : 'a tree list) (l2 : 'b tree list) : 'c tree list =
      if l1 = [] && l2 = []
      then []
      else
        let h1, t1 = list_decons l1
        and h2, t2 = list_decons l2 in
        f h1 h2 :: list_map2 f t1 t2

    let rec map2 (f : 'a option -> 'b option -> 'c option)
        (t1 : 'a tree) (t2 : 'b tree) : 'c tree =
      let to_leaf = function
        | Some p -> Leaf p
        | None -> Empty
      in
      match t1, t2 with
      | Empty, Empty -> Empty
      | Loop (i1, l1), Loop (i2, l2) ->
        assert (i1 == i2);
        Loop (i1, list_map2 (map2 f) l1 l2)
      | Loop (i, l1), Empty ->
        Loop (i, list_map2 (map2 f) l1 [])
      | Empty, Loop (i, l2) ->
        Loop (i, list_map2 (map2 f) [] l2)
      | Leaf p1, Leaf p2 ->
        to_leaf (f (Some p1) (Some p2))
      | Leaf p1, Empty ->
        to_leaf (f (Some p1) None)
      | Empty, Leaf p2 ->
        to_leaf (f None (Some p2))
      | Loop _, Leaf _
      | Leaf _, Loop _ -> assert false

    (* Creates a tree from t1 updated with t2. When there is an empty node in
       t1, creates a new one from ~def. When there is an empty node in t2,
       keep the original node from t1. *)
    let rec update ~(def : unit -> 'a) (f : 'a -> 'b -> unit)
        (t1 : 'a tree) (t2 : 'b tree) : 'a tree =
      match t2 with
      | Empty -> t1
      | Loop (i, l2) ->
        let l1 = match t1 with
          | Empty -> []
          | Loop (i', l1) -> assert (i' == i); l1
          | _ -> assert false
        in
        Loop (i, list_map2 (update ~def f) l1 l2)
      | Leaf p2 ->
        let p1 = match t1 with
          | Empty -> def ()
          | Leaf p -> p
          | Loop _ -> assert false
        in
        f p1 p2;
        Leaf p1

    let rec merge merge_leaf t1 t2 =
      match t1, t2 with
      | _, Empty -> t1
      | Empty, _ -> t2
      | Leaf p1, Leaf p2 -> Leaf (merge_leaf p1 p2)
      | Loop (i, l1), Loop (i', l2) ->
        assert (i == i');
        Loop (i, list_map2 (merge merge_leaf) l1 l2)
      | Leaf _, Loop _ | Loop _, Leaf _ -> assert false

    let pretty (printer : Format.formatter -> 'a -> unit)
        (fmt : Format.formatter) (t : 'a tree) : unit =
      let rec pretty indent t =
        Format.fprintf fmt "%*s" (indent * 2) "";
        match t with
        | Empty ->
          Format.fprintf fmt "Empty@."
        | Leaf leaf ->
          Format.fprintf fmt "Leaf @[%a@]@." printer leaf
        | Loop (_,l) ->
          Format.fprintf fmt "Loop@.";
          List.iter (pretty (indent+1)) l
      in
      pretty 0 t
  end


  type state = Domain.t

  (** Stores contains states which have already been propagated at a control
      point.

      They are organized as a partitioning tree whose leaves are sets of states.
      These sets are themselves partitioned into two parts.

      - Eternal states are states which once added will stay in the store
        forever.
      - Once the slevel has been exceeded, new propagated states are not
        considered eternal; instead, they are joined into an ultimate state of
        the corresponding leaf in the partition tree.

      The store also keeps information about the vertex it is associated with:
      - the [size_limit] of eternal states (slevel) for this store;
      - the presence of a [merge] operation on this store;
      - the stmt, if applicable, starting at this store;
      - a table indexing all the states of this store to speed up the inclusion
        tests. *)
  type store = {
    size_limit : int;
    merge : bool;
    store_stmt : stmt option;
    store_table : Partition.t;
    mutable store_tree : store_leaf tree;
    mutable store_size : int;
  }
  and store_leaf = {
    mutable eternal_states : state list;
    mutable ultimate_state : state or_bottom;
  }

  (** Propagations, like stores, are organized as partitioning trees, which
      help to keep track of how many iterations of each loop have been
      necessary to reach these states. At the leaf nodes of these trees, we only
      keep a list of propagated states. *)
  type propagation = {
    mutable propagation_tree : propagation_leaf tree;
  }
  and propagation_leaf = {
    mutable states : state list;
  }

  (** For this implementation, the [shadow] only remembers the number of
      already propagated states on an edge. *)
  type shadow = {
    mutable transfered_states : int;
  }

  (** The widening object allows widenings to be applied independently on each
      leaf of the partitioning tree. To control widening, each leaf contains
      - the widening counter telling how many iterations must be done before
        widening
      - the previous widening state, obtained after the last widening
      - the previous iteration state, obtained the last time we went at this
        point, even if there was no widening at this previous iteration. *)
  type widening = {
    widening_stmt : stmt;
    mutable widening_tree : widening_leaf tree;
  }
  and widening_leaf = {
    mutable widened_state : state or_bottom;
    mutable previous_state : state or_bottom;
    mutable widening_counter : int;
  }


  (** Tree manipulation *)

  let empty_store_leaf () : store_leaf =
    {
      eternal_states = [];
      ultimate_state = `Bottom
    }

  let empty_widening_leaf () : widening_leaf =
    {
      widened_state = `Bottom;
      previous_state = `Bottom;
      widening_counter = widening_delay;
    }

  let is_empty_propagation_tree t =
    not (Tree.exists (fun l -> l.states <> []) t)

  let join_propagation_tree t1 t2 =
    let merge_leaf p1 p2 = { states = p2.states @ p1.states } in
    Tree.merge merge_leaf t1 t2

  (* Constructors *)

  let empty_store ~(stmt : stmt option) : store =
    let size_limit, merge = match stmt with
      | None -> max_int, false
      | Some stmt -> slevel stmt, merge stmt
    in
    {
      size_limit; merge;
      store_stmt = stmt;
      store_table = Partition.empty ();
      store_tree = Empty;
      store_size = 0;
    }

  let empty_propagation () : propagation =
    { propagation_tree = Empty }

  let empty_shadow () : shadow =
    { transfered_states = 0 }

  let empty_widening ~(stmt : stmt option) : widening =
    {
      widening_stmt = Extlib.opt_conv Cil.invalidStmt stmt;
      widening_tree = Empty;
    }

  let initial_propagation (states : state list) : propagation =
    { propagation_tree = Leaf { states } }

  (* Pretty printing *)

  let pretty_store_leaf (fmt : Format.formatter) (s : store_leaf) : unit =
    List.iter (Domain.pretty fmt) s.eternal_states;
    match s.ultimate_state with
    | `Value state -> Domain.pretty fmt state
    | `Bottom -> ()

  let pretty_store (fmt : Format.formatter) (s : store) : unit =
    Tree.iter (pretty_store_leaf fmt) s.store_tree

  let pretty_propagation_leaf (fmt : Format.formatter) (p : propagation_leaf) =
    List.iter (Domain.pretty fmt) p.states

  let _pretty_propagation_tree  (fmt : Format.formatter) (p : propagation) =
    Tree.pretty pretty_propagation_leaf fmt p.propagation_tree

  let pretty_propagation (fmt : Format.formatter) (p : propagation) =
    Tree.iter (pretty_propagation_leaf fmt) p.propagation_tree

  (* Accessors *)

  let expanded (s : store) : state list =
    let collect s acc =
      Bottom.add_to_list s.ultimate_state (s.eternal_states @ acc)
    in
    Tree.fold collect s.store_tree []

  let smashed (s : store) : state or_bottom =
    let l = expanded s in
    Domain.join_list l

  let is_empty_store (s : store) : bool =
    expanded s = []

  let is_empty_propagation (p : propagation) : bool =
    is_empty_propagation_tree p.propagation_tree

  let is_empty_shadow (s : shadow) : bool =
    s.transfered_states = 0

  let store_size (s : store) : int =
    s.store_size

  let propagation_size (p : propagation) : int =
    Tree.fold (fun p acc -> acc + List.length p.states) p.propagation_tree 0

  (* Reset state (for hierchical convergence) *)

  let reset_store (s : store) : unit =
    Tree.iter (fun s -> s.ultimate_state <- `Bottom) s.store_tree

  let reset_propagation (p : propagation) : unit =
    p.propagation_tree <- Empty

  let reset_shadow (_s : shadow) : unit = ()

  let reset_widening (w : widening) : unit =
    let reset w =
      w.widened_state <- `Bottom;
      w.previous_state <- `Bottom;
      w.widening_counter <- widening_delay
    in
    Tree.iter reset w.widening_tree

  (* Operators *)

  let clear_propagation (p : propagation) : unit =
    p.propagation_tree <- Empty

  let transfer (f : state list -> state list) (p : propagation) : unit =
    let f p =
      let states = if p.states = [] then [] else f p.states in
      { states }
    in
    p.propagation_tree <- Tree.map f p.propagation_tree

  let merge ~(into : propagation) (source : propagation) : unit =
    into.propagation_tree <- join_propagation_tree into.propagation_tree
        source.propagation_tree

  let join (sources : (propagation*shadow) list) (dest : store) : propagation =
    (* Create a new propagation *)
    let p = empty_propagation () in
    (* Merge incomming propagations and update counters *)
    let update acc (p,s) =
      let size = propagation_size p in
      s.transfered_states <- s.transfered_states + size;
      (* The store size is incremented by the number of incoming states, even
         if those states are not distinct or already propagated. This is for
         compatibility with the original definition of slevel. *)
      dest.store_size <- dest.store_size + size;
      join_propagation_tree p.propagation_tree acc
    in
    p.propagation_tree <- List.fold_left update Empty sources;
    (* Merge / Merge after loop : join leafs *)
    if dest.merge then begin
      let smash p =
        { states = Bottom.to_list (Domain.join_list p.states) }
      in
      p.propagation_tree <- Tree.map smash p.propagation_tree;
    end;
    (* Filter states already propagated, on statement vertices only. *)
    if dest.store_stmt <> None then begin
      let update p =
        { states = Partition.merge_set_return_new p.states dest.store_table }
      in
      p.propagation_tree <- Tree.map update p.propagation_tree;
    end;
    (* Do we exceed slevel ? *)
    if dest.store_size <= dest.size_limit then begin
      (* No, add the incoming states to the list of eternal states *)
      let update s p =
        s.eternal_states <- p.states @ s.eternal_states
      in
      dest.store_tree <- Tree.update ~def:empty_store_leaf update
          dest.store_tree p.propagation_tree;
    end else begin
      (* Yes, add the incoming states as ultimate states *)
      let transcend s p =
        let state' = Domain.join_list p.states in
        if Bottom.is_included Domain.is_included state' s.ultimate_state then
          p.states <- []
        else begin
          begin match dest.store_stmt with
            | Some {skind=Cil_types.Loop _} when s.ultimate_state <> `Bottom ->
              Value_parameters.feedback ~level:1 ~once:true ~current:true
                "starting to merge loop iterations"
            | _ -> ()
          end;
          (* Join the new states in the previous ultimate state, and propagate
             the result. We could propagate the new states only, but this is
             kept for compatibility reasons. *)
          let state' = Bottom.join Domain.join s.ultimate_state state' in
          s.ultimate_state <- state';
          p.states <- Bottom.to_list state'
        end
      in
      dest.store_tree <- Tree.update ~def:empty_store_leaf transcend
          dest.store_tree p.propagation_tree
    end;
    p

  let widen (s : store) (w : widening) (p : propagation) : bool =
    let stmt = w.widening_stmt in
    (* Pair the store and propagation tree *)
    let pair s p =
      match s, p with
      | Some s, Some p -> Some (s, p)
      | None, Some _ -> assert false (* The propagated states are not stored! *)
      | _, None -> None
    in
    let tree = Tree.map2 pair s.store_tree p.propagation_tree in
    (* Apply widening to each leaf *)
    let widen w (s,p) =
      (* If the ultimate stored state is bottom but the propagation is not, we
         did not consume all the slevel yet and we don't want to do anything,
         in particular, we do not want to decrease the widening counter. *)
      if not (Bottom.is_bottom s.ultimate_state) then begin
        let current_state = s.ultimate_state in
        let previous_state = w.previous_state in
        w.previous_state <- current_state;
        w.widening_counter <- w.widening_counter - 1;
        match previous_state, current_state with
        | _, `Bottom | `Bottom, _ -> ()
        | `Value prev, `Value curr ->
          if Domain.is_included curr prev then
            p.states <- []
          else if w.widening_counter < 0 then begin
            Value_parameters.feedback ~level:1 ~once:true ~current:true
              ~dkey:Value_parameters.dkey_widening
              "applying a widening at this point";
            (* We join the previous widening state with the previous iteration
               state so as to allow the intermediate(s) iteration(s) (between
               two widenings) to stabilize at least a part of the state. *)
            let prev = match w.widened_state with
              | `Value v -> Domain.join prev v
              | `Bottom -> prev
            in
            let next = Domain.widen kf stmt prev (Domain.join prev curr) in
            p.states <- [next];
            w.previous_state <- `Value next;
            w.widened_state <- `Value next;
            w.widening_counter <- widening_period - 1
          end
      end
    in
    w.widening_tree <-
      Tree.update ~def:empty_widening_leaf widen w.widening_tree tree;
    (* Is there still something to propagate ? *)
    is_empty_propagation p


  let enter_loop (p : propagation) (i : loop) =
    p.propagation_tree <- Loop (i, [p.propagation_tree])

  let leave_loop (p : propagation) (i : loop) =
    p.propagation_tree <-
      match p.propagation_tree with
      | Empty -> Empty
      | Loop (i', l) ->
        assert (i == i');
        List.fold_left join_propagation_tree Empty l
      | Leaf _ -> assert false

  let next_loop_iteration (p : propagation) (i : loop) =
    p.propagation_tree <-
      match p.propagation_tree with
      | Empty -> Empty
      | Loop (i', l) ->
        assert (i == i');
        let n = unroll i in
        let l =
          if n <= 0 then l
          else
            let l = Empty :: l in
            if List.length l <= n + 1
            then l
            else begin
              let merge t1 t2 =
                if not (is_empty_propagation_tree t2) then
                  Value_parameters.warning ~once:true ~current:true
                    ~wkey:Value_parameters.wkey_loop_unrolling
                    "loop not completely unrolled";
                join_propagation_tree t1 t2
              in
              list_merge_two_last merge l
            end
        in
        Loop (i', l)
      | Leaf _ -> assert false
end

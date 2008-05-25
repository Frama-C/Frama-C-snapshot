(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* $Id: cabsint.ml,v 1.26 2008/02/05 12:10:47 marche Exp $ *)

(* TO DO:

   - call to [Ceffect] functions to compute effects of loops expects
   previous name disambiguation. This is not done, which can lead to
   errors, e.g.

   declare_heap_var : result ; oldtype = ()double ; newtype = ()global pointer 
   Anomaly: File "c/ceffect.ml", line 98, characters 6-12: Assertion failed

   - in [propagate], when doing backward prop, do not propagate on incoming
   edge with [None] value, control does not flow through this edge !

   - add location to the hooks for invariants/preconditions ?

   - problem with construction of graph for "do-while", not knowing
   if [fwd_node] or [bwd_node] is the place the invariant is checked.
   If both, problem.

   - see why code cannot be shared between [Make_PointWiseSemiLattice]
   and [Make_PointWiseLattice]. Problem with abstraction [t].

   - consider a declaration without initializer as a definition to some top
   value. Currently such declarations are ignored, which allows to translate
       char* q;
       if (b) q = p;
       *q = 0;
   by
       if (b) ;
       *p = 0;

   - treat as a test the expression in a switch statement. The difficulty here
   is that cases with multiple constants, e.g.
       case 1:
       case 2:
   should be translated by a test of the form
       (e == 1) || (e == 2)
   but [e] could be performing some side-effects. In this case a local variable
   (with switch scope) should be introduced, e.g.
       (tmp = e; (tmp == 1) || (tmp == 2))

   - check that the [default] case in a switch is properly defined by an empty
   set of constants, and use it in the definition of the operational graph.

*)

open Info
open Clogic
open Cast
open Cutil

let debug = Coptions.debug
let debug_more = false


(*****************************************************************************
 *                                                                           *
 * 		Basic modules for abstract interpretation                    *
 *                                                                           *
 *****************************************************************************)

(* type of widening performed, if any:
   - WidenFast : to infinity
   - WidenZero : if possible to 0, otherwise to infinity
   - WidenUnit : if possible to -1 or 0 or 1, otherwise to infinity
   - WidenSteps il : if possible to the first integer in [il] that fits,
                     otherwise to infinity
   Taken from Miné's example analysis.
 *)
type widening_t = WidenFast | WidenZero | WidenUnit | WidenSteps of int list

module type ELEMENT_OF_CONTAINER = sig
  type t
  val pretty : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end
 
module type VARIABLE = sig
  include ELEMENT_OF_CONTAINER
  val to_string : t -> string
end

(* general interface of any module representing integers *)

module type INT_VALUE = sig

  (* same as Int32/Int64 *)
  type t
  val compare : t -> t -> int
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val abs : t -> t
  val zero : t
  val one : t
  val minus_one : t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t

  (* added w.r.t. Int32/Int64 *)
  val pretty: Format.formatter -> t -> unit
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool
  val eq : t -> t -> bool
  val is_zero : t -> bool
  val is_one : t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val length : t -> t -> t (* b - a + 1 *) 
end

module type SEMI_LATTICE = sig
  type t
    (* [top] and [bottom] made functions so that they can depend 
       on the context, typed as [dim_t] (usually a dimension) *)
  type dim_t
  val top : dim_t -> t
  val bottom : dim_t -> t
  val init : dim_t -> t
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
  val join : ?backward:bool -> t -> t -> t    
    (* performs widening according to the widening strategy given as 1st
       argument, between the stored abstract value (2nd argument) and
       the new value computed (3rd argument).
       The new value should always be above the stored value in the lattice
       (or less precise), which should always be the case with monotone
       transfer functions. *)
  val widening : widening_t -> old_value:t -> new_value:t -> t
end

module type LATTICE = sig
  include SEMI_LATTICE
  val meet : t -> t -> t
end

module type POINT_WISE_SEMI_LATTICE = sig
  include SEMI_LATTICE
  type var_t
  type value_t
  type map_t
    (* creates a singleton map *)
  val singleton : var_t -> value_t -> t
    (* replace ignores the value already present for this variable, if any *)
  val replace : var_t -> value_t -> t -> t
    (* [add] performs a join if a value was already present for this variable, 
       otherwise a simple add *)
  val union : var_t -> value_t -> t -> t
    (* [addmap] performs an [add] for every entry in the map *)
  val union_map : map_t -> t -> t
    (* [find] never raises an exception, returns the bottom value to signify 
       the variable was not found *)
  val find : var_t -> t -> value_t
    (* [iter] not defined for [top] value *)
  val iter : (var_t -> value_t -> unit) -> t -> unit
    (* [fold] not defined for [top] value *)
  val fold : (var_t -> value_t -> 'a -> 'a) -> t -> 'a -> 'a
  val mapi : (var_t -> value_t -> value_t) -> t -> t
end

module type POINT_WISE_LATTICE = sig
  include POINT_WISE_SEMI_LATTICE
  val meet : t -> t -> t
end

module Make_LatticeFromSemiLattice (L : SEMI_LATTICE) =
struct
  include L
  let meet x y = failwith "Semi-lattice does not implement meet"
end

module Make_PairSemiLattice (L1 : SEMI_LATTICE with type dim_t = unit)
    (L2 : SEMI_LATTICE with type dim_t = unit) 
    : SEMI_LATTICE with type dim_t = unit and type t = L1.t * L2.t =
struct
  type t = L1.t * L2.t
  type dim_t = unit

  let bottom () = L1.bottom (),L2.bottom ()
  let top () = L1.top (),L2.top ()
  let init () = L1.init (),L2.init ()

  let equal p1 p2 = L1.equal (fst p1) (fst p2) && L2.equal (snd p1) (snd p2)

  let pretty fmt p =
    Format.fprintf fmt "(%a,%a)" L1.pretty (fst p) L2.pretty (snd p)

  let join ?(backward=false) p1 p2 = 
    L1.join ~backward (fst p1) (fst p2),L2.join ~backward (snd p1) (snd p2)

  let widening ws ~old_value ~new_value =
    L1.widening ws (fst old_value) (fst new_value),
    L2.widening ws (snd old_value) (snd new_value)
end

module Make_PairLattice (L1 : LATTICE with type dim_t = unit)
    (L2 : LATTICE with type dim_t = unit) 
    : LATTICE with type dim_t = unit and type t = L1.t * L2.t =
struct
  include Make_PairSemiLattice (L1) (L2)
  let meet p1 p2 = L1.meet (fst p1) (fst p2),L2.meet (snd p1) (snd p2)
end

(* public type so that it can be used in both [Make_PointWiseSemiLattice]
   and [Make_PointWiseLattice] *)
type 'map pointwise_t =
  | PWempty
  | PWmap of 'map
  | PWall
      
module Make_PointWiseSemiLattice
    (V : VARIABLE) (L : SEMI_LATTICE with type dim_t = unit) 
    (* no information needed to create L's bottom and top elements *)
  : POINT_WISE_SEMI_LATTICE with type var_t = V.t and type value_t = L.t 
	(* [t] is made public to allow its reuse in [Make_PointWiseLattice] *)
	and type dim_t = unit and type t = L.t Map.Make(V).t pointwise_t =
struct

  module VMap = Map.Make (V)

  type var_t = V.t
  type value_t = L.t
  type map_t = L.t VMap.t 

  type t = L.t VMap.t pointwise_t

  type dim_t = unit
  let bottom () = PWempty
  let top () = PWall
  let init () = PWempty

  let equal pw1 pw2 = match pw1,pw2 with
    | PWempty,PWempty -> true
    | PWmap m1,PWmap m2 -> VMap.equal L.equal m1 m2
    | PWall,PWall -> true
    | _ -> false

  let pretty fmt pw = match pw with
    | PWempty -> Format.fprintf fmt "PWempty"
    | PWmap m -> Format.fprintf fmt "PWmap{%a}"
	(fun fmt m -> VMap.iter 
	   (fun v a -> Format.fprintf fmt "(%a,%a); " 
	      V.pretty v L.pretty a) m) m
    | PWall -> Format.fprintf fmt "PWall"

  let singleton var value =
    PWmap (VMap.add var value VMap.empty)

  let replace var value pw = match pw with
    | PWempty -> PWmap (VMap.add var value VMap.empty)
    | PWmap m -> PWmap (VMap.add var value m)
    | PWall -> PWall

  let find var pw = match pw with
    | PWempty -> L.bottom ()
    | PWmap m ->
	begin 
	  try VMap.find var m with Not_found -> L.bottom ()
	end
    | PWall -> L.top ()

  let union var value pw = match pw with
    | PWempty -> PWmap (VMap.add var value VMap.empty)
    | PWmap m -> 
	let new_value = L.join value (find var pw) in
	PWmap (VMap.add var new_value m)
    | PWall -> PWall

  let union_map m1 pw = match pw with
    | PWempty -> PWmap m1
    | PWmap m2 -> 
	(* union all elements of [m1] in [m2] *)
	VMap.fold union m1 pw
    | PWall -> PWall

  (* ideally, [pw2] should be smaller than [pw1] when unioning maps *)
  let join ?(backward=false) pw1 pw2 = match pw1,pw2 with
    | PWempty,pw | pw,PWempty -> pw
    | PWall,_ | _,PWall -> PWall
    | PWmap m1,PWmap m2 -> 
	(* union all elements of [m2] in [m1] *)
	union_map m2 pw1
    
  let iter f pw = match pw with
    | PWempty -> ()
    | PWmap m -> VMap.iter f m
    | PWall -> failwith "[iter] should not be called on [PWall]"

  let fold f pw init = match pw with
    | PWempty -> init
    | PWmap m -> VMap.fold f m init
    | PWall -> failwith "[fold] should not be called on [PWall]"

  let mapi f pw = match pw with
    | PWempty -> PWempty
    | PWmap m -> PWmap (VMap.mapi f m)
    | PWall -> PWall

  let widening ws ~old_value ~new_value = match old_value,new_value with
    | PWempty,new_value -> new_value
    | PWmap m1,PWmap m2 -> 
	VMap.fold 
	  (fun v a2 m -> 
	     try 
	       let a1 = VMap.find v m1 in
	       let a = L.widening ws a1 a2 in
	       replace v a m
	     with Not_found -> 
	       m (* keep current binding from [new_value] *)
	  ) m2 new_value (* yes, both from [new_value] *)
    | _,PWall -> PWall
    | _ -> 
	(* the stored value [pw1] is less precise than the new computed value
	   [pw2], which should not be the case *)
	assert false
end

module Make_PointWiseLattice
    (V : VARIABLE) (L : LATTICE with type dim_t = unit) 
    : POINT_WISE_LATTICE with type var_t = V.t and type value_t = L.t 
    and type dim_t = unit =
struct

  include Make_PointWiseSemiLattice(V)(L) 

  module VMap = Map.Make (V)

  let intersect pw var value acc =
    let new_value = L.meet value (find var pw) in
    union var new_value acc

  let intersect_map m1 pw = match pw with
    | PWempty -> PWempty
    | PWmap m2 -> 
	(* intersect all elements of [m1] in [m2] *)
	VMap.fold (intersect pw) m1 PWempty
    | PWall -> PWmap m1

  (* ideally, [pw2] should be smaller than [pw1] when intersecting maps *)
  let meet pw1 pw2 = match pw1,pw2 with
    | PWempty,_ | _,PWempty -> PWempty
    | PWall,pw | pw,PWall -> pw
    | PWmap m1,PWmap m2 -> 
	(* intersect all elements of [m2] in [m1] *)
	intersect_map m2 pw1
end

      
(*****************************************************************************
 *                                                                           *
 * 		Abstract interpretation core                                 *
 *                                                                           *
 *****************************************************************************)

type direction_t = Forward | Backward

(* type of pair that allows a missing part *)
type 'a pair_t = Fst of 'a | Snd of 'a | Both of 'a * 'a

(* gives operations on a control-flow graph *)

module type INTER_LANG = sig
  type ilvar_t
  type ilfun_t
  type decl_t

  (* type of declaration/statement/expression in the intermediate language *)
  module Node : ELEMENT_OF_CONTAINER
  module NodeSet : Set.S with type elt = Node.t
  module NodeMap : Map.S with type key = Node.t
  module NodeHash : sig
    include Hashtbl.S with type key = Node.t
    val find_both : 'a pair_t t -> Node.t -> 'a option * 'a option
    val find_pre : 'a pair_t t -> Node.t -> 'a option
    val find_post : 'a pair_t t -> Node.t -> 'a option
    val replace_both : 'a pair_t t -> Node.t -> 'a -> 'a -> unit
    val replace_pre : 'a pair_t t -> Node.t -> 'a -> unit
    val replace_post : 'a pair_t t -> Node.t -> 'a -> unit
    val remove_both : 'a pair_t t -> Node.t -> unit
    val remove_pre : 'a pair_t t -> Node.t -> unit
    val remove_post : 'a pair_t t -> Node.t -> unit
    val iter_both : 
      (Node.t -> 'a option -> 'a option -> unit) -> 'a pair_t t -> unit
    val iter_pre : (Node.t -> 'a -> unit) -> 'a pair_t t -> unit
    val iter_post : (Node.t -> 'a -> unit) -> 'a pair_t t -> unit
    val fold_both : 
      (Node.t -> 'a option -> 'a option -> 'b -> 'b) -> 'a pair_t t -> 'b -> 'b
    val fold_pre : (Node.t -> 'a -> 'b -> 'b) -> 'a pair_t t -> 'b -> 'b
    val fold_post : (Node.t -> 'a -> 'b -> 'b) -> 'a pair_t t -> 'b -> 'b
  end

    (* is this node the function precondition ? *)
  val is_function_precondition_node : Node.t -> bool
    (* returns true if the argument is a widening node, i.e. a node where 
       the analysis is expected to perform widening if it does not
       naturally converge *)
  val is_widening_node : Node.t -> bool
    (* returns the value of the counter associated to a widening node *)
  val get_widening_count : Node.t -> int
    (* increment the counter associated to a widening node *)
  val incr_widening_count : Node.t -> unit
    (* reset the counter associated to a widening node *)
  val reset_widening_count : Node.t -> unit
    (* list of successors in the operational graph, e.g.
       - surrounding expression for a sub-expression
       - [else] and [then] branches for an [if] test expression
       ... 
       order does not matter *)
  val successors : ?ignore_looping:bool -> Node.t -> Node.t list
    (* list of predecessors in the operational graph.
       order does not matter *)
  val predecessors : ?ignore_looping:bool -> Node.t -> Node.t list
    (* is this node merging the upward and backward flows in a loop ? *)
  val is_loop_backward_destination_node : Node.t -> bool
    (* associates to some looping destination node its source node *)
  val looping_source : Node.t -> Node.t
    (* iterators *)
  val iter_operational : 
      direction_t -> roots:Node.t list -> (Node.t -> unit) -> unit
  val fold_operational : 
      direction_t -> roots:Node.t list -> (Node.t -> 'a -> 'a) -> 'a -> 'a
end

(* defines program points where result of analysis is useful *)
  
module type WATCH_POINT = sig
  (* replicates type of INTER_LANG *)
  type node_t
    (* defines watch-points *)
  val is_watch_node : node_t -> bool
end
  
(* connects the concrete and the abstract levels *)
  
module type CONNECTION = sig
    (* replicates type of INTER_LANG *)
  type node_t
  type 'a node_hash_t
    (* type of abstract value map *)
  type absval_t
    (* type of additional information used by [transform] *)
  type transform_t
    (* parametric type for the result of an analysis.
       It is an association from nodes to abstract values.
       First abstract value is the value before the node is entered.
       Second abstract value is the value after the node is exited. *)
  type 'a analysis_t = 'a pair_t node_hash_t
    (* result of abstract interpretation analysis *)
  type absint_analysis_t = absval_t analysis_t
    (* kind of widening performed, if any *)
  val widening_strategy : widening_t
    (* number of times a widening node can be updated before widening
       is performed. Setting it to [None] means that no widening will be
       ever performed. *)
  val widening_threshold : int option
    (* transfer function *)
  val transfer : 
      ?backward:bool -> ?with_assert:bool -> ?one_pass:bool
	-> ?previous_value:absval_t -> node_t -> absval_t -> absval_t
    (* takes the initial program and the formatted analysis, as well as 
       additional information if necessary.
       returns a transformed program. *)
  val transform : 
      absint_analysis_t -> transform_t -> node_t list -> node_t list
end

module type DATA_FLOW_ANALYSIS = sig
  (* replicates type of INTER_LANG *)
  type node_t
  type 'a node_hash_t
  type 'a node_map_t
    (* replicates types of CONNECTION *)
  type absval_t
  type absval_pair_t = absval_t * absval_t
  type 'a analysis_t
  type absint_analysis_t
    (* 
       core propagation function.
       It takes as 1st argument a record of type ['a propagate_t] that gives
       all the necessary basic blocks for the propagation algorithm.
       The list of nodes is used to initialize its working list. It may be 
       all the nodes in the graph, or only the root nodes, depending on
       the analysis. 
       The optional argument is the initial analysis. It will be used as is, 
       not copied (useful to remember as it is an hash-table).
       It produces an analysis that depends on 'a. 
     *)
  type 'a propagate_t =
      { 
	  (* direction *)
	direction : direction_t;
	  (* one-pass or until convergence *)
	one_pass : bool;
	  (* initialization points *)
	init : 'a node_map_t;
          (* transfer function *)
	transfer : ?previous_value:'a -> node_t -> 'a -> 'a;
	  (* default initial value *)
	bottom : unit -> 'a;
	  (* test of equality *)
	equal : 'a -> 'a -> bool;
	  (* pretty-printer *)
	pretty : Format.formatter -> 'a -> unit;
	  (* join function *)
	join : ?backward:bool -> 'a -> 'a -> 'a;
          (* join function on contexts *)
	join_ctxt : 'a -> 'a -> 'a;
	  (* widening function (if any) *)
	widening : widening_t -> old_value:'a -> new_value:'a -> 'a;
	  (* additional action after propagation *)
	action : node_t -> 'a -> unit;
      }
  val propagate : 
      ?analysis:'a analysis_t -> ?roots:node_t list -> 'a propagate_t
	-> 'a analysis_t
    (* 
       computes the result of an analysis on the implicit program.
       It is based on [propagate]. 
     *)
  val compute : node_t list -> absint_analysis_t
    (* compute backward from exit nodes *)
  val compute_back : node_t list -> absint_analysis_t
    (*
       same as compute, with additional assertion propagation.
       Takes as input the function that selects nodes to keep before assertion
       propagation.
    *)
  val compute_with_assert : 
    (node_t -> bool) -> node_t list -> absint_analysis_t
    (* 
       takes a program and computes the result of propagating forward
       the information, and then propagating backward/forward again from
       the nodes selected by [backward_select], with this new information
       being merged with the previous one only at nodes selected by
       [merge_select] using the function [merge_analyses]. 
     *)
  type compute_bnf_t = 
     {
         (* initial computation before back-and-forth propagation *)
       compute : node_t list -> absint_analysis_t;
         (* joins contexts and add conditionals, or default join otherwise *)
       join_context : absval_t -> absval_t -> absval_t;
         (* select nodes on which to propagate backward *)
       backward_select : node_t -> absval_t -> bool;
	 (* modifier to apply before initiating backward propagation *)
       backward_modify : node_t -> absval_t -> absval_t;
	 (* select nodes on which to keep forward information *)
       keep_select : node_t -> bool;
	 (* select nodes on which to merge forward and backward informations *)
       merge_select : node_t -> bool;
	 (* do merge forward and backward informations *)
       merge_analyses : absval_t -> absval_t -> absval_t;
     }
  val compute_back_and_forth :
    compute_bnf_t -> node_t list -> absint_analysis_t
end

(* very simple dataflow analysis, with fixed characteristics:
   forward, intra-procedural, context-insensitive *)

module Make_DataFlowAnalysis
    (V : VARIABLE) (IL : INTER_LANG) (L : LATTICE with type dim_t = unit)
    (* [L.dim_t] does not need to be [unit], change code if needed *)
    (* (W : WATCH_POINT with type node_t = IL.Node.t) *)
    (C : CONNECTION with type node_t = IL.Node.t and type absval_t = L.t
                    and type 'a node_hash_t = 'a IL.NodeHash.t)
  : DATA_FLOW_ANALYSIS
      with type node_t = IL.Node.t 
      and type 'a node_hash_t = 'a IL.NodeHash.t
      and type 'a node_map_t = 'a IL.NodeMap.t
      and type absval_t = C.absval_t
      and type absval_pair_t = C.absval_t * C.absval_t
      and type 'a analysis_t = 'a C.analysis_t
      and type absint_analysis_t = C.absint_analysis_t
= struct

  open IL

  type node_t = Node.t
  type 'a node_hash_t = 'a NodeHash.t
  type 'a node_map_t = 'a NodeMap.t
  type absval_t = L.t
  type absval_pair_t = absval_t * absval_t
  type 'a analysis_t = 'a C.analysis_t
  type absint_analysis_t = C.absint_analysis_t

  type 'a propagate_t =
      { 
	  (* direction *)
	direction : direction_t;
	  (* one-pass or until convergence *)
	one_pass : bool;
	  (* initialization points *)
	init : 'a node_map_t;
          (* transfer function *)
	transfer : ?previous_value:'a -> node_t -> 'a -> 'a;
	  (* default initial value *)
	bottom : unit -> 'a;
	  (* test of equality *)
	equal : 'a -> 'a -> bool;
	  (* pretty-printer *)
	pretty : Format.formatter -> 'a -> unit;
	  (* join function *)
	join : ?backward:bool -> 'a -> 'a -> 'a;
          (* join function on contexts *)
	join_ctxt : 'a -> 'a -> 'a;
	  (* widening function (if any) *)
	widening : widening_t -> old_value:'a -> new_value:'a -> 'a;
	  (* additional action after propagation *)
	action : node_t -> 'a -> unit;
      }

  type compute_bnf_t = 
     {
         (* initial computation before back-and-forth propagation *)
       compute : node_t list -> absint_analysis_t;
         (* joins contexts and add conditionals, or default join otherwise *)
       join_context : absval_t -> absval_t -> absval_t;
         (* select nodes on which to propagate backward *)
       backward_select : node_t -> absval_t -> bool;
	 (* modifier to apply before initiating backward propagation *)
       backward_modify : node_t -> absval_t -> absval_t;
	 (* select nodes on which to keep forward information *)
       keep_select : node_t -> bool;
	 (* select nodes on which to merge forward and backward informations *)
       merge_select : node_t -> bool;
	 (* do merge forward and backward informations.
	    Forward is [post] value while backward is [pre] value, which only
	    makes sense in general if the backward transfer function on
	    the node is the identity.
	 *)
       merge_analyses : absval_t -> absval_t -> absval_t;
     }

  let propagate ?analysis ?(roots=[]) params =

    if debug_more then Coptions.lprintf 
      "[propagate] %s@." (match params.direction with
      | Forward -> "forward"
      | Backward -> "backward");

    let change = ref true in
    let visited_set = ref NodeSet.empty in

    (* result of the analysis *)
    let res : 'a C.analysis_t = match analysis with
    | None -> NodeHash.create 0 
    | Some analysis -> analysis
    in

    (* find current values associated to [node] *)
    let res_val node =
      match NodeHash.find_both res node with
      | None,None ->
	  begin try 
	    let init_val = NodeMap.find node params.init in
	    begin match params.direction with
	    | Forward -> Some init_val,None,true
	    | Backward -> None,Some init_val,true
	    end
	  with Not_found -> None,None,false end
      | pre_val,post_val -> pre_val,post_val,false
    in

    let treat_node cur_node =
      if not (NodeSet.mem cur_node (!visited_set)) then
	begin 
	  visited_set := NodeSet.add cur_node (!visited_set);
	  if IL.is_widening_node cur_node then
	    (* reset the counter for widening *)
	    IL.reset_widening_count cur_node
	end;

      (* find value associated to [cur_node] *)
      let pre_val,post_val,is_init = res_val cur_node in

      let pre_val =
	if params.one_pass && is_loop_backward_destination_node cur_node then
	  match params.direction with
	    | Forward -> 
		let sce_node = looping_source cur_node in
		let _,sce_val,_ = res_val sce_node in
		begin match pre_val,sce_val with
		  | None,_ -> pre_val
		  | Some _,None -> pre_val
		  | Some pre_val,Some sce_val ->
		      Some (params.join_ctxt pre_val sce_val)
		end
	    | Backward -> pre_val
	else pre_val
      in

      let from_val,to_val = match params.direction with
        | Forward -> pre_val,post_val
	| Backward -> post_val,pre_val
      in
      if debug_more then Coptions.lprintf 
	"[propagate] take node in working list %a from val %a@."
	Node.pretty cur_node (Option.pretty params.pretty) from_val;

      let backward = match params.direction with
	| Backward -> true | Forward -> false
      in

      (* compute next value and replace existing one *)
      let cur_val = 
	(* do not call transfer function on init node in backward mode
	   (useful for access that is also a test, e.g. "if (t[i])") *)
	if backward && is_init then
	  from_val
	else match to_val with
	  | None -> 
	      Option.app (params.transfer cur_node) from_val
	  | Some to_val ->
	      Option.app (params.transfer ~previous_value:to_val cur_node) 
		from_val
      in
      (* perform widening if necessary *)
      let cur_val = match cur_val with
	| None -> None
	| Some cur_val ->
	    if IL.is_widening_node cur_node then
	      match C.widening_threshold with
		| None -> 
		    (* analysis does not require widening to converge *)
		    Some cur_val
		| Some threshold ->
		    let cur_count = IL.get_widening_count cur_node in
		    IL.incr_widening_count cur_node;
		    if cur_count = threshold then
		      (* perform widening *)
		      match to_val with
			| None -> Some cur_val
			| Some to_val -> 
			    if debug_more then Coptions.lprintf 
			      "[propagate] perform widening@.";
			    Some (params.widening C.widening_strategy 
				    ~old_value:to_val ~new_value:cur_val)
		    else if cur_count > threshold then
		      (* needed ???????? *)
		      to_val
		    else 
		      (* not yet time for widening *)
		      Some cur_val
	    else 
	      (* not a widening node *)
	      Some cur_val
      in
      if debug_more then Coptions.lprintf 
	"[propagate] computed value: %a@." 
	(Option.pretty params.pretty) cur_val;
      if debug_more then Coptions.lprintf 
	"[propagate] previous value: %a@." 
	(Option.pretty params.pretty) to_val;

      (* change value if different in fixpoint case, or in all cases when
	 doing a one-pass propagation. In the last case, the value of the node
	 is not modified, but the successors' values are. *)
      if not (Option.equal params.equal cur_val None) 
	&& (params.one_pass
	    || not (Option.equal params.equal cur_val to_val)) then
	begin
	  (* [from_val] and [cur_val] are [Some] options here *)
	  let from_val = 
	    match from_val with Some v -> v | None -> assert false in
	  let cur_val = 
	    match cur_val with Some v -> v | None -> assert false in

	  change := true;

	  if debug_more then Coptions.lprintf 
	    "[propagate] new value is different@.";
	  
	  begin match params.direction with
	    | Forward -> 
		NodeHash.replace_both res cur_node from_val cur_val;
	    | Backward -> 
		NodeHash.replace_both res cur_node cur_val from_val
	  end;

	  let next_nodes = match params.direction with
	    | Forward -> 
                IL.successors ~ignore_looping:params.one_pass cur_node 
	    | Backward -> 
                IL.predecessors ~ignore_looping:params.one_pass cur_node 
	  in
	  if debug_more then Coptions.lprintf 
	    "[propagate] node has %i successor(s)@." (List.length next_nodes);
	  if debug_more then Coptions.lprintf 
	    "[propagate] %a@." (fun fmt -> List.iter 
				(Coptions.lprintf "%a " Node.pretty))
	    next_nodes;
	  List.iter (fun nx_node ->
		       let nx_pre,nx_post,_ = res_val nx_node in
		       let nx_from,nx_to = match params.direction with
			 | Forward -> nx_pre,nx_post
			 | Backward -> nx_post,nx_pre
		       in
		       if debug_more then Coptions.lprintf 
			 "[propagate] succ prev value: %a@." 
			 (Option.pretty params.pretty) nx_from;
		       let nx_val = match nx_from with
			 | None -> cur_val 
			 | Some nx_from -> 
			     if is_function_precondition_node nx_node then
			       (* function precondition is non-empty, use it
				  rather than propagating initial value *)
			       nx_from (* ignore [cur_val] *)
			     else
			       params.join ~backward nx_from cur_val 
		       in
		       if debug_more then Coptions.lprintf 
			 "[propagate] succ cur value: %a@."
			   params.pretty nx_val;

		       (* change value if different *)
		       if not (Option.equal 
				 params.equal (Some nx_val) nx_from) then
			 begin
			   if debug_more then Coptions.lprintf 
			     "[propagate] new value for successor \
			      is different@.";
			   if debug_more then Coptions.lprintf 
			     "[propagate] new value: %a@." 
			       params.pretty nx_val;
			   let nx_to = match nx_to with
			     | None -> params.bottom ()
			     | Some nx_to -> nx_to
			   in
			   begin match params.direction with
			     | Forward ->
				 NodeHash.replace_both res nx_node 
				   nx_val nx_to
			     | Backward ->
				 NodeHash.replace_both res nx_node
				   nx_to nx_val
			   end;
(*
			   (* add node in working list/set if not present *)
			   add_node nx_node
*)
			 end
		    ) next_nodes
	end;

      (* perform additional action (if any) *)
      match cur_val with
	| None -> ()
	| Some cur_val -> params.action cur_node cur_val
    in

    while !change do
      change := false;
      if debug_more then Coptions.lprintf 
	  "[propagate] one more round of propagation@.";
      IL.iter_operational params.direction ~roots treat_node;
      (* immediately stop iteration in one-pass propagation *)
      if params.one_pass then change := false
    done;
    res

  let forward_params ?join_context
      ?(one_pass=false) ?(with_assert=false) init =
    { 
        (* direction *)
      direction = Forward;
        (* one-pass or until convergence *)
      one_pass = one_pass;
        (* initialization points *)
      init = init;
        (* transfer function *)
      transfer = C.transfer ~backward:false ~with_assert ~one_pass;
        (* default initial value *)
      bottom = L.bottom;
        (* test of equality *)
      equal = L.equal;
        (* pretty-printer *)
      pretty = L.pretty;
        (* join function *)
      join = L.join;
        (* join function on contexts *)
      join_ctxt = 
	begin match join_context with 
	  | None -> L.join ~backward:false 
	  | Some f -> f 
	end;
        (* widening function (if any) *)
      widening = L.widening;
        (* additional action after propagation *)
      action = fun _ _ -> ();
    }

  let backward_params ?join_context nodes cstr action =
    { 
        (* direction *)
      direction = Backward;
        (* one-pass or until convergence *)
      one_pass = true;
        (* initialization points *)
      init = List.fold_left (fun map node -> NodeMap.add node cstr map)
	NodeMap.empty nodes;
        (* transfer function *)
      transfer = C.transfer
	~backward:true ~with_assert:false ~one_pass:true;
        (* default initial value *)
      bottom = L.bottom;
        (* test of equality *)
      equal = L.equal;
        (* pretty-printer *)
      pretty = L.pretty;
        (* join function *)
      join = L.join;
        (* join function on contexts, only used in forward propagation *)
      join_ctxt = 
	begin match join_context with 
	  | None -> L.join ~backward:false 
	  | Some f -> f 
	end;
        (* widening function (if any) *)
      widening = L.widening;
        (* additional action after propagation *)
      action = action;
    }

  let keep_only_selected keep_select analysis =
    IL.iter_operational Forward ~roots:[] (fun node ->
      if keep_select node then
	NodeHash.remove_pre analysis node
      else
	NodeHash.remove analysis node
    )

  let compute decls =
    let init = List.fold_left (fun m decl -> NodeMap.add decl (L.init ()) m) 
      NodeMap.empty decls in
    propagate (forward_params init)

  let compute_back nodes =
    propagate (backward_params nodes (L.init()) (fun _ _ -> ()))

  (* propagation of assertions (e.g. resulting from verification conditions)
     should be done in one pass, without going through back edges in loops.
     Indeed, on the following code 
          int i = 0;
          int p[10]; // pp1
          while (i++) { // pp2
            p[i] = 0; // pp3
          }
     At program point 1 (pp1)  : i == 0 && arrlen(p) == 10
     At pp3 without assertions : i > 0 && arrlen(p) == 10
     At pp3 with assertions    : 0 < i < arrlen(p) == 10
     If we join pp1 and pp3 with assertions, we get a buggy assume-invariant
          0 <= i < arrlen(p) == 10
     The correct thing to do is to join pp1 and pp3 without assertions, to get
     the correct assume-invariant
          0 <= i && arrlen(p) == 10
     And then to propagate the assertions without going through back edges.
   *)
  let compute_with_assert keep_select decls =
    (* 1st step: propagate forward information *)
    let fwd_analysis = compute decls in

    (* 2nd step: propagate forward assertions *)
    keep_only_selected keep_select fwd_analysis;
    let init = List.fold_left (fun m decl -> NodeMap.add decl (L.init ()) m) 
      NodeMap.empty decls in
    propagate (forward_params ~with_assert:true ~one_pass:true init)
      ~analysis:fwd_analysis

  let compute_back_and_forth params decls =
    (* 1st step: propagate initial information *)
    let fwd_analysis = params.compute decls in

    (* 2nd step: propagate backward/forward again from every selected node *)
    IL.fold_operational Forward ~roots:[] (fun node cur_analysis ->

      let node_value_in_analysis analysis node =
	let pre_cur_val = match NodeHash.find_pre analysis node with
	| None -> 
	    (* use here top value, in order to correctly meet with value
	       from forward analysis *)
	    L.top ()
	| Some pre_val -> pre_val
	in
	let post_cur_val =  match NodeHash.find_post analysis node with
	| None -> 
	    (* use here top value, in order to correctly meet with value
	       from forward analysis *)
	    L.top ()
	| Some post_val -> post_val
	in
	(* always refer to [fwd_analysis] to keep initial fixpoint 
	   results. [analysis] may not have an appropriate value,
	   due to the fact some backward analysis may not have 
	   considered this node, and thus have "forgotten" the initial 
	   results. *)
	let pre_fwd_val,post_fwd_val =
	  match NodeHash.find_both fwd_analysis node with
	  | None,None -> 
	      (* use here top value to allow [meet] below *)
	      L.top (),L.top ()
	  | Some pre_val,None ->
	      pre_val,L.top ()
	  | None,Some post_val ->
	      L.top (),post_val
	  | Some pre_val,Some post_val ->
	      pre_val,post_val
	in
	let pre_cur_val = L.meet pre_cur_val pre_fwd_val in
	let post_cur_val = L.meet post_cur_val post_fwd_val in
	pre_cur_val,post_cur_val
      in

      let pre_val,_ = node_value_in_analysis cur_analysis node in

      if params.backward_select node pre_val then

	(* create an empty new analysis *)
	let mix_analysis = NodeHash.create (NodeHash.length cur_analysis) in
	let pre_val = params.backward_modify node pre_val in
	let bwd_action node pre_bwd_val =
	  if params.merge_select node || params.keep_select node then
	    let pre_cur_val,post_cur_val = 
	      node_value_in_analysis cur_analysis node
	    in
	    if params.merge_select node then
	      (* use [post] value, only one valid for previous analysis *)
	      let pre_mix_val = params.merge_analyses post_cur_val pre_bwd_val
	      in
	      (* for merge nodes, set same value as [pre] and [post] value *)
	      NodeHash.replace_both mix_analysis node pre_mix_val pre_mix_val
	    else if params.keep_select node then
	      NodeHash.replace_both mix_analysis node pre_cur_val post_cur_val
	    else assert false
	  else ()
	in
	(* propagate backward on the path that leads to this node *)
	let bwd_params = backward_params [node] pre_val bwd_action in
	let bwd_analysis = propagate bwd_params ~roots:[node] in
	ignore (bwd_analysis);
	(* add appropriate assume invariant when "forgotten" by last
	   backward analysis *)
	IL.iter_operational Forward ~roots:[] (fun node ->
	  if params.keep_select node then
	    let pre_mix_val,post_mix_val = 
	      node_value_in_analysis mix_analysis node
	    in
	    NodeHash.replace_both mix_analysis node pre_mix_val post_mix_val
	);
	(* propagate forward again *)
	keep_only_selected params.keep_select mix_analysis;
	let init =
	  List.fold_left (fun m decl -> NodeMap.add decl (L.init ()) m)
	    NodeMap.empty decls in
	let fwd_params = forward_params ~join_context:params.join_context 
	  ~with_assert:true ~one_pass:true init in
	propagate fwd_params ~analysis:mix_analysis

      else cur_analysis
    ) fwd_analysis

end


(*****************************************************************************
 *                                                                           *
 * 		Concrete intermediate language for analysis                  *
 *                                                                           *
 *****************************************************************************)

(* type used for offset from pointer *)
let int_offset_type = Ctypes.c_int
let int_offset_kind = Ctypes.Signed,Ctypes.Int
let int_offset_addop = Badd_int int_offset_kind

(* type to represent a function in the normalized code.
   It has the same elements as the hash-table [Cenv.c_functions]. *)
type func_t = 
    {
      name : string;
      spec : Cast.nspec; 
      typ  : Ctypes.ctype;
      f    : Info.fun_info;
      s    : Cast.nstatement option;
      loc  : Loc.position
    }

module ILVar : VARIABLE with type t = var_info = struct
  type t = var_info
  let pretty fmt v = Format.fprintf fmt "%s" v.var_name
  let to_string v = v.var_name
  let compare v1 v2 = Pervasives.compare v1.var_uniq_tag v2.var_uniq_tag
  let equal v1 v2 = compare v1 v2 = 0
  let hash v = v.var_uniq_tag
end

module ILVarMap = Map.Make (ILVar)
module ILVarSet = Set.Make (ILVar)

(* translation targetted at very simple syntaxical analysis on paths,
   since effects of calls are not considered, and unspecified evaluation order
   is translated in parallel edges in the CFG *)

module type CFG_LANG_INTERNAL = sig
  type node_t
  type var_tt
  type intern_t = 
   | InternOperational
   | InternOperationalBwdSrc
   | InternOperationalBwdDest
   | InternStructural
   | InternLogical
   | InternLogicalScope
  type count_t = { mutable count : int }
  type lvalue_t = Assign | OpAssign | IncrDecr
  type write_t = 
      { 
	(* list of variables written variables in the loop *)
	write_vars : var_tt list; 
	(* list of pointers whose content is read in the loop *)
	read_under_pointers : var_tt list; 
	(* list of pointers whose content is modified in the loop *)
	write_under_pointers : var_tt list; 
      }
  type norm_node = 
      (* coding constructs *)
    | Nexpr of nexpr
      (* execution proceeds to next nodes only if test succeeds. 
	 Replaces [Nexpr] where a test controls the flow: if, loop (switch ?).
	 The [then] branch will be preceded by the normal test of [if], while
	 the [else] branch will be preceded by the negation of this test.
	 The same occurs for loops.
	 This kind of node is produced for analysis on the operational graph
	 only. In particular, the subsequent transformations on the underlying
	 code are not supposed to return a [Ntest] node from a [Ntest] node.
	 They can return simply a [Nexpr] node.
       *)
    | Ntest of nexpr 
    | Nlvalue of nexpr * lvalue_t
    | Nstat of nstatement
    | Ndecl of func_t

      (* logical constructs *)
    | Nspec of nspec
    | Nannot of nloop_annot
    | Nterm of nterm
    | Npred of npredicate
      (* Various logical constructs act as "assume": assume statement,
	 function precondition, call postcondition, etc.
	 Each of these may already be part of an enclosing logical or
	 coding construct, e.g. the function precondition is already part of
	 a specification. Then the same [Nassume] node is used both 
	 in the operational graph and as a sub-node of an enclosing node
	 in the logical or the structural graph, e.g. an [Nspec] node 
	 in the logical graph for the function precondition. *)
    | Nassume of npredicate
      (* loop assume invariant is a special kind of assume *)
    | Nassinv of npredicate * write_t
      (* function precondition is a special kind of assume *)
    | Npre of npredicate
      (* Various logical constructs act as "assert": assert statement,
	 loop invariant, function postcondition, call precondition, etc.
	 Each of these may already be part of an enclosing logical or
	 coding construct, e.g. the loop invariant is already part of
	 an annotation. Then the same [Nassert] node is used both 
	 in the operational graph and as a sub-node of an enclosing node
	 in the logical or the structural graph, e.g. an [Nannot] node 
	 in the logical graph for the loop invariant. *)
    | Nassert of npredicate
      (* loop invariant is a special kind of assert *)
    | Ninv of npredicate * write_t

      (* special constructs *)
      (* "forward" node used in both hierarchical graphs (structural + logical)
	 and in the operational graph so far. 
	 The element it carries defines which graph it is part of.
	 This is used when changing the graph as the result of an analysis. *)
    | Nintern of intern_t
      (* widening node added in the operational lattice, to allow unbounded
	 analyses to terminate. The integer is a counter initialized to 0 and
	 incremented each time a new abstract value is computed for this node.
	 Depending on the analysis, some widening may be performed on 
	 the abstract value if the counter is above some threshold. *)
    | Nwiden of count_t

    (* create a temporary node that will not be part of either graph. 
       In particular, it will not be possible to add edges from/to this node.
       This is convenient for nodes created during the transformation of
       the code, that only store intermediate results. *)
  val create_tmp_node : norm_node -> node_t

  val get_e : node_t -> nexpr
  val get_expr : node_t -> nexpr_node
  val get_s : node_t -> nstatement
  val get_stat : node_t -> nstatement_node
  val get_decl : node_t -> func_t
  val get_spec : node_t -> nspec
  val get_annot : node_t -> nloop_annot
  val get_t : node_t -> nterm
  val get_term : node_t -> Ctypes.ctype nterm_node
  val get_p : node_t -> npredicate
  val get_predicate : node_t -> Ctypes.ctype npredicate_node

    (* is this expression an assignment ? *)
  val sub_expr_is_assign : nexpr -> bool
    (* if the left-hand side of this assignment is a variable, return it *)
  val sub_assign_get_lhs_var : nexpr -> var_tt option
    (* remove casts on top of the expression *)
  val sub_skip_casts : nexpr -> nexpr

  val decode_decl_list : nexpr -> nexpr c_initializer
  val change_sub_components_in_stat : node_t -> node_t list -> node_t
  val change_sub_components_in_expr : node_t -> node_t list -> node_t
  val change_sub_components_in_term : node_t -> node_t list -> node_t
  val change_sub_components_in_pred : node_t -> node_t list -> node_t
  val change_sub_components : node_t -> node_t list -> node_t

end

module type CFG_LANG_EXTERNAL = sig

  include INTER_LANG with type ilvar_t = ILVar.t 
		     and type ilfun_t = fun_info
		     and type decl_t = func_t

  (* successors in both structural and logical graph *)

    (* list of successors in the structural graph, e.g.
       - list of operands of an operation
       - caller and arguments of a function call
       - list of statements in a block statement 
       ... *)
  val code_children : Node.t -> Node.t list (* order matters *)
    (* list of successors in the logical graph, e.g.
       - loop annotation of a loop
       - spec of block of code
       - invariant of a loop annotation
       - sub-predicate in an invariant
       - term in a predicate
       ... *)
  val logic_children : Node.t -> Node.t list (* order matters *)
    (* beginning of logical block
       Used for \old annotations and specifications. *)
  val logic_begin : Node.t -> Node.t
    (* end of logical block
       Used for annotations and specifications. *)
  val logic_end : Node.t -> Node.t
    (* associated invariant relation, that relates corresponding invariant and
       assume invariant nodes for a specific loop *)
  val logic_invariant : Node.t -> Node.t option
    (* is it the beginning or the end of a logical scope ? *)
  val is_logic_scope : Node.t -> bool  

  (* 11 kinds of nodes: 
     for code: expression/test/statement/declaration
     for logic: specification/loop annotation/term/predicate/assume/assert
     for internal use: internal
  *)
  type node_kind = 
    (* coding *)  | NKexpr | NKtest | NKlvalue | NKstat | NKdecl 
    (* logical *) | NKspec | NKannot | NKterm | NKpred | NKassume | NKassert 
    (* special *) | NKnone

  (* query functions *)

    (* returns the node's kind *)
  val get_node_kind : Node.t -> node_kind
    (* is this node a [Ninv] node for a loop invariant ? *)
  val is_invariant_node : Node.t -> bool
    (* is this node a [Nassinv] node for a loop invariant ? *)
  val is_assume_invariant_node : Node.t -> bool
    (* is this node a [Nintern InternOperationalBwdSrc] node
       for collecting values on the backward edge of a loop before
       merging with the upward value ? *)
  val is_loop_backward_source_node : Node.t -> bool
  (* put in INTER_LANG interface :
     is_loop_backward_destination_node : Node.t -> bool
  *)
    (* get the list of variables modified in this loop *) 
  val get_loop_write_vars : Node.t -> ilvar_t list
    (* get the list of pointers whose content is read in this loop *) 
  val get_loop_read_under_pointers : Node.t -> ilvar_t list
    (* get the list of pointers whose content is modified in this loop *) 
  val get_loop_write_under_pointers : Node.t -> ilvar_t list
    (* returns the function's parameters *)
  val decl_get_params : Node.t -> ilvar_t list
    (* is it a return statement ? *)
  val stat_is_return : Node.t -> bool
    (* is it an assert statement ? *)
  val stat_is_assert : Node.t -> bool
    (* is it a loop statement ? *)
  val stat_is_loop : Node.t -> bool
    (* is it an if statement ? *)
  val stat_is_if : Node.t -> bool
    (* is it a spec statement ? *)
  val stat_is_spec : Node.t -> bool
    (* is it a label statement ? *)
  val stat_is_label : Node.t -> bool
    (* is it a declaration statement ? *)
  val stat_is_decl : Node.t -> bool
    (* get the label associated with this label statement *)
  val stat_get_label : Node.t -> string
    (* is it a jump statement ? *)
  val stat_is_jump : Node.t -> bool
    (* is it a block statement ? *)
  val stat_is_block : Node.t -> bool
    (* is it a no-op statement ? *)
  val stat_is_nop : Node.t -> bool
    (* get the variable associated with this declaration statement *)
  val decl_stat_get_var : Node.t -> ilvar_t
    (* get the next statement of this declaration statement *)
  val decl_stat_get_next : Node.t -> Node.t
    (* is this expression a -local- variable ? *)
  val expr_is_local_var : Node.t -> bool
    (* returns the variable in this term/expression over a variable *)
  val termexpr_var_get : Node.t -> ilvar_t
    (* is this expression an integer constant ? *)
  val expr_is_int_constant : Node.t -> bool
    (* returns the integer in this expression over an integer constant *)
  val expr_int_constant_get : Node.t -> int
    (* is this expression an assignment ? *)
  val expr_is_assign : Node.t -> bool
    (* get the right operand of this assignment (maybe the ony one, in case of
       an increment/decrement *)
  val assign_get_rhs_operand : Node.t -> Node.t
    (* get the left operand of this assignment (maybe the ony one, in case of
       an increment/decrement *)
  val assign_get_lhs_operand : Node.t -> Node.t
    (* is this a local variable ? *)
  val var_is_local : ilvar_t -> bool
    (* is this variable an integer ? *)
  val var_is_integer : ilvar_t -> bool
    (* is the type of this expression an integer type ? *)
  val expr_type_is_int : Node.t -> bool
    (* is the type of this expression a character type ? *)
  val expr_type_is_char : Node.t -> bool
    (* is this expression an integer assignment ? *)
  val expr_is_int_assign : Node.t -> bool
    (* is this variable a pointer ? *)
  val var_is_pointer : ilvar_t -> bool
    (* is the type of this expression a pointer type ? *)
  val expr_type_is_ptr : Node.t -> bool
    (* remove casts on top of the expression *)
  val skip_casts : Node.t -> Node.t
    (* is this term/expression a -local- variable ? *)
  val termexpr_is_local_var : Node.t -> bool
    (* is this expression a pointer assignment ? *)
  val expr_is_ptr_assign : Node.t -> bool
    (* if the left-hand side of this assignment is a -local- variable, 
       return it *)
  val assign_get_local_lhs_var : Node.t -> ilvar_t option
    (* is this expression a dereference ? *)
  val expr_is_deref : Node.t -> bool
    (* get the variable and the offset dereferenced, if any *)
  val deref_get_variable_and_offset : 
      Node.t -> (ilvar_t * Node.t option) option
    (* if the dereferenced expression is a -local- variable, return it *)
  val deref_get_local_var : Node.t -> ilvar_t option
    (* is this expression a call ? *)
  val expr_is_call : Node.t -> bool
    (* get the function called, if any *)
  val call_get_function : Node.t -> ilfun_t option
    (* get the arguments for the call, if any *)
  val call_get_args : Node.t -> Node.t list
    (* get precondition for function *)
  val function_get_precondition : ilfun_t -> Node.t option
    (* get parameters for function *)
  val function_get_params : ilfun_t -> ilvar_t list
    (* is this term/predicate an \old one ? *)
  val termpred_is_old : Node.t -> bool
    (* is this term/predicate an \at one ? *)
  val termpred_is_at : Node.t -> bool
    (* get the label associated with this \at term/predicate *)
  val termpred_get_label : Node.t -> string

  (* constructors.
     - [make_] functions operate directly on their arguments.
     - [change_] functions take a first node as context, and operate on 
     other arguments.
  *)

    (* create a new sequence expression node *)
  val make_seq_expr : Node.t -> Node.t -> Node.t
    (* create a new node expression + constant *)
  val make_int_expr_add_cst : Node.t -> int -> Node.t
    (* create a new node expression + variable *)
  val make_int_expr_add_var : Node.t -> ilvar_t -> Node.t
    (* create a new node term/expression + term/expression *)
  val make_int_termexpr_add_termexpr : Node.t -> Node.t -> Node.t
    (* create a new declaration statement for the variable, with the given
       statement as next statement *)
  val make_var_decl : Node.t -> ilvar_t -> Node.t
    (* create a new block statement node *)
  val make_seq_stat : Node.t -> Node.t -> Node.t
    (* make this node be an integer constant to be added to some pointer *)
  val change_in_int_offset_cst : Node.t -> int -> Node.t
    (* make this node be an assignment variable = constant *)
  val change_in_int_var_assign_cst : Node.t -> ilvar_t -> int -> Node.t
    (* make this node be an assignment variable = variable *)
  val change_in_int_var_assign_var : Node.t -> ilvar_t -> ilvar_t -> Node.t
    (* make this node be an assignment variable = expression *)
  val change_in_int_var_assign_expr : Node.t -> ilvar_t -> Node.t -> Node.t
    (* change the structural sub-components of this node *)
  val change_sub_components : Node.t -> Node.t list -> Node.t 

  (* language interface *)
  (* returns the list of roots *)
  val from_file : decl_t list -> (Node.t * Node.t) list
  val to_file : Node.t list -> decl_t list
    (* collect variables used and declared in the code *)
  val collect_vars : unit -> ILVarSet.t * ILVarSet.t
end

module CFGLangFromNormalized : sig
  include CFG_LANG_EXTERNAL
  include CFG_LANG_INTERNAL with type node_t = Node.t and type var_tt = ILVar.t
end = struct
  
  type var_tt = ILVar.t
  type ilvar_t = ILVar.t
  type ilfun_t = fun_info
  type decl_t = func_t

  type node_kind = 
    (* coding *)  | NKexpr | NKtest | NKlvalue | NKstat | NKdecl 
    (* logical *) | NKspec | NKannot | NKterm | NKpred | NKassume | NKassert 
    (* special *) | NKnone

  (* special node [Nintern] used for special purposes:
     - during construction to reference future nodes
     - to translate [switch] into appropriate structural tree
     - in loops to create a destination node for back edge
     ...
     It carries an element of type [intern_t] that defines which graph
     it is part of.
  *)
  type intern_t = 
   | InternOperational
   | InternOperationalBwdSrc
   | InternOperationalBwdDest
   | InternStructural
   | InternLogical
   | InternLogicalScope

  type count_t = { mutable count : int }
  type lvalue_t = Assign | OpAssign | IncrDecr
  type write_t = 
      { write_vars : ILVar.t list; 
	read_under_pointers : ILVar.t list;
	write_under_pointers : ILVar.t list; }

  let w_empty = { write_vars = []; 
		  read_under_pointers = [];
		  write_under_pointers = []; }

  type norm_node = 
      (* coding constructs *)
    | Nexpr of nexpr
    | Ntest of nexpr 
    | Nlvalue of nexpr * lvalue_t
    | Nstat of nstatement
    | Ndecl of func_t
      (* logical constructs *)
    | Nspec of nspec
    | Nannot of nloop_annot
    | Nterm of nterm
    | Npred of npredicate
    | Nassume of npredicate
    | Nassinv of npredicate * write_t
    | Npre of npredicate
    | Nassert of npredicate
    | Ninv of npredicate * write_t
      (* special constructs *)
    | Nintern of intern_t
    | Nwiden of count_t

  (* type of labels on edges in the CFG *)
  module NodeRelation = struct
    type t =
      | OperationalFwd  (* forward edge in the operational graph *)
      | OperationalBwd  (* backward edge in the operational graph *)
      | StructuralDown  (* edge to first sub-node in the structural graph *)
      | StructuralSame  (* other edges in the structural graph *)
      | LogicalDown     (* edge to first sub-node in the logical graph *)
      | LogicalSame     (* other edges in the logical graph *)
      | LogicScopeBegin (* edge to first node in logical block *)
      | LogicScopeEnd   (* edge to last node in logical block *)
      | LogicInvariant  (* edge to relate invariant and assume invariant *)
    (* arbitrary index to provide total ordering *)
    let index r = match r with
      | OperationalFwd  -> 0
      | OperationalBwd  -> 1
      | StructuralDown  -> 2
      | StructuralSame  -> 3
      | LogicalDown     -> 4
      | LogicalSame     -> 5
      | LogicScopeBegin -> 6
      | LogicScopeEnd   -> 7
      | LogicInvariant  -> 8
    let compare r1 r2 = Pervasives.compare (index r1) (index r2)
    (* if not stated otherwise, an edge is a forward operational one *)
    let default = OperationalFwd
  end

  (* We use here an abstract imperative labeled graph.
     Labels are of 4 kinds :
         - operational labels form a graph that respects evaluation order,
     to facilitate dataflow analysis
         - structural labels form a graph that respects the hierarchical order
     between expressions and statements, e.g. with [StructuralDown] labels from
     an expression to its first sub-expression, and [StructuralSame] label 
     from a sub-expression to the next sub-expression at the same level.
         - logical labels form a graph similar to the structural graph, except
     it is used for logical properties on the code instead of the code itself.
         - logical scope labels decorate some nodes in the structural or
     logical graph with information on the beginning and end of their logical
     scope. This is used to interpret correctly the logical annotations.

     Beware that since the operational graph is used for dataflow analysis 
     computation, the action associated to the node will be repeated when
     going through this node in the graph. This makes it necessary sometimes
     to create internal nodes whose sole purpose is to connect parts of 
     the graph with no associated action.
     It may be also necessary to create such internal nodes in the structural
     or logical graph, in order to encode/decode more easily the underlying
     structural/logical constructs.
  *)

  module Self = 
    Graph.Imperative.Digraph.AbstractLabeled
      (struct type t = norm_node end) (NodeRelation)
  module Node = struct
    include Self.V
    let pretty fmt n =
      match label n with
      | Nexpr e -> Cprint.nexpr fmt e
      | Ntest e -> Cprint.nexpr fmt e
      | Nlvalue (e,_) -> Cprint.nexpr fmt e
      | Nstat s -> 
	  begin match s.nst_node with
	    | NSnop -> Format.fprintf fmt "NSnop"
	    | NSassert _ -> Format.fprintf fmt "NSassert"
	    | NSassume _ -> Format.fprintf fmt "NSassume"
	    | NSlogic_label _ -> Format.fprintf fmt "NSlogic_label"
	    | NSexpr e -> Format.fprintf fmt "NSexpr(%a)" Cprint.nexpr e 
	    | NSif (e,s1,s2) -> Format.fprintf fmt "NSif"
	    | NSwhile (annot,e,s1) -> Format.fprintf fmt "NSwhile"
	    | NSdowhile (annot,s1,e) -> Format.fprintf fmt "NSdowhile"
	    | NSfor (annot,einit,etest,eincr,s1) -> Format.fprintf fmt "NSfor"
	    | NSblock sl -> Format.fprintf fmt "NSblock"
	    | NSreturn None -> Format.fprintf fmt "NSreturn"
	    | NSreturn (Some e) -> Format.fprintf fmt "NSreturn" 
	    | NSbreak -> Format.fprintf fmt "NSbreak"
	    | NScontinue -> Format.fprintf fmt "NScontinue" 
	    | NSgoto _ -> Format.fprintf fmt "NSgoto"
	    | NSlabel (str,s1) -> Format.fprintf fmt "NSlabel"
	    | NSspec (spec,s1) -> Format.fprintf fmt "NSspec"
	    | NSdecl (typ,var,None,s1) -> Format.fprintf fmt "NSdecl"
	    | NSdecl (typ,var,Some cinit,s1) -> Format.fprintf fmt "NSdecl"
	    | NSswitch (e,c,cases) -> Format.fprintf fmt "NSswitch" 
	  end
      | Ndecl _ -> Format.fprintf fmt "Ndecl"
      | Nspec _ -> Format.fprintf fmt "Nspec"
      | Nannot _ -> Format.fprintf fmt "Nannot"
      | Nterm t -> Cprint.nterm fmt t
      | Npred p -> Cprint.npredicate fmt p
      | Nassume _ -> Format.fprintf fmt "Nassume"
      | Nassinv _ -> Format.fprintf fmt "Nassinv"
      | Npre _ -> Format.fprintf fmt "Npre"
      | Nassert _ -> Format.fprintf fmt "Nassert"
      | Ninv _ -> Format.fprintf fmt "Ninv"
      | Nintern fk -> 
          begin match fk with
            | InternOperational -> 
		Format.fprintf fmt "Nintern(InternOperational)"
            | InternOperationalBwdSrc -> 
		Format.fprintf fmt "Nintern(InternOperationalBwdSrc)"
            | InternOperationalBwdDest -> 
		Format.fprintf fmt "Nintern(InternOperationalBwdDest)"
            | InternStructural ->
		Format.fprintf fmt "Nintern(InternStructural)"
            | InternLogical ->
		Format.fprintf fmt "Nintern(InternLogical)"
            | InternLogicalScope ->
		Format.fprintf fmt "Nintern(InternLogicalScope)"
          end
      | Nwiden c -> Format.fprintf fmt "Nwiden(%d)" c.count
  end
  module Edge = Self.E

  module NodeSet = Set.Make (Node)
  module NodeMap = Map.Make (Node)
    (* It is necessary to make it a hash-table based on [Node.equal] and
       [Node.hash], otherwise mutated nodes (e.g. the invariant node with
       its mutable field) are not recognized equal. *)
  module NodeHash = struct

    include Hashtbl.Make (Node)

    let find_both analysis node =
      try 
	match find analysis node with
	| Fst v -> Some v,None
	| Snd v -> None,Some v
	| Both (v1,v2) -> Some v1,Some v2
      with Not_found -> None,None
    let find_pre analysis node = fst (find_both analysis node)
    let find_post analysis node = snd (find_both analysis node)

    let replace_both analysis node v1 v2 = 
      replace analysis node (Both (v1,v2))
    let replace_pre analysis node v1 =
      match find_post analysis node with
      | None -> replace analysis node (Fst v1)
      | Some v2 -> replace analysis node (Both (v1,v2))
    let replace_post analysis node v2 =
      match find_pre analysis node with
      | None -> replace analysis node (Snd v2)
      | Some v1 -> replace analysis node (Both (v1,v2))

    let remove_both = remove
    let remove_pre analysis node =
      match find_post analysis node with
	| None -> remove analysis node
	| Some v -> remove analysis node; replace_post analysis node v
    let remove_post analysis node =
      match find_pre analysis node with
	| None -> remove analysis node
	| Some v -> remove analysis node; replace_pre analysis node v

    let iter_both f analysis =
      iter (fun node v -> match v with
	      | Fst v -> f node (Some v) None
	      | Snd v -> f node None (Some v)
	      | Both (v1,v2) -> f node (Some v1) (Some v2)
	   ) analysis
    let iter_pre f analysis =
      iter (fun node v -> match v with
	      | Fst v -> f node v
	      | Snd v -> ()
	      | Both (v1,v2) -> f node v1
	   ) analysis
    let iter_post f analysis =
      iter (fun node v -> match v with
	      | Fst v -> ()
	      | Snd v -> f node v
	      | Both (v1,v2) -> f node v2
	   ) analysis
	
    let fold_both f analysis init =
      fold (fun node v acc -> match v with
	      | Fst v -> f node (Some v) None acc
	      | Snd v -> f node None (Some v) acc
	      | Both (v1,v2) -> f node (Some v1) (Some v2) acc
	   ) analysis init
    let fold_pre f analysis init =
      fold (fun node v acc -> match v with
	      | Fst v -> f node v acc
	      | Snd v -> acc
	      | Both (v1,v2) -> f node v1 acc
	   ) analysis init
    let fold_post f analysis init =
      fold (fun node v acc -> match v with
	      | Fst v -> acc
	      | Snd v -> f node v acc
	      | Both (v1,v2) -> f node v2 acc
	   ) analysis init
  end

  let internal_graph = ref None
  let graph () = match !internal_graph with
  | None -> failwith "attempting to access graph before creation"
  | Some g -> g

    (* add a node *)
  type node_t = Node.t
  let add_vertex node = Self.add_vertex (graph ()) node
  let create_tmp_node node = Node.create node
    (* create a node and add it to the graph *)
  let create_node node = 
    let node = create_tmp_node node in
    add_vertex node; 
    node

  (* shortcut query functions *)

  let get_node_kind node =
    match Node.label node with
      | Nexpr _   -> NKexpr
      | Ntest _   -> NKtest
      | Nlvalue _ -> NKlvalue
      | Nstat _   -> NKstat
      | Ndecl _   -> NKdecl
      | Nspec _   -> NKspec
      | Nannot _  -> NKannot
      | Nterm _   -> NKterm
      | Npred _   -> NKpred
        (* [Npre] and [Nassinv] are special cases of [Nassume] *)
      | Nassume _ -> NKassume
      | Nassinv _ -> NKassume
      | Npre    _ -> NKassume
        (* [Ninv] is a special case of [Nassert] *)
      | Nassert _ -> NKassert
      | Ninv _    -> NKassert
        (* both forward and widening nodes belong to 
	   the "default" kind [NKnone] *)
      | Nintern _    -> NKnone
      | Nwiden _  -> NKnone

  let get_e node = 
    match Node.label node with
      | Nexpr e -> e
      | Ntest e -> e
      | Nlvalue (e,_) -> e
      | _ -> failwith "[get_e] should be called only on expression or test"
  let get_expr node = (get_e node).nexpr_node

  let get_s node = 
    match Node.label node with
      | Nstat s -> s
      | _ -> failwith "[get_s] should be called only on statement"
  let get_stat node = (get_s node).nst_node

  let get_decl node = 
    match Node.label node with
      | Ndecl d -> d
      | _ -> failwith "[get_decl] should be called only on declaration"

  let get_spec node = 
    match Node.label node with
      | Nspec s -> s
      | _ -> failwith "[get_spec] should be called only on specification"
	  
  let get_annot node = 
    match Node.label node with
      | Nannot a -> a
      | _ -> failwith "[get_annot] should be called only on loop annotation"

  let get_t node = 
    match Node.label node with
      | Nterm t -> t
      | _ -> failwith "[get_t] should be called only on term"
  let get_term node = (get_t node).nterm_node

  let get_p node = 
    match Node.label node with
      | Npred p | Nassume p | Nassinv (p,_) 
      | Npre p | Nassert p | Ninv (p,_) 
	-> p
      | _ -> failwith "[get_p] should be called only on predicate"
  let get_predicate node = (get_p node).npred_node

  let fwd_is_structural node = match Node.label node with
    | Nintern InternStructural -> true
    | Nintern _ -> false
    | _ -> failwith "[fwd_is_structural] should be called only on forward node"
  let fwd_is_logical node = match Node.label node with
    | Nintern InternLogical -> true
    | Nintern _ -> false
    | _ -> failwith "[fwd_is_logical] should be called only on forward node"

  let is_invariant_node node = match Node.label node with
    | Ninv _ -> true
    | _ -> false

  let is_assume_invariant_node node = match Node.label node with
    | Nassinv _ -> true
    | _ -> false

  let is_loop_backward_source_node node = match Node.label node with
    | Nintern InternOperationalBwdSrc -> true
    | _ -> false

  let is_loop_backward_destination_node node = match Node.label node with
    | Nintern InternOperationalBwdDest -> true
    | _ -> false

  let get_loop_write_vars node = match Node.label node with
    | Ninv (_,w) | Nassinv (_,w) -> w.write_vars
    | _ -> assert false

  let get_loop_read_under_pointers node = match Node.label node with
    | Ninv (_,w) | Nassinv (_,w) -> w.read_under_pointers
    | _ -> assert false

  let get_loop_write_under_pointers node = match Node.label node with
    | Ninv (_,w) | Nassinv (_,w) -> w.write_under_pointers
    | _ -> assert false

  let is_function_precondition_node node = match Node.label node with
    | Npre _ -> true
    | _ -> false

  let is_widening_node node = match Node.label node with
    | Nwiden _ -> true
    | _ -> false

  let get_widening_count node =
    match Node.label node with
      | Nwiden c -> c.count
      | _ -> failwith "[get_counter] should be called only on widening node"

  let incr_widening_count node =
    match Node.label node with
      | Nwiden c -> c.count <- c.count + 1
      | _ -> failwith ("[incr_widening_count] should be called only"
		       ^ " on widening node")

  let reset_widening_count node =
    match Node.label node with
      | Nwiden c -> c.count <- 0
      | _ -> failwith ("[reset_widening_count] should be called only"
		       ^ " on widening node")

  (* more elaborate query functions related to pointer usage *)

  let is_pointer_type ctyp = match ctyp.Ctypes.ctype_node with
    | Ctypes.Tvar _ -> 
	assert false (* not allowed in code *)
    | Ctypes.Tarray _ | Ctypes.Tpointer _ -> 
	true
    | Ctypes.Tvoid | Ctypes.Tint _ | Ctypes.Tfloat _ | Ctypes.Tfun _ 
    | Ctypes.Tstruct _ | Ctypes.Tunion _ | Ctypes.Tenum _ ->
	false

  let var_is_local var =
    not var.var_is_static

  let is_integer_type ctyp = match ctyp.Ctypes.ctype_node with
    | Ctypes.Tvar _ -> 
	assert false (* not allowed in code *)
    | Ctypes.Tint _ | Ctypes.Tenum _ ->
	true
    | Ctypes.Tarray _ | Ctypes.Tpointer _ 
    | Ctypes.Tvoid | Ctypes.Tfloat _ | Ctypes.Tfun _ 
    | Ctypes.Tstruct _ | Ctypes.Tunion _ ->
	false

  let is_character_type ctyp = match ctyp.Ctypes.ctype_node with
    | Ctypes.Tvar _ -> 
	assert false (* not allowed in code *)
    | Ctypes.Tint (_,Ctypes.Char) ->
	true
    | Ctypes.Tint _ | Ctypes.Tenum _ ->
	false
    | Ctypes.Tarray _ | Ctypes.Tpointer _ 
    | Ctypes.Tvoid | Ctypes.Tfloat _ | Ctypes.Tfun _ 
    | Ctypes.Tstruct _ | Ctypes.Tunion _ ->
	false

  let var_is_integer var = is_integer_type var.var_type
  let sub_expr_type_is_int e = is_integer_type e.nexpr_type
  let expr_type_is_int node = sub_expr_type_is_int (get_e node)

  let sub_expr_type_is_char e = is_character_type e.nexpr_type
  let expr_type_is_char node = sub_expr_type_is_char (get_e node)

  let is_pointer_type ctyp = match ctyp.Ctypes.ctype_node with
    | Ctypes.Tvar _ -> 
	assert false (* not allowed in code *)
    | Ctypes.Tarray _ | Ctypes.Tpointer _ -> 
	true
    | Ctypes.Tvoid | Ctypes.Tint _ | Ctypes.Tfloat _ | Ctypes.Tfun _ 
    | Ctypes.Tstruct _ | Ctypes.Tunion _ | Ctypes.Tenum _ ->
	false

  let var_is_pointer var = is_pointer_type var.var_type
  let sub_expr_type_is_ptr e = is_pointer_type e.nexpr_type
  let expr_type_is_ptr node = sub_expr_type_is_ptr (get_e node)

  let rec sub_skip_casts e = match e.nexpr_node with
    | NEcast (_,e1) -> sub_skip_casts e1
    | NEunary ((Ufloat_of_int | Uint_of_float
	       | Ufloat_conversion | Uint_conversion),e1) -> sub_skip_casts e1
    | _ -> e
    
  let skip_casts node = 
    let e = get_e node in
    let new_e = sub_skip_casts e in
    create_tmp_node (Nexpr new_e)

  let termexpr_is_local_var node =
    match get_node_kind node with
      | NKexpr | NKtest | NKlvalue -> 
	  begin match get_expr node with
	    | NEvar (Var_info var) -> var_is_local var
	    | _ -> false
	  end
      | NKterm ->
	  begin match get_term node with
	    | NTvar var -> var_is_local var
	    | _ -> false
	  end
      | _ -> assert false

  let termpred_is_old node = 
    match get_node_kind node with
      | NKpred | NKassume | NKassert -> 
	  begin match get_predicate node with
	    | NPold _ -> true
	    | _ -> false
	  end
      | NKterm ->
	  begin match get_term node with
	    | NTold _ -> true
	    | _ -> false
	  end
      | _ -> false

  let termpred_is_at node = 
    match get_node_kind node with
      | NKpred | NKassume | NKassert -> 
	  begin match get_predicate node with
	    | NPat _ -> true
	    | _ -> false
	  end
      | NKterm ->
	  begin match get_term node with
	    | NTat _ -> true
	    | _ -> false
	  end
      | _ -> false

  let termpred_get_label node = 
    match get_node_kind node with
      | NKpred | NKassume | NKassert -> 
	  begin match get_predicate node with
	    | NPat (_,lab) -> lab
	    | _ -> assert false
	  end
      | NKterm ->
	  begin match get_term node with
	    | NTat (_,lab) -> lab
	    | _ -> assert false
	  end
      | _ -> assert false

  let decl_get_params node =
    (get_decl node).f.args

  let stat_is_return node =
    match get_stat node with NSreturn _ -> true | _ -> false

  let stat_is_assert node =
    match get_stat node with NSassert _ -> true | _ -> false

  let stat_is_loop node = match get_stat node with 
      | NSwhile _ | NSdowhile _ | NSfor _ -> true | _ -> false

  let stat_is_if node = 
      match get_stat node with NSif _ -> true | _ -> false

  let stat_is_spec node =
    match get_stat node with NSspec _ -> true | _ -> false

  let stat_is_label node =
    match get_stat node with NSlabel _ | NSlogic_label _ -> true | _ -> false

  let stat_is_decl node =
    match get_stat node with NSdecl _ -> true | _ -> false

  let stat_get_label node =
    match get_stat node with 
      | NSlabel (lab,_) -> lab.label_info_name 
      | NSlogic_label lab -> lab
      | _ -> assert false

  let stat_is_jump node = match get_stat node with
    | NSreturn _ | NSbreak | NScontinue | NSgoto _ -> 
	true
    | NSnop | NSassert _ | NSassume _ | NSlogic_label _ | NSexpr _ 
    | NSif _ | NSwhile _ | NSdowhile _ | NSfor _ | NSblock _ 
    | NSlabel _ | NSspec _ | NSdecl _ | NSswitch _ ->
	false

  let stat_is_block node = 
    match get_stat node with NSblock _ -> true | _ -> false

  let stat_is_nop node =
    match get_stat node with NSnop -> true | _ -> false

  let decl_stat_get_var node =
    match get_stat node with NSdecl (_,var,_,_) -> var | _ -> assert false

  let decl_stat_get_next node =
    match get_stat node with 
      | NSdecl (_,_,_,ns) -> 
	  create_tmp_node (Nstat ns)
      | _ -> assert false
	  
  let expr_is_local_var node = match get_expr node with
    | NEvar (Var_info var) -> var_is_local var
    | _ -> false

  let termexpr_var_get node =
    match get_node_kind node with
      | NKexpr | NKtest | NKlvalue -> 
	  begin match get_expr node with
	    | NEvar (Var_info var) -> var
	    | _ -> assert false
	  end
      | NKterm ->
	  begin match get_term node with
	    | NTvar var -> var
	    | _ -> assert false
	  end
      | _ -> assert false

  let expr_is_int_constant node = match get_expr node with
    | NEconstant (IntConstant s) ->
	begin 
	  try ignore(int_of_string s); true 
	  with Failure "int_of_string" -> false
	end
    | _ -> false

  let expr_int_constant_get node = match get_expr node with
    | NEconstant (IntConstant s) -> 
	begin 
	  try int_of_string s
	  with Failure "int_of_string" -> assert false
	end
    | _ -> assert false

  let expr_is_deref node = match get_expr node with
    | NEarrow _ | NEunary (Ustar,_) -> true
    | _ -> false

  let expr_is_call node = match get_expr node with
    | NEcall _ -> true
    | _ -> false

  let call_get_function node = match get_expr node with
    | NEcall { ncall_fun = fe } -> 
	begin match fe.nexpr_node with
	  | NEvar (Fun_info f) -> Some f
	  | _ -> None
	end
    | _ -> assert false

  let call_get_args node = match get_expr node with
    | NEcall { ncall_args = el } -> 
	List.map (fun e -> create_tmp_node (Nexpr e)) el
    | _ -> assert false

  let function_get_precondition func =
    let func_name = func.fun_name in
    try
      let (spec,_,_,_,_) = Hashtbl.find Cenv.c_functions func_name in
      begin match spec.requires with
	| None -> None
	| Some p -> Some (create_tmp_node (Npred p))
      end
    with Not_found -> None

  let function_get_params func =
    let func_name = func.fun_name in
    try
      let (_,_,f,_,_) = Hashtbl.find Cenv.c_functions func_name in
      f.args
    with Not_found -> []

  let array_and_pointer_types_match arr_typ ptr_typ =
    match arr_typ.Ctypes.ctype_node,ptr_typ.Ctypes.ctype_node with
    | Ctypes.Tarray (avalid,atyp,_),Ctypes.Tpointer (pvalid,ptyp) ->
	avalid = pvalid && (atyp = ptyp)
    | _ -> false

  let change_in_int_offset_cst node index =
    let e = get_e node in
    let cst_e = NEconstant (IntConstant (string_of_int index)) in
    let cst_e = { e with nexpr_node = cst_e; nexpr_type = int_offset_type } in
    create_tmp_node (Nexpr cst_e)

  let deref_get_variable_and_offset node = match get_expr node with
    | NEarrow (e1,_,_) | NEunary (Ustar,e1) -> 
	(* exhaustive case analysis needed here, in order to 
	   guarantee no dereference can be missed *)
	let rec destruct e = match e.nexpr_node with
	    (* dereference from some variable [v] *)
	  | NEvar (Var_info v) -> Some (v,None)
	  | NEcast (typ,e1) ->
	      begin match destruct e1 with
	        | None -> None
		| Some (v,off) ->
		    if array_and_pointer_types_match v.var_type typ then
		      Some (v,off)
		    else None
	      end
	  | NEbinary (e1,Badd_pointer_int,_) | NEincr (_,e1) ->
	      let e2opt = match e.nexpr_node with
		| NEbinary (_,_,e2) -> Some e2
		| NEincr ((Upostfix_inc | Upostfix_dec),_) -> None
		| NEincr (Uprefix_inc,_) ->
		    Some (get_e (change_in_int_offset_cst node 1))
		| NEincr (Uprefix_dec,_) ->
		    Some (get_e (change_in_int_offset_cst node (-1)))
		| _ -> assert false
	      in		    
	      begin match destruct e1 with
	        | None -> None
		| Some (v,None) -> Some (v,e2opt)
		| Some (v,Some off) as e1destr ->
		    begin match e2opt with
		      | None -> e1destr
		      | Some e2 ->
			  let new_off = NEbinary (off,int_offset_addop,e2) in
			  let new_off = { off with nexpr_node = new_off } in
			  Some (v,Some new_off)
		    end
	      end
	    (* not a dereference from a variable *)
	  | NEarrow _ | NEunary (Ustar,_) | NEcall _ -> None
	    (* other cases should not be possible *)
	  | _ -> assert false
	in
	(* return a node as offset *)
	begin match destruct e1 with
	  | None -> None
	  | Some (v,None) -> Some (v,None)
	  | Some (v,Some e) -> Some (v,Some (create_tmp_node (Nexpr e)))
	end
    | _ -> assert false

  let deref_get_local_var node =
    match deref_get_variable_and_offset node with
      | None -> None
      | Some (var,_) -> if var_is_local var then Some var else None
    
  let sub_expr_is_assign e = match e.nexpr_node with
    | NEincr (_,e1) | NEassign (e1,_) | NEassign_op (e1,_,_) -> true
    | _ -> false      
	  
  let expr_is_assign node = 
    sub_expr_is_assign (get_e node)

  let sub_expr_is_int_assign e =
    sub_expr_is_assign e && (sub_expr_type_is_int e)
	
  let expr_is_int_assign node =
    sub_expr_is_int_assign (get_e node)

  let sub_expr_is_ptr_assign e =
    sub_expr_is_assign e && (sub_expr_type_is_ptr e)
    
  let expr_is_ptr_assign node =
    sub_expr_is_ptr_assign (get_e node)

  let assign_get_rhs_operand node = match (get_expr node) with
    | NEincr (_,e2) | NEassign (_,e2) ->
	create_tmp_node (Nexpr e2)
    | NEassign_op (e1,op,e2) -> 
	let rhs_e = NEbinary (e1,op,e2) in
	let rhs_e = { e1 with nexpr_node = rhs_e } in
	create_tmp_node (Nexpr rhs_e)
    | _ -> assert false

  let assign_get_lhs_operand node = match (get_expr node) with
    | NEincr (_,e1) | NEassign (e1,_) | NEassign_op (e1,_,_) -> 
	create_tmp_node (Nexpr e1)
    | _ -> assert false

  let sub_assign_get_lhs_var e = match e.nexpr_node with
    | NEincr (_,e1) | NEassign (e1,_) | NEassign_op (e1,_,_) ->
	begin
	  (* exhaustive case analysis needed here, in order to 
	     guarantee no assignment can be missed *)
	  match e1.nexpr_node with
	      (* cast not allowed here for the moment, it is checked
		 and error possibly reported in an earlier stage *)
	    | NEcast _ -> assert false
		(* not an assignment to a variable *)
	    | NEarrow _ | NEunary (Ustar,_) -> None
		(* assignment to some variable [var] *)
	    | NEvar (Var_info var) -> Some var
		(* other cases should not be possible *)
	    | _ -> assert false
	end
    | _ -> failwith ("[sub_assign_get_lhs_var] should be called"
		     ^ " only on assignment")

  let assign_get_lhs_var node =
    sub_assign_get_lhs_var (get_e node)

  let assign_get_local_lhs_var node =
    match assign_get_lhs_var node with
      | None -> None
      | Some var -> if var_is_local var then Some var else None

  (* construction of the graph(s), i.e. operational + structural *)

  (* [successors n] returns the operational successors of node [n] 
     for the dataflow analysis. Only [OperationalFwd] and [OperationalBwd]
     labels should be taken into account. 
     It is also the case for [predecessors]. *)
  let successors ?(ignore_looping=false) n = 
    let el = Self.succ_e (graph ()) n in
    let el = List.filter 
	(fun e -> 
	  Edge.label e = NodeRelation.OperationalFwd
	  || ((not ignore_looping)
              && (Edge.label e = NodeRelation.OperationalBwd))
        ) el in
    List.map Edge.dst el

  let predecessors ?(ignore_looping=false) n =
    let el = Self.pred_e (graph ()) n in
    let el = List.filter 
	(fun e -> 
	  Edge.label e = NodeRelation.OperationalFwd
	  || ((not ignore_looping)
              && (Edge.label e = NodeRelation.OperationalBwd))
        ) el in
    List.map Edge.src el

  let looping_source n =
    assert (is_loop_backward_destination_node n);
    let pl = predecessors n in
    let pl = List.filter is_loop_backward_source_node pl in
    match pl with
      | [sn] -> sn
      | _ -> assert false

  (* hierarchical successors. Used for structural and logical successors. *)
  let succ edge n = 
    let el = Self.succ_e (graph ()) n in
    let el = List.filter (fun e -> Edge.label e = edge) el in
    match List.map Edge.dst el with
      | [] -> None
      | [a] -> Some a
      | _ -> failwith ("[succ edge n] should find at most one successor"
		       ^ " for node [n]")
  (* hierarchical predecessors. Used for logical scopes. *)
  let has_pred edge n = 
    let el = Self.pred_e (graph ()) n in
    let el = List.filter (fun e -> Edge.label e = edge) el in
    match List.map Edge.src el with
      | [] -> false
      | _ -> true

  let child ?(structural=false) ?(logical=false) n = 
    if structural then
      succ NodeRelation.StructuralDown n
    else if logical then
      succ NodeRelation.LogicalDown n
    else (* no graph was specified, it is an error *)
      assert false
  let sibling ?(structural=false) ?(logical=false) n = 
    if structural then
      succ NodeRelation.StructuralSame n
    else if logical then 
      succ NodeRelation.LogicalSame n
    else (* no graph was specified, it is an error *)
      assert false
  let rec siblings ?(structural=false) ?(logical=false) n =
    match sibling ~structural ~logical n with
      | None -> []
      | Some m -> m :: (siblings ~structural ~logical m)
  let children ?(structural=false) ?(logical=false) n =
    match child ~structural ~logical n with
      | None -> []
      | Some m -> m :: (siblings ~structural ~logical m)

  (* structural successors. Only [StructuralDown] and [StructuralSame]
     labels should be taken into account. *)
  let code_children = children ~structural:true ~logical:false

  (* logical successors. Only [LogicalDown] and [LogicalSame]
     labels should be taken into account. *)
  let logic_children = children ~structural:false ~logical:true

  let logic_begin n = 
    match succ NodeRelation.LogicScopeBegin n with
      | Some nb -> nb
      | None -> failwith "no logical beginning node found"
  let logic_end n = 
    match succ NodeRelation.LogicScopeEnd n with
      | Some ne -> ne
      | None -> failwith "no logical end node found"

  let logic_invariant n = succ NodeRelation.LogicInvariant n

  let is_logic_begin n = has_pred NodeRelation.LogicScopeBegin n

  let is_logic_end n = has_pred NodeRelation.LogicScopeEnd n

  let is_logic_scope n = is_logic_begin n || is_logic_end n

  let add_edge edge v1 v2 =
    let e = Edge.create v1 edge v2 in
    Self.add_edge_e (graph ()) e

  (* topological order used by operations of iteration below.
     - [direction] is either forward or backward.
     - [roots] is empty for a classical propagation, and a single element
     for a propagation from a distinguished point of the program. It could
     be used also to propagate from a set of distinguished points, although
     not useful for now.
     - [sub_graph] is used when propagating from (a) distinguished point(s),
     to identify nodes in the resulting DAG.
  *)

  module OperationalTopologicalOrder =
    struct
      type t = 
	 {
	   direction : direction_t;
	   roots : NodeSet.t;
	   mutable sub_graph : NodeSet.t;
	 }

      module V = Node

      let iter_vertex f ord =
	if NodeSet.is_empty ord.roots then
	  Self.iter_vertex f (graph ())
	else
	  (* roots are forced. Force the input-degree too. *)
	  let work_set = ref ord.roots in
	  while (not (NodeSet.is_empty (!work_set))) do
	    let cur_work_set = !work_set in
	    work_set := NodeSet.empty;
	    ord.sub_graph <- NodeSet.union ord.sub_graph cur_work_set;
	    NodeSet.iter (fun node ->
	      let succ_nodes = match ord.direction with
	      | Forward -> successors ~ignore_looping:true node
	      | Backward -> predecessors ~ignore_looping:true node
	      in
	      List.iter (fun nx_node ->
		if not (NodeSet.mem nx_node ord.sub_graph) then
		  work_set := NodeSet.add nx_node !work_set) succ_nodes
	    ) cur_work_set
	  done;
	  Self.iter_vertex f (graph ())

      let iter_succ f ord node =
	let succ_nodes = match ord.direction with
	| Forward -> successors ~ignore_looping:true node
	| Backward -> predecessors ~ignore_looping:true node
	in
	let succ_nodes = 
	  if NodeSet.is_empty ord.roots then succ_nodes
	  else List.filter 
	      (fun node -> not (NodeSet.mem node ord.roots)) succ_nodes
	in
	List.iter f succ_nodes

      let in_degree ord node = 
	let pred_nodes = match ord.direction with
	| Forward -> predecessors ~ignore_looping:true node
	| Backward -> successors ~ignore_looping:true node
	in
	if NodeSet.is_empty ord.roots then
	  let pred_length = List.length pred_nodes in
	  if pred_length = 0 then
	    let succ_nodes = match ord.direction with
	    | Forward -> successors ~ignore_looping:true node
	    | Backward -> predecessors ~ignore_looping:true node
	    in
	    let succ_length = List.length succ_nodes in
	    (* ignore nodes not in the operational graph *)
	    if succ_length = 0 then 1
	    else pred_length
	  else pred_length
	else
	  (* only nodes in path should be taken into account *)
	  let pred_nodes = List.filter 
	      (fun node -> NodeSet.mem node ord.sub_graph) pred_nodes in
	  let pred_length = List.length pred_nodes in
	  (* force the nodes given as root to be such *)
	  if NodeSet.mem node ord.roots then 0
	  (* real root nodes or nodes not in the operational graph 
	     should be ignored in that case *)
	  else if pred_length = 0 then 1
	  else pred_length
    end

  module OperationalIterator = 
    Graph.Topological.Make (OperationalTopologicalOrder)

  let iter_operational direction ~roots f =
    let ord = {
      OperationalTopologicalOrder.direction = direction;
      OperationalTopologicalOrder.roots = 
      List.fold_right NodeSet.add roots NodeSet.empty;
      OperationalTopologicalOrder.sub_graph = NodeSet.empty;
    } in
    OperationalIterator.iter f ord

  let fold_operational direction ~roots f init =
    let ord = {
      OperationalTopologicalOrder.direction = direction;
      OperationalTopologicalOrder.roots =
      List.fold_right NodeSet.add roots NodeSet.empty;
      OperationalTopologicalOrder.sub_graph = NodeSet.empty;
    } in
    OperationalIterator.fold f ord init

  (* add an operational edge.
     - [force_add_opedge] should be used for edges that originate in 
     a jumping statement like a return/break/continue/goto.
     - [add_opedge] should be used in all other cases, with the knowledge
     that an edge will be added only if the source statement is not
     a jumping one. *)
  let force_add_opedge v1 v2 = Self.add_edge (graph ()) v1 v2
  let add_opedge v1 v2 =
    match get_node_kind v1 with
      | NKstat ->
	  if stat_is_jump v1 then
	    (* normal operational edges should not originate from
	       a jumping statement, in which case [force_add_opedge]
	       is used. Do nothing. *)
	    ()
	  else force_add_opedge v1 v2
      | _ -> force_add_opedge v1 v2

  (* add backward operational edge, for loops *)
  let add_backedge = add_edge NodeRelation.OperationalBwd
    
  (* add hierarchical edges. Used for adding structural and logical edges. *)
  let add_edges is_structural v vl = 
    let add_down_edge = 
      if is_structural then
	add_edge NodeRelation.StructuralDown 
      else
	add_edge NodeRelation.LogicalDown
    in
    let add_same_edge = 
      if is_structural then
	add_edge NodeRelation.StructuralSame
      else
	add_edge NodeRelation.LogicalSame
    in
    let rec add_same_edges vl = match vl with
      | a::b::r -> add_same_edge a b; add_same_edges (b::r)
      | _ -> ()
    in
    match vl with
      | [] -> ()
      | a::r -> add_down_edge v a; add_same_edges vl

  (* add structural edges: a [StructuralDown] edge from [v] to the first
     node in the list [vl], and [StructuralSame] edges between successive
     nodes in [vl]. Nothing to do with your hat's edge. *)
  let add_stedge = add_edges (* is_structural= *)true

  (* add logical edges: a [LogicalDown] edge from [v] to the first
     node in the list [vl], and [LogicalSame] edges between successive
     nodes in [vl]. Do not misinterpret it as a command to add luggage. *)
  let add_logedge = add_edges (* is_structural= *)false

  (* add logical block edge *)
  let add_begedge = add_edge NodeRelation.LogicScopeBegin
  let add_endedge = add_edge NodeRelation.LogicScopeEnd
  let add_invedge = add_edge NodeRelation.LogicInvariant

  (* constructors *)

  let make_seq_expr node1 node2 =
    let e1 = get_e node1 in
    let e2 = get_e node2 in
    let new_e = NEseq (e1,e2) in
    let new_e = { e2 with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  let make_int_expr_add_cst node cst =
    let e = get_e node in
    let typ = e.nexpr_type in
    let op = match typ.Ctypes.ctype_node with 
      | Ctypes.Tint ik -> Badd_int ik
      | _ -> failwith ("[make_int_expr_add_cst] should only be called on"
		       ^ " integer expression") in
    let cst_e = NEconstant (IntConstant (string_of_int cst)) in
    let cst_e = { e with nexpr_node = cst_e } in
    let new_e = NEbinary (e,op,cst_e) in
    let new_e = { e with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  let make_int_expr_add_var node var =
    let e = get_e node in
    let typ = e.nexpr_type in
    let op = match typ.Ctypes.ctype_node with 
      | Ctypes.Tint ik -> Badd_int ik
      | _ -> failwith ("[make_int_expr_add_cst] should only be called on"
		       ^ " integer expression") in
    let cst_e = NEvar (Var_info var) in
    let cst_e = { e with nexpr_node = cst_e } in
    (* prefer var + expr to expr + var, for ergonomic issues *)
    let new_e = NEbinary (cst_e,op,e) in
    let new_e = { e with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  let make_int_termexpr_add_termexpr node1 node2 =
    match get_node_kind node1 with
      | NKexpr | NKtest | NKlvalue -> 
	  let e1 = get_e node1 in
	  let e2 = get_e node2 in
	  let typ = e1.nexpr_type in
	  let op = match typ.Ctypes.ctype_node with 
	    | Ctypes.Tint ik -> Badd_int ik
	    | _ -> failwith ("[make_int_termexpr_add_termexpr] should only"
			     ^ " be called on integer expression") in
	  let new_e = NEbinary (e1,op,e2) in
	  let new_e = { e1 with nexpr_node = new_e } in
	  create_tmp_node (Nexpr new_e)    
      | NKterm ->
	  let t1 = get_t node1 in
	  let t2 = get_t node2 in
	  let op = Clogic.Badd in
	  let new_t = NTbinop (t1,op,t2) in
	  let new_t = { t1 with nterm_node = new_t } in
	  create_tmp_node (Nterm new_t)
      | _ -> assert false

  let make_var_decl node var =
    let s = get_s node in
    let new_s = NSdecl (var.var_type,var,None,s) in
    let new_s = { s with nst_node = new_s } in
    create_tmp_node (Nstat new_s)

  let make_seq_stat node1 node2 =
    let s1 = get_s node1 in
    let s2 = get_s node2 in
    let new_s = NSblock [s1;s2] in
    let new_s = { s1 with nst_node = new_s } in
    create_tmp_node (Nstat new_s)

  (* changes the expression's type if necessary *)
  let change_in_int_var_assign_cst node var index =
    let e = get_e node in
    let var_e = NEvar (Var_info var) in
    let typ_e = var.var_type in
    let var_e = { e with nexpr_node = var_e; nexpr_type = typ_e } in
    let cst_e = NEconstant (IntConstant (string_of_int index)) in
    let cst_e = { e with nexpr_node = cst_e; nexpr_type = typ_e } in
    let new_e = NEassign (var_e, cst_e) in
    let new_e = { e with nexpr_node = new_e; nexpr_type = typ_e } in
    create_tmp_node (Nexpr new_e)
    
  (* changes the expression's type if necessary *)
  let change_in_int_var_assign_var node var other_var =
    let e = get_e node in
    let var_e = NEvar (Var_info var) in
    let typ_e = var.var_type in
    let var_e = { e with nexpr_node = var_e; nexpr_type = typ_e } in
    let cst_e = NEvar (Var_info other_var) in
    let cst_e = { e with nexpr_node = cst_e; nexpr_type = typ_e } in
    let new_e = NEassign (var_e, cst_e) in
    let new_e = { e with nexpr_node = new_e; nexpr_type = typ_e } in
    create_tmp_node (Nexpr new_e)
      
  (* changes the expression's type if necessary *)
  let change_in_int_var_assign_expr node var sub_node =
    let e = get_e node in
    let var_e = NEvar (Var_info var) in
    let typ_e = var.var_type in
    let var_e = { e with nexpr_node = var_e; nexpr_type = typ_e } in
    let cst_e = get_e sub_node in
    let new_e = NEassign (var_e, cst_e) in
    let new_e = { e with nexpr_node = new_e; nexpr_type = typ_e } in
    create_tmp_node (Nexpr new_e)    

  (* to be matched with [encode_decl_list] below *)
  let rec decode_decl_list einit = match einit.nexpr_node with
    | NEcall ce ->
	let caller = ce.ncall_fun in
	if caller.nexpr_node = NEnop then
	  (* encoding of an initialization list *)
	  let args = ce.ncall_args in
	  Ilist (List.map decode_decl_list args)
	else
	  (* only other possibility is expression *)
	  Iexpr einit
    | _ -> Iexpr einit

  let change_sub_components_in_stat node sub_nodes =
    let s = get_s node in
    let new_s = match s.nst_node with
      | NSnop | NSlogic_label _ ->
	  assert (List.length sub_nodes = 0);
	  s.nst_node
      | NSassert _ ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NSassert new_p1	  
      | NSassume _ ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NSassume new_p1	  
      | NSexpr e -> 
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NSexpr new_e
      | NSif (e,s1,s2) ->
	  assert (List.length sub_nodes = 3);
	  let new_e,new_s1,new_s2 = list3 sub_nodes in
	  let new_e,new_s1,new_s2 
	    = get_e new_e,get_s new_s1,get_s new_s2 in
	  NSif (new_e,new_s1,new_s2)
      | NSwhile (annot,e,s1) ->
	  assert (List.length sub_nodes = 3);
	  let new_e,new_s1,new_a = list3 sub_nodes in
	  let new_e,new_s1,new_a = get_e new_e,get_s new_s1,get_annot new_a in
	  NSwhile (new_a,new_e,new_s1)
      | NSdowhile (annot,s1,e) ->
	  assert (List.length sub_nodes = 3);
	  let new_s1,new_e,new_a = list3 sub_nodes in
	  let new_s1,new_e,new_a = get_s new_s1,get_e new_e,get_annot new_a in
	  NSdowhile (new_a,new_s1,new_e)
      | NSfor (annot,einit,etest,eincr,s1) ->
	  assert (List.length sub_nodes = 5);
	  let new_einit,new_etest,new_eincr,new_s1,new_a = list5 sub_nodes in
	  let new_einit,new_etest,new_eincr,new_s1,new_a
	    = get_e new_einit,get_e new_etest,
	    get_e new_eincr,get_s new_s1,get_annot new_a in
	  NSfor (new_a,new_einit,new_etest,new_eincr,new_s1)
      | NSblock sl ->
	  let new_sl = List.map get_s sub_nodes in
	  NSblock new_sl
      | NSreturn None ->
	  assert (List.length sub_nodes = 0);
	  s.nst_node
      | NSreturn (Some e) -> 
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NSreturn (Some new_e)
      | NSbreak | NScontinue | NSgoto _ -> 
	  assert (List.length sub_nodes = 0);
	  s.nst_node
      | NSlabel (str,s1) ->
	  assert (List.length sub_nodes = 1);
	  let new_s = list1 sub_nodes in
	  let new_s = get_s new_s in
	  NSlabel (str,new_s)
      | NSspec (spec,s1) ->
	  assert (List.length sub_nodes = 2);
	  let new_s,new_spc = list2 sub_nodes in
	  let new_s,new_spc = get_s new_s,get_spec new_spc in
	  NSspec (new_spc,new_s)
      | NSdecl (typ,var,None,s1) ->
	  assert (List.length sub_nodes = 1);
	  let new_s = list1 sub_nodes in
	  let new_s = get_s new_s in
	  NSdecl (typ,var,None,new_s)
      | NSdecl (typ,var,Some cinit,s1) ->
	  assert (List.length sub_nodes = 2);
	  let new_e,new_s1 = list2 sub_nodes in
	  let new_e = get_e new_e in
	  let new_s1 = get_s new_s1 in
	  let lhs_expr,rhs_expr = match new_e.nexpr_node with 
	    | NEassign (lhs_expr,rhs_expr) -> lhs_expr,rhs_expr
	    | _ -> assert false in
	  let new_var = match lhs_expr.nexpr_node with
	    | NEvar (Var_info new_var) -> new_var
	    | _ -> assert false
	  in
	  assert (ILVar.equal var new_var);
	  let new_cinit = decode_decl_list rhs_expr in
	  NSdecl (typ,var,Some new_cinit,new_s1)
      | NSswitch (e,c,cases) -> 
	  let new_e = List.hd sub_nodes in
	  let new_cases = List.tl sub_nodes in
	  let new_e = get_e new_e in
	  (* remove [Nintern] node introduced for each [case] *)
	  let new_cases = 
	    List.map (fun n -> code_children n) new_cases in
	  let new_cases = List.map (List.map get_s) new_cases in
	  let new_cases = 
	    List.map2 (fun (cmap,_) sl -> (cmap,sl)) cases new_cases in
	  NSswitch (new_e,c,new_cases)
    in
    let new_s = { s with nst_node = new_s } in
    create_tmp_node (Nstat new_s)

  let change_sub_components_in_expr node sub_nodes =
    let e = get_e node in
    let new_e = match e.nexpr_node with
      | NEnop | NEconstant _ | NEstring_literal _ | NEvar _ ->
	  assert (List.length sub_nodes = 0);
	  e.nexpr_node
      | NEarrow (e1,zone,var) ->
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NEarrow (new_e,zone,var)
      | NEunary (op,e1) ->
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NEunary (op,new_e)
      | NEincr (op,e1) ->
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NEincr (op,new_e)
      | NEcast (typ,e1) ->
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NEcast (typ,new_e)
      | NEmalloc (typ,e1) ->
	  assert (List.length sub_nodes = 1);
	  let new_e = list1 sub_nodes in
	  let new_e = get_e new_e in
	  NEmalloc (typ,new_e)
      | NEseq (e1,e2) ->
	  assert (List.length sub_nodes = 2);
	  let new_e1,new_e2 = list2 sub_nodes in
	  let new_e1,new_e2 = get_e new_e1,get_e new_e2 in
	  NEseq (new_e1,new_e2)
      | NEassign (e1,e2) ->
	  assert (List.length sub_nodes = 2);
	  let new_e1,new_e2 = list2 sub_nodes in
	  let new_e1,new_e2 = get_e new_e1,get_e new_e2 in
	  NEassign (new_e1,new_e2)
      | NEassign_op (e1,op,e2) ->
	  assert (List.length sub_nodes = 2);
	  let new_e1,new_e2 = list2 sub_nodes in
	  let new_e1,new_e2 = get_e new_e1,get_e new_e2 in
	  NEassign_op (new_e1,op,new_e2)
      | NEbinary (e1,op,e2) ->
	  assert (List.length sub_nodes = 2);
	  let new_e1,new_e2 = list2 sub_nodes in
	  let new_e1,new_e2 = get_e new_e1,get_e new_e2 in
	  NEbinary (new_e1,op,new_e2)
      | NEcall c ->
	  let new_f = List.hd sub_nodes in
	  let new_args = List.tl sub_nodes in
	  let new_f = get_e new_f in
	  let new_args = List.map get_e new_args in
	  NEcall { c with ncall_fun = new_f; ncall_args = new_args }
      | NEcond (e1,e2,e3) ->
	  assert (List.length sub_nodes = 3);
	  let new_e1,new_e2,new_e3 = list3 sub_nodes in
	  let new_e1,new_e2,new_e3 
	    = get_e new_e1,get_e new_e2,get_e new_e3 in
	  NEcond (new_e1,new_e2,new_e3)
    in		
    let new_e = { e with nexpr_node = new_e } in
    create_tmp_node (Nexpr new_e)

  let change_sub_components_in_term node sub_nodes =
    let t = get_t node in
    let new_t = match t.nterm_node with
      | NTconstant _ | NTvar _ | NTminint _ | NTmaxint _ -> 
	  assert (List.length sub_nodes = 0);
	  t.nterm_node
      | NTapp a ->
	  let new_args = sub_nodes in
	  let new_args = List.map get_t new_args in
	  NTapp { a with napp_args = new_args }
      | NTunop (op,t1) ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTunop (op,new_t)
      | NTarrow (t1,zone,var) ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTarrow (new_t,zone,var)
      | NTold t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTold new_t
      | NTat (t1,label) ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTat (new_t,label)
      | NTbase_addr t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTbase_addr new_t
      | NToffset t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NToffset new_t
      | NTblock_length t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTblock_length new_t
      | NTarrlen t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTarrlen new_t
      | NTstrlen (t1,zone,var) ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTstrlen (new_t,zone,var)
      | NTmin (t1,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NTmin (new_t1,new_t2)
      | NTmax (t1,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NTmax (new_t1,new_t2)
      | NTcast (typ,t1) ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NTcast (typ,new_t)
      | NTbinop (t1,op,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NTbinop (new_t1,op,new_t2)
      | NTif (t1,t2,t3) ->
	  assert (List.length sub_nodes = 3);
	  let new_t1,new_t2,new_t3 = list3 sub_nodes in
	  let new_t1,new_t2,new_t3 
	    = get_t new_t1,get_t new_t2,get_t new_t3 in
	  NTif (new_t1,new_t2,new_t3)
      | NTrange (t1,t2opt,t3opt,zone,info) ->
	  assert (List.length sub_nodes = 3);
	  let new_t1,new_t2,new_t3 = list3 sub_nodes in
	  let new_t1 = get_t new_t1 in
	  let new_t2 = match logic_children new_t2 with
	    | [new_t2] -> Some (get_t new_t2)
	    | [] -> None
	    | _ -> assert false (* bad encoding *)
	  in
	  let new_t3 = match logic_children new_t3 with
	    | [new_t3] -> Some (get_t new_t3)
	    | [] -> None
	    | _ -> assert false (* bad encoding *)
	  in
	  NTrange (new_t1,new_t2,new_t3,zone,info)
    in		
    let new_t = { t with nterm_node = new_t } in
    create_tmp_node (Nterm new_t)

  let change_sub_components_in_pred node sub_nodes =
    let p = get_p node in
    let new_p = match p.npred_node with
      | NPfalse | NPtrue -> 
	  assert (List.length sub_nodes = 0);
	  p.npred_node
      | NPapp a ->
	  let new_args = sub_nodes in
	  let new_args = List.map get_t new_args in
	  NPapp { a with napp_args = new_args }
      | NPrel (t1,rel,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NPrel (new_t1,rel,new_t2)
      | NPvalid_index (t1,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NPvalid_index (new_t1,new_t2)
      | NPand (p1,p2) ->
	  assert (List.length sub_nodes = 2);
	  let new_p1,new_p2 = list2 sub_nodes in
	  let new_p1,new_p2 = get_p new_p1,get_p new_p2 in
	  NPand (new_p1,new_p2)		
      | NPor (p1,p2) ->
	  assert (List.length sub_nodes = 2);
	  let new_p1,new_p2 = list2 sub_nodes in
	  let new_p1,new_p2 = get_p new_p1,get_p new_p2 in
	  NPor (new_p1,new_p2)		
      | NPimplies (p1,p2) ->
	  assert (List.length sub_nodes = 2);
	  let new_p1,new_p2 = list2 sub_nodes in
	  let new_p1,new_p2 = get_p new_p1,get_p new_p2 in
	  NPimplies (new_p1,new_p2)		
      | NPiff (p1,p2) ->
	  assert (List.length sub_nodes = 2);
	  let new_p1,new_p2 = list2 sub_nodes in
	  let new_p1,new_p2 = get_p new_p1,get_p new_p2 in
	  NPiff (new_p1,new_p2)		
      | NPnot p1 ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPnot new_p1
      | NPforall (q,p1) ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPforall (q,new_p1)
      | NPexists (q,p1) ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPexists (q,new_p1)
      | NPold p1 ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPold new_p1
      | NPat (p1,label) ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPat (new_p1,label)
      | NPnamed (name,p1) ->
	  assert (List.length sub_nodes = 1);
	  let new_p1 = list1 sub_nodes in
	  let new_p1 = get_p new_p1 in
	  NPnamed (name,new_p1)
      | NPif (t1,p2,p3) ->
	  assert (List.length sub_nodes = 3);
	  let new_t1,new_p2,new_p3 = list3 sub_nodes in
	  let new_t1,new_p2,new_p3
	    = get_t new_t1,get_p new_p2,get_p new_p3 in
	  NPif (new_t1,new_p2,new_p3)		
      | NPvalid t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NPvalid new_t
      | NPfresh t1 ->
	  assert (List.length sub_nodes = 1);
	  let new_t = list1 sub_nodes in
	  let new_t = get_t new_t in
	  NPfresh new_t
      | NPvalid_range (t1,t2,t3) ->
	  assert (List.length sub_nodes = 3);
	  let new_t1,new_t2,new_t3 = list3 sub_nodes in
	  let new_t1,new_t2,new_t3 
	    = get_t new_t1,get_t new_t2,get_t new_t3 in
	  NPvalid_range (new_t1,new_t2,new_t3)
      | NPseparated (t1,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NPseparated (new_t1,new_t2)
      | NPfull_separated (t1,t2) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2 = list2 sub_nodes in
	  let new_t1,new_t2 = get_t new_t1,get_t new_t2 in
	  NPfull_separated (new_t1,new_t2)
      | NPbound_separated (t1,t2,t3,t4) ->
	  assert (List.length sub_nodes = 2);
	  let new_t1,new_t2,new_t3,new_t4 = list4 sub_nodes in
	  let new_t1,new_t2,new_t3,new_t4 =
	    get_t new_t1,get_t new_t2,get_t new_t3,get_t new_t4 in
	  NPbound_separated (new_t1,new_t2,new_t3,new_t4)
    in		
    let new_p = { p with npred_node = new_p } in
    create_tmp_node (Npred new_p)

  let change_sub_components_in_annot node sub_nodes =
    let a = get_annot node in
    assert (List.length sub_nodes = 4);
    let new_inv,new_assinv,new_ass,new_var = list4 sub_nodes in
    let new_inv = match logic_children new_inv with
      | [new_inv] -> 
	  let new_p = get_p new_inv in
          (* remove useless assert, possibly inserted by the analysis *)
	  begin match new_p.npred_node with
	    | NPtrue -> None 
	    | _ -> Some new_p
	  end
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_assinv = match logic_children new_assinv with
      | [new_assinv] ->
	  let new_p = get_p new_assinv in
          (* remove useless assume, possibly inserted by the analysis *)
	  begin match new_p.npred_node with
	    | NPtrue -> None 
	    | _ -> Some new_p
	  end
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_ass = match logic_children new_ass with
      | [new_ass] ->
	  let new_ass = List.map get_t (logic_children new_ass) in
	  Some (Loc.dummy_position, new_ass)
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let name_var = match a.variant with
      | None -> None
      | Some (_,so) -> so in
    let new_var = match logic_children new_var with
      | [new_var] -> Some (get_t new_var,name_var)
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_a = { invariant = new_inv; assume_invariant = new_assinv;
		  loop_assigns = new_ass; variant = new_var } in
    create_tmp_node (Nannot new_a)

  let change_sub_components_in_spec node sub_nodes =
    let s = get_spec node in
    assert (List.length sub_nodes = 4);
    let new_req,new_ass,new_ens,new_dec = list4 sub_nodes in
    let new_req = match logic_children new_req with
      | [new_req] ->
	  let new_p = get_p new_req in
          (* remove useless assume, possibly inserted by the analysis *)
	  begin match new_p.npred_node with
	    | NPtrue -> None 
	    | _ -> Some new_p
	  end
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_ass = match logic_children new_ass with
      | [new_ass] ->
	  let new_ass = List.map get_t (logic_children new_ass) in
	  Some (Loc.dummy_position, new_ass)
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_ens = match logic_children new_ens with
      | [new_ens] -> Some (get_p new_ens)
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let name_dec = match s.decreases with
      | None -> None
      | Some (_,so) -> so in
    let new_dec = match logic_children new_dec with
      | [new_dec] -> Some (get_t new_dec,name_dec)
      | [] -> None
      | _ -> assert false (* bad encoding *)
    in
    let new_s = { requires = new_req; assigns = new_ass;
		  ensures = new_ens; decreases = new_dec } in
    create_tmp_node (Nspec new_s)

  let change_sub_components node sub_nodes =
    match get_node_kind node with
      | NKnone ->
	  (* forward node for upper level. Rebuild the correct one-level 
	     sub-graph so that the upper level can rely on it if necessary.
	     Chained forward nodes can recreate more than one level. *)
	  let is_structural = fwd_is_structural node in
	  let is_logical = fwd_is_logical node in
	  let fwd_node = 
             if is_structural then create_node (Nintern InternStructural)
             else if is_logical then create_node (Nintern InternLogical)
             else 
               (* only forward nodes from the hierarchical graphs should be
		  reaching here *)
               assert false
          in
	  if is_structural then
	    add_stedge fwd_node sub_nodes
	  else
	    add_logedge fwd_node sub_nodes;
	  fwd_node
	  
      | NKdecl ->
	  let d = get_decl node in
	  let new_d =
	    match d.s with
	      | Some s ->
		  let new_s,new_spec = list2 sub_nodes in
		  let new_s,new_spec = get_s new_s,get_spec new_spec in
		  { d with s = Some new_s; spec = new_spec }
	      | _ -> d
	  in
	  create_tmp_node (Ndecl new_d)

      | NKstat ->
	  change_sub_components_in_stat node sub_nodes
	    
      | NKexpr | NKtest | NKlvalue ->
	  change_sub_components_in_expr node sub_nodes
	    
      | NKterm ->
	  change_sub_components_in_term node sub_nodes
	    
      | NKpred | NKassume | NKassert -> 
	  change_sub_components_in_pred node sub_nodes

      | NKannot ->
	  change_sub_components_in_annot node sub_nodes
	    
      | NKspec ->
	  change_sub_components_in_spec node sub_nodes

  (* extraction of graph from normalized AST *)

  let rec from_term (t : nterm) = 
    let tnode = create_node (Nterm t) in
    begin
      match t.nterm_node with
	| NTconstant _ | NTvar _ | NTminint _ | NTmaxint _ -> ()
	| NTapp a ->
	    let args_nodes = List.map from_term a.napp_args in
	    (* logic *) add_logedge tnode args_nodes
	| NTunop (_,t1) | NTarrow (t1,_,_) | NTold t1 | NTat (t1,_)
	| NTbase_addr t1 | NToffset t1 | NTblock_length t1 
	| NTarrlen t1 | NTstrlen (t1,_,_) | NTcast (_,t1) ->
	    let t1node = from_term t1 in
	    (* logic *) add_logedge tnode [t1node]
	| NTbinop (t1,_,t2) | NTmin (t1,t2) | NTmax (t1,t2) ->
	    let t1node = from_term t1 in
	    let t2node = from_term t2 in
	    (* logic *) add_logedge tnode [t1node; t2node]
	| NTif (t1,t2,t3) ->
	    let t1node = from_term t1 in
	    let t2node = from_term t2 in
	    let t3node = from_term t3 in
	    (* logic *) add_logedge tnode [t1node; t2node; t3node]
	| NTrange (t1,t2opt,t3opt,_,_) ->
	    let t1node = from_term t1 in
	    let t2node = create_node (Nintern InternLogical) in
	    begin match t2opt with 
	      | Some t2 ->
		  let t2optnode = from_term t2 in
		  (* logic *) add_logedge t2node [t2optnode]
	      | None -> ()
	    end;
	    let t3node = create_node (Nintern InternLogical) in
	    begin match t3opt with 
	      | Some t3 ->
		  let t3optnode = from_term t3 in
		  (* logic *) add_logedge t3node [t3optnode]
	      | None -> ()
	    end;
	    (* logic *) add_logedge tnode [t1node; t2node; t3node]
    end;
    tnode

  let rec from_pred ?(is_assume=false) ?(is_funpre=false)
   ?(is_assert=false) ?(is_invariant=false) ?(writes=w_empty) 
   (p : npredicate) =
    let pnode = 
      if is_invariant then 
	if is_assume then Nassinv (p,writes)
	else if is_assert then Ninv (p,writes)
	else assert false
      else if is_assume then Nassume p
      else if is_funpre then Npre p
      else if is_assert then Nassert p
      else Npred p
    in
    let pnode = create_node pnode in
    begin
      match p.npred_node with
	| NPfalse | NPtrue -> ()
	| NPapp a ->
	    let args_nodes = List.map from_term a.napp_args in
	    (* logic *) add_logedge pnode args_nodes
	| NPrel (t1,_,t2) | NPvalid_index (t1,t2) | NPseparated (t1,t2) 
	| NPfull_separated (t1,t2) ->
	    let t1node = from_term t1 in
	    let t2node = from_term t2 in
	    (* logic *) add_logedge pnode [t1node; t2node]
	| NPbound_separated (t1,t2,t3,t4) ->
  	    let t1node = from_term t1 in
	    let t2node = from_term t2 in
	    let t3node = from_term t3 in
	    let t4node = from_term t4 in
	    (* logic *) add_logedge pnode [t1node; t2node; t3node; t4node]
	| NPand (p1,p2) | NPor (p1,p2) | NPimplies (p1,p2) | NPiff (p1,p2) ->
	    let p1node = from_pred p1 in
	    let p2node = from_pred p2 in
	    (* logic *) add_logedge pnode [p1node; p2node]
	| NPnot p1 | NPforall (_,p1) | NPexists (_,p1) | NPold p1 
	| NPat (p1,_) | NPnamed (_,p1) ->
	    let p1node = from_pred p1 in
	    (* logic *) add_logedge pnode [p1node]
	| NPif (t1,p2,p3) ->
	    let t1node = from_term t1 in
	    let p2node = from_pred p2 in
	    let p3node = from_pred p3 in
	    (* logic *) add_logedge pnode [t1node; p2node; p3node]
	| NPvalid t1 | NPfresh t1 ->
	    let t1node = from_term t1 in
	    (* logic *) add_logedge pnode [t1node]
	| NPvalid_range (t1,t2,t3) ->
  	    let t1node = from_term t1 in
	    let t2node = from_term t2 in
	    let t3node = from_term t3 in
	    (* logic *) add_logedge pnode [t1node; t2node; t3node]
    end;
    pnode

  let from_spec ?(is_function=false) (s : nspec) = 
    let requires_node = create_node (Nintern InternLogical) in 
    let reqnode_opt = match s.requires with
      | Some p ->
	  let reqnode = from_pred ~is_funpre:is_function p in
	  (* logic *) add_logedge requires_node [reqnode];
	  Some reqnode
      | None ->
	  if is_function then
	    (* create a node [requires true] that will serve as a hook to 
	       improve on the assumed precondition *)
	    let ptrue = { npred_node = NPtrue; npred_loc = Loc.dummy_position }
	    in
	    let reqnode = from_pred ~is_funpre:is_function ptrue in
	    (* logic *) add_logedge requires_node [reqnode];
	    Some reqnode
	  else
	    None
    in
    let assigns_node = create_node (Nintern InternLogical) in
    begin match s.assigns with
      | Some (_, tl) ->
	  (* differenciate [None] from [Some([])] *)
	  let assnode = create_node (Nintern InternLogical) in
	  (* logic *) add_logedge assigns_node [assnode];
	  let tnodes = List.map from_term tl in
	  add_logedge assnode tnodes
      | None -> ()
    end;
    let ensures_node = create_node (Nintern InternLogical) in
    begin match s.ensures with
      | Some p ->
	  let ensnode = from_pred p in
	  (* logic *) add_logedge ensures_node [ensnode]
      | None -> ()
    end;
    let decreases_node = create_node (Nintern InternLogical) in
    begin match s.decreases with
      | Some (t,_) ->
	  let decnode = from_term t in
	  (* logic *) add_logedge decreases_node [decnode]
      | None -> ()
    end;
    let snode = create_node (Nspec s) in
    (* logic *) add_logedge snode [requires_node; assigns_node;
				   ensures_node; decreases_node];
    snode,reqnode_opt

  let from_annot (a : nloop_annot) writes =
    let assigns_node = create_node (Nintern InternLogical) in
    begin match a.loop_assigns with
      | Some (_,tl) ->
	  (* differenciate [None] from [Some([])] *)
	  let assnode = create_node (Nintern InternLogical) in
	  (* logic *) add_logedge assigns_node [assnode];
	  let tnodes = List.map from_term tl in
	  add_logedge assnode tnodes
      | None -> ()
    end;
    let invariant_node = create_node (Nintern InternLogical) in
    let invnode_opt = match a.invariant with
      | Some p ->
	  let invnode = from_pred 
	      ~is_invariant:true ~writes ~is_assert:true p 
	  in
	  (* logic *) add_logedge invariant_node [invnode];
	  Some invnode
      | None ->
	  (* create a node [assert true] that will serve as a hook to improve
	     on the asserted invariant *)
	  let ptrue = { npred_node = NPtrue; npred_loc = Loc.dummy_position }
	  in
	  let invnode = from_pred 
	      ~is_invariant:true ~writes ~is_assert:true ptrue
	  in
	  (* logic *) add_logedge invariant_node [invnode];
	  Some invnode
    in
    let assume_invariant_node = create_node (Nintern InternLogical) in
    let assinvnode_opt = match a.assume_invariant with
      | Some p ->
	  let assinvnode = from_pred 
	      ~is_invariant:true ~writes ~is_assume:true p in
	  (* logic *) add_logedge assume_invariant_node [assinvnode];
	  Some assinvnode
      | None ->
	  (* create a node [assume true] that will serve as a hook to improve
	     on the assumed invariant *)
	  let ptrue = { npred_node = NPtrue; npred_loc = Loc.dummy_position }
	  in
	  let assinvnode = from_pred 
	      ~is_invariant:true ~writes ~is_assume:true ptrue
	  in
	  (* logic *) add_logedge assume_invariant_node [assinvnode];
	  Some assinvnode	  
    in
    let variant_node = create_node (Nintern InternLogical) in
    begin match a.variant with
      | Some (t,_) ->
	  let varnode = from_term t in
	  (* logic *) add_logedge variant_node [varnode]
      | None -> ()
    end;
    let anode = create_node (Nannot a) in 
    (* logic *) add_logedge anode
      [invariant_node; assume_invariant_node; assigns_node; variant_node];
    anode,invnode_opt,assinvnode_opt

  (* shared code between the creation of a simple expression node and 
     the creation of a test node. The parameter [is_test] tells if the node
     created should be an expression or a test. The parameter [neg_test] is 
     used to specify the opposite of the test should be created. 
     The parameter [is_lvalue] specifies a lvalue is created.
     [in_incr] is set for an lvalue inside an increment/decrement operation.
     [in_opassign] is set for an lvalue inside an op-assignment.
  *)
  let rec from_expr start_node ?(is_test=false) ?(neg_test=false) 
      ?(is_lvalue=false) ?(in_incr=false) ?(in_opassign=false) (e : nexpr) =
    let e = match e.nexpr_node with
      | NEassign_op (e1,op,e2) ->
	  (* create an expression [e12] for the right-hand side of 
	     the assignment, in order to get rid of the opassign node *)
	  let e12 = { e with nexpr_node = NEbinary (e1,op,e2) } in
	  { e with nexpr_node = NEassign (e1,e12) }
      | _ -> e
    in
    let make_test_to_zero e = 
      let sub_e = sub_skip_casts e in
      match sub_e.nexpr_node with
	| NEvar (Var_info v) -> 
	    if var_is_pointer v then
	      let null_var = Info.default_var_info "null" in 
	      (* Info.set_assigned null_var; *)
	      Cenv.set_var_type (Var_info null_var) v.var_type false;
	      let null_expr =
		{ sub_e with nexpr_node = NEvar (Var_info null_var) } in
	      { e with nexpr_node = NEbinary (e,Bneq_pointer,null_expr) }
	    else e
	| _ -> e
    in
    let rec pure e = match e.nexpr_node with
      | NEnop | NEconstant _ | NEstring_literal _ | NEvar _ 
	  -> true
      | NEarrow (e1,_,_) | NEunary (_,e1) | NEcast (_,e1) 
	  -> pure e1
      | NEbinary (e1,_,e2) | NEseq (e1,e2) 
	  -> pure e1 && pure e2
      | NEcond (e1,e2,e3)
	  -> pure e1 && pure e2 && pure e3
      | NEmalloc _ | NEincr _ | NEassign _ | NEassign_op _ | NEcall _
	  -> false
    in
    let negative_lazy = match e.nexpr_node with
      | NEbinary (_,(Band | Bor),_) when neg_test -> true
      | _ -> false
    in
    let negative_not = match e.nexpr_node with
      | NEunary (Unot,_) when neg_test -> true
      | _ -> false
    in
    let push_not_inside = match e.nexpr_node with
      | NEunary (Unot,e1) when is_test && not neg_test ->
	  begin match e1.nexpr_node with
	    | NEunary (Unot,_) -> true
	    | NEbinary (_,(Band | Bor),_) -> true
	    | _ -> false
	  end
      | _ -> false
    in
    let e = 
      if negative_lazy then
	match e.nexpr_node with
	  | NEbinary (e1,(Band | Bor as op),e2) ->
	      let not_e1 = { e1 with nexpr_node = NEunary (Unot, e1) } in
	      let not_e2 = { e2 with nexpr_node = NEunary (Unot, e2) } in
	      begin match op with
		| Bor -> 
		    { e with nexpr_node = NEbinary (not_e1,Band,not_e2) }
		| Band ->
		    { e with nexpr_node = NEbinary (not_e1,Bor,not_e2) }
		| _ -> assert false
	      end
	  | _ -> assert false
      else if negative_not then
	match e.nexpr_node with
	  | NEunary (Unot,e1) -> make_test_to_zero e1
	  | _ -> assert false
      else if push_not_inside then
	e (* code for [Unot] below takes care of the negation *)
      else if neg_test then
        { e with nexpr_node = NEunary (Unot, make_test_to_zero e) }
      else if is_test then
	make_test_to_zero e
      else e
    in
    (* effect of [neg_test] taken into account at this point,
       ignore it, except for [Unot], when not in [negative_lazy] mode *)
    let enode =
      if is_test then Ntest e
      else if is_lvalue then
	if in_incr then Nlvalue (e,IncrDecr)
	else if in_opassign then Nlvalue (e,OpAssign)
	else Nlvalue (e,Assign)
      else Nexpr e
    in
    let enode = create_node enode in 
    begin
      match e.nexpr_node with
	| NEnop | NEconstant _ | NEstring_literal _ | NEvar _ ->
	    (* oper *) add_opedge start_node enode
	| NEunary (Unot,e1) when push_not_inside ->
	    assert (is_test && not neg_test);
	    let e1node = 
	      from_expr ~is_test ~neg_test:(not neg_test) start_node e1 in
	    (* oper *) add_opedge e1node enode;
	    (* struct *) add_stedge enode [e1node]
	| NEarrow (e1,_,_) | NEunary (_,e1) 
	| NEcast (_,e1) | NEmalloc (_,e1) ->
	    let e1node = from_expr start_node e1 in
	    (* oper *) add_opedge e1node enode;
	    (* struct *) add_stedge enode [e1node]
	| NEincr (_,e1) ->
	    let e1node = 
	      from_expr ~is_lvalue:true ~in_incr:true start_node e1 in
	    (* oper *) add_opedge e1node enode;
	    (* struct *) add_stedge enode [e1node]
	| NEseq (e1,e2) ->
	    let e1node = from_expr start_node e1 in
	    let e2node = from_expr e1node e2 in
	    (* oper *) add_opedge e2node enode;
	    (* struct *) add_stedge enode [e1node; e2node]
	| NEassign (e1,e2) ->
	    let e1node,e2node =
	      if Coptions.evaluation_order.Coptions.assign_left_to_right then
		let e1node = from_expr ~is_lvalue:true start_node e1 in
		let e2node = from_expr e1node e2 in
		(* oper *) add_opedge e2node enode;
	        e1node,e2node
    	      else
		let e2node = from_expr start_node e2 in
		let e1node = from_expr ~is_lvalue:true e2node e1 in
		(* oper *) add_opedge e1node enode;
	        e1node,e2node
	    in
	    (* struct *) add_stedge enode [e1node; e2node]
	| NEassign_op (e1,op,e2) ->
	    assert false (* expression should have been modified above *)
	| NEbinary (e1,Band,e2) when is_test ->
	    (* do not take [neg_test] into account, already done *)
	    (* AND succeeds only when both [e1] and [e2] succeed *)
	    let e1node = from_expr ~is_test start_node e1 in
	    let e2node = from_expr ~is_test e1node e2 in
	    (* oper *) add_opedge e2node enode;
	    (* if the and-test is free from side-effects, the order of
	       evaluation of [e1] and [e2] should not matter. We ensure this
	       by merging a path where [e1] is evaluated before [e2] with
	       a path where [e2] is evaluated before [e1]. *)
	    if pure e then
	      begin
		let e2node = from_expr ~is_test start_node e2 in
		let e1node = from_expr ~is_test e2node e1 in
		(* oper *) add_opedge e1node enode
	      end;
	    (* struct *) add_stedge enode [e1node; e2node]
	| NEbinary (e1,Bor,e2) when is_test ->
	    (* do not take [neg_test] into account, already done *)
	    (* OR succeeds whenever [e1] succeeds, 
	       or [e1] fails and [e2] succeeds *)
	    let e1node = from_expr ~is_test start_node e1 in
	    let e2node =
	      if pure e then
		from_expr ~is_test start_node e2
	      else
		let nege1node = 
		  from_expr ~is_test ~neg_test:true start_node e1 in
		from_expr ~is_test nege1node e2 
	    in
	    (* oper *) add_opedge e1node enode;
	    (* oper *) add_opedge e2node enode;
	    (* struct *) add_stedge enode [e1node; e2node]
	| NEbinary (e1,_,e2) ->
	    let e1node,e2node =
	      if Coptions.evaluation_order.Coptions.binary_left_to_right then
		let e1node = from_expr start_node e1 in
		let e2node = from_expr e1node e2 in
		(* oper *) add_opedge e2node enode;
	        e1node,e2node
    	      else
		let e2node = from_expr start_node e2 in
		let e1node = from_expr e2node e1 in
		(* oper *) add_opedge e1node enode;
	        e1node,e2node
	    in
	    (* struct *) add_stedge enode [e1node; e2node]
	| NEcall c ->
	    let expr_list = c.ncall_fun :: c.ncall_args in
	    let expr_list,list_reversal =
	      if Coptions.evaluation_order.Coptions.call_left_to_right then
		expr_list,false
	      else 
		List.rev expr_list,true
	    in
	    let anode,anodes = 
	      List.fold_left 
		(fun (startn,nlist) e -> 
		  let en = from_expr startn e in
		  en,en::nlist
		) (start_node,[]) expr_list
	    in
	    (* get back the function and the arguments in the correct order.
	       Take into account the fact that the call to [List.fold_left] 
	       already reverses the list. *)
	    let anodes = if list_reversal then anodes else List.rev anodes in
	    (* oper *) add_opedge anode enode;
	    (* struct *) add_stedge enode anodes
	| NEcond (e1,e2,e3) ->
	    let true_node = from_expr ~is_test:true start_node e1 in
	    let false_node =
	      from_expr ~is_test:true ~neg_test:true start_node e1 in
	    let e2node = from_expr true_node e2 in
	    let e3node = from_expr false_node e3 in
	    (* oper *) add_opedge e2node enode;
	    (* oper *) add_opedge e3node enode;
	    (* struct *) add_stedge enode [true_node; e2node; e3node]
    end;
    enode

  type context_descr = 
      { 
	(* target for [continue] statements in loops *)
	loop_starts : Node.t list; 
	(* target for [break] statements in loops and switches *)
	loop_switch_ends : Node.t list;
	(* context of \old logical terms *)
	function_begin : Node.t;
	(* target of return and last statement of function.
	   Set only if needed. *)
	function_end : Node.t option;
	(* targets for [goto] statements, and sources for [label] ones *)
	label_aliases : Node.t StringMap.t ref
      }

  (* Whenever a [goto] or [label] is found, this function is called 
     and returns a node that represents the label, and updates 
     globally all the contexts *)
  let update_context_for_label ctxt lab =
    try
      StringMap.find lab (!(ctxt.label_aliases))
    with Not_found ->
      let lab_node = create_node (Nintern InternOperational) in
      ctxt.label_aliases := StringMap.add lab lab_node (!(ctxt.label_aliases));
      lab_node

  let rec from_stat (ctxt : context_descr) start_node (s : nstatement) =
    (* structural statement node, for statement [s] *)
    let stsnode = create_node (Nstat s) in
    (* operational statement node for [s]. It is equal to [stsnode] in the cases
       where the control flow ends up in the node representing the statement. *)
    let opsnode = match s.nst_node with
      | NSdecl _ -> 
	  (* A declaration should be put in the operational graph before 
	     the statements that follow it. Since these statements are
	     a sub-field of the declaration, we create an internal node 
	     to serve in place of the declaration as end-node. *)
	  create_node (Nintern InternOperational)
      | _ -> stsnode
    in 
    begin
      match s.nst_node with
	| NSnop | NSlogic_label _ ->
	    (* oper *) add_opedge start_node opsnode
	| NSassert p ->
	    let pnode = from_pred ~is_assert:true p in
	    (* oper *) add_opedge start_node pnode;
	    (* oper *) add_opedge pnode opsnode;
	    (* logic *) add_logedge stsnode [pnode];
	    (* assert node is -only- its self-end *)
	    (* logic *) add_begedge stsnode ctxt.function_begin;
	    (* logic *) add_endedge stsnode opsnode
	| NSassume p ->
	    let pnode = from_pred ~is_assume:true p in
	    (* oper *) add_opedge start_node pnode;
	    (* oper *) add_opedge pnode opsnode;
	    (* logic *) add_logedge stsnode [pnode];
	    (* assume node is -only- its self-end *)
	    (* logic *) add_begedge stsnode ctxt.function_begin;
	    (* logic *) add_endedge stsnode opsnode
	| NSexpr e -> 
	    let enode = from_expr start_node e in
	    (* oper *) add_opedge enode opsnode;
	    (* struct *) add_stedge stsnode [enode]
	| NSif (e,s1,s2) ->
	    let then_node = from_expr ~is_test:true start_node e in
	    let else_node =
	      from_expr ~is_test:true ~neg_test:true start_node e in
	    let sts1node,ops1node = from_stat ctxt then_node s1 in
	    let sts2node,ops2node = from_stat ctxt else_node s2 in
	    (* oper *) add_opedge ops1node opsnode;
	    (* oper *) add_opedge ops2node opsnode;
	    (* struct *) add_stedge stsnode [then_node; sts1node; sts2node]
	| NSwhile (a,e,s1) ->
	    (* source of backward edge in loop *)
	    let bwd_node = create_node (Nintern InternOperationalBwdSrc) in
	    (* node that merges backward and upward flows *)
	    let mrg_node = create_node (Nintern InternOperationalBwdDest) in
	    (* connect backward edge *)
	    (* oper *) add_opedge start_node mrg_node;
	    (* oper *) add_backedge bwd_node mrg_node;
	    let loop_effect = 
	      Ceffect.ef_union (Ceffect.statement ~with_local:true s1)
		(Ceffect.expr ~with_local:true e) in
	    let write_vars =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_var) in
	    let write_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_under_pointer)
	    in
	    let read_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.reads_under_pointer)
	    in
	    let writes = { write_vars = write_vars; 
			   read_under_pointers = read_under_pointers;
			   write_under_pointers = write_under_pointers; } in
	    let anode,invnode_opt,assinvnode_opt = from_annot a writes in
	    let loop_node = match invnode_opt,assinvnode_opt with
	      | None,None -> mrg_node
	      | Some inode,None | None,Some inode ->
		  (* oper *) add_opedge mrg_node inode;
		  inode
	      | Some invnode,Some assinvnode ->
		  (* assume part of invariant has no successor *)
		  (* oper *) add_opedge mrg_node assinvnode;
		  (* oper *) add_opedge mrg_node invnode;
		  (* logic *) add_invedge invnode assinvnode;
		  (* logic *) add_invedge assinvnode invnode;
		  invnode
	    in
	    let test_node = from_expr ~is_test:true loop_node e in
	    let stop_node = 
	      from_expr ~is_test:true ~neg_test:true loop_node e in
	    let upd_ctxt =
	      { ctxt with 
		  loop_starts = bwd_node :: ctxt.loop_starts;
		  loop_switch_ends = opsnode :: ctxt.loop_switch_ends } in
            (* widening node is first node -in- the loop *)
            let widen_node = create_node (Nwiden { count = 0 }) in 
            (* oper *) add_opedge test_node widen_node;
	    let sts1node,ops1node = from_stat upd_ctxt widen_node s1 in
	    (* oper *) add_opedge ops1node bwd_node; (* before [e]'s eval *)
	    (* oper *) add_opedge stop_node opsnode; (* after [e]'s eval *)
	    (* struct *) add_stedge stsnode [test_node; sts1node];
	    (* logic *) add_logedge stsnode [anode];
	    (* the logical "annot" node is linked to the start and end nodes *)
	    (* [bwd_node] is the end node of the loop *)
	    (* logic *) add_begedge anode ctxt.function_begin;
	    (* logic *) add_endedge anode bwd_node
	| NSdowhile (a,s1,e) ->
	    (* source of backward edge in loop *)
	    let bwd_node = create_node (Nintern InternOperationalBwdSrc) in
	    (* node that merges backward and upward flows *)
	    let mrg_node = create_node (Nintern InternOperationalBwdDest) in
	    (* connect backward edge *)
	    (* oper *) add_opedge start_node mrg_node;
	    (* oper *) add_backedge bwd_node mrg_node;
	    let fwd_node = create_node (Nintern InternOperational) in
	    let upd_ctxt =
	      { ctxt with 
		  loop_starts = fwd_node :: ctxt.loop_starts;
		  loop_switch_ends = opsnode :: ctxt.loop_switch_ends } in
            (* widening node is first node -in- the loop *)
            let widen_node = create_node (Nwiden { count = 0 }) in 
            (* oper *) add_opedge mrg_node widen_node;
	    let sts1node,ops1node = from_stat upd_ctxt widen_node s1 in
	    let loop_effect = 
	      Ceffect.ef_union (Ceffect.statement ~with_local:true s1)
		(Ceffect.expr ~with_local:true e) in
	    let write_vars =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_var) in
	    let write_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_under_pointer)
	    in
	    let read_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.reads_under_pointer)
	    in
	    let writes = { write_vars = write_vars; 
			   read_under_pointers = read_under_pointers;
			   write_under_pointers = write_under_pointers; } in
	    let anode,invnode_opt,assinvnode_opt = from_annot a writes in
	    let loop_node = match invnode_opt,assinvnode_opt with
	      | None,None -> fwd_node
	      | Some inode,None | None,Some inode ->
		  (* oper *) add_opedge fwd_node inode;
		  inode
	      | Some invnode,Some assinvnode ->
		  (* assume part of invariant has no successor *)
		  (* oper *) add_opedge fwd_node assinvnode;
		  (* oper *) add_opedge fwd_node invnode;
		  (* logic *) add_invedge invnode assinvnode;
		  (* logic *) add_invedge assinvnode invnode;
		  invnode
	    in
	    let test_node = from_expr ~is_test:true loop_node e in
	    let stop_node = 
	      from_expr ~is_test:true ~neg_test:true loop_node e in
	    (* oper *) add_opedge ops1node fwd_node;
	    (* oper *) add_opedge test_node bwd_node;(* before [s1]'s eval *)
	    (* oper *) add_opedge stop_node opsnode;
	    (* struct *) add_stedge stsnode [sts1node; test_node];
	    (* logic *) add_logedge stsnode [anode];
	    (* the logical "annot" node is linked to the start and end nodes *)
	    (* [bwd_node] is the end node of the loop *)
	    (* logic *) add_begedge anode ctxt.function_begin;
	    (* logic *) add_endedge anode bwd_node
	| NSfor (a,einit,etest,eincr,s1) ->
	    (* source of backward edge in loop *)
	    let bwd_node = create_node (Nintern InternOperationalBwdSrc) in
	    (* node that merges backward and upward flows *)
	    let mrg_node = create_node (Nintern InternOperationalBwdDest) in
	    (* connect backward edge *)
	    let einit_node = from_expr start_node einit in
	    (* oper *) add_opedge einit_node mrg_node;
	    (* oper *) add_backedge bwd_node mrg_node;
	    let loop_effect = 
	      Ceffect.ef_union (Ceffect.expr ~with_local:true etest)
		(Ceffect.ef_union (Ceffect.statement ~with_local:true s1)
		   (Ceffect.expr ~with_local:true eincr))
	    in
	    let write_vars =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_var) in
	    let write_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.assigns_under_pointer)
	    in
	    let read_under_pointers =
	      HeapVarSet.elements (loop_effect.Ceffect.reads_under_pointer)
	    in
	    let writes = { write_vars = write_vars; 
			   read_under_pointers = read_under_pointers;
			   write_under_pointers = write_under_pointers; } in
	    let anode,invnode_opt,assinvnode_opt = from_annot a writes in
	    let loop_node = match invnode_opt,assinvnode_opt with
	      | None,None -> mrg_node
	      | Some inode,None | None,Some inode ->
		  (* oper *) add_opedge mrg_node inode;
		  inode
	      | Some invnode,Some assinvnode ->
		  (* assume part of invariant has no successor *)
		  (* oper *) add_opedge mrg_node assinvnode;
		  (* oper *) add_opedge mrg_node invnode;
		  (* logic *) add_invedge invnode assinvnode;
		  (* logic *) add_invedge assinvnode invnode;
		  invnode
	    in
	    let test_node = from_expr ~is_test:true loop_node etest in
	    let stop_node = 
	      from_expr ~is_test:true ~neg_test:true loop_node etest in
	    let fwd_node = create_node (Nintern InternOperational) in
	    let upd_ctxt =
	      { ctxt with 
		  loop_starts = fwd_node :: ctxt.loop_starts;
		  loop_switch_ends = opsnode :: ctxt.loop_switch_ends } in
            (* widening node is first node -in- the loop *)
            let widen_node = create_node (Nwiden { count = 0 }) in 
            (* oper *) add_opedge test_node widen_node;
	    let sts1node,ops1node = from_stat upd_ctxt widen_node s1 in
	    let eincr_node = from_expr fwd_node eincr in
	    (* oper *) add_opedge ops1node fwd_node;
	    (* oper *) add_opedge eincr_node bwd_node; (* before [etest] *)
	    (* oper *) add_opedge stop_node opsnode; (* after [etest]'s eval *)
	    (* struct *) add_stedge stsnode [einit_node; test_node;
					     eincr_node; sts1node];
	    (* logic *) add_logedge stsnode [anode];
	    (* the logical "annot" node is linked to the start and end nodes *)
	    (* [bwd_node] is the end node of the loop *)
	    (* logic *) add_begedge anode ctxt.function_begin;
	    (* logic *) add_endedge anode bwd_node
	| NSblock sl ->
	    let (opbnode,stsnodes) = 
	      List.fold_left 
		(fun (startopnode,sts1nodes) s1 -> 
		   let sts1node,ops1node = from_stat ctxt startopnode s1 in
		   ops1node,sts1node::sts1nodes
		) (start_node,[]) sl in
	    let stsnodes = List.rev stsnodes in
	    (* oper *) add_opedge opbnode opsnode;
	    (* struct *) add_stedge stsnode stsnodes
	| NSreturn None ->
	    (* oper *) add_opedge start_node opsnode;
	    begin match ctxt.function_end with
	      | Some function_end ->
		  (* logic *) force_add_opedge stsnode function_end
	      | None -> ()
	    end
	| NSreturn (Some e) -> 
	    let enode = from_expr start_node e in
	    (* oper *) add_opedge enode opsnode;
	    (* struct *) add_stedge stsnode [enode];
	    begin match ctxt.function_end with
	      | Some function_end ->
		  (* logic *) force_add_opedge stsnode function_end
	      | None -> ()
	    end
	| NSbreak -> 
	    (* oper *) add_opedge start_node opsnode;
	    (* oper *) force_add_opedge opsnode (List.hd ctxt.loop_switch_ends);
	| NScontinue -> 
	    (* oper *) add_opedge start_node opsnode;
	    (* Originally this edge was a backward one, crucial to make 
	       the topological walk work properly. Not the case anymore since
	       loops have 2 special nodes for the back edge. *)
	    (* oper *) add_opedge opsnode (List.hd ctxt.loop_starts)
	| NSgoto (_,lab) ->
            (* no problem of widening here since only forward gotos are 
	       accepted. Otherwise we should add a widening node in the induced
	       loop. *)
	    (* oper *) add_opedge start_node opsnode;
	    let label_node = update_context_for_label ctxt lab.label_info_name in
	    (* oper *) force_add_opedge opsnode label_node
	| NSspec (spc,s1) ->
	    let sts1node,ops1node = from_stat ctxt start_node s1 in
	    (* oper *) add_opedge ops1node opsnode;
	    (* struct *) add_stedge stsnode [sts1node];
	    (* ignore precondition for the moment *)
	    let spcnode,_ = from_spec spc in
	    (* logic *) add_logedge stsnode [spcnode];
	    (* the logical "spec" node is linked to the start and end nodes *)
	    (* logic *) add_begedge spcnode start_node;
	    (* logic *) add_endedge spcnode stsnode
	| NSlabel (lab,s1) ->
	    let label_node = update_context_for_label ctxt lab.label_info_name in
	    (* oper *) add_opedge start_node label_node;
	    let sts1node,ops1node = from_stat ctxt label_node s1 in
	    (* oper *) add_opedge ops1node opsnode;
	    (* struct *) add_stedge stsnode [sts1node]
	| NSdecl (_,_,None,s1) ->
	    (* here structural statement node and operational statement node
	       are not the same. [stsnode] is used in the operational graph
	       before nodes for [s1]. *)
	    (* oper *) add_opedge start_node stsnode;
	    let sts1node,ops1node = from_stat ctxt stsnode s1 in
	    (* oper *) add_opedge ops1node opsnode;
	    (* struct *) add_stedge stsnode [sts1node]
	| NSdecl (_,var,Some cinit,s1) ->
	    (* create an assignment expression node so that the treatment
	       of this initialization is shared with normal assignment *)
	    let rec first_expr cinit = match cinit with
	      | Iexpr e -> Some e
	      | Ilist clinit ->
		  List.fold_left (fun e_opt cinit ->
				    begin match e_opt with
				      | None -> first_expr cinit
				      | _ -> e_opt
				    end) None clinit
	    in
	    let base_expr = match cinit with
	      | Iexpr e -> e
	      | Ilist il -> 
		  begin match first_expr (Ilist il) with
		    | None -> 
			(* initialization list cannot be totally empty *)
			assert false 
		    | Some e -> e
		  end 
	    in
	    (* to be matched with [decode_decl_list] above *)
	    let rec encode_decl_list cinit = match cinit with
	      | Iexpr e -> e
	      | Ilist clinit ->
		  (* special encoding of [Ilist il] in the form of a fake call
		     to [NEnop] so that:
		     - assignments in the initialization expression are taken 
		     into account
		     - decoding is easy and safe *)
		  let encoded_clinit = List.map encode_decl_list clinit in
		  let fake_caller = { base_expr with nexpr_node = NEnop } in
		  let fake_call = 
		    { ncall_fun = fake_caller;
		      ncall_args = encoded_clinit;
		      ncall_zones_assoc = [] } in
		  { base_expr with nexpr_node = NEcall fake_call }
	    in
	    let var_expr = 
	      { base_expr with nexpr_node = NEvar (Var_info var) } in
	    let encoded_expr = match cinit with
	      | Iexpr e -> e
	      | Ilist il -> encode_decl_list cinit in
	    let explicit_assign = NEassign (var_expr,encoded_expr) in
	    let explicit_assign = 
	      { base_expr with nexpr_node = explicit_assign } in
	    (* here structural statement node and operational statement node
	       are not the same. [stsnode] is used in the operational graph
	       before nodes for [s1]. *)
	    (* oper *) add_opedge start_node stsnode;
	    let enode = from_expr stsnode explicit_assign in
	    let sts1node,ops1node = from_stat ctxt enode s1 in
	    (* oper *) add_opedge ops1node opsnode;
	    (* struct *) add_stedge stsnode [enode;sts1node]
	| NSswitch (e,_,cases) -> 
	    let enode = from_expr start_node e in
	    let upd_ctxt =
	      { ctxt with 
		  loop_starts = ctxt.loop_starts;
		  loop_switch_ends = opsnode :: ctxt.loop_switch_ends } in
	    let cnodes = List.map
	      (fun (_,sl) -> 
		 let opcnode,stclnodes =
		   List.fold_left 
		     (fun (startopnode,s1nodes) s1 -> 
			let sts1node,ops1node = 
			  from_stat upd_ctxt startopnode s1 in
			ops1node,sts1node::s1nodes
		     ) (enode,[]) sl
		 in
		 (* [cnode] is the node representing this [case], which is 
		    the same as the last statement in the list of statements
		    for this [case], when this list is not empty.
		    [clnodes] is the list of statements in this [case]. *)
		 opcnode,List.rev stclnodes
	      ) cases
	    in
	    let last_cnode =
	      List.fold_left 
		(fun prev_cnode (cnode,clnodes) -> 
		   begin match clnodes with
		     | [] -> (* no statement in this [case] *)
			 prev_cnode
		     | first_node::_ -> 
			 (* in case of fallthrough, control passes from 
			    the last statement of a [case] to the first 
			    statement of next [case].
			    If [prev_cnode] is a [break], call to 
			    [add_opedge] will do nothing. *)
			 (* oper *) add_opedge prev_cnode first_node;
			 cnode
		   end) enode cnodes
	    in
	    (* control flows from end of last case to end of switch *)
	    (* oper *) add_opedge last_cnode opsnode;
	    (* in absence of stable way of recognizing presence of [default]
	       case in switch (emptiness of integer map is not stable),
	       we consider the control always flows from start to end
	       of switch *)
	    (* oper *) add_opedge start_node opsnode;
	    let first_nodes = 
	      List.map (fun (_,clnodes) ->
			  (* special encoding so that [switch] can be seen as
			     a correct structured tree: a node [Nintern] is
			     created for every [case] and serves as 
			     intermediate *)
			  let fnode = 
			    create_node (Nintern InternStructural) in
			  begin match clnodes with
			    | [] -> ()
			    | _ -> (* struct *) add_stedge fnode clnodes
			  end;
			  fnode) cnodes
	    in
	    (* struct *) add_stedge stsnode (enode::first_nodes)
    end;
    stsnode,opsnode

  let rec from_decl d =
    if debug_more then Coptions.lprintf 
      "[from_decl] treating function %s@." d.name;
    (* clear all heap variables to avoid name clash between 
       different fucntions *)
    Hashtbl.clear Ceffect.heap_vars;
    let dnode = create_node (Ndecl d) in 
    (* In order to be able to transform any [ensures] part of 
       a function, we must create an end node for the function body,
       so that all parts of information are known under the same name 
       at that point. It is also used to provide a single entry point
       for backward propagation.
       All return statements and the last statement should be linked
       to this end node.
       This transformation may lead to a less precise analysis. 
       Maybe it could be restricted to functions that have an [ensures]
       part. It could be restricted further to functions that need it 
       to translate their [ensures] part.
    *)
    let end_node = create_node (Nintern InternOperational) in
    begin match d.s with
      | Some s ->
	  let start_ctxt = 
	    { 
	      loop_starts = []; 
	      loop_switch_ends = [];
	      function_begin = dnode;
	      function_end = Some end_node;
	      label_aliases = ref StringMap.empty
	    } in
	  let spcnode,reqnode_opt = from_spec ~is_function:true d.spec in
	  let func_node = match reqnode_opt with
	    | None -> dnode
	    | Some reqnode ->
		(* oper *) add_opedge dnode reqnode;
		reqnode
	  in
	  let stsnode,opsnode = from_stat start_ctxt func_node s in
	  (* oper *) add_opedge opsnode end_node;
	  (* struct *) add_stedge dnode [stsnode];
	  (* logic *) add_logedge dnode [spcnode];
	  (* the logical "spec" node is linked to the start and end nodes *)
	  (* logic *) add_begedge spcnode dnode; (* begin of decl is itself *)
	  (* logic *) add_endedge spcnode end_node
      | None -> ()
    end;
    (* clear all heap variables to avoid polluting real effects *)
    Hashtbl.clear Ceffect.heap_vars;
    dnode,end_node

  let from_file file =
    internal_graph := Some (Self.create ());
    (* only return functions that should be verified *)
    let file = 
      List.filter (fun func -> Coptions.verify func.f.fun_name) file in
    List.map from_decl file

  let to_decl node =
    match get_node_kind node with
      | NKdecl -> get_decl node
      | _ -> failwith "[to_decl] should be called only on declaration"

  let to_file nodes =
    let decls = List.map to_decl nodes in
    internal_graph := None;
    decls

  let collect_vars () =
    fold_operational Forward ~roots:[]
      (fun node (used_vars,decl_vars) ->
	 match get_node_kind node with
	   | NKexpr | NKtest | NKlvalue ->
	       (* only on code, not on logical part *)
	       let used_vars =
		 if expr_is_local_var node then
		   let var = termexpr_var_get node in
		   ILVarSet.add var used_vars
		 else
		   used_vars
	       in
	       used_vars,decl_vars
	   | NKstat ->
	       let decl_vars = 
		 if stat_is_decl node then
		   let var = decl_stat_get_var node in
		   ILVarSet.add var decl_vars
		 else
		   decl_vars
	       in
	       used_vars,decl_vars
	   | _ -> used_vars,decl_vars
      ) (ILVarSet.empty,ILVarSet.empty)

end

(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)

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

(** This module can be useful to store some information about different
 * elements of a function.
 *
 * {!module:PdgIndex.Signature} is used to store information
 * about function inputs/outputs either for the function itself or for its
 * calls. {!module:PdgIndex.Key} provides keys to identify the different
 * elements we want to speak about. {!module:PdgIndex.FctIndex} is the main
 * object that manages the stored information.
 *
 * This module is used for instance to store the relation between a function
 * elements and the nodes of its PDG, but it can also be used to store many
 * other things.*)

(** try to add in information while there is already something stored.
 * Should have used replace function *)
exception AddError

(** Some functions do not apply to call statements because the stored
 * information has a different type. *)
exception CallStatement

(** When we compare two things with different locations (no order) *)
exception Not_equal

(** What we call a [Signature] a mapping between keys that represent either a
 * function input or output, and some information.
 *)
module Signature : sig

  (** type of a signature where ['a] is the type of the information that we *
      want to store for each input/output. *)
  type 'a t

  (** key for input elements *)
  type in_key =
      private
    | InCtrl (** input control point *)
    | InNum of int (** parameters numbered from 1 *)
    | InImpl of Locations.Zone.t (** key for implicit inputs.
                                     Used in function signatures only *)

  type out_key =
      private
    | OutRet (** key for the output corresponding to the [return] *)
    | OutLoc of Locations.Zone.t (** key for output locations.
                                     used in call signatures only  *)

  (** a key represents either an input or an output of a function. *)
  type key = private In of in_key | Out of out_key

  val empty : 'a t
  (** build a new, empty signature *)

  val mk_undef_in_key : Locations.Zone.t -> in_key

  val cmp_in_key : in_key -> in_key -> int
  val cmp_out_key : out_key -> out_key -> int
  val equal_out_key : out_key -> out_key -> bool

  val find_info : 'a t -> key -> 'a
  val find_input : 'a t -> int -> 'a
  val find_in_ctrl : 'info t -> 'info
  val find_in_top : 'info t -> 'info
  val find_in_info : 'info t -> in_key -> 'info
  val find_out_ret : 'a t -> 'a
  val find_out_info : 'info t -> out_key -> 'info

  val fold : ('a -> key * 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_num_inputs : ('a -> int * 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_impl_inputs :
    ('a -> Locations.Zone.t * 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_matching_impl_inputs : Locations.Zone.t ->
    ('a -> Locations.Zone.t * 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_all_inputs : ('a -> in_key * 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_all_outputs : ('a -> out_key * 'b -> 'a) -> 'a -> 'b t -> 'a

  val pretty :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val pretty_key : Format.formatter -> key -> unit
  val pretty_in_key : Format.formatter -> in_key -> unit
  val pretty_out_key : Format.formatter -> out_key -> unit
end

(** The keys can be used to identify an element of a function.
    Have a look at the type [t] to know which kind of elements can be
    identified.*)
module Key : sig

  type key =
      private
    | SigKey of Signature.key
    (** key for an element of the function signature *)
    | VarDecl of Cil_types.varinfo   (** variable declaration *)
    | Stmt of Cil_types.stmt         (** any statement, except a call *)
    | CallStmt of Cil_types.stmt     (** call statement *)
    | Label of Cil_types.stmt * Cil_types.label (** program label *)
    | SigCallKey of Cil_types.stmt * Signature.key
    (** key for an element of a call signature *)

  include Datatype.S with type t = key

  val param_key : int -> t
  val implicit_in_key : Locations.Zone.t -> t
  val entry_point : t
  val top_input : t
  val output_key : t

  val out_from_key : Locations.Zone.t -> t

  val decl_var_key : Cil_types.varinfo -> t
  val label_key : Cil_types.stmt -> Cil_types.label -> t
  val stmt_key : Cil_types.stmt -> t

  val call_key : Cil_types.stmt -> t
  val call_input_key : Cil_types.stmt -> int -> t
  val call_output_key : Cil_types.stmt -> Locations.Zone.t -> t
  val call_outret_key : Cil_types.stmt -> t
  val call_ctrl_key : Cil_types.stmt -> t
  val call_topin_key : Cil_types.stmt -> t

  val stmt : t -> Cil_types.stmt option
  val call_from_id : Cil_types.stmt -> Cil_types.stmt

end


(** Mapping between the function elements we are interested in and some
  * information. Used for instance to associate the nodes with the statements,
  * or the marks in a slice.
  *)
module FctIndex : sig

  (** this type is used to build indexes between program objects and some
      information such as the PDG nodes or the slicing marks.
      - ['ni] if the type of the information to store for each element,
      - ['ci] if the type of the information that can be attached to call
      statements (calls are themselves composed of several elements, so ['ni]
      information stored for each of them (['ni Signature.t])) *)
  type ('ni, 'ci) t

  val create : int -> ('ni, 'ci) t
  val length : ('ni, 'ci) t -> int

  (** just copy the mapping *)
  val copy :  ('ni, 'ci) t ->  ('ni, 'ci) t

  (** merge the two indexes using given functions [merge_a] and [merge_b].
      These function are _not_ called when an element is in one index,
      but not the other. It is assumed that [merge_x x bot = x]. *)
  val merge : ('ni, 'ci) t ->  ('ni, 'ci) t ->
              ('ni -> 'ni -> 'ni) -> 
              ('ci -> 'ci -> 'ci) ->
              ('ni, 'ci) t

  (** get the information stored for the function signature *)
  val sgn :  ('ni, 'ci) t -> 'ni Signature.t

  (** find the information stored for the key. Cannot be used for
      [Key.CallStmt] keys because the type of the stored information is not the
      same. See [find_call] instead. *)
  val find_info :  ('ni, 'ci) t -> Key.t-> 'ni

  (** same than [find_info] except for call statements for which it gives the
      list of all the information in the signature of the call. *)
  val find_all :  ('ni, 'ci) t -> Key.t-> 'ni list

  (** Similar to [find_info] for a label *)
  val find_label: ('ni, 'ci) t -> Cil_types.label -> 'ni

  (** find the information stored for the call and its signature *)
  val find_call :
    ('ni, 'ci) t -> Cil_types.stmt -> 'ci option * 'ni Signature.t
  val find_call_key :  ('ni, 'ci) t -> Key.t -> 'ci option * 'ni Signature.t

  (** find the information stored for the call *)
  val find_info_call :  ('ni, 'ci) t -> Cil_types.stmt -> 'ci
  val find_info_call_key : ('ni, 'ci) t -> Key.t -> 'ci

  val fold_calls :
    (Cil_types.stmt -> 'ci option * 'ni Signature.t -> 'c -> 'c) ->
    ('ni, 'ci) t -> 'c -> 'c

  val fold : (Key.key -> 'ni -> 'a -> 'a) -> ('ni, 'ci) t -> 'a -> 'a

  (** store the information for the key.
      @raise AddError if there is already something stored. *)
  val add :  ('ni, 'ci) t -> Key.t-> 'ni -> unit
  (** store the information for the key. Replace the previously stored
      information if any. *)
  val add_or_replace :  ('ni, 'ci) t -> Key.t-> 'ni -> unit
  val add_info_call :
    ('ni, 'ci) t -> Cil_types.stmt -> 'ci -> replace:bool -> unit
  val add_info_call_key :  ('ni, 'ci) t -> Key.t -> 'ci -> replace:bool -> unit


  (** Structural destructor for unmarshaling *)
  val t_descr:
    ni:Structural_descr.t -> ci:Structural_descr.t -> Structural_descr.t

end

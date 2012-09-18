(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)


(** A bunch of generally useful functions.
    @plugin development guide
    @deprecated the whole module should migrate to {! Extlib}
*)

open Cil_types

(** composition of functions *)
val ($) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** [swap f x y] is [f y x] *)
val swap: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** Copy a hash table into another *)
val hash_copy_into: ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit


val list_drop : int -> 'a list -> 'a list
val list_droptail : int -> 'a list -> 'a list
val list_span: ('a -> bool) -> ('a list) -> 'a list * 'a list
val list_insert_by: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list
val list_head_default: 'a -> 'a list -> 'a
val list_iter3 : ('a -> 'b -> 'c -> unit) ->
  'a list -> 'b list -> 'c list -> unit
val get_some_option_list : 'a option list -> 'a list
val list_append: ('a list) -> ('a list) -> ('a list) (* tail-recursive append*)

(** Iterate over a list passing the index as you go *)
val list_iteri: (int -> 'a -> unit) -> 'a list -> unit
val list_mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

(** Like fold_left but pass the index into the list as well *)
val list_fold_lefti: ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc

(** Generates the range of integers starting with a and ending with b *)
val int_range_list : int -> int -> int list

(* Create a list of length l *)
val list_init : int -> (int -> 'a) -> 'a list

(** Find the first element in a list that returns Some *)
val list_find_first: 'a list -> ('a -> 'b option) -> 'b option

val list_last: 'a list -> 'a
  (** Returns the last element of a list; O(N), tail recursive.
      @raise Invalid_argument on an empty list. *)

(** mapNoCopy is like map but avoid copying the list if the function does not
 * change the elements *)

val mapNoCopy: ('a -> 'a) -> 'a list -> 'a list

val mapNoCopyList: ('a -> 'a list) -> 'a list -> 'a list

val filterNoCopy: ('a -> bool) -> 'a list -> 'a list


(** Join a list of strings *)
val joinStrings: string -> string list -> string

(** hasPrefix prefix str returns true with str starts with prefix *)
val hasPrefix: string -> string -> bool

(** Given a ref cell, produce a thunk that later restores it to its current
    value *)
val restoreRef: ?deepCopy:('a -> 'a) -> 'a ref -> unit -> unit

(** Given a hash table, produce a thunk that later restores it to its
    current value *)
val restoreHash: ?deepCopy:('b -> 'b) -> ('a, 'b) Hashtbl.t -> unit -> unit

(** Given an integer hash table, produce a thunk that later restores it to
    its current value. *)
val restoreIntHash: ?deepCopy:('b -> 'b) -> 'b Datatype.Int.Hashtbl.t -> unit -> unit

(** Given an array, produce a thunk that later restores it to its current
    value. *)
val restoreArray: ?deepCopy:('a -> 'a) -> 'a array -> unit -> unit


(** Given a list of thunks, produce a thunk that runs them all *)
val runThunks: (unit -> unit) list -> unit -> unit


val memoize: ('a, 'b) Hashtbl.t ->
            'a ->
            ('a -> 'b) -> 'b

(** Just another name for memoize *)
val findOrAdd: ('a, 'b) Hashtbl.t ->
            'a ->
            ('a -> 'b) -> 'b

val tryFinally:
    ('a -> 'b) -> (* The function to run *)
    ('b option -> unit) -> (* Something to run at the end. The None case is
                          * used when an exception is thrown *)
    'a -> 'b

val out_some : 'a option -> 'a
  (** @plugin development guide *)

(**
 * An accumulating for loop.
 *
 * Initialize the accumulator with init.  The current index and accumulator
 * from the previous iteration is passed to f.
 *)
val fold_for : init:'a -> lo:int -> hi:int -> (int -> 'a -> 'a) -> 'a

(************************************************************************
   Configuration
************************************************************************)
(** The configuration data can be of several types **)
type configData =
    ConfInt of int
  | ConfBool of bool
  | ConfFloat of float
  | ConfString of string
  | ConfList of configData list


(** Load the configuration from a file *)
val loadConfiguration: string -> unit

(** Save the configuration in a file. Overwrites the previous values *)
val saveConfiguration: string -> unit

(** Clear all configuration data *)
val clearConfiguration: unit -> unit

(** Set a configuration element, with a key. Overwrites the previous values *)
val setConfiguration: string -> configData -> unit

(** Find a configuration elements, given a key. Raises Not_found if it cannot
    * find it *)
val findConfiguration: string -> configData

(** Like findConfiguration but extracts the integer *)
val findConfigurationInt: string -> int

(** Looks for an integer configuration element, and if it is found, it uses
 * the given function. Otherwise, does nothing *)
val useConfigurationInt: string -> (int -> unit) -> unit

val findConfigurationFloat: string -> float
val useConfigurationFloat: string -> (float -> unit) -> unit

val findConfigurationBool: string -> bool
val useConfigurationBool: string -> (bool -> unit) -> unit

val findConfigurationString: string -> string
val useConfigurationString: string -> (string -> unit) -> unit

val findConfigurationList: string -> configData list
val useConfigurationList: string -> (configData list -> unit) -> unit


(************************************************************************)

(** Symbols are integers that are uniquely associated with names *)
type symbol = int

(** Get the name of a symbol *)
val symbolName: symbol -> string

(** Register a symbol name and get the symbol for it *)
val registerSymbolName: string -> symbol

(** Register a number of consecutive symbol ids. The naming function will be
 * invoked with indices from 0 to the counter - 1. Returns the id of the
 * first symbol created. The naming function is invoked lazily, only when the
 * name of the symbol is required. *)
val registerSymbolRange: int -> (int -> string) -> symbol


(** Make a fresh symbol. Give the name also, which ought to be distinct from
 * existing symbols. This is different from registerSymbolName in that it
 * always creates a new symbol. *)
val newSymbol: string -> symbol

(** Reset the state of the symbols to the program startup state *)
val resetSymbols: unit -> unit

(** Take a snapshot of the symbol state. Returns a thunk that restores the
 * state. *)
val snapshotSymbols: unit -> unit -> unit


(** Dump the list of registered symbols *)
val dumpSymbols: unit -> unit

(************************************************************************)

(** This has the semantics of (=) on OCaml 3.07 and earlier.  It can
   handle cyclic values as long as a structure in the cycle has a unique
   name or id in some field that occurs before any fields that have cyclic
   pointers. *)
val equals: 'a -> 'a -> bool

(* pretty-printing *)

open Format

(** [pretty sep print fmt l] pretty-prints the elements of [l] according
    to the formatting function [print] separated by [sep] on [fmt]
*)
val pretty_list:  (formatter -> unit) ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit

(** [pretty_list_del before after sep print fmt l] is the same as
    [pretty_list], but non-empty lists are enclosed between
    [before] and [after]
*)
val pretty_list_del:
  (formatter -> unit) ->(formatter -> unit) -> (formatter -> unit) ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit

val pretty_opt:
  (formatter -> 'a -> unit) -> formatter -> 'a option -> unit

val pretty_opt_nl:
  (formatter -> 'a -> unit) -> formatter -> 'a option -> unit

(** separator + breakable space *)
val space_sep: string -> formatter -> unit

(** forces newline *)
val nl_sep: formatter -> unit


(** Environment for placeholders in term to exp translation *)
type opaque_term_env = {
  term_lhosts: term_lhost Cil_datatype.Varinfo.Map.t;
  terms: term Cil_datatype.Varinfo.Map.t;
  vars: logic_var Cil_datatype.Varinfo.Map.t;
}

(** Environment for placeholders in exp to term translation *)
type opaque_exp_env = { exps: exp Cil_datatype.Varinfo.Map.t }

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

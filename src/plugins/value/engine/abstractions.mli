(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Registration and building of the analysis abstractions. *)

(** {2 Registration of abstractions.} *)

(** Dynamic registration of the abstractions to be used in an Eva analysis:
    - value abstractions, detailled in the {Abstract_value} signature;
    - location abstractions, detailled in the {Abstract_location} signature;
    - state abstractions, or abstract domains, detailled in {Abstract_domain}.
*)

(** Module types of value abstractions: either a single leaf module, or
    a compound of several modules described by a structure. *)
type 'v value =
  | Single of (module Abstract_value.Leaf with type t = 'v)
  | Struct of 'v Abstract.Value.structure

(** For the moment, all domains must use [precise_loc] as their location
    abstraction, and no new location abstraction can be registered for an
    analysis.
    If you need to build a new location abstraction, please contact us. *)
type precise_loc = Precise_locs.precise_location

(** Module type of a leaf domain over precise_loc abstraction. *)
module type leaf_domain = Abstract_domain.Leaf with type location = precise_loc

(** Module type of a functor building a leaf domain from a value abstraction.
    The resulting domain must use the input value as value abstraction. *)
module type domain_functor = functor
  (Value: Abstract.Value.External) -> (leaf_domain with type value = Value.t)

(** Type of domain to be registered: either a leaf module with ['v] as value
    abstraction, or a functor building a domain from any value abstraction. *)
type 'v domain =
  | Domain: (module leaf_domain with type value = 'v) -> 'v domain
  | Functor: (module domain_functor) -> _ domain

(** Abstraction to be registered. The name of each abstraction must be unique.
    The priority can be any integer; domains with higher priority are always
    processed first. The domains currently provided by Eva have priority ranging
    between 1 and 19, so a priority of 0 (respectively 20) ensures that a new
    domain is processed after (respectively before) the classic Eva domains. *)
type 'v abstraction =
  { name: string;      (** Name of the abstraction. Must be unique. *)
    priority: int;     (** Domains with higher priority are processed first. *)
    values: 'v value;  (** The value abstraction. *)
    domain: 'v domain; (** The domain over the value abstraction. *)
  }

(** Register an abstraction. The abstraction is used in an Eva analysis only if
    [enable ()] returns true at the start of the analysis.  *)
val register: enable:(unit -> bool) -> 'v abstraction -> unit

(** Register a dynamic abstraction: the abstraction is built by applying
    [make (configure ())] at the start of each analysis. *)
val dynamic_register:
  configure:(unit -> 'a option) -> make:('a -> 'v abstraction) -> unit

(** Value reduced product between two value abstractions, identified by their
    keys. *)
type ('a, 'b) value_reduced_product =
  'a Abstract.Value.key * 'b Abstract.Value.key * ('a -> 'b -> 'a * 'b)

(** Register a reduction function for a value reduced product. *)
val register_value_reduction: ('a, 'b) value_reduced_product -> unit


(** {2 Types used in the engine.} *)

(** The external signature of value abstractions, plus the reduction function
    of the reduced product. *)
module type Value = sig
  include Abstract.Value.External
  val reduce : t -> t
end

(** The three abstractions used in an Eva analysis. *)
module type S = sig
  module Val : Value
  module Loc : Abstract.Location.External with type value = Val.t
  module Dom : Abstract.Domain.External with type value = Val.t
                                         and type location = Loc.location
end

(** The three abstractions plus an evaluation engine for these abstractions. *)
module type Eva = sig
  include S
  module Eval: Evaluation.S with type state = Dom.t
                             and type value = Val.t
                             and type loc = Loc.location
                             and type origin = Dom.origin
end

(** Register a hook modifying the three abstractions after their building by
    the engine, before the start of each analysis. *)
val register_hook: ((module S) -> (module S)) -> unit

(** {2 Configuration of an analysis.} *)

(** Configuration defining the abstractions to be used in an analysis. *)
module Config : sig
  (** Flag for an abstraction. *)
  type flag = Flag: 'v abstraction -> flag

  (** A configuration is a set of flags, i.e. a set of enabled abstractions. *)
  include Set.S with type elt = flag

  (** Flags for the standard domains currently provided in Eva. *)

  val cvalue: flag
  val equality: flag
  val symbolic_locations: flag
  val gauges: flag
  val octagon: flag
  val bitwise: flag
  val inout: flag
  val sign: flag
  val traces: flag
  val printer: flag

  val default: t (** The default configuration of Eva. *)
  val legacy: t (** The configuration corresponding to the old "Value" analysis,
                    with only the cvalue domain enabled. *)
end

(** Creates the configuration according to the analysis parameters. *)
val configure: unit -> Config.t

(** Builds the abstractions according to a configuration. *)
val make: Config.t -> (module S)

(** Two abstractions are instantiated at compile time for the default and legacy
    configurations (which may be the same). *)

module Legacy : S
module Default : S

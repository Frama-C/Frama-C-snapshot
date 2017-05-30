(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Maps from bases to memory maps. The memory maps are those of the
    [Offsetmap] module.
    @plugin development guide *)

(** Contents of a variable when it is not present in the state.
    See function [default_contents] in the functor below *)
type 'a default_contents =
  | Bottom
  | Top of 'a
  | Constant of 'a
  | Other 

module Make_LOffset
  (V: sig
     include module type of Offsetmap_lattice_with_isotropy
     include Lattice_type.With_Top_Opt with type t := t
   end)
  (Offsetmap: module type of Offsetmap_sig
              with type v = V.t
              and type widen_hint = V.generic_widen_hint)
  (Default_offsetmap: sig
     val name: string
     (** Used to create different datatypes each time the functor is applied *)

     val default_offsetmap : Base.t -> Offsetmap.t Bottom.or_bottom
     (** Value returned when a map is queried, and the base is not present.
         [`Bottom] indicates that the base is never bound in such a map. *)

     val default_contents: V.t default_contents
     (** This function is used to optimize functions that add keys in a map,
         in particular when maintaining canonicity w.r.t. default contents.
         It describes the contents [c] of the offsetmap
         resulting from [default_offsetmap b]. The possible values are:
         - [Bottom] means that [c] is [V.bottom] everywhere, and furthermore
           that [V.bottom] has an empty concretization. We deduce from this
           fact that unmapped keys do not contribute to a join, and that
           [join c v] is never [c] as soon as [v] is not itself [v].
         - [Top] means that [c] is [V.top] everywhere. Thus unmapped keys
           have a default value more general than the one in a map where the
           key is bound.
         - [`Constant v] means that [c] is an offsetmap with a single interval
           containing [v] everywhere. [v] must be isotropic (in the sense
           of {!V.is_isotropic}).
         - [`Other] means that [default_offsetmap] returns something arbitrary.

         This function is only used on keys that change values. Thus it is
         safe to have [default_offsetmap] return something that do not
         match [default_contents] on constant keys.
     *)
  end):
  module type of Lmap_sig
    with type v = V.t
    and type widen_hint_base = V.generic_widen_hint
    and type offsetmap = Offsetmap.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

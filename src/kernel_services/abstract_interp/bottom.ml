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


module Type = struct

  type 'a or_bottom = [ `Value of 'a | `Bottom ]

  (* This monad propagates the `Bottom value if needed. *)
  let (>>-) t f = match t with
    | `Bottom  -> `Bottom
    | `Value t -> f t

  (* Use this monad if the following function returns a simple value. *)
  let (>>-:) t f = match t with
    | `Bottom  -> `Bottom
    | `Value t -> `Value (f t)

end

include Type

let non_bottom = function
  | `Value v -> v
  | `Bottom  -> assert false

let equal equal x y = match x, y with
  | `Bottom, `Bottom     -> true
  | `Value vx, `Value vy -> equal vx vy
  | _                    -> false

let is_included is_included x y = match x, y with
  | `Bottom, _           -> true
  | _, `Bottom           -> false
  | `Value vx, `Value vy -> is_included vx vy

let join join x y = match x, y with
  | `Value vx, `Value vy    -> `Value (join vx vy)
  | `Bottom, (`Value _ as v)
  | (`Value _ as v), `Bottom
  | (`Bottom as v), `Bottom -> v

let narrow narrow x y = match x, y with
  | `Value vx, `Value vy    -> narrow vx vy
  | `Bottom, `Value _
  | `Value _, `Bottom
  | `Bottom, `Bottom -> `Bottom

let pretty pretty fmt = function
  | `Bottom  -> Format.fprintf fmt "Bottom"
  | `Value v -> pretty fmt v


let counter = ref 0

module Make_Datatype
    (Domain: Datatype.S)
  =
  Datatype.Make (
  struct
    include Datatype.Serializable_undefined
    type t = Domain.t or_bottom
    let () = incr counter
    let name = Domain.name ^ "+bottom(" ^ string_of_int !counter ^ ")"
    let reprs = [`Bottom; `Value (List.hd Domain.reprs)]
    let structural_descr = Structural_descr.t_unknown
    (* Structural_descr.t_sum [| [| Domain.packed_descr |] |] *)

    let equal a b = match a, b with
      | `Bottom, `Bottom   -> true
      | `Value v, `Value w -> Domain.equal v w
      | _, _               -> false

    let compare a b = match a, b with
      | `Bottom, `Bottom   -> 0
      | `Bottom, _         -> -1
      | _, `Bottom         -> 1
      | `Value v, `Value w -> Domain.compare v w

    let hash = function
      | `Bottom  -> 0
      | `Value v -> Domain.hash v

    let rehash = Datatype.identity

    let copy = function
      | `Bottom  -> `Bottom
      | `Value v -> `Value (Domain.copy v)

    let pretty fmt = function
      | `Bottom  -> Format.fprintf fmt "Bottom"
      | `Value v -> Domain.pretty fmt v

    let mem_project = Datatype.never_any_project
  end)


module Bound_Lattice
    (Lattice: Lattice_type.Join_Semi_Lattice)
= struct
  include Make_Datatype (Lattice)

  let bottom = `Bottom
  let join = join Lattice.join
  let is_included = is_included Lattice.is_included
end


let bot_of_list = function
  | [] -> `Bottom
  | l  -> `Value l

let list_of_bot = function
  | `Bottom  -> []
  | `Value l -> l

let add_to_list elt list = match elt with
  | `Bottom    -> list
  | `Value elt -> elt :: list


module Top = struct

  type 'a or_top_bottom = [ 'a or_bottom | `Top ]

  let join vjoin x y = match x, y with
    | `Top, _ | _, `Top -> `Top
    | (#or_bottom as x), (#or_bottom as y) -> join vjoin x y

  let narrow vnarrow x y = match x, y with
    | `Top, v | v, `Top -> v
    | (#or_bottom as x), (#or_bottom as y) ->
      (narrow vnarrow x y :> _ or_top_bottom)

end

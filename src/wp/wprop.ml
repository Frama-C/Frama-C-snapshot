(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- WP Internal State                                                  --- *)
(* -------------------------------------------------------------------------- *)

module WP = State_builder.Ref
  (Datatype.Unit)
  (struct
     let name = "WP"
     let dependencies = [Ast.self]
     let default () = ()
   end)

(* -------------------------------------------------------------------------- *)
(* --- Indexed Interface                                                  --- *)
(* -------------------------------------------------------------------------- *)

type property =
  | Later of Property.t
  | Proxy of Property.t * Emitter.t * Property.t list

module type Info =
sig
  include State_builder.Info_with_size
  type key
  val property : key -> property
end

module type Indexed =
sig
  type key
  val mem : key -> bool
  val property : key -> Property.t
  val add_hook : (key -> Property.t -> unit) -> unit
end

module type Indexed2 =
sig
  type key1
  type key2
  val mem : key1 -> key2 -> bool
  val property : key1 -> key2 -> Property.t
  val add_hook : (key1 -> key2 -> Property.t -> unit) -> unit
end

(* -------------------------------------------------------------------------- *)
(* --- Index-1 Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

module Indexed
  (Key:Datatype.S_with_collections)
  (Info:Info with type key = Key.t) =
struct

  type key = Key.t

  module H = State_builder.Hashtbl(Key.Hashtbl)(Property)(Info)

  let hooks = ref []
  let add_hook f = hooks := !hooks @ [f]

  let mem = H.mem

  let property (key:key) =
    try H.find key
    with Not_found ->
      let ip =
	match Info.property key with
	  | Later ip -> ip
	  | Proxy(ip,emitter,ips) -> 
	      Property_status.logical_consequence emitter ip ips ; ip
      in
      List.iter (fun f -> f key ip) !hooks ; 
      H.add key ip ; ip

end

(* -------------------------------------------------------------------------- *)
(* --- Index-2 Wrapper                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Indexed2
  (Key1:Datatype.S_with_collections)
  (Key2:Datatype.S_with_collections)
  (Info:Info with type key = Key1.t * Key2.t) =
struct

  module P = Datatype.Pair_with_collections(Key1)(Key2)
    (struct
       let module_name = Info.name
     end)
  module I = Indexed(P)(Info)

  type key1 = Key1.t
  type key2 = Key2.t

  let mem a b = I.mem (a,b)
  let property a b = I.property (a,b)
  let add_hook f = I.add_hook (fun (a,b) -> f a b)

end

(* -------------------------------------------------------------------------- *)

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

module type WeakHashable =
sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
  val id : string
end

module type S = sig
  type data
  type t

  val create : int -> t
  val merge : t -> data -> data
  val iter : t -> (data -> unit) -> unit
  val clear : t -> unit
  val release : t -> unit
  val shallow_copy : t -> t
  val addr : t -> int
  val overwrite : old:t -> fresh:t -> unit
  val pretty_debug : Format.formatter -> t -> int -> unit
end;;

module MakeBig(H:WeakHashable) =
struct
  module W = Weak.Make(H)
  type t = W.t ref
  let addr _t = 0
  type data = H.t
  let create c = ref (W.create c)
  let merge t d = W.merge !t d
  let iter t f = W.iter f (!t)
  let clear t = W.clear (!t)
  let release _t = ()
  let pretty_debug _ = assert false
  let shallow_copy t = ref !t
  let overwrite ~old ~fresh =
    old := !fresh
end



let () =
  let gc_params = Gc.get () in
  Gc.set
    { gc_params with
      Gc.minor_heap_size = 1 lsl 18 ;
      major_heap_increment = 1 lsl 22;
      (* space_overhead = 40 ; max_overhead = 100 *)
    };

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

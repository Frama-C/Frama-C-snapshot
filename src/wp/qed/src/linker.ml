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

open Format
open Logic
open Plib

module B = Map.Make(String)
module S = Set.Make(String)

(* -------------------------------------------------------------------------- *)
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let is_letter c = c = '_'
                  || ('a' <= c && c <= 'z') 
                  || ('a' <= c && c <= 'z') 
                  || ('0' <= c && c <= '9')

let is_ident op = is_letter op.[String.length op - 1]

let ident base =
  let p = Buffer.create 32 in
  for i=0 to String.length base - 1 do
    let c = base.[i] in
    if is_letter c then Buffer.add_char p c
  done ;
  Buffer.contents p

(* -------------------------------------------------------------------------- *)
(* --- Allocation                                                         --- *)
(* -------------------------------------------------------------------------- *)

type allocator = {
  mutable base : int B.t ;
  mutable domain : S.t ;
}

let rec lookup d a k =
  let s = Printf.sprintf "%s_%d" a k in
  if S.mem s d then lookup d a (succ k) else s,k

let fresh m a =
  let k0 = try B.find a m.base with Not_found -> 0 in
  let s,k = lookup m.domain a k0 in
  m.base <- B.add a (succ k) m.base ;
  m.domain <- S.add s m.domain ; s

let declare m x = m.domain <- S.add x m.domain

let allocator () = { domain = S.empty ; base = B.empty }

let copy m = { domain = m.domain ; base = m.base }

(* -------------------------------------------------------------------------- *)
(* --- Linker                                                             --- *)
(* -------------------------------------------------------------------------- *)

class type ['a,'idx] linker = 
  object
    method lock  : unit
    method clear : unit
    method push  : 'idx
    method pop   : 'idx -> unit
    method mem   : 'a -> bool
    method find  : 'a -> string
    method link  : 'a -> string -> unit
    method print : 'a printer
    method alloc : basename:string -> 'a -> string
    method alloc_with : allocator -> unit
    method reserve : basename:string -> string
    method bind_reserved : 'a -> string -> unit
  end

module Link(A : Symbol) =
struct
  module I = Map.Make(A)

  type index = string I.t

  class alinker =
    object(self)
      val mutable alloc : allocator option = None
      val mutable index : index = I.empty

      method push = index
      method pop idx = index <- idx

      method lock = alloc <- None
      method alloc_with allocator = alloc <- Some allocator
      method clear = index <- I.empty
      method find a = I.find a index
      method mem a = I.mem a index
      method print fmt a =
        try pp_print_string fmt (I.find a index)
        with Not_found -> fprintf fmt "<%a>" A.pretty a
      method link a f = 
        match alloc with
        | None -> failwith "Qed.Linker.Locked"
        | Some allocator ->
            declare allocator f ; 
            index <- I.add a f index
      method alloc ~basename a =
        let s = self#reserve ~basename in
        index <- I.add a s index ; s
      method reserve ~basename =
        match alloc with
        | None -> failwith "Qed.Linker.Locked"
        | Some allocator -> fresh allocator basename
      method bind_reserved a s = index <- I.add a s index
    end

  let linker () = (new alinker :> (A.t,index) linker)
end

(* -------------------------------------------------------------------------- *)
(* --- Records                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Record(T : Logic.Term) =
struct

  module Smap = Map.Make
      (struct
        type t = T.Field.t list
        let compare = Hcons.compare_list T.Field.compare
      end)

  module Amap = Map.Make(T.ADT)

  type t = {
    mutable fields : T.Field.t list Amap.t ;
    mutable record : T.ADT.t Smap.t ;
  }

  let create () = { fields = Amap.empty ; record = Smap.empty }

  let register m adt fs =
    begin
      m.fields <- Amap.add adt fs m.fields ;
      m.record <- Smap.add fs adt m.record ;
    end

  let get_fields m a = Amap.find a m.fields
  let get_record m s = Smap.find s m.record

end

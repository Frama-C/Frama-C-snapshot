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
(* --- Topological Sort                                                   --- *)
(* -------------------------------------------------------------------------- *)

module type Element =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module Make(E : Element) =
struct

  module H = Hashtbl.Make(E)

  type succ = (E.t -> unit) -> E.t -> unit
  type root = (E.t -> unit) -> unit

  type t = {
    root : int H.t ;
    comp : int H.t ;
    succ : (E.t -> unit) -> E.t -> unit ;
    mutable stack : (int * E.t) list ;
    mutable ndfs : int ;
    mutable ncomp : int ;
  }

  let create f n = {
    root = H.create n ;
    comp = H.create n ;
    succ = f ;
    stack = [] ;
    ndfs = 0 ;
    ncomp = 0 ;
  }

  let rec pop g n = function
    | (k, w) :: l when k > n -> 
        H.add g.comp w g.ncomp ;
        pop g n l
    | l -> l

  let push g v n = g.stack <- (n,v) :: g.stack

  let rec visit g v =
    if not (H.mem g.root v) then
      begin
        let n = g.ndfs in 
        g.ndfs <- succ n ; 
        H.add g.root v n ;
        g.succ 
          (fun w -> 
             visit g w ;
             if not (H.mem g.comp w) then 
               let r_v = H.find g.root v in
               let r_w = H.find g.root w in
               H.replace g.root v (min r_v r_w)
          ) v ;
        if H.find g.root v = n then 
          begin
            H.add g.comp v g.ncomp ;
            g.stack <- pop g n g.stack ;
            g.ncomp <- succ g.ncomp ;
          end
        else 
          push g v n
      end

  let components ~succ ~root ?(size=997) () =
    let g = create succ size in
    root (visit g) ;
    let t = Array.make g.ncomp [] in
    H.iter
      (fun v i -> t.(i) <- v::t.(i))
      g.comp ; t

end

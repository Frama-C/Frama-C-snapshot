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
(* --- Control Flow Graph Library                                         --- *)
(* -------------------------------------------------------------------------- *)

open CfgTypes

module Make(T : Transition) =
struct

  module T = T

  let nil = (-1)
  let is_nil k = k < 0

  type node = int
  type transition = node T.transition
      
  type cfg = cell Vector.t
  and cell = {
    mutable pred : node list ;
    mutable next : node T.transition ;
  }

  let create = Vector.create 

  let size cfg = Vector.size cfg

  let rec remove p = function
    | [] -> assert false
    | q::qs -> if p==q then qs else q :: remove p qs

  let set_pred cfg n t =
    T.iter (fun p ->
      let cell = Vector.get cfg p in
      cell.pred <- n :: cell.pred
    ) t

  let del_pred cfg n t =
    T.iter (fun p ->
      let cell = Vector.get cfg p in
      cell.pred <- remove n cell.pred
    ) t

  let node cfg = Vector.addi cfg { next=T.empty ; pred=[] }

  let set cfg n t = 
    begin
      let cell = Vector.get cfg n in
      del_pred cfg n cell.next ;
      cell.next <- t ;
      set_pred cfg n t ;
    end

  let add cfg t = let n = node cfg in set cfg n t ; n

  let next cfg p = (Vector.get cfg p).next
  let pred cfg p = (Vector.get cfg p).pred
  let iter f cfg = Vector.iteri (fun n c -> f n c.next) cfg
  let iter_succ cfg f p = T.iter f (Vector.get cfg p).next
  let iter_pred cfg f p = List.iter f (Vector.get cfg p).pred
  let succ cfg p =
    let qs = ref [] in
    iter_succ cfg (fun q -> qs := q :: !qs) p ; !qs

  let id p = p
  let nid p = p

  type marks = Bitvector.t
  let marks cfg = Bitvector.create (size cfg)
  let once = Bitvector.once 

  type dotter = Format.formatter
  type pp_cfg = dotter -> node -> transition -> unit

  let pp_node fmt n msg =
    Format.fprintf fmt "  n%d [" n ;
    Format.kfprintf
      (fun fmt -> Format.fprintf fmt "] ;@.")
      fmt msg
  let pp_edge fmt n p msg =
    Format.fprintf fmt "  n%d -> n%d [" n p ;
    Format.kfprintf
      (fun fmt -> Format.fprintf fmt "] ;@.")
      fmt msg
      
  let dot cfg label file =
    let name , format =
      let base = Filename.chop_extension file in
      let n = String.length file in
      let p = Pervasives.succ (String.length base) in
      Filename.basename base , String.sub file p (n-p)
    in
    let fdot,out = 
      if format = "dot" then
	(file , open_out file)
      else
	Filename.open_temp_file "cfg" ".dot" in
    let fmt = Format.formatter_of_out_channel out in
    let s =
      try
	Format.fprintf fmt "digraph %S {@\n" name ;
	Format.fprintf fmt "  rankdir = TB ;@\n" ;
	Format.fprintf fmt "  node [ style = filled, shape = box ] ;@\n" ;
	iter (fun n t -> label fmt n t) cfg ;
	Format.fprintf fmt "}@." ;
	close_out out ;
	if format = "dot" then 0 else
	  let cmd = Printf.sprintf "dot -T%s %s > %s" format fdot file
	  in Sys.command cmd
      with e -> 
	Format.pp_print_flush fmt () ;
	close_out out ; 
	raise e
    in if s <> 0 then failwith ("CfgLib.dot exit " ^ string_of_int s)

end

module Attr(C : Cfg) =
struct
  
  type 'a t = 'a * 'a Vector.t
  let create cfg default = 
    let m = Vector.create () in
    Vector.resize m (C.size cfg) ; 
    (default,m)

  let get (default,m) nd = Vector.find m ~default (C.id nd)
  let set (default,m) nd v = Vector.update m ~default (C.id nd) v
  let merged (default,m) nd f = 
    let k = C.id nd in
    let v = f (Vector.find m ~default k) in
    Vector.update m ~default k v ; v
  let merge m nd f = ignore (merged m nd f)

  let merge_op f m nd v = merge m nd (fun w -> f w v)
  let merged_op f m nd v = merged m nd (fun w -> f w v)

  let iter f (_,m) = Vector.iteri (fun k v -> f (C.nid k) v) m

end

module Labels(C : Cfg)(H : Hashtbl.S) =
struct
  open C
  type t = cfg * node H.t 
  type label = H.key

  let create ?(size=31) cfg = cfg , H.create size
  let set_label (_,idx) = H.replace idx

  let label (cfg,idx) e = 
    try H.find idx e
    with Not_found -> 
      let n = node cfg in H.add idx e n ; n

  let iter f (_,idx) = H.iter f idx
end

module Transform(A : Cfg)(B : Cfg) =
struct
  type t = A.cfg * B.cfg * B.node array

  let create a b = a , b , Array.create (A.size a) B.nil
  let set_image (_,_,idx) a b = idx.(A.id a) <- b

  let image (_,cfg,idx) a =
    let i = A.id a in
    let n = idx.(i) in
    if B.is_nil n then
      let n = B.node cfg in
      idx.(i) <- n ; n
    else n

  let copy ((a,b,_) as w) f =
    A.iter (fun n t -> B.set b (image w n) (f n t)) a
end

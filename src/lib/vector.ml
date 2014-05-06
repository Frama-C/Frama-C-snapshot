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


(* -------------------------------------------------------------------------- *)
(* --- Extensible Array                                                   --- *)
(* -------------------------------------------------------------------------- *)

type 'a t = {
  dumb : 'a ;
  mutable elt : 'a array ;
  mutable top : int ;
}

let create () = {
  dumb = Obj.magic (ref ()) ;
  top = 0 ; (* Invariant top <= length elt *)
  elt = [| |] ; (* Invariant elt.(k) == dump for top <= k *)
}

(* Requires n > length elt *)
let do_grow w n =
  begin
    let elt = Array.create n w.dumb in
    Array.blit w.elt 0 elt 0 w.top ;
    w.elt <- elt ;
  end

(* Requires 0 <= n < length elt *)
let do_shrink w n =
  begin
    w.elt <- Array.sub w.elt 0 n ;
    if n < w.top then w.top <- n ;
  end

let resize w n =
  let m = Array.length w.elt in
  if 0 <= n && n < m then do_shrink w n else
    if n > m then do_grow w n

let shrink w = resize w w.top

let size w = w.top
let length w = w.top
let capacity w = Array.length w.elt

let get w k = 
  if 0 <= k && k < w.top then w.elt.(k) else raise Not_found

let set w k e =
  if 0 <= k && k < w.top then w.elt.(k) <- e else raise Not_found

let addi w e =
  let k = w.top in
  let s = Array.length w.elt in
  if s <= k then do_grow w (max 1 (2*s)) ;
  w.top <- succ w.top ;
  w.elt.(k) <- e ; k

let add w e = ignore (addi w e)

let clear w = 
  begin
    w.top <- 0 ;
    Array.fill w.elt 0 (Array.length w.elt) w.dumb ;
  end

let iter f w  = for k = 0 to w.top - 1 do f w.elt.(k) done
let iteri f w = for k = 0 to w.top - 1 do f k w.elt.(k) done

let map f w = 
  {
    dumb = Obj.magic w.dumb ;
    top = w.top ;
    elt = Array.init w.top (fun i -> f w.elt.(i)) ;
  }

let mapi f w =
  {
    dumb = Obj.magic w.dumb ;
    top = w.top ;
    elt = Array.init w.top (fun i -> f i w.elt.(i)) ;
  }

let find w ?default ?(exn=Not_found) k =
  if 0 <= k && k < w.top then w.elt.(k) else
    match default with
      | None -> raise exn
      | Some e -> e

let update w ~default k e =
  if k >= w.top then
    begin
      let n = succ k in
      let s = Array.length w.elt in
      if s <= k then do_grow w (max n (2*s)) ;
      if k > 0 then Array.fill w.elt w.top k default ;
      w.top <- n ;
    end ;
  w.elt.(k) <- e 

let of_array e = {
  dumb = Obj.magic (ref ()) ;
  elt = Array.copy e ;
  top = Array.length e ;
}

let to_array w = Array.sub w.elt 0 w.top

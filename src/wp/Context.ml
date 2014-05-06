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
(* --- Location                                                           --- *)
(* -------------------------------------------------------------------------- *)

let with_current_loc loc phi x =
  let tmp = Cil_const.CurrentLoc.get () in
  try 
    Cil_const.CurrentLoc.set loc ;
    let y = phi x in
    Cil_const.CurrentLoc.set tmp ; y
  with error ->
    Cil_const.CurrentLoc.set tmp ; raise error

(* -------------------------------------------------------------------------- *)
(* --- Local Context                                                      --- *)
(* -------------------------------------------------------------------------- *)

type 'a value = {
  name : string ; (* Descriptive *)
  mutable current : 'a option ;
}

let create ?default name = { name = name ; current = default }
let name s = s.name

let defined env = match env.current with None -> false | Some _ -> true

let get env =
  match env.current with
    | Some e -> e
    | None -> Wp_parameters.fatal "Context '%s' non-initialized." env.name

let set env s =
  env.current <- Some s

let clear env =
  env.current <- None

let update env f =
  match env.current with
    | Some e -> env.current <- Some (f e)
    | None -> Wp_parameters.fatal "Context '%s' non-initialized." env.name

let bind_with env w f e =
  let tmp = env.current in env.current <- w ;
  try let e = f e in env.current <- tmp ; e
  with error -> env.current <- tmp ; raise error

let bind env s f e = bind_with env (Some s) f e
let free env f e = bind_with env None f e

let push env x = let old = env.current in env.current <- Some x ; old
let pop env old = env.current <- old

let once f =
  let once = ref (Some f) in
  (fun () -> match !once with Some f -> once := None ; f () | None -> ())

(* -------------------------------------------------------------------------- *)

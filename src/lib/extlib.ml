(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: extlib.ml,v 1.16 2009-02-13 07:59:29 uid562 Exp $ *)

let nop _ = ()

let find_or_none f v = try Some(f v) with Not_found -> None

let adapt_filename f =
  let change_suffix ext =
    try Filename.chop_extension f ^ ext
    with Invalid_argument _ -> f ^ ext
  in
  change_suffix
    (if Dynlink_common_interface.is_native then ".cmxs" else ".cmo")

(* [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
   induced by tags creation. This ordering is defined as follow:
   forall tags t1 t2,
   t1 <= t2 iff
   t1 is before t2 in the finite sequence
   [0; 1; ..; max_int; min_int; min_int-1; -1] *)
let max_cpt c1 c2 = max (c1 + min_int) (c2 + min_int) - min_int

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception NotYetImplemented of string
let not_yet_implemented s = raise (NotYetImplemented s)

let mk_fun s = 
  ref (fun _ -> failwith (Printf.sprintf "Function '%s' not registered yet" s))

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

let ($) f g x = f (g x)

let swap f x y = f y x

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

let as_singleton = function
  | [a] -> a
  | _ -> invalid_arg "Extlib.as_singleton"

let filter_out f ls = List.filter (fun x -> not (f x)) ls

let product_fold f acc e1 e2 =
  List.fold_left
    (fun acc e1 -> List.fold_left (fun acc e2 -> f acc e1 e2) acc e2)
    acc e1

let product f e1 e2 = product_fold (fun acc e1 e2 -> f e1 e2 ::acc) [] e1 e2

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

let may f = function
  | None -> ()
  | Some x -> f x

(** [may_map f ?dft x] applies [f] to the value of [x] if exists. Otherwise
    returns the default value [dft].
    Assume that either [x] or [dft] is defined. *)
let may_map f ?dft x =
  match x, dft with
  | None, None -> assert false
  | None, Some dft -> dft
  | Some x, _ -> f x

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_filter f = function
  | None -> None
  | (Some x) as o -> if f x then o else None

let the = function None -> invalid_arg "Extlib.the" | Some x -> x

external getperfcount: unit -> int = "getperfcount"
external getperfcount1024: unit -> int = "getperfcount1024"

external address_of_value: 'a -> int = "address_of_value"

let try_finally ~finally f x =
  try
    let r = f x in
    finally ();
    r
  with e ->
    finally ();
    raise e

let full_command cmd args ~stdin ~stdout ~stderr =
  let pid = Unix.create_process cmd args stdin stdout stderr in
  let _,status = Unix.waitpid [Unix.WUNTRACED] pid in
  match status with
  | Unix.WEXITED s -> s
  | Unix.WSIGNALED s -> s
  | Unix.WSTOPPED s -> s

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

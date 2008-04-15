(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* $Id: extlib.ml,v 1.13 2008/11/17 12:37:45 uid568 Exp $ *)

exception NotYetImplemented of string
let not_yet_implemented s = raise (NotYetImplemented s)
let mk_fun s = ref (fun _ -> not_yet_implemented s)

let deprecated name f x =
  Format.printf "Use of a deprecated function: %s@." name;
  f x

let nop _ = ()

let ($) f g x = f (g x)

let swap f x y = f y x

let find_or_none f v = try Some(f v) with Not_found -> None


let adapt_filename f =
  let change_suffix ext =
    try
      Filename.chop_extension f ^ ext
    with Invalid_argument _ -> f^ext
  in
  change_suffix (if MyDynlink.is_native then ".cmxs" else ".cmo")

(* ************************************************************************** *)
(** {2 Lists} *)
(* ************************************************************************** *)

let as_singleton = function
  | [a] -> a
  | _ -> invalid_arg "Extlib.as_singleton"

let filter_out f ls = List.filter (fun x -> not (f x)) ls

(* ************************************************************************** *)
(** {2 Options} *)
(* ************************************************************************** *)

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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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
(* --- Pretty Printing Library                                            --- *)
(* -------------------------------------------------------------------------- *)

open Format

let kprintf f text =
  let buffer = Buffer.create 80 in
  kfprintf
    (fun fmt ->
       pp_print_flush fmt () ;
       f (Buffer.contents buffer)
    ) (formatter_of_buffer buffer) text

let sprintf text = kprintf (fun s -> s) text
let failure text = kprintf (fun s -> failwith s) text
let to_string pp x =
  let buffer = Buffer.create 80 in
  let fmt = formatter_of_buffer buffer in
  pp fmt x ; pp_print_flush fmt () ; Buffer.contents buffer

type 'a printer = formatter -> 'a -> unit
type 'a printer2 = formatter -> 'a -> 'a -> unit

let pp_call_var ~f pp fmt = function
  | [] -> pp_print_string fmt f
  | x::xs -> 
      fprintf fmt "@[<hov 2>%s(%a" f pp x ;
      List.iter (fun y -> fprintf fmt ",@ %a" pp y) xs ;
      fprintf fmt ")@]"

let pp_call_void ~f pp fmt = function
  | [] -> fprintf fmt "%s()" f
  | x::xs -> 
      fprintf fmt "@[<hov 2>%s(%a" f pp x ;
      List.iter (fun y -> fprintf fmt ",@ %a" pp y) xs ;
      fprintf fmt ")@]"

let pp_call_apply ~f pp fmt = function
  | [] -> pp_print_string fmt f
  | xs -> 
      fprintf fmt "@[<hov 2>(%s" f ;
      List.iter (fun y -> fprintf fmt "@ %a" pp y) xs ;
      fprintf fmt ")@]"

let pp_binop ~op pp fmt a b =
  fprintf fmt "%a@ %s %a" pp a op pp b

let print_not_empty s fmt = function
  | Some e -> pp_print_string fmt e
  | None -> invalid_arg ("Plib.print_not_empty: invariant broken: empty list for " ^ s)
(** neutral element not given but the list is empty *)

let pp_assoc ?e ~op pp fmt = function
  | [] -> print_not_empty op fmt e
  | x::xs ->
      pp fmt x ; List.iter (fun y -> fprintf fmt " %s@ %a" op pp y) xs

let rec pp_fold_binop ?e ~op pp fmt = function
  | [] -> print_not_empty op fmt e
  | [x] -> pp fmt x
  | x::xs -> fprintf fmt "(%a %s@ %a)" pp x op (pp_fold_binop ?e ~op pp) xs

let rec pp_fold_call ?e ~f pp fmt = function
  | [] -> print_not_empty f fmt e
  | [x] -> pp fmt x
  | x::xs -> fprintf fmt "%s(%a,@ %a)" f pp x (pp_fold_call ?e ~f pp) xs

let rec pp_fold_apply ?e ~f pp fmt = function
  | [] -> print_not_empty f fmt e
  | [x] -> pp fmt x
  | x::xs -> fprintf fmt "(%s@ %a@ %a)" f pp x (pp_fold_apply ?e ~f pp) xs

let rec pp_fold_call_rev ?e ~f pp fmt = function
  | [] -> print_not_empty f fmt e 
  | [x] -> pp fmt x
  | x::xs -> fprintf fmt "%s(%a,@ %a)" f (pp_fold_call_rev ?e ~f pp) xs pp x

let rec pp_fold_apply_rev ?e ~f pp fmt = function
  | [] -> print_not_empty f fmt e 
  | [x] -> pp fmt x
  | x::xs -> fprintf fmt "(%s@ %a@ %a)" f pp x (pp_fold_apply_rev ?e ~f pp) xs

let pp_listcompact ~sep pp fmt = function
  | [] -> ()
  | x::xs -> 
      pp fmt x ;
      List.iter (fun x -> fprintf fmt "%s@,%a" sep pp x) xs

let pp_listsep ~sep pp fmt = function
  | [] -> ()
  | x::xs -> 
      pp fmt x ;
      List.iter (fun x -> fprintf fmt "%s@ %a" sep pp x) xs    

type index = Isingle | Ifirst | Ilast | Imiddle

let iteri f = function
  | [] -> ()
  | [x] -> f Isingle x
  | x::xs -> 
      let rec iterk f = function
        | [] -> ()
        | [x] -> f Ilast x
        | x::xs -> f Imiddle x ; iterk f xs
      in f Ifirst x ; iterk f xs

let iterk f xs =
  let rec step f k = function
    | [] -> ()
    | x::xs -> f k x ; step f (succ k) xs
  in step f 0 xs

let mapk f xs =
  let rec step f k = function
    | [] -> []
    | x::xs -> 
        let y = f k x in
        y :: step f (succ k) xs
  in step f 0 xs

(** the regexp shouldn't match empty *)
let global_substitute_fmt regexp repl_fun fmt text =
  let rec replace start =
    if start < String.length text then
      try
        let pos = Str.search_forward regexp text start in
        let end_pos = Str.match_end () in
        assert (start <= pos && pos < end_pos);
        if start < pos then
          pp_print_string fmt (String.sub text start (pos - start));
        repl_fun fmt (Str.matched_group 1 text);
        replace end_pos
      with Not_found ->
          pp_print_string fmt (Str.string_after text start)
  in
  replace 0

let regexp_arg_pos = Str.regexp "%\\([0-9]+\\)"
let substitute_list print s fmt l =
  let args = Array.of_list l in
  let repl_fun fmt grp =
    let i = int_of_string grp in
    print fmt args.(i-1) in
  global_substitute_fmt regexp_arg_pos repl_fun fmt s

(** the regexp shouldn't match empty *)
let iter_group regexp iter_fun text =
  let rec iter start =
    if start < String.length text then
      try
        let pos = Str.search_forward regexp text start in
        let end_pos = Str.match_end () in
        assert (start <= pos && pos < end_pos);
        iter_fun (Str.matched_group 1 text);
        iter end_pos
      with Not_found -> ()
  in
  iter 0

(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: pp.ml,v 1.15 2008/02/26 10:16:36 moy Exp $ i*)

(*s Pretty-print library *)

open Format

let print_option f fmt = function
  | None -> ()
  | Some x -> f fmt x

let print_option_or_default default f fmt = function
  | None -> fprintf fmt "%s" default
  | Some x -> f fmt x

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let print_list_or_default default sep print fmt = function
  | [] -> fprintf fmt "%s" default
  | l -> print_list sep print fmt l

let print_list_par sep pr fmt l =
  print_list sep (fun fmt x -> fprintf fmt "(%a)" pr x) fmt l

let print_list_delim start stop sep pr fmt = function
  | [] -> ()
  | l -> fprintf fmt "%a%a%a" start () (print_list sep pr) l stop ()

let comma fmt () = fprintf fmt ",@ "
let underscore fmt () = fprintf fmt "_"
let semi fmt () = fprintf fmt ";@ "
let space fmt () = fprintf fmt " "
let alt fmt () = fprintf fmt "|@ "
let newline fmt () = fprintf fmt "@\n"
let arrow fmt () = fprintf fmt "@ -> "
let lbrace fmt () = fprintf fmt "{"
let rbrace fmt () = fprintf fmt "}"
let nothing fmt () = ()
let string fmt s = fprintf fmt "%s" s

let hov n fmt f x = pp_open_hovbox fmt n; f x; pp_close_box fmt ()

let open_formatter ?(margin=78) cout =
  let fmt = formatter_of_out_channel cout in
  pp_set_margin fmt margin;
  pp_open_box fmt 0; 
  fmt

let close_formatter fmt = 
  pp_close_box fmt ();
  pp_print_flush fmt ()

let open_file_and_formatter ?(margin=78) f =
  let cout = open_out f in
  let fmt = open_formatter ~margin cout in
  cout,fmt

let close_file_and_formatter (cout,fmt) =
  close_formatter fmt;
  close_out cout

let print_in_file_no_close ?(margin=78) p f =
  let cout,fmt = open_file_and_formatter ~margin f in
  p fmt;
  close_formatter fmt;
  cout

let print_in_file ?(margin=78) p f =
  let cout = print_in_file_no_close ~margin p f in
  close_out cout



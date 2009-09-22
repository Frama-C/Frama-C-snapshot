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

(* $Id: pretty_utils.ml,v 1.5 2008-11-04 10:05:05 uid568 Exp $ *)

let sfprintf fmt =
  let b = Buffer.create 5 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt

let space_sep = format_of_string "@ "
let nl_sep = format_of_string "@\n"

let rec pp_print_string_fill out s =
  if String.contains s ' ' then begin
    let i = String.index s ' ' in
    let l = String.length s in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s (i+1) (l - i - 1) in
      Format.fprintf out "%s@ %a" s1 pp_print_string_fill s2
  end else Format.pp_print_string out s

let open_box = format_of_string "@["
let close_box = format_of_string "@]"

type sformat = (unit,Format.formatter,unit) Pervasives.format
type 'a formatter = Format.formatter -> 'a -> unit
type ('a,'b) formatter2 = Format.formatter -> 'a -> 'b -> unit

let pp_list 
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "") 
    ?(suf=format_of_string "@]")
    pp_elt f l =
  let rec aux f l =
    match l with
        [] -> ()
      | e::l -> Format.fprintf f "%(%)%a%a" sep pp_elt e aux l
  in match l with
      [] -> ()
    | e::l -> Format.fprintf f "%(%)%a%a%(%)" pre pp_elt e aux l suf

let pp_array 
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "") 
    ?(suf=format_of_string "@]")
    pp_elt f xs =
  match xs with
    | [| |] -> ()
    | xs ->
	begin
	  Format.fprintf f pre ;
	  pp_elt f 0 xs.(0) ;
	  for i = 1 to Array.length xs - 1 do
	    Format.fprintf f sep ;
	    pp_elt f i xs.(i) ;
	  done ;
	  Format.fprintf f suf ;
	end

let pp_opt
    ?(pre=format_of_string "")  ?(suf=format_of_string "") pp_elt f o =
  match o with
      None -> ()
    | Some v ->  Format.fprintf f "%(%)%a%(%)" pre pp_elt v suf

let pp_cond ?(pr_false=format_of_string "") cond f pr_true =
  Format.fprintf f "%(%)" (if cond then pr_true else pr_false)

let escape_underscores = Str.global_replace (Str.regexp_string "_") "__"

let pp_flowlist ?(left="(") ?(sep=",") ?(right=")") f out = 
  function
    | [] -> Format.fprintf out "%s%s" left right
    | x::xs -> 
	begin
	  Format.fprintf out "@[<hov 1>%s%a" left f x ; 
	  List.iter (fun x -> Format.fprintf out "%s@,%a" sep f x) xs ; 
	  Format.fprintf out "%s@]" right ;
	end

let pp_blocklist ?(left="{") ?(right="}") f out = 
  function
    | [] -> Format.fprintf out "%s%s" left right
    | xs ->
	Format.fprintf out "@[<hv 0>%s@[<hv 2>" left ;
	List.iter (fun x -> Format.fprintf out "@ %a" f x) xs ;
	Format.fprintf out "@]@ %s@]" right

let pp_open_block out msg = Format.fprintf out ("@[<hv 0>@[<hv 2>" ^^ msg )
let pp_close_block out msg = Format.fprintf out ("@]@ " ^^ msg ^^ "@]")

(*
Local Variables:
compile-command: "make -C ../.. -j"
End:
*)

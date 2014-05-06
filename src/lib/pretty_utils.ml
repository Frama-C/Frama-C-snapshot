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

let sfprintf fmt =
  let b = Buffer.create 20 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt

let to_string pp x =
  let b = Buffer.create 20 in
  let f = Format.formatter_of_buffer b in
  pp f x ;
  Format.pp_print_flush f () ;
  Buffer.contents b

let rec pp_print_string_fill out s =
  if String.contains s ' ' then begin
    let i = String.index s ' ' in
    let l = String.length s in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s (i+1) (l - i - 1) in
      Format.fprintf out "%s@ %a" s1 pp_print_string_fill s2
  end else Format.pp_print_string out s

type sformat = (unit,Format.formatter,unit) Pervasives.format
type 'a formatter = Format.formatter -> 'a -> unit
type ('a,'b) formatter2 = Format.formatter -> 'a -> 'b -> unit

let pp_list
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "@,")
    ?(last=sep)
    ?(suf=format_of_string "@]")
    pp_elt f l =
  let rec aux f = function
    | [] -> assert false
    | [ e ] -> Format.fprintf f "%a" pp_elt e
    | [ e1; e2 ] -> Format.fprintf f "%a%(%)%a" pp_elt e1 last pp_elt e2
    | e :: l -> Format.fprintf f "%a%(%)%a" pp_elt e sep aux l
  in
  match l with
  | [] -> ()
  | _ :: _ as l -> Format.fprintf f "%(%)%a%(%)" pre aux l suf

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

let pp_iter
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "")
    ?(suf=format_of_string "@]")
    iter pp fmt v =
  let need_sep = ref false in
  Format.fprintf fmt pre;
  iter (fun v ->
          if !need_sep then Format.fprintf fmt sep else need_sep := true;
          pp fmt v;
       ) v;
  Format.fprintf fmt suf;
;;

let pp_opt ?(pre=format_of_string "@[")  ?(suf=format_of_string "@]") pp_elt f =
  function
  | None -> ()
  | Some v ->  Format.fprintf f "%(%)%a%(%)" pre pp_elt v suf

let pp_cond ?(pr_false=format_of_string "") cond f pr_true =
  Format.fprintf f "%(%)" (if cond then pr_true else pr_false)

let escape_underscores = Str.global_replace (Str.regexp_string "_") "__"

let pp_flowlist ?(left=format_of_string "(") ?(sep=format_of_string ",") ?(right=format_of_string ")") f out =
  function
    | [] -> Format.fprintf out "%(%)%(%)" left right
    | x::xs ->
        begin
          Format.fprintf out "@[<hov 1>%(%)%a" left f x ;
          List.iter (fun x -> Format.fprintf out "%(%)@,%a" sep f x) xs ;
          Format.fprintf out "%(%)@]" right ;
        end

let pp_blocklist ?(left=format_of_string "{") ?(right=format_of_string "}") f out =
  function
    | [] -> Format.fprintf out "%(%)%(%)" left right
    | xs ->
        Format.fprintf out "@[<hv 0>%(%)@[<hv 2>" left ;
        List.iter (fun x -> Format.fprintf out "@ %a" f x) xs ;
        Format.fprintf out "@]@ %(%)@]" right

let pp_open_block out msg =
  Format.fprintf out ("@[<hv 0>@[<hv 2>" ^^ msg)

let pp_close_block out msg = Format.fprintf out ("@]@ " ^^ msg ^^ "@]")

let pp_trail pp fmt x =
  begin
    Format.fprintf fmt "@[<h 0>(**" ;
    let out newlined fmt s k n =
      for i=k to k+n-1 do
        if !newlined then
          ( Format.fprintf fmt "@\n * " ; newlined := false ) ;
        if s.[i] = '\n'
          then newlined := true
        else Format.pp_print_char fmt s.[i]
      done
    in
    let nwl = ref true in
    let ftt = Format.make_formatter (out nwl fmt) (fun () -> ()) in
    pp ftt x ;
    Format.pp_print_flush ftt () ;
    Format.fprintf fmt "@\n **)@]" ;
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

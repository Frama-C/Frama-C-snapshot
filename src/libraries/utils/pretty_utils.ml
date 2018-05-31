(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let null = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
let with_null k msg = Format.kfprintf (fun _ -> k ()) null msg
let nullprintf msg = Format.ifprintf null msg

let ksfprintf f fmt =
  let b = Buffer.create 20 in
  let return fmt = Format.pp_print_flush fmt (); f (Buffer.contents b) in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt

let sfprintf = Format.asprintf

let to_string ?margin pp x =
  let b = Buffer.create 20 in
  let f = Format.formatter_of_buffer b in
  Extlib.may (Format.pp_set_margin f) margin;
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
    ?(empty=format_of_string "")
    pp_elt f l =
  let rec aux f = function
    | [] -> assert false
    | [ e ] -> Format.fprintf f "%a" pp_elt e
    | [ e1; e2 ] -> Format.fprintf f "%a%(%)%a" pp_elt e1 last pp_elt e2
    | e :: l -> Format.fprintf f "%a%(%)%a" pp_elt e sep aux l
  in
  match l with
  | [] -> Format.fprintf f "%(%)" empty
  | _ :: _ as l -> Format.fprintf f "%(%)%a%(%)" pre aux l suf

let pp_array
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "")
    ?(suf=format_of_string "@]")
    ?(empty=format_of_string "")
    pp_elt f xs =
  match xs with
    | [| |] -> Format.fprintf f "%(%)" empty
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

let pp_iter2
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "")
    ?(suf=format_of_string "@]")
    ?(between=format_of_string "@ ")
    iter pp_key pp_v fmt v =
  let need_sep = ref false in
  Format.fprintf fmt pre;
  iter (fun key v ->
      if !need_sep then Format.fprintf fmt sep else need_sep := true;
      Format.fprintf fmt "%a%(%)%a" pp_key key between pp_v v
    ) v;
  Format.fprintf fmt suf;
;;

let pp_opt ?(pre=format_of_string "@[")  ?(suf=format_of_string "@]") ?(none=format_of_string "") pp_elt f =
  function
  | None -> Format.fprintf f "%(%)" none
  | Some v ->  Format.fprintf f "%(%)%a%(%)" pre pp_elt v suf

let pp_cond ?(pr_false=format_of_string "") cond f pr_true =
  Format.fprintf f "%(%)" (if cond then pr_true else pr_false)

let pp_pair
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string ",@,")
    ?(suf=format_of_string "@]")
    pp_a pp_b fmt (a, b) =
  Format.fprintf fmt "%(%)%a%(%)%a%(%)" pre pp_a a sep pp_b b suf

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

(* -------------------------------------------------------------------------- *)
(* --- Margins                                                            --- *)
(* -------------------------------------------------------------------------- *)

type marger = int ref
let marger () = ref 0
let add_margin marger ?(margin=0) ?(min=0) ?(max=80) text =
  let size = String.length text + margin in
  let n = Pervasives.min max (Pervasives.max min size) in
  if n > !marger then marger := n

type align = [ `Center | `Left | `Right ]

let pp_margin ?(align=`Center) ?(pp=Format.pp_print_string) marger fmt text =
  let n = String.length text in
  let m = !marger in
  if n > m then
    if m < 8 then
      pp fmt (String.sub text 0 m)
    else
      pp fmt (String.sub text 0 (m-3) ^ "...")
  else
    let space fmt s =
      if s > 0 then
        Format.pp_print_string fmt (String.make s ' ') in
    let w = m-n in
    match align with
    | `Center ->
        let l = w / 2 in
        let r = w - l in
        space fmt l ; pp fmt text ; space fmt r ;
    | `Left ->
        pp fmt text ; space fmt w
    | `Right ->
        space fmt w ; pp fmt text

let pp_items ?align ?margin ?min ?max ~title ~iter ?pp_title ~pp_item fmt =
  let m = marger () in
  iter (fun e -> add_margin m ?margin ?min ?max (title e)) ;
  let pp = pp_margin ?align ?pp:pp_title m in
  iter (pp_item pp fmt)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

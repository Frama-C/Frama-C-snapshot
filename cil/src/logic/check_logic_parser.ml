(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

let file = open_in "logic_parser.mly"

module Strings = Set.Make(String)

let tokens = ref Strings.empty

let wildcards = ref Strings.empty

type state = Throw | Wildcard

let is_token_line s = String.length s >= 6 && String.sub s 0 6 = "%token"

let add_tokens s =
  let rec add_token s1 =
    Scanf.sscanf s1 " %[A-Za-z0-9_] %s@$" 
      (fun kw tl ->
        if kw <> "" then begin
          tokens:=Strings.add kw !tokens;
          add_token tl
        end)
  in
  let s = String.sub s 7 (String.length s - 7) in
  let s =
    if String.contains s '>' then begin
      let idx = String.index s '>' in
      String.sub s (idx+1) (String.length s - idx - 1)
    end else s
  in add_token s

let wildcard_rules =
  [ "bs_keyword"; "wildcard"; "keyword"; "c_keyword"; 
    "non_logic_keyword"; "acsl_c_keyword"; "is_ext_spec";
    "is_acsl_spec"; "is_acsl_decl_or_code_annot";
    "is_acsl_other"; "post_cond";
    "identifier_or_typename"
  ]

let find_rule_name s =
  let l = String.index s ':' in
  String.sub s 0 l

let is_wildcard_rule s =
  if String.contains s ':' then begin
    let rule = find_rule_name s in
    let res =
      List.mem rule wildcard_rules in
    res
  end else false

let is_other_rule s =
  if String.contains s ':' then begin
    let rule = find_rule_name s in
    not (List.mem rule wildcard_rules)
  end else false

let add_wildcards s =
  let s = 
    if String.contains s ':' then begin
      let l = String.index s ':' in
      String.sub s (l+1) (String.length s - l - 1)
    end else s
  in
  let rec add_wildcard s =
    Scanf.sscanf s " | %s { %_s@} %s"
      (fun kw tl ->
        wildcards := Strings.add kw !wildcards;
        if tl <> "" then add_wildcard tl)
  in
  if s <> "" then 
    try
      add_wildcard s
    with Scanf.Scan_failure _ -> ()

let () =
  try
    let state = ref Throw in
    while true do
      let s = input_line file in
      if is_token_line s then add_tokens s
      else if !state = Throw then begin
        if is_wildcard_rule s then begin
          state:=Wildcard;
          add_wildcards s
        end
      end
      else (* state is Wildcard *)
        if is_other_rule s then state:=Throw 
        else add_wildcards s
    done
  with End_of_file -> ()

let whitelist =
  List.fold_right 
    Strings.add 
    [ "EOF" ]
    Strings.empty

let () =
  let diff = Strings.diff (Strings.diff !tokens whitelist) !wildcards in
  if not (Strings.is_empty diff) then begin
    prerr_endline 
      "Some tokens are not captured by wildcard rules. This will cause issue \
       if those tokens appear in a contract. Please add the following tokens \
       in the appropriate rule:";
    Strings.iter prerr_endline diff;
    exit 2
  end

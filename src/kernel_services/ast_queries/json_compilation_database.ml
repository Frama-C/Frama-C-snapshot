(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

module StringList = Datatype.List(Datatype.String)

module Flags =
  State_builder.Hashtbl
    (Datatype.Filepath.Hashtbl)
    (StringList)
    (struct
      let name ="JsonCompilationDatabase.Flags"
      let dependencies = [Kernel.JsonCompilationDatabase.self]
      let size = 32
    end)

type arg_type =
    Path of string
  | Define of string
  | Undefine of string

let whitelisted_prefixes =
  [Path "-I"; Path "-include"; Path "-imacros"; Define "-D"; Undefine "-U"]

let string_of_arg_type = function
    Path s | Define s | Undefine s -> s

let whitelist =
  List.map (fun p ->
      let s = string_of_arg_type p in
      p, Str.regexp (s ^ "\\(.*\\)")
    ) whitelisted_prefixes

exception Found_whitelisted of arg_type * string

(* Tests if any whitelisted prefix matches [s], and returns
   the matched suffix (s minus the prefix, which can be ""),
   or None if no match. *)
let has_whitelisted_prefix s =
  try
    List.iter (fun (prefix, re) ->
        if Str.string_match re s 0 then
          try
            raise (Found_whitelisted (prefix, Str.matched_group 1 s))
          with Not_found ->
            (* found the prefix, but with an empty suffix *)
            raise (Found_whitelisted (prefix, ""))
      ) whitelist;
    None
  with Found_whitelisted (prefix, suffix) -> Some (prefix, suffix)

type arg_parser_state = Inside_quote of char | Outside_quote

(** Parses a 'command' string, splitting arguments into a list of strings.
    Handles quoted strings containing spaces. *)
let split_command_args s =
  let n = String.length s in
  let buf = Buffer.create 20 in
  let rec aux i prev_c state acc =
    if i >= n then begin
      if Buffer.length buf > 0 then Buffer.contents buf :: acc else acc
    end else
      let c = String.get s i in
      let new_state, new_acc =
        match state, prev_c, c with
        | Outside_quote, '\\', c when c = '\"' || c = '\'' ->
          (* escaped quote, continue with previous arg *)
          Buffer.add_char buf c;
          state, acc
        | Outside_quote, _, q when q = '\'' || q = '\"' ->
          (* continue previous arg with q *)
          Buffer.add_char buf q;
          Inside_quote q, acc
        | Outside_quote, _, ws when ws = ' ' || ws = '\t' ->
          if Buffer.length buf = 0 then
            (* in whitespace between args *)
            Outside_quote, acc
          else
            (* close previous arg and start another *)
            let new_arg = Buffer.contents buf in
            Buffer.clear buf;
            Outside_quote, new_arg :: acc
        | Outside_quote, _, _ ->
          (* continue previous arg with c *)
          Buffer.add_char buf c;
          Outside_quote, acc
        | Inside_quote q, '\\', ch when ch = q ->
          (* escaped quote, continue with previous arg *)
          Buffer.add_char buf c;
          state, acc
        | Inside_quote q, _, ch when q = ch ->
          (* unescaped quote, close arg and start another *)
          Buffer.add_char buf c;
          let new_arg = Buffer.contents buf in
          Buffer.clear buf;
          Outside_quote, new_arg :: acc
        | Inside_quote q, _, _ ->
          (* continue previous arg with c *)
          Buffer.add_char buf c;
          Inside_quote q, acc
      in
      aux (i+1) c new_state new_acc
  in
  let args = aux 0 ' ' Outside_quote [""] in
  let res = List.filter (fun s -> s <> "") args in
  List.rev res

(** The 'arguments' given in a compile_commands.json are unescaped,
    but cannot be directly passed to the compiler. In particular,
    macro definitions and strings containing quotes need to be
    "re-quoted" before they are given to the preprocessor.
    This only needs to be applied to definitions; undefinitions (-U)
    never need quotes. *)
let quote_define_argument arg = Format.sprintf "%S" arg

let parse_entry ?(cwd=Sys.getcwd()) r =
  let open Yojson.Basic.Util in
  let filename = r |> member "file" |> to_string in
  let dirname  = r |> member "directory" |> to_string_option |> Extlib.opt_conv "" in
  let path = Datatype.Filepath.of_string ~base_name:dirname filename in

  (* get the list of arguments, and a flag indicating if the arguments
     were given via 'command' or 'arguments'; the latter require quoting *)
  let string_option_list, requote =
    (* Note: the JSON Compilation Databse specification specifies that
       "either arguments or command is required", but does NOT specify what
       happens when both are present. There is a LLVM commit from 2015
       (https://reviews.llvm.org/D10365) that mentions:
       "Arguments and Command can now be in the same compilation database for
        the same file. Arguments are preferred when both are present."
       The code below follows this behavior. *)
    try
      let args = List.map to_string (r |> member "arguments" |> to_list) in
      args, true
    with _ ->
    try
      let s = r |> member "command" |> to_string in
      split_command_args s, false
    with _ ->
      Kernel.abort "compilation database: expected 'arguments' or 'command'"
  in
  (* Normalize path names in include directives: first we normalize w.r.t.
     the file's directory (following the compilation database logic),
     then we relativize w.r.t. to Frama-C's PWD if possible
     (to avoid overly long absolute paths). *)
  let convert_path arg =
    let abs = Filepath.normalize ~base_name:dirname arg in
    Filepath.relativize ~base_name:cwd abs
  in
  let convert_define arg =
    if requote then quote_define_argument arg else arg
  in
  let process_prefix prefix suffix =
    match prefix with
    | Path s -> s ^ convert_path suffix
    | Define s -> s ^ convert_define suffix
    | Undefine s -> s ^ suffix
  in
  (* we must process the arguments in-order, since several -D and -U may
     exist in the command line *)
  (* prev is the prefix of the previous argument (if any) *)
  let _, res =
    List.fold_left (fun (prev, acc_res) arg ->
        match prev with
        | None -> begin
            match has_whitelisted_prefix arg with
            | None ->
              Kernel.feedback ~dkey:Kernel.dkey_compilation_db
                "dropping non-whitelisted argument: %s" arg;
              (None, acc_res)
            | Some (prefix, suffix) ->
              if suffix = "" then begin
                (* delay argument for next iteration *)
                Kernel.feedback ~dkey:Kernel.dkey_compilation_db
                  "storing whitelisted lonely prefix: %s" arg;
                (Some prefix, acc_res)
              end else begin
                Kernel.feedback ~dkey:Kernel.dkey_compilation_db
                  "adding whitelisted attached prefix: %s" arg;
                let new_arg = process_prefix prefix suffix in
                (None, new_arg :: acc_res)
              end
          end
        | Some prefix -> begin
            Kernel.feedback ~dkey:Kernel.dkey_compilation_db
              "adding stored prefix to suffix: %s %s"
              (string_of_arg_type prefix) arg;
            let new_arg = process_prefix prefix arg in
            (None, new_arg :: acc_res)
          end
      ) (None, []) string_option_list
  in
  (* Note: the same file may be compiled several times, under different
     (and possibly incompatible) configurations, leading to multiple
     occurrences in the list. Since we cannot infer which of them is the
     "right" one, we replace them with the latest ones found, warning the
     user if previous flags were different. *)
  let flags = List.rev res in
  try
    let previous_flags = Flags.find path in
    if previous_flags <> flags then
      let removed_flags = List.filter (fun e -> not (List.mem e previous_flags)) flags in
      let removed_str =
        if removed_flags = [] then "" else
          Format.asprintf "@ Old flags no longer present: %a"
            (Pretty_utils.pp_list ~sep:" " Format.pp_print_string) removed_flags
      in
      let added_flags = List.filter (fun e -> not (List.mem e flags)) previous_flags in
      let added_str =
        if added_flags = [] then "" else
          Format.asprintf "@ New flags not previously present: %a"
            (Pretty_utils.pp_list ~sep:" " Format.pp_print_string) added_flags
      in
      Kernel.warning ~wkey:Kernel.wkey_jcdb
        "@[<v>found duplicate flags for '%a', replacing old flags.%s%s@]"
        Datatype.Filepath.pretty path removed_str added_str;
      Flags.replace path flags
  with
  | Not_found ->
    Flags.add path flags

let get_flags f =
  if Kernel.JsonCompilationDatabase.get () <> "" then begin
    if not (Flags.is_computed ()) then begin
      let database = Kernel.JsonCompilationDatabase.get () in
      let path =
        if Sys.is_directory database then
          Filename.concat database "compile_commands.json"
        else database
      in
      Kernel.feedback ~dkey:Kernel.dkey_compilation_db
        "using compilation database: %s" path;
      begin
        try
          let r_list =
            Yojson.Basic.from_file path |> Yojson.Basic.Util.to_list
          in
          List.iter parse_entry r_list;
        with
        | Sys_error msg
        | Yojson.Json_error msg
        | Yojson.Basic.Util.Type_error (msg, _) ->
          Kernel.abort "could not parse compilation database: %s@ %s"
            database msg
      end;
      Flags.mark_as_computed ()
    end;
    try
      let flags = Flags.find f in
      Kernel.feedback ~dkey:Kernel.dkey_compilation_db
        "flags found for '%a': %a"  Datatype.Filepath.pretty f StringList.pretty flags;
      flags
    with Not_found ->
      Kernel.feedback ~dkey:Kernel.dkey_compilation_db
        "no flags found for '%a'"  Datatype.Filepath.pretty f;
      []
  end
  else []

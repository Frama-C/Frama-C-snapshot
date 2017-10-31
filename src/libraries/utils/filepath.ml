(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type path = {
  hash : int ;
  path_name : string ;
  base_name : string ; (* Filename.basename *)
  dir : path option ; (* path whose path_name is Filename.dirname *)
  mutable symbolic_name : string option ; (* Symbolic name *)
}

let dummy = {
  path_name = "";
  hash = 0;
  base_name = ".";
  dir = None;
  symbolic_name = None
}

(* re_drive and re_root match drive expressions to deal with non-Cygwin
   Windows-like paths (e.g. with MinGW) *)
let re_drive = Str.regexp "[A-Za-z]:"
let re_path = Str.regexp "[/\\\\]"
let re_root = Str.regexp "/\\|\\([A-Za-z]:\\\\\\)\\|\\([A-Za-z]:/\\)"

(* -------------------------------------------------------------------------- *)
(* --- Path Indexing                                                      --- *)
(* -------------------------------------------------------------------------- *)

(* Can not use Weak, since the internal [path] representation is not returned.
   Can not use a weak-cache because each minor GC
   may empty the cache (see #191). *)

module HPath =
struct
  module H = Hashtbl.Make
      (struct
        type t = path
        let hash p = p.hash
        let equal p q = p.path_name = q.path_name
      end)
  let find = H.find
  let create = H.create
  let merge h p = try H.find h p with Not_found -> H.add h p p ; p
end

let hcons = HPath.create 128
let cache = Array.make 256 None

let root path_name =
  HPath.merge hcons { dummy with path_name ; hash = Hashtbl.hash path_name }

let make dir base_name =
  let path_name = Printf.sprintf "%s/%s" dir.path_name base_name in
  let hash = Hashtbl.hash path_name in
  HPath.merge
    hcons
    { dummy with
      path_name;
      hash;
      base_name = base_name;
      dir = Some dir
    }

let getdir path =
  match path.dir with
  | None -> dummy (* the parent of the root directory is itself *) 
  | Some d -> d

let rec norm path = function
  | [] -> path
  | ".."::ps -> norm (getdir path) ps
  | "."::ps -> norm path ps
  | p::ps -> norm (make path p) ps

let insert base path_name =
  let full_path_name = 
    (* if a <base> is provided while a <file> is already absolute 
       (and thus matches [re_root]) then the <base> is not taken 
       into account *)
    if Str.string_match re_root path_name 0
    then path_name
    else base.path_name ^ "/" ^ path_name in
  let hash = Hashtbl.hash full_path_name in
  match Array.get cache (hash land 255) with
  | Some (pn, p) when full_path_name = pn -> p
  | _ ->
    let p = { dummy with path_name = full_path_name; hash } in
    try HPath.find hcons p
    with Not_found ->
      let base =
        (* if a <base> is provided while a <file> is already absolute 
           (and thus matches [re_root]) then the <base> is not taken 
           into account *)
        if Str.string_match re_root path_name 0
        then root (String.sub path_name 0 (Str.group_end 0 - 1))
        else base in
      let name_parts = Str.split re_path path_name in
      (* Windows paths may start with '<drive>:'. If so, remove it *)
      let parts = if List.length name_parts > 0 &&
                     Str.string_match re_drive (List.nth name_parts 0) 0 then
          List.tl name_parts
        else name_parts
      in
      let path = norm base parts in
      Array.set cache (hash land 255) (Some (path_name, path));
      path

let cwd = insert dummy (Sys.getcwd())

let normalize ?base_name path_name =
  if path_name = "" then raise (Invalid_argument "Filepath.normalize");
  let base =
    match base_name with
      | None -> cwd
      | Some b -> insert cwd b in
  let norm_path_name = (insert base path_name).path_name in
  if norm_path_name = "" then "/" else norm_path_name

(* -------------------------------------------------------------------------- *)
(* --- Symboling Names                                                    --- *)
(* -------------------------------------------------------------------------- *)

let add_symbolic_dir name dir = (insert cwd dir).symbolic_name <- Some name

let rec add_path buffer path =
  let open Buffer in
  match path.symbolic_name with
    | None ->
        begin
          match path.dir with
            | None -> add_string buffer path.path_name
            | Some d ->
              if d != cwd (* hconsed *) then
                ( add_path buffer d ; add_char buffer '/' ) ;
              add_string buffer path.base_name
        end
    | Some sn -> add_string buffer sn

let rec skip_dot file_name =
  if Extlib.string_prefix "./" file_name then
    skip_dot (String.sub file_name 2 (String.length file_name - 2))
  else file_name

let pretty file_name =
  if Filename.is_relative file_name then
    skip_dot file_name
  else
    let path = insert cwd file_name in
    let file_name = path.path_name in
    let cwd_name = cwd.path_name in
    if Extlib.string_prefix ~strict:true cwd_name file_name then
      let n = 1 + String.length cwd_name in
      String.sub file_name n (String.length file_name - n)
    else
      let buffer = Buffer.create 80 in
      add_path buffer path ;
      Buffer.contents buffer

(* -------------------------------------------------------------------------- *)
(* --- Relative Paths                                                     --- *)
(* -------------------------------------------------------------------------- *)

let relativize ?base_name file_name =
  let file_name = (insert cwd file_name).path_name in
  let base_name = match base_name with
    | None -> cwd.path_name
    | Some b -> (insert cwd b).path_name in
  if Extlib.string_prefix ~strict:true base_name file_name then
    let n = 1 + String.length base_name in
    String.sub file_name n (String.length file_name - n)
  else file_name

let is_relative ?base_name file_name =
  let file_name = (insert cwd file_name).path_name in
  let base_name = match base_name with
    | None -> cwd.path_name
    | Some b -> (insert cwd b).path_name in
  Extlib.string_prefix ~strict:true base_name file_name

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

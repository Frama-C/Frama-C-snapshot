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
  path : string ; (* full path *)
  name : string ; (* Filename.basename *)
  dir : path option ; (* Filename.dirname *)
  mutable symbol : string ; (* Symbolic name, or "" *)
}

let dummy = {path="";hash=0;name="";dir=None;symbol=""}

(* re_drive and re_root match drive expressions to deal with non-Cygwin
   Windows-like paths (e.g. with MinGW) *)
let re_drive = Str.regexp "[A-Za-z]:"
let re_path = Str.regexp "[/\\\\]"
let re_root = Str.regexp "/\\|\\([A-Za-z]:\\\\\\)\\|\\([A-Za-z]:/\\)"

(* -------------------------------------------------------------------------- *)
(* --- H-Consing                                                          --- *)
(* -------------------------------------------------------------------------- *)

(* Can not use Weak, since the internal [path] 
   representation is not returned. Use a weak-cache, however. *)

module W =
struct
  module H = Hashtbl.Make
      (struct
        type t = path
        let hash p = p.hash
        let equal p q = p.path = q.path
      end)
  let find = H.find
  let create = H.create
  let merge h p = try H.find h p with Not_found -> H.add h p p ; p
  end

let hcons = W.create 128
let cache = Weak.create 256

let root path =
  W.merge hcons { dummy with path ; hash = Hashtbl.hash path }

let make dir name =
  let path = Printf.sprintf "%s/%s" dir.path name in
  let hash = Hashtbl.hash path in
  W.merge hcons { dummy with path ; hash ; name ; dir = Some dir }

let getdir p =
  match p.dir with
  | None -> (* the parent of the root directory is itself *) dummy
  | Some d -> d

let rec norm path = function
  | [] -> path
  | ".."::fs -> norm (getdir path) fs
  | "."::fs -> norm path fs
  | f::fs -> norm (make path f) fs

let insert pwd file =
  let hash = Hashtbl.hash file in
  match Weak.get cache (hash land 255) with
  | Some (f0,p0) when file = f0 -> p0
  | _ ->
      let p0 = { dummy with path = file ; hash } in
      try W.find hcons p0
      with Not_found ->
        let base =
          if Str.string_match re_root file 0
          then root (String.sub file 0 (Str.group_end 0 - 1))
          else pwd in
        let parts = Str.split re_path file in
        (* Windows paths may start with '<drive>:'. If so, remove it *)
        let parts = if List.length parts > 0 &&
                       Str.string_match re_drive (List.nth parts 0) 0 then
            List.tl parts
          else parts
        in
        let path = norm base parts in
        Weak.set cache (hash land 255) (Some(file,path)) ;
        path

let pwd = insert dummy (Sys.getcwd())

let normalize ?base p =
  if p = "" then raise (Invalid_argument "Filepath.normalize");
  let base = match base with None -> pwd | Some p -> insert pwd p in
  let res = (insert base p).path in
  if res = "" then "/" else res

(* -------------------------------------------------------------------------- *)
(* --- Symboling Names                                                    --- *)
(* -------------------------------------------------------------------------- *)
                    
let add_symbolic_dir name dir = (insert pwd dir).symbol <- name

let rec add_path buffer p =
  let open Buffer in
  if p.symbol <> "" then
    add_string buffer p.symbol
  else
    match p.dir with
    | None -> add_string buffer p.path
    | Some d ->
        if d != pwd (* hconsed *) then
          ( add_path buffer d ; add_char buffer '/' ) ;
        add_string buffer p.name

let rec skip_dot f =
  if Extlib.string_prefix "./" f then
    skip_dot (String.sub f 2 (String.length f - 2))
  else f

let pretty file =
  if Filename.is_relative file then
    skip_dot file
  else
    let path = insert pwd file in
    let file = path.path in
    let pwd = pwd.path in
    if Extlib.string_prefix ~strict:true pwd file then
      let n = 1 + String.length pwd in
      String.sub file n (String.length file - n)
    else
      let buffer = Buffer.create 80 in
      add_path buffer path ;
      Buffer.contents buffer

(* -------------------------------------------------------------------------- *)
(* --- Relative Paths                                                     --- *)
(* -------------------------------------------------------------------------- *)

let relativize ?base file =
  let file = (insert pwd file).path in
  let base = match base with
    | None -> pwd.path
    | Some base -> (insert pwd base).path in
  if Extlib.string_prefix ~strict:true base file then
    let n = 1 + String.length base in
    String.sub file n (String.length file - n)
  else file

let is_relative ?base file =
  let file = (insert pwd file).path in
  let base = match base with
    | None -> pwd.path
    | Some base -> (insert pwd base).path in
  Extlib.string_prefix ~strict:true base file

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

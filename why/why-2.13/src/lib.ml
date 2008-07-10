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

(*i $Id: lib.ml,v 1.11 2008/07/03 07:34:28 marche Exp $ i*)

module Sset = Set.Make(String)

(* small library common to Why and Caduceus *)

let mkdir_p dir =
  if Sys.file_exists dir then begin
    if (Unix.stat dir).Unix.st_kind <> Unix.S_DIR then
      failwith ("failed to create directory " ^ dir)
  end else
    Unix.mkdir dir 0o777

let file ~dir ~file = 
  mkdir_p dir;
  Filename.concat dir (Filename.basename file)

let file_subdir ~dir ~file = 
  let d = Filename.dirname file in
  let d = Filename.concat d dir in
  mkdir_p d;
  Filename.concat d (Filename.basename file)

let file_copy src dest =
  let cin = open_in src
  and cout = open_out dest
  and buff = String.make 1024 ' ' 
  and n = ref 0 
  in
  while n := input cin buff 0 1024; !n <> 0 do 
    output cout buff 0 !n
  done;
  close_in cin; close_out cout

let file_copy_if_different src dst =
  if not (Sys.file_exists dst) || Digest.file dst <> Digest.file src then
    file_copy src dst



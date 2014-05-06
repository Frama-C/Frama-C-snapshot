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

open Bigarray
open Unix
let compare_files f f' = 
  let fd = Unix.openfile f [Unix.O_RDONLY] 0o000 in
  let fd' = Unix.openfile f' [Unix.O_RDONLY] 0o000 in
  let size_byte = (Unix.fstat fd).st_size in
  let size_byte' = (Unix.fstat fd').st_size in
  if size_byte' <> size_byte then false
  else
    (try 
       let initial_padding = size_byte mod 8 in
       for i = 1 to initial_padding do
         let s = "_" in 
         let s' = "_" in
         assert (Unix.read fd s 0 1=1); 
         assert (Unix.read fd' s' 0 1=1);
         if s <> s' then raise Not_found
       done;
       let size_bigarray = size_byte / 8 in 
       let mapped = Array1.map_file fd int64 c_layout false size_bigarray in
       let mapped' = Array1.map_file fd' int64 c_layout false size_bigarray in
       mapped = mapped'
     with Not_found -> false)

let () = Format.printf "GOT:%b@." (compare_files "/tmp/big.mmap" "/tmp/big.mmap")

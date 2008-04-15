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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Unix

let usage_string = "cadlog [options] file"

let usage () =
  eprintf "usage: %s@." usage_string;
  exit 1

let file = ref ""

let def_file s =
  if !file="" then file := s
  else usage () 
  
let jessie = ref false

let krakatoa = ref false

let _ = 
  Arg.parse 
      [ "-jc", Arg.Set jessie, "  Jessie bench";
	"-java", Arg.Set krakatoa, "  Krakatoa bench";
      ]
      def_file usage_string

let file = if !file ="" then usage() else !file

let c = open_out file

let fmt = formatter_of_out_channel c

let tool = 
  if !jessie then "Jessie" else 
  if !krakatoa then "Krakatoa" else 
    "Caduceus"

let version,date =
  if !jessie then Jc_version.version, Jc_version.date else 
  if !krakatoa then Java_version.version, Java_version.date else 
    Cversion.version, Cversion.date

let d,m,y =
  let tm = localtime (time ()) in
    tm.tm_mday, 1+tm.tm_mon, 1900+tm.tm_year

let () =
  fprintf fmt "%8s version         : %s@." tool version;
  fprintf fmt "%8s compilation date: %s@." tool date;
  fprintf fmt "Bench execution date     : %d/%d/%d@." d m y;
  try
    while true do
      let s = read_line () in
	print_endline s;
	fprintf fmt "%s@." s
    done
  with End_of_file ->
    close_out c

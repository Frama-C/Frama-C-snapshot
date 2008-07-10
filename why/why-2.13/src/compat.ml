(*
 * The Caduceus certification tool
 * Copyright (C) 2003 Jean-Christophe Filliâtre - Claude Marché
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(*i $Id: compat.ml4,v 1.2 2005/01/11 13:20:04 hubert Exp $ i*)

(* compatibility file between ocaml 3.07 and 3.08 
   preprocessed with camlp4 with -DOCAML307 or -DOCAML308 *)

let make_loc (b, e as l) = b.Lexing.pos_cnum, e.Lexing.pos_cnum

let offset ofs (b, e) =
  {b with Lexing.pos_cnum = b.Lexing.pos_cnum + ofs},
  {e with Lexing.pos_cnum = e.Lexing.pos_cnum + ofs}

let compare_for_set_fold x y = Pervasives.compare y x

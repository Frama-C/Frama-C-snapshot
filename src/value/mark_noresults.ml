(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Cil_types

class mark_visitor = object(_self)
  inherit Cil.nopCilVisitor as super

  method vstmt s =
    Db.Value.update_table (Kstmt s) Relations_type.Model.empty;
    Cil.DoChildren

end

let run () =
  let names = (Value_parameters.NoResultsFunctions.get ()) in
    if Value_parameters.NoResultsAll.get() ||
      not (Cilutil.StringSet.is_empty names)
    then
      let visitor = new mark_visitor in
	Globals.Functions.iter_on_fundecs
	  (fun afundec -> 
	     if Value_parameters.NoResultsAll.get() ||
	       Cilutil.StringSet.mem afundec.svar.vname names
	     then
	       ignore (Cil.visitCilFunction (visitor:>Cil.cilVisitor) afundec);)
    

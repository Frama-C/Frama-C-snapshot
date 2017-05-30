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

module Printer_extension(X:Printer.PrinterClass) = struct

  class printer = object
    inherit Printer.extensible_printer () as super

    method! global fmt g =
      let loc, _ = Cil_datatype.Global.loc g in
      let file = loc.Lexing.pos_fname in
      if file = "" || List.exists
        (fun s -> Filepath.normalize s = file)
        (Kernel.Files.get ())
      then super#global fmt g

  end

end

let () = Printer.update_printer (module Printer_extension)

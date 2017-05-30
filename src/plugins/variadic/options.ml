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

module Self = Plugin.Register
  (struct
    let name = "Variadic"
    let shortname = "variadic"
    let help = "Variadic functions translation"
   end)

module Enabled = Self.True
  (struct
    let option_name = "-variadic-translation"
    let help = "translate variadic functions and calls to semantic \
                equivalents with only a fixed list of formal parameters"
   end)

module Strict = Self.True
  (struct
    let option_name = "-variadic-strict"
    let help = "display warnings about non-portable implicit casts in the \
                calls of standard variadic functions, i.e. casts between \
                distinct integral types which has the same size and \
                signedness"
   end)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

module Self = Plugin.Register
  (struct
    let name = "Variadic"
    let shortname = "va"
    let help = "Variadic functions translation"
   end)

module Enabled = Self.False
  (struct
    let option_name = "-va"
    let help = "translate variadic functions and calls to semantic \
                equivalents with only a fixed list of formal parameters"
   end)

module Strict = Self.True
  (struct
    let option_name = "-va-strict"
    let help = "display warnings about non-portable implicit casts in the \
                calls of standard variadic functions, i.e. casts between \
                distinct integral types which has the same size and \
                signedness"
   end)

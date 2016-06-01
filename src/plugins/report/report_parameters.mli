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

include Plugin.General_services
module Enabled : Parameter_sig.Bool

module PrintProperties: Parameter_sig.Bool

module Untried: Parameter_sig.Bool
module Specialized: Parameter_sig.Bool
module Proven: Parameter_sig.Bool

module CSVFile: Parameter_sig.String

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

include Plugin.General_services

module Print: Parameter_sig.Bool
module PrintProperties: Parameter_sig.Bool

module Untried: Parameter_sig.Bool
module Specialized: Parameter_sig.Bool
module Proven: Parameter_sig.Bool

module CSVFile: Parameter_sig.String
module Classify: Parameter_sig.Bool
module Rules: Parameter_sig.String_list
module Warning: Parameter_sig.String
module Error: Parameter_sig.String
module Status: Parameter_sig.Bool
module UntriedStatus: Parameter_sig.String
module UnknownStatus: Parameter_sig.String
module InvalidStatus: Parameter_sig.String

module Output: Parameter_sig.String
module OutputReviews: Parameter_sig.String
module OutputErrors: Parameter_sig.String
module OutputUnclassified: Parameter_sig.String
module AbsolutePath: Parameter_sig.Bool
module Stdout: Parameter_sig.Bool
module Stderr: Parameter_sig.Bool
module Exit: Parameter_sig.Bool

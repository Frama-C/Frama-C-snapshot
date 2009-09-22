(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id$ *)

open Plugin

include Log.Messages

module Verbose : INT
module Ltl_File : STRING
module To_Buchi: STRING
module Buchi: STRING
module Ya: STRING
module Output_Spec : BOOL
module Output_C_File : STRING
module Dot : BOOL
module AbstractInterpretation : BOOL
module Axiomatization : BOOL
module ConsiderAcceptance : BOOL
module AutomataSimplification : BOOL
module Test : INT
module AddingOperationNameAndStatusInSpecification:BOOL

val is_on : unit -> bool
val promela_file: unit -> string
val advance_abstract_interpretation: unit -> bool

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

(* $Id: ltlast.mli,v 1.3 2008-10-13 09:21:24 uid588 Exp $ *)

(** The abstract tree of LTL formula. Such tree is used by ltl parser/lexer before its translation into Buchi automata by the LTL2BA external tool. *)


(** LTL formula parsed abstract syntax trees *)
type formula =
  | LNext of formula              (** 'Next' temporal operator *)
  | LUntil of formula * formula   (** 'Until' temporal operator *)
  | LFatally of formula           (** 'Fatally' temporal operator *)
  | LGlobally of formula          (** 'Globally' temporal operator *)
  | LRelease of formula * formula (** 'Release' temporal operator (reminder: f1 R f2 <=> !(!f1 U !f2)) *)

  | LNot of formula               (** 'not' logic operator *)
  | LAnd of formula * formula     (** 'and' logic operator *)
  | LOr of formula * formula      (** 'or' logic operator *)
  | LImplies of formula * formula (** '=>' logic operator *)
  | LIff of formula * formula     (** '<=>' logic operator *)

  | LTrue                         (** 'true' logic constant *)
  | LFalse                        (** 'false' logic constant *)

  | LCall of string               (** Logic predicate. The String has to be the name of an operation from C program *)
  | LReturn of string             (** Logic predicate. The String has to be the name of an operation from C program *)
  | LCallOrReturn of string       (** Logic predicate. The String has to be the name of an operation from C program *)

  | LIdent of string              (** Logic expression. The String is the name of a fresh variable defined by the expression and used to be in conformance with the input syntax of LTL2BA tool. *)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

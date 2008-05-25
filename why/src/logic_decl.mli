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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: logic_decl.mli,v 1.15 2008/02/20 14:34:26 marche Exp $ i*)

(*s Logical declarations. 
    This is what is sent to the various provers (see main.ml and the provers
    interfaces). *)

open Cc
open Logic

type loc = Loc.floc
type 'a scheme = 'a Env.scheme

type expl_kind = 
  | EKAbsurd
  | EKAssert
  | EKLoopInvInit of string
  | EKLoopInvPreserv of string
  | EKPost
  | EKPre of string
  | EKOther of string
  | EKVarDecr
  | EKWfRel 
  | EKLemma
      
type vc_expl =
    { lemma_or_fun_name : string;
      behavior : string;
      vc_loc : Loc.floc;
      vc_kind : expl_kind }

type obligation = Loc.floc * vc_expl * string * sequent
    (* loc, explanation, id, sequent *) 

type t =
  | Dtype          of loc * string list * string
  | Dlogic         of loc * string * logic_type scheme
  | Dpredicate_def of loc * string * predicate_def scheme
  | Dfunction_def  of loc * string * function_def scheme
  | Daxiom         of loc * string * predicate scheme
  | Dgoal          of loc * vc_expl * string * sequent scheme


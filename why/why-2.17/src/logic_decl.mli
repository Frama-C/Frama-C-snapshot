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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: logic_decl.mli,v 1.20 2008/11/05 14:03:17 filliatr Exp $ i*)

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
  | Dpredicate_def of loc * Ident.t * predicate_def scheme
  | Dinductive_def of loc * Ident.t * inductive_def scheme
  | Dfunction_def  of loc * Ident.t * function_def scheme
  | Daxiom         of loc * string * predicate scheme
  | Dgoal          of loc * vc_expl * string * sequent scheme


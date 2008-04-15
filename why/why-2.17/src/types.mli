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

(*i $Id: types.mli,v 1.25 2008/11/05 14:03:18 filliatr Exp $ i*)

(*s Types for programs.
    Types of values ([type_v]) and computations ([type_c]) *)

open Logic

type assertion = predicate

type precondition = assertion

type postcondition = assertion * (Ident.t * assertion) list

type 'a binder = Ident.t * 'a

type type_v = 
  | PureType of pure_type
  | Ref of pure_type
  | Arrow of type_v binder list * type_c

and type_c = 
  { c_result_name : Ident.t;
    c_result_type : type_v;
    c_effect : Effect.t;
    c_pre    : precondition list;
    c_post   : postcondition option }

type transp = Transparent | Opaque


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


type goal = private {
  goal_expl : Logic_decl.vc_expl;
  goal_file : string;
  sub_goal : goal list;
  proof : (string*string*string*string*string) list;
  mutable goal_tags : (string*string) list;
}

type lemma = private {
  lemma_name : string;
  lemma_loc : Loc.floc;
  lemma_goal : goal; 
  mutable lemma_tags : (string*string) list; 
}

type behavior = {
  behavior_name : string;
  behavior_loc : Loc.floc;
  mutable behavior_goals : goal list;
  mutable behavior_tags : (string*string) list; 
}

type funct = private {
  function_name : string;
  function_loc : Loc.floc;
  mutable function_behaviors : behavior list;
  mutable function_tags : (string*string) list; 
}
  

type t = private {
  project_name : string;
  mutable project_context_file : string;
  mutable project_lemmas : lemma list;
  mutable project_functions : funct list;
}

(* creations *)
val create : string -> t
val set_project_context_file : t -> string -> unit
val add_lemma : t -> string -> Logic_decl.vc_expl -> string -> lemma
val add_function : t -> string -> Loc.floc -> funct
val add_behavior : funct -> string -> Loc.floc -> behavior
val add_goal : behavior -> Logic_decl.vc_expl -> string -> goal

(* toggle visibility *)

val toggle_lemma : lemma -> unit
val toggle_function : funct -> unit
val toggle_behavior : behavior -> unit
val toggle_goal : goal -> unit

(* save/load *)

val save : t -> string -> unit

val load : string -> t

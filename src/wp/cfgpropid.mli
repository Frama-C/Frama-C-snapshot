(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(** Indexed goals by [prop_id] *)

module Create (W : Mcfg.S) :
sig
  type description
  type t_goal = {
    g_id : WpPropId.prop_id ;
    g_prop : W.t_prop;
    g_descr : description;
  }

  val pp_goal : Format.formatter -> string -> t_goal -> unit

  val pp_descr :  Format.formatter -> t_goal -> unit

  val iter_description :
    (Wpo.warning -> unit) ->
    (Property.t -> unit) ->
    description -> unit

  include Mcfg.S with type t_prop = t_goal list

end

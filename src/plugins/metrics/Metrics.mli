(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Metrics plugin. *)

(** See {!Metrics_coverage}. *)
module Metrics_coverage : sig
  val compute_syntactic:
    libc:bool -> Kernel_function.t -> Cil_datatype.Varinfo.Set.t

  (**/**)
  val compute_semantic:
    libc:bool -> Cil_datatype.Varinfo.Set.t
end

(** See {!Metrics_base}. *)
module Metrics_base : sig
  module OptionKf :
    Datatype.S_with_collections with type t = Kernel_function.t option
  module BasicMetrics : sig
    type t = {
      cfile_name : Filepath.Normalized.t;
      cfunc : Kernel_function.t option;
      cslocs: int;
      cifs: int;
      cloops: int;
      ccalls: int;
      cgotos: int;
      cassigns: int;
      cexits: int;
      cfuncs: int;
      cptrs: int;
      cdecision_points: int;
      cglob_vars: int;
      ccyclo: int;
    }
  end
end

(** See {!Metrics_cilast}. *)
module Metrics_cilast : sig
  val get_metrics_map: libc:bool ->
    (Metrics_base.BasicMetrics.t Metrics_base.OptionKf.Map.t)
      Datatype.Filepath.Map.t
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

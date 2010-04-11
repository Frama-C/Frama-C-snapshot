(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

open Cil_types
open Cilutil
open Project.Datatype

(** Datatypes of some useful CIL types.
    @plugin development guide *)

(** @plugin development guide *)
module Varinfo : S with type t = varinfo

module Location : S with type t = location

module Block: S with type t = block

(** @plugin development guide *)
module Stmt: sig
  include S with type t = stmt
  val compare: t -> t -> int
end

(** @plugin development guide *)
module Kinstr: S with type t = kinstr

(** @plugin development guide *)
module Lval: S with type t = lval

(** Datatype for a cil file. *)
module File: S with type t = file
module UntypedFiles: S with type t = Cabs.file list
module InitInfo: S with type t = initinfo
  (** @deprecated since Boron-20100401 *)

module Initinfo: S with type t = initinfo

module Enuminfo : S with type t = enuminfo
  (** @since Boron-20100401 *)

module Typeinfo : S with type t = typeinfo
  (** @since Boron-20100401 *)

(** {3 Hashtables for Cil types} *)

module IntHashtbl(Data:S) :
  S with type t = Data.t Inthash.t

module InstrHashtbl(Data:S) :
  S with type t = Data.t InstrHashtbl.t

module StmtHashtbl(Data:S) :
  S with type t = Data.t StmtHashtbl.t

(** @plugin development guide *)
module VarinfoHashtbl(Data:S) :
  S with type t = Data.t VarinfoHashtbl.t

(** {3 Sets} *)

(** Datatype for a set of statements.
    @plugin development guide *)
module StmtSet: S with type t = Cilutil.StmtSet.t

(** Datatype for a reference to a set of statements. *)
module StmtSetRef: S with type t = Cilutil.StmtSet.t ref

(** @since Boron-20100401 *)
module VarinfoSet: S with type t = Cilutil.VarinfoSet.t

(** @since Boron-20100401 *)
module EnuminfoSet: S with type t = Cilutil.EnuminfoSet.t

(** @since Boron-20100401 *)
module TypeinfoSet: S with type t = Cilutil.TypeinfoSet.t

(** {3 Lists} *)

(** Datatype for a set of datatypes. *)
module StmtList: S with type t = stmt list

(** @since Boron-20100401 *)
module VarinfoList: S with type t = varinfo list

(** {3 Annotations} *)

module Code_Annotation: S with type t = code_annotation
module Logic_Info: S with type t = logic_info
module Builtin_Logic_Info: S with type t = builtin_logic_info
module Logic_Type_Info: S with type t = logic_type_info
module Logic_Ctor_Info: S with type t = logic_ctor_info

module Annot_Status: S with type t = annot_status
  (** @deprecated Boron-20100401 *)

module Annot_Status_List: S with type t = annot_status list
  (** @deprecated Boron-20100401 *)

module Annotation_Status: S with type t = annotation_status

(*
module Predicate_Info: S with type t = predicate_info
*)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

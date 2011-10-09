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

(** Datatypes of some useful CIL types.
    @plugin development guide *)

open Cil_types
open Datatype

(**************************************************************************)
(** {3 Cabs types} *)
(**************************************************************************)

module Cabs_file: S with type t = Cabs.file

(**************************************************************************)
(** {3 C types}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Block: S with type t = block
module Compinfo: S_with_collections with type t = compinfo
module Enuminfo: S_with_collections with type t = enuminfo
module Enumitem: S_with_collections with type t = enumitem

(** Note that the equality is based on eid. For structural equality, use
    {!Cil.compareExp} *)
module Exp: sig
  include S_with_collections with type t = exp
  val dummy: exp (** @since Nitrogen-20111001 *)
end

module Fieldinfo: S_with_collections with type t = fieldinfo
module File: S with type t = file

module Global: sig
  include S_with_collections with type t = global
  val loc: t -> location
end

module Initinfo: S with type t = initinfo

module Instr: sig
  include S with type t = instr
  val loc: t -> location
end

module Kinstr: sig
  include S_with_collections with type t = kinstr
  val kinstr_of_opt_stmt: stmt option -> kinstr
    (** @since Nitrogen-20111001. *)

  val loc: t -> location
end

module Label: S with type t = label

(** Single position in a file
    @since Nitrogen-20111001
*)
module Position: S_with_collections with type t = Lexing.position

(** Cil locations *)
module Location: sig
  include S_with_collections with type t = location
  val unknown: t
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

(** Note that the equality is based on eid (for sub-expressions). 
    For structural equality, use {!Cil.compareLval} *)
module Lval: sig
  include S_with_collections with type t = lval
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

(** Same remark as for Lval. For structural equality, use {!Cil.compareOffset} *)
module Offset: sig
  include S_with_collections with type t = offset
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module Stmt: sig
  include S_with_collections with type t = stmt
  module Hptset: sig include Hptset.S with type elt = stmt
                     val self: State.t end
  val loc: t -> location
  val pretty_sid: Format.formatter -> t -> unit
    (** Pretty print the sid of the statement
        @since Nitrogen-20111001 *)
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module Typ: sig
  include S_with_collections with type t = typ
(**/**)
val pretty_ref: (Format.formatter -> t -> unit) ref
end
(**/**) (* Forward declarations from Cil *)
val pbitsSizeOf : (typ -> int) ref
val ptypeAddAttributes: (attributes -> typ -> typ) ref
(**/**)


module Typeinfo: S_with_collections with type t = typeinfo

module Varinfo: sig
  include S_with_collections with type t = varinfo
  module Hptset: sig include Hptset.S with type elt = t
                     val self: State.t end
  val dummy: t
  val pretty_vname: Format.formatter -> t -> unit
  (** Pretty print the name of the varinfo.
      @since Nitrogen-20111001 *)
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
  val internal_pretty_code_ref:
    (Type.precedence -> Format.formatter -> t -> unit) ref
end

module Kf: sig
  include Datatype.S_with_collections with type t = kernel_function
  val vi: t -> varinfo
  val id: t -> int

  (**/**)
  val set_formal_decls: (varinfo -> varinfo list -> unit) ref
(**/**)
end

(**************************************************************************)
(** {3 ACSL types}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Builtin_logic_info: S_with_collections with type t = builtin_logic_info

module Code_annotation: sig
  include S_with_collections with type t = code_annotation
  val loc: t -> location option
end

module Rooted_code_annotation: Datatype.S with type t = rooted_code_annotation

module Global_annotation: sig
  include S_with_collections with type t = global_annotation
  val loc: t -> location
end

module Logic_ctor_info: S_with_collections with type t = logic_ctor_info
module Logic_info: S_with_collections with type t = logic_info

module Logic_type: sig
  include S_with_collections with type t = logic_type
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module Logic_type_info: S_with_collections with type t = logic_type_info
module Identified_term: S_with_collections with type t = identified_term

module Logic_var: sig
  include S_with_collections with type t = logic_var
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module Term: sig
  include S_with_collections with type t = term
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module Term_lhost: S_with_collections with type t = term_lhost
module Term_offset: S_with_collections with type t = term_offset
module Term_lval: S_with_collections with type t = term_lval

module Logic_label: S_with_collections with type t = logic_label

(**************************************************************************)
(** {3 Logic_ptree}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Lexpr: S with type t = Logic_ptree.lexpr

(**************************************************************************)
(** {3 Other types} *)
(**************************************************************************)

module Int_hashtbl: Hashtbl with type 'a t = 'a Inthash.t and type key = int

module Localisation: Datatype.S with type t = localisation

module Alarm: Datatype.S_with_collections with type t = alarm

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

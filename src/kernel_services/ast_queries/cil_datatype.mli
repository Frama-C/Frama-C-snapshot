(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Datatypes of some useful CIL types.
    @plugin development guide *)

open Cil_types
open Datatype

(** Auxiliary module for datatypes that can be pretty-printed. For those that
    do not have this signature, module {!Printer} must be used. *)
module type S_with_pretty = sig
  include S 
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end
module type S_with_collections_pretty = sig
  include S_with_collections 
  (**/**)
  val pretty_ref: (Format.formatter -> t -> unit) ref
end


(**************************************************************************)
(** {3 Localisations} *)
(**************************************************************************)


(** Single position in a file.
    @since Nitrogen-20111001 *)
module Position: S_with_collections_pretty with type t = Lexing.position

(** Cil locations. *)
module Location: sig
  include S_with_collections_pretty with type t = location
  val unknown: t
  val pretty_long : t Pretty_utils.formatter
    (** Pretty the location under the form [file <f>, line <l>], without
        the full-path to the file. The default pretty-printer [pretty] echoes
        [<dir/f>:<l>] *)
  val pretty_line: t Pretty_utils.formatter
  (** Prints only the line of the location *)
end

module Localisation: Datatype.S with type t = localisation

(**************************************************************************)
(** {3 Cabs types} *)
(**************************************************************************)

module Cabs_file: S_with_pretty with type t = Cabs.file

(**************************************************************************)
(** {3 C types}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Block: S_with_pretty with type t = block
(* Blocks cannot compared or hashed, so collections are not available *)

module Compinfo: S_with_collections_pretty with type t = compinfo
module Enuminfo: S_with_collections_pretty with type t = enuminfo
module Enumitem: S_with_collections_pretty with type t = enumitem

(**
   @since Fluorine-20130401
*)
module Wide_string: S_with_collections with type t = int64 list

(**
   @since Oxygen-20120901 
*)
module Constant: S_with_collections_pretty with type t = constant

(** Note that the equality is based on eid. For structural equality, use
    {!ExpStructEq} *)
module Exp: sig
  include S_with_collections_pretty with type t = exp
  val dummy: exp (** @since Nitrogen-20111001 *)
end

module ExpStructEq: S_with_collections with type t = exp

module Fieldinfo: S_with_collections_pretty with type t = fieldinfo

module File: S with type t = file

module Global: sig
  include S_with_collections_pretty with type t = global
  val loc: t -> location
  val attr: t -> attributes
  (** @since Phosphorus-20170501-beta1 *)
end

module Initinfo: S_with_pretty with type t = initinfo

module Instr: sig
  include S_with_pretty with type t = instr
  val loc: t -> location
end

module Kinstr: sig
  include S_with_collections with type t = kinstr
  val kinstr_of_opt_stmt: stmt option -> kinstr
    (** @since Nitrogen-20111001. *)

  val loc: t -> location
end

module Label: S_with_collections_pretty with type t = label

(** Note that the equality is based on eid (for sub-expressions). 
    For structural equality, use {!LvalStructEq} *)
module Lval: S_with_collections_pretty with type t = lval

(**
   @since Oxygen-20120901 
*)
module LvalStructEq: S_with_collections with type t = lval

(** Same remark as for Lval. 
    For structural equality, use {!OffsetStructEq}. *) 
module Offset: S_with_collections_pretty with type t = offset

(** @since Oxygen-20120901 *)
module OffsetStructEq: S_with_collections with type t = offset

module Stmt_Id:  Hptmap.Id_Datatype with type t = stmt
module Stmt: sig
  include S_with_collections_pretty with type t = stmt
  module Hptset: sig
    include Hptset.S with type elt = stmt
                     and type 'a shape = 'a Hptmap.Shape(Stmt_Id).t
    val self: State.t
  end
  val loc: t -> location
  val pretty_sid: Format.formatter -> t -> unit
    (** Pretty print the sid of the statement
        @since Nitrogen-20111001 *)
end

module Attribute: S_with_collections_pretty with type t = attribute
module Attributes: S_with_collections with type t = attributes


(** Types, with comparison over struct done by key and unrolling of typedefs. *)
module Typ: sig
  include S_with_collections_pretty with type t = typ
  val toplevel_attr: t -> attributes
    (** returns the attributes associated to the toplevel type, without adding
        attributes from compinfo, enuminfo or typeinfo. Use {!Cil.typeAttrs}
        to retrieve the complete set of attributes. *)
end

(** Types, with comparison over struct done by name and no unrolling. *)
module TypByName: S_with_collections_pretty with type t = typ

(** Types, with comparison over struct done by key and no unrolling
    @since Fluorine-20130401 
 *)
module TypNoUnroll: S_with_collections_pretty with type t = typ


module Typeinfo: S_with_collections with type t = typeinfo

module Varinfo_Id: Hptmap.Id_Datatype

(** @plugin development guide *)
module Varinfo: sig
  include S_with_collections_pretty with type t = varinfo
  module Hptset: sig
    include Hptset.S with type elt = varinfo
                     and type 'a shape = 'a Hptmap.Shape(Varinfo_Id).t
    val self: State.t
  end
  val dummy: t
  (**/**)
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

module Builtin_logic_info: S_with_collections_pretty with type t = builtin_logic_info

module Code_annotation: sig
  include S_with_collections_pretty with type t = code_annotation
  val loc: t -> location option
end

module Funbehavior: S with type t = funbehavior

module Funspec: S_with_pretty with type t = funspec

(** @since Fluorine-20130401 *)
module Fundec: S_with_collections_pretty with type t = fundec

module Global_annotation: sig
  include S_with_collections_pretty with type t = global_annotation
  val loc: t -> location

  val attr: t -> attributes
  (** attributes tied to the global annotation.
      @since Phosphorus-20170501-beta1 *)
end

module Identified_term: S_with_collections_pretty with type t = identified_term

module Logic_ctor_info: S_with_collections_pretty with type t = logic_ctor_info
module Logic_info: S_with_collections_pretty with type t = logic_info
module Logic_constant: S_with_collections_pretty with type t = logic_constant

module Logic_label: S_with_collections_pretty with type t = logic_label

(** Logic_type. See the various [Typ*] modules for the distinction between
    those modules *)
module Logic_type: S_with_collections_pretty with type t = logic_type
module Logic_type_ByName: S_with_collections_pretty with type t = logic_type
module Logic_type_NoUnroll: S_with_collections_pretty with type t = logic_type

module Logic_type_info: S_with_collections_pretty with type t = logic_type_info

module Logic_var: S_with_collections_pretty with type t = logic_var

(** @since Oxygen-20120901 *)
module Model_info: S_with_collections_pretty with type t = model_info

module Term: S_with_collections_pretty with type t = term

module Term_lhost: S_with_collections_pretty with type t = term_lhost
module Term_offset: S_with_collections_pretty with type t = term_offset
module Term_lval: S_with_collections_pretty with type t = term_lval

module Predicate: S_with_pretty with type t = predicate
module Identified_predicate: 
  S_with_collections_pretty with type t = identified_predicate
(** @since Neon-20140301 *)

(**************************************************************************)
(** {3 Logic_ptree}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Lexpr: S_with_pretty with type t = Logic_ptree.lexpr

(**/**)
(* ****************************************************************************)
(** {2 Internal API} *)
(* ****************************************************************************)

(* Forward declarations from Cil *)
val drop_non_logic_attributes : (attributes -> attributes) ref
val constfoldtoint : (exp -> Integer.t option) ref
val punrollType: (typ -> typ) ref
(**/**)

val clear_caches: unit -> unit


(**/**)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

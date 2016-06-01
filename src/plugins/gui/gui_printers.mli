(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(** Special pretty-printers for the GUI. Some sub-elements are annotated
    by format tags, in order to make them reactive. *)

val get_type_specifier: Cil_types.typ -> Cil_types.typ
(** Returns the base type for a pointer/array, otherwise [t] itself.
    E.g. for [t = int***], returns [int]. *)

val pp_typ: Format.formatter -> Cil_types.typ -> unit
(** Same as {!Printer.pp_typ}, except that the type is output between
    Format tags [@{<link:typN>}], that are recognized by the GUI. *)

val pp_typ_unfolded: Format.formatter -> Cil_types.typ -> unit
(** Pretty-prints a type, unfolding it once if it is a typedef, enum, struct or
    union. *)

module LinkPrinter: Printer.PrinterExtension
(** Special pretty-printer that outputs tags [link:vidN] around varinfos,
    and [link:typN] around types. *)

exception NoMatch

val varinfo_of_link: string -> Cil_types.varinfo
(** Convert a string of the form [link:vidN] into the varinfo of vid [N]. This
    varinfo must have been printed by a pretty-printer extended with
    {!LinkPrinter}.
    Raise [NoMatch] if the link is not of the form [link:vidN]. *)

val typ_of_link: string -> Cil_types.typ
(** Convert a string of the form [link:typN] into a type. The association
    between [N] and the type is done by printing the type once using
    {!pp_typ}, or by using a printer extended with {!LinkPrinter}.
    Raise [NoMatch] if the link is not of the form [link:typN]. *)

val loc_of_link: string -> Cil_types.location
(** Convert a string of the form [link:locN] into the location of id [N]. This
    location must have been printed by a pretty-printer extended with
    {!LinkPrinter}.
    Raise [NoMatch] if the link is not of the form [link:locN]. *)

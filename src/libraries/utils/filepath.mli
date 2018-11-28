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

(** Functions manipulating filepaths.
    In these functions, references to the current working directory refer
    to the result given by function Sys.getcwd.

    NOTE: Prefer using the [Normalized] module whenever possible.
*)

(** Returns an absolute path leading to the given file.
    The result is similar to [realpath --no-symlinks].
    Some special behaviors include:
    - [normalize ""] (empty string) returns ""
      (realpath returns an error);
    - [normalize] preserves multiple sequential '/' characters,
      unlike [realpath];
    - non-existing directories in [realpath] may lead to ENOTDIR errors,
      but [normalize] may accept them.
    @modify Aluminium-20160501 optional base_name. *)
val normalize: ?base_name:string -> string -> string

(** [relativize base_name file_name] returns a relative path name of
    [file_name] w.r.t. [base_name], if [base_name] is a prefix of [file];
    otherwise, returns [file_name] unchanged.
    The default base name is the current working directory name.
    @since Aluminium-20160501 *)
val relativize: ?base_name:string -> string -> string

(** returns true if the file is relative to [base]
    (that is, it is prefixed by [base_name]), or to the current working directory
    if no base is specified.
    @since Aluminium-20160501 *)
val is_relative: ?base_name:string -> string -> bool

(** DEPRECATED: use [Normalized.to_pretty_string] instead.
    Pretty-print a path according to these rules:
    - relative filenames are kept, except for leading './', which are stripped;
    - absolute filenames are relativized if their prefix is included in the
      current working directory; also, symbolic names are resolved,
      i.e. the result may be prefixed by known aliases (e.g. FRAMAC_SHARE).
      See {!add_symbolic_dir} for more details.
    Therefore, the result of this function may not designate a valid name
    in the filesystem.

    @since Neon-20140301
    @deprecated since 18.0-Argon
*)
val pretty: string -> string
[@@deprecated "Use Filepath.Normalized.to_pretty_string instead."]

(** [add_symbolic_dir name dir] indicates that the (absolute) path [dir] must
    be replaced by [name] when pretty-printing paths.
    This alias ensures that system-dependent paths such as FRAMAC_SHARE are
    printed identically in different machines. *)
val add_symbolic_dir: string -> string -> unit

(** The [Normalized] module is simply a wrapper that ensures that paths are
    always normalized. Used by [Datatype.Filepath].
    @since 18.0-Argon *)
module Normalized: sig

  (** The normalized (absolute) path. *)
  type t = private string

  (** [of_string s] converts [s] into a normalized path.
      @raise Invalid_argument if [s] is the empty string. *)
  val of_string: ?base_name:string -> string -> t

  (** [to_pretty_string p] returns [p] prettified,
      that is, a relative path-like string.
      Note that this prettified string may contain symbolic dirs and is thus
      is not a path.
      See [pretty] for details about usage. *)
  val to_pretty_string: t -> string

  val equal: t -> t -> bool

  (** Compares normalized paths *)
  val compare: t -> t -> int

  (** Compares prettified (i.e. relative) paths, with or without
      case sensitivity (by default, [case_sensitive = false]). *)
  val compare_pretty : ?case_sensitive:bool -> t -> t -> int

  (** Pretty-print a path according to these rules:
      - relative filenames are kept, except for leading './',
        which are stripped;
      - absolute filenames are relativized if their prefix is included in the
        current working directory; also, symbolic names are resolved,
        i.e. the result may be prefixed by known aliases (e.g. FRAMAC_SHARE).
        See {!add_symbolic_dir} for more details.
      Therefore, the result of this function may not designate a valid name
      in the filesystem and must ONLY be used to pretty-print information;
      it must NEVER to be converted back to a filepath later.
  *)
  val pretty: Format.formatter -> t -> unit

  (** Pretty-prints the normalized (absolute) path. *)
  val pp_abs: Format.formatter -> t -> unit

  (** Unknown filepath, used as 'dummy' for [Datatype.Filepath]. *)
  val unknown: t
end

(** Describes a position in a source file.
    @since 18.0-Argon
*)
type position =
  {
    pos_path : Normalized.t;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

(** Pretty-prints a position, in the format file:line.
    @since 18.0-Argon
*)
val pp_pos : Format.formatter -> position -> unit

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)

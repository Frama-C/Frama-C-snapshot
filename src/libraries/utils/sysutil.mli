(**************************************************************************)
(*                                                                        *)
(*  The Why3 Verification Platform   /   The Why3 Development Team        *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University        *)
(*                                                                        *)
(*  This software is distributed under the terms of the GNU Lesser        *)
(*  General Public License version 2.1, with the special exception        *)
(*  on linking described in file LICENSE.                                 *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** System utilities (filename management, etc). *)

val backup_file : string -> unit
(** Create a backup copy of a file if it exists. Do nothing otherwise. *)

val channel_contents : in_channel -> string
(** @return the content of an in-channel. *)

val channel_contents_buf : in_channel -> Buffer.t
(** @return the content of an in_channel in a buffer. *)

val channel_contents_fmt : in_channel -> Format.formatter -> unit
(** Put the content of an in_channel in a formatter *)

val fold_channel : ('a -> string -> 'a) -> 'a -> in_channel -> 'a
(** Fold on the line of a file. *)

val file_contents : string -> string
(** @return the content of a file. *)

val file_contents_buf : string -> Buffer.t
(** @return the content of a file in a buffer *)

val file_contents_fmt : string -> Format.formatter -> unit
(** Put the content of a file in a formatter. *)

val open_temp_file :
  ?debug:bool ->
  string -> (string -> out_channel -> 'a) -> 'a
(** [open_temp_file debug suffix usefile]
    creates a temporary file with suffix [suffix],
    and call [usefile] on this file (filename and open_out).
    [usefile] can close the file.
    If [debug] is [true] (default is [false]), don't remove the file. *)

val copy_file : string -> string -> unit
(** [copy_file from to] copy the file from [from] to [to]. *)

val copy_dir : string -> string -> unit
(** [copy_dir from to] copy the directory recursively from [from] to [to],
    currently the directory must contains only directories and common files. *)


val path_of_file : string -> string list
(** @return the absolute path of the given filename. *)

val relativize_filename : string -> string -> string
(** [relativize_filename base filename] relativizes the filename
    [filename] according to [base]. *)

val absolutize_filename : string -> string -> string
(** [absolutize_filename base filename] absolutizes the filename
    [filename] according to [base]. *)

val uniquify : string -> string
(** Find filename that doesn't exists based on the given filename.  Be careful
    the file can be taken after the return of this function. *)

(*
  Local Variables:
  compile-command: "make -C .."
  End:
*)

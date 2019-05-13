(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Logging Services for Frama-C Kernel and Plugins.
    @since Beryllium-20090601-beta1 *)

open Format

type kind = Result | Feedback | Debug | Warning | Error | Failure
(** @since Beryllium-20090601-beta1 *)

type event = {
  evt_kind : kind ;
  evt_plugin : string ;
  evt_category : string option ; (** message or warning category *)
  evt_source : Filepath.position option ;
  evt_message : string ;
}
(** @since Beryllium-20090601-beta1 *)

type 'a pretty_printer =
  ?current:bool -> ?source:Filepath.position ->
  ?emitwith:(event -> unit) -> ?echo:bool -> ?once:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,formatter,unit) format -> 'a
(**
    Generic type for the various logging channels which are not aborting
    Frama-C.
    - When [current] is [false] (default for most of the channels),
   no location is output. When it is [true], the last registered location
   is used as current (see {!Cil_const.CurrentLoc}).
   - [source] is the location to be output. If nil, [current] is used to
   determine if a location should be output
   - [emitwith] function which is called each time an event is processed
   - [echo] is [true] if the event should be output somewhere in addition
   to [stdout]
   - [append] adds some actions performed on the formatter after the event
   has been processed.
   @since Beryllium-20090601-beta1 *)

type ('a,'b) pretty_aborter =
  ?current:bool -> ?source:Filepath.position -> ?echo:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,formatter,unit,'b) format4 -> 'a
(** @since Beryllium-20090601-beta1
    Same as {!Log.pretty_printer} except that channels having this type
    denote a fatal error aborting Frama-C.
*)

(* -------------------------------------------------------------------------- *)
(** {2 Exception Registry}
    @plugin development guide
    @since Beryllium-20090601-beta1 *)
(* -------------------------------------------------------------------------- *)

exception AbortError of string
(** User error that prevents a plugin to terminate. Argument is the name
    of the plugin.
    @since Beryllium-20090601-beta1 *)

exception AbortFatal of string
(** Internal error that prevents a plugin to terminate. Argument is the
    name of the plugin.
    @since Beryllium-20090601-beta1 *)

exception FeatureRequest of string * string
(** Raised by [not_yet_implemented].
    You may catch [FeatureRequest(p,r)] to support degenerated behavior.
    The responsible plugin is 'p' and the feature request is 'r'. *)

(* -------------------------------------------------------------------------- *)
(** {2 Option_signature.Interface}
    @since Beryllium-20090601-beta1 *)
(* -------------------------------------------------------------------------- *)

type ontty = [
  | `Message   (** Normal message (default) *)
  | `Feedback  (** Temporary visible on console, normal message otherwise *)
  | `Transient (** Temporary visible, only on console *)
  | `Silent    (** Not visible on console *)
]

(** status of a warning category
    @since Chlorine-20180501
*)
type warn_status =
  | Winactive (** nothing is emitted. *)
  | Wfeedback_once (** combines feedback and once. *)
  | Wfeedback (** emit a feedback message. *)
  | Wonce (** emit a warning message, but only the first time the category
              is encountered. *)
  | Wactive (** emit a warning message. *)
  | Werror_once (** combines once and error. *)
  | Werror
  (** emit a message. Execution continues, but exit status will not be 0 *)
  | Wabort (** emit a message and abort execution *)

(** @since Beryllium-20090601-beta1
    @plugin development guide *)
module type Messages = sig

  type category
  (** category for debugging/verbose messages. Must be registered before
      any use. 
      Each column in the string defines a sub-category, e.g.
      a:b:c defines a subcategory c of b, which is itself a subcategory of a.
      Enabling a category (via -plugin-msg-category) will enable all its
      subcategories.
      @since Fluorine-20130401
      @modify Chlorine-20180501 categories are an abstract type of each plug-in
  *)

  type warn_category
  (** Same as above, but for warnings
      @since Chlorine-20180501
  *)

  val verbose_atleast : int -> bool
  (** @since Beryllium-20090601-beta1 *)

  val debug_atleast : int -> bool
  (** @since Beryllium-20090601-beta1 *)

  val printf : ?level:int -> ?dkey:category -> 
    ?current:bool -> ?source:Filepath.position ->
    ?append:(Format.formatter -> unit) ->
    ?header:(Format.formatter -> unit) ->
    ('a,formatter,unit) format -> 'a
  (** Outputs the formatted message on [stdout]. Levels and
      key-categories are taken into account like event messages.
      The header formatted message is emitted as a regular [result]
      message. *)

  val result : ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Results of analysis. Default level is 1.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

  val feedback : ?ontty:ontty -> ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Progress and feedback. Level is tested against the verbosity level.
      @since Beryllium-20090601-beta1
      @modify Fluorine-20130401 Optional parameter [?dkey]
      @modify Magnesium-20151001 Optional parameter [?ontty]
      @plugin development guide *)

  val debug   : ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Debugging information dedicated to Plugin developers.
      Default level is 1. The debugging key is used in message headers.
      See also [set_debug_keys] and [set_debug_keyset].
      @since Beryllium-20090601-beta1
      @modify Nitrogen-20111001 Optional parameter [dkey]
      @plugin development guide *)

  val warning : ?wkey:warn_category -> 'a pretty_printer
  (** Hypothesis and restrictions.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

  val error   : 'a pretty_printer
  (** user error: syntax/typing error, bad expected input, etc.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

  val abort   : ('a,'b) pretty_aborter
  (** user error stopping the plugin.
      @raise AbortError with the channel name.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

  val failure : 'a pretty_printer
  (** internal error of the plug-in.
      @plugin development guide *)

  val fatal   : ('a,'b) pretty_aborter
  (** internal error of the plug-in.
      @raise AbortFatal with the channel name.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

  val verify : bool -> ('a,bool) pretty_aborter
  (** If the first argument is [true], return [true] and do nothing else,
      otherwise, send the message on the {i fatal} channel and return
      [false].

      The intended usage is: [assert (verify e "Bla...") ;].
      @since Beryllium-20090601-beta1 
      @plugin development guide *)

  val not_yet_implemented : ('a,formatter,unit,'b) format4 -> 'a
  (** raises [FeatureRequest] but {i does not} send any message.
      If the exception is not caught, Frama-C displays a feature-request
      message to the user.
      @since Beryllium-20090901 *)

  val deprecated: string -> now:string -> ('a -> 'b) -> ('a -> 'b)
  (** [deprecated s ~now f] indicates that the use of [f] of name [s] is now
      deprecated. It should be replaced by [now].
      @return the given function itself
      @since Lithium-20081201 in Extlib
      @since Beryllium-20090902 *)

  val with_result  : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_result f fmt] calls [f] in the same condition as [logwith].
      @since Beryllium-20090601-beta1
      @modified 18.0-Argon the argument of the continuation is optionnal *)

  val with_warning : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_warning f fmt] calls [f] in the same condition as [logwith].
      @since Beryllium-20090601-beta1
      @modified 18.0-Argon the argument of the continuation is optionnal *)

  val with_error   : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_error f fmt] calls [f] in the same condition as [logwith].
      @since Beryllium-20090601-beta1
      @modified 18.0-Argon the argument of the continuation is optionnal *)

  val with_failure : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_failure f fmt] calls [f] in the same condition as [logwith].
      @since Beryllium-20090601-beta1
      @modified 18.0-Argon the argument of the continuation is optionnal *)

  val log : ?kind:kind -> ?verbose:int -> ?debug:int -> 'a pretty_printer
  (** Generic log routine. The default kind is [Result]. Use cases (with
      [n,m > 0]):
      - [log ~verbose:n]: emit the message only when verbosity level is
      at least [n].
      - [log ~debug:n]: emit the message only when debugging level is
      at least [n].
      - [log ~verbose:n ~debug:m]: any debugging or verbosity level is
      sufficient.
      @since Beryllium-20090901
      @plugin development guide *)

  val logwith : (event option -> 'b) ->
    ?wkey:warn_category -> ?emitwith:(event -> unit) -> ?once:bool ->
    ('a,'b) pretty_aborter
  (** Recommanded generic log routine using [warn_category] instead of [kind].
      [logwith continuation ?wkey fmt] similar to [warning ?wkey fmt]
      and then calling the [continuation].
      The optional continuation argument refers to the corresponding event.
      [None] is used iff no message is logged.
      In case the [wkey] is considered as a [Failure], the continution is not called.
      This kind of message denotes a fatal error aborting Frama-C.
      Notice that the [~emitwith] action is called iff a message is logged.
      @since 18.0-Argon *)

  val register : kind -> (event -> unit) -> unit
  (** Local registry for listeners. *)

  val register_tag_handlers : (string -> string) * (string -> string) -> unit

  (** {3 Category management} *)

  val register_category: string -> category
  (** register a new debugging/verbose category.
      Note: categories must be added (e.g. via [add_debug_keys])
      after registration.
      @since Fluorine-20130401
  *)

  val pp_category: Format.formatter -> category -> unit
  (** pretty-prints a category.
      @since Chlorine-20180501
  *)

  val dkey_name: category -> string
  (** returns the category name as a string.
      @since 18.0-Argon
  *)

  val is_registered_category: string -> bool
  (** true iff the string corresponds to a registered category
      @since Chlorine-20180501
  *)

  val get_category: string -> category option
  (** returns the corresponding registered category or [None] if no
      such category exists.
      @since Fluorine-20130401
      @modify Chlorine-20180501 return an option
  *)

  val get_all_categories: unit -> category list
  (** returns all registered categories. *)

  val add_debug_keys : category -> unit
  (** adds categories corresponding to string (including potential
      subcategories) to the set of categories for which messages are
      to be displayed. The string must have been registered beforehand.
      @since Fluorine-20130401 use categories instead of plain string
      @modify Chlorine-20180501 accepts a string as argument. Takes care
                          of propagating to subcategories.
  *)

  val del_debug_keys: category -> unit
  (** removes the given categories from the set for which messages are printed.
      The string must have been registered beforehand.
      @since Fluorine-20130401
      @modify Chlorine-20180501 accepts a string category as argument, takes care
                          of propagating to subcategories
  *)

  val get_debug_keys: unit -> category list
  (** Returns currently active keys
      @since Fluorine-20130401
      @modify Chlorine-20180501 returns a list instead of a set
  *)

  val is_debug_key_enabled: category -> bool
  (** Returns [true] if the given category is currently active
      @since Fluorine-20130401
  *)

  val get_debug_keyset : unit -> category list
  (** Returns currently active keys
      @since Nitrogen-20111001
      @deprecated Fluorine-20130401 use get_debug_keys instead
  *)

  val register_warn_category: string -> warn_category

  val is_warn_category: string -> bool

  val pp_warn_category: Format.formatter -> warn_category -> unit

  val pp_all_warn_categories_status: unit -> unit

  val wkey_name: warn_category -> string
  (** returns the warning category name as a string.
      @since 18.0-Argon
  *)

  val get_warn_category: string -> warn_category option

  val get_all_warn_categories: unit -> warn_category list

  val get_all_warn_categories_status: unit -> (warn_category * warn_status) list

  val set_warn_status: warn_category -> warn_status -> unit

  val get_warn_status: warn_category -> warn_status

end

(** Split an event category into its constituants.
    @since 18.0-Argon *)
val evt_category : event -> string list

(** Split a category specification into its constituants.
    ["*"] is considered as empty, and [""] categories are skipped.
    @since 18.0-Argon *)
val split_category : string -> string list

(** Sub-category checks.
    [is_subcategory a b] checks whether [a] is a sub-category of [b].
    Indeed, it checks whether [b] is a prefix of [a], that is,
    that [a] equals [b] or refines [b] with (a list of) sub-category(ies).
    @since 18.0-Argon *)
val is_subcategory : string list -> string list -> bool

(** Each plugin has its own channel to output messages.
    This functor should not be directly applied by plug-in developer.
    They should apply {!Plugin.Register} instead.
    @since Beryllium-20090601-beta1 *)
module Register
    (P : sig
       val channel : string
       val label : string
       val verbose_atleast : int -> bool
       val debug_atleast : int -> bool
     end)
  : Messages

(* -------------------------------------------------------------------------- *)
(** {2 Echo and Notification} *)
(* -------------------------------------------------------------------------- *)

val set_echo : ?plugin:string -> ?kind:kind list -> bool -> unit
(** Turns echo on or off. Applies to all channel unless specified,
    and all kind of messages unless specified.
    @since Beryllium-20090601-beta1 
    @plugin development guide *)

val add_listener : ?plugin:string -> ?kind:kind list -> (event -> unit) -> unit
(** Register a hook that is called each time an event is
    emitted. Applies to all channel unless specified,
    and all kind of messages unless specified.
    @since Beryllium-20090601-beta1 
    @plugin development guide *)

val echo : event -> unit
(** Display an event of the terminal, unless echo has been turned off.
    @since Beryllium-20090601-beta1 *)

val notify : event -> unit
(** Send an event over the associated listeners.
    @since Beryllium-20090601-beta1 *)

(* -------------------------------------------------------------------------- *)
(** {2 Channel interface}
    This is the {i low-level} interface to logging services.
    Not to be used by casual users.
*)
(* -------------------------------------------------------------------------- *)

type channel
(** @since Beryllium-20090601-beta1 *)

val new_channel : string -> channel
(** @since Beryllium-20090901 
    @plugin development guide *)

val log_channel : channel ->
  ?kind:kind -> 'a pretty_printer
(** logging function to user-created channel.
    @since Beryllium-20090901
    @modify Chlorine-20180501 removed ~prefix
    @plugin development guide *)

val kernel_channel_name: string
(** the reserved channel name used by the Frama-C kernel.
    @since Beryllium-20090601-beta1 *)

val kernel_label_name: string
(** the reserved label name used by the Frama-C kernel.
    @since Beryllium-20090601-beta1 *)

val source : file:Filepath.Normalized.t -> line:int -> Filepath.position
(** @since Chlorine-20180501
    @modify 18.0-Argon change type of [file]
 *)

val get_current_source : unit -> Filepath.position

(* -------------------------------------------------------------------------- *)
(** {2 Terminal interface}
    This is the {i low-level} interface to logging services.
    Not to be used by casual users. *)
(* -------------------------------------------------------------------------- *)

val clean : unit -> unit
(** Flushes the last transient message if necessary. *)

val null : formatter
[@@ deprecated "Use 'Pretty_utils.null' instead"]
(** Prints nothing.
    @since Beryllium-20090901 
    @deprecated Chlorine-20180501 use {!Pretty_utils} instead. *)

val nullprintf :  ('a,formatter,unit) format -> 'a
[@@ deprecated "Use 'Pretty_utils.nullprintf' instead"]
(** Discards the message and returns unit.
    @since Beryllium-20090901
    @deprecated Chlorine-20180501 use {!Pretty_utils} instead. *)

val with_null : (unit -> 'b) -> ('a,formatter,unit,'b) format4 -> 'a
[@@ deprecated "Use 'Pretty_utils.with_null' instead"]
(** Discards the message and call the continuation.
    @since Beryllium-20090901
    @deprecated Chlorine-20180501 use {!Pretty_utils} instead. *)

val set_output : ?isatty:bool -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
(** This function has the same parameters as Format.make_formatter.
    @since Beryllium-20090901 
    @plugin development guide *)

val print_on_output : (Format.formatter -> unit) -> unit
(** Direct printing on output.
    Message echo is delayed until the output is finished.
    Then, the output is flushed and all pending message are echoed.
    Notification of listeners is not delayed, however.

    Can not be recursively invoked.
    @since Beryllium-20090901 
    @modify Nitrogen-20111001 signature changed 
    @plugin development guide *)

val print_delayed : (Format.formatter -> unit) -> unit
(** Direct printing on output.  Same as [print_on_output], except
    that message echo is not delayed until text material is actually
    written. This gives an chance for formatters to emit messages
    before actual pretty printing.

    Can not be recursively invoked.
    @since Beryllium-20090901
    @modify Nitrogen-20111001 signature changed 
    @plugin development guide *)

(**/**)
val set_current_source : (unit -> Filepath.position) -> unit
(* Forward reference to the function returning the current location,
    used when [~current:true] is set on printers. Currently set
    in {Cil}. Not for the casual user. *)

val check_not_yet: (event -> bool) ref
(* Checks whether a message been emitted already, in which case it is
   not reprinted. Currently set in {Messages}. Not for the casual user.
*)

val tty : (unit -> bool) ref
(* Callback for command-line option '-(no)-tty' *)

val cmdline_error_occurred: (exn -> unit) ref

val cmdline_at_error_exit: ((exn -> unit) -> unit) ref

val treat_deferred_error: unit -> unit
(* call this function when it is a good time to raise an exception following
   a delayed error or failure. Currently done:
   - after each command-line stage.
   - after each analysis step (as separated by -then and its derivatives),
   including the last one.
*)

(**/**)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(** Kernel User Interface. 
    @plugin development guide *)

module Varinfo : sig
(** Varinfo to identify C functions and C variables. *)

  type t (** Abstract data type for varinfos which identify C functions and C variables.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.Glob.html}Glob.get_symb},
             {{:../html/Kui.Func.html}Func.get_symb},
             {{:../html/Kui.Varinfos.html}Varinfos.find_elt} and
             {{:../html/Kui.Varinfos.html}Varinfos.find_elt_from_name}.
             The {{:../html/Kui.Varinfos.html}Varinfos} module provides some iteration functions.

             This type [t] is used for the definition of {{:../html/Kui.Base.html}Base.t}.
         *)

  exception Invalid of t

  (** {b Test/comparaison functions:} *)

  val compare : t -> t -> int
    (** A total ordering function similar to the generic structural comparison function [compare]. *)
  val is_glob_var : t -> bool
    (** Is the varinfo [s1] related to a C global variable? *)

  val is_func : t -> bool
    (** Is the varinfo [s1] related to a C function? *)

  val is_void_func : t -> bool
    (** Is the varinfo [s1] related to a C function with a void return type? *)

  (** {b Acces functions:} *)

  val get_name : t -> string
    (** Name of the varinfo [s1]. *)

 end

module Varinfos : sig
(** Set of C varinfos. *)

  type t (** Abstract data type for sets of {{:../html/Kui.Varinfo.html}C varinfos}.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.Func.html}Func.get_params} and
             {{:../html/Kui.Func.html}Func.get_locals}.
         *)

  type elt = Varinfo.t
      (** Type of the elements. *)

  (** {b Iteration functions:} *)

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t

  (** {b Other functions:} *)

  val find_elt : (elt -> bool) -> t -> elt
    (** Raise [Not_found] exception when no element is founded. *)
  val find_elt_from_name : string -> t -> elt
    (** Shortcut using [find_elt] and [get_name] on [elt].
        Raise [Not_found] exception when no element is founded. *)
end

module Glob : sig
(** Global variable. *)

  type t (** Abstract data type for C global variables.

             There is no way for the user to build such data from crash.
             The acces function
             {{:../html/Kui.Appl.html}Appl.find_glob_from_name} returns [t] values.
             The {{:../html/Kui.Globs.html}Globs} module provides iteration functions.
         *)

  exception Uninitialized of t

  (** {b Test/comparaison functions:} *)

  val compare : t -> t ->int
    (** A total ordering function similar to the generic structural comparison function [compare]. *)

  val has_init : t -> bool
    (** Has the variable [v1] an initialization? *)

  (** {b Acces functions:} *)

  val get_symb : t -> Varinfo.t
    (** C varinfo related to a variable [v1] . *)
  val get_name : t -> string
    (** Shortcut to get the varinfo name related to [v1]. *)
  val get_file_def : t -> string
    (** C module where the variable [v1] is initialized.
        Raise [Uninitialized(v1)] exception for variable without explicit initialisation. *)
end

module Globs : sig
(** Set of global variables. *)

  type t (** Abstract data type for sets of {{:../html/Kui.Glob.html}C global variables}.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.Appl.html}Appl.get_globs} and
             {{:../html/Kui.File.html}File.get_globs}.
         *)

  type elt = Glob.t
      (** Type of the elements. *)

  (** {b Iteration functions:} *)

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t

  (** {b Other functions:} *)

  val find_elt : (elt -> bool) -> t -> elt
    (** Raise [Not_found] exception when no element is founded. *)
  val find_elt_from_name : string -> t -> elt
    (** Shortcut using [find_elt] and [get_name] on [elt].
        Raise [Not_found] exception when no element is founded. *)
end

module Base : sig
(** Base memory to identify a memory zone. *)

  type cell_class_attributes (** Abstract data type to support recursive C data structures. *)

  (** Data type to identify a {{:../html/Kui.Zone.html}memory zone}.
      The separation of the base memory will serve as the basis of the static analysis:
      There is no overlapping between two memory zones identified by different bases.

      The [private] definition of [t] hides the constructor.
      There is no way for the user to build such data from crash.
      The acces function
      {{:../html/Kui.Zone.html}Zone.get_base} returns [t] values.
      There is an iteration function: {{:../html/Kui.Zones.html}Zones.fold_base}.
  *)
  type t = Base.t

  (** {b Values:} *)

  val null : t
    (** Base memory for memory zones relative to absolue addresses like [0x123].
        Can be used as argument of [is_equal]. *)

  (** {b Test/comparaison functions:} *)

  val compare : t -> t -> int
    (** A total ordering function similar to the generic structural comparison function [compare]. *)
  val is_null : t -> bool
    (** Is [b1] based on an absolute address (like [0x123])?
        Shortcut for [compare null b1].*)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end


module Location : sig
  (** Location relative to C lvalue. *)
  type t (** Abstract data type for locations.

             There is no way for the user to build such data from scratch. *)

  (** {b Constructors:} *)

  val from_symb : Varinfo.t -> t
    (** Make location related to the symbol [s1]. *)

  val from_base : Base.t -> t
    (** Make location related to the base [b1]. *)

end

module Zone : sig
(** Zone relative to a [Base]. *)

  type t (** Abstract data type for zone of the memory relative to a {{:../html/Kui.Base.html}Base memory}.

             The separation of the base memory will serve as the basis of the static analysis:
             There is no overlapping between two memory zones identified by different bases.

             There is no way for the user to build such data from crash.
             In addition to the acces function {{:../html/Kui.From.html}From.get_output_zone},.
             the {{:../html/Kui.Zones.html}Froms} module provides an iteration function
             {{:../html/Kui.Zones.html}Zones.fold}.
         *)
  (** {b Test/comparaison functions:} *)

  val is_equal : t -> t -> bool
    (** Is it the same element? *)
  val is_included : t -> t -> bool
    (** Is [z1] included into [z2]? *)
  val intersects : t -> t -> bool
    (** Is there an intersection between [z1] and [z2]? *)

  (** {b Acces functions:} *)

  val get_base : t -> Base.t
    (** Get the zone identifier related to [z1]. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end

module Zones : sig
(** Set of memory zones. *)

  type t (** Abstract data type for sets of {{:../html/Kui.Zone.html}memory zones} (possibly empty).

             It is possible to iter on each memory zone ([fold] and [fold_base] functions) which
             compose [t] value (one iteration by base memory).

             That data type can also be used to represent approximate memory zones whose base memory
             are unknown.

             The acces functions
             {{:../html/Kui.Func.html}Func.get_external_inputs},
             {{:../html/Kui.Func.html}Func.get_functional_inputs},
             {{:../html/Kui.Func.html}Func.get_internal_inputs},
             {{:../html/Kui.Func.html}Func.get_internal_outputs},
             {{:../html/Kui.Func.html}Func.get_external_outputs},
             {{:../html/Kui.Func.html}Froms.get_input_zones} and
             {{:../html/Kui.From.html}From.get_input_zones} return [t] values.
         *)(* TODO
             {{:../html/Kui.Func.html}Func.get_functional_outputs},
            *)

  type elt = Zone.t

  exception Base_Error of t

  (** {b Values:} *)

  val empty : t
    (** Can be used as argument of [is_equal]. *)

  (** {b Constructors:} *)

  val from_zone : elt -> t
    (** Make zones containing only the zone [z1]. *)

  val from_locations : Location.t -> t
    (** Make zones from locations [l1]. *)

  (** {b Test/comparaison functions:} *)

  val is_equal : t -> t -> bool
    (** Is it the same element? *)
  val is_included : t -> t -> bool
    (** Is [s1] included into [s2]? *)
  val intersects : t -> t -> bool
    (** Is there an intersection between [s1] and [s2]? *)

  (** {b Iterations:} *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** Raise [Base_Error] exception on zones not related to [Base.t] values. *)

  val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Shortcut for [fold] and [Zone.get_base] on each element of the iteration.
        Raise [Base_Error] exception on zones not related to [Base.t] values. *)

  (** {b Other functions:} *)

  val union : Zone.t -> Zone.t -> t
    (** Union. *)

  val join : t -> t -> t
    (** Over-approximation of union. If both zones are enumerable by Base, it returns the exact union. *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end

module From : sig
(** Dependencies related to an output of a function. *)

  type t (** Abstract data type for dependencies related to an output of a function.

             There is no way for the user to build such data from crash.
             The {{:../html/Kui.Froms.html}Froms} module provides some iteration functions.
         *)

  (** {b Acces functions:} *)

  val get_output_zone : t -> Zone.t
    (** Over-approximation of the modified zone. *)
  val get_input_zones : t -> Zones.t
    (** Over-approximation input zones whose values are used to derive the new value of the modified zone. *)
  val may_be_unmodified : t -> bool
    (** The function returns [false] when the value of the modified zone is only derived from the value of the input zones.
        It returns [true] when the value of the modified zone is derived from the value of the input zones, or
        keep its initial value. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end

module Froms : sig
(** Set of dependencies related to a function. *)

  type t (** Abstract data type for dependencies related to a function.

             There is no way for the user to build such data from crash.
             The acces function
             {{:../html/Kui.Func.html}Func.get_froms} returns [t] values.
         *)

  type elt = From.t
      (** Type of the elements. *)

  exception Base_Error of t

  (** {b Acces functions:} *)

  val get_input_zones : t-> Zones.t -> Zones.t
    (** Over-approximation input zones whose values are used to derive the new value of the zone [z1]. *)

  (** {b Iterations:} *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** Iter on each dependency by output. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end

module Func : sig
(** Function. *)

  type t (** Abstract data type for C functions.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.Stmt.html}Stmt.get_func},
             {{:../html/Kui.Appl.html}Appl.find_func_from_name}.
             The {{:../html/Kui.Funcs.html}Funcs} module provides some iteration functions.
         *)

  exception Never_Called of t
  exception Undefined of t
  exception Void_Func of t

  (** {b Constructors:} *)

  val find_from_func_symb : Varinfo.t -> t
    (** Find the function related to the symb [s1].
        Raise [Varinfo.Invalid(s1)] exception if the varinfo is not related to a C function. *)

  val find_from_local_var_or_param_symb : Varinfo.t -> t * bool
    (** Find the function where the local variable or formal paramater related to the varinfo [s1] is declared.
        The boolean result is [true] when the varinfo [s1] is related to a formal paramater ([false] otherwise).
        Raise [Varinfo.Invalid(s1)] exception if the varinfo is not related to a local variable or formal paramater. *)

  (** {b Test/comparaison functions:} *)

  val compare : t -> t -> int
    (** A total ordering function similar to the generic structural comparison function [compare]. *)
  val has_def : t -> bool
    (** Has the function [f1] a definition? *)
  val never_called : t -> bool
    (** Returns true when it is established the function [f1] is never called (false means the function may not be called). *)
  val never_terminates : t -> bool
    (** Returns true when it is established the function [f1] never terminates (false means the function may not terminate).
        Raise [Never_Called(f1)] exception on function never called. *)

  val is_void_func : t -> bool
    (** Is the function [f1] related to a C function with a void return type? *)

  (** {b Acces functions:} *)

  val get_symb : t -> Varinfo.t
    (** C varinfo related to function [f1]. *)
  val get_name : t -> string
    (** Shortcut to get the varinfo name related to function [f1]. *)
  val get_file_def : t -> string
    (** Name of the C module where the function [f1] is defined.
        Raise [Undefined(f1)] exception for undefined function. *)

  val get_params : t -> Varinfos.t
    (** C symbs related to the formal parameters of the function [f1]. *)
  val get_locals : t -> Varinfos.t
    (** C symbs related to the local variables of the function [f1]. *)

  val get_return_locations : t -> Location.t
    (** Get the location of the lvalue returned by [f1] *)

  (** {b Acces functions to static analysis results:} *)

  val get_internal_inputs : t -> Zones.t
    (** Over-approximation of internal inputs (zones read by the function [f1]) including the local variables of the functions (these local variables may be read before beeing written).
        Raise [Never_Called(f1)] exception on function never called. *)
  val get_external_inputs : t -> Zones.t
    (** Over-approximation of external inputs (zones read by the function [f1]).
        By the way, external inputs of a function [f1] are included into its internal inputs.
        Raise [Never_Called(f1)] exception on function never called. *)
  val get_functional_inputs : t -> Zones.t
    (** Over-approximation of the functional inputs (zones affecting the values of the zones modified by the function [f1], when it terminates).
        Zones thoses values don't affect the output values aren't considered as functional inputs.
        By the way, functional inputs of a function [f1] are included into its external inputs.
        Raise [Never_Called(f1)] exception on function never called. *)

  val get_internal_outputs : t -> Zones.t
    (** Over-approximation of internal outputs (zones written by the function [f1], when it terminates) including the local variables of the functions.
        Zones never written aren't considered as internal outputs.
        Raise [Never_Called(f1)] exception on function never called. *)
  val get_external_outputs : t -> Zones.t
    (** Over-approximation of external outputs (zones written by the function [f1], when it terminates).
        External zones never written aren't considered as external outputs.
        By the way, external outputs of a function [f1] are included into its internal outputs.
        Raise [Never_Called(f1)] exception on function never called. *)
  val get_sure_external_outputs : t -> Zones.t
    (** Under-approximation of external outputs (zones written by the function [f1], when it terminates).
        Raise [Never_Called(f1)] exception on function never called. *)

  val get_froms : t -> Froms.t
    (** Over-approximation of the functional dependencies of the function result,
        and its external outputs, from the functional inputs.
        Raise [Never_Called(f1)] exception on function never called. *)

end

module Funcs : sig
(** Set of functions. *)

  type t (** Abstract data type for set of {{:../html/Kui.Func.html}C functions}.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.File.html}File.get_funcs},
             {{:../html/Kui.Appl.html}Appl.get_funcs}.
         *)

  type elt = Func.t
      (** Type of the elements. *)

  (** {b Constructors:} *)

  val find_callers : elt -> t
    (** Get functions calling semanticaly the given function [f1].
        Raise [Never_Called(f1)] exception on function never called. *)

  (** {b Iteration functions:} *)

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t

  (** {b Other functions:} *)

  val find_elt : (elt -> bool) -> t -> elt
    (** Raise [Not_found] exception when no element is founded. *)
  val find_elt_from_name : string -> t -> elt
    (** Shortcut using [find_elt] and [get_name] on [elt].
        Raise [Not_found] exception when no element is founded. *)
end

module Value : sig
(** Value domain. *)

  type t (** Abstract data type for value domains of C expressions.

             There is no way for the user to build such data from crash.
             Some acces functions of Kui modules return [t] values:
             {{:../html/Kui.Stmt.html}Stmt.get_value_before_stmt_of_locations},
             {{:../html/Kui.Stmt.html}Stmt.get_value_after_stmt_of_locations}.
         *)

  (** {b Test/comparaison functions:} *)

  val is_equal : t -> t -> bool
    (** Is it the same element? *)

  val is_included : t -> t -> bool
   (** Is [v1] included into [v2]? *)
  val intersects : t -> t -> bool
    (** Is there an intersection between [v1] and [v2]? *)

  (** {b Others functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)

end

module Stmt : sig
(** Statement. *)

  type t (** Abstract data type for statements of an application.

             There is no way for the user to build such data from crash.
             The acces function
             {{:../html/Kui.Appl.html}Appl.find_stmt_from_num} returns [t] values.
         *)

  exception Never_Executed

  (** {b Constructors: } *)

  val find_last_from_func : Func.t -> t
    (** Get the last statement of [f1] (its only one return).
        Raise [Not_found] exception when the function has no last statement.
        Raise [Stmt.Never_Executed] exception when the statement isn't executed. *)

  val find_first_from_func : Func.t -> t
    (** Get the first statement of [f1] (just after the declaration of its locals).
        Raise [Not_found] exception when the function has no first statement.
        Raise [Stmt.Never_Executed] exception when the statement isn't executed. *)

  (** {b Acces functions: } *)

  val get_func : t -> Func.t
    (** Get the function where the statement [s1] is defined. *)

  val get_locations : string -> t -> Location.t
    (** Get an over-approximation of locations related to
        the C lvalue [s1] within the scope of statement [s2]. *)

  val get_value_before_stmt_of_locations : t -> Location.t -> Value.t
    (** Get an over-approximation of the values, just before the executions of statement [s1], of the locations [z2]. *)
  val get_value_after_stmt_of_locations : t -> Location.t -> Value.t
    (** Get an over-approximation of the values, just after the executions of statement [s1], of the locations [z2]. *)

  (** {b Other functions:} *)

  val get_called_funcs : t -> Funcs.t
    (** Over-approximation of the set of functions that may be called by this statement. *)

end

module File : sig
(** Module. *)

  type t (** Abstract data type for C modules of an application.

             There is no way for the user to build such data from crash.
             The acces function
             {{:../html/Kui.Appl.html}Appl.find_file_from_name} returns [t] values.
             The {{:../html/Kui.Files.html}Files} module provides some iteration functions.
         *)

  (** {b Test/comparaison functions:} *)

  val compare : t -> t -> int
    (** A total ordering function similar to the generic structural comparison function [compare]. *)

  (** {b Acces functions:} *)

  val get_name : t -> string
    (** Name of the C module [m1]. *)
  val get_globs : t -> Globs.t
    (** Globals syntaxicaly used/declared/defined by the C module [m1]. *)
  val get_funcs : t -> Funcs.t
    (** Functions syntaxicaly used/declared/defined by the C module [m1].
        Warning: built-in functions may be missing. *)
end

module Files : sig
(** Set of modules. *)

  type t (** Abstract data type for sets of {{:../html/Kui.File.html}C modules}.

             There is no way for the user to build such data from crash.
             The acces function
             {{:../html/Kui.Appl.html}Appl.get_files} returns [t] values.
         *)

  type elt = File.t
      (** Type of the elements. *)

  (** {b Iteration functions:} *)

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t

  (** {b Other functions:} *)

  val find_elt : (elt -> bool) -> t -> elt
    (** Raise [Not_found] exception when no element is founded. *)
  val find_elt_from_name : string -> t -> elt
    (** Shortcut using [find_elt] and [get_name] on [elt].
        Raise [Not_found] exception when no element is founded. *)
end

module Appl : sig
(** Application. *)

  type t
    (**  Abstract data type for a C application. *)

  (** {b Management functions:} *)

  val get_current : unit -> t
    (** The [current] application. *)

  val set_current : t -> unit
    (** Set the application as the [current] one. *)

  (** {b Access functions relative to the [current] application:} *)

  val get_files : unit -> Files.t
    (** Files of the [current] application.*)
  val get_globs : unit -> Globs.t
    (** Globals syntaxicaly used/declared/defined by the [current] application. *)
  val get_funcs : unit -> Funcs.t
    (** Functions syntaxicaly used/declared/defined by the [current] application.
        Warning: built-in functions may be missing. *)

  (** {b Other functions relative to the [current] application:} *)

  val find_file_from_name : string -> File.t
    (** Find a file of the [current] application from its internal name.
        Raise [Not_found] exception when the application has no file having this name. *)
  val find_glob_from_name : string -> Glob.t
    (** Find a global variable of the [current] application from its internal name.
        Raise [Not_found] exception when the application has no global variable having this name. *)
  val find_func_from_name : string -> Func.t
    (** Find a function of the [current] application from its internal name.
        Raise [Not_found] exception when the application has no function having this name. *)
  val find_stmt_from_num : int -> Stmt.t
    (** Find a statement of the [current] application from its internal number.
        Raise [Not_found] exception when the application has no statement having this number as identifier.
        Raise [Stmt.Never_Executed] exception when the statement isn't executed. *)
end

module Project : sig
  (** Slicing project. *)

  type t
    (** Abstract data type for a slicing project. *)

  (** {b Constructor:} *)

  val make : string -> t
    (** To use to start a new slicing project relative to the [current] application. *)

  (** {b Test functions:} *)

  val is_func_called : t -> Func.t -> bool
    (** Return [true] if the source function is called (even indirectly) from a slice of the project. *)

  val has_persistent_selection : t -> Func.t -> bool
    (** Return [true] if the source function is called from a slice of the project. *)

  (** {b Other functions:} *)

  val change_slicing_level : t -> Func.t -> int -> unit
    (** change the slicing level of this function
        (see the [-slicing-level] option documentation to know the meaning of the
        number). *)

  val export : t -> string -> unit
    (** Build a new [application] from all [Slice.t] of a project,
        and print it in a file if [filename] is provided,
        to [stdout] otherwise. *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)

end

module Mark : sig
  type t (** Abtract data type for statement marks of a slicing project. *)

  (** {b Constructors:} *)

    val make : data:bool -> addr:bool -> ctrl:bool -> t
    (** To construct a mark such as
         [(is_ctrl result, is_data result, isaddr result) =
                                                    (~ctrl, ~data, ~addr)],
         [(is_bottom result) = false] and
         [(is_spare result) = not (~ctrl || ~data || ~addr)]
     *)

  (** {b Test/comparaison functions:} *)

    val is_bottom : t -> bool
      (** [true] if the mark is empty : it is the only case where
          the associated element is invisible. *)

    val is_spare : t -> bool
      (** The smallest visible mark. Usually used to mark element that need to be
          visible for compilation purpose, not really for the selected
          computations *)

    val is_data : t -> bool
      (** [true] if the element is used to compute selected data.
          Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
          at the same time. *)

    val is_ctrl : t -> bool
      (** [true] if the element controls the program point of a selected data.
          Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
          at the same time. *)

    val is_addr : t -> bool
      (** [true] if the element is used to compute the address of a selected data.
          Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
          at the same time. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing the mark. *)
end

module Select : sig
  (** Slicing selections *)

  type t
      (** Abstract data type for slicing selections. *)

  (** {b Constructors:} *)
  val empty : unit -> t
    (** Empty selection set *)

  val from_stmt_ctrl : t -> spare:bool -> Stmt.t -> t
    (** Add a selection of a statement reachability. *)

  val from_stmt : t -> spare:bool -> Stmt.t -> t
    (** Add a selection of the whole statement. *)

  val from_stmt_zone : t -> Mark.t -> Zones.t -> before:bool -> Stmt.t -> t
    (** Add a selection of a zone value relative to a statement. *)

  val from_stmt_annots : t -> Mark.t -> spare:bool -> ai:bool ->
    user_assert:bool -> slicing_pragma:bool ->
        loop_inv:bool -> loop_var:bool -> Stmt.t -> t
    (** To select the annotations related to a statement. *)

  val from_func_return : t -> spare:bool -> Func.t -> t
    (** Shortcut to select the return statement of a function. *)

  val from_func_calls_to : t -> spare:bool -> Func.t -> t
    (** Shortcut to select the all function calls to a function. *)

  val from_func_calls_into : t -> spare:bool -> Func.t -> t
    (** To select every calls to the given function without the selection of
	its inputs/outputs. *)

  val from_func_zone : t -> Mark.t -> Zones.t -> Func.t -> t
    (** Add a selection of an output zone related to a function. *)

  val from_func_annots : t -> Mark.t -> spare:bool -> ai:bool ->
    user_assert:bool -> slicing_pragma:bool ->
    loop_inv:bool -> loop_var:bool -> Func.t -> t
    (** To select the annotations related to a function. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end


module Slice : sig
(** Function slice. *)

  type t
    (** Abtract data type for a function slice. *)

  (** {b Acces functions:} *)

  val get_func : t -> Func.t
    (** Get the function related to that slice. *)

  val find_called_slice : t -> Stmt.t -> t option
    (** To get the slice directly called by the statement of a slice.
        Returns None when the statement mark is bottom,
        or else the statement isn't a call
        or else the statement is a call to one or or several (via pointer) source functions. *)

  val find_called_funcs : t -> Stmt.t -> Funcs.t
    (** To get the source functions called by the statement of a slice.
        Returns an empty set when the statement mark is bottom,
        or else the statement isn't a call
        or else the statement is a call to a function slice. *)

  val find_mark_from_stmt : t -> Stmt.t -> Mark.t
    (** Get the mark value of a statement of a function slice.
        Raise [Not_found] exception when no mark is founded for this statement. *)

  val find_mark_from_local_or_param : t -> Varinfo.t -> Mark.t
    (** Get the mark value of a local variable or parameter declaration.
        Raise [Not_found] exception when no mark is founded for this variable. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty printing. *)
end

module Slices : sig
(** Set of function slices. *)

  type t (** Abstract data type for sets of {{:../html/Kui.Slice.html}function slice}.
             There is no way for the user to build such data from crash.
         *)

  type elt = Slice.t
    (** Type of the elements. *)

  (** {b Other functions:} *)

  val find_from_project_func : Project.t -> Func.t -> t
    (** Get all slices of a slicing project related to a function. *)

  val find_callers : elt -> t
    (** Get all slices having direct calls to a slice. *)

  (** {b Iteration functions:} *)

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t

end

module Request : sig
  (** Slicing resquests are part of a slicing project.
      So, user requests affect slicing project. *)

  val add_persistent_selection: Project.t -> Select.t -> unit
    (** Add a persistent selection request to all slices (already existing or created later) of a function to the project requests. *)

  val apply_all: Project.t -> unit
    (** Apply all slicing requests. *)

  (** {b Other functions:} *)

  val pretty : Format.formatter -> Project.t -> unit
    (** Pretty printing the slicing request related to a project. *)

end

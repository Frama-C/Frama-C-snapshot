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

open Cil_types
open Cil_datatype

(** Slicing API. *)
module Api:sig

  val self: State.t
  (** Internal state of the slicing tool from project viewpoints. *)

  val set_modes : ?calls:int -> ?callers:bool -> ?sliceUndef:bool ->
    ?keepAnnotations:bool -> unit -> unit
  (** Sets slicing parameters related to command line options
      [-slicing-level], [-slice-callers], [-slice-undef-functions],
      [-slicing-keep-annotations].
      @modified Sulfur-20171101 the optional argument and the related
      deprecated option [-slice-print] have been removed. *)

  (* ---------------------------------------------------------------------- *)

  (** Slicing project management. *)
  module Project : sig

    val reset_slicing : unit -> unit
    (** Function that can be used for:
        - initializing the slicing tool before starting a slicing project;
        - removing all computed slices and all internal pending requests
          of the current slicing project. *)

    (** {3 Kernel function} *)

    val is_called : kernel_function -> bool
    (** Return [true] iff the source function is called (even indirectly via
        transitivity) from a [Slice.t]. *)

    val has_persistent_selection : kernel_function -> bool
    (** Return [true] iff the source function has persistent selection *)

    val change_slicing_level : kernel_function -> int -> unit
    (** Change the slicing level of this function (see the [-slicing-level]
        option documentation to know the meaning of the number).
        @raise SlicingTypes.ExternalFunction if [kf] has no definition.
        @raise SlicingTypes.WrongSlicingLevel if [n] is not valid. *)

    (** {3 Extraction} *)

    val default_slice_names : kernel_function -> bool  -> int -> string
    (** Default function used for the optional [?f_slice_names] argument of
        [extract] function. *)

    val extract : ?f_slice_names:(kernel_function -> bool  -> int -> string) ->
                   string -> Project.t
    (** Build a new [Db.Project.t] from all [Slice.t] of a project.
        The string argument is used for naming the new project.
        Can optionally specify how to name the sliced functions
        by defining [f_slice_names].
        [f_slice_names kf src_visi num_slice] has to return the name
        of the exported functions based on the source function [kf].
        - [src_visi] tells if the source function name is used
                     (if not, it can be used for a slice)
        - [num_slice] gives the number of the slice to name.
        The entry point function is only exported once :
        it is VERY recommended to give to it its original name,
        even if it is sliced.
        @modified Sulfur-20171101 argument order and arity. *)

    (** {3 Not for casual users} *)

    val is_directly_called_internal : kernel_function -> bool
    (** Return [true] if the source function is directly (even via pointer
        function) called from a [Slice.t]. *)

    val print_dot : filename:string -> title:string -> unit
    (** May be used to for debugging... 
        Pretty print a representation of the slicing project (call graph)
        in a dot file which name is the given string. *)

    val pretty : Format.formatter -> unit
    (** May be used for debugging... Pretty print project information. *)

  end

  (* ---------------------------------------------------------------------- *)

  (** Access to slicing results. *)
  module Mark : sig

    type t
    (** Abstract data type for mark value. *)
    val dyn_t : t Type.t
    (** For dynamic type checking and journalization. *)

    val make : data:bool -> addr:bool -> ctrl:bool -> t
    (** To construct a mark such as
        [(is_ctrl result, is_data result, isaddr result) =
        (~ctrl, ~data, ~addr)],
        [(is_bottom result) = false] and
        [(is_spare result) = not (~ctrl || ~data || ~addr)]. *)

    val compare : t -> t -> int
    (** A total ordering function similar to the generic structural
        comparison function [compare].
        Can be used to build a map from [t] marks to, for example, colors for
        the GUI. *)

    val is_bottom : t -> bool
    (** [true] iff the mark is empty: it is the only case where the
        associated element is invisible. *)

    val is_spare : t -> bool
    (** Smallest visible mark. Usually used to mark element that need to be
        visible for compilation purpose, not really for the selected
        computations. *)

    val is_data : t -> bool
    (** The element is used to compute selected data.
        Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
        at the same time. *)

    val is_ctrl : t -> bool
    (** The element is used to control the program point of a selected
        data. *)

    val is_addr : t -> bool
    (** The element is used to compute the address of a selected data. *)

    val get_from_src_func : kernel_function -> t
    (** The mark [m] related to all statements of a source function [kf].
        Property : [is_bottom (get_from_func proj kf) = not (Project.is_called proj kf) ] *)

    (** {3 Not for casual users} *)

    val pretty : Format.formatter -> t -> unit
    (** May be used for debugging... Pretty mark information. *)

  end

  (* ---------------------------------------------------------------------- *)

  (** Slicing selections. *)
  module Select : sig

    type t
    (** Internal selection. *)

    val dyn_t : t Type.t
    (** For dynamic type checking and journalization. *)

    type set 
    (** Set of colored selections. *)

    val dyn_set : set Type.t
    (** For dynamic type checking and journalization. *)

    (** {3 Journalized selectors} *)

    val empty_selects : set
    (** Empty selection. *)

    val select_stmt : set -> spare:bool -> stmt -> kernel_function -> set
    (** To select a statement. *)

    val select_stmt_ctrl : set -> spare:bool -> stmt -> kernel_function -> set
    (** To select a statement reachability.
        Note: add also a transparent selection on the whole statement. *)

    val select_stmt_lval_rw :
      (set ->
       Mark.t ->
       rd:Datatype.String.Set.t ->
       wr:Datatype.String.Set.t ->
       stmt ->
       eval:stmt ->
       kernel_function -> set)
    (** To select rw accesses to lvalues (given as a string) related to a
        statement.
        Variable names used in the sets of strings [~rd] and [~wr] are relative
        to the function scope.
        The interpretation of the address of the lvalues is
        done just before the execution of the statement [~eval].
        The selection preserves the [~rd] and ~[wr] accesses contained into
        the statement [ki].
        Note: add also a transparent selection on the whole statement.
        @modify Magnesium-20151001 argument [~scope] removed. *)

    val select_stmt_lval :
      (set -> Mark.t -> Datatype.String.Set.t -> before:bool -> stmt ->
        eval:stmt -> kernel_function -> set)
    (** To select lvalues (given as string) related to a statement.
        Variable names used in the sets of strings [~rd] and [~wr] are relative
        to the function scope.
        The interpretation of the address of the lvalue is
        done just before the execution of the statement [~eval].
        The selection preserve the value of these lvalues before or
        after (c.f. boolean [~before]) the statement [ki].
        Note: add also a transparent selection on the whole statement.
        @modify Magnesium-20151001 argument [~scope] removed.  *)

    val select_stmt_annots :
      (set -> Mark.t -> spare:bool -> threat:bool -> user_assert:bool ->
        slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        stmt -> kernel_function -> set)
    (** To select the annotations related to a statement.
        Note: add also a transparent selection on the whole statement. *)

    val select_func_lval_rw :
      (set -> Mark.t -> rd:Datatype.String.Set.t -> wr:Datatype.String.Set.t ->
        eval:stmt -> kernel_function -> set)
    (** To select rw accesses to lvalues (given as a string) related to a
        function.
        Variable names used in the sets of strings [~rd] and [~wr] are relative
        to the function scope.
        The interpretation of the address of the lvalues is
        done just before the execution of the statement [~eval].
        The selection preserve the value of these lvalues into the whole
        project.
        @modify Magnesium-20151001 argument [~scope] removed. *)

    val select_func_lval :
      (set -> Mark.t -> Datatype.String.Set.t -> kernel_function -> set)
    (** To select lvalues (given as a string) related to a function.
        Variable names used in the sets of strings [lval_str] string are
        relative to the scope of the first statement of [kf].
        The interpretation of the address of the lvalues is
        done just before the execution of the first statement [kf].
        The selection preserve the value of these lvalues before
        execution of the return statement. *)

    val select_func_return : set -> spare:bool -> kernel_function -> set
    (** To select the function result (returned value). *)

    val select_func_calls_to : set -> spare:bool -> kernel_function -> set
    (** To select every calls to the given function, i.e. the call keeps
        its semantics in the slice. *)

    val select_func_calls_into : set -> spare:bool -> kernel_function -> set
    (** To select every calls to the given function without the selection of
        its inputs/outputs. *)

    val select_func_annots :
      (set -> Mark.t -> spare:bool -> threat:bool -> user_assert:bool ->
        slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        kernel_function -> set)
    (** To select the annotations related to a function. *)

    (** {3 Selectors that are not journalized} *)

    val select_func_zone :
      (set -> Mark.t -> Locations.Zone.t -> kernel_function -> set)
    (** To select an output zone related to a function. *)

    val select_stmt_zone :
      (set -> Mark.t -> Locations.Zone.t -> before:bool -> stmt ->
       kernel_function -> set)
    (** To select a zone value related to a statement.
        Note: add also a transparent selection on the whole statement. *)

    val select_stmt_term :
      (set -> Mark.t -> term -> stmt ->
        kernel_function -> set)
    (** To select a predicate value related to a statement.
        Note: add also a transparent selection on the whole statement. *)

    val select_stmt_pred :
      (set -> Mark.t -> predicate -> stmt ->
        kernel_function -> set)
    (** To select a predicate value related to a statement.
        Note: add also a transparent selection on the whole statement. *)

    val select_stmt_annot :
      (set -> Mark.t -> spare:bool -> code_annotation ->  stmt ->
       kernel_function -> set)
    (** To select the annotations related to a statement.
        Note: add also a transparent selection on the whole statement. *)

    val select_pdg_nodes :
      (set -> Mark.t  -> PdgTypes.Node.t list -> kernel_function -> set)
    (** To select nodes of the PDG
       - if [is_ctrl_mark m],
         propagate ctrl_mark on ctrl dependencies
       - if [is_addr_mark m],
         propagate addr_mark on addr dependencies
       - if [is_data_mark m],
         propagate data_mark on data dependencies
       - mark the node with a spare_mark and propagate so that
         the dependencies that were not selected yet will be marked spare. *)

    (** {3 Not for casual users and not journalized} *)

    val get_function : t -> kernel_function
    (** May be used to get the function related to an internal selection. *)

    val merge_internal : t -> t -> t
    val add_to_selects_internal : t -> set -> set
    val iter_selects_internal : (t -> unit) -> set -> unit
    val fold_selects_internal : ('a -> t -> 'a) -> 'a -> set -> 'a
 
    val select_stmt_internal : (kernel_function -> ?select:t ->
                                stmt -> Mark.t -> t)
    (** May be used to select a statement :
        - if [is_ctrl_mark m],
          propagates ctrl_mark on ctrl dependencies of the statement
        - if [is_addr_mark m],
          propagates addr_mark on addr dependencies of the statement
        - if [is_data_mark m],
          propagates data_mark on data dependencies of the statement
        - otherwise, marks the node with a spare_mark and propagate so that
          the dependencies that were not selected yet will be marked spare.
        When the statement is a call, its functional inputs/outputs are
        also selected (The call is still selected even it has no output).
        When the statement is a composed one (block, if, etc...),
        all the sub-statements are selected.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_label_internal : (kernel_function -> ?select:t ->
                                 Logic_label.t -> Mark.t -> t)
    (** May be used to select a label. *)

    val select_min_call_internal :
      (kernel_function -> ?select:t -> stmt -> Mark.t -> t)
    (** May be used to select a statement call without its
        inputs/outputs so that it doesn't select the statements computing the
        inputs of the called function as [select_stmt_internal] would do.
        @raise Invalid_argument when the [stmt] isn't a call.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_stmt_zone_internal :
      (kernel_function -> ?select:t ->
       stmt -> before:bool -> Locations.Zone.t -> Mark.t -> t)
    (** May be used to select a zone value at a program point.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_zone_at_entry_point_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t)
    (** May be used to select a zone value at the beginning of a function.
        For a defined function, it is similar to [select_stmt_zone_internal]
        with the initial statement, but it can also be used for undefined
        functions.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_zone_at_end_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t)
    (** May be used to select a zone value at the end of a function.
        For a defined function, it is similar to [select_stmt_zone_internal]
        with the return statement, but it can also be used for undefined
        functions.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_modified_output_zone_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t)
    (** May be used to select the statements that modify the
        given zone considered as in output.
        Be careful that it is NOT the same as selecting the zone at the end!
        (the 'undef' zone is not propagated...). *)

    val select_stmt_ctrl_internal : kernel_function -> ?select:t -> stmt -> t
    (** May be used to select a statement reachability :
        Only propagate a ctrl_mark on the statement control dependencies.
        @raise SlicingTypes.NoPdg when there is no PDG for the
               [kernel_function] (related to [PdgTypes.Pdg.is_top]). *)

    val select_entry_point_internal :
      (kernel_function -> ?select:t ->  Mark.t -> t)
    val select_return_internal :
      (kernel_function -> ?select:t ->  Mark.t -> t)
    val select_decl_var_internal :
      (kernel_function -> ?select:t ->  Cil_types.varinfo -> Mark.t -> t)

    val select_pdg_nodes_internal :
      (kernel_function -> ?select:t -> PdgTypes.Node.t list -> Mark.t -> t)
    (** May be used to select PDG nodes. *)

    val pretty : Format.formatter -> t -> unit
    (** May be used for debugging... Pretty mark information. *)

  end

  (* ---------------------------------------------------------------------- *)

  (** Function slice. *)
  module Slice : sig

    type t
    (** Abstract data type for function slice. *)

    val dyn_t : t Type.t
    (** For dynamic type checking and journalization. *)

    val create : kernel_function -> t
    (** Used to get an empty slice (nothing selected) related to a
        function. *)

    val remove : t -> unit
    (** Remove the slice from the project. The slice shouldn't be called. *)

    val remove_uncalled : unit -> unit
    (** Remove the uncalled slice from the project. *)

   (** {3 Getters} *)

    val get_all: kernel_function -> t list
    (** Get all slices related to a function. *)

    val get_function : t -> kernel_function
    (** To get the function related to a slice *)

    val get_callers : t -> t list
    (** Get the slices having direct calls to a slice. *)

    val get_called_slice : t -> stmt -> t option
    (** To get the slice directly called by the statement of a slice.
        Returns None when the statement mark is bottom,
        or else the statement isn't a call
        or else the statement is a call to one or several (via pointer)
        source functions. *)

    val get_called_funcs : t -> stmt -> kernel_function list
    (** To get the source functions called by the statement of a slice.
        Returns an empty list when the statement mark is bottom,
        or else the statement isn't a call
        or else the statement is a call to a function slice. *)

    val get_mark_from_stmt : t -> stmt -> Mark.t
    (** Get the mark value of a statement. *)

    val get_mark_from_label : t -> stmt -> Cil_types.label -> Mark.t
    (** Get the mark value of a label. *)

    val get_mark_from_local_var : t -> varinfo -> Mark.t
    (** Get the mark value of local variable. *)

    val get_mark_from_formal : t -> varinfo -> Mark.t
    (** Get the mark from the formal of a function. *)

    val get_user_mark_from_inputs : t -> Mark.t
    (** Get a mark that is the merged user inputs marks of the slice *)

    (** {3 Not for casual users} *)

    val get_num_id : t -> int

    val from_num_id : kernel_function -> int -> t

    val pretty : Format.formatter -> t -> unit
    (** May be used for debugging... Pretty print slice information. *)

  end

  (* ---------------------------------------------------------------------- *)

  (** Requests for slicing jobs.
      Slicing requests are part of a slicing project.
      So, user requests affect slicing project. *)
  module Request : sig

    (** {3 Applying the added requests} *)

    val apply_all: propagate_to_callers:bool -> unit
      (** Apply all slicing requests. *)

    (** {3 Adding slicing requests} *)

    val add_selection: Select.set -> unit
      (** Add a selection request to all (existing) slices
          of a function to the project requests. *)

    val add_persistent_selection: Select.set -> unit
    (** Add a persistent selection request to all slices (already existing or
        created later) of a function to the project requests. *)

    val add_persistent_cmdline : unit -> unit
    (** Add persistent selection from the command line. *)

    (** {3 Not for casual users and not journalized} *)

    val add_slice_selection_internal:Slice.t -> Select.t -> unit
    (** May be used to add a selection request for a function slice
        to the project requests. *)

    val add_selection_internal: Select.t -> unit
    (** May be used to add a selection request to the project requests.
        This selection will be applied to every slicies of the function
        (already existing or created later). *)

    val add_call_slice:caller:Slice.t -> to_call:Slice.t -> unit
    (** May be used to change every call to any [to_call] source or specialisation in order
        to call [to_call] in [caller]. *)

    val add_call_fun: caller:Slice.t -> to_call:kernel_function -> unit
    (** May be used to change every call to any [to_call] source or specialisation
        in order to call the source function [to_call] in [caller]. *)

    val add_call_min_fun: caller:Slice.t -> to_call:kernel_function -> unit
    (** May be used to change each call to [to_call] in [caller] such that, at least, it
        will be visible at the end, ie. call either the source function or
        one of [to_call] slice (depending on the [slicing_level]). *)

    val is_request_empty_internal: unit -> bool
    (** May be used to know if internal requests are pending. *)

    (* REMOVED: val is_already_selected_internal: Slice.t -> Select.t -> bool *)
    (** Return true when the requested selection is already selected into the
        slice. *)

    val apply_all_internal: unit -> unit
    (** May be used to apply all slicing requests. *)

    val apply_next_internal: unit -> unit
    (** May be used to apply the first slicing request of the project list
        and remove it from the list.
        That may modify the contents of the remaining list.
        For example, new requests may be added to the list. *)

    val merge_slices: Slice.t  -> Slice.t -> replace:bool -> Slice.t
      (** May be used to build a new slice which marks is a merge of the two given slices.
          [choose_call] requests are added to the project in order to choose
          the called functions for this new slice.
          If [replace] is true, more requests are added to call this new
          slice instead of the two original slices. When these requests will
          be applied, the user will be able to remove those two slices using
          [Db.Slicing.Slice.remove]. *)

    val copy_slice: Slice.t -> Slice.t
    (** May be used to copy the input slice. The new slice is not called, so it is the user
        responsibility to change the calls if he wants to. *)

    val split_slice: Slice.t  -> Slice.t list
    (** May be used to copy the input slice to have one slice for each call of the original
        slice and generate requests in order to call them.
        @return the newly created slices. *)

    val propagate_user_marks : unit -> unit
    (** May be used to apply pending request then propagate user marks to callers
        recursively then apply pending requests *)

    val pretty : Format.formatter -> unit
    (** May be used for debugging... Pretty print the request list. *)

  end

end

(* ---------------------------------------------------------------------- *)
(** For debugging purpose only.

    API used by the tests of slicing (see tests/slicing/libSelect.ml). *)

module PrintSlice: sig
  val print_fct_stmts:
    Format.formatter ->
    kernel_function ->
    unit
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

let empty_string = ""

let positive_debug_ref = ref 0

let dummy_deprecated = fun _ ~now:_ _ -> assert false
let deprecated_ref = ref dummy_deprecated
let deprecated_ref2 = ref dummy_deprecated
let deprecated_ref3 = ref dummy_deprecated
  (* several distinct functions since type variables cannot be generalized.
     Okay: quite hackish :( *)

let at_normal_exit f =
  !deprecated_ref
    "Plugin.at_normal_exit"
    ~now:"Cmdline.at_normal_exit"
    Cmdline.at_normal_exit
    f

let run_normal_exit_hook () =
  !deprecated_ref2
    "Plugin.run_normal_exit_hook"
    ~now:"Cmdline.run_normal_exit_hook"
    Cmdline.run_normal_exit_hook
    ()

type group = Cmdline.Group.t

let selection : (State.t * bool) list ref = ref []

let get_selection ?(is_set=true) () =
  let l =
    if is_set then
      List.map fst !selection
    else
      List.fold_left
        (fun acc (x, b) -> if b then acc else x :: acc)
        []
        !selection
  in
  State_selection.of_list l

let extend_selection is_set s = selection := (s, is_set) :: !selection

let get_selection_context ?is_set () =
  let has_dependencies s =
    State_dependency_graph.G.out_degree State_dependency_graph.graph s > 0
  in
  (* automatically select all options which have some dependencies:
     they have an impact of some analysis. *)
  let states =
    State_selection.fold
      (fun s acc -> if has_dependencies s then s :: acc else acc)
      (get_selection ?is_set ())
      []
  in
  State_selection.of_list states

(* ************************************************************************* *)
(** {2 Delayed Kernel Initialisation} *)
(* ************************************************************************* *)

let function_names = ref (fun _ -> [])
let set_function_names f = function_names := f

let no_ast_hook = fun _ -> ()
let ast_hook = ref no_ast_hook
let init_ast_hooks = ref []
let set_ast_hook f = ast_hook := f
let apply_ast_hook f =
  let ah = !ast_hook in
  if ah == no_ast_hook then init_ast_hooks := f :: !init_ast_hooks else ah f

(* ************************************************************************* *)
(** {2 Signatures} *)
(* ************************************************************************* *)

module type Parameter = sig
  type t
  val parameter: Parameter.t
  val set: t -> unit
  val add_set_hook: (t -> t -> unit) -> unit
  val add_update_hook: (t -> t -> unit) -> unit
  val get: unit -> t
  val clear: unit -> unit
  val is_default: unit -> bool
  val option_name: string
  val print_help: Format.formatter -> unit
  include State_builder.S
  val equal: t -> t -> bool
  val add_aliases: string list -> unit
  val add_alias: string list -> unit
  val is_set: unit -> bool
  val unsafe_set: t -> unit
end

module type Bool = sig
  include Parameter with type t = bool
  val on: unit -> unit
  val off: unit -> unit
end

module type WithOutput = sig
  include Bool
  val set_output_dependencies: State.t list -> unit
  val output: (unit -> unit) -> unit
end

module type Int = sig
  include Parameter with type t = int
  val incr: unit -> unit
  val set_range: min:int -> max:int -> unit
  val get_range: unit -> int * int
end

module type String = sig
  include Parameter with type t = string
  val set_possible_values: string list -> unit
  val get_possible_values: unit -> string list
end

module type String_collection = sig
  include Parameter
  val add: string -> unit
  val remove: string -> unit
  val is_empty: unit -> bool
  val get_set: ?sep:string -> unit -> string
  val iter: (string -> unit) -> unit
  val fold: (string -> 'a -> 'a) -> 'a -> 'a
  val exists: (string -> bool) -> bool
  val set_possible_values: string list -> unit
  val get_possible_values: unit -> string list
end

module type String_set = String_collection with type t = Datatype.String.Set.t
module type String_list = String_collection with type t = string list

module type String_hashtbl = sig
  include String_collection with type t = Datatype.String.Set.t
  type value
  val find: string -> value
end

(** option interface *)
module type Indexed_val = sig
  include String
  type value (** the real type for the option*)
  val add_choice: string -> value -> unit
    (** adds a new choice for the option. *)
  val get_val: unit -> value
end

module type Parameter_input = sig
  val option_name: string
  val help: string
end

module type Parameter_input_with_arg = sig
  include Parameter_input
  val arg_name: string
end

(** input signature for [IndexedVal] *)
module type Indexed_val_input = sig
  include Parameter_input_with_arg
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val ty: t Type.t
end

module type S = sig
  include Log.Messages
  val add_group: ?memo:bool -> string -> group
  module Help: Bool
  module Verbose: Int
  module Debug: Int
  module Debug_category: String_set
  module Share: sig
    exception No_dir
    val dir: ?error:bool -> unit -> string
    val file: ?error:bool -> string -> string
  end
  val help: group
  val messages: group
  val parameters: unit -> Parameter.t list
end

module type General_services = sig

  include S

  (** {2 Functors for generating a new parameter} *)

  module Bool
    (X:sig
       include Parameter_input
       val default: bool
     end) : Bool

  module Action(X: Parameter_input) : Bool
  module False(X: Parameter_input) : Bool
  module True(X: Parameter_input) : Bool

  module WithOutput(X: sig include Parameter_input
                           val output_by_default: bool end) : WithOutput

  module Int
    (X: sig val default: int include Parameter_input_with_arg end) : Int
  module Zero(X:Parameter_input_with_arg) : Int

  module String
    (X: sig include Parameter_input_with_arg val default: string end) : String
  module EmptyString(X: Parameter_input_with_arg) : String

  module StringSet(X: Parameter_input_with_arg) : String_set
  module FilledStringSet
    (X: sig include Parameter_input_with_arg
            val default: Datatype.String.Set.t end)
    : String_set
  module StringList(X: Parameter_input_with_arg) : String_list

  module IndexedVal (V:Indexed_val_input) : Indexed_val with type value = V.t

  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
      include Datatype.S
      val parse: string -> string * t
      val redefine_binding: string -> old:t -> t -> t
      val no_binding: string -> t
    end) :
    String_hashtbl with type value = V.t

end

(* ************************************************************************* *)
(** {2 Optional parameters of functors} *)
(* ************************************************************************* *)

let kernel = ref false
let kernel_ongoing = ref false

let register_kernel =
  let used = ref false in
  fun () ->
    if !used then
      invalid_arg "The Frama-C kernel should be registered only once."
    else begin
      kernel := true;
      used := true
    end

let is_kernel () = !kernel

let share_visible_ref = ref false
let is_share_visible () = share_visible_ref := true

let reset_plugin () =
  kernel := false;
  share_visible_ref := false

let cmdline_stage_ref = ref Cmdline.Configuring
let set_cmdline_stage s = cmdline_stage_ref := s

let journalize_ref = ref true
let do_not_journalize () = journalize_ref := false

let negative_option_name_ref = ref None
let set_negative_option_name s = negative_option_name_ref := Some s

let negative_option_help_ref = ref empty_string
let set_negative_option_help s = negative_option_help_ref := s

let unset_option_name_ref = ref empty_string
let set_unset_option_name s = unset_option_name_ref := s

let unset_option_help_ref = ref empty_string
let set_unset_option_help s = unset_option_help_ref := s

let must_save_ref = ref true
let do_not_save () = must_save_ref := false

let projectify_ref = ref true
let do_not_projectify () =
  projectify_ref := false;
  do_not_save ()

let empty_format = ("": (unit, Format.formatter, unit) format)
let optional_help_ref = ref empty_format
let set_optional_help fmt = optional_help_ref := fmt
let set_optional_help fmt =
  !deprecated_ref3
    "Plugin.set_optional_help"
    ~now:"<none>"
    set_optional_help
    fmt

let module_name_ref = ref empty_string
let set_module_name s = module_name_ref := s

let argument_is_function_name_ref = ref false
let argument_is_function_name () = argument_is_function_name_ref := true

let group_ref = ref Cmdline.Group.default
let set_group s = group_ref := s

let do_iterate_ref = ref None
let do_iterate () = do_iterate_ref := Some true
let do_not_iterate () = do_iterate_ref := Some false

let is_visible_ref = ref true
let is_invisible () =
  is_visible_ref := false;
  do_not_iterate ()

let reset () =
  cmdline_stage_ref := Cmdline.Configuring;
  journalize_ref := true;
  negative_option_name_ref := None;
  negative_option_help_ref := empty_string;
  unset_option_name_ref:= empty_string;
  unset_option_help_ref:= empty_string;
  optional_help_ref := empty_format;
  projectify_ref := true;
  must_save_ref := true;
  module_name_ref := empty_string;
  group_ref := Cmdline.Group.default;
  do_iterate_ref := None;
  is_visible_ref := true;
  argument_is_function_name_ref := false

(* ************************************************************************* *)
(** {2 Generic functors} *)
(* ************************************************************************* *)

let kernel_name = "kernel"

type plugin =
    { p_name: string;
      p_shortname: string;
      p_help: string;
      p_parameters: (string, Parameter.t list) Hashtbl.t }

let plugins: plugin list ref = ref []
let iter_on_plugins f =
  let cmp p1 p2 =
    (* the kernel is the smaller plug-in *)
    match p1.p_name, p2.p_name with
    | s1, s2 when s1 = kernel_name && s2 = kernel_name -> 0
    | s1, _ when s1 = kernel_name -> -1
    | _, s2 when s2 = kernel_name -> 1
    | s1, s2 -> String.compare s1 s2
  in
  List.iter f (List.sort cmp !plugins)

let get_from_name s = List.find (fun p -> p.p_name = s) !plugins ;;
let get_from_shortname s = List.find (fun p -> p.p_shortname = s) !plugins
 (* type [plugin] must be declared before [deprecated_ref4] *)
let deprecated_ref4 = ref dummy_deprecated
let get s = 
  !deprecated_ref4 "Plugin.get" ~now:"Plugin.get_from_name" get_from_name s

let iter_on_this_parameter stage =  match !do_iterate_ref, stage with
  | Some false, _
  | None, (Cmdline.Early | Cmdline.Extending | Cmdline.Extended
              | Cmdline.Exiting | Cmdline.Loading) ->
    false
  | Some true, _ | None, Cmdline.Configuring ->
    true

module Build
  (X:sig
    include Datatype.S
    val default: unit -> t
    val option_name: string
    val functor_name: string
   end) =
struct

  let is_dynamic = (*not !kernel_ongoing*)true
  let projectify = !projectify_ref
  let must_save = !must_save_ref
  let is_visible = !is_visible_ref
  let module_name = !module_name_ref
  let group = !group_ref

  module D = Datatype

  let () = match !cmdline_stage_ref with
    | Cmdline.Early | Cmdline.Extending | Cmdline.Extended
    | Cmdline.Exiting | Cmdline.Loading ->
        do_not_projectify ()
    | Cmdline.Configuring ->
        ()

  (* quite an inlining of [State_builder.Ref]; but handle [projectify_ref] *)
  module Option_state_builder
    (X:sig
      include Datatype.S
      val unique_name: string
      val pretty_name: string
      val default: unit -> t
    end) =
  struct

    type data = X.t

    let create () = ref (X.default ())
    let state = ref (create ())

    include State_builder.Register
    (struct
      include Datatype.Ref(X)
      let descr = if must_save then descr else Descr.unmarshable
     end)
    (struct
       type t = data ref
       let get () = !state
       let create = if projectify then create else (* do an alias *) get
       let clear x = if projectify then x := X.default ()
       let set x =
         if projectify then state := x (* else there is already an alias *)
       let clear_some_projects _ _ = false (* parameters cannot be projects *)
     end)
    (struct
      let name = X.pretty_name
      let unique_name = X.unique_name
      let dependencies = []
     end)

    let set v = !state := v
    let get () = !(!state)

  end

  module Internal_state =
    Option_state_builder
      (struct
        include X
        let unique_name = X.option_name
        let pretty_name =
          if X.option_name = empty_string then "Input C files"
          else X.option_name
       end)

  include Internal_state

  let self = Internal_state.self
  type t = Internal_state.data
  let () = extend_selection false self

  let is_default () = X.equal (X.default ()) (Internal_state.get ())

  module Is_set =
    Option_state_builder
      (struct
        include D.Bool
         let pretty_name = X.option_name ^ " is set"
         let unique_name = pretty_name
         let default () = false
       end)
  let () =
    State_dependency_graph.add_dependencies ~from:Is_set.self [ self ];
    extend_selection true Is_set.self

  module Set_hook = Hook.Build(struct type t = X.t * X.t end)
  let add_set_hook f = Set_hook.extend (fun (old, x) -> f old x)

  let add_update_hook f =
    add_set_hook f;
    add_hook_on_update
      (fun x ->
        let old = get () in
        let new_ = !x in
        if not (X.equal old new_) then f old new_)

  let gen_journalized name ty set =
    let name =
      if is_dynamic then
        Dynamic.Parameter.get_name X.functor_name name X.option_name
      else
        "Kernel." ^ module_name ^ "." ^ name
    in
    if !journalize_ref then
      Journal.register ~is_dyn:is_dynamic name (D.func ty D.unit) set
    else
      set

  (* like set, but do not clear the dependencies *)
  let unsafe_set =
    let set x =
      Is_set.set true;
      let old = Internal_state.get () in
      if not (X.equal x old) then begin
        Internal_state.set x;
        Set_hook.apply (old, x)
      end
    in
    gen_journalized "unsafe_set" X.ty set

  let force_set x =
    let old = Internal_state.get () in
    if projectify then begin
      (* [JS 2009/05/25] first clear the dependency and next apply the hooks
         since these hooks may set some states in the dependencies *)
      let selection =
        State_selection.diff
          (State_selection.with_dependencies self)
          (State_selection.singleton Is_set.self)
      in
      Project.clear ~selection ()
    end;
    Internal_state.set x;
    Set_hook.apply (old, x)

  let journalized_force_set = gen_journalized "set" X.ty force_set

  let set x = 
    Is_set.set true;
    if not (X.equal x (Internal_state.get ())) then journalized_force_set x

  let unguarded_clear =
    gen_journalized "clear" D.unit
      (fun () ->
         force_set (X.default ());
         Is_set.set false)

  let clear () =
    (* write this call in the journal if and only if there is something to do *)
    if Is_set.get () || not (is_default ()) then unguarded_clear ()

  let equal = X.equal

  let register_dynamic name ty1 ty2 f =
    if is_dynamic then
      let ty = D.func ty1 ty2 in
      Dynamic.register
        ~plugin:empty_string
        (Dynamic.Parameter.get_name X.functor_name name X.option_name)
        ~journalize:false
        ty
        f
    else
      f

  let get, set, clear, is_set, is_default =
    register_dynamic "get" D.unit X.ty Internal_state.get,
    register_dynamic "set" X.ty D.unit set,
    register_dynamic "clear" D.unit D.unit clear,
    register_dynamic "is_set" D.unit D.bool Is_set.get,
    register_dynamic "is_default" D.unit D.bool is_default

  let stage = !cmdline_stage_ref

  let option_name = X.option_name

end

(* ************************************************************************* *)
(** {2 The functor [Register]} *)
(* ************************************************************************* *)

module Register
  (P: sig
     val name: string (* the name is "" for the kernel *)
     val shortname: string
     val help: string
   end) =
struct

  let parameters_ref : Parameter.t list ref = ref []
  let parameters () = !parameters_ref

  let verbose_level = ref (fun () -> 1)
  let debug_level = ref (fun () -> 0)

  include Log.Register
    (struct
       let channel =
         if is_kernel () then Log.kernel_channel_name else P.shortname
       let label = if is_kernel () then Log.kernel_label_name else P.shortname
       let debug_atleast level = !debug_level () >= level
       let verbose_atleast level = !verbose_level () >= level
     end)

  let () =
    if is_kernel () then begin
      deprecated_ref := deprecated;
      deprecated_ref2 := deprecated;
      deprecated_ref3 := deprecated;
      deprecated_ref4 := deprecated;
      Cmdline.kernel_verbose_atleast_ref := verbose_atleast;
      Cmdline.kernel_debug_atleast_ref := debug_atleast
    end

  let plugin =
    let name = if is_kernel () then kernel_name else P.name in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl empty_string [];
    { p_name = name; p_shortname = P.shortname; p_help = P.help; p_parameters = tbl }

  let add_parameter group stage param =
    if iter_on_this_parameter stage then begin
      parameters_ref := param :: !parameters_ref;
      let parameter_groups = plugin.p_parameters in
      try
        let group_name = Cmdline.Group.name group in
        let parameters = Hashtbl.find plugin.p_parameters group_name in
        Hashtbl.replace parameter_groups group_name (param :: parameters)
      with Not_found ->
        assert false
    end

  let add_group ?memo name =
    let parameter_groups = plugin.p_parameters in
    let g, new_g = Cmdline.Group.add ?memo ~plugin:P.shortname name in
    if new_g then Hashtbl.add parameter_groups name [];
    g

  let () =
    (try Cmdline.add_plugin P.name ~short:P.shortname ~help:P.help
     with Invalid_argument s ->
       abort "cannot register plug-in `%s': %s" P.name s);
    kernel_ongoing := is_kernel ();
    plugins := plugin :: !plugins

  module Bool
    (X:sig
       val default: bool
       include Parameter_input
     end) =
  struct

    include Build
      (struct
        include Datatype.Bool
        include X
        let default () = default
        let functor_name = "Bool"
       end)

    let on = register_dynamic "on" D.unit D.unit (fun () -> set true)
    let off = register_dynamic "off" D.unit D.unit (fun () -> set false)

    let generic_add_option name help visible value =
      Cmdline.add_option
        name
        ~plugin:P.shortname
        ~group
        ~help
        ~visible
        ~ext_help:!optional_help_ref
        stage
        (Cmdline.Unit (fun () -> set value))

    let negative_option_name name =
      let s = !negative_option_name_ref in
      match s with
      | None ->
          (* do we match '-shortname-'? (one dash before, one after) *)
          let len = String.length P.shortname + 2  in
          if String.length name <= len || P.shortname = empty_string then
            "-no" ^ name
          else
            let bef = Str.string_before name len in
            if bef = "-" ^ P.shortname ^ "-" then
              bef ^ "no-" ^ Str.string_after name len
            else
              "-no" ^ name
      | Some s ->
          assert (s <> empty_string);
          s

    let default_message opp = Pretty_utils.sfprintf " (set by default%s)" opp

    let add_option opp name =
      let opp_msg name = "opposite option is " ^ negative_option_name name in
      let help =
        if X.default then
          if X.help = empty_string then empty_string
          else
            X.help ^
              if opp then default_message (", " ^ opp_msg name)
              else default_message ""
        else
          if opp then Pretty_utils.sfprintf "%s (%s)" X.help (opp_msg name)
          else X.help
      in
      generic_add_option name help is_visible true

    let add_negative_option name =
      let neg_name = negative_option_name name in
      let mk_help s =
        if is_visible then if X.default then s else s ^ default_message ""
        else  ""
      in
      let neg_help, neg_visible =
        match !negative_option_name_ref, !negative_option_help_ref with
        | None, "" -> (* no user-specific config: no help *) "", false
        | Some _, "" -> 
	  mk_help ("opposite of option \"" ^ name ^ "\""), is_visible
        | _, s -> assert (s <> empty_string); mk_help s, is_visible
      in
      generic_add_option neg_name neg_help neg_visible false;
      neg_name

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let parameter =
      let negative_option =
        match !negative_option_name_ref, stage with
        | Some "", _  | None, Cmdline.Exiting ->
          add_option false X.option_name;
          None
        | _ ->
          add_option true X.option_name;
          Some (add_negative_option X.option_name)
      in
      let accessor =
        Parameter.Bool
          ({ Parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           negative_option)
      in
      let p =
        Parameter.create
          ~name:Internal_state.name
          ~help:X.help
          ~accessor:accessor
          ~is_set
      in
      add_parameter !group_ref stage p;
      reset ();
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string X.option_name Parameter.ty ~journalize:false p
      else p

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group X.option_name

  end

  module False(X: Parameter_input) =
    Bool(struct include X let default = false end)

  module True(X: Parameter_input) =
    Bool(struct include X let default = true end)

  module Action(X: Parameter_input) = struct

    (* [JS 2011/09/29]
       The ugly hack seems to be required anymore neither for Value nor Wp.
       Maybe it is time to remove it? :-) *)

    (* do not save it but restore the "good" behavior when creating by copy *)

    let () = do_not_save ()
    (* [JS 2011/01/19] Not saving this kind of options is a quite bad hack with
       several drawbacks (see Frama-C commits 2011/01/19, message of JS around
       15 PM). I'm quite sure there is a better way to not display
       results too many times (e.g. by using the "isset" flag).
       That is also the origin of bug #687 *)

    include False(X)

    let () =
      Project.create_by_copy_hook
        (fun src p ->
          Project.copy
            ~selection:(State_selection.singleton Is_set.self) ~src p;
          let selection = State_selection.singleton self in
          let opt = Project.on ~selection src get () in
          if opt then Project.on ~selection p set true)

  end

  (** {3 Integer} *)

  module Int(X: sig include Parameter_input_with_arg val default: int end) =
  struct

    include Build
      (struct
        include Datatype.Int
         include X
         let default () = default
         let functor_name = "Int"
       end)

    let incr =
      let incr () = set (succ (get ())) in
      register_dynamic "incr" D.unit D.unit incr

    let add_option name =
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~ext_help:!optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.Int set)

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let range = ref (min_int, max_int)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let parameter =
      add_set_hook
        (fun _ n ->
           let min, max = !range in
           if n < min then
             abort
               "argument of %s must be at least %d." Internal_state.name min;
           if n > max then
             abort
               "argument of %s must be no more than %d."
               Internal_state.name max);
      let accessor =
        Parameter.Int
          ({ Parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_range)
      in
      let p =
        Parameter.create
          ~name:Internal_state.name
          ~help:X.help
          ~accessor
          ~is_set:is_set
      in
      add_parameter !group_ref stage p;
      add_option X.option_name;
      reset ();
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string X.option_name Parameter.ty ~journalize:false p
      else p

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group X.option_name

  end

  module Zero(X: Parameter_input_with_arg) =
    Int(struct include X let default = 0 end)

  (** {3 String} *)

  module Pervasives_string = String

  module String
    (X: sig include Parameter_input_with_arg val default: string end) =
  struct

    include Build
      (struct
        include Datatype.String
         include X
         let default () = default
         let functor_name = "String"
       end)

    let add_option name =
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~ext_help:!optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.String set)

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      if !argument_is_function_name_ref then begin
        apply_ast_hook (fun _ -> set_possible_values (!function_names ()))
      end

    let parameter =
      add_set_hook
        (fun _ s ->
          match !possible_values with
          | [] -> ()
          | v when List.mem s v -> ()
          | _ -> abort "invalid input `%s' for %s" s Internal_state.name);
      let accessor =
        Parameter.String
          ({ Parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_possible_values)
      in
      let p =
        Parameter.create
          ~name:Internal_state.name
          ~help:X.help
          ~accessor
          ~is_set
      in
      add_parameter !group_ref stage p;
      add_option X.option_name;
      reset ();
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string X.option_name Parameter.ty ~journalize:false p
      else
        p

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group X.option_name

  end

  module EmptyString(X: Parameter_input_with_arg) =
    String(struct include X let default = empty_string end)

  (** {3 String set and string list} *)

  module Build_string_set
    (S: sig
      include Datatype.S
       val empty: t
       val is_empty: t -> bool
       val add: string -> t -> t
       val remove: string -> t -> t
       val mem: string -> t -> bool
       val for_all: (string -> bool) -> t -> bool
       val fold: (string -> 'acc -> 'acc) -> t -> 'acc -> 'acc
       val iter: (string -> unit) -> t -> unit
       val exists: (string -> bool) -> t -> bool
       val functor_name: string
       val default: unit -> t
     end)
    (X:Parameter_input_with_arg) =
  struct

    include Build
      (struct
         include S
         include X
       end)

    let add =
      let add x = set (S.add x (get ())) in
      let add = gen_journalized "add" D.string add in
      register_dynamic "add" D.string D.unit add

    let remove =
      let remove x = set (S.remove x (get ())) in
      let remove = gen_journalized "remove" D.string remove in
      register_dynamic "remove" D.string D.unit remove

    let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      if !argument_is_function_name_ref then
        apply_ast_hook (fun _ -> set_possible_values (!function_names ()))

    let guarded_set_set x =
      match split_set x with
      | [] when not (S.is_empty (get ())) ->
          set S.empty
      | l ->
        List.iter
          (fun s ->
             if !possible_values != [] then
               if not (List.mem s !possible_values) then
                 abort "invalid input `%s' for %s" s Internal_state.name)
          l;
        if not (List.for_all (fun s -> S.mem s (get ())) l) ||
          not (S.for_all (fun s -> List.mem s l) (get ()))
        then
          set (List.fold_right S.add l S.empty)

    let get_set ?(sep=", ") () =
      S.fold
        (fun s acc -> if acc <> empty_string then s ^ sep ^ acc else s)
        (get ())
        empty_string

    let is_empty =
      let is_empty () = S.is_empty (get ()) in
      register_dynamic "is_empty" D.unit D.bool is_empty

    let iter =
      let iter f = S.iter f (get ()) in
      register_dynamic "iter" (D.func D.string D.unit) D.unit iter

    let fold f =
      S.fold f (get ())

    let exists =
      let exists f = S.exists f (get()) in
      register_dynamic "exists" (D.func D.string D.bool) D.bool exists

    let add_generic_option name help f =
      Cmdline.add_option
        name
        ~plugin:P.shortname
        ~group
        ~argname:X.arg_name
        ~help
        ~visible:is_visible
        ~ext_help:!optional_help_ref
        stage
        (Cmdline.String_list (List.iter f))
        
    let add_option name help = add_generic_option name help add

    let add_option_unset name help = add_generic_option name help remove

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group X.option_name

  end

  module FilledStringSet
    (X: sig
      include Parameter_input_with_arg
      val default: Datatype.String.Set.t
    end) =
  struct

      include Build_string_set
        (struct
          include Datatype.String.Set
          let functor_name = "StringSet"
          let default () = X.default
         end)
        (X)

    let parameter =
      let accessor =
        Parameter.String_set
          { Parameter.get = get_set;
            set = guarded_set_set;
            add_set_hook = add_set_hook;
            add_update_hook = add_update_hook }
      in
      let p =
        Parameter.create
          ~name:Internal_state.name
          ~help:X.help
          ~accessor:accessor
          ~is_set
      in
      add_parameter !group_ref stage p;
      add_option X.option_name X.help;
      if !unset_option_name_ref <> "" then begin
        let help =
          if !unset_option_help_ref = "" then
            "opposite of option " ^ X.option_name
          else !unset_option_help_ref
        in
        add_option_unset !unset_option_name_ref help
      end;
      reset ();
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string X.option_name Parameter.ty ~journalize:false p
      else
        p

  end

  module StringSet(X: Parameter_input_with_arg) =
    FilledStringSet
      (struct
        include X
        let default = Datatype.String.Set.empty
       end)

  module StringList(X: Parameter_input_with_arg) = struct

    include Build_string_set
      (struct
        include Datatype.List(Datatype.String)
        let empty = []
        let is_empty = equal []
        let add s l = l @ [ s ]
        let remove s l = List.filter ((<>) s) l
        let mem s = List.exists (((=) : string -> _) s)
        let for_all = List.for_all
        let fold f l acc = List.fold_left (fun acc x -> f x acc) acc l
        let iter = List.iter
        let exists = List.exists
        let functor_name = "StringList"
        let default () = []
       end)
      (X)

    let parameter =
      let accessor =
        Parameter.String_list
          { Parameter.get = get_set;
            set = guarded_set_set;
            add_set_hook = add_set_hook;
            add_update_hook = add_update_hook }
      in
      let p =
        Parameter.create
          ~name:Internal_state.name
          ~help:X.help
          ~accessor:accessor
          ~is_set
      in
      add_parameter !group_ref stage p;
      add_option X.option_name X.help;
      reset ();
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string X.option_name Parameter.ty ~journalize:false p
      else p

  end

  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
       include Datatype.S
       val parse: string -> string * t
       val redefine_binding: string -> old:t -> t -> t
       val no_binding: string -> t
     end) =
  struct

    module Initial_Datatype = Datatype
    include StringSet(X)

    module H =
      State_builder.Hashtbl
        (Initial_Datatype.String.Hashtbl)
        (V)
        (struct
          let name = X.option_name ^ " (hashtbl)"
          let size = 7
          let dependencies = [ self ]
         end)

    type value = V.t
    let self = H.self

    let parse () =
      iter
        (fun s ->
           let k, v = V.parse s in
	   let v = try
	       let old = H.find k
	       in V.redefine_binding k ~old v 
	     with Not_found -> v
	   in H.replace k v);
      H.mark_as_computed ()

    let find s =
      if not (H.is_computed ()) then parse ();
      try H.find s
      with Not_found -> V.no_binding s

  end

  (** {3 Complex values indexed by strings} *)

  module IndexedVal (V:Indexed_val_input) : Indexed_val with type value = V.t =
  struct

    type value = V.t

    let is_dynamic = not !kernel_ongoing

    let options = Hashtbl.create 13
    let add_choice k v  = Hashtbl.add options k v
    let () = add_choice V.default_key V.default_val

    let create () = ref V.default_key

    let curr_choice = ref (create ())

    module StateAux = struct
      let name = V.option_name
      let unique_name = V.option_name
      let create = create

      type t = string ref

      let get () = !curr_choice
      let set s =
        if s != get () then
          let v = !s in
          if Hashtbl.mem options v then curr_choice := s
          else abort "invalid input %s for %s" v V.option_name

      let clear tbl = tbl := V.default_key
      let dependencies = []
      let clear_some_projects _ _ = false (* a parameter cannot be a project *)
   end

    module State =
      State_builder.Register(Datatype.Ref(Datatype.String))(StateAux)(StateAux)
    include State

    let () = extend_selection false self

    type t = string

    let equal : t -> t -> _ = (=)

    let get () = !(!curr_choice)
    let get_val () = Hashtbl.find options (get())

    module Set_hook = Hook.Build(struct type t = string * string end)
    let add_set_hook f = Set_hook.extend (fun (old, x) -> f old x)

    let add_update_hook f =
      add_set_hook f;
      add_hook_on_update
        (fun x ->
          (* this hook is applied just **before** the value is set *)
          let old = get () in
          let new_ = !x in
          if old <> new_ then f old new_)

    let unguarded_set s =
      if Hashtbl.mem options s then begin
        let old = !(!curr_choice) in
        !curr_choice := s;
        Set_hook.apply (old, s)
      end else
        warning
          "identifier %s is not a valid index for parameter %s. \
Option is unchanged.\n" s V.option_name

    let set s = if s <> get () then unguarded_set s

    let clear () = !curr_choice := V.default_key

    (* [JS 2009/04/17] TODO: reimplement is_set according to its new
       specification *)
    let is_set () = (*!(!curr_choice) <> V.default_key*) assert false
    let is_default () = !(!curr_choice) = V.default_key

    let unsafe_set = set

    let stage = !cmdline_stage_ref
    let group = !group_ref

    let add_option name =
      Cmdline.add_option
        name
        ~plugin:P.shortname
        ~group
        ~argname:V.arg_name
        ~help:V.help
        ~visible:!is_visible_ref
        ~ext_help:!optional_help_ref
        stage
        (Cmdline.String unguarded_set)

    let add_aliases =
      Cmdline.add_aliases V.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let option_name = V.option_name

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group V.option_name

    let parameter =
      let accessor =
        Parameter.String
          ({ Parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           (fun () -> []))
      in
      let p =
        Parameter.create
          ~name:V.option_name
          ~help:V.help
          ~accessor
          ~is_set
      in
      if is_dynamic then
        Dynamic.register
          ~plugin:empty_string V.option_name Parameter.ty ~journalize:false p
      else p

    let () =
      add_option V.option_name;
      reset ();

  end

  let messages = add_group "Output Messages"

  (** Options that directly cause an output. *)
  module WithOutput
    (X: sig include Parameter_input val output_by_default: bool end) =
  struct

    (* Requested command-line option *)
    include False(X)

    (* Command-line option for output. *)
    let () = set_group messages
    module Output =
      Bool(struct
        let default = X.output_by_default
        let option_name = X.option_name ^ "-print"
        let help = "print results for option " ^ X.option_name
      end)

    (* Boolean that indicates whether the results have never been output
       in the current mode. As usual, change in dependencies automatically
       reset the value *)
    module ShouldOutput =
      State_builder.True_ref(struct
        let dependencies = [] (* To be filled by the user when calling the
                                 output function *)
        let name = X.option_name ^ "ShouldOutput"
      end)

    (* Output has been requested by the user. Set the "output should be
       printed" boolean to true *)
    let () = Output.add_set_hook (fun _ v -> if v then ShouldOutput.set true)

    let set_output_dependencies deps =
      State_dependency_graph.add_codependencies ~onto:ShouldOutput.self deps

    let output f =
      (* Output only if our two booleans are at true *)
      if Output.get () && ShouldOutput.get () then begin
        (* One output will occur, do not output anything next time (unless
           dependencies change, or the user requests it on the command-line) *)
        ShouldOutput.set false;
        f ();
      end
  end

  (** {3 Generic options for each plug-in} *)

  let prefix =
    if P.shortname = empty_string then "-kernel-" else "-" ^ P.shortname ^ "-"

  module Share = struct

    let is_visible = !share_visible_ref
    let () = set_cmdline_stage Cmdline.Extended
    let () = if is_visible then do_iterate () else is_invisible ()
    module SpecificShare =
      EmptyString
        (struct
          let option_name = prefix ^ "share"
          let arg_name = "dir"
          let help =
            if is_visible then "set the plug-in share directory to <dir> \
(may be used if the plug-in is not installed at the same place than Frama-C)"
            else ""
         end)

    exception No_dir

    let get_and_check_dir ?(error=true) f =
      if (try Sys.is_directory f with Sys_error _ -> false) then
        Filepath.normalize f
      else
        if error then abort "no share directory %s for plug-in %s." f P.name
        else raise No_dir

    let dir ?error () =
      let d = if is_visible then SpecificShare.get () else empty_string in
      if d = empty_string then
        if P.shortname = empty_string then
          get_and_check_dir ?error Config.datadir
        else
          get_and_check_dir ?error (Config.datadir ^ "/" ^ P.shortname)
      else
        get_and_check_dir ?error d

    let file ?error f = dir ?error () ^ "/" ^ f

  end

  let help = add_group "Getting Information"

  let () = set_group help
  let () = set_cmdline_stage Cmdline.Exiting
  let () = if is_kernel () then set_module_name "Help"
  module Help =
    False
      (struct
        let option_name = prefix ^ "help"
        let help =
          if is_kernel () then "help of the Frama-C kernel"
          else "help of plug-in " ^ P.name
       end)
  let () =
    Cmdline.run_after_exiting_stage
      (fun () ->
         if Help.get () then Cmdline.plugin_help P.shortname else Cmdline.nop);
    Help.add_aliases [ prefix ^ "h" ]

  let output_mode modname optname =
    set_group messages;
    do_not_projectify ();
    do_not_journalize ();
    do_iterate ();
    if is_kernel () then begin
      set_cmdline_stage Cmdline.Early;
      set_module_name modname;
      "-" ^ kernel_name ^ "-" ^ optname
    end else begin
      set_cmdline_stage Cmdline.Extended;
      prefix ^ optname
    end

  let verbose_optname = output_mode "Verbose" "verbose"
  module Verbose = struct
    include
      Int(struct
            let default = !verbose_level ()
            let option_name = verbose_optname
            let arg_name = "n"
            let help =
              (if is_kernel () then "level of verbosity for the Frama-C kernel"
               else "level of verbosity for plug-in " ^ P.name)
              ^ " (default to " ^ string_of_int default ^ ")"
          end)
    let get () = if is_set () then get () else Cmdline.Verbose_level.get ()
    let () =
      verbose_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      if is_kernel () then 
	match !Cmdline.Kernel_verbose_level.value_if_set with
	| None -> ()
	| Some n -> set n
  end

  let debug_optname = output_mode "Debug" "debug"
  module Debug = struct
    include
      Int(struct
            let default = !debug_level ()
            let option_name = debug_optname
            let arg_name = "n"
            let help =
              (if is_kernel () then "level of debug for the Frama-C kernel"
               else "level of debug for plug-in " ^ P.name)
              ^ " (default to " ^ string_of_int default ^ ")"
          end)
    let get () = if is_set () then get () else Cmdline.Debug_level.get ()
    let () =
      debug_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      add_set_hook
        (fun old n ->
	  (* the level of verbose is at least the level of debug *)
	  if n > Verbose.get () then Verbose.set n;
          if n = 0 then Pervasives.decr positive_debug_ref
          else if old = 0 then Pervasives.incr positive_debug_ref);
      if is_kernel () then
	match !Cmdline.Kernel_debug_level.value_if_set with
	| None -> ()
	| Some n -> set n
  end

  let debug_category_optname = output_mode "Msg_key" "msg-key"
  let () = set_unset_option_name (output_mode "Msg_key" "msg-key-unset")
  let () =
    set_unset_option_help
      "disables message display for categories <k1>,...,<kn>"
  module Debug_category = struct
    include
      StringSet(struct
        let option_name = debug_category_optname
        let arg_name="k1[,...,kn]"
        let help =
          "enables message display for categories <k1>,...,<kn>. Use "
          ^ debug_category_optname
          ^ " help to get a list of available categories, and * to enable \
              all categories"
      end)
      let () = add_set_hook
        (fun before after ->
          if not (D.String.Set.mem "help" before)
            && D.String.Set.mem "help" after then
            begin
              (* level 0 just in case user ask to display all categories
                 in an otherwise quiet run *)
              feedback ~level:0 "@[<v 2>Available message categories are:%a@]"
                (fun fmt set ->
                  Category_set.iter
                    (fun s -> 
                      let s = (s:category:>string) in
                      if s <> "" then Format.fprintf fmt "@;%s" s)
                    set)
                (get_all_categories ())
            end;
          let add_category c s = D.String.Set.add (c:category:>string) s in
          let subcategory_closure s =
            D.String.Set.fold
              (fun s acc -> Category_set.union (get_category s) acc)
              s Category_set.empty
          in
          let string_of_cat_set s =
            Category_set.fold add_category s D.String.Set.empty
          in
          let remove = D.String.Set.diff before after in
          let added = D.String.Set.diff after before in
          let added = subcategory_closure added in
          let remove = subcategory_closure remove in
          add_debug_keys added;
          del_debug_keys remove;
          (* we add the subcategories to ourselves *)
          let after = 
            D.String.Set.union after (string_of_cat_set added)
          in
          let after =
            D.String.Set.diff after (string_of_cat_set remove)
          in
          Internal_state.set after;
          (* implicitly set debugging to 1 if at least one category is
             enabled. *)
          if Debug.get () < 1 && not (D.String.Set.is_empty after) then
            Debug.set 1
        )
  end

  let () = reset_plugin ()

end (* Register *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

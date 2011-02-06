(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let positive_debug_ref = ref 0

let deprecated_ref = ref (fun _ ~now:_ _ -> assert false)
let deprecated_ref2 = ref (fun _ ~now:_ _ -> assert false)
  (* Two distinct functions since type variables cannot be generalized.
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

let selection : State.t list ref = ref []
let get_selection () = State_selection.of_list !selection
let extend_selection s = selection := s :: !selection

(* ************************************************************************* *)
(** {2 Signatures} *)
(* ************************************************************************* *)

module type Parameter = sig
  type t
  val set: t -> unit
  val add_set_hook: (t -> t -> unit) -> unit
  val get: unit -> t
  val clear: unit -> unit
  val is_default: unit -> bool
  val is_set: unit -> bool
  val option_name: string
  include State_builder.S
  val equal: t -> t -> bool
  val unsafe_set: t -> unit
  val add_aliases: string list -> unit
  val add_alias: string list -> unit
end

module type BOOL = sig
  include Parameter with type t = bool
  val on: unit -> unit
  val off: unit -> unit
end

module type INT = sig
  include Parameter with type t = int
  val incr: unit -> unit
  val set_range: min:int -> max:int -> unit
  val get_range: unit -> int * int
end

module type STRING = sig
  include Parameter with type t = string
  val set_possible_values: string list -> unit
  val get_possible_values: unit -> string list
end

module type GEN_STRING_SET = sig
  include Parameter
  val add: string -> unit
  val remove: string -> unit
  val is_empty: unit -> bool
  val get_set: ?sep:string -> unit -> string
  val iter: (string -> unit) -> unit
  val exists: (string -> bool) -> bool
end

module type STRING_SET = GEN_STRING_SET with type t = Datatype.String.Set.t
module type STRING_LIST = GEN_STRING_SET with type t = string list

module type STRING_HASHTBL = sig
  include GEN_STRING_SET with type t = Datatype.String.Set.t
  type value
  val find: string -> value
end

(** option interface *)
module type INDEXED_VAL = sig
  include STRING
  type value (** the real type for the option*)
  val add_choice: string -> value -> unit
    (** adds a new choice for the option. *)
  val get_val: unit -> value
end

module type Parameter_input = sig
  val option_name: string
  val help: string
  val kind: [> `Correctness | `Tuning | `Irrelevant ]
end

module type Parameter_input_with_arg = sig
  include Parameter_input
  val arg_name: string
end

(** input signature for [IndexedVal] *)
module type COMPLEX_VALUE = sig
  include Parameter_input_with_arg
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val ty: t Type.t
end

module type S = sig
  include Log.Messages
  val add_group: ?memo:bool -> string -> group
  module Help: BOOL
  module Verbose: INT
  module Debug: INT
  val help: group
  val messages: group
end

module type General_services = sig

  include S

  (** {2 Functors for generating a new parameter} *)

  module Bool
    (X:sig
       include Parameter_input
       val default: bool
     end) : BOOL

  module Action(X: Parameter_input) : BOOL
  module False(X: Parameter_input) : BOOL
  module True(X: Parameter_input) : BOOL

  module Int
    (X: sig val default: int include Parameter_input_with_arg end) : INT
  module Zero(X:Parameter_input_with_arg) : INT

  module String
    (X: sig include Parameter_input_with_arg val default: string end) : STRING
  module EmptyString(X: Parameter_input_with_arg) : STRING

  module StringSet(X: Parameter_input_with_arg) : STRING_SET
  module StringList(X: Parameter_input_with_arg) : STRING_LIST

  module IndexedVal (V:COMPLEX_VALUE) : INDEXED_VAL with type value = V.t

  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
      include Datatype.S
      val parse: string -> string * t
      val no_binding: string -> t
    end) :
    STRING_HASHTBL with type value = V.t

end

(* ************************************************************************* *)
(** {2 Optional parameters of the functor [Register]} *)
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
let reset_plugin () = kernel := false

(* ************************************************************************* *)
(** {2 Optional parameters of functors exported by Register} *)
(* ************************************************************************* *)

let cmdline_stage_ref = ref Cmdline.Configuring
let set_cmdline_stage s = cmdline_stage_ref := s

let journalize_ref = ref true
let do_not_journalize () = journalize_ref := false

let negative_option_name_ref = ref None
let set_negative_option_name s = negative_option_name_ref := Some s

let negative_option_help_ref = ref ""
let set_negative_option_help s = negative_option_help_ref := s

let projectify_ref = ref true
let do_not_projectify () = projectify_ref := false

let must_save_ref = ref true
let do_not_save () = must_save_ref := false

let optional_help_ref = ref ("": (unit, Format.formatter, unit) format)
let set_optional_help fmt = optional_help_ref := fmt

let module_name_ref = ref ""
let set_module_name s = module_name_ref := s

let group_ref = ref Cmdline.Group.default
let set_group s = group_ref := s

let is_visible_ref = ref None
let is_visible () = is_visible_ref := Some true
let is_invisible () = is_visible_ref := Some false

let reset () =
  cmdline_stage_ref := Cmdline.Configuring;
  journalize_ref := true;
  negative_option_name_ref := None;
  negative_option_help_ref := "";
  optional_help_ref := "";
  projectify_ref := true;
  must_save_ref := true;
  module_name_ref := "";
  group_ref := Cmdline.Group.default;
  is_visible_ref := None

(* ************************************************************************* *)
(** {2 Generic functors} *)
(* ************************************************************************* *)

let kernel_name = "kernel"

type 'a option_accessor =
    { get: unit -> 'a ; set: 'a -> unit; is_set: unit -> bool }

type kind =
  | Bool of bool option_accessor * string option
  | Int of int option_accessor * (unit -> int * int)
  | String of string option_accessor * (unit -> string list)
  | StringSet of string option_accessor

type parameter = { o_name: string; o_help: string; o_kind: kind }
type plugin =
    { p_name: string;
      p_help: string;
      p_parameters: (string, parameter list) Hashtbl.t }

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

(*let dynamic_plugin_name name = "Dynamic." ^ name
let dynamic_function_name funname statename =
  funname ^ " \"" ^ statename ^ "\""*)
let dynamic_name functor_name fct_name option_name =
  Format.sprintf "Parameters.Dynamic.%s.%s %S"
    functor_name fct_name option_name

module Build
  (X:sig
    include Datatype.S
     val default: unit -> t
     val option_name: string
     val functor_name: string
     val kind: [> `Correctness | `Tuning | `Irrelevant ]
   end) =
struct

  let is_dynamic = not !kernel_ongoing
  let projectify = !projectify_ref
  let must_save = !must_save_ref && projectify
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
      val kind: State.kind
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
       let is_default x = !x = (X.default ())
       let clear_some_projects _ _ = false (* parameters cannot be projects *)
     end)
    (struct
      let name = X.pretty_name
      let unique_name = X.unique_name
      let dependencies = []
      let kind = X.kind
     end)

    let set v = !state := v
    let get () = !(!state)
    let clear () = set (X.default ())

  end

  module Internal_state = struct

    let pretty_value v =
      if X.option_name = "" then "Input C files"
      else
        if Type.equal X.ty Datatype.bool
          || Type.equal X.ty Datatype.int
          || Type.equal X.ty Datatype.float
        then
        Pretty_utils.sfprintf "%s: %a" X.option_name X.pretty v
        else X.option_name

    include Option_state_builder
      (struct
	include X
	let unique_name = X.option_name
        let pretty_name = pretty_value (X.default ())
       end)

    let set v =
      State.set_name self (pretty_value v);
      set v

  end

  include Internal_state

  let self = Internal_state.self
  type t = Internal_state.data

  let () = extend_selection self

  let is_default () = X.equal (X.default ()) (Internal_state.get ())

  module Is_set =
    Option_state_builder
      (struct
	include D.Bool
	 let pretty_name = X.option_name ^ " is set"
	 let unique_name = pretty_name
	 let default () = false
         let kind = `Internal
       end)
  let () =
    State_dependency_graph.Static.add_dependencies ~from:Is_set.self [ self ]

  module Set_Hook = Hook.Build(struct type t = X.t * X.t end)
  let add_set_hook f = Set_Hook.extend (fun (old, x) -> f old x)

  let gen_journalized name ty set =
    let name =
      if is_dynamic then dynamic_name X.functor_name name X.option_name
      else "Parameters." ^ module_name ^ "." ^ name
    in
    if !journalize_ref then
      Journal.register ~is_dyn:is_dynamic name (D.func ty D.unit) set
    else
      set

  let unsafe_set =
    let set x =
      Is_set.set true;
      let old = Internal_state.get () in
      if not (X.equal x old) then begin
	Internal_state.set x;
	Set_Hook.apply (old, x)
      end
    in
    gen_journalized "unsafe_set" X.ty set

  let force_set x =
    let old = Internal_state.get () in
    Internal_state.set x;
    if projectify then begin
      (* [JS 2009/05/25] first clear the dependency and next apply the hooks
	 since these hooks may set some states in the dependencies *)
      let selection =
	State_selection.Dynamic.diff
	  (State_selection.Dynamic.only_dependencies self)
	  (State_selection.singleton Is_set.self)
      in
      Project.clear ~selection ()
    end;
    Set_Hook.apply (old, x)

  let unjournalized_set x =
    Is_set.set true;
    if not (X.equal x (Internal_state.get ())) then force_set x

  let unguarded_set = gen_journalized "set" X.ty unjournalized_set

  (* [TODO] not very efficient since the test of modification is done twice. *)
  let set x = if not (X.equal x (Internal_state.get ())) then unguarded_set x

  let clear =
    gen_journalized "clear" D.unit
      (fun () ->
	 force_set (X.default ());
	 Is_set.set false)

  let equal = X.equal

  let register_dynamic name ty1 ty2 f =
    if is_dynamic then
      let ty = D.func ty1 ty2 in
      Dynamic.register
	~plugin:""
	(dynamic_name X.functor_name name X.option_name)
	~journalize:false
	ty
	f
    else
      f

  let get, set, clear, is_set =
    register_dynamic "get" D.unit X.ty Internal_state.get,
    register_dynamic "set" X.ty D.unit set,
    register_dynamic "clear" D.unit D.unit clear,
    register_dynamic "is_set" D.unit D.bool Is_set.get

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

  let () = if is_kernel () then begin
      deprecated_ref := deprecated;
      deprecated_ref2 := deprecated;
      Cmdline.kernel_verbose_atleast_ref := verbose_atleast;
      Cmdline.kernel_debug_atleast_ref := debug_atleast
    end

  let plugin =
    let name = if is_kernel () then kernel_name else P.name in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl "" [];
    { p_name = name; p_help = P.help; p_parameters = tbl }

  let add_parameter group stage name help kind =
    match !is_visible_ref, stage with
    | Some false, _
    | None,
      (Cmdline.Early | Cmdline.Extending | Cmdline.Extended
      | Cmdline.Exiting | Cmdline.Loading) ->
	()
    | Some true, _ | None, Cmdline.Configuring ->
	let parameter_groups = plugin.p_parameters in
	let parameter = { o_name = name; o_help = help; o_kind = kind } in
	try
	  let group_name = Cmdline.Group.name group in
	  let parameters = Hashtbl.find plugin.p_parameters group_name in
	  Hashtbl.replace parameter_groups group_name (parameter :: parameters)
	with Not_found ->
	  assert false

  let add_group ?memo name =
    let parameter_groups = plugin.p_parameters in
    let g, new_g = Cmdline.Group.add ?memo ~plugin:P.shortname name in
    if new_g then Hashtbl.add parameter_groups name [];
    g

  let () =
    Cmdline.add_plugin P.name ~short:P.shortname ~help:P.help;
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

    let generic_add_option name help value =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group
	~help
	~ext_help:!optional_help_ref
	stage
	(Cmdline.Unit (fun () -> unguarded_set value))

    let default_message = " (set by default)"

    let add_option name =
      let help = if X.default then X.help ^ default_message else X.help in
      generic_add_option name (Some help) true

    let negative_option_name name =
      let s = !negative_option_name_ref in
      match s with
      | None ->
	  let len = String.length P.shortname + 1 (* +1: the initial '-' *) in
	  if String.length name <= len || P.shortname = "" then
	    "-no" ^ name
	  else
	    let bef = Str.string_before name len in
	    if bef = "-" ^ P.shortname then
	      bef ^ "-no" ^ Str.string_after name len
	    else
	      "-no" ^ name
      | Some s ->
	  assert (s <> "");
	  s

    let add_negative_option name =
      let neg_name = negative_option_name name in
      let mk_help s = Some (if X.default then s else s ^ default_message) in
      let neg_help =
	match !negative_option_name_ref, !negative_option_help_ref with
	| None, "" -> (* no user-specific config: no help *) None
	| Some _, "" -> mk_help ("opposite of option \"" ^ name ^ "\"")
	| _, s -> assert (s <> ""); mk_help s
      in
      generic_add_option neg_name neg_help false;
      neg_name

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let () =
      add_option X.option_name;
      let negative_option =
	match !negative_option_name_ref, stage with
	| Some "", _  | None, Cmdline.Exiting -> None
	| _ -> Some (add_negative_option X.option_name)
      in
      add_parameter
	!group_ref stage Internal_state.name X.help
	(Bool({ get = get; set = set; is_set = is_set}, negative_option));
      reset ()

  end

  module False(X: Parameter_input) =
    Bool(struct include X let default = false end)

  module True(X: Parameter_input) =
    Bool(struct include X let default = true end)

  module Action(X: Parameter_input) = struct

    (* do not save it but restore the "good" behavior when creating by copy *)

    let () = do_not_save ()
    (* [JS 2011/01/19] Not saving this kind of options is a quite bad hack with
       several drawbacks (see Frama-C commits 2011/01/19, message of JS around
       15 PM). I'm quite sure there is a better way to not display
       results too many times (e.g. by using the "isset" flag). *)

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
	~help:(Some X.help)
	~ext_help:!optional_help_ref
	~plugin:P.shortname
	~group
	stage
	(Cmdline.Int unguarded_set)

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let range = ref (min_int, max_int)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let () =
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
      add_parameter
	!group_ref stage Internal_state.name X.help
	(Int({ get = get; set = set; is_set = is_set }, get_range));
      add_option X.option_name;
      reset ()

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
	~help:(Some X.help)
	~ext_help:!optional_help_ref
	~plugin:P.shortname
	~group
	stage
	(Cmdline.String unguarded_set)

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      add_set_hook
	(fun _ s ->
	   match !possible_values with
	   | [] -> ()
	   | v when List.mem s v -> ()
	   | _ -> abort "invalid input %s for %s" s Internal_state.name);
      add_parameter
	!group_ref stage Internal_state.name X.help
	(String({ get = get; set = set; is_set = is_set },
		get_possible_values));
      add_option X.option_name;
      reset ()

  end

  module EmptyString(X: Parameter_input_with_arg) =
    String(struct include X let default = "" end)

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
     end)
    (X:Parameter_input_with_arg) =
  struct

    include Build
      (struct
	 let default () = S.empty
	 let functor_name = "StringSet"
	 include S
	 include X
       end)

    let add =
      let add x = unguarded_set (S.add x (get ())) in
      register_dynamic "add" D.string D.unit add

    let remove =
      let remove x = unguarded_set (S.remove x (get ())) in
      register_dynamic "remove" D.string D.unit remove

    let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

    let guarded_set_set x =
      match split_set x with
      | [] when not (S.is_empty (get ())) ->
	  unguarded_set S.empty
      | l ->
	  if not (List.for_all (fun s -> S.mem s (get ())) l) ||
	    not (S.for_all (fun s -> List.mem s l) (get ()))
	  then
	    unguarded_set (List.fold_right S.add l S.empty)

    let get_set ?(sep=", ") () =
      S.fold (fun s acc -> if acc <> "" then s ^ sep ^ acc else s) (get ()) ""

    let is_empty =
      let is_empty () = S.is_empty (get ()) in
      register_dynamic "is_empty" D.unit D.bool is_empty

    let iter =
      let iter f = S.iter f (get ()) in
      register_dynamic "iter" (D.func D.string D.unit) D.unit iter

    let exists =
      let exists f = S.exists f (get()) in
      register_dynamic "exists" (D.func D.string D.bool) D.bool exists

    let add_option name =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group
	~argname:X.arg_name
	~help:(Some X.help)
	~ext_help:!optional_help_ref
	stage
	(Cmdline.String_list (List.iter add))

    let add_aliases =
      Cmdline.add_aliases X.option_name ~plugin:P.shortname ~group stage

    let add_alias =
      deprecated "Plugin.add_alias" ~now:"Plugin.add_aliases" add_aliases

    let () =
      add_parameter
	!group_ref stage Internal_state.name X.help
	(StringSet{ get = (fun () -> get_set ());
		    set = guarded_set_set;
		    is_set = is_set });
      add_option X.option_name;
      reset ()

  end

  module StringSet = Build_string_set(Datatype.String.Set)

  module StringList =
    Build_string_set
      (struct
	include Datatype.List(Datatype.String)
	let empty = []
	let is_empty = equal []
	let add s l = s :: l
	let remove s l = List.filter ((<>) s) l
	let mem s = List.exists (((=) : string -> _) s)
	let for_all = List.for_all
	let fold = List.fold_right
	let iter = List.iter
	let exists = List.exists
       end)

  (** {3 Complex values indexed by strings} *)

  module IndexedVal (V:COMPLEX_VALUE):INDEXED_VAL with type value = V.t =
  struct

    type value = V.t

    let options = Hashtbl.create 13
    let add_choice k v  = Hashtbl.add options k v
    let () = add_choice V.default_key V.default_val

    let create () = ref V.default_key

    let curr_choice = ref (create ())

    module StateAux = struct
      let name = V.option_name
      let unique_name = V.option_name
      let kind = V.kind
      let create = create

      type t = string ref

      let get () = !curr_choice
      let set s =
	if s != get () then
	  if Hashtbl.mem options !s then
            curr_choice := s
	  else
	    (* [JS 2009/05/25] well, quite difficult to use functor
	       Log.Register here without using a recursive module.
	       Maybe a lighter solution could be implemented. *)
            Printf.eprintf
              "Warning: %s: identifier %s is not a valid index for this \
option. Option is unchanged.\n" V.option_name !s

      let copy s = ref !s
      let clear tbl = tbl := V.default_key
      let is_default x = !x = V.default_key
      let dependencies = []
      let clear_some_projects _ _ = false (* a parameter cannot be a project *)
   end

    module State =
      State_builder.Register(Datatype.Ref(Datatype.String))(StateAux)(StateAux)
    include State

    let () = extend_selection self

    type t = string

    let equal : t -> t -> _ = (=)

    let get () = !(!curr_choice)
    let get_val () = Hashtbl.find options (get())

    module Set_Hook = Hook.Build(struct type t = string * string end)
    let add_set_hook f = Set_Hook.extend (fun (old, x) -> f old x)

    let unguarded_set s =
      if Hashtbl.mem options s then begin
	let old = !(!curr_choice) in
	!curr_choice := s;
	Set_Hook.apply (old, s)
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
	~help:(Some V.help)
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

    let () =
      add_option V.option_name;
      reset ()

  end

  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
       include Datatype.S
       val parse: string -> string * t
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
          let kind = `Internal
	 end)

    type value = V.t
    let self = H.self

    let parse k =
      iter
	(fun s ->
	   let k, v = V.parse s in
	   H.add k v);
      try H.find k
      with Not_found ->
	let v = V.no_binding k in
	H.add k v;
	v

    let find s = H.memo parse s

  end


  (** {2 Generic options for each plug-in} *)

  let prefix = if P.shortname = "" then "-kernel-" else "-" ^ P.shortname ^ "-"

  let help = add_group "Getting Information"

  let () = set_group help
  let () = set_cmdline_stage Cmdline.Exiting
  let () = if is_kernel () then set_module_name "Help"
  module Help =
    False(struct
	    let option_name = prefix ^ "help"
	    let help =
	      if is_kernel () then "help of the Frama-C kernel"
	      else "help of plug-in " ^ P.name
            let kind = `Irrelevant
	  end)
  let () =
    Cmdline.run_after_exiting_stage
      (fun () ->
	 if Help.get () then Cmdline.plugin_help P.shortname else Cmdline.nop);
    Help.add_aliases [ prefix ^ "h" ]

  let messages = add_group "Output Messages"

  let output_mode modname optname =
    set_group messages;
    do_not_projectify ();
    do_not_journalize ();
    is_visible ();
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
	      ^ " (defaults to " ^ string_of_int default ^ ")"
            let kind = `Irrelevant
	  end)
    let get () = if is_set () then get () else !Cmdline.verbose_level_ref
    let () =
      verbose_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      if is_kernel () then set Cmdline.kernel_verbose_level
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
	      ^ " (defaults to " ^ string_of_int default ^ ")"
            let kind = `Irrelevant
	  end)
    let get () = if is_set () then get () else !Cmdline.debug_level_ref
    let () =
      debug_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      add_set_hook
	(fun old n ->
	   if n = 0 then Pervasives.decr positive_debug_ref
	   else if old = 0 then Pervasives.incr positive_debug_ref);
      if is_kernel () then set Cmdline.kernel_debug_level
  end

  let () = reset_plugin ()

end (* Register *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

module NormalExit = Hook.Make(struct end)
let at_normal_exit = NormalExit.extend
let run_normal_exit_hook = NormalExit.apply

type group = Cmdline.Group.t

let selection = ref Project.Selection.empty
let get_selection () = !selection
let add_option select =
  selection := select Kind.Do_Not_Select_Dependencies !selection

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
  include Project.Computation.OUTPUT
  val equal: t -> t -> bool
  val unsafe_set: t -> unit
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
  val set_set: string -> unit
  val get_set: ?sep:string -> unit -> string
  val add: string -> unit
  val add_set: string -> unit
  val iter: (string -> unit) -> unit
  val fold: (string -> 'a -> 'a) -> 'a -> 'a
end

module type STRING_SET = sig
  include GEN_STRING_SET with type t = Cilutil.StringSet.t
  val is_empty: unit -> bool
  val remove: string -> unit
  val remove_set: string -> unit
end

module type STRING_LIST = GEN_STRING_SET with type t = string list

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
  val descr: string
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
  val add_group: string -> group
  module Help: BOOL
  module Verbose: INT
  module Debug: INT
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
    (** Should not be used by casual users *)

  module IndexedVal (V:COMPLEX_VALUE) : INDEXED_VAL with type value = V.t

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

let negative_option_descr_ref = ref ""
let set_negative_option_descr s = negative_option_descr_ref := s

let projectify_ref = ref true
let do_not_projectify () = projectify_ref := false

let optional_descr_ref = ref ("": (unit, Format.formatter, unit) format)
let set_optional_descr fmt = optional_descr_ref := fmt

let module_name_ref = ref ""
let set_module_name s = module_name_ref := s

let group_ref = ref Cmdline.Group.default
let set_group s = group_ref := s

let reset () =
  cmdline_stage_ref := Cmdline.Configuring;
  journalize_ref := true;
  negative_option_name_ref := None;
  negative_option_descr_ref := "";
  optional_descr_ref := "";
  projectify_ref := true;
  module_name_ref := "";
  group_ref := Cmdline.Group.default

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

type parameter = { o_name: string; o_descr: string; o_kind: kind }
type plugin = 
    { p_name: string; 
      p_descr: string; 
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

let dynamic_funname ~modname ~funname ~statename =
  "Dynamic." ^ modname ^ "." ^ funname ^ " \"" ^ statename ^ "\""

module Build
  (X:sig
     type t
     val default: unit -> t
     val option_name: string
     val functor_name: string
     val ty: t Type.t
     val equal: t -> t -> bool
   end) =
struct

  let is_dynamic = not !kernel_ongoing
  let projectify = !projectify_ref
  let module_name = !module_name_ref

  module D = Datatype

  let () = match !cmdline_stage_ref with
    | Cmdline.Early | Cmdline.Extending | Cmdline.Extended 
    | Cmdline.Exiting | Cmdline.Loading -> 
	do_not_projectify ()
    | Cmdline.Configuring -> 
	()

  (* quite an inlining of [Computation.Ref]; but handle [projectify_ref] *)
  module StateBuilder
    (X:sig type t val name: string val default: unit -> t end) = 
  struct

    type data = X.t
	
    let create () = ref (X.default ())
    let state = ref (create ())

    include Project.Computation.Register
    (Datatype.Ref(Project.Datatype.Persistent(X)))
    (struct
       type t = data ref
       let get () = !state
       let create = if projectify then create else (* do an alias *) get
       let clear x = if projectify then x := X.default ()
       let set x = 
	 if projectify then state := x (* else there is already an alias *)
       let clear_if_project _ _ = false (* a parameter cannot be a project *)
     end)
    (struct let name = X.name let dependencies = [] end)

    let set v = !state := v
    let get () = !(!state)
    let clear () = !state := X.default ()

  end

  module State = StateBuilder(struct include X let name = X.option_name end)
  include State

  type t = State.data

  let () = add_option State.select

  let is_default () = X.equal (X.default ()) (State.get ())

  module Is_set =
    StateBuilder
      (struct 
	 type t = bool 
	 let name = X.option_name ^ " is set"
	 let default () = false
       end)
  let () = State.depend Is_set.self

  let do_not_save () =
    do_not_save ();
    (* We don't want to know which options was set in the saved session while
       loading. *)
    Is_set.do_not_save ();
    (* However we want to copy each state [Is_set] while creating a project by
       copy. *)
    Project.create_by_copy_hook
      (fun src p -> 
	 Project.copy
	   ~only:(Project.Selection.singleton Is_set.self
		    Kind.Do_Not_Select_Dependencies) 
	   ~src 
	   p)

  module Set_Hook = Hook.Build(struct type t = X.t * X.t end)
  let add_set_hook f = Set_Hook.extend (fun (old, x) -> f old x)

  let gen_journalized name ty set =
    let name =
      if is_dynamic then dynamic_funname X.functor_name name X.option_name
      else module_name ^ "." ^ name
    in
    if !journalize_ref then
      Journal.register
	~is_dyn:is_dynamic
	("Parameters." ^ name)
	(Type.func ty Type.unit)
	set
    else
      set

  let unsafe_set =
    let set x =
      Is_set.set true;
      let old = State.get () in
      if not (X.equal x old) then begin
	State.set x;
	Set_Hook.apply (old, x)
      end
    in
    gen_journalized "unsafe_set" X.ty set

  let force_set x =
    let old = State.get () in
    State.set x;
    if projectify then
      (* [JS 2009/05/25] first clear the dependency and next apply the hooks
	 since these hooks may set some states in the dependencies *)
      Project.clear
	~only:(Project.Selection.singleton State.self
		 Kind.Only_Select_Dependencies)
        ~except:(Project.Selection.singleton Is_set.self 
		   Kind.Do_Not_Select_Dependencies)
        ();
    Set_Hook.apply (old, x)

  let unjournalized_set x =
    Is_set.set true;
    if not (X.equal x (State.get ())) then force_set x

  let unguarded_set = gen_journalized "set" X.ty unjournalized_set

  let set x = 
    if not (X.equal x (State.get ())) then 
      (* call [set] for journalisation *) 
      unguarded_set x

  let clear =
    gen_journalized "clear" Type.unit
      (fun () ->
	 force_set (X.default ());
	 Is_set.set false)

  let equal = X.equal

  let register_dynamic name ty1 ty2 f =
    if is_dynamic then
      let ty = Type.func ty1 ty2 in
      Dynamic.register
	(dynamic_funname X.functor_name name X.option_name)
	~journalize:false
	ty
	f
    else
      f

  let get, set, clear, is_set =
    register_dynamic "get" Type.unit X.ty State.get,
    register_dynamic "set" X.ty Type.unit set,
    register_dynamic "clear" Type.unit Type.unit clear,
    register_dynamic "is_set" Type.unit Type.bool Is_set.get

  let stage = !cmdline_stage_ref

end

(* ************************************************************************* *)
(** {2 The functor [Register]} *)
(* ************************************************************************* *)

module Register
  (P: sig
     val name: string (* the name is "" for the kernel *)
     val shortname: string
     val descr: string
   end) =
struct

  let verbose_level = ref 1
  let debug_level = ref 0

  include Log.Register
    (struct
       let channel = 
	 if is_kernel () then Log.kernel_channel_name else P.shortname
       let label = 
	 if is_kernel () then Log.kernel_label_name else P.shortname
       let debug_atleast level = !debug_level >= level
       let verbose_atleast level = !verbose_level >= level
     end)

  let plugin = 
    let name = if is_kernel () then kernel_name else P.name in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl "" [];
    { p_name = name; p_descr = P.descr; p_parameters = tbl }

  let add_parameter group stage name descr kind = 
    match stage with
    | Cmdline.Early | Cmdline.Extending | Cmdline.Extended 
    | Cmdline.Exiting | Cmdline.Loading -> 
	()
    | Cmdline.Configuring ->
	let parameter_groups = plugin.p_parameters in
	let parameter = { o_name = name; o_descr = descr; o_kind = kind } in
	try 
	  let group_name = Cmdline.Group.name group in
	  let parameters = Hashtbl.find plugin.p_parameters group_name in
	  Hashtbl.replace parameter_groups group_name (parameter :: parameters)
	with Not_found ->
	  assert false

  let add_group name = 
    let parameter_groups = plugin.p_parameters in
    if Hashtbl.mem parameter_groups name then 
      fatal "group %s already exists" name;
    Hashtbl.add parameter_groups name [];
    Cmdline.Group.add ~plugin:P.shortname name

  let () = 
    Cmdline.add_plugin P.name ~short:P.shortname ~descr:P.descr;
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
	 type t = bool
	 include X
	 let default () = default
	 let functor_name = "Bool"
	 let equal = (=)
	 let ty = Type.bool
       end)

    let on = register_dynamic "on" Type.unit Type.unit (fun () -> set true)
    let off = register_dynamic "off" Type.unit Type.unit (fun () -> set false)

    let generic_add_option name descr value =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group:!group_ref
	~descr
	~ext_descr:!optional_descr_ref
	stage
	(Cmdline.Unit (fun () -> unguarded_set value))

    let default_message = " (set by default)"

    let add_option name = 
      let descr = if X.default then X.descr ^ default_message else X.descr in
      generic_add_option name (Some descr) true

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
      let mk_descr s = Some (if X.default then s else s ^ default_message) in
      let neg_descr =
	match !negative_option_name_ref, !negative_option_descr_ref with
	| None, "" -> (* no user-specific config: no help *) None
	| Some _, "" -> mk_descr ("opposite of option \"" ^ name ^ "\"")
	| _, s -> assert (s <> ""); mk_descr s
      in
      generic_add_option neg_name neg_descr false;
      neg_name

    let add_alias = List.iter add_option

    let () =
      add_option X.option_name;
      let negative_option = 
	match !negative_option_name_ref, stage with
	| Some "", _  | None, Cmdline.Exiting -> None
	| _ -> Some (add_negative_option X.option_name)
      in
      add_parameter
	!group_ref stage State.name X.descr 
	(Bool({ get = get; set = set; is_set = is_set}, negative_option));
      reset ()

  end

  module False(X: Parameter_input) =
    Bool(struct include X let default = false end)

  module True(X: Parameter_input) =
    Bool(struct include X let default = true end)

  module Action(X: Parameter_input) =
  struct
    include False(X)
    let () = do_not_save ()
    let () = Project.create_by_copy_hook
      (* LC: to preserve tests oracle_copy. Not sure it is really what we want! *)
      (fun source target ->
	 let opt = Project.on source get () in
	 if opt then Project.on target set true)
  end

  (** {3 Integer} *)

  module Int(X: sig include Parameter_input_with_arg val default: int end) =
  struct

    include Build
      (struct
	 type t = int
	 include X
	 let default () = default
	 let functor_name = "Int"
	 let equal = (=)
	 let ty = Type.int
       end)

    let incr =
      let incr () = set (succ (get ())) in
      register_dynamic "incr" Type.unit Type.unit incr

    let add_option name =
      Cmdline.add_option
	name
	~argname:X.arg_name
	~descr:(Some X.descr)
	~ext_descr:!optional_descr_ref
	~plugin:P.shortname
	~group:!group_ref
	stage
	(Cmdline.Int unguarded_set)

    let add_alias = List.iter add_option

    let range = ref (min_int, max_int)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let () =
      add_set_hook 
	(fun _ n -> 
	   let min, max = !range in 
	   if n < min || n > max then 
	   abort "argument of %s must be between %d and %d" 
	     State.name
	     min
	     max);
      add_parameter 
	!group_ref stage State.name X.descr 
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
	 type t = string
	 include X
	 let default () = default
	 let functor_name = "String"
	 let equal = (=)
	 let ty = Type.string
       end)

    let add_option name =
      Cmdline.add_option
	name
	~argname:X.arg_name
	~descr:(Some X.descr)
	~ext_descr:!optional_descr_ref
	~plugin:P.shortname
	~group:!group_ref
	stage
	(Cmdline.String unguarded_set)

    let add_alias = List.iter add_option

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      add_set_hook 
	(fun _ s -> 
	   match !possible_values with
	   | [] -> ()
	   | v when List.mem s v -> ()
	   | _ -> abort "invalid input %s for %s" s State.name);
      add_parameter 
	!group_ref stage State.name X.descr 
	(String({ get = get; set = set; is_set = is_set }, 
		get_possible_values));
      add_option X.option_name;
      reset ()

  end

  module EmptyString(X: Parameter_input_with_arg) =
    String(struct include X let default = "" end)

  (** {3 String set and string list} *)

  module StringSet(X: Parameter_input_with_arg) = struct
    open Cilutil
    include Build
      (struct
	 let default () = StringSet.empty
	 let functor_name = "StringSet"
	 include StringSet 
	 include X 
	 let ty = Kernel_type.string_set
       end)

    let unguarded_add x = unguarded_set (StringSet.add x (get ()))
    let add x = set (StringSet.add x (get ()))
    let remove x = set (StringSet.remove x (get()))

    let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

    let set_and_split x acc =
      set (List.fold_right StringSet.add (split_set x) acc)
    let set_set x = set_and_split x StringSet.empty

    let guarded_set_set x =
      let l = split_set x in
      if not (List.for_all (fun s -> StringSet.mem s (get ())) l) then
	set (List.fold_right StringSet.add l StringSet.empty)

    let add_set x = set_and_split x (get ())

    let get_set ?(sep=", ") () =
      StringSet.fold
	(fun s acc -> if acc <> "" then s ^ sep ^ acc else s) (get ()) ""

    let remove_set x =
      set (List.fold_right StringSet.remove (split_set x) (get()))

    let is_empty () = StringSet.is_empty (get ())
    let iter f = StringSet.iter f (get ())
    let fold f acc = StringSet.fold f (get ()) acc

    let add_option name =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group:!group_ref
	~argname:X.arg_name
	~descr:(Some X.descr)
	~ext_descr:!optional_descr_ref
	stage
	(Cmdline.String_list (List.iter unguarded_add))

    let add_alias = List.iter add_option

    let () =
      add_parameter 
	!group_ref stage State.name X.descr 
	(StringSet{ get = (fun () -> get_set ()); 
		    set = guarded_set_set;
		    is_set = is_set });
      add_option X.option_name;
      reset ()

  end

  module StringList(X: Parameter_input_with_arg) = struct

    include Build
      (struct
	 let default () = []
	 include X
	 let functor_name = "StringList"
	 let ty = Type.list Type.string
	 type t = string list
	 let equal = (=)
       end)

    let add = gen_journalized "add" Type.string	(fun x -> set (x :: (get ())))

    let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

    let set_and_split x acc =
      set (List.fold_right (fun a b -> a::b) (split_set x) acc)
    let set_set x = set_and_split x []
    let add_set x = set_and_split x (get ())

    let get_set ?(sep=", ") () =
      List.fold_right
	(fun s acc -> if acc<>"" then s^sep^acc else s) (get ()) ""
    let iter f = List.iter f (get ())
    let fold f acc = List.fold_right f (get ()) acc

    let add_alias _ = assert false

  end

  (** {3 Complex values indexed by strings} *)

  module IndexedVal (V:COMPLEX_VALUE):INDEXED_VAL with type value = V.t =
  struct

    type value = V.t

    let options = Hashtbl.create 13
    let add_choice k v  = Hashtbl.add options k v
    let () = add_choice V.default_key V.default_val

    let create () = ref V.default_key

    let curr_choice = ref (create())

    module StateAux = struct
      let name = V.option_name
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
      let rehash s = copy s
      let clear tbl = tbl:= V.default_key
      let dependencies = [] 
      let clear_if_project _ _ = false (* a parameter cannot be a project *)
   end

    let equal = (=)

    module State =
      Project.Computation.Register
	(Datatype.Ref(Datatype.String))(StateAux)(StateAux)

    type t = string

    let () = add_option State.select

    include State

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

    let add_option name =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group:!group_ref
	~argname:V.arg_name
	~descr:(Some V.descr)
	~ext_descr:!optional_descr_ref
	stage
	(Cmdline.String unguarded_set)

    let add_alias = List.iter add_option

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      add_option V.option_name;
      reset ()

  end

  (** {2 Generic options for each plug-in} *)

  let prefix = if P.shortname = "" then "-kernel-" else "-" ^ P.shortname ^ "-"

  let () = set_cmdline_stage Cmdline.Exiting
  let () = if is_kernel () then set_module_name "Help"
  module Help =
    False(struct
	    let option_name = prefix ^ "help"
	    let descr =
	      if is_kernel () then "help of the Frama-C kernel"
	      else "help of plug-in " ^ P.name
	  end)
  let () =
    Cmdline.run_after_exiting_stage
      (fun () -> 
	 if Help.get () then Cmdline.plugin_help P.shortname 
	 else Cmdline.nop)

  let () = do_not_projectify ()
  let () = do_not_journalize ()
  let () = if is_kernel () then set_module_name "Verbose"
  let () = 
    set_cmdline_stage 
      (if is_kernel () then Cmdline.Early else Cmdline.Extended)
  module Verbose =
    Int(struct
	  let default = !verbose_level
	  let option_name = prefix ^ "verbose"
	  let arg_name = "n"
	  let descr =
	    (if is_kernel () then "level of verbosity for the Frama-C kernel"
	     else "level of verbosity for plug-in " ^ P.name)
	    ^ " (default to 1)"
	end)

  let () = do_not_projectify ()
  let () = do_not_journalize ()
  let () = if is_kernel () then set_module_name "Debug"
  let () = 
    set_cmdline_stage 
      (if is_kernel () then Cmdline.Early else Cmdline.Extended)
  module Debug =
    Int(struct
	  let default = !debug_level
	  let option_name = prefix ^ "debug"
	  let arg_name = "n"
	  let descr =
	    (if is_kernel () then "level of debug for the Frama-C kernel"
	     else "level of debug for plug-in " ^ P.name)
	    ^ " (default to 0)"
	end)

  (* set level of debug/verbose of the plug-in according to the level of
     debug/verbose of frama-c *)
  let () =
    Cmdline.run_after_configuring_stage
      (fun () ->
	 if Cmdline.verbose_isset && not (Verbose.is_set ()) then
	   Verbose.unjournalized_set Cmdline.verbose_level;
	 if Cmdline.debug_isset && not (Debug.is_set ()) then
	   Debug.unjournalized_set Cmdline.debug_level;
	 debug_level := Debug.get ();
	 verbose_level := Verbose.get ())

  let () = reset_plugin ()

end (* Register *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

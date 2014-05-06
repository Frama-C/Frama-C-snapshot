(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

module D = Datatype (* hide after applying Parameter_state.Make *)
let empty_string = ""

(* ************************************************************************* *)
(** {2 Specific functors} *)
(* ************************************************************************* *)

let iter_on_this_parameter stage = 
  match !Parameter_customize.do_iterate_ref, stage with
  | Some false, _
  | None, (Cmdline.Early | Cmdline.Extending | Cmdline.Extended
              | Cmdline.Exiting | Cmdline.Loading) ->
    false
  | Some true, _ | None, Cmdline.Configuring ->
    true

module Make
  (P: sig
    val shortname: string
    val parameters: (string, Typed_parameter.t list) Hashtbl.t
    module L: sig 
      val abort: ('a,'b) Log.pretty_aborter
      val warning: 'a Log.pretty_printer
    end
    val messages_group: Cmdline.Group.t
  end) =
struct

  module Build = Parameter_state.Make(P)

  let parameters_ref : Typed_parameter.t list ref = ref []
  let parameters () = !parameters_ref

  let add_parameter group stage param =
    if iter_on_this_parameter stage then begin
      parameters_ref := param :: !parameters_ref;
      let parameter_groups = P.parameters in
      try
        let group_name = Cmdline.Group.name group in
        let parameters = Hashtbl.find P.parameters group_name in
        Hashtbl.replace parameter_groups group_name (param :: parameters)
      with Not_found ->
        assert false
    end

  (* ************************************************************************ *)
  (** {3 Bool} *)
  (* ************************************************************************ *)
  
  module Bool(X:sig include Parameter_sig.Input val default: bool end) = struct
    
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
	~ext_help:!Parameter_customize.optional_help_ref
	stage
	(Cmdline.Unit (fun () -> set value))

    let negative_option_name name =
      let s = !Parameter_customize.negative_option_name_ref in
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
              else default_message empty_string
	else
          if opp then Pretty_utils.sfprintf "%s (%s)" X.help (opp_msg name)
          else X.help
      in
      generic_add_option name help is_visible true

    let add_negative_option name =
      let neg_name = negative_option_name name in
      let mk_help s =
	if is_visible then 
	  if X.default then s else s ^ default_message empty_string
	else empty_string
      in
      let neg_help, neg_visible =
	match
	  !Parameter_customize.negative_option_name_ref, 
	  !Parameter_customize.negative_option_help_ref 
	with
	| None, "" -> (* no user-specific config: no help *) empty_string, false
	| Some _, "" -> 
	  mk_help ("opposite of option \"" ^ name ^ "\""), is_visible
	| _, s -> assert (s <> empty_string); mk_help s, is_visible
      in
      generic_add_option neg_name neg_help neg_visible false;
      neg_name

    let parameter =
      let negative_option =
	match !Parameter_customize.negative_option_name_ref, stage with
	| Some "", _  | None, Cmdline.Exiting ->
          add_option false X.option_name;
          None
	| _ ->
          add_option true X.option_name;
          Some (add_negative_option X.option_name)
      in
      let accessor =
	Typed_parameter.Bool
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           negative_option)
      in
      let p =
	Typed_parameter.create ~name ~help:X.help ~accessor:accessor ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      Parameter_customize.reset ();
      if is_dynamic then
	let plugin = empty_string in
	Dynamic.register
          ~plugin X.option_name Typed_parameter.ty ~journalize:false p
      else p

  end

  module False(X: Parameter_sig.Input) =
    Bool(struct include X let default = false end)

  module True(X: Parameter_sig.Input) =
    Bool(struct include X let default = true end)

  module Action(X: Parameter_sig.Input) = struct

    (* [JS 2011/09/29]
       The ugly hack seems to be required anymore neither for Value nor Wp.
       Maybe it is time to remove it? :-) *)

    (* do not save it but restore the "good" behavior when creating by copy *)

    let () = Parameter_customize.do_not_save ()
    (* [JS 2011/01/19] Not saving this kind of options is a quite bad hack with
       several drawbacks (see Frama-C commits 2011/01/19, message of JS around
       15 PM). I'm quite sure there is a better way to not display results too
       many times (e.g. by using the "isset" flag).  That is also the origin of
       bug #687 *)

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

  (* ************************************************************************ *)
  (** {3 Integer} *)
  (* ************************************************************************ *)

  module Int(X: sig include Parameter_sig.Input_with_arg val default: int end) =
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
	~ext_help:!Parameter_customize.optional_help_ref
	~plugin:P.shortname
	~group
	stage
	(Cmdline.Int set)

    let range = ref (min_int, max_int)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let parameter =
      add_set_hook
	(fun _ n ->
          let min, max = !range in
          if n < min then
            P.L.abort "argument of %s must be at least %d." name min;
          if n > max then
            P.L.abort "argument of %s must be no more than %d." name max);
      let accessor =
	Typed_parameter.Int
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_range)
      in
      let p =
	Typed_parameter.create ~name ~help:X.help ~accessor ~is_set:is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
	let plugin = empty_string in
	Dynamic.register
          ~plugin X.option_name Typed_parameter.ty ~journalize:false p
      else p

  end

  module Zero(X: Parameter_sig.Input_with_arg) =
    Int(struct include X let default = 0 end)

  (* ************************************************************************ *)
  (** {3 String} *)
  (* ************************************************************************ *)

  module Pervasives_string = String

  module String
    (X: sig include Parameter_sig.Input_with_arg val default: string end) =
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
	~ext_help:!Parameter_customize.optional_help_ref
	~plugin:P.shortname
	~group
	stage
	(Cmdline.String set)

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let () =
      if !Parameter_customize.argument_is_function_name_ref then begin
	Parameter_customize.apply_ast_hook set_possible_values
      end

    let parameter =
      add_set_hook
	(fun _ s ->
          match !possible_values with
          | [] -> ()
          | v when List.mem s v -> ()
          | _ -> P.L.abort "invalid input `%s' for %s" s name);
      let accessor =
	Typed_parameter.String
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_possible_values)
      in
      let p =
	Typed_parameter.create ~name ~help:X.help ~accessor ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
	let plugin = empty_string in
	Dynamic.register
          ~plugin X.option_name Typed_parameter.ty ~journalize:false p
      else
	p
	  
  end

  module EmptyString(X: Parameter_sig.Input_with_arg) =
    String(struct include X let default = empty_string end)

  (* ************************************************************************ *)
  (** {3 String set and string list} *)
  (* ************************************************************************ *)

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
  (X:Parameter_sig.Input_with_arg) =
  struct

    include Build(struct include S include X end)
      
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
      if !Parameter_customize.argument_is_function_name_ref then
	Parameter_customize.apply_ast_hook set_possible_values

    let guarded_set_set x =
      match split_set x with
      | [] when not (S.is_empty (get ())) ->
	set S.empty
      | l ->
	List.iter
          (fun s ->
            if !possible_values != [] then
              if not (List.mem s !possible_values) then
		P.L.abort "invalid input `%s' for %s" s name)
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

    let fold f = S.fold f (get ())
      
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
	~ext_help:!Parameter_customize.optional_help_ref
	stage
	(Cmdline.String_list (List.iter f))
        
    let add_option name help = add_generic_option name help add
    let add_option_unset name help = add_generic_option name help remove
	
  end

  module FilledStringSet
    (X: sig
      include Parameter_sig.Input_with_arg
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
	Typed_parameter.String_set
          { Typed_parameter.get = get_set;
            set = guarded_set_set;
            add_set_hook = add_set_hook;
            add_update_hook = add_update_hook }
      in
      let p =
	Typed_parameter.create ~name ~help:X.help ~accessor:accessor ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name X.help;
      if !Parameter_customize.unset_option_name_ref <> empty_string then begin
	let help =
          if !Parameter_customize.unset_option_help_ref = empty_string then
            "opposite of option " ^ X.option_name
          else !Parameter_customize.unset_option_help_ref
	in
	add_option_unset !Parameter_customize.unset_option_name_ref help
      end;
      Parameter_customize.reset ();
      if is_dynamic then
	let plugin = empty_string in
	Dynamic.register
	  ~plugin X.option_name Typed_parameter.ty ~journalize:false p
      else
	p

  end

  module StringSet(X: Parameter_sig.Input_with_arg) =
    FilledStringSet
      (struct
	include X
	let default = Datatype.String.Set.empty
       end)
    
  module StringList(X: Parameter_sig.Input_with_arg) = struct

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

    let append_before = 
      let append_before l = set (l @ get ()) in
      register_dynamic "append_before" (D.list D.string) D.unit append_before

    let append_after =
      let append_after l = set (get() @ l) in
      register_dynamic "append_after" (D.list D.string) D.unit append_after

    let parameter =
      let accessor =
	Typed_parameter.String_list
          { Typed_parameter.get = get_set;
            set = guarded_set_set;
            add_set_hook = add_set_hook;
            add_update_hook = add_update_hook }
      in
      let p =
	Typed_parameter.create ~name ~help:X.help ~accessor:accessor ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name X.help;
      Parameter_customize.reset ();
      if is_dynamic then
	let plugin = empty_string in
	Dynamic.register
	  ~plugin X.option_name Typed_parameter.ty ~journalize:false p
      else p

  end

  module StringHashtbl
    (X: Parameter_sig.Input_with_arg)
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

  (* ************************************************************************ *)
  (** {3 Complex values indexed by strings} *)
  (* ************************************************************************ *)

  module IndexedVal(V: Parameter_sig.Indexed_val_input): 
    Parameter_sig.Indexed_val with type value = V.t =
  struct

    type value = V.t

    let is_dynamic = true
    
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
          else P.L.abort "invalid input %s for %s" v V.option_name

      let clear tbl = tbl := V.default_key
      let dependencies = []
      let clear_some_projects _ _ = false (* a parameter cannot be a project *)
    end

    module State =
      State_builder.Register(Datatype.Ref(Datatype.String))(StateAux)(StateAux)
    include State

    let () = 
      Parameter_state.extend_selection false self;
      if not !Parameter_customize.reset_on_copy_ref then
	Parameter_state.extend_no_reset_selection false self

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
	P.L.warning
          "identifier %s is not a valid index for parameter %s. \
Option is unchanged.\n" s V.option_name

    let set s = if s <> get () then unguarded_set s
      
    let clear () = !curr_choice := V.default_key

    (* [JS 2009/04/17] TODO: reimplement is_set according to its new
       specification *)
    let is_set () = (*!(!curr_choice) <> V.default_key*) assert false
    let is_default () = !(!curr_choice) = V.default_key

    let unsafe_set = set

    let stage = !Parameter_customize.cmdline_stage_ref
    let group = !Parameter_customize.group_ref

    let add_option name =
      Cmdline.add_option
	name
	~plugin:P.shortname
	~group
	~argname:V.arg_name
	~help:V.help
	~visible:!Parameter_customize.is_visible_ref
	~ext_help:!Parameter_customize.optional_help_ref
	stage
	(Cmdline.String unguarded_set)

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let option_name = V.option_name

    let add_aliases = 
      Cmdline.add_aliases option_name ~plugin:P.shortname ~group stage

    let print_help fmt =
      Cmdline.print_option_help fmt ~plugin:P.shortname ~group V.option_name

    let parameter =
      let accessor =
	Typed_parameter.String
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           (fun () -> []))
      in
      let p =
	Typed_parameter.create
          ~name:V.option_name
          ~help:V.help
          ~accessor
          ~is_set
      in
      if is_dynamic then
	Dynamic.register
          ~plugin:empty_string 
	  V.option_name 
	  Typed_parameter.ty 
	  ~journalize:false 
	  p
      else p

    let () =
      add_option V.option_name;
      Parameter_customize.reset ()

  end

  (** Options that directly cause an output. *)
  module WithOutput
    (X: sig include Parameter_sig.Input val output_by_default: bool end) =
  struct
    
    (* Requested command-line option *)
    include False(X)

    (* Command-line option for output. *)
    let () = Parameter_customize.set_group P.messages_group
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

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

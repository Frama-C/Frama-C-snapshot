(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

module D = Datatype (* hide after applying Parameter_state.Make *)
let empty_string = ""

let find_kf_by_name
    : (string -> kernel_function) ref
    = Extlib.mk_fun "Parameter_builder.find_kf_by_name"

let find_kf_def_by_name
    : (string -> kernel_function) ref
    = Extlib.mk_fun "Parameter_builder.find_kf_def_by_name"

let kf_category
    : (unit -> kernel_function Parameter_category.t) ref
    = Extlib.mk_fun "Parameter_builder.kf_category"

let kf_def_category
    : (unit -> kernel_function Parameter_category.t) ref
    = Extlib.mk_fun "Parameter_builder.kf_def_category"

let fundec_category
    : (unit -> fundec Parameter_category.t) ref
    = Extlib.mk_fun "Parameter_builder.fundec_category"

let kf_string_category
    : (unit -> string Parameter_category.t) ref
    = Extlib.mk_fun "Parameter_builder.kf_string_category"

let force_ast_compute
    : (unit -> unit) ref
    = Extlib.mk_fun "Parameter_builder.force_ast_compute"

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

    let get_function_name =
      let allow_fundecl = !Parameter_customize.argument_may_be_fundecl_ref in
      fun () ->
        let s = get () in
        (* Using a parameter that is in fact a function name only makes sense
           if we have an AST somewhere. *)
        !force_ast_compute();
        let possible_funcs = Parameter_customize.get_c_ified_functions s in
        let possible_funcs =
          if allow_fundecl then possible_funcs
          else
            Cil_datatype.Kf.Set.filter
              (fun s ->
                match s.fundec with
                  | Definition _ -> true
                  | Declaration _ -> false)
              possible_funcs
        in
        if Cil_datatype.Kf.Set.is_empty possible_funcs then
          P.L.abort
            "'%s' is not a %sfunction. \
             Please choose a valid function name for option %s"
            s (if allow_fundecl then "" else "defined ") name
        else begin
          if Cil_datatype.Kf.Set.cardinal possible_funcs > 1 then
            P.L.warning
              "ambiguous function name %s for option %s. \
               Choosing arbitrary function with corresponding name."
              s name;
          (Cil_datatype.Kf.vi
             (Cil_datatype.Kf.Set.choose possible_funcs)).vname
        end

    let get_plain_string = get

    let get =
      if !Parameter_customize.argument_is_function_name_ref then
        get_function_name
      else get

    let parameter =
      add_set_hook
	(fun _ s ->
          match !possible_values with
          | [] -> ()
          | v when List.mem s v -> ()
          | _ -> P.L.abort "invalid input '%s' for option %s." s name);
      let accessor =
	Typed_parameter.String
          ({ Typed_parameter.get = get_plain_string; set = set;
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

  module Empty_string(X: Parameter_sig.Input_with_arg) =
    String(struct include X let default = empty_string end)

  (* ************************************************************************ *)
  (** {3 Collections} *)
  (* ************************************************************************ *)

  type collect_action = Add | Remove

  exception Cannot_build of string

  let cannot_build msg = raise (Cannot_build msg)
  let no_element_of_string msg = cannot_build msg

  module Make_collection
    (E: sig (* element in the collection *)
      type t
      val ty: t Type.t
      val of_string: string -> t (* may raise [Cannot_build] *)
      val to_string: t -> string
    end)
    (C: sig (* the collection, as a persistent datastructure *)
      type t
      val equal: t -> t -> bool
      val empty: t
      val is_empty: t -> bool
      val add: E.t -> t -> t
      val remove: E.t -> t -> t
      val iter: (E.t -> unit) -> t -> unit
      val fold: (E.t -> 'a -> 'a) -> t -> 'a -> 'a
      val of_singleton_string: string -> t 
      (* For specific ways to parse a collection from a single string.
         If physically equal to [no_element_of_string], we revert back to
         using [E.of_string]
       *)
      val reorder: t -> t 
      (* Used after having parsed a comma-separated string representing 
         parameters. The add actions are done in the reverse order with
         respect to the list. Can be [Extlib.id] for unordered collections.
       *)
    end)
    (S: sig (* the collection, as a state *)
      include State_builder.S
      val memo: (unit -> C.t) -> C.t
      val clear: unit -> unit
    end)
    (X: (* standard option builder *) sig
      include Parameter_sig.Input_with_arg
      val default: C.t
    end)
    =
  struct

    type t = C.t
    type elt = E.t

    (* ********************************************************************** *)
    (* Categories *)
    (* ********************************************************************** *)

    type category = E.t Parameter_category.t

    (* the available custom categories for this option *)
    let available_categories
	: category Datatype.String.Hashtbl.t
	= Datatype.String.Hashtbl.create 7

    module Category = struct

      type elt = E.t
      type t = category
      
      let check_category_name s =
	if Datatype.String.Hashtbl.mem available_categories s
          || Datatype.String.equal s "all"
          || Datatype.String.equal s ""
          || Datatype.String.equal s "default"
	then
	  P.L.abort "invalid category name '%s'" s

      let use categories = 
	List.iter
	  (fun c -> 
	    Parameter_category.use S.self c;
	    Datatype.String.Hashtbl.add
	      available_categories
	      (Parameter_category.get_name c)
	      c)
	  categories

      let unsafe_add name states accessor =
        let c =
          Parameter_category.create name E.ty ~register:false states accessor
        in
        use [ c ];
        c

      let add name states get_values =
        check_category_name name;
        unsafe_add name states get_values

      let none =
        let o = object
          method fold: 'b. ('a -> 'b -> 'b) -> 'b -> 'b = (fun _ acc -> acc);
          method mem = fun _ -> false
        end in
        unsafe_add "" [] o

      let default_ref = ref none
      let () = Datatype.String.Hashtbl.add available_categories "default" none

      let default () = !default_ref
      let set_default c =
        Datatype.String.Hashtbl.replace available_categories "default" c;
        default_ref := c

      let all_ref: t option ref = ref None
      let all () = !all_ref

      let on_enable_all c =
        (* interpretation may have change:
           reset the state to force the interpretation again *)
        S.clear ();
        all_ref := Some c

      let enable_all_as c =
        use [ c ];
        let all = Parameter_category.copy_and_rename "all" ~register:false c in
        Datatype.String.Hashtbl.add available_categories "all" all;
        on_enable_all all

      let enable_all states get_values =
        let all = unsafe_add "all" states get_values in
        on_enable_all all;
        all

    end

    (* ********************************************************************** *)
    (* Parsing *)
    (* ********************************************************************** *)

    let use_category = !Parameter_customize.use_category_ref

    (* parsing builds a list of triples  (action, is_category?, word) *)

    let add_action a l = (a, false, None) :: l

    let add_char c = function
      | [] -> assert false
      | (a, f, None) :: l ->
	(* first char of a new word *)
	let b = Buffer.create 7 in
	Buffer.add_char b c;
	(a, f, Some b) :: l
      | ((_, _, Some b) :: _) as l ->
	(* extend the current word *)
	Buffer.add_char b c;
	l

    let set_category_flag = function
      | (a, false, None) :: l -> (a, true, None) :: l
      | _ -> assert false

    type position =
      | Start (* the very beginning or after a comma *)
      | Word of (* action already specified, word is being read *)
	  bool (* [true] iff beginning a category with '@' is allowed *)
      | Escaped (* the next char is escaped in the current word *)

    let parse_error msg =
      P.L.abort "@[@[incorrect argument for option %s@ (%s).@]"
        X.option_name msg

    (* return the list of tokens, in reverse order *)
    let parse s = 
      let len = Pervasives_string.length s in
      let rec aux acc pos i s =
	if i = len then acc
	else 
	  let next = i + 1 in
	  let read_char_in_word f_acc new_pos =
            (* assume 'Add' by default *)
            let acc = if pos = Start then add_action Add acc else acc in
	    aux (f_acc acc) new_pos next s
	  in
	  let read_std_char_in_word c =
	    read_char_in_word (add_char c) (Word false)
	  in
	  match Pervasives_string.get s i, pos with
          | '+', Start when use_category ->
            aux (add_action Add acc) (Word true) next s
          | '-', Start when use_category ->
            aux (add_action Remove acc) (Word true) next s
          | '\\', (Start | Word _) -> read_char_in_word (fun x -> x) Escaped
	  | ',', (Start | Word _) -> read_char_in_word (fun x -> x) Start
	  | (' ' | '\t' | '\n' | '\r'), Start -> 
	    (* ignore whitespaces at beginnning of words (must be escaped) *)
	    aux acc pos next s
	  | '@', (Start | Word true) when use_category ->
	    read_char_in_word set_category_flag (Word false)
	  | c, (Start | Word _) -> read_std_char_in_word c
	  | (',' | '\\' as c), Escaped -> read_std_char_in_word c
	  | ('+' | '-' | '@' | ' ' | '\t' | '\n' | '\r' as c), 
	    Escaped when i = 1 ->
            if use_category then read_std_char_in_word c
            else
              parse_error
                ("invalid escaped char '" ^ Pervasives_string.make 1 c ^ "'")
	  | c, Escaped ->
	    parse_error
              ("invalid escaped char '" ^ Pervasives_string.make 1 c ^ "'")
      in
      aux [] Start 0 s

    (* ********************************************************************** *)
    (* The parameter itself, as a special string option *)
    (* ********************************************************************** *)

    let string_of_collection c =
      if C.is_empty c then ""
      else
        let b = Buffer.create 17 in
        let first = ref true in
        C.iter
          (fun e ->
            let s = E.to_string e in
            if !first then begin if s <> "" then first := false end
            else Buffer.add_string b ",";
            Buffer.add_string b (E.to_string e))
          c;
        Buffer.contents b

    (* a collection is a standard string option... *)
    module As_string = 
      String(struct
	include X
	let default = string_of_collection X.default
      end)

    (* ... which is cumulative, when set from the cmdline (but uniquely from
       this way since it is very counter-intuitive from the other ways
       (i.e. programmatically or the GUI). *)
    let () =
      Cmdline.replace_option_setting 
	X.option_name
	~plugin:P.shortname
	~group:As_string.group
	(Cmdline.String
	   (fun s ->
	     let old = As_string.get () in
	     As_string.set 
	       (if Datatype.String.equal old empty_string then s 
		else old ^ "," ^ s)))

    (* JS personal note: I'm still not fully convinced by this cumulative
       semantics. *)

    (* Note: no dependency between [As_string] and [State], but consistency
       handles by the hook below. Setting a dependency between those states
       would break [Parameter_state.get_selection_context]. *)

    let () =
      (* reset the state, but delayed its computation untill its first access
         to get the correct interpretation. *)
      As_string.add_update_hook (fun _ _ -> S.clear ())

    let check_possible_value elt = match Category.all () with
      | None -> ()
      | Some a ->
	if not (Parameter_category.get_mem a elt) then
          parse_error ("impossible value " ^  E.to_string elt)

    (* may be costly: use it with parsimony *)
    let collection_of_string ~check s =
      (*        Format.printf "READING %s: %s@." X.option_name s;*)
      let tokens = parse s in
      (* remember: tokens are in reverse order. So handle the last one
         first. *)
      let unparsable, col =
        List.fold_right
          (fun (action, is_category, word) (unparsable, col) ->
            let extend = match action with
              | Add -> C.add
              | Remove -> C.remove
            in
            let word = match word with
              | None -> "" 
              | Some b -> Buffer.contents b 
            in
              (*              Format.printf "TOKEN %s@." word;*)
            if is_category then
              try
                let c =
                  Datatype.String.Hashtbl.find available_categories word
                in
                if word = "all" then
                  match action with
                  | Add ->
                    unparsable, Parameter_category.get_fold c C.add C.empty
                  | Remove ->
                      (* -@all is always equal to the emptyset, even if there
                         were previous elements which are now impossible *)
                      None, C.empty
                else
                    unparsable, Parameter_category.get_fold c extend col
              with Not_found ->
                parse_error ("unknown category '" ^ word ^ "'")
            else (* not is_category *)
                try
                  if C.of_singleton_string == no_element_of_string then begin
                    let elt = E.of_string word in
                    unparsable, extend elt col
                  end else begin
                    let elts = C.of_singleton_string word in
                    unparsable, C.fold extend elts col
                  end
                with Cannot_build msg ->
                  Some msg, col)
            tokens
            (None, C.empty)
        in
        let col = C.reorder col in
        (* check each element after parsing all of them,
           since an element may be added, then removed later (e.g +h,-@all):
           that has to be accepted *)
        if check then begin
          Extlib.may parse_error unparsable;
          C.iter check_possible_value col
        end;
        col

    (* ********************************************************************** *)
    (* Memoized access to the state *)
    (* ********************************************************************** *)

    let get_nomemo () = S.memo (fun () -> raise Not_found)

    let get () =
      S.memo
        (fun () ->
          (*let c = *)collection_of_string ~check:true (As_string.get ()) (*in
          Format.printf "GET %s@." (As_string.get ());
          C.iter (fun s -> Format.printf "ELT %s@." (E.to_string s)) c;
          c*))

    (* ********************************************************************** *)
    (* Implement the state, by overseded [As_string]:

       not the more efficient, but the simplest way that prevent to introduce
       subtle bugs *)
    (* ********************************************************************** *)

    let set c = As_string.set (string_of_collection c)
    let unsafe_set c = As_string.unsafe_set (string_of_collection c)

    let convert_and_apply f = fun old new_ ->
      f
        (collection_of_string ~check:false old)
        (collection_of_string ~check:true new_)

    let add_set_hook f = As_string.add_set_hook (convert_and_apply f)
    let add_update_hook f = As_string.add_update_hook (convert_and_apply f)

    (* ********************************************************************** *)
    (* Implement operations *)
    (* ********************************************************************** *)

    let add e = set (C.add e (get ()))
    let is_empty () = C.is_empty (get ())
    let iter f = C.iter f (get ())
    let fold f acc = C.fold f (get ()) acc

    (* ********************************************************************** *)
    (* Re-export values *)
    (* ********************************************************************** *)

    let name = As_string.name
    let option_name = As_string.option_name
    let is_default = As_string.is_default
    let is_set = As_string.is_set
    let clear = As_string.clear
    let print_help = As_string.print_help
    let add_aliases = As_string.add_aliases
    let self = As_string.self
    let parameter = As_string.parameter

    let equal = C.equal
    let is_computed = S.is_computed
    let mark_as_computed = S.mark_as_computed

    (* [Datatype] is fully abstract from outside anyway *)
    module Datatype = As_string.Datatype

    (* cannot be called anyway since [Datatype] is abstract *)
    let howto_marshal _marshal _unmarshal =
      P.L.abort "[how_to_marshal] cannot be implemented for %s." X.option_name

    (* same as above *)
    let add_hook_on_update _ =
      P.L.abort "[add_hook_on_update] cannot be implemented for %s." 
	X.option_name

  end

  module Make_set
    (E: Parameter_sig.String_datatype_with_collections)
    (X: sig 
      include Parameter_sig.Input_with_arg
      val default: E.Set.t
    end):
    Parameter_sig.Set with type elt = E.t and type t = E.Set.t =
  struct

    module C = struct
      include E.Set
      let reorder = Extlib.id
      let of_singleton_string = E.of_singleton_string
    end

    module S = struct

      include State_builder.Option_ref
	(E.Set)
	(struct 
	  let name = X.option_name ^ " set"
	  let dependencies = [] 
	 end)

      let memo f = memo f (* ignore the optional argument *)
    end

    include Make_collection(E)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let mem e = E.Set.mem e (get ())
    let exists f = E.Set.exists f (get ())

  end

  module String_for_collection = struct
    include Datatype.String
    let of_string = Datatype.identity
    let to_string = Datatype.identity
    let of_singleton_string = no_element_of_string
  end

  module String_set(X: Parameter_sig.Input_with_arg) =
    Make_set
      (String_for_collection)
      (struct include X let default = Datatype.String.Set.empty end)

  module Filled_string_set = Make_set(String_for_collection)

  let check_function s must_exist no_function set =
    if no_function set then
      let error s = cannot_build (Pretty_utils.sfprintf "no function '%s'" s) in
      if must_exist then
        error s
      else
        if !Parameter_customize.is_permissive_ref then begin
          P.L.warning "ignoring non-existing function '%s'." s;
          set
        end else
          error s
    else
      set

  module Kernel_function_string(
    A: sig val accept_fundecl: bool
           val must_exist: bool
    end) =
  struct

    include Cil_datatype.Kf

    let of_string s =
      try
        (if A.accept_fundecl then !find_kf_by_name else !find_kf_def_by_name) s
      with Not_found ->
        cannot_build
          (Pretty_utils.sfprintf "no%s function '%s'"
             (if A.accept_fundecl then "" else " defined")
             s)

    (* Cannot reuse any code to implement [to_string] without forward
       reference. Prefer small code duplication here. *)
    let to_string kf = match kf.fundec with
      | Definition(d, _) -> d.svar.vname
      | Declaration(_, vi, _, _) -> vi.vname

    let of_singleton_string s =
      let fcts = Parameter_customize.get_c_ified_functions s in
      let res =
        if A.accept_fundecl then fcts else
          Set.filter
            (fun s ->
              match s.fundec with
                | Definition _ -> true
                | Declaration _ -> false)
            fcts
      in
      check_function s A.must_exist Set.is_empty res

  end

  module Kernel_function_set(X: Parameter_sig.Input_with_arg) = struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_set
    (Kernel_function_string(A))
    (struct include X let default = Cil_datatype.Kf.Set.empty end)

    let () =
      if A.accept_fundecl then Category.enable_all_as (!kf_category ())
      else Category.enable_all_as (!kf_def_category ())

  end

  module Fundec_set(X: Parameter_sig.Input_with_arg) = struct
    let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref

    include Make_set
    (struct
      include Cil_datatype.Fundec
      let of_string s =
        try
          let kf = !find_kf_def_by_name s in
          match kf.fundec with
          | Definition (f, _) -> f
          | Declaration _ -> assert false
        with Not_found ->
          cannot_build (Pretty_utils.sfprintf "no defined function '%s'" s)

      let to_string f = f.svar.vname

      let of_singleton_string s =
        let fcts = Parameter_customize.get_c_ified_functions s in
        let defs = 
          Cil_datatype.Kf.Set.fold
            (fun s acc ->
              match s.fundec with
                | Definition(f,_) -> Set.add f acc
                | Declaration _ -> acc)
            fcts Set.empty
        in
        check_function s must_exist Set.is_empty defs

     end)
    (struct include X let default = Cil_datatype.Fundec.Set.empty end)

    let () = Category.enable_all_as (!fundec_category ())

  end

  module Make_list
    (E: sig
      include Parameter_sig.String_datatype
      val of_singleton_string: string -> t list
    end)
    (X: sig include Parameter_sig.Input_with_arg val default: E.t list end):
    Parameter_sig.List with type elt = E.t and type t = E.t list =
  struct

    module C = struct
      include Datatype.List(E)
      let empty = []
      let is_empty l = l == []
      let add (x:E.t) l = x :: l
      let remove x l = List.filter (fun y -> not (E.equal x y)) l
      let iter = List.iter
      let fold f l acc = List.fold_left (fun acc x -> f x acc) acc l
      let reorder = List.rev
      let of_singleton_string = E.of_singleton_string
    end

    module S = struct

      include State_builder.Option_ref
        (C)
        (struct
          let name = X.option_name ^ " list"
          let dependencies = []
         end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(E)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let append_before l = set (l @ get ())
    let append_after l = set (get () @ l)

  end

  module String_list(X: Parameter_sig.Input_with_arg) =
    Make_list
      (String_for_collection)
      (struct include X let default = [] end)

  module Make_map
    (K: Parameter_sig.String_datatype_with_collections)
    (V: Parameter_sig.Value_datatype with type key = K.t)
    (X: sig include Parameter_sig.Input_with_arg val default: V.t K.Map.t end) =
  struct

    type key = K.t
    type value = V.t

    let find_ref = ref (fun _ -> assert false)

    let of_val ~key k ~prev v =
      try V.of_string ~key ~prev v
      with Cannot_build s ->
        cannot_build
          (Pretty_utils.sfprintf "@[value bound to '%s':@ %s@]" k s)

    module Pair = struct
      include Datatype.Pair(K)(Datatype.Option(V))
      let of_string =
        let r = Str.regexp_string ":" in
        fun s ->
          match Str.bounded_split_delim r s 2 with
          | [] -> cannot_build ("cannot interpret '" ^ s ^ "'")
          | [ k ] ->
            let key = K.of_string k in
            let prev = try Some (!find_ref key) with Not_found -> None in
            key, of_val ~key k ~prev None
          | [ k; v ] ->
            let key = K.of_string k in
            let prev = try Some (!find_ref key) with Not_found -> None in
            key, of_val ~key k ~prev (Some v)
          | _ :: _ :: _ :: _ ->
            (* by definition of [Str.bounded_split_delim]: *)
            assert false
      let to_string (key, v) =
        let v = V.to_string ~key v in
        let delim, v = match v with
          | None -> "", ""
          | Some v -> ":", v
        in
        Pretty_utils.sfprintf "%s%s%s" (K.to_string key) delim v
    end

    module C = struct
      type t = V.t K.Map.t
      let equal = K.Map.equal V.equal
      let empty = K.Map.empty
      let is_empty = K.Map.is_empty
      let add (k, v) m = match v with
        | None ->
          (* no value associated to the key: remove the previous binding *)
          K.Map.remove k m
        | Some v ->
          try
            let old = K.Map.find k m in
            if V.equal old v then
              m
            else begin
              P.L.warning "@[option %s:@ '%a' previously bound to '%a';@ \
now bound to '%a'.@]"
                X.option_name K.pretty k V.pretty old V.pretty v;
              K.Map.add k v m
            end
          with Not_found ->
            K.Map.add k v m
      let remove (k, _v) m = K.Map.remove k m
      let iter f m = K.Map.iter (fun k v -> f (k, Some v)) m
      let fold f m acc = K.Map.fold (fun k v -> f (k, Some v)) m acc
      let reorder = Extlib.id

      exception Found of V.t
      let of_singleton_string =
	let r = Str.regexp "\\([^:]\\|^\\):\\([^:]\\|$\\)" in
        (* delimiter is no more than 3 characters long, the first belonging to
           the element before it, the third belonging to the element after it.
           Treats :: as part of a word to be able to handle C++ function names
           in a non too awkward manner.
         *)
        let split_delim d = (* handle different possible lenght of the delimiter *)
          let rbis = Str.regexp ":" in
	  match Str.bounded_full_split rbis d 2 with
          | [ Str.Delim _] -> (empty_string, empty_string)
          | [ Str.Delim _; Str.Text t2 ] -> (empty_string, t2)
          | [ Str.Text t1; Str.Delim _; ] -> (t1, empty_string)
          | [ Str.Text t1; Str.Delim _; Str.Text t2 ] -> (t1, t2)
          | _ -> (* impossible case *)
            raise (Cannot_build ("delimiter="^d))
        in
	let k_of_singleton_string = 
	  if (K.of_singleton_string==no_element_of_string) 
	  then (fun x -> K.Set.singleton (K.of_string x)) 
	  else K.of_singleton_string
	in
        fun s ->
          let (keys, value) =
            let get_pairing k v_opt =
              let keys = k_of_singleton_string k in
              let key = ref None in
              let prev =
                try
                  K.Set.iter
                    (fun k ->
                      key := Some k;
                    (* choose any previous value, whatever it is:
                       don't know which clear semantics one would like *)
                      try raise (Found (!find_ref k)) with Not_found -> ())
                    keys;
                  (* assume there is always at least a key *)
                  None
                with Found v ->
                  Some v
              in
              match !key with
              | None -> K.Set.empty, None
              | Some key -> keys, of_val ~key k ~prev v_opt
            in
            match Str.bounded_full_split r s 2 with
            | ([] | [ Str.Text _ ]) ->  (* no delimiter ':' *)
              get_pairing s None
            | [ Str.Delim d ] ->
              let (f,s) = split_delim d in
              get_pairing f (Some s)
            | [ Str.Delim d; Str.Text t ] ->
              let (f,s) = split_delim d in
              get_pairing f (Some (s ^ t))
            | [ Str.Text t1; Str.Delim d; Str.Text t2 ] ->
              let (f,s) = split_delim d in
              get_pairing (t1 ^ f) (Some (s ^ t2))
            | [ Str.Text t; Str.Delim d] ->
              let (f,s) = split_delim d in
              get_pairing (t ^ f) (Some s)
            | _ -> (* by definition of [Str.bounded_full_split]: *)
              assert false
          in
          K.Set.fold (fun key map -> add (key, value) map) keys K.Map.empty
    end

    module S = struct

      include State_builder.Option_ref
        (K.Map.Make(V))
        (struct
          let name = X.option_name ^ " map"
          let dependencies = []
         end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(Pair)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let find k = K.Map.find k (get ())
    let mem k = K.Map.mem k (get ())
    let () = find_ref := (fun k -> K.Map.find k (get_nomemo ()))

  end

  module String_map = Make_map(String_for_collection)

  module Kernel_function_map
    (V: Parameter_sig.Value_datatype with type key = kernel_function)
    (X: sig
      include Parameter_sig.Input_with_arg
      val default: V.t Cil_datatype.Kf.Map.t
    end) =
  struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_map(Kernel_function_string(A))(V)(X)

  end

  module Make_multiple_map
    (K: Parameter_sig.String_datatype_with_collections)
    (V: Parameter_sig.Multiple_value_datatype with type key = K.t)
    (X: sig
      include Parameter_sig.Input_with_arg
      val default: V.t list K.Map.t
    end) =
  struct

    type key = K.t
    type value = V.t

    let find_ref = ref (fun _ -> assert false)

    let of_val ~key k ~prev v =
      try V.of_string ~key ~prev v
      with Cannot_build s ->
        cannot_build
          (Pretty_utils.sfprintf "@[value bound to '%s':@ %s@]" k s)

    module Pair = struct
      include Datatype.Pair(K)(Datatype.List(V))

      let of_string =
        let r = Str.regexp_string ":" in
        fun s -> match Str.split_delim r s with
        | [] -> cannot_build ("cannot interpret '" ^ s ^ "'")
        | k :: l ->
          let key = K.of_string k in
          let prev = try Some (!find_ref key) with Not_found -> None in
          let l = match l with
            | [] ->
              (match of_val ~key k ~prev None with
              | None -> []
              | Some v -> [ v ])
            | _ :: _ ->
              List.fold_right (* preserve order *)
                (fun v acc -> match of_val ~key k ~prev (Some v) with
                | None -> acc
                | Some v -> v :: acc)
                l
                []
          in
          key, l

      let to_string (key, l) =
        Pretty_utils.sfprintf "%s%t"
          (K.to_string key)
          (fun fmt ->
            let rec pp_custom_list = function
              | [] -> ()
              | v :: l ->
                Extlib.may
                  (fun v -> Format.fprintf fmt ":%s" v)
                  (V.to_string ~key (Some v));
                pp_custom_list l
            in
            pp_custom_list l)
    end

    module C = struct
      type t = V.t list K.Map.t
      let equal = K.Map.equal (List.for_all2 V.equal)
      let empty = K.Map.empty
      let is_empty = K.Map.is_empty
      let add (k, l) m =
        try
          let l' = K.Map.find k m in
          K.Map.add k (l @ l') m
        with Not_found ->
          K.Map.add k l m
      let remove (k, _) m = K.Map.remove k m
      let iter f m = K.Map.iter (fun k l -> f (k, l)) m
      let fold f m acc = K.Map.fold (fun k v -> f (k, v)) m acc
      let reorder = Extlib.id

      exception Found of V.t list

      let of_singleton_string =
        let r = Str.regexp "[^:]:[^:]" in
        let split_delim d =
          (Pervasives_string.sub d 0 1, Pervasives_string.sub d 2 1)
        in
        let remove_none_and_rev l =
          List.fold_left
            (fun acc v -> match v with None -> acc | Some v -> v :: acc)
            []
            l
        in
        let rec parse_values ~key k ~prev acc s = function
          | [] -> remove_none_and_rev (of_val ~key k ~prev (Some s) :: acc)
          | [Str.Text t] ->
            remove_none_and_rev (of_val ~key k ~prev (Some (s ^ t)) :: acc)
          | Str.Text t :: Str.Delim d :: l ->
            let (suf, pre) = split_delim d in
            let v = of_val ~key k ~prev (Some (s ^ t ^ suf)) in
            parse_values ~key k ~prev (v :: acc) pre l
          | Str.Delim d :: l ->
            let (suf,pre) = split_delim d in
            let v = of_val ~key k ~prev (Some (s ^ suf)) in
            parse_values ~key k ~prev (v :: acc) pre l
          | Str.Text _ :: Str.Text _ :: _ ->
            (* By construction, there must be a Delim between two consecutive
               Text in the value returned by full_split *)
            assert false
        in
        fun s ->
          let (keys, values) =
            let get_pairing k v l =
              let keys = K.of_singleton_string k in
              let key = ref None in
              let prev =
                try
                  K.Set.iter
                    (fun k ->
                      key := Some k;
                      (* choose any previous value, whatever it is:
                         don't know which clear semantics one would like *)
                      try raise (Found (!find_ref k)) with Not_found -> ())
                    keys;
                  None
                with Found v ->
                  Some v
              in
              match !key with
              | None -> K.Set.empty, []
              | Some key -> keys, parse_values ~key k ~prev [] v l
            in
            match Str.full_split r s with
            | [] -> cannot_build ("cannot interpret '" ^ s ^ "'")
            | [Str.Text t] -> K.of_singleton_string t, []
            | Str.Delim d :: l ->
              let (f,s) = split_delim d in
              get_pairing f s l
            | Str.Text t :: Str.Delim d :: l ->
              let (f,s) = split_delim d in
              get_pairing (t ^ f) s l
            | Str.Text _ :: Str.Text _ :: _ -> (* see above *) assert false
          in
          K.Set.fold (fun key map -> K.Map.add key values map) keys K.Map.empty
    end

    module S = struct

      include State_builder.Option_ref
        (K.Map.Make(Datatype.List(V)))
        (struct
          let name = X.option_name ^ " map"
          let dependencies = []
         end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(Pair)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let find k = K.Map.find k (get ())
    let mem k = K.Map.mem k (get ())
    let () = find_ref := (fun k -> K.Map.find k (get_nomemo ()))

  end

  module String_multiple_map = Make_multiple_map(String_for_collection)

  module Kernel_function_multiple_map
    (V: Parameter_sig.Multiple_value_datatype with type key = kernel_function)
    (X: sig
      include Parameter_sig.Input_with_arg
      val default: V.t list Cil_datatype.Kf.Map.t
    end) =
  struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_multiple_map(Kernel_function_string(A))(V)(X)

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
compile-command: "make -C ../../.."
End:
*)

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

(* ************************************************************************* *)
(** {2 Handling group of parameters} *)
(* ************************************************************************* *)

let selection : (State.t * bool) list ref = ref []
let no_reset_selection: (State.t * bool) list ref = ref []

let get_selection_gen ?(is_set=true) selection =
  let l =
    if is_set then
      List.map fst selection
    else
      List.fold_left
        (fun acc (x, b) -> if b then acc else x :: acc)
        []
        selection
  in
  State_selection.of_list l

let get_selection ?is_set () = get_selection_gen ?is_set !selection

let get_selection_context ?is_set () =
  let has_dependencies s =
    State_dependency_graph.G.out_degree State_dependency_graph.graph s > 0
  in
  (* automatically select all options which have some dependencies:
     they have an impact on some analysis. *)
  let states =
    State_selection.fold
      (fun s acc -> if has_dependencies s then s :: acc else acc)
      (get_selection ?is_set ())
      []
  in
  State_selection.of_list states

let get_reset_selection ?is_set () =
  let all = get_selection ?is_set () in
  let no_reset = get_selection_gen ?is_set !no_reset_selection in
  State_selection.diff all no_reset

let extend_selection is_set s = selection := (s, is_set) :: !selection

let extend_no_reset_selection is_set s =
  no_reset_selection := (s,is_set) :: !no_reset_selection

(* ************************************************************************* *)
(** {2 Generic implementation} *)
(* ************************************************************************* *)

module Make
  (P: sig val shortname: string end)
  (X: sig
    include Datatype.S
    val default: unit -> t
    val option_name: string
    val functor_name: string
   end) =
struct

  let is_dynamic = true
  let projectify = !Parameter_customize.projectify_ref
  let reset_on_copy = !Parameter_customize.reset_on_copy_ref
  let must_save = !Parameter_customize.must_save_ref
  let is_visible = !Parameter_customize.is_visible_ref
  let module_name = !Parameter_customize.module_name_ref
  let group = !Parameter_customize.group_ref
  let stage = !Parameter_customize.cmdline_stage_ref

  let () = match !Parameter_customize.cmdline_stage_ref with
    | Cmdline.Early | Cmdline.Extending | Cmdline.Extended
    | Cmdline.Exiting | Cmdline.Loading ->
        Parameter_customize.do_not_projectify ()
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
          if X.option_name = "" then "Input C files" else X.option_name
       end)

  module D = Datatype
  include Internal_state

  type t = Internal_state.data
  let () = 
    extend_selection false self;
    if not reset_on_copy then extend_no_reset_selection false self

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
    extend_selection true Is_set.self;
    if not reset_on_copy then extend_no_reset_selection true self

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
    if !Parameter_customize.journalize_ref then
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
        ~plugin:""
        (Dynamic.Parameter.get_name X.functor_name name X.option_name)
        ~journalize:false
        ty
        f
    else
      f

  let get, set, unsafe_set, clear, is_set, is_default =
    register_dynamic "get" D.unit X.ty Internal_state.get,
    register_dynamic "set" X.ty D.unit set,
    register_dynamic "unsafe_set" X.ty D.unit unsafe_set,
    register_dynamic "clear" D.unit D.unit clear,
    register_dynamic "is_set" D.unit D.bool Is_set.get,
    register_dynamic "is_default" D.unit D.bool is_default

  let option_name = X.option_name

  let add_aliases = 
    Cmdline.add_aliases option_name ~plugin:P.shortname ~group stage

  let print_help fmt =
    Cmdline.print_option_help fmt ~plugin:P.shortname ~group option_name

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

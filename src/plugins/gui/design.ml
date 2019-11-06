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

(** Main GUI skeleton *)
open Cil_types
open Cil_datatype
open Cil
open Pretty_source
open Gtk_helper

let dkey = Gui_parameters.register_category "design"
let dkey_scroll = Gui_parameters.register_category "scroll"

let use_external_viewer = false

class type reactive_buffer = object
  inherit error_manager
  method buffer : GSourceView.source_buffer
  method locs : Pretty_source.Locs.state
  method rehighlight: unit
  method redisplay: unit
end

class type view_code = object
  method scroll : Pretty_source.localizable -> unit
  method view_stmt : stmt -> unit
  method view_original_stmt : stmt -> location
  method view_original : location -> unit
  method display_globals : global list -> unit
  method select_or_display_global : global -> unit
end

class type main_window_extension_points = object
  inherit Launcher.basic_main
  inherit view_code
  method toplevel : main_window_extension_points
  method menu_manager: unit -> Menu_manager.menu_manager
  method file_tree : Filetree.t
  method file_tree_view : GTree.view
  method annot_window : Wtext.text
  method pretty_information: 'a. ?scroll:bool -> ('a, Format.formatter, unit) format -> 'a
  (** Pretty print a message in the [annot_window]. *)

  method launcher : unit -> unit
  method source_viewer : GSourceView.source_view
  method source_viewer_scroll : GBin.scrolled_window
  method display_globals : global list -> unit
  method register_source_selector :
    (GMenu.menu GMenu.factory -> main_window_extension_points -> button:int
     -> Pretty_source.localizable -> unit)
    -> unit
  method register_source_highlighter :
    (reactive_buffer -> localizable ->
     start:int -> stop:int -> unit)
    -> unit
  method register_panel :
    (main_window_extension_points ->
     (string * GObj.widget *(unit-> unit) option)) -> unit
  method rehighlight : unit -> unit
  method redisplay : unit -> unit
  method reactive_buffer: reactive_buffer
  method original_source_viewer : Source_manager.t
  method reset : unit -> unit
  method error : 'a.
    ?parent:GWindow.window_skel -> ?reset:bool ->
    ('a, Format.formatter, unit) format ->
    'a
  method push_info : 'a. ('a, Format.formatter, unit) format -> 'a
  method pop_info : unit -> unit
  method show_ids: bool
  method help_message : 'a 'b.
    (<event : GObj.event_ops ; .. > as 'a)
    -> ('b, Format.formatter, unit) format
    -> 'b
  method lower_notebook : GPack.notebook
end

(** The list of registered extension *)
let (handlers:(main_window_extension_points -> unit) list ref) = ref []
(** Insert an extension *)
let register_extension f =
  handlers := f::!handlers
(** Apply all extensions *)
let process_extensions window =
  List.iter (fun f -> f window) (List.rev !handlers)

(** The list of reset extensions.
    Such extensions are used for example when the current project
    is changed. *)
let (reset_handlers:(main_window_extension_points -> unit) list ref) = ref []

(** Insert a reset extension *)
let register_reset_extension f =
  reset_handlers := f::!reset_handlers

(** Apply all reset extensions *)
let reset_extensions window =
  List.iter (fun f -> f window) (List.rev !reset_handlers)

(** Memoization of the class used to display a list of globals inside
    a GTK source view (reactive_buffer) *)
module Globals_GUI = struct
  include Hashtbl.Make
      (struct
        type t = global list
        let equal x y =
          try List.for_all2 (==) x y
          with Invalid_argument _ -> false
        let hash = Hashtbl.hash
      end)
  let tbl: reactive_buffer t = create 17
  let find k = find tbl k

  let add k = add tbl k

  let clear () = clear tbl
end

let filetree_selector
    (main_ui:main_window_extension_points)
    ~was_activated
    ~activating
    globals
  =
  (*Format.printf "filetree selector:%b@." (not was_activated && activating);*)
  if not was_activated && activating then begin
    let source = main_ui#source_viewer in
    (match globals with
     | Filetree.File (f, l) ->
       Source_manager.load_file
         main_ui#original_source_viewer
         ~filename:f ~line:1
         ~click_cb:(fun _ ->
             (* original_source callback unnecessary here *) ()) ();
       main_ui#display_globals l
     | Filetree.Global (GVarDecl (vi, _)) ->
       (* try to find a definition instead of a declaration, which is more
          informative. *)
       main_ui#display_globals [Ast.def_or_last_decl vi]
     | Filetree.Global g ->
       main_ui#display_globals [g];
    );
    source#scroll_to_mark ~use_align:true ~xalign:0. ~yalign:0.5 `INSERT;
    let print_one_global prefix (v,loc) =
      main_ui#protect ~cancelable:false
        (fun () ->
           main_ui#view_original loc;
           main_ui#pretty_information "%s '%s'@." prefix v.vname)
    in
    main_ui#annot_window#clear;
    begin match globals with
      | Filetree.Global g ->
        begin
          History.push (History.Global g);
          match g with
          | GFun ({svar=v},loc) -> print_one_global "Function" (v,loc)
          | GVar (v,_,loc) -> print_one_global "Variable" (v,loc)
          | GVarDecl (v, loc) -> print_one_global "Variable" (v,loc)
          | GFunDecl (_, v, loc) ->
            print_one_global "Declared function" (v,loc)
          | _ -> () (* cannot currently happen, we do not display the
                       other globals in the filetree *)
        end
      | Filetree.File (f, globals) ->
        let max_length = 40 in
        let cons_limit r g l = if !r >= max_length then l else (incr r;g::l) in
        let gfun_c,gtyp_c,gcomptagdecl_c,genumtagdecl_c,gvardecl_c,gvar_c=
          ref 0,ref 0,ref 0,ref 0,ref 0,ref 0
        in
        let (gfun,gtype,gcomp,genum,gvardecl,gvar) =
          List.fold_right
            (fun g (gfun,gtype,gcomp,genum,gvardecl,gvar) ->
               match g with
               | GFun _ ->
                 (cons_limit gfun_c g gfun,gtype,gcomp,genum,gvardecl,gvar)
               | GFunDecl _ ->
                 (cons_limit gfun_c g gfun,gtype,gcomp,genum,gvardecl,gvar)
               | GType _ -> (gfun,cons_limit gtyp_c g gtype,gcomp,genum,gvardecl,gvar)
               | GCompTagDecl _ ->
                 (gfun,gtype,cons_limit gcomptagdecl_c g gcomp,genum,gvardecl,gvar)
               | GEnumTagDecl _ ->
                 (gfun,gtype,gcomp,cons_limit genumtagdecl_c g genum,gvardecl,gvar)
               | GVarDecl _ ->
                 (gfun,gtype,gcomp,genum,cons_limit gvardecl_c g gvardecl,gvar)
               | GVar _ ->
                 (gfun,gtype,gcomp,genum,gvardecl,cons_limit gvar_c g gvar)
               | _ -> (gfun,gtype,gcomp,genum,gvardecl,gvar))
            globals
            ([],[],[],[],[],[])
        in
        main_ui#pretty_information "@[File %a@]@." Datatype.Filepath.pretty f;
        let printing
            (head:string)
            (ellipsis:bool)
            (f:Format.formatter -> 'a -> unit)
            (l:'a list)
          =
          if l <> [] then
            main_ui#pretty_information "@[%s @[<hov>%a@]%s@]@\n" head
              (Pretty_utils.pp_list ~sep:",@ " f) l
              (if ellipsis then "..." else "")
        in
        printing
          "Functions:"
          (!gfun_c>=max_length)
          (fun fmt -> (function GFun ({svar=v},_) | GFunDecl (_, v, _) ->
               Varinfo.pretty fmt v
                              | _ -> assert false))
          gfun;
        printing
          "Types:"
          (!gtyp_c>=max_length)
          (function fmt -> (function (GType ({tname=name},_)) ->
               Format.pp_print_string fmt name
                                   | _ -> assert false))
          gtype;
        printing
          "Composite types:"
          (!gcomptagdecl_c>=max_length)
          (function fmt ->
             (function  GCompTagDecl
                 ({cname=name},_) |GCompTag ({cname=name},_)->
               Format.pp_print_string fmt name
                      | _ -> assert false))
          gcomp;
        printing
          "Enums:"
          (!genumtagdecl_c>=max_length)
          (function fmt ->
             (function GEnumTagDecl
                 ({ename=name},_) | GEnumTag ({ename=name},_)->
               Format.pp_print_string fmt name
                     |_ -> assert false))
          genum;
        printing
          "Declared variables:"
          (!gvardecl_c>=max_length)
          (function fmt ->
             (function GVarDecl (v,_) ->
                Varinfo.pretty fmt v
                     | _ -> assert false))
          gvardecl;
        printing
          "Variables:"
          (!gvar_c>=max_length)
          (fun fmt -> (function GVar(v,_,_) ->
               Varinfo.pretty fmt v
                              | _ -> assert false))
          gvar;
        main_ui#pretty_information "%!"
    end
  end

let pretty_predicate_status fmt p =
  if Property.has_status p then
    let s = Property_status.get p in
    Format.fprintf fmt "Status: %a@." Property_status.pretty s

(* This is called when a localizable is selected in the pretty-printed source
   buffer *)
let to_do_on_real_select _menu
    (main_ui:main_window_extension_points)
    ~button
    selected
  =
  History.push (History.Localizable selected);
  if button = 1 then begin
    main_ui#annot_window#clear;
  end

(* Returns a pair (name, callback), where [name] is the name of the selected
   global variable, function or label, and [callback] executes a jump to the
   definition of [selected], if it is possible. Otherwise, returns [None]. *)
let go_to_definition selected main_ui =
  match selected with
  | PLval (_kf, _ki, lv) ->
    begin
      match lv with
      | Var vi, _ when vi.vsource && vi.vglob ->
        let typ = Cil.typeOfLval lv in
        let glob =
          if Cil.isFunctionType typ
          then Kernel_function.get_global (Globals.Functions.get vi)
          else GVarDecl (vi, Location.unknown)
        in
        let name =
          Pretty_utils.escape_underscores
            (Format.asprintf "%a" Varinfo.pretty vi)
        in
        Some (name, fun () -> ignore (main_ui#select_or_display_global glob))
      | _ -> None (* cannot go to definition *)
    end
  | PStmt (kf,{skind = Goto (stmt, _)}) ->
    begin
      match !stmt.labels with
      | Label (lbl, _, _) :: _ ->
        let name = Pretty_utils.escape_underscores lbl in
        Some (name, fun () -> ignore (main_ui#scroll (PStmt (kf, !stmt))))
      | _ -> None
    end
  | _ -> None (* cannot go to definition *)

(* Print the code annotations on the given statement *)
let print_code_annotations (main_ui:main_window_extension_points) kf stmt =
  Annotations.iter_code_annot
    (fun e a ->
       let kind =
         if Emitter.equal e Emitter.end_user then
           "user annotation"
         else match Alarms.find a with
           | Some _ -> "alarm"
           | None ->
             Format.asprintf "emitted by %a" Emitter.pretty e
       in
       main_ui#pretty_information "@[%s: @[<hov>%a@]@]@.%a@."
         kind Printer.pp_code_annotation a
         (Pretty_utils.pp_list ~sep:"@\n" pretty_predicate_status)
         (Property.ip_of_code_annot kf stmt a))
    stmt

(* When the statement is a call, print the statuses of the preconditions
   of the called functions at this specific call site. *)
let print_call_preconditions (main_ui: main_window_extension_points) stmt =
  let by_ptr_call = match stmt.skind with
    | Instr (Call (_, e, _, _)) -> Some (Kernel_function.get_called e = None)
    | Instr (Local_init (_, ConsInit _, _)) -> Some false
    | _ -> None
  in
  match by_ptr_call with
  | None -> () (* Not a call *)
  | Some by_ptr ->
    let called_at = Statuses_by_call.all_functions_with_preconditions stmt in
    let aux_callee kf =
      let warn_missing = false in
      let l= Statuses_by_call.all_call_preconditions_at ~warn_missing kf stmt in
      let pp_kf fmt =
        if by_ptr then
          Format.fprintf fmt "of %a:@ " Kernel_function.pretty kf
      in
      let aux_prop (orig, copy) =
        main_ui#pretty_information "@[Precondition %t%a@.%a@]@."
          pp_kf Property.pretty orig pretty_predicate_status copy
      in
      List.iter aux_prop l
    in
    Kernel_function.Hptset.iter aux_callee called_at

(* This is called when a localizable is selected in the pretty-printed source
   buffer, and also when a localizable is clicked on in the information panel *)
let to_do_on_select
    (menu_factory:GMenu.menu GMenu.factory)
    (main_ui:main_window_extension_points)
    ~button
    selected
  =
  let view_original ?loc stmt =
    Gui_parameters.debug ~dkey:dkey_scroll
      "view_original: %a, stmt id %d"
      (Pretty_utils.pp_opt ~none:"None" Printer.pp_location) loc
      stmt.sid;
    match loc with
    | None -> main_ui#view_original_stmt stmt
    | Some loc -> main_ui#view_original loc; loc
  in
  let current_statement_msg ?loc kf stmt =
    main_ui#pretty_information
      "Function: %t@."
      (fun fmt -> match kf with
         | None -> Format.pp_print_string fmt "<none>"
         | Some kf -> Kernel_function.pretty fmt kf);
    match stmt with
    | Kglobal -> main_ui#pretty_information "@."
    | Kstmt s ->
      let loc = view_original ?loc s in
      if main_ui#show_ids then
        main_ui#pretty_information
          "Statement: %d (%a)@.@." s.sid Printer.pp_location loc
      else main_ui#pretty_information "Line %a@.@." Printer.pp_location loc
  in
  let pp_decl fmt loc =
    if Cil_datatype.Location.equal loc Cil_datatype.Location.unknown then ()
    else Format.fprintf fmt " (declared at %a)" Printer.pp_location loc
  in
  let formal_or_local vi =
    if vi.vformal then "formal parameter" else "local variable"
  in
  let pp_defining_fun fmt vi =
    match Kernel_function.find_defining_kf vi with
    | None -> ()
    | Some kf -> Format.fprintf fmt " of function %a" Kernel_function.pretty kf
  in
  let pp_var_with_decl fmt vi =
    if vi.vglob then
      Format.fprintf fmt "%sglobal variable%a"
        (if vi.vsource then "" else "generated ")
        pp_decl vi.vdecl
    else
      Format.fprintf fmt "%s%s%a%a"
        (if vi.vsource then "" else "generated ")
        (formal_or_local vi) pp_defining_fun vi pp_decl vi.vdecl
  in
  if button = 1 then begin
    let open Property in match selected with
    | PStmtStart _ -> ()
    | PStmt (kf, stmt) ->
      current_statement_msg (Some kf) (Kstmt stmt);
      print_code_annotations main_ui kf stmt;
      print_call_preconditions main_ui stmt;
    | PIP (IPCodeAnnot {ica_kf;ica_stmt;ica_ca} as ip) ->
      current_statement_msg
        ?loc:(Cil_datatype.Code_annotation.loc ica_ca)
        (Some ica_kf) (Kstmt ica_stmt);
      if main_ui#show_ids then
        main_ui#pretty_information "Code annotation id: %d@." ica_ca.annot_id;
      main_ui#pretty_information "%a@." pretty_predicate_status ip
    | PIP(IPAllocation _ as ip) ->
      main_ui#pretty_information "This is an allocation clause@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP(IPAssigns _ as ip) ->
      main_ui#pretty_information "This is an assigns clause@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP(IPFrom _ as ip) ->
      main_ui#pretty_information "This is a from clause@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKRequires _} as ip) ->
      main_ui#pretty_information "This is a requires clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPExtended {ie_ext={ext_name}} as ip) ->
      main_ui#pretty_information "This clause is a %s extension.@.%a@."
        ext_name pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKTerminates} as ip) ->
      main_ui#pretty_information "This is a terminates clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKEnsures (_,Normal)} as ip) ->
      main_ui#pretty_information "This is an ensures clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKEnsures (_,Exits)} as ip) ->
      main_ui#pretty_information "This is an exits clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKEnsures (_,Returns)} as ip) ->
      main_ui#pretty_information "This is a returns clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKEnsures (_,Breaks)} as ip) ->
      main_ui#pretty_information "This is a breaks clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKEnsures (_,Continues)} as ip) ->
      main_ui#pretty_information "This is a continues clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPPredicate {ip_kind = PKAssumes _} as ip) ->
      main_ui#pretty_information "This is an assumes clause.@.";
      main_ui#view_original (location ip)
    | PIP (IPDecrease {id_kinstr=Kglobal} as ip) ->
      main_ui#pretty_information
        "This is a decreases clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP (IPDecrease {id_kinstr=Kstmt _} as ip) ->
      main_ui#pretty_information
        "This is a loop variant.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP(IPDisjoint _ as ip) ->
      main_ui#pretty_information
        "This is a disjoint behaviors clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP(IPComplete _ as ip) ->
      main_ui#pretty_information
        "This is a complete behaviors clause.@.%a@."
        pretty_predicate_status ip;
      main_ui#view_original (location ip)
    | PIP(IPAxiom _ as ip) ->
      main_ui#pretty_information "This is an axiom.@.";
      main_ui#view_original (location ip)
    | PIP(IPAxiomatic _ as ip) ->
      main_ui#pretty_information "This is an axiomatic.@.";
      main_ui#view_original (location ip)
    | PIP(IPLemma _ as ip) ->
      main_ui#pretty_information "This is a lemma.@.";
      main_ui#view_original (location ip)
    | PIP(IPTypeInvariant _ as ip) ->
      main_ui#pretty_information "This is a type invariant.@.";
      main_ui#view_original (location ip)
    | PIP(IPGlobalInvariant _ as ip) ->
      main_ui#pretty_information "This is a global invariant.@.";
      main_ui#view_original (location ip)
    | PIP(IPBehavior _ as ip) ->
      main_ui#pretty_information "This is a behavior.@.";
      main_ui#view_original (location ip)
    | PIP (IPPropertyInstance {ii_ip=ip'} as ip) ->
      main_ui#pretty_information "@[This is an instance of property `%a'.@]@."
        short_pretty ip';
      main_ui#view_original (location ip)
    | PIP(IPReachable _ | IPOther _) ->
      (* these properties are not selectable *)
      assert false
    | PGlobal _g -> main_ui#pretty_information "This is a global.@.";

    | PLval (kf, ki,lv) ->
      let ty = typeOfLval lv in
      if isFunctionType ty then begin
        begin
          match ki with
          | Kstmt s -> ignore (view_original s)
          | Kglobal -> ();
        end;
        main_ui#pretty_information "This is a C function of type `%a'@."
          Gui_printers.pp_typ ty
      end
      else begin
        current_statement_msg kf ki;
        match lv with
        | Var vi,NoOffset ->
          main_ui#pretty_information
            "Variable %a has type `%a'.@\nIt is a %a.@\n\
             %tIt is %sreferenced and its address is %staken.@."
            Varinfo.pretty vi
            Gui_printers.pp_typ vi.vtype
            pp_var_with_decl vi
            (fun fmt ->
               match vi.vdescr with
               | None -> ()
               | Some s ->
                 Format.fprintf fmt
                   "This is a temporary variable for \"%s\".@\n" s)
            (if vi.vreferenced then "" else "not ")
            (if vi.vaddrof then "" else "not ")
        | _ ->
          let typ = typeOfLval lv in
          main_ui#pretty_information "This is an lvalue of type %a@."
            Gui_printers.pp_typ typ
      end

    | PExp (_kf, _ki, e) ->
      begin
        let typ = typeOf e in
        match constFoldToInt e with
        | Some i ->
          begin match e.enode with
            | Const (CEnum {eihost}) ->
              let typ_enum = TEnum (eihost, []) in
              main_ui#pretty_information
                "This is a C enumeration constant, \
                 defined in %a with a value of %a.@."
                Gui_printers.pp_typ typ_enum Abstract_interp.Int.pretty i
            | _ ->
              main_ui#pretty_information
                "This is a constant C expression of type %a, equal to %a.@."
                Gui_printers.pp_typ typ Abstract_interp.Int.pretty i
          end
        | None ->
          main_ui#pretty_information "This is a pure C expression of type %a.@."
            Gui_printers.pp_typ typ
      end
    | PTermLval (_, _, ip, tlv) ->
      main_ui#pretty_information "This is a logical left-value, \
                                  of logic type %a.@."
        Printer.pp_logic_type (Cil.typeOfTermLval tlv);
      main_ui#view_original (Property.location ip)

    | PVDecl (kf,_,vi) ->
      if vi.vglob
      then begin
        main_ui#view_original (Global.loc (Ast.def_or_last_decl vi));
        main_ui#pretty_information
          "This is the last declaration or definition of %s %a.@\n\
           It is %sreferenced and its address is %staken.@."
          (if Cil.isFunctionType vi.vtype
           then "function" else "global variable")
          Varinfo.pretty vi
          (if vi.vreferenced then "" else "not ")
          (if vi.vaddrof then "" else "not ")
      end else begin
        main_ui#view_original vi.vdecl;
        let kf = Extlib.the kf in
        main_ui#pretty_information
          "This is the declaration of %s %a in function %a%t@."
          (formal_or_local vi) Varinfo.pretty vi
          Kernel_function.pretty kf
          (fun fmt -> match vi.vdescr with None -> ()
                                         | Some s ->  Format.fprintf fmt
                                                        "@\nThis is a temporary variable for \"%s\".@." s)
      end
  end
  else if button = 3 then begin
    match go_to_definition selected main_ui with
    | None -> () (* no menu to show *)
    | Some (escaped_name, callback) ->
      ignore (menu_factory#add_item
                ("Go to definition of " ^ escaped_name)
                ~callback)
  end


module Feedback =
struct

  module F = Property_status.Feedback

  let category = function
    | F.Never_tried -> "never_tried"
    | F.Considered_valid -> "considered_valid"
    | F.Valid -> "surely_valid"
    | F.Invalid -> "surely_invalid"
    | F.Invalid_but_dead -> "invalid_but_dead"
    | F.Valid_but_dead -> "valid_but_dead"
    | F.Unknown_but_dead -> "unknown_but_dead"
    | F.Unknown -> "unknown"
    | F.Valid_under_hyp -> "valid_under_hyp"
    | F.Invalid_under_hyp -> "invalid_under_hyp"
    | F.Inconsistent -> "inconsistent"

  let long_category = function
    | F.Never_tried -> "Never tried: no status is available for this property"
    | F.Considered_valid -> "Considered valid: this is a hypothesis that shall be verified outside Frama-C"
    | F.Valid -> "Surely valid: verified (including all of its dependencies)"
    | F.Invalid -> "Surely invalid: refuted (and all of its dependencies have been verified)"
    | F.Invalid_but_dead -> "Invalid but dead: refuted, but unreachable"
    | F.Valid_but_dead -> "Valid but dead: verified, but unreachable"
    | F.Unknown_but_dead -> "Unknown but dead: unknown status, and unreachable"
    | F.Unknown -> "Unknown: a verification has been attempted, but without conclusion"
    | F.Valid_under_hyp -> "Valid under hypotheses: verified (but has dependencies with Unknown status)"
    | F.Invalid_under_hyp -> "Invalid under hypotheses: refuted (but has dependencies with Unknown status)"
    | F.Inconsistent -> "Inconsistent: got both true and false statuses (possibly cyclic dependencies, or an incorrect axiomatization)"

  (* Two extra categories are used to add folding or unfolding icons on call
     sites with preconditions. *)
  let fold_category = "fold"
  let unfold_category = "unfold"

  (*GTK3 does not exist anymore in gsourceview3. *)
  let declare_markers (source:GSourceView.source_view) =
    GSourceView.make_marker_attributes
      ~source ~category:fold_category ~priority:2
      ~pixbuf:(Gtk_helper.Icon.(get Fold)) ();
    GSourceView.make_marker_attributes
      ~source ~category:unfold_category ~priority:2
      ~pixbuf:(Gtk_helper.Icon.(get Unfold)) ();
    List.iter
      (fun v ->
         GSourceView.make_marker_attributes
           ~source ~category:(category v) ~priority:1
           ~pixbuf:(Gtk_helper.Icon.get (Gtk_helper.Icon.Feedback v)) ())
      [ F.Never_tried;
        F.Considered_valid;
        F.Valid;
        F.Invalid;
        F.Invalid_but_dead;
        F.Valid_but_dead;
        F.Unknown;
        F.Unknown_but_dead;
        F.Valid_under_hyp;
        F.Invalid_under_hyp;
        F.Inconsistent ]

  (* tooltip marks are recreated whenever the buffer changes *)
  let tooltip_marks : (int, string) Hashtbl.t = Hashtbl.create 8

  (* Binds the line of a callsite to the corresponding statement.
     Used to fold or unfold preconditions at a call site when the user clicks
     on the bullet (we need to retrieve the statement from the line clicked). *)
  let call_sites : (int, stmt) Hashtbl.t = Hashtbl.create 8

  let clear_tables () =
    Hashtbl.clear tooltip_marks;
    Hashtbl.clear call_sites

  let mark (source:GSourceView.source_buffer) ?call_site ~offset validity =
    let iter = source#get_iter_at_char offset in
    let mark = iter#set_line_offset 0 in
    let category = category validity in
    source#remove_source_marks mark mark () ;
    ignore (source#create_source_mark ~category mark) ;
    Hashtbl.replace tooltip_marks iter#line (long_category validity);
    match call_site with
    | None -> ()
    | Some stmt ->
      Hashtbl.replace call_sites iter#line stmt;
      if Pretty_source.are_preconds_unfolded stmt
      then ignore (source#create_source_mark ~category:fold_category mark)
      else ignore (source#create_source_mark ~category:unfold_category mark)

end


(** Widgets that might result in a localizable being selected:
    - the main ui reactive buffer (pretty-printed source)
    - the information panel, when the user clicks on a localizable *)
type localizable_selection_origin = ReactiveBuffer | InformationPanel

(** Global selectors and highlighters *)
let highlighter = ref []
let selector = ref ([] :
                      ((GMenu.menu GMenu.factory -> main_window_extension_points ->
                        button:int -> Pretty_source.localizable -> unit
                       ) * localizable_selection_origin list) list)

class protected_menu_factory (host:Gtk_helper.host) (menu:GMenu.menu) = object
  inherit [GMenu.menu] GMenu.factory menu as super

  method! add_item ?key ?callback ?submenu string =
    let callback = match callback with
        None -> None
      | Some cb ->
        Some (fun () -> ignore (host#full_protect ~cancelable:true cb))
    in
    super#add_item ?key ?callback ?submenu string

  method! add_check_item ?active ?key ?callback string =
    let callback = match callback with
        None -> None
      | Some cb ->
        Some (fun b ->
            ignore (host#full_protect ~cancelable:false (fun () -> cb b)))
    in
    super#add_check_item ?active ?key ?callback string

end

(* This function reacts to the section of a localizable. The [origin]
   arguments identifies the widget where the selection occurred *)
let selector_localizable (main_ui:main_window_extension_points) origin ~button localizable =
  let popup_factory =
    new protected_menu_factory (main_ui:>Gtk_helper.host) (GMenu.menu())
  in
  List.iter
    (fun (f, origins) ->
       if List.mem origin origins then
         f popup_factory main_ui ~button localizable
    )
    !selector;
  if button = 3 && popup_factory#menu#children <> [] then
    let time = GtkMain.Main.get_current_event_time () in
    popup_factory#menu#popup ~button ~time


class reactive_buffer_cl (main_ui:main_window_extension_points)
    ?(parent_window=main_ui#main_window)
    globs :reactive_buffer  =
  let buffer = Source_viewer.buffer () in
  let locs = Pretty_source.Locs.create () in
  object(self)
    inherit error_manager
        ~reset:main_ui#reset (parent_window:>GWindow.window_skel)
    method buffer = buffer
    method locs = locs
    method rehighlight = Pretty_source.hilite locs
    method redisplay = self#init

    method private init =
      Feedback.clear_tables ();
      let highlighter localizable ~start ~stop =
        List.iter (fun f -> f (self:>reactive_buffer) localizable ~start ~stop) !highlighter
      in
      let selector = selector_localizable main_ui ReactiveBuffer in
      Pretty_source.display_source
        globs
        buffer
        ~host:(self:>Gtk_helper.host)
        ~highlighter
        ~selector
        locs;
      self#rehighlight

    initializer
      self#init;
      Globals_GUI.add globs (self:> reactive_buffer)
  end

(* This is a dummy instance of [reactive_buffer], used to bootstrap the
   creation of the main window of the GUI. *)
let dummy_reactive_buffer (parent_window:GWindow.window) =
  let buffer = Source_viewer.buffer () in
  let locs = Pretty_source.Locs.create () in
  object
    inherit error_manager (parent_window:>GWindow.window_skel)
    method buffer = buffer
    method locs = locs
    method rehighlight = ()
    method redisplay = ()
  end


let reactive_buffer main_ui ?parent_window globs  =
  try
    Globals_GUI.find globs
  with Not_found ->
    new reactive_buffer_cl main_ui ?parent_window globs


(* Reference to the view used by the stdout console, to enable use of Ctrl+F. *)
let console_view : GText.view option ref = ref None

(** The main application window *)
class main_window () : main_window_extension_points =
  let final_w,width = try true,Configuration.find_int "window_width"
    with Not_found -> false,(Gdk.Screen.width ())*7/8
  in
  let final_h,height =try true,Configuration.find_int "window_height"
    with Not_found -> false,(Gdk.Screen.height ())*7/8
  in
  let max_width = (* maximum width for this height *)
    height * 8 / 5 (* 16/10 ratio *)
  in
  let width, height =
    if width > max_width
    then (if final_w then width else max_width), height
    else
      let max_height = width * 3 / 4 in
      let new_height = min height max_height in
      width, if final_h then height else new_height
  in
  let main_window =
    Gtk_compat.window
      ?icon:framac_icon
      ~title:"Frama-C"
      ~position:`CENTER
      ~resizable:true
      ~show:false
      ()
  in
  let () = main_window#set_default_size ~width ~height in
  let () = main_window#set_geometry_hints ~min_size:(1,1) main_window#coerce in
  let watch_cursor = Gdk.Cursor.create `WATCH in
  let arrow_cursor = Gdk.Cursor.create `ARROW in

  (* On top one finds the menubar *)
  let toplevel_vbox = GPack.box `VERTICAL ~packing:main_window#add () in

  (* toplevel_vbox->*bottom_hbox *)
  let bottom_hbox = GPack.box `HORIZONTAL
      ~packing:(toplevel_vbox#pack ~expand:false ~fill:false ~from:`END)
      ()
  in
  (* status bar (at bottom) *)
  (* toplevel_vbox->bottom_hbox-> *statusbar *)
  let statusbar = GMisc.statusbar ~packing:bottom_hbox#add () in
  let status_context = statusbar#new_context "messages" in

  (* progress bar (at bottom) *)
  (* toplevel_vbox->bottom_hbox-> [statusbar;*progress_bar] *)
  let progress_bar =
    GRange.progress_bar
      ~pulse_step:0.01
      ~packing:(bottom_hbox#pack ~fill:false)
      ()
  in

  (* Split below the bars *)
  (* toplevel_vbox->[*toplevel_hpaned;bottom_hbox] *)
  let toplevel_hpaned = GPack.paned `HORIZONTAL
      ~packing:(toplevel_vbox#pack ~expand:true ~fill:true ~from:`END) ()
  in
  (* Save the handle ratio whenever it is changed *)
  let _ = toplevel_hpaned#event#connect#button_release
      ~callback:(fun _ -> save_paned_ratio "toplevel_hpaned" toplevel_hpaned;
                  false)
  in

  let filetree_panel_vpaned =
    GPack.paned `VERTICAL ~packing:(toplevel_hpaned#add1) ()
  in
  let _ = filetree_panel_vpaned#event#connect#button_release
      ~callback:(fun _ ->
          save_paned_ratio "filetree_panel_vpaned" filetree_panel_vpaned;
          false)
  in

  (* The left filetree inside an automatic scrolled window and a nice frame *)
  let filetree_frame =
    GBin.frame ~shadow_type:`ETCHED_OUT ~packing:filetree_panel_vpaned#add1 ()
  in
  let filetree_scrolled_window =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:filetree_frame#add ()
  in
  let file_tree_view = GTree.view ~packing:filetree_scrolled_window#add () in
  let () = file_tree_view#misc#set_name "file tree" in
  let () = file_tree_view#selection#set_mode `SINGLE in
  let () = file_tree_view#set_rules_hint true in
  let () = file_tree_view#set_headers_clickable true in

  (* splits between messages and sources *)
  let vb_message_sources =
    GPack.paned `VERTICAL  ~border_width:3 ~packing:toplevel_hpaned#add2 ()
  in
  let _ = vb_message_sources#event#connect#button_release
      ~callback:(fun _ ->
          save_paned_ratio "vb_message_sources" vb_message_sources;
          false)
  in

  (* splits between messages and sources *)
  let hb_sources =
    GPack.paned `HORIZONTAL  ~border_width:3 ~packing:vb_message_sources#add1 ()
  in
  (* Save the handle ratio whenever it is changed *)
  let _ = hb_sources#event#connect#button_release
      ~callback:(fun _ -> save_paned_ratio "hb_sources" hb_sources; false)
  in
  (* lower notebook *)

  let fr2 =
    GBin.frame ~shadow_type:`ETCHED_OUT  ~packing:vb_message_sources#add2 ()
  in
  let lower_notebook =
    GPack.notebook ~scrollable:true ~show_tabs:true ~packing:fr2#add ()
  in

  (* lower text view and its scroll view: annotations and messages *)
  let tab_label = GMisc.label ~markup:"Information" () in
  let annot_sw =
    GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC
      ~packing:(fun w -> ignore (lower_notebook#insert_page
                                   ~tab_label:tab_label#coerce w))
      ()
  in
  let annot_window = new Wtext.text () in
  let () = annot_sw#add_with_viewport annot_window#coerce in
  let () = Printer.update_printer
      (module Gui_printers.LinkPrinter: Printer.PrinterExtension)
  in
  (* upper text view: source code *)
  let fr1 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:hb_sources#add1 () in

  let source_viewer_scroll = GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:fr1#add ()
  in

  let source_viewer = Source_viewer.make ~packing:source_viewer_scroll#add () in
  let () =
    begin
      source_viewer#set_show_line_numbers false ;
      source_viewer#set_show_line_marks true ;
      let _ =
        source_viewer#event#connect#motion_notify ~callback:
          (fun ev ->
             let x = GdkEvent.Motion.x ev in
             if x < 20.0 (* roughly the width of the left bar *) then begin
               let y = GdkEvent.Motion.y ev in
               let (xbuf, ybuf) = source_viewer#window_to_buffer_coords
                   ~tag:`WIDGET ~x:(int_of_float x) ~y:(int_of_float y)
               in
               let iterpos = source_viewer#get_iter_at_location xbuf ybuf in
               let line = iterpos#line in
               if Hashtbl.mem Feedback.tooltip_marks line then begin
                 let text = Hashtbl.find Feedback.tooltip_marks line in
                 source_viewer#misc#set_has_tooltip true;
                 source_viewer#misc#set_tooltip_text text;
               end else begin
                 source_viewer#misc#set_has_tooltip false;
               end
             end else
               source_viewer#misc#set_has_tooltip false;
             ; false)
      in
      Feedback.declare_markers source_viewer ;
    end
  in

  let original_source_viewer = Source_manager.make ~packing:hb_sources#add2 ()
  in

  let () =
    (* Remove default pango menu (cut/paste, etc) for original source textview*)
    ignore (source_viewer#event#connect#button_press ~callback:
              (fun ev -> GdkEvent.Button.button ev = 3));
    (* startup configuration *)
    source_viewer#buffer#place_cursor ~where:source_viewer#buffer#start_iter
  in
  let original_reactive_buffer = dummy_reactive_buffer main_window in
  object (self:#main_window_extension_points)
    val mutable launcher = []
    val mutable panel = []
    val mutable main_window_metrics = { Gtk.width=0; height=0; x=0; y=0}
    val mutable file_tree = None
    val mutable current_buffer_state: reactive_buffer  = original_reactive_buffer
    val mutable menu_manager = None

    (* Stores the last text inserted into the "Find text" field. *)
    val mutable last_find_text = ""

    method toplevel = (self:>main_window_extension_points)
    method main_window = main_window
    method menu_manager () = match menu_manager with
      | None ->
        (* toplevel_vbox->[*self#menu_manager();toplevel_hpaned;bottom_hbox] *)
        let m =
          new Menu_manager.menu_manager
            ~packing:(toplevel_vbox#pack ~expand:false ~fill:false ~from:`START)
            ~host:(self :> Gtk_helper.host)
        in
        menu_manager <- Some m;
        m
      | Some s ->
        s
    method file_tree = Extlib.the file_tree
    method file_tree_view = file_tree_view
    method annot_window = annot_window

    method pretty_information : 'a. ?scroll:bool -> ('a, Format.formatter, unit) format -> 'a
      = annot_window#printf

    method source_viewer = source_viewer
    method source_viewer_scroll = source_viewer_scroll

    method private register_source_selector_origin origins f =
      selector := (f, origins)::!selector
    method register_source_selector f =
      self#register_source_selector_origin [InformationPanel; ReactiveBuffer] f

    method register_source_highlighter f = highlighter := f::!highlighter
    method register_panel f = panel <- f::panel

    method private initialize_panels () =
      let to_refresh = ref [] in
      let sw =
        GBin.scrolled_window
          ~vpolicy:`AUTOMATIC
          ~hpolicy:`AUTOMATIC
          ~packing:filetree_panel_vpaned#add2
          ()
      in
      let vbox = GPack.vbox ~packing:sw#add_with_viewport () in
      let targets = [
        { Gtk.target =
            "application/x" ; Gtk.flags = [] ; Gtk.info = 0 }]
      in
      let dragged_frame = ref None in
      List.iter
        (fun f ->
           let text,widget,refresh = f (self:>main_window_extension_points) in
           let key_config = text in
           let expander = GBin.expander
               ~expanded:(Configuration.find_bool ~default:true key_config)
               ~packing:vbox#pack () in
           let label_hb = GPack.hbox () in
           let _label = GMisc.label
               ~markup:("<b>"^text^"</b>")
               ~packing:label_hb#pack
               ()
           in
           expander#set_label_widget (label_hb#coerce);
           ignore (expander#connect#activate
                     (fun () -> (* Save expansion of panels*)
                        Configuration.set key_config
                          (Configuration.ConfBool (not expander#expanded))));
           let frame = GBin.frame ~packing:expander#add () in
           frame#add widget;

           (* Drag stuff *)
           expander#drag#source_set ~modi:[`BUTTON1] ~actions:[`MOVE] targets;
           ignore (expander#drag#connect#beginning
                     (fun _ -> dragged_frame:=Some (frame,text)));
           ignore (expander#drag#connect#ending (fun _ -> dragged_frame:=None));

           (* Refreshers *)
           Extlib.may
             (fun refresh ->
                to_refresh:=
                  (fun ()->
                     if !Gtk_helper.gui_unlocked && expander#expanded then
                       refresh ())
                  ::!to_refresh)
             refresh)
        panel;

      (* Drop machinery *)
      let dropper (widget:GObj.widget) =
        widget#drag#dest_set ~flags:[`ALL] ~actions:[`MOVE] targets;
        ignore (widget#drag#connect#drop
                  (fun drag_context ~x:_ ~y:_ ~time:_ ->
                     match !dragged_frame with
                     | None (* Not dropping a panel *) -> true
                     | Some (frame,title) ->
                       (*Format.printf "Hello %d %d %ld@." x y time;*)
                       let w = drag_context#source_widget in
                       let new_w =
                         GWindow.window ~position:`MOUSE ~title ~show:true () in
                       let b = GPack.vbox ~packing:new_w#add () in
                       frame#misc#reparent b#coerce;
                       ignore (new_w#connect#destroy
                                 (fun () ->
                                    frame#misc#reparent w;
                                    w#misc#show ()));
                       w#misc#hide ();
                       true));
        ignore (widget#drag#connect#motion
                  (fun drag_context ~x:_ ~y:_ ~time ->
                     (*Format.printf "Motion %d %d %ld@." x y time;*)
                     drag_context#status ~time (Some `MOVE);
                     true));
        ignore (widget#drag#connect#leave
                  (fun drag_context ~time ->
                     (*Format.printf "Motion %d %d %ld@." x y time;*)
                     drag_context#status ~time (Some `MOVE)));
      in
      dropper main_window#coerce;
      dropper source_viewer#coerce;

      let refresh_all _ = (List.iter (fun f -> f ()) !to_refresh;true) in
      ignore (Glib.Timeout.add ~ms:500 ~callback:refresh_all)

    method launcher () =
      Launcher.show
        ~width:(try Configuration.find_int "launcher_width"
                with Not_found -> main_window_metrics.Gtk.width/2)
        ~height:(try Configuration.find_int "launcher_height"
                 with Not_found -> 2*main_window_metrics.Gtk.height/3)
        ~host:(self:>Launcher.basic_main)
        ()

    method original_source_viewer = original_source_viewer
    method reactive_buffer = current_buffer_state

    method display_globals globs =
      Gui_parameters.debug ~dkey "display_globals";
      let buff = reactive_buffer self#toplevel globs in
      current_buffer_state <- buff;
      self#source_viewer#set_buffer (buff#buffer:>GText.buffer);
      self#rehighlight () (* This should not be needed, but for some reason
                             gtk does not highlight the buffer by default *)

    (* Cf .mli doc. In the first case, the callbacks of the filetree are called,
       but not in the second case.  As of 2011-05-16, the only callback is
       registered here (in  design.ml) and calls filetree_selector *)
    method select_or_display_global g =
      if not (self#toplevel#file_tree#select_global g) then
        filetree_selector self#toplevel
          ~was_activated:false ~activating:true (Filetree.Global g)

    method redisplay () =
      current_buffer_state#redisplay;
      History.show_current ()

    method rehighlight () =
      current_buffer_state#rehighlight ;

      (* General idea: if there is a current buffer AND [loc] is inside,
         scroll to [loc]. Otherwise, open a relevant buffer by finding a
         varinfo or a global for [loc], then scroll to [loc]. *)
    method scroll loc =
      Gui_parameters.debug ~dkey:dkey_scroll
        "main_ui: scroll: localizable %a" Printer_tag.Localizable.pretty loc;
      (* Used to avoid having two different history events, one created
         by [select_global], the other by [scroll] *)
      let history = History.on_current_history () in
      (* [current_buffer_state] contains [loc], [o] is the offset,
         let's scroll to it *)
      let show o =
        history (fun () -> History.push (History.Localizable loc));
        let iter = self#source_viewer#buffer#get_iter (`OFFSET o) in
        Gui_parameters.debug
          ~dkey:dkey_scroll "scrolling in current view at iter %d,%d"
          iter#line iter#line_offset
        ;
        ignore (self#source_viewer#backward_display_line_start iter);
        self#source_viewer#buffer#place_cursor iter;
        ignore (self#source_viewer#scroll_to_mark
                  ~use_align:true ~yalign:0.5 ~xalign:0. `INSERT);
        let adj = source_viewer_scroll#hadjustment in
        adj#set_value adj#lower
      in
      match Pretty_source.locate_localizable current_buffer_state#locs loc with
      | Some (b,_) -> show b
      | None ->
        (* Searching in [current_buffer_state] did not work, let's try
           to open a good one *)
        begin
          match Pretty_source.kf_of_localizable loc with
          | Some kf ->
            let g = Kernel_function.get_global kf in
            self#select_or_display_global g
          | None ->
            match loc with
            | PGlobal g -> self#select_or_display_global g
            | _ ->
              Gui_parameters.debug ~dkey "does not know how to scroll to loc"
              (* In this case, there is nothing we can do: we do not
                   know which file/global to open to scroll in *)
        end;
        match Pretty_source.locate_localizable current_buffer_state#locs loc with
        | Some (b, _) -> show b
        | None ->
          (* Can appear eg. for an if (i<5) inside a loop,
             which is not shown in general in the source code *)
          Gui_parameters.debug ~dkey "Unable to scroll to loc, probably \
                                      not shown in the buffer"

    method view_stmt stmt =
      let kf = Kernel_function.find_englobing_kf stmt in
      let loc = PStmt (kf, stmt) in
      self#scroll loc;
      ignore (self#view_original_stmt stmt)

    method view_original loc =
      Gui_parameters.debug ~dkey:dkey_scroll
        "main_ui: view_original: location %a" Location.pretty loc;
      if not (Location.equal loc Location.unknown) then
        Source_manager.load_file
          self#original_source_viewer
          ~filename:(fst loc).Filepath.pos_path
          ~line:(fst loc).Filepath.pos_lnum
          ~click_cb:(fun olocz ->
              match olocz with
              | None -> ()
              | Some locz ->
                let scroll_to_locz locz =
                  Wutil.later (fun () ->
                      (* Prevent filetree selector from resetting the
                         original source viewer. *)
                      Source_manager.selection_locked := true;
                      self#scroll locz;
                      (* The selection lock is asynchronously released by a
                         callback, and cannot be released here. *)
                    )
                in
                match locz with
                | PVDecl (_okf, _, vi) -> begin
                    (* if it is a global variable, show it instead of the
                       current function *)
                    try
                      ignore (Globals.Vars.find vi);
                      let glob = GVarDecl (vi, loc) in
                      Wutil.later (fun () ->
                          Source_manager.selection_locked := true;
                          self#select_or_display_global glob;
                        )
                    with
                    | Not_found ->
                      (* not a global variable, treat as usual *)
                      scroll_to_locz locz
                  end
                | PGlobal g -> begin
                    (* if it is a type declaration/definition, ignore it, since
                       types are not displayed in the file tree *)
                    match g with
                    | GType _ | GCompTag _ | GCompTagDecl _ | GEnumTag _
                    | GEnumTagDecl _ -> ()
                    | _ -> scroll_to_locz locz
                  end
                | _ -> scroll_to_locz locz
            )
          ()

    method view_original_stmt st =
      let loc = Stmt.loc st in
      if use_external_viewer then begin
        if not (Location.equal loc Location.unknown) then
          let args_for_emacs =
            Format.sprintf "emacsclient -n +%d %S"
              (fst loc).Filepath.pos_lnum ((fst loc).Filepath.pos_path :> string)
              (*          Format.sprintf "mate -a -l %d %s" line file  *)
          in
          Gui_parameters.debug ~dkey "Running %s" args_for_emacs;
          ignore (Sys.command args_for_emacs);
      end else
        self#view_original loc;
      loc

    method pop_info ()  =
      status_context#pop ();

    method private push_info_buffer :
      'a. ?buffer:Buffer.t -> ('a, Format.formatter, unit) format -> 'a =
      fun ?buffer fmt ->
      let b = match buffer with
        | None -> Buffer.create 80
        | Some b -> b
      in
      let bfmt = Format.formatter_of_buffer b  in
      Format.kfprintf
        (function fmt ->
           Format.pp_print_flush fmt ();
           let content = Buffer.contents b in
           ignore (status_context#push content))
        bfmt
        fmt

    method push_info fmt = self#push_info_buffer fmt

    method show_ids = Gui_parameters.debug_atleast 1

    method help_message w fmt =
      let buffer = Buffer.create 80 in
      let bfmt = Format.formatter_of_buffer buffer  in
      Format.kfprintf
        (function _ ->
           ignore (w#event#connect#leave_notify
                     (fun _ -> self#pop_info ();true));
           ignore (w#event#connect#enter_notify
                     (fun _ ->
                        Format.pp_print_flush bfmt ();
                        self#push_info_buffer ~buffer "" ;false)))
        bfmt
        fmt

    inherit error_manager (main_window:>GWindow.window_skel)

    (* These private method might be exported when necessary *)
    method private toplevel_vbox = toplevel_vbox
    method private toplevel_hpaned = toplevel_hpaned
    method private statusbar = statusbar

    method lower_notebook = lower_notebook
    method private reset_no_extensions () =
      Gui_parameters.debug ~dkey "Redisplaying gui";
      Globals_GUI.clear ();
      current_buffer_state <- original_reactive_buffer;
      self#file_tree#reset ();
      (self#menu_manager ())#refresh ()

    method reset () =
      self#reset_no_extensions ();
      reset_extensions self#toplevel;
      if History.is_empty () then (
        self#default_screen ())
      else
        History.show_current ()

    method private default_screen () =
      try
        (* If some files have been specified on the command-line, we try
           to find the main (if possible a definition, not a prototype),
           and display it *)
        let main, _ = Globals.entry_point () in
        self#select_or_display_global (Kernel_function.get_global main)
      with Globals.No_such_entry_point _ | Not_found ->
        source_viewer#buffer#set_text
          "Please select a file in the left panel\nor start a new project."

    (* Performs a forward text search on the currently focused element,
       starting at its current cursor position.
       Selects the next occurrence of the searched text (if found),
       otherwise displays a message saying it was not found.
       If [use_dialog] is true, displays a dialog asking for the text
       to be found (e.g. Ctrl+F). Otherwise, uses the last searched
       text (e.g. F3). *)
    method private focused_find_text use_dialog =
      let find_text_in_viewer ~where (viewer : [`GTextViewer of GText.view |`GSourceViewer of GSourceView.source_view]) text =
        let buffer, scroll_to_iter =
          match viewer with
          | `GTextViewer v -> v#buffer,v#scroll_to_iter
          | `GSourceViewer v -> v#buffer,v#scroll_to_iter
        in
        let cursor_iter = buffer#get_iter_at_mark `INSERT in
        let after_cursor = cursor_iter#forward_char in
        let notify_not_found = ref true in (* to avoid redundant 'not found' *)
        let found_iters =
          match
            after_cursor#forward_search
              ~flags:[]
              text
          with
          | Some _ as iters -> iters
          | None ->
            let title = "Find " ^ where in
            (* try to wrap search if user wishes *)
            if GToolbox.question_box ~title
                (Printf.sprintf "No more occurrences for: %s\n\
                                 Search from beginning?" text)
                ~buttons:["Yes"; "No"] = 1 (*yes*) then
              let cursor_iter = buffer#get_iter `START in
              (* note: may end up searching twice some parts of the buffer *)
              cursor_iter#forward_search
                ~flags:[]
                text
            else
              (notify_not_found := false; None)
        in
        match found_iters with
        | Some (i1,i2) ->
          buffer#place_cursor i1;
          buffer#select_range i1 i2;
          ignore (scroll_to_iter i1
                    ~use_align:false
                    ~within_margin:0.025
                 );
          last_find_text <- text
        | None -> if !notify_not_found then
            GToolbox.message_box ~title:("Not found " ^ where)
              (Printf.sprintf "Not found %s: %s" where text)
      in
      let focused_widget = GtkWindow.Window.get_focus main_window#as_window in
      let focused_name = Gobject.Property.get focused_widget GtkBase.Widget.P.name
      in
      let opt_where_view =
        if focused_name = "source" then
          Some ("in CIL code", `GSourceViewer source_viewer)
        else if focused_name = "original_source" then
          let original_buffer = (Source_manager.get_current_source_view
                                   original_source_viewer)
          in
          Some ("in original code", `GSourceViewer original_buffer)
        else if focused_name = "file tree" then
          begin
            let text =
              if use_dialog then
                Extlib.opt_conv ""
                  (Gtk_helper.input_string
                     ~parent:main_window
                     ~title:"Find global" ~ok:"Find" ~cancel:"Cancel"
                     "Find global:" ~text:last_find_text)
              else last_find_text
            in
            if text <> "" then
              match self#file_tree#find_visible_global text with
              | None -> GToolbox.message_box ~title:"Global not found"
                          (Printf.sprintf "Global not found: %s" text)
              | Some g ->
                last_find_text <- text;
                self#select_or_display_global g
            else ();
            None (* indicates that we are done processing the command *)
          end
        else
          begin
            let information_view = annot_window#get_view in
            if Gobject.Property.get information_view#as_widget
                GtkBase.Widget.P.has_focus then
              Some ("in Information",`GTextViewer information_view)
            else
              let console_view_focused =
                match !console_view with
                | Some v ->
                  if Gobject.Property.get v#as_widget GtkBase.Widget.P.has_focus then
                    Some ("in Console",`GTextViewer v)
                  else
                    None
                | None -> None
              in
              if console_view_focused <> None then console_view_focused
              else
                (* TODO: add more places where text can be searched *)
                None
          end
      in
      match opt_where_view with
      | None -> (* no searchable focused element, or already processed *) ()
      | Some (where,viewer) ->
        let text =
          if use_dialog then
            Extlib.opt_conv ""
              (Gtk_helper.input_string
                 ~parent:main_window
                 ~title:("Find " ^ where) ~ok:"Find" ~cancel:"Cancel"
                 ("Find text (" ^ where ^ "):") ~text:last_find_text)
          else last_find_text
        in
        if text <> "" then find_text_in_viewer ~where viewer text else ()

    initializer
      self#set_reset self#reset;
      let menu_manager = self#menu_manager () (* create the menu_manager *) in
      main_window#add_accel_group menu_manager#factory#accel_group;

      (* When the user clicks on the bullet of a call site, folds or unfolds the
         preconditions at this call site. The relative position of an event is
         relative to the innermost widget at the given position, regardless of
         the widget on which the event is bound. The same coordinate can thus
         refer to different positions according to the reference widget. So we
         need to use absolute coordinate to precisely check where the click
         happened. *)
      let _ = source_viewer#event#connect#button_release
          ~callback:(fun ev ->
              (* Absolute x position of the event on the screen. *)
              let abs_x = int_of_float (GdkEvent.Button.x_root ev) in
              (* This function returns the absolute position of the top window,
                 or the relative position of an intern widget. *)
              let rec get_rel_from_main acc win =
                let x = fst (Gdk.Window.get_position win) in
                let acc = acc + x in
                let win = Gdk.Window.get_parent win in
                if Gobject.get_oid win =
                   Gobject.get_oid main_window#misc#window
                then acc
                else get_rel_from_main acc win
              in
              let get_x obj = fst (Gdk.Window.get_position obj#misc#window) in
              (* Absolute position of the main window on the screen. *)
              let window_abs_x = get_x main_window in
              (* Relative position of the source_viewer in the main windows. *)
              let viewer_rel_x =
                get_rel_from_main 0 source_viewer#misc#window
              in
              (* Width of the bullet column in the source viewer. *)
              if abs_x - (window_abs_x + viewer_rel_x) < 20 then
                begin
                  let x, y = GdkEvent.Button.(x ev, y ev) in
                  let (xbuf, ybuf) = source_viewer#window_to_buffer_coords
                      ~tag:`WIDGET ~x:(int_of_float x) ~y:(int_of_float y)
                  in
                  let iterpos = source_viewer#get_iter_at_location xbuf ybuf in
                  let line = iterpos#line in
                  try
                    let stmt = Hashtbl.find Feedback.call_sites line in
                    Pretty_source.fold_preconds_at_callsite stmt;
                    self#reset_no_extensions ();
                    (* give some time for the sourceview to recompute
                       its height, otherwise scrolling is broken. *)
                    let has_stabilized = ref false in
                    (* According to the blog post here
                       https://picheta.me/articles/2013/08/gtk-plus--a-method-to-guarantee-scrolling.html
                       the best way to check whether we have correctly scrolled
                       is to retrieve the rectangle corresponding to the mark,
                       the rectangle effectively displayed, and see whether
                       the former is included in the latter.
                    *)
                    let check () =
                      (* not entirely accurate because of
                         the (un)fold action, but should do the trick.
                         We will do the real scroll after stabilization
                         anyway.
                      *)
                      let iter =
                        source_viewer#buffer#get_iter (`LINE line)
                      in
                      let my_rect = source_viewer#get_iter_location iter in
                      let visible_rect = source_viewer#visible_rect in
                      (* in Gdk, x,y represents the top left corner of the
                         rectangle. We just check whether the beginning of the
                         selection is visible (we only have one line of text
                         anyway). *)
                      let res =
                        Gdk.Rectangle.(
                          y my_rect >= y visible_rect &&
                          y my_rect <= y visible_rect + height visible_rect
                        )
                      in
                      Gdk.Rectangle.(Gui_parameters.debug ~dkey:dkey_scroll
                                       "my  rect is %d (+%d) %d (+%d)@\n\
                                        vis rect is  %d (+%d) %d (+%d)@\n\
                                        my rect is visible: %B@."
                                       (x my_rect) (width my_rect) (y my_rect) (height my_rect)
                                       (x visible_rect) (width visible_rect) (y visible_rect)
                                       (height visible_rect) res);
                      has_stabilized := res;
                      (* when added as an idle procedure below, check will
                         be removed whenever it returns false. *)
                      not res
                    in
                    (* in case we were lucky and have stabilized directly. *)
                    ignore (check());
                    let proc = Glib.Idle.add check in
                    (* in case we are unlucky, stop waiting after
                       0.5 second and hope for the best. *)
                    let alarm =
                      Glib.Timeout.add
                        ~ms:500
                        ~callback:
                          (fun () ->
                             has_stabilized := true;
                             Glib.Idle.remove proc;
                             false)
                    in
                    while (not !has_stabilized) do
                      (* do one main loop step so that buffer gets
                         a chance to recompute its height. *)
                      ignore (Glib.Main.iteration false)
                    done;
                    Glib.Timeout.remove alarm;
                    self#view_stmt stmt;
                  with Not_found -> ()
                end;
              false)
      in
      let extra_accel_group = GtkData.AccelGroup.create () in
      GtkData.AccelGroup.connect extra_accel_group
        ~key:GdkKeysyms._F
        ~modi:[`CONTROL]
        ~callback:
          (fun _ -> self#focused_find_text true);
      (* Ctrl+F is bound to an action which opens a popup asking for a string,
         and then this string is searched in the text starting from the current
         position. *)
      GtkData.AccelGroup.connect extra_accel_group
        ~key:GdkKeysyms._F3
        ~callback:(fun _ -> self#focused_find_text false);
      (* F3 is bound to "Find again": searches the last string
         input with Ctrl+F without opening a popup window. *)
      main_window#add_accel_group extra_accel_group;
      let lock_gui lock =
        (* lock left part of the GUI. *)
        filetree_panel_vpaned#misc#set_sensitive (not lock);
        if lock then
          ignore (Glib.Timeout.add ~ms:50
                    ~callback:(fun () ->
                        progress_bar#pulse ();
                        not !Gtk_helper.gui_unlocked));
        Gdk.Window.set_cursor
          main_window#misc#window
          (if lock then watch_cursor else arrow_cursor);
        if lock then begin
          progress_bar#misc#show ();
          ignore (status_context#push "Computing")
        end
        else begin
          status_context#pop();
          progress_bar#misc#hide ()
        end
      in
      register_locking_machinery
        ~lock:(fun _cancelable -> lock_gui true)
        ~unlock:(fun () -> lock_gui false)
        ();

      ignore (main_window#connect#destroy ~callback:Cmdline.bail_out);
      (* Set the relative position for all paned whenever the main window is
         resized *)
      ignore (main_window#misc#connect#size_allocate
                (fun ({Gtk.width=w;Gtk.height=h} as rect) ->
                   Configuration.set "window_width" (Configuration.ConfInt w);
                   Configuration.set "window_height" (Configuration.ConfInt h);

                   if main_window_metrics.Gtk.width <> w
                   || main_window_metrics.Gtk.height <> h then
                     begin
                       place_paned hb_sources
                         (Configuration.find_float ~default:0.5 "hb_sources");
                       place_paned vb_message_sources
                         (Configuration.find_float ~default:0.71
                            "vb_message_sources");
                       place_paned filetree_panel_vpaned
                         (Configuration.find_float ~default:0.5
                            "filetree_panel_vpaned");
                       place_paned toplevel_hpaned
                         (Configuration.find_float ~default:0.18
                            "toplevel_hpaned");
                     end;
                   main_window_metrics <- rect));

      file_tree <- Some (Filetree.make file_tree_view);
      self#file_tree#add_select_function (filetree_selector self#toplevel);
      process_extensions self#toplevel;
      self#register_source_selector to_do_on_select;
      self#register_source_selector_origin [ReactiveBuffer] to_do_on_real_select;

      self#initialize_panels ();
      main_window#show ();
      Gdk.Window.set_cursor main_window#misc#window arrow_cursor;

      let warnings_tab_label = (GMisc.label ~text:"Messages" ())#coerce in
      let warning_manager =
        let packing w =
          ignore
            (lower_notebook#insert_page ~pos:1
               ~tab_label:warnings_tab_label w);
          let text = Format.sprintf "Messages (%d)" (Messages.nb_messages ()) in
          let label = GtkMisc.Label.cast warnings_tab_label#as_widget in
          GtkMisc.Label.set_text label text
        in
        let callback e _column =
          Extlib.may
            (fun pos ->
               Extlib.may self#scroll (Pretty_source.loc_to_localizable pos);
               (* Note: the code below generates double scrolling:
                  the previous call to self#scroll causes the original source
                  viewer to scroll to the beginning of the function, and then
                  the code below re-scrolls it to the exact location. *)
               self#view_original (pos,pos))
            e.Log.evt_source
        in
        Warning_manager.make ~packing ~callback
      in
      let display_warnings () =
        Messages.reset_once_flag ();
        Warning_manager.clear warning_manager;
        Messages.iter (fun event -> Warning_manager.append warning_manager event);
        let text = Format.sprintf "Messages (%d)" (Messages.nb_messages ()) in
        let label = GtkMisc.Label.cast warnings_tab_label#as_widget in
        GtkMisc.Label.set_text label text
      in
      display_warnings ();

      (* Management of navigation history *)
      ignore (History.create_buttons (self#menu_manager ()));
      History.set_display_elt_callback
        (function
          | History.Global g ->
            self#select_or_display_global g
          | History.Localizable l ->
            self#scroll l
        );

      register_reset_extension (fun _ -> display_warnings ());
      self#default_screen ();
      menu_manager#refresh ();
      Project.register_after_set_current_hook
        ~user_only:true
        (fun _ -> self#reset ());

      let pp_def_loc pp typ =
        try
          let opt_tag_name =
            match typ with
            | TNamed (ti, _) -> Some (Logic_typing.Typedef, ti.torig_name)
            | TComp (ci, _, _) ->
              let tag = if ci.cstruct then Logic_typing.Struct
                else Logic_typing.Union
              in
              let name = if ci.corig_name <> "" then ci.corig_name else ci.cname in
              Some (tag, name)
            | TEnum (ei, _) ->
              let name = if ei.eorig_name <> "" then ei.eorig_name else ei.ename in
              Some (Logic_typing.Enum, name)
            | _ -> None
          in
          match opt_tag_name with
          | None -> ()
          | Some (tag, name) ->
            let g = Globals.Types.global tag name in
            let loc = Cil_datatype.Global.loc g in
            Format.fprintf pp ", defined at %a" Printer.pp_location loc
        with
        | Not_found -> ()
      in

      annot_window#links#connect
        (fun button (_,_,s) ->
           begin
             try
               (* Retrieve a potential varinfo from the selection *)
               let vi = Gui_printers.varinfo_of_link s in
               (* Now that we have a varinfo, we re-synthesize a kinstr from
                  the current localizable, as it must be supplied to the callbacks *)
               match History.selected_localizable () with
               | None -> ()
               | Some loc ->
                 let kfopt = Pretty_source.kf_of_localizable loc in
                 let ki = Pretty_source.ki_of_localizable loc in
                 let var_localizable =
                   Pretty_source.PLval (kfopt, ki, (Var vi, NoOffset))
                 in
                 let button = GdkEvent.Button.button button in
                 if button = 1 then self#pretty_information "@.";
                 selector_localizable self#toplevel
                   InformationPanel ~button var_localizable
             with Gui_printers.NoMatch -> ()
           end;
           begin
             try
               (* Retrieve a potential typ from the selection *)
               let typ = Gui_printers.typ_of_link s in
               match typ with
               | TComp _ | TEnum _ | TPtr _ | TArray _ | TNamed _ ->
                 let base_type = Gui_printers.get_type_specifier typ in
                 let sizeof_str =
                   try Format.sprintf "sizeof %d" (Cil.bytesSizeOf base_type)
                   with Cil.SizeOfError (b, _) -> "unknown size: " ^ b
                 in
                 self#pretty_information ~scroll:true
                   "@.Type information for `%a':@.(%s%a)@. @[%a@]"
                   Printer.pp_typ base_type sizeof_str pp_def_loc typ
                   Gui_printers.pp_typ_unfolded base_type
               | _ -> () (* avoid printing anything for basic types;
                            also, function types are not supported *)
             with Gui_printers.NoMatch -> ()
           end;
           try
             let loc = Gui_printers.loc_of_link s in
             (* Retrieve a potential loc from the selection *)
             let modi = Gdk.Convert.modifier (GdkEvent.Button.state button) in
             let button = GdkEvent.Button.button button in
             if button = 1 then
               if List.mem `CONTROL modi then
                 (* Control-click: open current location using external viewer
                    (Emacs) *)
                 open_in_external_viewer (fst loc).Filepath.pos_path
                   ~line:(fst loc).Filepath.pos_lnum;
             self#view_original loc
           with Gui_printers.NoMatch -> ())
  end

let make_splash () =
  GMain.Rc.add_default_file (Config.datadir ^"/frama-c.rc");
  GMain.Rc.add_default_file (Config.datadir ^"/frama-c-user.rc");
  (*print_endline ("BOOT: " ^ (Glib.Main.setlocale `ALL None));*)
  let (_:string) = GtkMain.Main.init ~setlocale:false () in
  (*print_endline ("START: " ^ (Glib.Main.setlocale `ALL None));*)

  let w =
    GWindow.window
      ~title:"Splash" ~width:640 ~height:480 ~position:`CENTER_ALWAYS
      ~show:false ?icon:framac_icon
      ()
  in
  ignore(w#event#connect#key_press
           ~callback:(fun key ->
               if GdkEvent.Key.keyval key = GdkKeysyms._Escape then
                 Cmdline.bail_out (); false));
  let _ = w#event#connect#delete ~callback:(fun _ -> Cmdline.bail_out ()) in
  let tid =
    Glib.Timeout.add ~ms:500 ~callback:(fun () -> w#show (); false)
  in
  let bx = GPack.vbox ~packing:w#add () in
  let notebook = GPack.notebook ~packing:bx#add () in
  let close_button =
    GButton.button
      ~packing:(bx#pack ~expand:false ~fill:false) ~stock:`CANCEL ()
  in
  ignore (close_button#connect#released ~callback:Cmdline.bail_out);
  let reparent,stdout = Gtk_helper.make_text_page ~pos:2 notebook "Console" in
  console_view := Some stdout;
  Gtk_helper.log_redirector
    (fun s -> stdout#buffer#insert ~iter:stdout#buffer#end_iter s);
  let force () =
    Glib.Timeout.remove tid;
    w#show ()
  in
  tid, stdout, w, reparent, force

let toplevel play =
  Gtk_helper.Configuration.load ();
  Db.progress := Gtk_helper.refresh_gui;
  let in_idle () =
    let tid, splash_out, splash_w, reparent_console, force_s=  make_splash () in
    let error_manager =
      new Gtk_helper.error_manager (splash_w:>GWindow.window_skel)
    in
    let init_crashed = ref true in
    error_manager#protect
      ~cancelable:true ~parent:(splash_w:>GWindow.window_skel)
      (fun () ->
         (try
            play ();
            (* This is a good point to start using real asynchronous tasks
               management: plug-ins launched from command line have finished
               their asynchronous tasks thanks to the default Task.on_idle. *)
            Task.on_idle :=
              (fun f -> ignore (Glib.Timeout.add ~ms:50 ~callback:f));
            let project_name = Gui_parameters.Project_name.get () in
            if project_name = "" then
              Project.set_current_as_last_created ()
            else
              Project.set_current (Project.from_unique_name project_name);
            Ast.compute ()
          with e -> (* An error occurred: we need to enforce the splash screen
                       realization before we create the error dialog widget.*)
            force_s (); raise e);
         init_crashed := false);
    if Ast.is_computed () then
      (* if the ast has parsed, but a plugin has crashed, we display the gui *)
      error_manager#protect ~cancelable:false
        (fun () ->
           let main_ui = new main_window () in
           Gtk_helper.gui_unlocked := true;
           Glib.Timeout.remove tid;
           reparent_console main_ui#lower_notebook;
           splash_w#destroy ();
           (* Display the console if a crash has occurred. Otherwise, display
              the information panel *)
           if !init_crashed then
             (main_ui#lower_notebook#goto_page 2;
              (* BY TODO: this should scroll to the end of the console. It
                 does not work at all after the reparent, and only partially
                 before (scrollbar is wrong) *)
              let end_console = splash_out#buffer#end_iter in
              ignore (splash_out#scroll_to_iter ~yalign:0. end_console)
             )
           else
             main_ui#lower_notebook#goto_page 0
        )
  in
  ignore (Glib.Idle.add (fun () -> in_idle (); false));
  GMain.Main.main ()

let () = Db.Toplevel.run := toplevel

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

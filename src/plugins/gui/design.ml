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

(** Main GUI skeleton *)
open Cil_types
open Cil_datatype
open Cil
open Pretty_source
open Gtk_helper

let use_external_viewer = false

class type reactive_buffer = object
  inherit error_manager
  method buffer : GSourceView2.source_buffer
  method locs : Pretty_source.Locs.state option
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
  method annot_window : Toolbox.text
  method pretty_information: 'a. ?scroll:bool -> ('a, Format.formatter, unit) format -> 'a
    (** Pretty print a message in the [annot_window]. *)

  method launcher : unit -> unit
  method source_viewer : GSourceView2.source_view
  method source_viewer_scroll : GBin.scrolled_window
  method display_globals : global list -> unit
  method register_source_selector :
    (GMenu.menu GMenu.factory -> main_window_extension_points -> button:int
       -> Pretty_source.localizable -> unit)
    -> unit
  method register_source_highlighter :
    (GSourceView2.source_buffer -> localizable ->
       start:int -> stop:int -> unit)
    -> unit
  method register_panel :
    (main_window_extension_points ->
       (string * GObj.widget *(unit-> unit) option)) -> unit
  method rehighlight : unit -> unit
  method redisplay : unit -> unit
  method reactive_buffer: reactive_buffer option
  method original_source_viewer : Source_manager.t
  method reset : unit -> unit
  method error : 'a.
    ?parent:GWindow.window_skel -> ?reset:bool ->
    ('a, Format.formatter, unit) format ->
    'a
  method push_info : 'a. ('a, Format.formatter, unit) format -> 'a
  method pop_info : unit -> unit
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

(** Memoization of displayed globals *)
module Globals_GUI = struct
  include Hashtbl.Make
    (struct
       type t = global list
       let equal x y =
         try List.for_all2 (==) x y
         with Invalid_argument _ -> false
       let hash = Hashtbl.hash
     end)
  let tbl = create 17
  let find k =
    let r = find tbl k in
    r#rehighlight;
    r

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
        main_ui#pretty_information "@[File %s@]@." f;
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

(* Returns a pair (varinfo, callback), where [varinfo] is the selected
   global variable or function call, and [callback] executes a jump to the
   definition of [selected], if it is possible. Otherwise, returns [None]. *)
let go_to_definition selected main_ui =
    match selected with
    | PLval (_kf, _ki, lv) ->
      begin
        let ty = typeOfLval lv in
        match lv with
        | Var vi, NoOffset when isFunctionType ty ->
          (* only simple literal calls can be resolved syntactically *)
          let kf = Globals.Functions.get vi in
          let glob = Kernel_function.get_global kf in
          Some (vi, fun () -> ignore (main_ui#select_or_display_global glob))
        | Var vi, _ when not (isFunctionType ty) ->
          (try
             (* do nothing if the variable is not a global *)
             ignore (Globals.Vars.find vi);
             let g = GVarDecl (vi, Location.unknown) in
             Some (vi, fun () -> ignore (main_ui#select_or_display_global g))
           with Not_found -> None)
        | _ -> None (* cannot go to definition *)
      end
    | _ -> None (* cannot go to definition *)

(* This is called when a localizable is selected in the pretty-printed source
   buffer, and also when a localizable is clicked on in the information panel *)
let to_do_on_select
    (menu_factory:GMenu.menu GMenu.factory)
    (main_ui:main_window_extension_points)
    ~button
    selected
    =
  let current_statement_msg ?loc kf stmt =
    main_ui#pretty_information
      "Function: %t@."
      (fun fmt -> match kf with
         | None -> Format.pp_print_string fmt "<none>"
         | Some kf -> Kernel_function.pretty fmt kf);
    match stmt with
      | Kglobal -> ()
      | Kstmt ki ->
          let loc = match loc with
            | None -> main_ui#view_original_stmt ki
            | Some loc ->
                main_ui#view_original loc;
                loc
          in
          let skind =
            if Gui_parameters.debug_atleast 1 then
              match ki with
                | {skind=Block _} -> "Block "
                | {skind=Instr (Skip _)} -> "Skip "
                | _ -> ""
            else ""
          in
          main_ui#pretty_information
            "%sStatement: %d (line %d in %s)@."
            skind
            ki.sid
            (fst loc).Lexing.pos_lnum
            (Filepath.pretty (fst loc).Lexing.pos_fname)
  in
  let formal_or_local kf vi =
    if Kernel_function.is_formal vi kf
    then "formal parameter" else "local variable"
  in
  if button = 1 then begin
    match selected with
    | PStmt (kf, stmt) ->
      main_ui#protect
        ~cancelable:false
        (fun () ->
          current_statement_msg (Some kf) (Kstmt stmt);
          (* Code annotations for this statement *)
          Annotations.iter_code_annot
            (fun e a ->
              let pos, a = "Before", a in
              let user =
                if Emitter.equal e Emitter.end_user then "user" else "alarm"
              in
              main_ui#pretty_information "@[%s(%s): @[<hov>%a@]@]@.%a@."
                pos user Printer.pp_code_annotation a
                (Pretty_utils.pp_list ~sep:"@\n" pretty_predicate_status)
                (Property.ip_of_code_annot kf stmt a))
            stmt)
    | PIP (Property.IPCodeAnnot (kf,stmt,ca) as ip) ->
        current_statement_msg
          ?loc:(Cil_datatype.Code_annotation.loc ca) (Some kf) (Kstmt stmt);
        main_ui#pretty_information "Code annotation id: %d@.%a@." ca.annot_id
          pretty_predicate_status ip
    | PIP(Property.IPAllocation _ as ip) ->
        main_ui#pretty_information "This is an allocation clause@.%a@."
          pretty_predicate_status ip
    | PIP(Property.IPAssigns _ as ip) ->
        main_ui#pretty_information "This is an assigns clause@.%a@."
          pretty_predicate_status ip
    | PIP(Property.IPFrom _ as ip) ->
      main_ui#pretty_information "This is a from clause@.%a@."
        pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKRequires _,_,_,_) as ip) ->
        main_ui#pretty_information "This is a requires clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKTerminates,_,_,_) as ip) ->
        main_ui#pretty_information "This is a terminates clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKEnsures (_,Normal),_,_,_) as ip) ->
        main_ui#pretty_information "This is an ensures clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKEnsures (_,Exits),_,_,_) as ip) ->
        main_ui#pretty_information "This is an exits clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKEnsures (_,Returns),_,_,_) as ip) ->
        main_ui#pretty_information "This is a returns clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate (Property.PKEnsures (_,Breaks),_,_,_) as ip) ->
        main_ui#pretty_information "This is a breaks clause.@.%a@."
          pretty_predicate_status ip
    | PIP
        (Property.IPPredicate (Property.PKEnsures (_,Continues),_,_,_) as ip) ->
        main_ui#pretty_information "This is a continues clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPPredicate(Property.PKAssumes _,_,_,_)) ->
        main_ui#pretty_information "This is an assumes clause.@."
    | PIP (Property.IPDecrease (_,Kglobal,_,_) as ip) ->
        main_ui#pretty_information
          "This is a decreases clause.@.%a@."
          pretty_predicate_status ip
    | PIP (Property.IPDecrease (_,Kstmt _,_,_) as ip) ->
        main_ui#pretty_information
          "This is a loop variant.@.%a@."
          pretty_predicate_status ip
    | PIP(Property.IPDisjoint _ as ip) ->
        main_ui#pretty_information
          "This is a disjoint behaviors clause.@.%a@."
          pretty_predicate_status ip
    | PIP(Property.IPComplete _ as ip) ->
        main_ui#pretty_information
          "This is a complete behaviors clause.@.%a@."
          pretty_predicate_status ip
    | PIP(Property.IPAxiom _) ->
      main_ui#pretty_information "This is an axiom.@.";
    | PIP(Property.IPAxiomatic _) ->
      main_ui#pretty_information "This is an axiomatic.@.";
    | PIP(Property.IPLemma _) ->
      main_ui#pretty_information "This is a lemma.@.";
    | PIP(Property.IPTypeInvariant _) ->
      main_ui#pretty_information "This is a type invariant.@.";
    | PIP(Property.IPGlobalInvariant _) ->
      main_ui#pretty_information "This is a global invariant.@.";
    | PIP(Property.IPBehavior _) ->
      main_ui#pretty_information "This is a behavior.@.";
    | PIP(Property.IPReachable _ | Property.IPOther _
           | Property.IPPropertyInstance _) ->
      (* these properties are not selectable *)
      assert false
    | PGlobal _g -> main_ui#pretty_information "This is a global.@.";

    | PLval (kf, ki,lv) ->
      let ty = typeOfLval lv in
      if isFunctionType ty then
        main_ui#pretty_information "This is a C function of type `%a'@."
	  Gui_printers.pp_typ ty
      else begin
        current_statement_msg kf ki;
	match lv with
	| Var vi,NoOffset ->
          main_ui#pretty_information
            "Variable %a has type `%a'.@\nIt is a %s.@\n\
                 %tIt is %sreferenced and its address is %staken.@."
            Varinfo.pretty vi
            Gui_printers.pp_typ vi.vtype
            (if vi.vglob then "global variable"
             else formal_or_local (Extlib.the kf) vi)
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
	main_ui#pretty_information
	  "This is a constant C expression of type %a, equal to %a.@."
          Gui_printers.pp_typ typ Datatype.Integer.pretty i
      | None ->
	main_ui#pretty_information "This is a pure C expression of type %a.@."
          Gui_printers.pp_typ typ
      end
    | PTermLval (_, _, _, tlv) ->
        main_ui#pretty_information "This is a logical left-value, \
            of logic type %a.@." Printer.pp_logic_type (Cil.typeOfTermLval tlv)

    | PVDecl (kf,vi) ->
        main_ui#view_original vi.vdecl;
        if vi.vglob
        then
          main_ui#pretty_information
            "This is the declaration of %s %a.@\nIt is %sreferenced and \
             its address is %staken.@."
            (if Cil.isFunctionType vi.vtype
             then "function" else "global variable")
            Varinfo.pretty vi
            (if vi.vreferenced then "" else "not ")
            (if vi.vaddrof then "" else "not ")
        else
          let kf = Extlib.the kf in
          main_ui#pretty_information
            "This is the declaration of %s %a in function %a%t@."
            (formal_or_local kf vi) Varinfo.pretty vi
            Kernel_function.pretty kf
            (fun fmt -> match vi.vdescr with None -> ()
             | Some s ->  Format.fprintf fmt
                 "@\nThis is a temporary variable for \"%s\".@." s)
    end
  else if button = 3 then begin
    match go_to_definition selected main_ui with
    | None -> () (* no menu to show *)
    | Some (vi, callback) ->
      ignore (menu_factory#add_item
                ("Go to definition of " ^
                 (Pretty_utils.escape_underscores
                    (Pretty_utils.sfprintf "%a" Varinfo.pretty vi)))
                ~callback)
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
   arguments identifies the widget where the selection occured *)
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
object(self)
  inherit error_manager
    ~reset:main_ui#reset (parent_window:>GWindow.window_skel)
  val buffer = Source_viewer.buffer ()
  val mutable locs = None
  method buffer = buffer
  method locs = locs
  method rehighlight = Extlib.may Pretty_source.hilite locs
  method redisplay = self#init
  method private init =
    let highlighter localizable ~start ~stop =
      List.iter (fun f -> f buffer localizable ~start ~stop) !highlighter
    in
    let selector = selector_localizable main_ui ReactiveBuffer in
    Extlib.may Locs.finalize locs;
    locs <- Some(
      Pretty_source.display_source
        globs
        buffer
        ~host:(self:>Gtk_helper.host)
        ~highlighter
        ~selector)

  initializer
    self#init;
    Globals_GUI.add globs (self:> reactive_buffer)
end

let reactive_buffer main_ui ?parent_window globs  =
  try
    Globals_GUI.find globs
  with Not_found ->
    new reactive_buffer_cl main_ui ?parent_window globs

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

  let declare_markers (source:GSourceView2.source_view) =
    List.iter
      (fun v ->
	source#set_mark_category_pixbuf
	  ~category:(category v)
	  (Some (Gtk_helper.Icon.get (Gtk_helper.Icon.Feedback v))))
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

  let mark (source:GSourceView2.source_buffer) ~start ~stop:_ validity =
    begin
      let iter = source#get_iter_at_char start in
      let category = category validity in
      source#remove_source_marks iter iter () ;
      ignore (source#create_source_mark ~category iter) ;
    end

  let update (reactive_buffer:reactive_buffer) prop =
    Extlib.may
      (fun loc_table ->
	let validity = F.get prop in
	let loc = Pretty_source.PIP prop in
	let loc = locate_localizable loc_table loc in
	Extlib.may
	  (fun (start,stop) ->
	    mark reactive_buffer#buffer ~start ~stop validity)
	  loc)
      reactive_buffer#locs
end

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
    GWindow.window
      ?icon:framac_icon
      ~title:"Frama-C"
      ~width
      ~height
      ~position:`CENTER
      ~allow_shrink:true
      ~allow_grow:true
      ~show:false
      ()
  in
  let () = main_window#set_default_size ~width ~height in
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
  let statusbar =
    GMisc.statusbar ~has_resize_grip:false ~packing:bottom_hbox#add ()
  in
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
  let annot_window = new Toolbox.text () in
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
object (self:#main_window_extension_points)
  val mutable launcher = []
  val mutable panel = []
  val mutable main_window_metrics = { Gtk.width=0; height=0; x=0; y=0}
  val mutable file_tree = None
  val mutable current_buffer_state : reactive_buffer option = None
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
    Gui_parameters.debug "display_globals";
    let buff = reactive_buffer self#toplevel globs in
    current_buffer_state <- Some buff;
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
    Extlib.may (fun f -> f#redisplay) current_buffer_state;
    History.show_current ()

  method rehighlight () =
    Extlib.may (fun f -> f#rehighlight) current_buffer_state;
(*    self#file_tree#model#foreach
      (fun p i -> self#file_tree#model#row_changed p i;false) *)

  (* General idea: if there is a current buffer AND [loc] is inside,
     scroll to [loc]. Otherwise, open a relevant buffer by finding a
     varinfo or a global for [loc], then scroll to [loc]. *)
  method scroll loc =
    (* Used to avoid having two different history events, one created
       by [select_global], the other by [scroll] *)
    let history = History.on_current_history () in
    let update_source_view () =
      match Pretty_source.kf_of_localizable loc with
        | Some kf ->
            let g = Kernel_function.get_global kf in
            self#select_or_display_global g
        | None ->
            match loc with
              | PGlobal g -> self#select_or_display_global g
              | _ -> if Gui_parameters.debug_atleast 3 then
                  self#error "Gui: does not know how to scroll to loc"
                    (* In this case, there is nothing we can do: we do not
                       know which file/global to open to scroll in *)
    in
    (* We need a non-empty [current_buffer_state] to do something later *)
    (match current_buffer_state with
      | Some _ -> ()
      | None -> update_source_view ()
    );
    match current_buffer_state with
      | None -> ()
      | Some state ->
         (* [current_buffer_state] contains [loc], [o] is the offset,
            let's scroll to it *)
         let show o =
           history (fun () -> History.push (History.Localizable loc));
           let iter = self#source_viewer#buffer#get_iter (`OFFSET o) in
           ignore (self#source_viewer#backward_display_line_start iter);
           self#source_viewer#buffer#place_cursor iter;
           ignore (self#source_viewer#scroll_to_mark
                     ~use_align:true ~yalign:0.5 ~xalign:0. `INSERT);
           let adj = source_viewer_scroll#hadjustment in
           adj#set_value adj#lower
         in
         match Pretty_source.locate_localizable (Extlib.the state#locs) loc with
           | Some (b,_) -> show b
           | None ->
               (* Searching in [current_buffer_state] did not work, let's try
                  to open a good one *)
               update_source_view ();
               let state = Extlib.the current_buffer_state in
               match Pretty_source.locate_localizable
                (Extlib.the state#locs) loc with
                  | Some (b, _) -> show b
                  | None ->
                      if Gui_parameters.debug_atleast 3 then
                        self#error "Unable to scroll to loc, probably \
                                  not shown in the buffer"
                          (* Can appear eg. for an if (i<5) inside a loop,
                             which is not shown in general in the source code *)

  method view_stmt stmt =
    let kf = Kernel_function.find_englobing_kf stmt in
    let loc = PStmt (kf, stmt) in
    self#scroll loc;
    ignore (self#view_original_stmt stmt)

  method view_original loc =
    if not (Location.equal loc Location.unknown) then
      Source_manager.load_file
        self#original_source_viewer
        ~filename:(fst loc).Lexing.pos_fname
        ~line:(fst loc).Lexing.pos_lnum
        ~click_cb:(fun olocz ->
           match olocz with
           | None -> ()
           | Some locz ->
             let scroll_to_locz locz =
               Gtk_helper.later (fun () ->
                   (* Prevent filetree selector from resetting the
                      original source viewer. *)
                   Source_manager.selection_locked := true;
                   self#scroll locz;
                   (* The selection lock is asynchronously released by a
                      callback, and cannot be released here. *)
                 )
             in
             match locz with
             | PVDecl (_okf, vi) ->
               (* if it is a global variable, show it instead of the current function *)
               begin
                 try
                   ignore (Globals.Vars.find vi);
                   let glob = GVarDecl (vi, loc) in
                   Gtk_helper.later (fun () ->
                       Source_manager.selection_locked := true;
                       self#select_or_display_global glob;
                     )
                 with
                 | Not_found ->
                   (* not a global variable, treat as usual *)
                   scroll_to_locz locz
               end
             | PGlobal g ->
               (* if it is a type declaration/definition, ignore it, since
                  types are not displayed in the file tree *)
               begin
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
          Format.sprintf "emacsclient -n +%d %s"
            (fst loc).Lexing.pos_lnum (fst loc).Lexing.pos_fname
        (*          Format.sprintf "mate -a -l %d %s" line file  *)
        in
        if Gui_parameters.debug_atleast 1 then
          self#push_info "Running %s" args_for_emacs;
        ignore (Sys.command args_for_emacs);
        if Gui_parameters.debug_atleast 1 then self#pop_info ()
    end else
      self#view_original loc;
    loc

  method private info_string s =
    ignore (status_context#push s)

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
          self#info_string content)
        bfmt
        fmt

  method push_info fmt = self#push_info_buffer fmt

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
  method reset () =
    Gui_parameters.debug "Redisplaying gui";
    Globals_GUI.clear ();
    current_buffer_state <- None;
    self#file_tree#reset ();
    (self#menu_manager ())#refresh ();
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
    let find_text_in_viewer (viewer : [`GTextViewer of GText.view |`GSourceViewer of GSourceView2.source_view]) text =
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
          (* try to wrap search if user wishes *)
          if GToolbox.question_box ~title:"Not found"
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
          GToolbox.message_box ~title:"Not found"
            (Printf.sprintf "Not found: %s" text)
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
                (GToolbox.input_string
                   ~title:"Find" ~ok:"Find" ~cancel:"Cancel"
                   "Find global:" ~text:last_find_text)
            else last_find_text
          in
          if text <> "" then
            match self#file_tree#find_visible_global text with
            | None -> GToolbox.message_box ~title:"Not found"
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
    | Some (where_to_find,viewer) ->
      let text =
        if use_dialog then
          Extlib.opt_conv ""
            (GToolbox.input_string
               ~title:"Find" ~ok:"Find" ~cancel:"Cancel"
               ("Find text (" ^ where_to_find ^ "):") ~text:last_find_text)
        else last_find_text
      in
      if text <> "" then find_text_in_viewer viewer text else ()

  initializer
    self#set_reset self#reset;
    let menu_manager = self#menu_manager () (* create the menu_manager *) in
    main_window#add_accel_group menu_manager#factory#accel_group;

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

    (* Gestion of navigation history *)
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
            Some (tag, ci.corig_name)
          | TEnum (ei, _) -> Some (Logic_typing.Enum, ei.eorig_name)
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

    annot_window#on_link
      (fun button s ->
       begin
        try
          (* Retrieve a potential varinfo from the selection *)
          let vi = Gui_printers.varinfo_of_link s in
          (* Now that we have a varinfo, we re-synthetize a kinstr from
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
             open_in_external_viewer (fst loc).Lexing.pos_fname
               ~line:(fst loc).Lexing.pos_lnum;
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
  if Gui_parameters.Debug.get () = 0 then begin
    Gtk_helper.log_redirector
      (fun s -> stdout#buffer#insert ~iter:stdout#buffer#end_iter s);
    let it = make_tag stdout#buffer "italic" [`STYLE `OBLIQUE] in
    let tag_stack = Stack.create () in
    let open_tag s =
      (match s with
      | "i" ->
          Stack.push (it,(stdout#buffer#get_iter `INSERT)#offset) tag_stack;
      | _ -> ());
      ""
    in
    let close_tag s =
      (match s with
       | "i" ->
           let stop = stdout#buffer#end_iter in
           let tag,start_offset = Stack.pop tag_stack in
           let start = stdout#buffer#get_iter (`OFFSET start_offset) in
           (* Format.printf "start:%d stop:%d@." start#offset stop#offset; *)
           stdout#buffer#apply_tag tag ~start ~stop
       | _ -> ());
      ""
    in
    Kernel.register_tag_handlers (open_tag,close_tag)
  end;
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
           if project_name <> "" then
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

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

open Format
open Cil_types
open Db_types
open Db
open Gtk_helper
open Cil_datatype

(** The kind of object that can be selected in the source viewer *)
(* [VP] TODO: unify all annotations related constructor into
   a PAnnotation of Property_status.identified_property
 *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PTermLval of (kernel_function option * kinstr * term_lval)
  | PVDecl of (kernel_function option * varinfo)

  | PGlobal of global (* all globals but variable declarations and function
			 definitions. *)
  | PIP of Property.t

module Localizable =
  Datatype.Make
    (struct
      include Datatype.Undefined
      type t = localizable
      let name = "Pretty_source.Localizable"
      let reprs = List.map (fun g -> PGlobal g) Global.reprs
      let equal l1 l2 = match l1,l2 with
	| PStmt (_,ki1), PStmt (_,ki2) -> ki1.sid = ki2.sid
	| PLval (_,ki1,lv1), PLval (_,ki2,lv2) ->
	  Kinstr.equal ki1 ki2 && lv1 == lv2
	| PTermLval (_,ki1,lv1), PTermLval (_,ki2,lv2) ->
	  Kinstr.equal ki1 ki2 && Logic_utils.is_same_tlval lv1 lv2
	(* [JS 2008/01/21] term_lval are not shared: cannot use == *)
	| PVDecl (_,v1), PVDecl (_,v2) -> Varinfo.equal v1 v2
	| PIP ip1, PIP ip2 -> Property.equal ip1 ip2
	| PGlobal g1, PGlobal g2 -> g1 == g2
	(* TODO: add a proper comparison between two globals *)
	| (PStmt _ | PLval _ | PTermLval _ | PVDecl _ | PIP _ | PGlobal _), _
	  ->  false
      let mem_project = Datatype.never_any_project
     end)

let kf_of_localizable loc = match loc with
    | PLval (kf_opt, _, _)
    | PTermLval(kf_opt, _,_)
    | PVDecl (kf_opt, _) -> kf_opt
    | PStmt (kf, _) -> Some kf
    | PIP ip -> Property.get_kf ip
    | PGlobal (GFun ({svar = vi}, _)) -> Some (Globals.Functions.get vi)
    | PGlobal _ -> None

let ki_of_localizable loc = match loc with
    | PLval (_, ki, _)
    | PTermLval(_, ki,_) -> ki
    | PVDecl (_, _) -> Kglobal
    | PStmt (_, st) -> Kstmt st
    | PIP ip -> Property.get_kinstr ip
    | PGlobal _ -> Kglobal

let varinfo_of_localizable loc =
  match kf_of_localizable loc with
    | Some kf -> Some (Kernel_function.get_vi kf)
    | None ->
        match loc with
          | PGlobal (GVar (vi, _, _)
                   | GVarDecl (_, vi, _)
                   | GFun ({svar = vi }, _)) -> Some vi
          | _ -> None



module Locs:sig
  type state
  val add: state -> int * int -> localizable -> unit
  val iter : state -> (int * int -> localizable -> unit) -> unit
  val create : unit -> state
  val find_next_start :  state -> int -> (localizable -> bool) -> int
  val find : state -> int -> (int * int) * localizable
  val hilite : state -> unit
  val set_hilite : state -> (unit -> unit) -> unit
end
=
struct
  type state = { table : (int*int,localizable) Hashtbl.t;
                 mutable hiliter : unit -> unit}

  let create () =
    {table = Hashtbl.create 97;
     hiliter = (fun () -> ())}

  let hilite state = state.hiliter ()

  let set_hilite state f =
    state.hiliter <- f

  (* Add a location range only if it is not already there.
     Visually only the innermost pretty printed entity is kept.
     For example: 'loop assigns x;' will be indexed as an assigns
     and not as a code annotation.
  *)
  let add state loc v =
    if not (Hashtbl.mem state.table loc) then
      Hashtbl.add state.table loc v

  let find state p =
    let best = ref None in
    let update ((b,e) as loc) sid =
      if b <= p && p <= e then
	match !best with
	  | None -> best := Some (loc, sid)
	  | Some ((b',e'),_) -> if e-b < e'-b' then best := Some (loc, sid)
    in
    Hashtbl.iter update state.table ;
    match !best with None -> raise Not_found | Some (loc,sid) -> loc, sid

  (* Find the closest localizable q after position p such that [predicate q]. *)
  let find_next_start state p predicate =
    let current,_localized = find state p in
    let next = ref (p+1) in
    while
      let next_start,next_item = find state !next in
      next_start = current || not (predicate next_item)
    do
      incr next
    done;
    (* Parameters.debug "Char %d has next %d" p !next;*)
    !next

  let iter state f =
    (*Parameters.debug "Iterate on %d locations" (Hashtbl.length locs);*)
    Hashtbl.iter f state.table
    (*Parameters.debug "DONE: Iterate on %d locations" (Hashtbl.length locs);*)

  let size state = Hashtbl.length state.table

end

let hilite state = Locs.hilite state

module Tag = struct
  type t = localizable
  exception Wrong_decoder
  let make_modem charcode =
    let h = Hashtbl.create 17 in
    let current = ref 0 in
    (function lv ->
      incr current;
      Hashtbl.add h !current lv;
      sprintf "%c%x" charcode !current),
    (function code ->
       Scanf.sscanf code "%c%x"
         (fun c code ->
            if c=charcode then
              try Hashtbl.find h code with Not_found -> assert false
            else raise Wrong_decoder))

  let encode_stmt,decode_stmt = make_modem 's'
  let encode_lval,decode_lval = make_modem 'l'
  let encode_termlval,decode_termlval = make_modem 't'
  let encode_vdecl,decode_vdecl = make_modem 'd'
  let encode_global,decode_global = make_modem 'g'
  let encode_ip,decode_ip = make_modem 'i'

  let create = function
    | PStmt sid -> encode_stmt sid
    | PLval lval -> encode_lval lval
    | PTermLval lval -> encode_termlval lval
    | PVDecl vi -> encode_vdecl vi
    | PGlobal g -> encode_global g
    | PIP ip -> encode_ip ip

  let get s =
    try
      PStmt (decode_stmt s)
    with Wrong_decoder -> try
      PLval (decode_lval s)
    with Wrong_decoder -> try
      PTermLval (decode_termlval s)
    with Wrong_decoder -> try
      PVDecl (decode_vdecl s)
    with Wrong_decoder -> try
      PGlobal (decode_global s)
    with Wrong_decoder -> try
      PIP (decode_ip s)
    with Wrong_decoder ->
      assert false
end

class tagPrinterClass = object(self)
  inherit Printer.print () as super
  method private current_kinstr =
    match self#current_stmt with
    | None -> Kglobal
    | Some st -> Kstmt st

  method private current_sid =
    match super#current_stmt with
    | None -> assert false
    | Some st -> st.sid

  method private current_kf =
    match super#current_function with
    | None -> None
    | Some fd -> Some (Globals.Functions.get fd)

  val mutable localize_predicate = true (* wrap all identified predicates *)

  val mutable current_ca = None

  method private current_behavior_or_loop =
    match current_ca with
        None -> Property.Id_behavior (Extlib.the self#current_behavior)
      | Some ca -> Property.Id_code_annot ca

  method pStmtNext next fmt current =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PStmt (Extlib.the self#current_kf,current)))
      (super#pStmtNext next) current


  method pLval fmt lv =
    match self#current_kinstr with
    | Kglobal -> super#pLval fmt lv
        (* Do not highlight the lvals in initializers. *)
    | Kstmt _ as ki ->
        let alive =
          not (Value.is_computed ())
	  || Db.Value.is_accessible self#current_kinstr
	in
        if alive then
          Format.fprintf fmt "@{<%s>"
            (Tag.create (PLval (self#current_kf,ki,lv)));
        (match lv with
           | Var vi, (Field _| Index _ as o) ->
               (* Small hack to be able to click on the arrays themselves
                  in the easy cases *)
               self#pLval fmt (Var vi, NoOffset);
               self#pOffset fmt o
           | _ -> super#pLval fmt lv
        );
        if alive then Format.fprintf fmt "@}"

  method pTerm_lval fmt lv =
    (* similar to pLval *)
    match self#current_kinstr with
    | Kglobal -> super#pTerm_lval fmt lv
        (* Do not highlight the lvals in initializers. *)
    | Kstmt _ as ki ->
        let alive =
          not (Value.is_computed ())
	  || Db.Value.is_accessible self#current_kinstr
	in
        if alive then
          Format.fprintf fmt "@{<%s>"
            (Tag.create (PTermLval (self#current_kf,ki,lv)));
        (match lv with
           | TVar vi, (TField _| TIndex _ as o) ->
               self#pTerm_lval fmt (TVar vi, TNoOffset);
               self#pTerm_offset fmt o
           | _ -> super#pTerm_lval fmt lv
        );
        if alive then Format.fprintf fmt "@}"

  method pVDecl fmt vi =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PVDecl (self#current_kf,vi)))
      super#pVDecl vi

  method pCode_annot fmt ca =
    match ca.annot_content with
        AAssert _ | AInvariant _ | APragma _ | AVariant _ ->
          let ip = 
            Property.ip_of_code_annot_single
              (Extlib.the self#current_kf)
              (Extlib.the self#current_stmt)
              ca
          in
          localize_predicate <- false;
          Format.fprintf fmt "@{<%s>%a@}"
            (Tag.create (PIP ip))
            super#pCode_annot ca;
          localize_predicate <- true
      | AStmtSpec _ ->
        (* tags will be set in the inner nodes. *)
        super#pCode_annot fmt ca
      | AAssigns _  ->
        (* tags will be set in the inner nodes. *)
        current_ca <- Some ca;
        super#pCode_annot fmt ca;
        current_ca <- None

  method pGlobal fmt g =
    match g with
      (* these globals are already covered by PVDecl *)
    | GVarDecl _ | GVar _ | GFun _ -> super#pGlobal fmt g
    | _ ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PGlobal g))
          super#pGlobal
          g

  method pRequires fmt p =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP 
            (Property.ip_of_requires 
               (Extlib.the self#current_kf) self#current_kinstr b p)))
      super#pRequires p;
    localize_predicate <- true

  method pBehavior fmt b =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP 
            (Property.ip_of_behavior 
	       (Extlib.the self#current_kf) self#current_kinstr b)))
      super#pBehavior b

  method pDecreases fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
	    (Property.ip_of_decreases
	       (Extlib.the self#current_kf) self#current_kinstr t)))
      super#pDecreases t;
    localize_predicate <- true

  method pTerminates fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
	    (Property.ip_of_terminates
               (Extlib.the self#current_kf) self#current_kinstr t)))
      super#pTerminates t;
    localize_predicate <- true

  method pComplete_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
	    (Property.ip_of_complete
	       (Extlib.the self#current_kf) self#current_kinstr t)))
      super#pComplete_behaviors t

  method pDisjoint_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
	    (Property.ip_of_disjoint
	       (Extlib.the self#current_kf) self#current_kinstr t)))
      super#pDisjoint_behaviors t

  method pAssumes fmt p =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
	 (PIP
	    (Property.ip_of_assumes
               (Extlib.the self#current_kf) self#current_kinstr b p)))
      super#pAssumes p;
    localize_predicate <- true

  method pPost_cond fmt pc =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP 
            (Property.ip_of_ensures 
               (Extlib.the self#current_kf) self#current_kinstr b pc)))
      super#pPost_cond pc;
    localize_predicate <- true

  method pAssigns s fmt a =
    match 
      Property.ip_of_assigns (Extlib.the self#current_kf) self#current_kinstr
        self#current_behavior_or_loop a
    with
        None -> super#pAssigns s fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) (super#pAssigns s) a

  method pFrom s fmt from =
    match 
      Property.ip_of_from (Extlib.the self#current_kf) self#current_kinstr
        self#current_behavior_or_loop from
    with
        None -> super#pFrom s fmt from
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) (super#pFrom s) from

(* Not used anymore: all identified predicates are selectable somewhere up
    - assert and loop invariants are PCodeAnnot
    - contracts members have a dedicated tag.
  *)
 (* method pIdentified_predicate fmt ip =
    if localize_predicate then
      Format.fprintf fmt "@{<%s>%a@}"
        (Tag.create (PPredicate (self#current_kf,self#current_kinstr,ip)))
        super#pIdentified_predicate ip
    else super#pIdentified_predicate fmt ip
  *)
end

exception Found of int*int

let locate_localizable state loc =
  try
    Locs.iter
      state
      (fun (b,e) v -> if Localizable.equal v loc then raise (Found(b,e)));
    None
  with Found (b,e) -> Some (b,e)

let localizable_from_locs state ~file ~line =
  let loc_localizable = function
    | PStmt (_,st) | PLval (_,Kstmt st,_) | PTermLval(_,Kstmt st,_) ->
      Stmt.loc st
    | PIP ip ->
      (match Property.get_kinstr ip with
      | Kglobal ->
        (match Property.get_kf ip with
          None -> Location.unknown
        | Some kf -> Kernel_function.get_location kf)
      | Kstmt st -> Stmt.loc st)
    | PVDecl (_,vi) -> vi.vdecl
    | PGlobal g -> Global.loc g
    | (PLval _ | PTermLval _) as localize ->
      (match kf_of_localizable localize with
      | None -> Location.unknown
      | Some kf -> Kernel_function.get_location kf)
  in
  let r = ref [] in
  Locs.iter
    state
    (fun _ v ->
      let loc,_ = loc_localizable v in
      if line = loc.Lexing.pos_lnum && loc.Lexing.pos_fname = file then
        r := v::!r);
  !r

let buffer_formatter state source =
  let starts = Stack.create () in
  let emit_open_tag s =
    (*    Parameters.debug "EMIT TAG";*)
    Stack.push
      (source#end_iter#offset, Tag.get s)
      starts;
    ""
  in
  let emit_close_tag _s =
    (try
       let (p,sid) = Stack.pop starts in
       Locs.add state (p, source#end_iter#offset) sid
     with Stack.Empty ->
       Gui_parameters.debug "empty stack in emit_tag");
    ""
  in
  let gtk_fmt = Gtk_helper.make_formatter source in
  Format.pp_set_tags gtk_fmt true;
  Format.pp_set_print_tags gtk_fmt false;
  Format.pp_set_mark_tags gtk_fmt true;
  Format.pp_set_formatter_tag_functions
    gtk_fmt {(Format.pp_get_formatter_tag_functions gtk_fmt ()) with
               Format.mark_open_tag = emit_open_tag;
               Format.mark_close_tag = emit_close_tag;};

  Format.pp_set_margin gtk_fmt 79;
  gtk_fmt

let display_source globals source ~(host:Gtk_helper.host) ~highlighter ~selector =
  let state = Locs.create () in
  host#protect
    ~cancelable:false
    (fun () ->
       Gtk_helper.refresh_gui  ();
       source#set_text "";
       source#remove_all_tags ~start:source#start_iter ~stop:source#end_iter;
       let hiliter () =
         let event_tag = Gtk_helper.make_tag source ~name:"events" [] in
         Gtk_helper.cleanup_all_tags source;
         Locs.iter
           state
           (fun (pb,pe) v ->
              Gtk_helper.refresh_gui  ();
              match v with
              | PStmt (_,ki) ->
                  (try
	             let pb,pe = match ki with
                     | {skind = Instr _ | Return _ | Goto _
                       | Break _ | Continue _} -> pb,pe
	             | {skind = If _ | Loop _
                       | Switch _ } ->
		         (* These statements contain other statements.
		            We highlight only until the start of the first
			    included statement. *)
                         pb,
                         (try Locs.find_next_start state pb
		            (fun p -> match p with
		             | PStmt _ -> true
		             | _ -> false (* Do not stop on expressions*))
                          with Not_found -> pb+1)
	             | {skind = Block _ | TryExcept _ | TryFinally _
                       | UnspecifiedSequence _} ->
                         pb,
                         (try Locs.find_next_start state pb (fun _ -> true)
                          with Not_found -> pb+1)
	             in
	             highlighter v ~start:pb ~stop:pe
                   with Not_found -> ())
              | PTermLval _ | PLval _ | PVDecl _ | PGlobal _
              | PIP _ ->
	          highlighter v  ~start:pb ~stop:pe);
         (*  Parameters.debug "Highlighting done (%d occurrences)" (Locs.size ());*)

         (* React to events on the text *)
         source#apply_tag ~start:source#start_iter ~stop:source#end_iter event_tag;
         (*  Parameters.debug "Event tag done";*)
       in
       Locs.set_hilite state hiliter;

       (*  Parameters.debug "Display source starts";*)
       let gtk_fmt = buffer_formatter state (source:>GText.buffer) in
       let tagPrinter = new tagPrinterClass in
       let display_global g =
         Gtk_helper.refresh_gui  ();
         tagPrinter#pGlobal gtk_fmt g;
         Format.pp_print_flush gtk_fmt ()
       in
       (*  Parameters.debug "Before Display globals %d" (List.length globals);*)
       let counter = ref 0 in
       begin try
         List.iter
           (fun g ->
              incr counter;
              if !counter > 20 then raise Exit;
              display_global g)
           globals;
       with Exit ->
         Format.fprintf
           gtk_fmt
           "@.<<Cannot display more than %d globals at a time. Skipping end of file>>@."
           !counter;
	 (*let ca = source#create_child_anchor source#end_iter in
	   source_view#add_child_at_anchor (GButton.button
	   ~text:"See 10 more globals"
	   ~callback:(fun _ -> call_cc next_10)
	   ()) ca *)
       end;
       (*  Parameters.debug "Displayed globals";*)

       source#place_cursor source#start_iter;
       (* Highlight the localizable *)
       hiliter ();
       let last_shown_area =
         Gtk_helper.make_tag source ~name:"last_show_area"
           [`BACKGROUND "light green"]
       in
       let event_tag = Gtk_helper.make_tag source ~name:"events" [] in
       ignore
         (event_tag#connect#event ~callback:
            (fun ~origin:_ ev it ->
               if !Gtk_helper.gui_unlocked then
                    if GdkEvent.get_type ev = `BUTTON_PRESS then begin
                      let coords = GtkText.Iter.get_offset it in
	              try
		        let ((pb,pe), selected) = Locs.find state coords in
		        (* Highlight the pointed term *)
                        source#remove_tag
		          ~start:source#start_iter
		          ~stop:source#end_iter
		          last_shown_area;
		        apply_tag source last_shown_area pb pe;
		        let event_button = GdkEvent.Button.cast ev in
		        let button = GdkEvent.Button.button event_button in
		        host#protect ~cancelable:false
			  (fun () -> selector ~button selected);
	              with Not_found -> () (* no statement at this offset *)
                    end;
             false)));
  state


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

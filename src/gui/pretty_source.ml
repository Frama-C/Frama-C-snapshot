(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: pretty_source.ml,v 1.36 2009-02-13 07:59:29 uid562 Exp $ *)

open Format
open Cil_types
open Db_types
open Db
open Gtk_helper

(** The kind of object that can be selected in the source viewer *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PTermLval of (kernel_function option * kinstr * term_lval)
  | PVDecl of (kernel_function option * varinfo)

  | PCodeAnnot of (kernel_function * stmt * code_annotation)
  | PGlobal of global (* all globals but variable declarations and function
			 definitions. *)
  | PRequires of (kernel_function*kinstr*funbehavior option*identified_predicate)
  | PBehavior of (kernel_function*kinstr*funbehavior)
  | PVariant of (kernel_function*kinstr*term variant)
  | PTerminates of (kernel_function*kinstr*identified_predicate)
  | PComplete_behaviors of (kernel_function*kinstr*string list)
  | PDisjoint_behaviors of (kernel_function*kinstr*string list)

  | PAssumes of (kernel_function*kinstr*funbehavior option*identified_predicate)
  | PPost_cond of
      (kernel_function*kinstr*funbehavior option*
         (termination_kind * identified_predicate))
  | PAssigns of (kernel_function*kinstr*funbehavior option*identified_term assigns list)
  | PPredicate of (kernel_function option*kinstr*identified_predicate)

module Localizable_Datatype =
  Project.Datatype.Imperative
    (struct
       type t = localizable
       let copy _ = assert false (* TODO *)
       let name = "localizable"
     end)

let kf_of_localizable loc = match loc with
    | PLval (kf_opt, _, _)
    | PTermLval(kf_opt, _,_)
    | PVDecl (kf_opt, _)
    | PPredicate (kf_opt, _, _)
      -> kf_opt
    | PStmt (kf, _)
    | PCodeAnnot (kf,  _,  _)
    | PRequires (kf, _, _, _)
    | PBehavior (kf, _, _)
    | PVariant (kf, _, _)
    | PTerminates (kf, _, _)
    | PComplete_behaviors (kf, _, _)
    | PDisjoint_behaviors (kf, _, _)
    | PAssumes (kf, _, _, _)
    | PPost_cond (kf, _, _, _)
    | PAssigns (kf, _, _, _)
      -> Some kf
    | PGlobal _ 
      -> None


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
  let encode_code_annot,decode_code_annot = make_modem 'c'
  let encode_global,decode_global = make_modem 'g'
  let encode_behavior,decode_behavior = make_modem 'b'
  let encode_assigns,decode_assigns = make_modem 'a'
  let encode_post_cond,decode_post_cond = make_modem 'e'
  let encode_assumes,decode_assumes = make_modem 'u'
  let encode_disjoint,decode_disjoint = make_modem 'i'
  let encode_complete,decode_complete = make_modem 'o'
  let encode_terminates,decode_terminates = make_modem 'r'
  let encode_variant,decode_variant = make_modem 'v'
  let encode_requires,decode_requires = make_modem 'q'
  let encode_predicate,decode_predicate = make_modem 'p'

  let create = function
    | PStmt sid -> encode_stmt sid
    | PLval lval -> encode_lval lval
    | PTermLval lval -> encode_termlval lval
    | PVDecl vi -> encode_vdecl vi
    | PCodeAnnot ca -> encode_code_annot ca
    | PGlobal g -> encode_global g
    | PBehavior b -> encode_behavior b
    | PAssigns a -> encode_assigns a
    | PPredicate p ->  encode_predicate p
    | PPost_cond p -> encode_post_cond p
    | PAssumes p -> encode_assumes p
    | PDisjoint_behaviors p -> encode_disjoint p
    | PComplete_behaviors p -> encode_complete p
    | PTerminates p -> encode_terminates p
    | PVariant v -> encode_variant v
    | PRequires p -> encode_requires p

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
      PCodeAnnot (decode_code_annot s)
    with Wrong_decoder -> try
      PGlobal (decode_global s)
    with Wrong_decoder -> try
      PBehavior (decode_behavior s)
    with Wrong_decoder -> try
      PAssigns (decode_assigns s)
    with Wrong_decoder -> try
    PPredicate (decode_predicate s)
    with Wrong_decoder -> try
    PPost_cond (decode_post_cond s)
    with Wrong_decoder -> try
    PAssumes (decode_assumes s)
    with Wrong_decoder -> try
    PDisjoint_behaviors (decode_disjoint s)
    with Wrong_decoder -> try
    PComplete_behaviors (decode_complete s)
    with Wrong_decoder -> try
    PTerminates (decode_terminates s)
    with Wrong_decoder -> try
    PVariant (decode_variant s)
    with Wrong_decoder -> try
    PRequires (decode_requires s)
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

  val mutable current_behavior = None
  method private current_behavior =
    current_behavior

  val mutable localize_predicate = true (* wrap all identified predicates *)

  method pStmtNext next fmt current =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PStmt (Cilutil.out_some self#current_kf,current)))
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
        super#pLval fmt lv;
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
        super#pTerm_lval fmt lv;
        if alive then Format.fprintf fmt "@}"

  method pVDecl fmt vi =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PVDecl (self#current_kf,vi)))
      super#pVDecl vi

  method pCode_annot fmt ca =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PCodeAnnot (Cilutil.out_some self#current_kf,
			       Cilutil.out_some self#current_stmt,
			       ca)))
      super#pCode_annot ca;
    localize_predicate <- true

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
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PRequires (Cilutil.out_some self#current_kf,
			      self#current_kinstr,
                              self#current_behavior,
			      p)))
      super#pRequires p;
    localize_predicate <- true

  method pBehavior fmt b =
    current_behavior <- Some b;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PBehavior (Cilutil.out_some self#current_kf,self#current_kinstr,b)))
      super#pBehavior b;
    current_behavior <- None

  method pDecreases fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PVariant (Cilutil.out_some self#current_kf,self#current_kinstr,t)))
      super#pDecreases t;
    localize_predicate <- true

  method pTerminates fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PTerminates (Cilutil.out_some self#current_kf,self#current_kinstr,t)))
      super#pTerminates t;
    localize_predicate <- true

  method pComplete_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PComplete_behaviors (Cilutil.out_some self#current_kf,self#current_kinstr,t)))
      super#pComplete_behaviors t

  method pDisjoint_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PDisjoint_behaviors (Cilutil.out_some self#current_kf,self#current_kinstr,t)))
      super#pDisjoint_behaviors t

  method pAssumes fmt p =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PAssumes (Cilutil.out_some self#current_kf,
			     self#current_kinstr,
                               self#current_behavior,
			     p)))
      super#pAssumes p;
    localize_predicate <- true

  method pPost_cond fmt p =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PPost_cond (Cilutil.out_some self#current_kf,
			      self#current_kinstr,
                              self#current_behavior,
			      p)))
      super#pPost_cond p;
    localize_predicate <- true

  method pAssigns s fmt a =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PAssigns (Cilutil.out_some self#current_kf,
                    self#current_kinstr,self#current_behavior,a)))
      (super#pAssigns s) a

  method pIdentified_predicate fmt ip =
    if localize_predicate then
      Format.fprintf fmt "@{<%s>%a@}"
        (Tag.create (PPredicate (self#current_kf,self#current_kinstr,ip)))
        super#pIdentified_predicate ip
    else super#pIdentified_predicate fmt ip

end

let equal_localizable l1 l2 =
  match l1,l2 with
  | PStmt (_,ki1), PStmt (_,ki2) -> ki1.sid = ki2.sid
  | PLval (_,ki1,lv1), PLval (_,ki2,lv2) ->
      Cilutil.Instr.equal ki1 ki2 && lv1 == lv2
  | PTermLval (_,ki1,lv1), PTermLval (_,ki2,lv2) ->
      Cilutil.Instr.equal ki1 ki2 && Logic_utils.is_same_tlval lv1 lv2
	(* [JS 21/01/08:] term_lval are not shared: cannot use == *)
  | PVDecl (_,v1), PVDecl (_,v2) ->
      v1.vid == v2.vid
  | PCodeAnnot (_,_,{annot_id=id1}),PCodeAnnot (_,_,{annot_id=id2}) ->
      id1 = id2
  | _ -> false (*TODO: add the full diagonal*)

exception Found of int*int

let locate_localizable state loc =
  try
    Locs.iter
      state
      (fun (b,e) v -> if equal_localizable v loc then raise (Found(b,e)));
    None
  with Found (b,e) -> Some (b,e)

let localizable_from_locs state ~file ~line =
  let loc_localizable = function
    | PStmt (_,st) | PLval (_,Kstmt st,_) | PTermLval(_,Kstmt st,_)
    | PCodeAnnot (_,st,_) ->
        Cilutil.get_stmtLoc st.skind
    | PVDecl (_,vi) -> vi.vdecl
    | PGlobal g -> Cilutil.get_globalLoc g
    | _ -> Cilutil.locUnknown
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
		         (* these statements contain other statements.
		            We highlight only until the start of the first included
		            statement *)
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
              | PTermLval _ | PLval _ | PVDecl _ | PCodeAnnot _ | PGlobal _
              | PBehavior _ | PPredicate _ | PAssigns _
              | PPost_cond _| PAssumes _| PDisjoint_behaviors _
              | PComplete_behaviors _
              | PTerminates _| PVariant _| PRequires _ ->
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
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

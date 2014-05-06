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

open Format
open Cil_types
open Gtk_helper
open Cil_datatype

(** The kind of object that can be selected in the source viewer *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PTermLval of (kernel_function option * kinstr * term_lval)
  | PVDecl of (kernel_function option * varinfo)
  | PGlobal of global
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
        | PGlobal g1, PGlobal g2 -> Cil_datatype.Global.equal g1 g2
        | (PStmt _ | PLval _ | PTermLval _ | PVDecl _ | PIP _ | PGlobal _), _
          ->  false
      let mem_project = Datatype.never_any_project
      let pretty fmt = function
        | PStmt (_, s) -> Format.fprintf fmt "LocalizableStmt %d (%a)"
            s.sid Printer.pp_location (Cil_datatype.Stmt.loc s)
        | PLval (_, ki, lv) -> 
	  Format.fprintf fmt "LocalizableLval %a (%a)"
            Printer.pp_lval lv 
	    Cil_datatype.Location.pretty (Cil_datatype.Kinstr.loc ki)
        | PTermLval (_, ki, tlv) ->
            Format.fprintf fmt "LocalizableTermLval %a (%a)"
            Printer.pp_term_lval tlv
	    Cil_datatype.Location.pretty (Cil_datatype.Kinstr.loc ki)
        | PVDecl (_, vi) ->
            Format.fprintf fmt "LocalizableVDecl %a" Printer.pp_varinfo vi
        | PGlobal g ->
            Format.fprintf fmt "LocalizableGlobal %a" Printer.pp_global g
        | PIP ip ->
            Format.fprintf fmt "LocalizableIP %a" Description.pp_property ip
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
  val find : state -> int -> (int * int) * localizable
  val hilite : state -> unit
  val set_hilite : state -> (unit -> unit) -> unit
  val add_finalizer: state -> (unit -> unit) -> unit
  val finalize: state -> unit
  val size : state -> int
end
=
struct
  type state = { table : (int*int,localizable) Hashtbl.t;
                 mutable hiliter : unit -> unit;
                 mutable finalizers: (unit -> unit) list;
               }

  let create () =
    {table = Hashtbl.create 97;
     hiliter = (fun () -> ());
     finalizers = [];
    }

  let hilite state = state.hiliter ()

  let set_hilite state f =
    state.hiliter <- f

  let add_finalizer state f =
    state.finalizers <- f :: state.finalizers

  let finalize state =
    List.iter (fun f -> f ()) (List.rev state.finalizers)

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

  let iter state f =
    (*Kernel.debug "Iterate on %d locations" (Hashtbl.length locs);*)
    Hashtbl.iter f state.table
    (*Kernel.debug "DONE: Iterate on %d locations" (Hashtbl.length locs);*)

  let size state = Hashtbl.length state.table

end

let hilite state = Locs.hilite state

module LocsArray:sig
  type t
  val create: Locs.state -> t
  val length : t -> int
  val get : t -> int -> (int * int) * localizable
  val find_next : t -> int -> (localizable -> bool) -> int
end
=
struct
(* computes an ordered array containing all the elements of a Locs.state,
   the order (<) being such that loc1 < loc2 if either loc1 starts
   before loc2, or loc1 and loc2 start at the same position but
   loc1 spawns further than loc2.
*)
   
  type t = ((int*int) * localizable option) array
      
  let create state = 
    let arr = Array.make (Locs.size state) ((0,0), None) in
    let index = ref 0 in 
    Locs.iter 
      state
      (fun (pb,pe) v ->
	Array.set arr !index ((pb,pe), Some v) ;
	incr index
      )
    ;
    Array.sort 
      (fun ((pb1,pe1),_) ((pb2,pe2),_) ->
	if (pb1 = pb2) then 
	  if (pe1 = pe2) then 0
	  else 
	    (* most englobing comes first *)
	    Pervasives.compare pe2 pe1 
	else Pervasives.compare pb1 pb2
      ) arr
    ;
    arr

  let length arr = Array.length arr

  (* get loc at index i;
     raises Not_found if none exists *)
  let get arr i = 
    if i >= Array.length arr then raise Not_found
    else 
      match Array.get arr i with
	| ((_,_),None) -> raise Not_found
	| ((pb,pe),Some v) -> ((pb,pe),v)
	  
  (* find the next loc in array starting at index i
     which satifies the predicate; 
     raises Not_found if none exists *)
  let find_next arr i predicate = 
    let rec fnext i = 
      let ((pb',_pe'),v) = get arr i in
      if predicate v then pb'
      else fnext (i+1)
    in fnext i

end

module Tag = struct
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

class tagPrinterClass : Printer.extensible_printer = object(self)

  inherit Printer.extensible_printer () as super

  method! varname fmt x = Printer.pp_varname fmt x

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

  method! next_stmt next fmt current =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PStmt (Extlib.the self#current_kf,current)))
      (super#next_stmt next) current

  method! lval fmt lv =
    match self#current_kinstr with
    | Kglobal -> super#lval fmt lv
        (* Do not highlight the lvals in initializers. *)
    | Kstmt _ as ki ->
        Format.fprintf fmt "@{<%s>"
          (Tag.create (PLval (self#current_kf,ki,lv)));
        (match lv with
           | Var vi, (Field _| Index _ as o) ->
               (* Small hack to be able to click on the arrays themselves
                  in the easy cases *)
               self#lval fmt (Var vi, NoOffset);
               self#offset fmt o
           | _ -> super#lval fmt lv
        );
        Format.fprintf fmt "@}"

  method! term_lval fmt lv =
    (* similar to pLval *)
    match self#current_kinstr with
    | Kglobal -> super#term_lval fmt lv
        (* Do not highlight the lvals in initializers. *)
    | Kstmt _ as ki ->
        Format.fprintf fmt "@{<%s>"
          (Tag.create (PTermLval (self#current_kf,ki,lv)));
        (match lv with
           | TVar vi, (TField _| TIndex _ as o) ->
               self#term_lval fmt (TVar vi, TNoOffset);
               self#term_offset fmt o
           | _ -> super#term_lval fmt lv
        );
        Format.fprintf fmt "@}"

  method! vdecl fmt vi =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create (PVDecl (self#current_kf,vi)))
      super#vdecl vi

  method! code_annotation fmt ca =
    match ca.annot_content with
      | APragma p when not (Logic_utils.is_property_pragma p) ->
        (* Not currently localizable. Will be linked to the next stmt *)
        super#code_annotation fmt ca
      | AAssert _ | AInvariant _ | APragma _ | AVariant _ ->
          let ip =
            Property.ip_of_code_annot_single
              (Extlib.the self#current_kf)
              (Extlib.the self#current_stmt)
              ca
          in
          localize_predicate <- false;
          Format.fprintf fmt "@{<%s>%a@}"
            (Tag.create (PIP ip))
            super#code_annotation ca;
          localize_predicate <- true
      | AStmtSpec _ ->
        (* tags will be set in the inner nodes. *)
        super#code_annotation fmt ca
      | AAllocation _ 
      | AAssigns _  ->
        (* tags will be set in the inner nodes. *)
        current_ca <- Some ca;
        super#code_annotation fmt ca;
        current_ca <- None

  method! global fmt g =
    match g with
      (* these globals are already covered by PVDecl *)
    | GVarDecl _ | GVar _ | GFun _ -> super#global fmt g
    | _ ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PGlobal g))
          super#global
          g

  method! requires fmt p =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_requires
               (Extlib.the self#current_kf) self#current_kinstr b p)))
      super#requires p;
    localize_predicate <- true

  method! behavior fmt b =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_behavior
               (Extlib.the self#current_kf) self#current_kinstr b)))
      super#behavior b

  method! decreases fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_decreases
               (Extlib.the self#current_kf) self#current_kinstr t)))
      super#decreases t;
    localize_predicate <- true

  method! terminates fmt t =
    localize_predicate <- false;
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_terminates
               (Extlib.the self#current_kf) self#current_kinstr t)))
      super#terminates t;
    localize_predicate <- true

  method! complete_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_complete
               (Extlib.the self#current_kf) self#current_kinstr t)))
      super#complete_behaviors t

  method! disjoint_behaviors fmt t =
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_disjoint
               (Extlib.the self#current_kf) self#current_kinstr t)))
      super#disjoint_behaviors t

  method! assumes fmt p =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_assumes
               (Extlib.the self#current_kf) self#current_kinstr b p)))
      super#assumes p;
    localize_predicate <- true

  method! post_cond fmt pc =
    localize_predicate <- false;
    let b = Extlib.the self#current_behavior in
    Format.fprintf fmt "@{<%s>%a@}"
      (Tag.create
         (PIP
            (Property.ip_of_ensures
               (Extlib.the self#current_kf) self#current_kinstr b pc)))
      super#post_cond pc;
    localize_predicate <- true

  method! assigns s fmt a =
    match
      Property.ip_of_assigns (Extlib.the self#current_kf) self#current_kinstr
        self#current_behavior_or_loop a
    with
        None -> super#assigns s fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) (super#assigns s) a

  method! from s fmt ((_, f) as from) =
    match f with
      | FromAny -> super#from s fmt from
      | From _ ->
          let ip =
            Property.ip_of_from (Extlib.the self#current_kf) self#current_kinstr
              self#current_behavior_or_loop from
          in
              Format.fprintf fmt "@{<%s>%a@}"
              (Tag.create (PIP ip)) (super#from s) from

  method! global_annotation fmt a = 
    match Property.ip_of_global_annotation_single a with
    | None -> super#global_annotation fmt a
    | Some ip ->
      Format.fprintf fmt "@{<%s>%a@}" 
	(Tag.create (PIP ip)) super#global_annotation a

  method! allocation ~isloop fmt a =
    match
      Property.ip_of_allocation (Extlib.the self#current_kf) self#current_kinstr
        self#current_behavior_or_loop a
    with
        None -> super#allocation ~isloop fmt a
      | Some ip ->
          localize_predicate <- true;
          Format.fprintf fmt "@{<%s>%a@}"
            (Tag.create (PIP ip)) (super#allocation ~isloop) a;
          localize_predicate <- false;

  initializer force_brace <- true

(* Not used anymore: all identified predicates are selectable somewhere up
    - assert and loop invariants are PCodeAnnot
    - contracts members have a dedicated tag.
  *)
 (* method pIdentified_predicate fmt ip =
    if localize_predicate then
      Format.fprintf fmt "@{<%s>%a@}"
        (Tag.create (PPredicate (self#current_kf,self#current_kinstr,ip)))
        super#identified_predicate ip
    else super#identified_predicate fmt ip
  *)
end

exception Found of int*int

(* This function identifies two distinct localizable that happen to have
   the same location in the source code, typically because one of them
   is not printed. Feel free to add other heuristics if needed. *)
let equal_or_same_loc loc1 loc2 =
  Localizable.equal loc1 loc2 ||
    match loc1, loc2 with
      | PIP (Property.IPReachable (_, Kstmt s, _)), PStmt (_, s')
      | PStmt (_, s'), PIP (Property.IPReachable (_, Kstmt s, _)) when
          Cil_datatype.Stmt.equal s s' -> true
      | PIP (Property.IPReachable (Some kf, Kglobal, _)),
          (PVDecl (_, vi) | PGlobal (GFun ({ svar = vi }, _)))
      | (PVDecl (_, vi) | PGlobal (GFun ({ svar = vi }, _))),
          PIP (Property.IPReachable (Some kf, Kglobal, _))
         when Kernel_function.get_vi kf = vi
           -> true
      | _ -> false

let locate_localizable state loc =
  try
    Locs.iter
      state
      (fun (b,e) v -> if equal_or_same_loc v loc then raise (Found(b,e)));
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
    (*    Kernel.debug "EMIT TAG";*)
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

let display_source globals 
    (source:GSourceView2.source_buffer) ~(host:Gtk_helper.host) 
    ~highlighter ~selector =
  let state = Locs.create () in
(*  let highlighter _ ~start:_ ~stop:_ = () in *)
  host#protect
    ~cancelable:false
    (fun () ->
       Gtk_helper.refresh_gui  ();
       source#set_text "";
       source#remove_source_marks 
         ~start:source#start_iter ~stop:source#end_iter ();
       let hiliter () =
         let event_tag = Gtk_helper.make_tag source ~name:"events" [] in
         Gtk_helper.cleanup_all_tags source;
	 let locs_array = LocsArray.create state in
	 let index_max =  LocsArray.length locs_array in
	 let index = ref 0 in
	 while(!index < index_max) do (
	   try	     
	     let ((pb,pe),v) = LocsArray.get locs_array !index in
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
			(try LocsArray.find_next locs_array (!index+1)
			       (fun p -> match p with
				 | PStmt _ -> true
				 | _ -> false (* Do not stop on expressions*))
			 with Not_found -> pb+1)
		      | {skind = Block _ | TryExcept _ | TryFinally _
			    | UnspecifiedSequence _} ->
                          pb,
                        (try LocsArray.find_next locs_array (!index+1) (fun _ -> true)
                         with Not_found -> pb+1)
                    in
                    highlighter v ~start:pb ~stop:pe
                  with Not_found -> ())
	       | PTermLval _ | PLval _ | PVDecl _ | PGlobal _
	       | PIP _ ->
                 highlighter v  ~start:pb ~stop:pe	       
	   with Not_found -> () ) ;
	   incr index
	 done;
         (*  Kernel.debug "Highlighting done (%d occurrences)" (Locs.size ());*)
	 
         (* React to events on the text *)
         source#apply_tag ~start:source#start_iter ~stop:source#end_iter event_tag;
         (*  Kernel.debug "Event tag done";*)
       in
       Locs.set_hilite state hiliter;

       (*  Kernel.debug "Display source starts";*)
       let gtk_fmt = buffer_formatter state (source:>GText.buffer) in
       let tagPrinter = new tagPrinterClass in
       let display_global g =
         Gtk_helper.refresh_gui  ();
         tagPrinter#global gtk_fmt g;
         Format.pp_print_flush gtk_fmt ()
       in
       (*  Kernel.debug "Before Display globals %d" (List.length globals);*)
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
       (*  Kernel.debug "Displayed globals";*)

       source#place_cursor source#start_iter;
       (* Highlight the localizable *)
       hiliter ();
       let last_shown_area =
         Gtk_helper.make_tag source ~name:"last_shown_area"
           [`BACKGROUND "light green"]
       in
       let event_tag = Gtk_helper.make_tag source ~name:"events" [] in
       let id = event_tag#connect#event ~callback:
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
             false)
       in
       Locs.add_finalizer state
         (fun () -> GtkSignal.disconnect event_tag#as_tag id);
    );
  state


module LineToLocalizable =
  Datatype.Hashtbl(Datatype.Int.Hashtbl)(Datatype.Int)
    (struct let module_name = "Pretty_source.LineToLocalizable" end)
module FileToLines =
  Datatype.Hashtbl(Datatype.String.Hashtbl)(Datatype.String)
    (struct let module_name = "Pretty_source.FilesToLine" end)

module MappingLineLocalizable = struct
  module LineToLocalizableAux =
    LineToLocalizable.Make( Datatype.Pair(Location)(Localizable))

  include State_builder.Hashtbl(FileToLines)(LineToLocalizableAux)
      (struct
        let size = 5
        let dependencies = [Ast.self]
        let name = "Pretty_source.line_to_localizable"
       end)
end

class pos_to_localizable =
object (self)
  inherit Visitor.frama_c_inplace

  method add_range loc (localizable : localizable) =
    if not (Location.equal loc Location.unknown) then (
      let p1, p2 = loc in
      assert (p1.Lexing.pos_fname = p2.Lexing.pos_fname);
      let file = p1.Lexing.pos_fname in
      let hfile =
        try MappingLineLocalizable.find file
        with Not_found ->
          let h = LineToLocalizable.create 17 in
          MappingLineLocalizable.add file h;
          h
      in
      for i = p1.Lexing.pos_lnum to p2.Lexing.pos_lnum do
        LineToLocalizable.add hfile i (loc, localizable);
      done
    );

  method! vstmt_aux s =
    Gui_parameters.debug ~level:3 "Locs for Stmt %d" s.sid;
    self#add_range (Stmt.loc s) (PStmt (Extlib.the self#current_kf, s));
    Cil.DoChildren

  method! vglob_aux g =
    Gui_parameters.debug ~level:3 "Locs for global %a" Printer.pp_global g;
    (match g with
      | GFun ({ svar = vi }, loc) ->
          self#add_range loc (PVDecl (Some (Globals.Functions.get vi), vi))
      | GVar (vi, _, loc) ->
          self#add_range loc (PVDecl (None, vi))
      | GVarDecl (_, vi, loc) ->
          if Cil.isFunctionType vi.vtype then
            self#add_range loc (PVDecl (Some (Globals.Functions.get vi), vi))
          else
            self#add_range loc (PVDecl (None, vi))
      | _ -> self#add_range (Global.loc g) (PGlobal g)
    );
    Cil.DoChildren
end

let loc_to_localizable loc =
  if not (MappingLineLocalizable.is_computed ()) then (
    Gui_parameters.debug "Computing inverse locs";
    let vis = new pos_to_localizable in
    Visitor.visitFramacFile (vis :> Visitor.frama_c_visitor) (Ast.get ());
    MappingLineLocalizable.mark_as_computed ();
  );
  try
    (* Find the mapping from this file to locs-by-line *)
    let hfile = MappingLineLocalizable.find loc.Lexing.pos_fname in
    (* Find the localizable for this line *)
    let all = LineToLocalizable.find_all hfile loc.Lexing.pos_lnum in
    (* Try to a find the good localizable. When we have more than one matches
       with the exact same location, we pick the last one in the list. This
       will be the first statement that has been encountered, and this
       criterion seems to work well with temporaries introduced by Cil *)
    let last l = match List.rev l with [] -> None | (_, loc) :: _ -> Some loc in
    (match all, List.filter (fun ((loc', _), _) -> loc = loc') all with
      | [], _ -> None
      | _, (_ :: _ as exact) -> last exact (* a pos exactly corresponds *)
      | (l, _) :: __, [] -> (* No exact loc. We consider the innermost
                               statements, ie those at the top of the list *)
          last (List.filter (fun (l', _) -> Location.equal l l') all)
    )
  with Not_found ->
    Gui_parameters.debug "No pretty-printed loc found";
    None


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

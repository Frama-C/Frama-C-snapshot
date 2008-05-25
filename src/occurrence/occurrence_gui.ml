(* $Id: occurrence_gui.ml,v 1.8 2008/03/06 08:23:29 uid568 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types

exception Highlight of Db_types.kernel_function * stmt

let apply_tag main_ui name color =
  let view = main_ui#source_viewer in
  let tag = make_tag view#buffer name [`BACKGROUND color ] in
  cleanup_tag view#buffer tag;
  List.iter
    (fun (ki, lv) ->
       let skf, kf = match ki with 
	 | Kglobal -> None, None
	 | Kstmt s -> 
	     let s, kf = Kernel_function.find_from_sid s.sid in
	     Some (s, kf), Some kf
       in
       let highlight = main_ui#highlight ~scroll:true tag in
       let try_and_highlight loc = 
	 match Pretty_source.locate_localizable loc, skf with
	 | None, None -> invalid_arg "some occurrence cannot be highlighted"
	 | None, Some (s, kf) -> raise (Highlight(kf, s))
	 | Some _, _ -> highlight loc
       in
       (* Try to highlight as a lval *)
       try try_and_highlight (PLval (kf, ki, lv))
       with 
       | Invalid_argument _ | Highlight _ ->
	   (* If that doesn't work, try to highlight as a term_lval *)
	   try 
	     try_and_highlight 
	       (PTermLval (kf, ki, Logic_const.lval_to_term_lval lv))
	   with 
	   | Highlight(kf, s) -> 
	       (* Possible to highlight the whole stmt *)
	       highlight (PStmt (kf, s))
	   | Invalid_argument msg -> 
	       (* Cannot highlight *)
	       Format.printf "%s@." msg)

let occurrence_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  if button = 3 then
    match localizable with
    | PVDecl (_,vi) ->
	if not (Cil.isFunctionType vi.vtype) then begin
          let callback () =
	    let lvals = !Db.Occurrence.get vi in
	    apply_tag main_ui "occurrence" "yellow" lvals
	  in
          ignore (popup_factory#add_item "_Occurrence" ~callback)
	end
    | _ ->
	()

let main main_ui = main_ui#register_source_selector occurrence_selector

let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

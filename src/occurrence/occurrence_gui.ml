(* $Id: occurrence_gui.ml,v 1.12 2008/07/11 12:44:15 uid568 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types
open Cilutil

let highlight_state = ref ([], None)

let occurrence_highlighter buffer loc ~start ~stop = 
  let highlight_stmt, highlight_vi = !highlight_state in
  match highlight_vi with
  | None -> (* occurrence not computed *)
      ()
  | Some vi ->
    let tag = make_tag buffer "occurrence" [`BACKGROUND "yellow" ] in
    match loc with 
    | PLval (_,ki,lval) -> 
	let same_lval (k, l) = 
	  KinstrComparable.equal k ki && Cilutil.equals l lval
	in
	if List.exists same_lval highlight_stmt then 
          apply_tag buffer tag start stop
    | PTermLval (_,ki,term_lval) -> 
	let same_tlval (k, l) =
          Logic_const.is_same_tlval (Logic_const.lval_to_term_lval l) term_lval
	  && KinstrComparable.equal k ki 
	in
	if List.exists same_tlval highlight_stmt then
          apply_tag buffer tag start stop
    | PVDecl(_, vi') when VarinfoComparable.equal vi vi' ->
	apply_tag buffer tag start stop
    | PVDecl _ | PStmt _ -> 
	()

let occurrence_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  if button = 3 then
    match localizable with
    | PVDecl(_,vi) 
    | PLval(_, _, (Var vi, NoOffset)) 
    | PTermLval(_, _, (TVar { lv_origin = Some vi }, TNoOffset))->
	if not (Cil.isFunctionType vi.vtype) then begin
          let callback () =
	    highlight_state := !Db.Occurrence.get vi, Some vi;
            main_ui#rehighlight ()
	  in
          ignore (popup_factory#add_item "_Occurrence" ~callback)
	end
    | PLval _ | PTermLval _ | PStmt _ ->
	()

let main main_ui = 
  main_ui#register_source_selector occurrence_selector;
  main_ui#register_source_highlighter occurrence_highlighter

let () = 
  Design.register_extension main;
  Design.register_reset_extension (fun _ -> highlight_state := [], None)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

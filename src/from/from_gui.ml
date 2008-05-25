

(* $Id: from_gui.ml,v 1.2 2008/04/18 09:36:05 uid528 Exp $ *)

open Db
open Cil_types
open Cil

let main (main_ui:Design.main_window_extension_points) =

  let filetree_selector ~was_activated ~activating globals = 
    if Value.is_computed () then begin 
      if not was_activated && activating then begin match globals with
      | [GFun ({svar=v},_)] -> 
          begin try 
            let kf = Globals.Functions.get v in
            let s = fprintf_to_string "@[Functional dependencies:@\n%a@]@." !From.pretty kf in
            main_ui#annot_window#buffer#insert s
          with Not_found -> () 
          end
      | _ -> ();
      end;
    end
  in
  main_ui#file_tree#add_select_function filetree_selector


let () =
  Design.register_extension main



(** Extension of the GUI in order to support the value analysis.
    No function is exported. *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)


(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: register.ml,v 1.10 2008/11/18 07:38:00 uid568 Exp $ *)

open Cil
open Cil_types
open Db_types
open Visitor

let print_results =
  List.iter (fun s -> Format.printf "sid %d: %a@." s.sid Cil.d_stmt s)

let from_stmt s =
  let kf = snd (Kernel_function.find_from_sid s.sid) in
  !Db.Security.impact_analysis kf s

let compute_one_stmt s =
  let res = from_stmt s in
  if Cmdline.Impact.Print.get () then begin
    Format.printf "Impacted statements of stmt %d are:@." s.sid;
    print_results res
  end

let compute_pragmas () =
  let pragmas = ref [] in
  let visitor = object
    inherit Visitor.generic_frama_c_visitor
      (Project.current ()) (inplace_visit ())
    method vstmt_aux s =
      pragmas :=
	List.map
	  (fun a -> s, a)
	  (Annotations.get_filter Logic_const.is_impact_pragma s)
      @ !pragmas;
      DoChildren
  end in
  (* fill [pragmas] with all the pragmas of all the selected functions *)
  Cmdline.Impact.Pragma.iter
    (fun s ->
       try
	 match (Globals.Functions.find_def_by_name s).fundec with
	 | Definition(f, _) ->
	     ignore (visitFramacFunction visitor f)
	 | Declaration _ -> assert false
       with Not_found ->
	 Cil.log "[impact analysis] Function %s not found.@." s);
  (* compute impact analyses on [!pragmas] *)
  List.iter
    (fun (s, a) ->
       match a with
       | Before (User a) ->
	   (match a.annot_content with
	    | APragma (Impact_pragma IPstmt) -> compute_one_stmt s
	    | APragma (Impact_pragma (IPexpr _)) ->
		raise (Extlib.NotYetImplemented "impact pragmas: expr")
	    | _ -> assert false)
       | _ -> assert false)
    !pragmas

let main _fmt =
  if not (Cmdline.Impact.Pragma.is_empty ()) then 
    !Db.Impact.compute_pragmas ()

let () = Db.Main.extend main

let () =
  Db.Impact.compute_pragmas := compute_pragmas;
  Db.Impact.from_stmt := from_stmt

let () =
  Options.add_plugin
    ~name:"impact (experimental)" ~descr:"impact analysis" ~shortname:"impact"
    [ "-impact-pragma",
      Arg.String Cmdline.Impact.Pragma.add_set,
      "f1,...,fn : use the impact pragmas in the code of functions f1,...,fn\n\
      \t//@impact pragma expr <expr_desc>; : \
impact of the value from the next statement (not yet implemented)\n \
      \t//@impact pragma stmt; : impact of the next statement";

      "-impact-print",
      Arg.Unit Cmdline.Impact.Print.on,
      ": print the impacted stmt";
(*
      "-impact-slice",
      Arg.Unit Cmdline.Impact.Slicing.on,
      ": slice from the impacted stmt"*) ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

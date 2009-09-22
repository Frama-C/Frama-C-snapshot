(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: users_register.ml,v 1.17 2008-11-18 12:13:41 uid568 Exp $ *)

(**  @plugin development guide *)

include
  Plugin.Register
    (struct
       let name = "users"
       let shortname = "users"
       let descr = "function callees"
     end)

(** @plugin development guide *)
module ForceUsers =
  False
    (struct
       let option_name = "-users"
       let descr = "compute function callees"
     end)

open Db

module Users =
  Kernel_function.Make_Table
    (Kernel_function.Set.Datatype)
    (struct 
       let name = "Users" 
       let size = 17 
       let dependencies = [ Value.self; ForceUsers.self ]
     end)

let call_for_users (_state, call_stack) =
  Users.mark_as_computed ();
  match call_stack with
  | [] -> assert false
  | (current_function, _call_site) :: tail ->
      let treat_element (user, _call_site) =
	ignore 
	  (Users.memo
	     ~change:(Kernel_function.Set.add current_function)
	     (fun _ -> Kernel_function.Set.singleton current_function)
	     user)
      in
      List.iter treat_element tail

let add_value_hook () = Db.Value.Call_Value_Callbacks.extend call_for_users

let init () = if ForceUsers.get () then add_value_hook ()
let () = Cmdline.run_after_configuring_stage init

let get kf = 
  if Users.is_computed () then
    Users.find kf
  else begin
    if Db.Value.is_computed () then begin
      feedback "requiring again the computation of the value analysis";
      Project.clear
	~only:
	(Project.Selection.singleton Db.Value.self Kind.Select_Dependencies)
	()
    end else
      feedback ~level:2 "requiring the computation of the value analysis";
    add_value_hook ();
    !Db.Value.compute ();
    Users.find kf
  end

let () = Db.Users.get := get

let main () =
  if ForceUsers.get () then 
    begin
      result "====== DISPLAYING USERS ======@\n%t\
              ====== END OF USERS =========="
	(fun fmt ->
	   !Db.Semantic_Callgraph.topologically_iter_on_functions
	     (fun kf ->
		try
		  Format.fprintf fmt "@[%a: @[%a@]@]@\n"
		    Kernel_function.pretty_name kf
		    Kernel_function.Set.pretty (!Db.Users.get kf)
		with Not_found -> 
		  () (* [kf] is not called during analysis *))
	) ;
    end

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j 4"
End:
*)

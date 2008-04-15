(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* $Id: users_register.ml,v 1.17 2008/11/18 12:13:41 uid568 Exp $ *)

(**  @plugin development guide *)

open Db

module Users =
  Kernel_function.Make_Table
    (Kernel_function.Set.Datatype)
    (struct 
       let name = "Users" 
       let size = 17 
       let dependencies = [ Value.self ]
     end)

let () = Db.Users.get := Users.find

let call_for_users (_state, call_stack) =
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

let main fmt =
  if Cmdline.ForceUsers.get () then begin
    Format.fprintf fmt "@\n====== DISPLAYING USERS ======@.";
    !Db.Semantic_Callgraph.topologically_iter_on_functions
      (fun kf ->
	 try
	   Format.fprintf fmt "%a: @[%a@]@\n"
	     Kernel_function.pretty_name kf
	     Kernel_function.Set.pretty (!Db.Users.get kf)
	 with Not_found -> 
	   () (* k is not called during analysis *))
    ;
    Format.fprintf fmt "@\n====== END OF USERS ======@."
  end

let () = Db.Main.extend main

let init () =
  if Cmdline.ForceUsers.get () then
    Db.Value.Call_Value_Callbacks.extend call_for_users

let () = 
  Options.add_plugin 
    ~name:"users" ~descr:"users of functions" ~init
    [ "-users", Arg.Unit Cmdline.ForceUsers.on, 
      ": compute users (through value analysis)"; ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j 4"
End:
*)

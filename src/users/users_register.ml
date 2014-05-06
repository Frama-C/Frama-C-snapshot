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

(**  @plugin development guide *)

include
  Plugin.Register
    (struct
       let name = "users"
       let shortname = "users"
       let help = "function callees"
     end)

(** @plugin development guide *)
module ForceUsers =
  False
    (struct
       let option_name = "-users"
       let help = "compute function callees"
     end)

module Users =
  Kernel_function.Make_Table
    (Kernel_function.Hptset)
    (struct
       let name = "Users"
       let size = 17
       let dependencies = [ Db.Value.self; ForceUsers.self ]
     end)

let call_for_users (_state, call_stack) =
  match call_stack with
  | [] -> assert false
  | (current_function, _call_site) :: tail ->
      if tail = [] then begin
        (* End of Value analysis, we record that Users has run. We should not
           do this after the explicit call to Db.Value.compute later in this
           file, as Value can run on its own and execute Users while doing so.*)
         Users.mark_as_computed ()
      end;
      let treat_element (user, _call_site) =
        ignore
          (Users.memo
             ~change:(Kernel_function.Hptset.add current_function)
             (fun _ -> Kernel_function.Hptset.singleton current_function)
             user)
      in
      List.iter treat_element tail

let add_value_hook () = Db.Value.Call_Value_Callbacks.extend_once call_for_users

let init () = if ForceUsers.get () then add_value_hook ()
let () = Cmdline.run_after_configuring_stage init

let get kf =
  let find kf =
    try Users.find kf
    with Not_found -> Kernel_function.Hptset.empty
  in
  if Users.is_computed () then
    find kf
  else begin
    if Db.Value.is_computed () then begin
      feedback "requiring again the computation of the value analysis";
      Project.clear
	~selection:(State_selection.with_dependencies Db.Value.self)
        ()
    end else
      feedback ~level:2 "requiring the computation of the value analysis";
    add_value_hook ();
    !Db.Value.compute ();
    find kf
  end

let () =
  Db.register
    (Db.Journalize("Users.get",
                   Datatype.func Kernel_function.ty Kernel_function.Hptset.ty))
    Db.Users.get
    get

let print () =
  if ForceUsers.get () then
      result "@[<v>====== DISPLAYING USERS ======@ %t\
                   ====== END OF USERS =========="
        (fun fmt ->
           !Db.Semantic_Callgraph.topologically_iter_on_functions
             (fun kf ->
               let callees = !Db.Users.get kf in
               if not (Kernel_function.Hptset.is_empty callees) then
                 Format.fprintf fmt "@[<hov 4>%a: %a@]@ "
                   Kernel_function.pretty kf
                   (Pretty_utils.pp_iter
                      ~pre:"" ~sep:"@ " ~suf:"" Kernel_function.Hptset.iter
                      Kernel_function.pretty)
                   callees))

let print_once, _self_print =
  State_builder.apply_once "Users_register.print" [ Users.self ] print

let () = Db.Main.extend print_once

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

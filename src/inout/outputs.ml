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

open Cil_types
open Cil
open Db
open Db_types
open Pretty
open Locations

let call_stack = Stack.create ()
exception Ignore

class do_it = object(self)
  inherit nopCilVisitor as super
  val mutable current_stmt = Kglobal
  val mutable outs = Zone.bottom

  method set_current_stmt s = Kstmt s

  method result = outs

  method vstmt s =
    current_stmt <- Kstmt s;
    match s.skind with
        UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_) ->
               ignore(visitCilStmt (self:>cilVisitor) stmt))
          seq;
          SkipChildren
      | _ -> super#vstmt s

  method join new_ =
    outs <- Zone.join new_ outs;

  method do_assign lv =
    let loc = !Value.lval_to_loc ~with_alarms:CilE.warn_none_mode current_stmt lv in
(*    Format.printf "out: loc=%a@\n" pretty loc;*)
    if not (Location_Bits.equal loc.loc Location_Bits.bottom)
    then
      begin
        if Location_Bits.equal
          loc.loc
          Location_Bits.top
        then begin
          warn "Problem with %a" !Ast_printer.d_lval lv;
          let state = Value.get_state current_stmt in
          warn "Value at this point:@\n%a@\n"
            Value.pretty_state
            state
          (*assert false*)
        end;
        let bits_loc = valid_enumerate_bits loc in
        (* Format.printf "Adding %a===@\n" Zone.pretty bits_loc;*)
        self#join bits_loc
      end

  method vinst i =
    begin match i with
      | Set (lv,_,_) -> self#do_assign lv
      | Call (lv_opt,exp,_,_) ->
          (match lv_opt with None -> ()
             | Some lv -> self#do_assign lv);
          let _, callees =
	    !Value.expr_to_kernel_function
	      ~with_alarms:CilE.warn_none_mode ~deps:None current_stmt exp
	  in
          List.iter (fun kf -> self#join (!Db.Outputs.get_external kf)) callees
      | _ -> ()
    end;
    SkipChildren

end

let statement stmt =
  let computer = new do_it in
  ignore (visitCilStmt (computer:>cilVisitor) stmt);
  computer#result

module Internals =
  Kf_state.Make
    (struct
       let name = "internal_outs"
       let dependencies = [ Value.self ]
     end)

let get_internal =
  Internals.memo
    (fun kf ->
       !Value.compute ();
       match kf.fundec with
       | Definition (f,_) ->
           (try
	      Stack.iter
		(fun g -> if kf == g then begin
                   warn
		     "recursive call detected during out analysis of %a. Ignoring it is safe if the value analysis suceeded without problem."
		     Kernel_function.pretty_name kf;
                   raise Ignore
                 end
		)
		call_stack;

	      (* No out to compute if the values were not computed for [kf] *)
	      (* if not (Value.is_accessible kf) then raise Ignore; *)

	      Stack.push kf call_stack;
	      let computer = new do_it in
	      ignore (visitCilFunction (computer:>cilVisitor) f);
	      let _ = Stack.pop call_stack in
	      computer#result
		(*
		  let initial_stmt = find_first_stmt kf in
		  let initial_state = Value.get_state initial_stmt in
		  let inputs = InOutContext.get_over_input_context
		  (!InOutContext.get_external kf)
		  in
		  let out_bases =
		  Zone.fold_bases
		  BaseUtils.BaseSet.add
		  res
		  BaseUtils.BaseSet.empty
		  in
		  let access_path = Access_path.compute initial_state out_bases in
		  let access_path = Access_path.filter access_path inputs in
		*)
		(*TODO*)
            with Ignore ->
	      Zone.bottom)
       | Declaration (_,_,_,_) ->
           let behaviors =
	     !Value.valid_behaviors kf (Value.get_initial_state kf)
           in
           let assigns = Ast_info.merge_assigns behaviors in
           (try
              let state = Value.get_initial_state kf in
              List.fold_left
                (fun acc (loc,_) ->
                   match loc with
                       Location loc ->
                         if (Logic_const.tsets_is_result loc.its_content)
                         then acc
                         else
                           List.fold_left
                             (fun acc lval ->
 		                Zone.join acc
                                  (!Value.lval_to_zone_state state lval))
                             acc (!Properties.Interp.tsets_to_lval
                                    loc.its_content)
                     | Nothing -> acc
                )
                Zone.bottom assigns
            with Invalid_argument "not a lvalue" ->
              warn "unsupported assigns clause for function %a; Ignoring it."
                Kernel_function.pretty_name kf;
              Zone.bottom)
    )

let externalize kf x =
  if Kernel_function.is_definition kf then
    let fundec = Kernel_function.get_definition kf in
    Zone.filter_base
      (fun v -> not (Base.is_formal_or_local v fundec))
      x
  else
    x

module Externals =
  Kf_state.Make
    (struct
       let name = "external_outs"
       let dependencies = [ Internals.self ]
     end)

let get_external =
  Externals.memo (fun kf -> externalize kf (get_internal kf))

let pretty_internal fmt kf =
  try
    Format.fprintf fmt "@[Out (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
      Kernel_function.pretty_name kf
      Zone.pretty (get_internal kf)
  with Not_found ->
    ()

(* unused :
let pretty_external fmt kf =
  match kf.external_out with
  | Some o ->
      Format.fprintf fmt "@[Out for function %s:@\n@[<hov 2>  %a@]@]@\n"
        (get_name kf)
        Zone.pretty o
  | None -> ()
*)

(* unused:
let display () = iter_on_functions (pretty_internal Format.std_formatter)
*)

let () =
  Db.Outputs.self_internal := Internals.self;
  Db.Outputs.self_external := Externals.self;
  Db.Outputs.get_internal := get_internal;
  Db.Outputs.get_external := get_external;
  Db.Outputs.compute := (fun kf -> ignore (get_internal kf));
  Db.Outputs.display := pretty_internal;
  Db.Outputs.statement := statement

let option =
  "-out",
  Arg.Unit Cmdline.ForceOut.on,
  ": force internal out display; this is an over-approximation of the set of written tsets "

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

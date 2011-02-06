(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Cil_types
open Cil
open Db
open Db_types
open Locations
open Abstract_value
open Abstract_interp

let call_stack = Stack.create ()
exception Ignore

class do_it = object(self)
  inherit nopCilVisitor as super
  val mutable derefs = Zone.bottom

  method result = derefs

  method join new_ =
    derefs <- Zone.join new_ derefs;

(*  method vstmt s =
    DoChildren
*)

  method vlval (base,_ as lv) =
    begin match base with
      | Var _ -> ()
      | Mem e ->
	  let state = 
	    Value.get_state 
	      (Kstmt (Cilutil.out_some self#current_stmt)) 
	  in
	  let r = !Value.eval_expr  ~with_alarms:CilE.warn_none_mode state e in
	  self#join (valid_enumerate_bits (loc_without_size_to_loc lv r))
    end;
    DoChildren

end

let statement stmt =
  let computer = new do_it in
  ignore (visitCilStmt (computer:>cilVisitor) stmt);
  computer#result

module Internals = 
  Kf_state.Make
    (struct 
       let name = "Internal derefs" 
       let dependencies = [ Value.self ]
       let kind = `Correctness
     end)

let get_internal =
  Internals.memo
    (fun kf ->
      match kf.fundec with
      | Definition (f,_) ->
          (try
             Stack.iter
               (fun g -> if kf == g then begin
                  Cil.warn
                    "recursive call detected during deref analysis of %a. Ignoring it is safe if the value analysis suceeded without problem."
                    Kernel_function.pretty_name kf;
                  raise Ignore
                end
               )
               call_stack;

             (* No deref to compute if the values were not computed for [kf] *)
             (* if not (Value.is_accessible kf) then raise Ignore; *)

             Stack.push kf call_stack;
             let computer = new do_it in
             ignore (visitCilFunction (computer:>cilVisitor) f);
             let _ = Stack.pop call_stack in
             computer#result
           with Ignore -> 
	     Zone.bottom)
      | Declaration _ -> 
	  Zone.bottom)

let externalize _return fundec x =
  Zone.filter_base
    (fun v -> not (Base.is_formal_or_local v fundec))
    x

module Externals = 
  Kf_state.Make
    (struct 
       let name = "External derefs" 
       let dependencies = [ Internals.self ]
       let kind = `Correctness
     end)

let get_external =
  Externals.memo 
    (fun kf -> 
       !Value.compute ();
       if Kernel_function.is_definition kf then
	 externalize 
	   (Kernel_function.find_return kf) 
	   (Kernel_function.get_definition kf) 
	   (get_internal kf)
       else
	 (* assume there is no deref for leaf functions *)
	 Zone.bottom)

let compute_external kf = ignore (get_external kf)

let pretty_internal fmt kf =
  Format.fprintf fmt "@[Derefs (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty_name kf
    Zone.pretty (get_internal kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[Derefs for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty_name kf
    Zone.pretty (get_external kf)

let () =
  Db.Derefs.self_internal := Internals.self;
  Db.Derefs.self_external := Externals.self;
  Db.Derefs.get_internal := get_internal;
  Db.Derefs.get_external := get_external;
  Db.Derefs.compute := compute_external;
  Db.Derefs.display := pretty_external;
  Db.Derefs.statement := statement

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

(* $Id: memzone.ml,v 1.21 2008/04/01 09:25:21 uid568 Exp $ *)

open Memzone_type
open Cil_types
open Cil
open Db
open Pretty
open Locations
open Abstract_value
open Abstract_interp

class do_it = object(self)
  inherit nopCilVisitor as super
  val mutable memzones = Lmap_bitwise_with_empty_default.empty

  val mutable state = Relations_type.Model.bottom
  method set_state s = state <- s

  method result = memzones
   
  method join accessed =
    let there = Lmap_bitwise_with_empty_default.find memzones accessed in
    let union = Zone_with_empty_default.join there accessed in
    memzones <- 
      Lmap_bitwise_with_empty_default.add_binding ~exact:true
      memzones
      union
      union

  method vstmt s = 
    match s.skind with 
    | Instr _ -> DoChildren
    | Switch (exp, _, _, _) 
    | If (exp, _, _, _)
    | TryExcept (_, (_,exp), _, _) -> 
        ignore (self#vexpr exp);
        SkipChildren
    | Return ((None|Some (Lval (Var _,_))),_)
    | TryFinally (_, _, _) 
    | Block _ | UnspecifiedSequence _
    | Loop (_, _, _, _, _) 
    | Continue _|Break _|Goto (_, _) -> SkipChildren 
    | Return _ -> assert false

  method vlval (base,_offset as lv) =
    begin match base with
      | Var _host -> ()
      | Mem e -> 
          let r = !Value.eval_expr ~with_alarms:CilE.warn_none_mode state e in
          (*Format.printf "Got Mem:%a@\n" Location_Bytes.pretty r;*)
	  self#join 
	    (valid_enumerate_bits
	       (loc_without_size_to_loc lv r))
    end;
    DoChildren 

(*TODO
  method vexp exp = 
    begin match exp with 
    | Binop (e1,e2,Eq,_) -> 
        let v1 = Cvalue.eval_expr ~with_alarms:false state e1 in
        let v2 = Cvalue.eval_expr ~with_alarms:false state e2 in
  FAIRE UNE LOC AVEC TOUTE LA BASE PUIS JOIN
  | _ -> ()
    end;
    DoChildren
*)        
end
  

let computer = new do_it
let compute () = computer#result
let on_values kinstr state = 
  if Cmdline.ForceMemzones.get () then begin
    match kinstr with 
    | Kglobal -> ()
    | Kstmt s -> 
        (*Format.printf "GOT(%d):%a@\n" 
          s.sid Relations_type.Model.pretty state;*)
        computer#set_state state;
        ignore (visitCilStmt (computer:>cilVisitor) s)
  end
    
let () = 
  Db.Memzone.compute := compute;
  Db.Memzone.pretty := Lmap_bitwise_with_empty_default.pretty
(*  ; Db.Value.register_record_value_callback on_values : le type a changé
   pascal 07/2007*)

let () = 
  Options.add_plugin 
    ~name:"memzones (experimental)" ~shortname:"memzones"
    ~descr:""
    ["-memzones",
     Arg.Bool Cmdline.ForceMemzones.set,
     ": force memzones display (undocumented)";]

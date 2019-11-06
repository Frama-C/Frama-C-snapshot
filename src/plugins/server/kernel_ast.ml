(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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


open Data
module Sy = Syntax
module Md = Markdown
module Js = Yojson.Basic.Util
open Cil_types

let page = Doc.page `Kernel ~title:"Ast Services" ~filename:"ast.md"

(* -------------------------------------------------------------------------- *)
(* --- Compute Ast                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () = Request.register ~page
    ~kind:`EXEC ~name:"kernel.ast.compute"
    ~descr:(Md.plain "Ensures that AST is computed")
    ~input:(module Junit) ~output:(module Junit) Ast.compute

(* -------------------------------------------------------------------------- *)
(* ---  Printers                                                          --- *)
(* -------------------------------------------------------------------------- *)

module Tag =
struct

  open Printer_tag

  type index = (string,localizable) Hashtbl.t

  let kid = ref 0

  let index () = Hashtbl.create 0

  module TYPE : Datatype.S with type t = index =
    Datatype.Make
      (struct
        type t = index
        include Datatype.Undefined
        let reprs = [index()]
        let name = "Server.Jprinter.Index"
        let mem_project = Datatype.never_any_project
      end)

  module STATE = State_builder.Ref(TYPE)
      (struct
        let name = "Server.Jprinter.State"
        let dependencies = []
        let default = index
      end)

  let of_stmt s = Printf.sprintf "#s%d" s.sid
  let of_start s = Printf.sprintf "#k%d" s.sid
  let of_varinfo v = Printf.sprintf "#v%d" v.vid

  let create_tag = function
    | PStmt(_,st) -> of_stmt st
    | PStmtStart(_,st) -> of_start st
    | PVDecl(_,_,vi) -> of_varinfo vi
    | PLval _ -> Printf.sprintf "#l%d" (incr kid ; !kid)
    | PExp _ -> Printf.sprintf "#e%d" (incr kid ; !kid)
    | PTermLval _ -> Printf.sprintf "#t%d" (incr kid ; !kid)
    | PGlobal _ -> Printf.sprintf "#g%d" (incr kid ; !kid)
    | PIP _ -> Printf.sprintf "#p%d" (incr kid ; !kid)

  let create item =
    let tag = create_tag item in
    let index = STATE.get () in
    Hashtbl.add index tag item ; tag

  let lookup = Hashtbl.find (STATE.get())

end

module PP = Printer_tag.Make(Tag)

(* -------------------------------------------------------------------------- *)
(* --- Ast Data                                                           --- *)
(* -------------------------------------------------------------------------- *)

module Stmt = Data.Collection
    (struct
      type t = stmt
      let syntax = Sy.publish ~page ~name:"stmt"
          ~synopsis:Sy.ident
          ~descr:(Md.plain "Code statement identifier") ()
      let to_json st = `String (Tag.of_stmt st)
      let of_json js =
        let id = Js.to_string js in
        try
          let open Printer_tag in
          match Tag.lookup id with
          | PStmt(_,st) -> st
          | _ -> raise Not_found
        with Not_found ->
          Data.failure "Unknown stmt id: '%s'" id
    end)

module Ki = Data.Collection
    (struct
      type t = kinstr
      let syntax = Sy.union [ Sy.tag "global" ; Stmt.syntax ]
      let to_json = function
        | Kglobal -> `String "global"
        | Kstmt st -> `String (Tag.of_stmt st)
      let of_json = function
        | `String "global" -> Kglobal
        | js -> Kstmt (Stmt.of_json js)
    end)

module Kf = Data.Collection
    (struct
      type t = kernel_function
      let syntax = Sy.publish ~page ~name:"fct-id"
          ~synopsis:Sy.ident
          ~descr:(Md.plain "Function identified by its global name.") ()
      let to_json kf =
        `String (Kernel_function.get_name kf)
      let of_json js =
        let key = Js.to_string js in
        try Globals.Functions.find_by_name key
        with Not_found -> Data.failure "Undefined function '%s'" key
    end)

(* -------------------------------------------------------------------------- *)
(* --- Functions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let () = Request.register ~page
    ~kind:`GET ~name:"kernel.ast.getFunctions"
    ~descr:(Md.plain "Collect all functions in the AST")
    ~input:(module Junit) ~output:(module Kf.Jlist)
    begin fun () ->
      let pool = ref [] in
      Globals.Functions.iter (fun kf -> pool := kf :: !pool) ;
      List.rev !pool
    end

let () = Request.register ~page
    ~kind:`GET ~name:"kernel.ast.printFunction"
    ~descr:(Md.plain "Print the AST of a function")
    ~input:(module Kf) ~output:(module Jtext)
    (fun kf -> Jbuffer.to_json PP.pp_global (Kernel_function.get_global kf))

(* -------------------------------------------------------------------------- *)

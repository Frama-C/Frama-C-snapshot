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

let page = Doc.page `Kernel ~title:"Project Management" ~filename:"project.md"

(* -------------------------------------------------------------------------- *)
(* --- Project Info                                                       --- *)
(* -------------------------------------------------------------------------- *)

module ProjectInfo =
  Collection
    (struct
      type t = Project.t

      let syntax = Sy.publish ~page ~name:"project-info"
          ~descr:(Md.plain "Project informations")
          ~synopsis:Sy.(record[ "id",ident; "name",string; "current",boolean ])
          ()

      let of_json js =
        Js.member "id" js |> Js.to_string |> Project.from_unique_name

      let to_json p =
        `Assoc [
          "id", `String (Project.get_unique_name p) ;
          "name", `String (Project.get_name p) ;
          "current", `Bool (Project.is_current p) ;
        ]
    end)

(* -------------------------------------------------------------------------- *)
(* --- Project Requests                                                   --- *)
(* -------------------------------------------------------------------------- *)

module ProjectRequest =
struct

  type t = Project.t * string * json

  let syntax = Sy.publish ~page ~name:"project-request"
      ~synopsis:(Sy.(record[ "project",ident; "request",string; "data",any; ]))
      ~descr:(Md.plain "Request to be executed on the specified project.") ()

  let of_json js =
    begin
      Project.from_unique_name Js.(member "project" js |> to_string) ,
      Js.(member "request" js |> to_string) ,
      Js.(member "data" js)
    end

  let process kind (project,request,data) =
    match Main.find request with
    | Some(kd,handler) when kd = kind -> Project.on project handler data
    | Some _ -> failwith (Printf.sprintf "Incompatible kind for '%s'" request)
    | None -> failwith (Printf.sprintf "Request '%s' undefined" request)

end

(* -------------------------------------------------------------------------- *)
(* --- Project Requests                                                   --- *)
(* -------------------------------------------------------------------------- *)

let () = Request.register ~page
    ~kind:`GET ~name:"kernel.project.getCurrent"
    ~descr:(Md.plain "Returns the current project")
    ~input:(module Junit) ~output:(module ProjectInfo)
    Project.current

let () = Request.register ~page
    ~kind:`SET ~name:"kernel.project.setCurrent"
    ~descr:(Md.plain "Switches the current project")
    ~input:(module Jident) ~output:(module Junit)
    (fun pid -> Project.(set_current (from_unique_name pid)))

let () = Request.register ~page
    ~kind:`GET ~name:"kernel.project.getList"
    ~descr:(Md.plain "Returns the list of all projects")
    ~input:(module Junit) ~output:(module ProjectInfo.Jlist)
    (fun () -> Project.fold_on_projects (fun ids p -> p :: ids) [])

let () = Request.register ~page
    ~kind:`GET ~name:"kernel.project.getOn"
    ~descr:(Md.plain "Execute a GET request within the given project")
    ~input:(module ProjectRequest) ~output:(module Jany)
    (ProjectRequest.process `GET)

let () = Request.register ~page
    ~kind:`SET ~name:"kernel.project.setOn"
    ~descr:(Md.plain "Execute a SET request within the given project")
    ~input:(module ProjectRequest) ~output:(module Jany)
    (ProjectRequest.process `SET)

let () = Request.register ~page
    ~kind:`EXEC ~name:"kernel.project.execOn"
    ~descr:(Md.plain "Execute an EXEC request within the given project")
    ~input:(module ProjectRequest) ~output:(module Jany)
    (ProjectRequest.process `EXEC)

(* -------------------------------------------------------------------------- *)

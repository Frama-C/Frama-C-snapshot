(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Log

let scope = function
  | None -> "Global"
  | Some s ->
      Printf.sprintf "%s:%d"
        (Filepath.pretty s.Lexing.pos_fname) s.Lexing.pos_lnum

type w = Log.event
type t =
  { widget: (int*w) Wtable.columns;
    append : event -> unit;
    clear : unit -> unit;}

module Data = Indexer.Make(
  struct
    type t = int*w
    let compare (x,_) (y,_) = Pervasives.compare x y
  end)

let make ~packing ~callback =
  let model = object(self)
    val mutable m = Data.empty
    val mutable age = 0
    method data = m
    method size =  Data.size m
    method index i = Data.index i m
    method get i = Data.get i m
    method add i = age<-age+1; m <- Data.add (age,i) m;age,i
    method reload = age<-0; m <- Data.empty
    method coerce = (self:> (int*w) Wtable.listmodel)
  end
  in
  let w = new Wtable.list
    ~packing ~headers:true ~rules:true model#coerce
  in
  let append e = w#insert_row (model#add e)
  in
  let clear () =
    (* Post a reload request before clearing.
       The current model is used to know how many rows
       must be deleted. *)
    w#reload ;
  in
  let _ = w#add_column_pixbuf ~title:"Kind" [`YALIGN 0.0;`XALIGN 0.5]
      (fun (_,e) -> match e with
         | {evt_kind=Error} -> [`STOCK_ID "gtk-dialog-error"]
         | {evt_kind=Warning} -> [`STOCK_ID  "gtk-dialog-warning"]
         | _ -> [`STOCK_ID "gtk-dialog-info"])
  in
  let _ = w#add_column_text ~title:"Source" [`YALIGN 0.0]
      (fun (_,{evt_source=src}) -> [`TEXT (scope src)])
  in
  let _ = w#add_column_text ~title:"Plugin" [`YALIGN 0.0]
      (fun (_,{evt_plugin=m}) -> [`TEXT m])
  in
  let _ = w#add_column_text ~title:"Message" [`YALIGN 0.0]
      (fun (_,{evt_message=m}) -> [`TEXT m])
  in
  w#on_click (fun (_,w) c -> callback w c);
  {widget=w;append=append;clear=clear}

let append t message = t.append message

let clear t = t.clear ()

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

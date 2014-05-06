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

(* ------------------------------------------------------------------------ *)
(* ---  Forms Factory                                                   --- *)
(* ------------------------------------------------------------------------ *)

type demon = (unit -> unit) list ref
let demon () = ref []
let register demon f = demon := !demon @ [f]
let refresh demon () =
  List.iter
    (fun f -> try f() with _ -> ())
    !demon

(* ------------------------------------------------------------------------ *)
(* --- Utilities                                                        --- *)
(* ------------------------------------------------------------------------ *)

type 'a field =
    ?tooltip:string -> packing:(GObj.widget -> unit) ->
  (unit -> 'a) -> ('a -> unit) -> demon -> unit

let mk_tooltip ?tooltip obj = match tooltip with
  | None -> ()
  | Some text ->
    let tooltip = GData.tooltips () in
    tooltip#set_tip ~text obj#coerce

(* ------------------------------------------------------------------------ *)
(* --- Check Button                                                     --- *)
(* ------------------------------------------------------------------------ *)

let check ?label ?tooltip ~packing get set demon =
  let button =
    GButton.check_button ?label ~packing ~active:(get ()) ()
  in
  mk_tooltip ?tooltip button ;
  ignore (button#connect#toggled ~callback:(fun () -> set button#active));
  register demon (fun () -> button#set_active (get()))

(* ------------------------------------------------------------------------ *)
(* --- Menu Button                                                      --- *)
(* ------------------------------------------------------------------------ *)

let menu entries ?width ?tooltip ~packing get set demon =
  let strings = List.map fst entries in
  let combo_box, (_model, column) =
    GEdit.combo_box_text
      ~strings ?width ~wrap_width:1 ~packing ()
  in
  let callback () =
    try
      match combo_box#active_iter with
        | None -> ()
        | Some row ->
            let title = (combo_box#model#get ~row ~column) in
            let (_,item) = List.find (fun (t,_) -> t=title) entries in
            set item
    with Not_found -> ()
  in
  let rec lookup k item = function
    | [] -> raise Not_found
    | (_,value) :: entries ->
        if value = item then k else lookup (succ k) item entries
  in
  let update () =
    try combo_box#set_active (lookup 0 (get ()) entries)
    with Not_found -> ()
  in
  ignore (combo_box#connect#changed callback) ;
  mk_tooltip ?tooltip combo_box ;
  register demon update

(* ------------------------------------------------------------------------ *)
(* ---  Spinner                                                         --- *)
(* ------------------------------------------------------------------------ *)

let spinner ?(lower=0) ?(upper=max_int) ?width ?tooltip ~packing get set demon =
  let spin = GEdit.spin_button ~digits:0 ?width ~packing () in
  spin#adjustment#set_bounds
    ~lower:(float lower) ~upper:(float upper) ~step_incr:1. () ;
  let callback () = 
    let a = spin#value_as_int in
    let b = get () in
    if a<>b then set a in
  let update () = spin#adjustment#set_value (float (get ())) in
  ignore (spin#connect#value_changed ~callback) ;
  mk_tooltip ?tooltip spin ;
  register demon update

(* ------------------------------------------------------------------------ *)
(* ---  Forms                                                           --- *)
(* ------------------------------------------------------------------------ *)

class form ~packing =
object

  val table = GPack.table ~rows:2 ~col_spacings:8 ~packing ()
  val mutable top = 0

  method label text =
    ignore (GMisc.label ~text
              ~packing:(table#attach ~top ~left:0 ~expand:`NONE) ())

  method item obj =
    table#attach ~top ~left:1 ~expand:`X ~fill:`X obj ;
    top <- succ top

  method row obj =
    table#attach ~top ~left:0 ~right:2 ~expand:`X ~fill:`X obj ;
    top <- succ top

end

let label ~text ~packing () =
  ignore (GMisc.label ~xpad:3 ~text ~packing ())

let button ~label ?tooltip ~callback ~packing () =
  let b = GButton.button ~label ~packing () in
  mk_tooltip ?tooltip b ;
  ignore (b#connect#clicked ~callback)

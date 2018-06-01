(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open Design

let all_alarms () =
  let all_red_statuses = Red_statuses.get_all () in
  let all = List.map
      (fun (kinstr, ap, number) ->
         let kf_name = match kinstr with
           | Kstmt stmt ->
             Kernel_function.(get_name (find_englobing_kf stmt))
           | Kglobal ->
             match ap with
             | Red_statuses.Alarm _ -> "<global>"
             | Red_statuses.Prop p ->
               match Property.get_kf p with
               | Some kf -> Kernel_function.get_name kf
               | None -> "<global>"
         in
         (kf_name, kinstr, ap, number)
      )
      all_red_statuses
  in
  let kf_name_compare kfname =
    Transitioning.String.uncapitalize_ascii kfname
  in
  (* Sort by function names, then stmt, then alarms *)
  let cmp (k1, ki1, ap1, _) (k2, ki2, ap2, _) =
    let n = String.(compare (kf_name_compare k1) (kf_name_compare k2)) in
    if n <> 0 then n
    else
      let n = Cil_datatype.Kinstr.compare ki1 ki2 in
      if n <> 0 then n
      else Red_statuses.AlarmOrProp.compare ap1 ap2
  in
  List.sort cmp all

type red_alarm = {
  function_name:string;
  kind:string;
  acsl: string;
  alarm_or_prop: Red_statuses.alarm_or_property;
  (* Status here means 'final' status as emitted by all plugins. Currentlt not
     shown. *)
  ip: Property.t;
  callstacks: int; (* Number of callstacks in which the red alarm occured. *)
}

let get_predicate ca =
  match ca.annot_content with
  | AAssert (_, p) -> { p with pred_name = [] }
  | _ -> assert false

let make_red_alarm function_name ki alarm callstacks =
  let kf, stmt = match ki with
    | Kstmt s -> Kernel_function.find_englobing_kf s, s
    | Kglobal -> (* Bug in initializer. Do the same thing as the kernel. *)
      let main = fst (Globals.entry_point ()) in
      let first_stmt = Kernel_function.find_first_stmt main in
      main, first_stmt
  in
  let ca, _ = Alarms.to_annot (Kstmt stmt) alarm in
  let ip = Property.ip_of_code_annot_single kf stmt ca in
  let kind = Transitioning.String.capitalize_ascii (Alarms.get_name alarm) in
  let p = get_predicate ca in
  let acsl = Format.asprintf "@[<hov>%a@]" Cil_datatype.Predicate.pretty p in
  let alarm_or_prop = Red_statuses.Alarm alarm in
  { function_name; ip; kind; alarm_or_prop; acsl; callstacks }

let make_red_prop function_name ip callstacks =
  let kind = "property" (* TODO *) in
  let acsl = Format.asprintf "@[<hov>%a@]" Property.pretty ip in
  let alarm_or_prop = Red_statuses.Prop ip in
  { function_name; ip; kind; alarm_or_prop; acsl; callstacks }



(* Semi generic-code for the model of Gtk list of red alarms *)

type row = red_alarm

type t =
  { widget: (int*row) Wtable.columns;
    append : row -> unit;
    clear : unit -> unit;}

module Data = Indexer.Make(
  struct
    type t = int*row
    let compare (x,_) (y,_) = Pervasives.compare x y
  end)

let append t message = t.append message

let clear t = t.clear ()

let information =
  "This panel lists the properties which were invalid in at least one state \
   of the Eva analysis. The consolidated status of these properties for \
   all states may not be Invalid, but these properties should often be \
   investigated first  — either as potential true alarms, or to make the \
   analysis more precise. \n\
   It should be noted that this list depends on the state partitioning \
   performed during the analysis."

let build_list () =
  let model = object(self)
    val mutable m = Data.empty
    val mutable age = 0
    method data = m
    method size =  Data.size m
    method index i = Data.index i m
    method get i = Data.get i m
    method add i = age<-age+1; m <- Data.add (age,i) m;age,i
    method reload = age<-0; m <- Data.empty
    method coerce = (self:> (int*row) Wtable.listmodel)
  end
  in
  let w = new Wtable.list ~headers:true ~rules:true model#coerce in
  let c = w#add_column_empty (* for alignment *) in
  (* Sets an help icon with an explanatory tooltip to the right of the last
     column header. *)
  let help = GMisc.image ~stock:`HELP () in
  help#misc#set_tooltip_text information;
  c#set_alignment 1.;
  c#set_widget (Some help#coerce);
  let append e = w#insert_row (model#add e) in
  let clear () =
    (* Post a reload request before clearing.
       The current model is used to know how many rows
       must be deleted. *)
    w#reload ;
  in
  let r = {widget=w; append; clear} in
(** End of generic code *)
  let props = [`YALIGN 0.0] in
  let _ = w#add_column_text ~title:"Function" props
      (function (_, {function_name}) -> [`TEXT function_name])
  in
  let _ = w#add_column_text ~title:"Kind" props
      (function (_, {kind}) -> [`TEXT kind])
  in
  let _ = w#add_column_text ~title:"Alarm" props
      (function (_, {acsl}) -> [`TEXT acsl])
  in
  let _ = w#add_column_text ~title:"Nb contexts" props
      (function (_, {callstacks}) ->
         [`TEXT (string_of_int callstacks)])
  in
  r

(* Fill the table of red alarms from scratch *)
let fill t =
  clear t;
  let alarms = all_alarms () in
  let aux (kf_name, ki, ap, cs) =
    match ap with
    | Red_statuses.Alarm a -> append t (make_red_alarm kf_name ki a cs)
    | Red_statuses.Prop ip -> append t (make_red_prop kf_name ip cs)
  in
  List.iter aux alarms

let make_panel (main_ui:main_window_extension_points) =
  let w = build_list () in
  w.widget#on_click
    (fun (_, {ip}) _col ->
       (* Same code is found in Design, in the callback for the
          warning_manager *)
       ignore (main_ui#scroll (Pretty_source.PIP ip));
       main_ui#view_original (Property.location ip)
    );
  let tab_label = (GMisc.label ~text:"Red Alarms" ())#coerce in
  (* This panel is automatically refreshed *)
  Design.register_reset_extension (fun _ -> fill w);
  (* Fill the table when it is created. We are probably missing a call to
     'reset' once the saved state is loaded through -load... *)
  let (_:GtkSignal.id) =
    w.widget#coerce#misc#connect#after#realize (fun () -> fill w)
  in
  (* Insert the page in the notebook, then return *)
  let n =
    main_ui#lower_notebook#append_page ~tab_label w.widget#coerce
  in
  main_ui#lower_notebook#get_nth_page n

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)

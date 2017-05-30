(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Composer Panel                                                     --- *)
(* -------------------------------------------------------------------------- *)

let peek n s =
  let rec prefix n s =
    if n <= 0 then ([],s) else
      match s with
      | [] -> raise Not_found
      | e::s ->
          let es,s = prefix (pred n) s in
          e :: es , s
  in try Some(prefix n s) with Not_found -> None

class composer (focused : GuiSequent.focused) =
  object(self)

    val mutable stack : Tactical.selection list = []
    val mutable update = (fun () -> ())

    method clear = stack <- []
    method connect f = update <- f

    method private pp_typeof fmt v =
      try Lang.F.pp_tau fmt (Lang.F.typeof (Tactical.selected v))
      with Not_found -> Format.pp_print_string fmt "?"

    method private pp_selection fmt v =
      begin
        focused#pp_selection fmt v ;
        let open Tactical in
        match v with
        | Compose(Code _) | Inside _ | Clause _ ->
            Format.fprintf fmt "@ @{<fg:grey>(%a)@}" self#pp_typeof v
        | _ -> ()
      end

    method private pp_select cc args ~quit fmt =
      match args with
      | e::_ when cc#is_valid e ->
          let callback () = cc#set_value e ; quit () in
          Format.fprintf fmt "%t @{<it>and quit composer@}@\n"
            (focused#button ~title:"Select A" ~callback)
      | _ -> ()

    method private pp_range cc args ~quit fmt =
      match args with
      | a::b::_ when cc#ranged ->
          begin
            match self#get_int a, self#get_int b with
            | Some a,Some b when a <= b ->
                let callback () =
                  cc#set_value (Tactical.range a b) ;
                  quit () in
                Format.fprintf fmt "%t @{<it>for range selection@}@\n"
                  (focused#button ~title:"Select A..B" ~callback)
            | _ -> ()
          end
      | _ -> ()


    method private pp_stack args sel fmt =
      if not (Tactical.is_empty sel) then
        let callback () =
          stack <- args ;
          ignore focused#unselect ;
          update () in
        focused#button ~title:"Stack" ~callback fmt

    method private get_int = function
      | Tactical.Compose(Tactical.Cint z) ->
          (try Some (Integer.to_int z) with _ -> None)
      | _ -> None

    method private op1 title job args fmt = match args with
      | a::w ->
          let callback () =
            stack <- job a w ; ignore focused#unselect ; update () in
          focused#button ~title ~callback fmt
      | _ -> ()

    method private op2 title job args fmt = match args with
      | a::b::w ->
          let callback () =
            stack <- job a b w ; ignore focused#unselect ; update () in
          focused#button ~title ~callback fmt
      | _ -> ()

    method private op3 title job args fmt = match args with
      | a::b::c::w ->
          let callback () =
            stack <- job a b c w ; ignore focused#unselect ; update () in
          focused#button ~title ~callback fmt
      | _ -> ()

    method private destruct args fmt = match args with
      | a::w ->
          let ps = Tactical.destruct a in
          if ps <> [] then
            let callback () =
              stack <- ps @ w ; update () in
            Format.fprintf fmt
              "%t @{<it>Decompose into (selectable) sub-terms@}@\n"
              (focused#button ~title:"Destruct A" ~callback)
      | _ -> ()

    val mutable stacked = true
    val mutable group = ""
    val mutable help = false

    method private compose fmt args (cc : Tactical.composer) =
      match peek cc#arity args with
      | None -> ()
      | Some (es,tail) ->
          let vs = List.map Tactical.selected es in
          if cc#filter vs then
            begin
              let callback () =
                let s = Tactical.compose cc#id es in
                stack <-  if es = [] then tail @ [s] else s :: tail ;
                ignore focused#unselect ;
                update () in
              let button = focused#button ~title:cc#title ~callback in
              let descr = cc#descr in
              if descr = "" then
                ( if not stacked && cc#group <> group then
                    Format.pp_print_newline fmt () ;
                  button fmt ;
                  stacked <- false )
              else
                begin
                  if not stacked then Format.pp_print_newline fmt () ;
                  Format.fprintf fmt "%t @{<it>%s@}@\n" button descr ;
                  stacked <- true
                end ;
              group <- cc#group
            end

    method private hrule fmt =
      if not stacked then Format.pp_print_newline fmt () ;
      Format.fprintf fmt "---------------------------------------@\n" ;
      stacked <- true

    method private helper fmt (cc : Tactical.composer) =
      begin
        if cc#group <> group && group <> "" then self#hrule fmt ;
        Format.fprintf fmt "[ @{<bf>%s@} ]" cc#title ;
        let descr = cc#descr in
        if descr = "" then
          if cc#arity > 0 then
            ( Format.fprintf fmt " @{<it>arity %d@}@\n" cc#arity ;
              stacked <- true )
          else stacked <- false
        else
          ( Format.fprintf fmt " @{<it>%s@}@\n" descr ;
            stacked <- true ) ;
        group <- cc#group
      end

    method private openhelp () = help <- true ; update ()
    method private closehelp () = help <- false ; update ()

    method print (cc : GuiTactic.composer) ~quit fmt =
      begin
        focused#set_target Tactical.Empty ;
        Format.fprintf fmt "@{<bf>Selection:@} @{<ul>%s@} %t%t@\n"
          cc#title
          (focused#button ~title:"Help" ~callback:self#openhelp)
          (focused#button ~title:"Cancel" ~callback:quit) ;
        let tooltip = cc#descr in
        if tooltip <> "" then Format.fprintf fmt "@\n@{<it>%s@}@\n@\n" tooltip ;
        let current = cc#get_value in
        if not (Tactical.is_empty current) then
          begin
            let clear () = cc#set_value Tactical.Empty ; quit () in
            let edit () =
              stack <-
                begin match current with
                  | Tactical.Compose(Tactical.Range(a,b)) ->
                      [ Tactical.int a ; Tactical.int b ]
                  | _ -> [current] end ;
              update () in
            Format.fprintf fmt "Current: @[<hov 2>%a@]@."
              focused#pp_selection current ;
            Format.fprintf fmt "%t @{<it>in edition stack@}@\n"
              (focused#button ~title:"Edit" ~callback:edit) ;
            Format.fprintf fmt "%t @{<it>and quit composer@}@\n"
              (focused#button ~title:"Clear" ~callback:clear) ;
          end ;
        let selection = focused#selection in
        let args =
          if not (Tactical.is_empty selection)
          then stack @ [selection] else stack in
        Format.fprintf fmt "@{<bf>Edition Stack:@}@\n@\n" ;
        Array.iteri
          (fun i v ->
             if i < 26 then
               let h = if v == selection then '>' else ' ' in
               let c = char_of_int (int_of_char 'A' + i) in
               Format.fprintf fmt "%c %c: @[<hov 2>%a@]@\n" h c
                 self#pp_selection v ;
          ) (Array.of_list args) ;
        stacked <- true ;
        group <- "" ;
        if help then
          begin
            Format.fprintf fmt
              "@\nRegistered Operations %t:@\n@\n"
              (focused#button ~title:"Close" ~callback:self#closehelp) ;
            Tactical.iter_composer (self#helper fmt) ;
          end
        else
          begin
            Format.fprintf fmt "@\n%t%t%t@\n%t%t%t%t@\n@\n%t"
              (self#pp_select cc args ~quit)
              (self#pp_range cc args ~quit)
              (self#pp_stack args selection)
              (self#op1 "Dup A" (fun a w -> a :: a :: w) args)
              (self#op1 "Drop A" (fun _ w -> w) args)
              (self#op2 "Swap A,B" (fun a b w -> b::a::w) args)
              (self#op3 "Roll A,B,C" (fun a b c w -> c::a::b::w) args)
              (self#destruct args) ;
            self#hrule fmt ;
            Tactical.iter_composer (self#compose fmt args) ;
          end
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Browser                                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec less ~wanted ~listed =
  if wanted < listed then wanted else less ~wanted:(wanted-10) ~listed

class browser (focused : GuiSequent.focused) =
  object
    val mutable wanted = 10
    val mutable listed = 0
    val mutable update = (fun () -> ())
    method connect f = update <- f
    method clear = wanted <- 10
    method print ( cc : GuiTactic.browser ) ~quit fmt =
      begin
        focused#set_target cc#target ;
        let clear () = cc#choose None ; quit () in
        Format.fprintf fmt "@{<bf>Selection for %s:@} %t%t@\n@\n"
          cc#title
          (focused#button ~title:"Clear" ~callback:clear)
          (focused#button ~title:"Cancel" ~callback:quit) ;
        let tooltip = cc#descr in
        if tooltip <> "" then Format.fprintf fmt "@{<it>%s@}@\n@\n" tooltip ;
        listed <- 0 ;
        let open Tactical in
        cc#search (fun item ->
            listed <- succ listed ;
            let title = Printf.sprintf "#%02d" listed in
            let callback () = cc#choose (Some item.vid) ; quit () in
            Format.fprintf fmt "%t %s@\n"
              (focused#button ~title ~callback) item.title ;
            if item.descr <> "" then
              Format.fprintf fmt "@{<fg:grey>%s@}@\n@\n" item.descr ;
          ) wanted ;
        if listed = 0 then
          Format.fprintf fmt "@{<it>No Result@}@\n" ;
        Format.pp_print_newline fmt () ;
        let less = less ~wanted ~listed in
        if 0 < less && less < wanted then
          begin
            let title = Printf.sprintf "1-%d" less in
            let callback () = wanted <- less ; update () in
            focused#button ~title ~callback fmt ;
          end ;
        if listed >= wanted then
          begin
            let title = Printf.sprintf "1-%d" (wanted+10) in
            let callback () = wanted <- wanted + 10 ; update () in
            focused#button ~title ~callback fmt ;
          end ;
        Format.pp_print_newline fmt () ;
      end
  end

(* -------------------------------------------------------------------------- *)

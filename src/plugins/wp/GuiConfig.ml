(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* ------------------------------------------------------------------------ *)
(* ---  Prover List in Configuration                                    --- *)
(* ------------------------------------------------------------------------ *)

class provers key =
  object(self)
    inherit [Why3.Whyconf.Sprover.t] Wutil.selector Why3.Whyconf.Sprover.empty

    method private load () =
      let open Gtk_helper.Configuration in
      let prover_of_conf acc = function
        | ConfList [ConfString prover_name;
                    ConfString prover_version;
                    ConfString prover_altern] ->
            Why3.Whyconf.Sprover.add
              Why3.Whyconf.{ prover_name; prover_version; prover_altern }
              acc
        | _ -> acc in
      try
        let data = Gtk_helper.Configuration.find key in
        match data with
        | ConfList data ->
            (List.fold_left prover_of_conf Why3.Whyconf.Sprover.empty data)
        | _ -> Why3.Whyconf.Sprover.empty
      with Not_found -> Why3.Whyconf.Sprover.empty

    method private save () =
      let open Gtk_helper.Configuration in
      let conf_of_prover dp = ConfList Why3.Whyconf.[ConfString dp.prover_name;
                                                     ConfString dp.prover_version;
                                                     ConfString dp.prover_altern] in
      Gtk_helper.Configuration.set key
        (ConfList (List.map conf_of_prover
                     (Why3.Whyconf.Sprover.elements self#get)))

    initializer
      begin
        let settings = self#load () in
        (** select automatically the provers set on the command line *)
        let cmdline = Wp_parameters.Provers.get () in
        let selection = List.fold_left
            (fun acc e ->
               match Why3Provers.find_opt e with
               | None -> acc
               | Some p -> Why3.Whyconf.Sprover.add p acc)
            settings cmdline
        in
        self#set selection ;
        self#on_event self#save ;
      end

  end

(* ------------------------------------------------------------------------ *)
(* ---  WP Provers Configuration Panel                                  --- *)
(* ------------------------------------------------------------------------ *)

class dp_chooser
    ~(main:Design.main_window_extension_points)
    ~(provers:provers)
  =
  let dialog = new Wpane.dialog
    ~title:"Why3 Provers"
    ~window:main#main_window
    ~resize:false () in
  let array = new Wpane.warray () in
  object(self)

    val mutable selected = Why3.Whyconf.Mprover.empty

    method private enable dp e =
      selected <- Why3.Whyconf.Mprover.add dp e selected

    method private lookup dp =
      Why3.Whyconf.Mprover.find dp selected

    method private entry dp =
      let text = Why3Provers.title dp in
      let sw = new Widget.switch () in
      let lb = new Widget.label ~align:`Left ~text () in
      sw#set (self#lookup dp) ;
      sw#connect (self#enable dp) ;
      let hbox = GPack.hbox ~spacing:10 ~homogeneous:false () in
      hbox#pack ~expand:false sw#coerce ;
      hbox#pack ~expand:true lb#coerce ;
      (object
        method widget = hbox#coerce
        method update () = sw#set (self#lookup dp)
        method delete () = ()
      end)

    method private configure dps =
      begin
        array#set (Why3.Whyconf.Sprover.elements dps) ;
        array#update () ;
      end

    method private detect () =
      begin
        self#configure (Why3Provers.provers_set ());
      end

    method private apply () =
      provers#set
        (Why3.Whyconf.Mprover.map_filter
           (function
             | true -> Some ()
             | false -> None)
           selected)

    method run () =
      let dps = Why3Provers.provers_set () in
      let sel = provers#get in
      selected <- Why3.Whyconf.Mprover.merge
          (fun _ avail enab ->
             match avail, enab with
             | None, _ -> None
             | Some (), Some () -> Some true
             | Some (), None -> Some false)
          dps sel;
      self#configure dps ;
      dialog#run ()

    initializer
      begin
        dialog#button ~action:(`ACTION self#detect) ~label:"Detect Provers" () ;
        dialog#button ~action:(`CANCEL) ~label:"Cancel" () ;
        dialog#button ~action:(`APPLY) ~label:"Apply" () ;
        array#set_entry self#entry ;
        dialog#add_block array#coerce ;
        dialog#on_value `APPLY self#apply ;
      end

  end

(* ------------------------------------------------------------------------ *)

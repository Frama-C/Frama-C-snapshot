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

(** The traditional 'Hello world!' plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-hello".
    When this option is set it just pretty prints a message on the standard
    output. *)

(** Register the new plug-in "Hello World" and provide access to some plug-in
    dedicated features. *)
module Self =
  Plugin.Register
    (struct
       let name = "Hello world"
       let shortname = "hello"
       let help = "The famous 'Hello world' plugin"
     end)

(** Register the new Frama-C option "-hello". *)
module Enabled =
  Self.False
    (struct
       let option_name = "-hello"
       let help = "pretty print \"Hello world!\""
     end)

let print () = Self.result "Hello world!"

(** The function [print] below is not mandatory: you can ignore it in a first
    reading. It provides an API for the plug-in, so that the function [run] is
    callable by another plug-in and journalized: first, each plug-in can call
    [Dynamic.get "Hello.run" (Datatype.func Datatype.unit Datatype.unit)] in
    order to call [print] and second, each call to [print] is written in the
    Frama-C journal. *)
let print =
  Dynamic.register
    ~comment:"[Dynamic.get \"Hello.run\" (Datatype.func Datatype.unit \
Datatype.unit)] calls [run] and pretty prints \"Hello world!\""
    ~plugin:"Hello"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    print

(** Print 'Hello World!' whenever the option is set. *)
let run () =  if Enabled.get () then print ()

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend run

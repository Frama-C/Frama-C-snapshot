(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: wserver.mli,v 1.3 2008/11/05 14:03:18 filliatr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

(* module [Wserver]: elementary web service *)

val f :
  string option -> int -> int -> int option ->
    (Unix.sockaddr * string list -> string -> string -> unit) -> unit
   (* [Wserver.f addr port tmout maxc g] starts an elementary
       httpd server at port [port] in the current machine. The variable
       [addr] is [Some the-address-to-use] or [None] for any of the
       available addresses of the present machine. The port number is
       any number greater than 1024 (to create a client < 1024, you must be
       root). At each connection, the function [g] is called:
       [g (addr, request) scr cont] where [addr] is the client identification
       socket, [request] the browser request, [scr] the script name (extracted
       from [request]) and [cont] the stdin contents . The function
       [g] has [tmout] seconds to answer some text on standard output.
       If [maxc] is [Some n], maximum [n] clients can be treated at the
       same time; [None] means no limit. See the example below. *)

val wprint : ('a, out_channel, unit) format -> 'a
    (* To be called to print page contents. *)

val wflush : unit -> unit
    (* To flush page contents print. *)

val http : string -> unit
    (* [Wserver.http answer] sends the http header where [answer]
       represents the answer status. If empty string, "200 OK" is assumed. *)

val encode : string -> string
    (* [Wserver.encode s] encodes the string [s] in another string
       where spaces and special characters are coded. This allows
       to put such strings in html links <a href=...>. This is
       the same encoding done by Web browsers in forms. *)

val decode : string -> string
    (* [Wserver.decode s] does the inverse job than [Wserver.code],
       restoring the initial string. *)

val extract_param : string -> char -> string list -> string
    (* [extract_param name stopc request] can be used to extract some
       parameter from a browser [request] (list of strings); [name]
       is a string which should match the beginning of a request line,
       [stopc] is a character ending the request line. For example, the
       string request has been obtained by: [extract_param "GET /" ' '].
       Answers the empty string if the parameter is not found. *)

val get_request_and_content : char Stream.t -> string list * string

val sock_in : string ref
val sock_out : string ref
val noproc : bool ref

val wserver_oc : out_channel ref

(* Example:

   - Source program "foo.ml":
        Wserver.f None 2371 60 None
           (fun _ s _ ->
              Wserver.html ""; Wserver.wprint "You said: %s...\n" s);;
   - Compilation:
        ocamlc -custom unix.cma -cclib -lunix wserver.cmo foo.ml
   - Run:
        ./a.out
   - Launch a Web browser and open the location:
        http://localhost:2368/hello   (but see the remark below)
   - You should see a new page displaying the text:
        You said: hello...

  Possible problem: if the browser says that it cannot connect to
      "localhost:2368",
  try:
      "localhost.domain:2368" (the domain where your machine is)
      "127.0.0.1:2368"
      "machine:2368"          (your machine name)
      "machine.domain:2368"   (your machine name)
      "addr:2368"             (your machine internet address)

*)

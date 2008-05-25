(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

open Errormsg
open Cil_types
open Format

type message = 
    { m_file : string;
      m_line : int;
      m_msg : string;
      m_severity : [ `Warning | `Error | `Info ] }

module Warn_table = 
  Computation.Make_Hashtbl
    (Inthash)
    (Project.Datatype.Persistent(struct type t = message end))
    (struct 
       let name = Project.Computation.Name.make "warn_table"
       let size = 17 
       let dependencies = [] 
     end)

module Warn_counter = 
  Computation.Ref
    (struct include Datatype.Int let default = 0 end)
    (struct 
       let name = Project.Computation.Name.make "warn_counter" 
       let dependencies = [] 
     end)

let depend s = Warn_table.depend s; Warn_counter.depend s

let current_warn = Buffer.create 157
let current_file = ref ""
let current_line = ref (-1)
let current_severity = ref `Warning

let clear () = 
  Warn_table.clear ();
  Warn_counter.clear ()

let iter = Warn_table.iter

let level_to_string l = match l with
| `Warning -> "Warning"
| _ -> "Unknown"

let err_formatter = ref Format.err_formatter 

let std_formatter () = err_formatter := Format.err_formatter

let collect = ref false

let collect_formatter = 
  let fmt_emit s  start length =
    Buffer.add_substring current_warn s start length
  in
  let fmt_flush () = 
    let m = Buffer.contents current_warn in
    Buffer.clear current_warn;
    if !collect then 
      let message = {m_file = !current_file;
                     m_line = !current_line;
                     m_msg = m;
                     m_severity = !current_severity}
      in
      begin 
	let c = Warn_counter.get () in
        Warn_table.add c message;
        Warn_counter.set (succ c)
      end
    else 
      begin 
        Format.pp_print_string Format.err_formatter m;
        Format.pp_print_newline  Format.err_formatter ()
      end
    
      
    (*    printf "Got msg %d@." !warn_counter; *)
  in
  Format.make_formatter fmt_emit fmt_flush

let () = pp_set_tags collect_formatter true
let () = 
  pp_set_formatter_tag_functions 
    collect_formatter
    {(pp_get_formatter_tag_functions collect_formatter ()) with
       mark_open_tag = (fun s ->
                          (match s with 
                          | "Warning" -> 
                              current_severity := `Warning;
                              if !collect then ""
                              else s
                          | "Error" -> 
                              current_severity := `Error;
                              if !collect then ""
                              else s
                          | "Info" -> 
                              current_severity := `Info;
                              if !collect then ""
                              else s
                          | _ -> 
                              try 
                                let file,loc = 
                                  Scanf.sscanf s "%d:%s"
                                    (fun d s -> s,d)
                                in
                                current_line := loc;
                                current_file := file;
                                if !collect then ""
                                else file^":"^(string_of_int loc)
                              with Scanf.Scan_failure _ -> "")^
                       (if !collect then "" else ": "));
       mark_close_tag = fun _s -> "";
    }

(*let () = 
  at_exit 
    (fun () -> 
      let co = open_out_bin "err.html" in
      let fmt = formatter_of_out_channel co in
      fprintf fmt "<TABLE BORDER=\"3\" CELLSPACING=\"1\" CELLPADDING=\"1\">
                   <CAPTION>The warnings</CAPTION>
                  ";
      Hashtbl.iter (fprintf fmt "<TR> <TD>%d</TD>%s</TR>")  warn_table;
      fprintf fmt "</TABLE>@.";
      close_out co)
*)


let enable_collect () = collect := true

let d_loc fmt f = 
  fprintf fmt "@{<%d:%s>@}" (fst f).Lexing.pos_lnum (fst f).Lexing.pos_fname 

let emit loc _level fstring = 
  fprintf collect_formatter
    ("@[%a@{<Warning>@}@[" ^^ fstring ^^ "@]@]@?") 
    d_loc loc

(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
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
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

open Cabs
open Cabshelper

(* This isn't the most efficient way to do things.
 * It would probably be better to not reparse rather
 * than keep the tokens in memory *)
 
(* In particular, most of the tokens we hold will be
   header files that we don't need *)

(* map cabslocs to token indexes *)

(* TODO: gather until end of line, then decide where to split *)

(* NOTE: If you find yourself getting lots of nomatch errors with
 * parens in them, then that may mean you are printing 
 * a cabs file that has had it's parens removed *)

let tokenmap : ((string * int),int) Hashtbl.t = Hashtbl.create 1000
let nextidx = ref 0

let gonebad = ref false

(* array of tokens and whitespace *)
let tokens = GrowArray.make 0 (GrowArray.Elem  ("",""))

let cabsloc_to_str cabsloc =
  (fst cabsloc).Lexing.pos_fname ^ ":" ^ 
    string_of_int (fst cabsloc).Lexing.pos_lnum ^ ":" ^ 
    string_of_int (fst cabsloc).Lexing.pos_cnum

let lastline = ref 0

let wraplexer_enabled lexer lexbuf = 
    let white,lexeme,token,cabsloc = lexer lexbuf in
    GrowArray.setg tokens !nextidx (white,lexeme);
    Hashtbl.add tokenmap ((fst cabsloc).Lexing.pos_fname,(fst cabsloc).Lexing.pos_cnum) !nextidx;
    nextidx := !nextidx + 1;
    token

let wraplexer_disabled lexer lexbuf = 
    let _white,_lexeme,token,_cabsloc = lexer lexbuf in
    token

let enabled = ref false

let wraplexer lexer =
    if !enabled then wraplexer_enabled lexer 
    else wraplexer_disabled lexer
    
let finalwhite = ref "\n"    
    
let setFinalWhite w = finalwhite := w 
    
let curidx = ref 0  
let noidx = -1  
let out = ref stdout
    
let setLoc cabsloc =
    if cabsloc != cabslu && !enabled then begin
        try 
            curidx := Hashtbl.find tokenmap ((fst cabsloc).Lexing.pos_fname,(fst cabsloc).Lexing.pos_cnum)
        with
            Not_found -> 
	      Cilmsg.fatal "setLoc with location for non-lexed token: %s" (cabsloc_to_str cabsloc)
    end else begin curidx := noidx; () end
    
let setOutput out_chan = 
    out := out_chan

(* TODO: do this properly *)
let invent_white () = " "

let rec chopwhite str =
    if String.length str = 0 then str 
    else if String.get str (String.length str - 1) = ' ' then
        chopwhite (String.sub str 0 (String.length str - 1))
    else if String.get str 0 = ' ' then
        chopwhite (String.sub str 1 (String.length str - 1)) 
    else str
    
let last_was_maybe = ref false    
let last_str = ref ""
    
let print str =
    let str = chopwhite str in
    if str = "" then ()
    else if !curidx == noidx || not !enabled then 
        output_string !out (invent_white() ^ str) 
    else begin
        let srcwhite,srctok = GrowArray.getg tokens !curidx in
        let white = if str = srctok 
            then srcwhite
            else if !gonebad then invent_white ()
            else begin 
              Cilmsg.warnOpt "nomatch:[%s] expected:[%s] - NOTE: cpp not supported"
		(String.escaped str) (String.escaped srctok) ;
              gonebad := true;
              invent_white ()
            end in
        if !last_was_maybe && str = !last_str then () else begin
            output_string !out (white ^ str);
            curidx := !curidx + 1
        end
    end;
    last_was_maybe := false

let printl strs = 
    List.iter print strs   
    
let printu str =
    if not !enabled then print str
    else
        let _srcwhite,srctok = GrowArray.getg tokens !curidx in
        if chopwhite str = "" then () 
        else if srctok = str 
          || srctok = str ^ "__" 
          || srctok = "__" ^ str
          || srctok = "__" ^ str ^ "__"
          then
          print srctok
        else (print_endline ("u-nomatch:["^str^"]"); print str)
                
let print_maybe str =
    if not !enabled then print str
    else
        let _srcwhite,srctok = GrowArray.getg tokens !curidx in
        if str = srctok then begin 
            print str;
            last_was_maybe := true;
            last_str := str
        end else ()


let printEOF () = output_string !out !finalwhite



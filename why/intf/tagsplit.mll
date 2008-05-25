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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

{ 
  open Lexing
  open Format
  open Colors

  let current_tags = ref []

  let buf = Buffer.create 1024

  let is_num = 
    let r_num = Str.regexp "\\([0-9]+\\)" in
    fun tag ->
      Str.string_match r_num tag 0
	  
  let used_tags = Hashtbl.create 97

  let insert_tagged_text (tbuf:GText.buffer) (tag:string) text = 
    Hashtbl.replace used_tags tag ();
    let it = tbuf#end_iter in
    let new_tag = Tags.get_gtktag tag in 
    tbuf#insert ~tags:[new_tag] ~iter:it text

  let insert_text (tbuf:GText.buffer) tag text = 
    let (fc, bc) = get_color tag 
    and it = tbuf#end_iter in
    let new_tag = tbuf#create_tag [`BACKGROUND bc; `FOREGROUND fc] in
    tbuf#insert ~tags:[new_tag] ~iter:it text

  let output tbuf () =
    let s = Buffer.contents buf in
    Buffer.reset buf;
    match !current_tags with
      | [] -> 
	  insert_text tbuf "" s
      | p :: _ -> 
	  if Hashtbl.mem Tags.gtktags p
	  then insert_tagged_text tbuf p s
	  else insert_text tbuf p s

}

let tag = ['a'-'z' 'A'-'Z' '0'-'9' '_']+

rule split tbuf = parse
  | "<" (tag as t) ">" 
      {
	output tbuf ();
	current_tags := t :: !current_tags; 
	split tbuf lexbuf 
      }
  | "</" (tag as t) ">" 
      { 
	output tbuf ();
	match !current_tags with
	  | t' :: ct -> 
	      assert (t' = t); 
	      current_tags := ct; 
	      split tbuf lexbuf 
	  | [] -> 
	      assert false 
      }
  | [^ '<']* as s
      { Buffer.add_string buf s; split tbuf lexbuf  }
  | _ as c
      { Buffer.add_char buf c; split tbuf lexbuf }
  | eof 
      { output tbuf () }


{
  let split buf lb =
    Hashtbl.clear used_tags;
    split buf lb;
    Hashtbl.fold (fun t _ l -> t :: l) used_tags []
}

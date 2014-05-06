(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

open Cabs

let nextident = ref 0
let getident () =
    nextident := !nextident + 1;
    !nextident

let currentLoc () = Errorloc.getPosition ()

let cabslu = Lexing.dummy_pos,Lexing.dummy_pos

let continue_annot _ job default msg =
  try
    Cilmsg.push_errors () ;
    let result = job () in
    if Cilmsg.had_errors () then failwith "Annotation has errors" ;
    Cilmsg.pop_errors () ;
    Log.with_null (fun _ -> result) msg ;
  with exn when Kernel.ContinueOnAnnotError.get () ->
    Kernel.debug "Continue on annotation error (%s)" (Printexc.to_string exn) ;
    Cilmsg.pop_errors ();
    Log.with_null (fun _ -> default ()) msg

module Comments =
  struct
    module MapDest = struct
      include Datatype.List(Datatype.Pair(Cil_datatype.Position)(Datatype.String))
      let fast_equal (_:t) (_:t) = false
    end
    module MyTable = 
      Rangemap.Make 
        (Cil_datatype.Position)
        (MapDest)
    module MyState =
      State_builder.Ref
        (MyTable)
        (struct
          let name = "Cabshelper.Comments"
          let dependencies = [ ]
          (* depends from File.self and Ast.self which add 
             the dependency themselves. *)
          let default () = MyTable.empty
         end)
    let self = MyState.self

    (* What matters is the beginning of the comment. *)
    let add (first,last) comment =
      let state = MyState.get () in
      let acc = try MyTable.find first state with Not_found -> [] in
      MyState.set ((MyTable.add first ((last,comment)::acc)) state)

    let get (first,last) =
      Kernel.debug "Searching for comments between positions %a and %a@."
        Cil_datatype.Position.pretty first
        Cil_datatype.Position.pretty last;
      MyTable.fold_range
        (fun pos ->
          match Cil_datatype.Position.compare first pos with
            | n when n > 0 -> Rangemap.Below
            | 0 -> Rangemap.Match
            | _ ->
              if Cil_datatype.Position.compare pos last <= 0 then 
                Rangemap.Match
              else
                Rangemap.Above)
        (fun _ comments acc -> acc @ List.rev_map snd comments)
        (MyState.get ())
        []
      
    let iter f =
      MyTable.iter 
        (fun first comments ->
          List.iter (fun (last,comment) -> f (first,last) comment) comments)
        (MyState.get())

    let fold f acc =
      MyTable.fold
        (fun first comments acc ->
          List.fold_left
            (fun acc (last,comment) -> f (first,last) comment acc) acc comments)
        (MyState.get()) acc
      
end

(*********** HELPER FUNCTIONS **********)

let missingFieldDecl = (Cil.missingFieldName, JUSTBASE, [], cabslu)

let rec isStatic = function
    [] -> false
  | (SpecStorage STATIC) :: _ -> true
  | _ :: rest -> isStatic rest

let rec isExtern = function
    [] -> false
  | (SpecStorage EXTERN) :: _ -> true
  | _ :: rest -> isExtern rest

let rec isInline = function
    [] -> false
  | SpecInline :: _ -> true
  | _ :: rest -> isInline rest

let rec isTypedef = function
    [] -> false
  | SpecTypedef :: _ -> true
  | _ :: rest -> isTypedef rest


let get_definitionloc (d : definition) : cabsloc =
  match d with
  | FUNDEF(_,_, _, l, _) -> l
  | DECDEF(_,_, l) -> l
  | TYPEDEF(_, l) -> l
  | ONLYTYPEDEF(_, l) -> l
  | GLOBASM(_, l) -> l
  | PRAGMA(_, l) -> l
  | LINKAGE (_, l, _) -> l
  | GLOBANNOT({Logic_ptree.decl_loc = l }::_) -> l
  | GLOBANNOT [] -> assert false
  | CUSTOM (_,_,l) -> l

let get_statementloc (s : statement) : cabsloc =
begin
  match s.stmt_node with
  | NOP(loc) -> loc
  | COMPUTATION(_,loc) -> loc
  | BLOCK(_,loc,_) -> loc
  | SEQUENCE(_,_,loc) -> loc
  | IF(_,_,_,loc) -> loc
  | WHILE(_,_,_,loc) -> loc
  | DOWHILE(_,_,_,loc) -> loc
  | FOR(_,_,_,_,_,loc) -> loc
  | BREAK(loc) -> loc
  | CONTINUE(loc) -> loc
  | RETURN(_,loc) -> loc
  | SWITCH(_,_,loc) -> loc
  | CASE(_,_,loc) -> loc
  | CASERANGE(_,_,_,loc) -> loc
  | DEFAULT(_,loc) -> loc
  | LABEL(_,_,loc) -> loc
  | GOTO(_,loc) -> loc
  | COMPGOTO (_, loc) -> loc
  | DEFINITION d -> get_definitionloc d
  | ASM(_,_,_,loc) -> loc
  | TRY_EXCEPT(_, _, _, loc) -> loc
  | TRY_FINALLY(_, _, loc) -> loc
  | (CODE_SPEC (_,l) |CODE_ANNOT (_,l)) -> l
end


let explodeStringToInts (s: string) : int64 list =
  let rec allChars i acc =
    if i < 0 then acc
    else allChars (i - 1) (Int64.of_int (Char.code (String.get s i)) :: acc)
  in
  allChars (-1 + String.length s) []

let valueOfDigit chr =
  let int_value =
    match chr with
      '0'..'9' -> (Char.code chr) - (Char.code '0')
    | 'a'..'z' -> (Char.code chr) - (Char.code 'a') + 10
    | 'A'..'Z' -> (Char.code chr) - (Char.code 'A') + 10
    | _ -> Kernel.fatal "not a digit"
  in
  Int64.of_int int_value


let d_cabsloc fmt cl =
  Format.fprintf fmt "%s:%d"
    (fst cl).Lexing.pos_fname
    (fst cl).Lexing.pos_lnum

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

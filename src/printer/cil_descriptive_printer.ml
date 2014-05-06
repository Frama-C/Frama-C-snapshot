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

open Cil_types

(** Like standard Cil printer, but instead of temporary variable
    names it prints the description that was provided when the temp was
    created.  This is usually better for messages that are printed for end
    users, although you may want the temporary names for debugging.  *)
class descriptive_printer = object (self)

  inherit Cil_printer.extensible_printer () as super

  val mutable temps: (varinfo * string * string option) list = []
  val mutable useTemps: bool = false

  method private pVarDescriptive fmt (vi: varinfo) =
    match vi.vdescr with
    | Some vd ->
      if vi.vdescrpure || not useTemps then
	Format.fprintf fmt "%s" vd
      else begin
	try
	  let _, name, _ = List.find (fun (vi', _, _) -> vi == vi') temps in
	  Format.fprintf fmt "%s" name
	with Not_found ->
	  let name = "tmp" ^ string_of_int (List.length temps) in
	  temps <- (vi, name, vi.vdescr) :: temps;
	  Format.fprintf fmt "%s" name
      end
    | None ->
      super#varinfo fmt vi

  (* Only substitute temp vars that appear in expressions.
     (Other occurrences of lvalues are the left-hand sides of assignments,
     but we shouldn't substitute there since "foo(a,b) = foo(a,b)"
     would make no sense to the user.)  *)
  method! exp fmt e = match e.enode with
  | Lval (Var vi, o)
  | StartOf (Var vi, o) ->
    Format.fprintf fmt "%a%a" self#pVarDescriptive vi self#offset o
  | AddrOf (Var vi, o) ->
    (* No parens needed, since offsets have higher precedence than & *)
    Format.fprintf fmt "& %a%a" self#pVarDescriptive vi self#offset o
  | _ -> super#exp fmt e

end

include Printer_builder.Make(struct class printer () = descriptive_printer end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Logic_ptree

[@@@ warning "-42"]

type slevel_annotation =
  | SlevelMerge
  | SlevelDefault
  | SlevelLocal of int

type unroll_annotation = term option

type flow_annotation =
  | FlowSplit of term
  | FlowMerge of term


(* We use two representations for annotations :
   - the high level representation (HL) which is exported from this module
   - the low level representation (Cil) which is used by the kernel to store
     any annotation

   Annotations in this module define the export and import function to go from
   one to another. Then, the parse and print functions works directly on the
   high level representation.

             add  --+
                    |
   ACSL --> parse --+--> HL --> export --> Cil --> import --+--> HL --> print
                                                            |
                                                            +--> get
*)

exception Parse_error

module type Annotation =
sig
  type t

  val name : string
  val is_loop_annot : bool
  val parse : typing_context:Logic_typing.typing_context -> lexpr list -> t
  val export : t -> acsl_extension_kind
  val import : acsl_extension_kind -> t
  val print : Format.formatter -> t -> unit
end

module Register (M : Annotation) =
struct
  include M

  let typing_ext ~typing_context ~loc args =
    try export (parse ~typing_context args)
    with Parse_error ->
      typing_context.Logic_typing.error loc "Invalid %s directive" name

  let printer_ext _pp fmt lp =
    print fmt (import lp)

  let () =
    if is_loop_annot then begin
      Logic_typing.register_code_annot_next_loop_extension name false typing_ext;
      Cil_printer.register_loop_annot_extension name printer_ext
    end else begin
      Logic_typing.register_code_annot_next_stmt_extension name false typing_ext;
      Cil_printer.register_code_annot_extension name printer_ext
    end

  let get stmt =
    let filter_add _emitter annot acc =
      match annot.annot_content with
      | Cil_types.AExtended (_, is_loop_annot', {ext_name=name'; ext_kind})
        when name' = name && is_loop_annot' = is_loop_annot ->
        import ext_kind :: acc
      | _ -> acc
    in
    List.rev (Annotations.fold_code_annot filter_add stmt [])

  let add ~emitter ~loc stmt annot =
    let param = M.export annot in
    let extension = Logic_const.new_acsl_extension "slevel" loc false param in
    let annot_node = Cil_types.AExtended ([], false, extension) in
    let code_annotation = Logic_const.new_code_annotation annot_node in
    Annotations.add_code_annot emitter stmt code_annotation
end


module Slevel = Register (struct
    type t = slevel_annotation

    let name = "slevel"
    let is_loop_annot = false

    let parse ~typing_context:_ = function
      | [{lexpr_node = PLvar "default"}] -> SlevelDefault
      | [{lexpr_node = PLvar "merge"}] -> SlevelMerge
      | [{lexpr_node = PLconstant (IntConstant i)}] ->
        let i =
          try int_of_string i
          with Failure _ -> raise Parse_error
        in
        if i < 0 then raise Parse_error;
        SlevelLocal i
      | _ -> raise Parse_error

    let export = function
      | SlevelDefault -> Ext_terms [Logic_const.tstring "default"]
      | SlevelMerge -> Ext_terms [Logic_const.tstring "merge"]
      | SlevelLocal i -> Ext_terms [Logic_const.tinteger i]

    let import = function
      | Ext_terms [{term_node}] ->
        begin match term_node with
          | TConst (LStr "default") -> SlevelDefault
          | TConst (LStr "merge") -> SlevelMerge
          | TConst (Integer (i, _)) -> SlevelLocal (Integer.to_int i)
          | _ -> SlevelDefault (* be kind. Someone is bound to write a visitor
                                  that will simplify our term into something
                                  unrecognizable... *)
        end
      | _ -> assert false

    let print fmt = function
      | SlevelDefault -> Format.pp_print_string fmt "default"
      | SlevelMerge -> Format.pp_print_string fmt "merge"
      | SlevelLocal i -> Format.pp_print_int fmt i
  end)

module SimpleTermAnnotation =
struct
  type t = term

  let parse ~typing_context = function
    | [t] ->
      let open Logic_typing in
      typing_context.type_term typing_context typing_context.pre_state t
    | _ -> raise Parse_error

  let export t =
    Ext_terms [t]

  let import = function
    | Ext_terms [t] -> t
    | _ -> assert false

  let print = Printer.pp_term
end

module OptionalTermAnnotation =
struct
  type t = term option

  let parse ~typing_context = function
    | [] -> None
    | [t] ->
      let open Logic_typing in
      Some (typing_context.type_term typing_context typing_context.pre_state t)
    | _ -> raise Parse_error

  let export t =
    Ext_terms (Extlib.list_of_opt t)

  let import = function
    | Ext_terms l -> Extlib.opt_of_list l
    | _ -> assert false

  let print = Pretty_utils.pp_opt Printer.pp_term
end

module Unroll = Register (struct
    include OptionalTermAnnotation
    let name = "unroll"
    let is_loop_annot = true
  end)

module Split = Register (struct
    include SimpleTermAnnotation
    let name = "split"
    let is_loop_annot = false
  end)

module Merge = Register (struct
    include SimpleTermAnnotation
    let name = "merge"
    let is_loop_annot = false
  end)


let get_slevel_annot stmt =
  try Some (List.hd (Slevel.get stmt))
  with Failure _ -> None

let get_unroll_annot stmt = Unroll.get stmt

let get_flow_annot stmt =
  List.map (fun a -> FlowSplit a) (Split.get stmt) @
  List.map (fun a -> FlowMerge a) (Merge.get stmt)


let add_slevel_annot = Slevel.add

let add_unroll_annot = Unroll.add

let add_flow_annot ~emitter ~loc stmt = function
  | FlowSplit annot -> Split.add ~emitter ~loc stmt annot
  | FlowMerge annot -> Merge.add ~emitter ~loc stmt annot

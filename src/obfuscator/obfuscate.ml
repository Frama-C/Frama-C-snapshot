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
open Cil_datatype

let warn kind name = 
  Options.warning ~once:true "unobfuscated %s name `%s'" kind name

let has_literal_string = ref false

class visitor = object

  inherit Visitor.frama_c_inplace

  val varinfos_visited = Varinfo.Hashtbl.create 17
  val logic_vars_visited = Logic_var.Hashtbl.create 7
  val id_pred_visited = Identified_predicate.Hashtbl.create 7

  method! vglob_aux = function
  | GType (ty,_) ->
    ty.tname <- Dictionary.fresh Obfuscator_kind.Type ty.tname; 
    Cil.DoChildren
  | GVarDecl (_, v, _) | GVar (v, _, _) | GFun ({svar = v}, _)
      when Cil.is_unused_builtin v ->
    Cil.SkipChildren
  | _ ->
    Cil.DoChildren

  method! vcompinfo ci =  
    ci.cname <- Dictionary.fresh Obfuscator_kind.Type ci.cname; 
    Cil.DoChildren

  method! venuminfo ei = 
    ei.ename <- Dictionary.fresh Obfuscator_kind.Type ei.ename; 
    Cil.DoChildren

  method! vfieldinfo fi =
    fi.fname <- Dictionary.fresh Obfuscator_kind.Field fi.fname; 
    Cil.DoChildren

  method! venumitem ei = 
    ei.einame <- Dictionary.fresh Obfuscator_kind.Enum ei.einame; 
    Cil.DoChildren

  method! vexpr e = match e.enode with
  | Const(CStr str) -> 
    has_literal_string := true;
    (* ignore the result: will be handle by hacking the pretty printer *)
    (try
       ignore (Dictionary.id_of_literal_string str)
     with Not_found -> 
       ignore (Dictionary.fresh Obfuscator_kind.Literal_string str));
    Cil.SkipChildren
  | _ -> 
    Cil.DoChildren

  method! vvdec vi =
    (* Varinfo can be visited (and obfuscated) more than once:
       functions for their declaration and definition, variables
       as parts of the type of the function, and in the body of
       the function declaration, etc. Thus we make sure that the
       obfuscator does not visit them twice *)
    if Varinfo.Hashtbl.mem varinfos_visited vi then
      Cil.SkipChildren
    else begin
      if Cil.isFunctionType vi.vtype then begin
	if vi.vname <> "main" then 
	  vi.vname <- Dictionary.fresh Obfuscator_kind.Function vi.vname
      end else begin
	let add =
          if vi.vglob then Dictionary.fresh Obfuscator_kind.Global_var
          else if vi.vformal then Dictionary.fresh Obfuscator_kind.Formal_var
          else Dictionary.fresh Obfuscator_kind.Local_var
	in
        vi.vname <- add vi.vname;
      end;
      Varinfo.Hashtbl.add varinfos_visited vi ();
      Cil.DoChildren
    end

  method! vlogic_var_decl lvi =
    match lvi.lv_kind with
    | LVGlobal | LVFormal | LVQuant | LVLocal ->
      if Logic_var.Hashtbl.mem logic_vars_visited lvi then
	Cil.SkipChildren
      else begin
	lvi.lv_name <- Dictionary.fresh Obfuscator_kind.Logic_var lvi.lv_name;
	Logic_var.Hashtbl.add logic_vars_visited lvi ();
	Cil.DoChildren
      end
    | LVC -> 
      Cil.SkipChildren

  method! vstmt_aux stmt =
    let labels = 
      List.map
	(function
	| Label(s, loc, true) -> 
	  (* only obfuscate user's labels, not Cil's ones *)
	  let s' = Dictionary.fresh Obfuscator_kind.Label s in
	  Label(s', loc, true)
	| Label(_, _, false) | Case _ | Default _ as label -> label)
	stmt.labels
    in
    stmt.labels <- labels;
    Cil.DoChildren

  method! videntified_predicate p =
    if Identified_predicate.Hashtbl.mem id_pred_visited p then
      Cil.SkipChildren
    else begin
      Identified_predicate.Hashtbl.add id_pred_visited p ();
      p.ip_name <- 
	List.map (Dictionary.fresh Obfuscator_kind.Predicate) p.ip_name;
      Cil.DoChildren
    end

  method! vterm t = 
    List.iter (fun s -> warn "term" s) t.term_name;
    Cil.DoChildren

  method! vannotation = function
  | Daxiomatic(str, _, _) ->
    warn "axiomatic" str;
    Cil.DoChildren
  | Dlemma(str, axiom, _, _, _, _) ->
    warn (if axiom then "axiom" else "lemma") str;
    Cil.DoChildren
  | _ -> 
    Cil.DoChildren

  method! vmodel_info mi = 
    warn "model" mi.mi_name;
    Cil.DoChildren

  method! vlogic_type_info_decl lti =
    warn "logic type" lti.lt_name;
    Cil.DoChildren

  method! vlogic_ctor_info_decl lci =
    warn "logic constructor" lci.ctor_name;
    Cil.DoChildren

  method! vattr = function
  | Attr(str, _) | AttrAnnot str -> 
    warn "attribute" str; 
    Cil.DoChildren

  method! vattrparam p = 
    (match p with
    | AStr str | ACons(str, _) | ADot(_, str) -> warn "attribute parameter" str
    | _ -> ());
    Cil.DoChildren

  initializer has_literal_string := false

end

let obfuscate_behaviors () =
  (* inheriting method vbehavior or vspec does not work since only a copy of the
     piece of spec is provided. *)
  Globals.Functions.iter
    (fun kf -> 
      let h = Datatype.String.Hashtbl.create 7 in
      Annotations.iter_behaviors
	(fun emitter b -> 
	  if Emitter.equal emitter Emitter.end_user 
	    && not (Cil.is_default_behavior b)
	  then begin
	    Annotations.remove_behavior ~force:true emitter kf b;
	    let new_ = Dictionary.fresh Obfuscator_kind.Behavior b.b_name in
	    Datatype.String.Hashtbl.add h b.b_name new_;
	    b.b_name <- new_;
	    Annotations.add_behaviors emitter kf [ b ];
	  end)
	kf;
      let handle_bnames iter remove add =
	iter
	  (fun emitter l ->
	    remove emitter kf l;
	    add emitter kf (List.map (Datatype.String.Hashtbl.find h) l))
	  kf
      in
      handle_bnames
	Annotations.iter_complete
	Annotations.remove_complete
	Annotations.add_complete;
      handle_bnames 
	Annotations.iter_disjoint
	Annotations.remove_disjoint
	Annotations.add_disjoint)

class obfuscated_printer () = object
  inherit Printer.extensible_printer () as super
  method! constant fmt = function
  | CStr str -> Format.fprintf fmt "%s" (Dictionary.id_of_literal_string str)
  | c -> super#constant fmt c

  method! file fmt ast =
    if !has_literal_string then begin
      let string_fmt =
	if Options.Literal_string.is_default () then fmt
	else begin
	  let file = Options.Literal_string.get () in
	  try
	    let cout = open_out file in
	    Format.formatter_of_out_channel cout
	  with Sys_error _ as exn ->
	    Options.error "@[cannot generate the literal string dictionary \
into file `%s':@ %s@]"
	      file
	      (Printexc.to_string exn);
	    fmt
	end
      in
      Format.fprintf string_fmt "\
/* *********************************************************** */@\n\
/* start of dictionary required to compile the obfuscated code */@\n\
/* *********************************************************** */@\n";
      Dictionary.pretty_kind string_fmt Obfuscator_kind.Literal_string;
      Format.fprintf string_fmt "\
/* ********************************************************* */@\n\
/* end of dictionary required to compile the obfuscated code */@\n\
/* ********************************************************* */@\n@\n";
      if fmt != string_fmt then begin
	Format.pp_print_flush string_fmt ();
	Format.fprintf fmt "\
/* include the dictionary of literal strings */@\n\
@[#include \"%s\"@]@\n@\n" 
	  (Options.Literal_string.get ())
      end
    end;
    super#file fmt ast

end

let obfuscate () =
  Dictionary.mark_as_computed ();
  obfuscate_behaviors ();
  Visitor.visitFramacFileSameGlobals 
    (new visitor :> Visitor.frama_c_visitor) 
    (Ast.get ());
  Printer.change_printer (new obfuscated_printer)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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

open Cil
open Cil_types
open Logic_const

let emitter =
  Emitter.create "Inferred annotations"
    [Emitter.Funspec; Emitter.Property_status] [] []

let assigns_from_prototype kf =
  let vi = Kernel_function.get_vi kf in
  let formals =
    try
      let formals = getFormalsDecl vi in
      (* Do ignore anonymous names *)
      List.filter (fun vi -> vi.vname <> "") formals
    with Not_found -> []
    (* this may happen for function pointer used as formal parameters.*)
  in
  let rtyp, _, _, _ = splitFunctionTypeVI vi in
  let pointer_args,basic_args =
    List.partition (fun vi -> isPointerType vi.vtype) formals in
  (* Remove args of type pointer to pointer *)
  let pointer_args =
    List.filter
      (fun vi -> not (isPointerType (typeOf_pointed vi.vtype))) pointer_args
  in
  (* Convert void* pointers to char* *)
  let pointer_args =
    List.map
      (fun vi ->
        let loc = vi.vdecl in
        let t = tvar (cvar_to_lvar vi) in
        let typ = vi.vtype in
        if Cil.isVoidPtrType typ then
          let const = hasAttribute "const" (typeAttrs (Cil.typeOf_pointed typ)) in
          let typ' = if const then Cil.charConstPtrType else Cil.charPtrType in
          (Logic_utils.mk_cast ~loc typ' t, typ')
        else (t, typ)
      ) pointer_args
  in
  (* Generate the term [*(t+(0..))] with the appropriate array bounds (if
     they are known), and possibly add some [[..]] if v has points to one or
     more arrays *)
  let mk_star (t, typ) =
    let loc = t.term_loc in
    let zero = Logic_const.tinteger ~loc 0 in
    (* Range [0..length-1], or [0..] if length is not known *)
    let make_range length = match length with
      | None -> Logic_const.trange ~loc (Some zero, None)
      | Some length ->
        let high = Logic_const.tint ~loc (Integer.pred length) in
        Logic_const.trange ~loc (Some zero, Some high)
    in
    (* Generate the required numbers of [[..]] until with find a non-array
       type *)
    let rec mk_offset set typ =
      match Cil.unrollType typ with
        | TArray (typ_elem, size, _, _) ->
          let range = match size with
            | None -> make_range None
            | Some size ->
              match (Cil.constFold true size).enode with
                | Const(CInt64(length,_,_)) -> make_range (Some length)
                | _ -> make_range None
          in
          let offs, typ = mk_offset true typ_elem in
          TIndex (range, offs), typ
        | _ ->
          TNoOffset,
          (if set then make_set_type (Ctype typ) else (Ctype typ))
    in
(* make_set_type (Ctype typ_pointed) *)

    let typ_pointed = Cil.typeOf_pointed typ in
    (* Generate the initial term: [*(t+(0..))] for array types or char*
       pointers, *t for other pointer types. It would have been better to
       recognize formals with type [typ[]] instead of [typ *], but this
       information is lost during normalization *)
    let t_range_node, set = 
      match findAttribute "arraylen" (typeAttr typ) with
        | [AInt length] -> TBinOp (PlusPI, t, make_range (Some length)), true
        | _ ->
          if Cil.isCharPtrType typ
          then TBinOp (PlusPI, t, make_range None), true
          else t.term_node, false
    in
    let offset_arrays, typ_with_offset = mk_offset true typ_pointed in
    let t_range =
      Logic_const.term ~loc t_range_node (if set then make_set_type (Ctype typ) else Ctype typ)
    in
    Logic_const.new_identified_term
      (term ~loc (TLval (TMem t_range, offset_arrays)) typ_with_offset)
  in
  let to_assign =
    List.map
      mk_star
      (List.filter
         (fun (_t, typ) ->
            let pointed_type = typeOf_pointed typ in
            not (hasAttribute "const" (typeAttrs pointed_type))
         )
         pointer_args)
  in
  let pointer_args_content =
    List.map mk_star pointer_args
  in
  let inputs =
    (pointer_args_content
     @(List.map
         (fun v ->
           Logic_const.new_identified_term
             { term_node = TLval (TVar (cvar_to_lvar v),TNoOffset);
               term_type = Ctype v.vtype;
               term_name = [];
                  term_loc = v.vdecl })
         basic_args))
  in
  let arguments =
    List.map (fun content -> content, From inputs) to_assign
  in
  match rtyp with
  | TVoid _ ->
    (* assigns all pointer args from basic args and content of pointer args *)
      arguments
  | _ -> 
    (* assigns result from basic args and content of pointer args *)
    let loc = vi.vdecl in
    let result = Logic_const.(new_identified_term (tresult ~loc rtyp)) in
    (result, From inputs):: arguments

let is_frama_c_builtin name =
  Ast_info.is_frama_c_builtin name

(* Put an 'Unknown' status on all 'assigns' and 'from' clauses that we
   generate. *)
let emit_unknown_status_on_assigns kf bhv assigns =
  let pptopt =
    Property.ip_of_assigns kf Kglobal (Property.Id_behavior bhv) assigns
  in
  match pptopt with
  | None -> ()
  | Some ppt ->
    Property_status.emit emitter [] ppt Property_status.Dont_know;
    match assigns with
    | WritesAny -> ()
    | Writes froms ->
      let emit from =
        let ppt =
          Property.ip_of_from kf Kglobal (Property.Id_behavior bhv) from
        in
        Property_status.emit emitter [] ppt Property_status.Dont_know
      in
      List.iter emit froms
      

module Is_populated =
  State_builder.Hashtbl
    (Kernel_function.Hashtbl)
    (Datatype.Unit)
    (struct
      let size = 17
      let dependencies = [ Annotations.funspec_state ]
      let name = "Infer_annotations.Is_populated"
     end)

let () = Ast.add_linked_state Is_populated.self

let populate_funspec_aux kf spec =
  let name = Kernel_function.get_name kf in
  match spec.spec_behavior with
  | [] -> 
    (* case 1: there is no initial specification -> use generated_behavior *)
    if not (is_frama_c_builtin name) then begin
      Kernel.warning ~once:true ~current:true
        "Neither code nor specification for function %a, \
generating default assigns from the prototype"
        Kernel_function.pretty kf;
    end;
    let assigns = Writes (assigns_from_prototype kf) in
    let bhv = Cil.mk_behavior ~assigns () in
    Annotations.add_behaviors emitter kf [ bhv ];
    emit_unknown_status_on_assigns kf bhv assigns

  | _ :: _ -> 
    (* case 2: there is a specification, so look at assigns clause *)
    let bhv = match Cil.find_default_behavior spec with
      | None -> Cil.mk_behavior ()
      | Some bhv -> bhv
    in
    if bhv.b_assigns = WritesAny then
      (* case 2.2 : some assigns have to be generated *)
      (* step 2.1: looks at ungarded behaviors and then at complete
	 behaviors *)
      let warn_if_not_builtin explicit_name name orig_name =
	if not (is_frama_c_builtin name) then
	  Kernel.warning ~once:true ~current:true
	    "No code nor %s assigns clause for function %a, \
generating default assigns from the %s"
	    explicit_name Kernel_function.pretty kf orig_name
      in
      let assigns = Ast_info.merge_assigns_from_spec ~warn:false spec in
      let assigns = 
	if assigns <> WritesAny then begin
	  (* case 2.2.1. A correct assigns clause has been found *)  
	  warn_if_not_builtin "explicit" name "specification";
	  assigns
	end else begin 
	  (* case 2.2.1. No correct assigns clause can be found *)
	  let assigns = 
	    try (* Takes the union the assigns clauses, even if it 
		   is not advertised as complete behaviors. 
		   Not more arbitrary than using prototype to infer
		   assigns.*)
	      List.fold_left
		(fun acc bhv -> 
		  if Cil.is_default_behavior bhv then acc 
		  else match acc, bhv.b_assigns with
		  | _, WritesAny -> raise Not_found
		  | WritesAny, a -> a
		  | Writes l1, Writes l2 -> Writes (l1 @ l2))
		WritesAny
		spec.spec_behavior
	    with Not_found -> 
	      WritesAny
	  in
	  if assigns <> WritesAny then begin
	    warn_if_not_builtin "implicit" name "specification" ;
	    assigns 
	  end else begin (* The union gave WritesAny, so use the prototype *)
	    warn_if_not_builtin "implicit" name "prototype";
            Writes (assigns_from_prototype kf);
	  end
	end
      in
      Annotations.add_assigns ~keep_empty:false emitter kf bhv.b_name assigns;
      emit_unknown_status_on_assigns kf bhv assigns

let populate_funspec kf spec =
  if Is_populated.mem kf then
    false (* No need to add the spec again *)
  else (
    Is_populated.add kf ();
    populate_funspec_aux kf spec;
    true
  )

let () = Annotations.populate_spec_ref := populate_funspec

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

let emitter = Emitter.create "Inferred annotations" [Emitter.Funspec] [] []

let assigns_from_prototype kf =
  let vi = Kernel_function.get_vi kf in
  let formals = try let formals = getFormalsDecl vi in
  (* Do ignore anonymous names *)
  List.filter (fun vi -> vi.vname <> "") formals
  with Not_found -> []
    (* this may happen for function pointer used as formal parameters.*)
  in
  let rtyp, _, _, _ = splitFunctionTypeVI vi in
  let pointer_args,basic_args =
    List.partition (fun vi -> isPointerType vi.vtype) formals in
  (* Remove pointer to pointer types and pointer to void *)
  let pointer_args =
    List.filter
      (fun vi -> not (isVoidPtrType vi.vtype
                      || isPointerType (typeOf_pointed vi.vtype))) pointer_args
  in
  let get_length full_typ =
    let attr = typeAttr full_typ in
    findAttribute "arraylen" attr
  in

  let mk_star_v v =
    let typ = unrollType v.vtype in
    let loc = v.vdecl in
    match get_length typ with
        [AInt length] ->
          let low = Logic_const.tinteger ~loc 0 in
          let high = Logic_const.tint ~loc (Integer.pred length) 
	  in
          let range = Logic_const.trange ~loc (Some low,Some high) in
          let shift = Logic_const.term ~loc
            (TBinOp(PlusPI,tvar(cvar_to_lvar v),range))
            (make_set_type (Ctype typ))
          in
          Logic_const.new_identified_term
            (term ~loc (TLval(TMem shift,TNoOffset))
               (make_set_type (Ctype (Cil.typeOf_pointed typ))))
      | _ ->

          let cell = tvar ~loc (cvar_to_lvar v) in
          Logic_const.new_identified_term
            (term ~loc (TLval (TMem cell,TNoOffset))
               (Ctype (Cil.typeOf_pointed typ)))
  in
  let to_assign =
    List.map
      mk_star_v
      (List.filter
         (fun v ->
            let pointed_type = typeOf_pointed v.vtype in
            not (hasAttribute "const" (typeAttrs pointed_type))
            && not (Cil.isVoidType pointed_type)
         )
         pointer_args)
  in
  let pointer_args_content =
    List.map
      mk_star_v
      pointer_args
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
    ((Logic_const.new_identified_term
        (Logic_const.tat ~loc
           (Logic_const.tresult ~loc rtyp,
	    Logic_const.post_label)),From inputs):: arguments)

let is_frama_c_builtin name =
  Ast_info.is_frama_c_builtin name

let populate_funspec kf spec =
  assert (not (Kernel_function.is_definition kf));
  let name = Kernel_function.get_name kf in
  match spec.spec_behavior with
  | [] -> 
    (* case 1: there is no initial specification -> use generated_behavior *)
    if not (is_frama_c_builtin name) then begin
      Kernel.warning ~once:true
        "Neither code nor specification for function %a, \
generating default assigns from the prototype"
        Kernel_function.pretty kf;
    end;
    let assigns = Writes (assigns_from_prototype kf) in
    let bhv = Cil.mk_behavior ~assigns () in
    Annotations.add_behaviors emitter kf [ bhv ]

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
	  Kernel.warning ~once:true
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
      Annotations.add_assigns ~keep_empty:false emitter kf bhv.b_name assigns
	
let () = Annotations.populate_spec_ref := populate_funspec

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

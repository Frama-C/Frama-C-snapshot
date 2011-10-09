(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types
open Db
open Logic_const

let assigns_from_prototype vi =
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
          let low = Logic_const.tinteger ~loc ~ikind:IInt 0 in
          let high = Logic_const.tinteger ~loc ~ikind:IInt (length - 1) in
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
    Writes arguments
  | _ -> 
    (* assigns result from basic args and content of pointer args *)
    let loc = vi.vdecl in
    Writes
      ((Logic_const.new_identified_term
          (Logic_const.tat ~loc
             (Logic_const.tresult ~loc rtyp,
	      Logic_const.post_label)),From inputs):: arguments)

let is_frama_c_builtin name =
  (Ast_info.is_frama_c_builtin name)
  || (!Db.Value.mem_builtin name)

(* No need to call [Kernel_function.set_spec] here: update manually done *)
let populate_funspec kf =
  assert (not (Kernel_function.is_definition kf));
  let name = Kernel_function.get_name kf in
  let generated_assigns =
    lazy (assigns_from_prototype (Kernel_function.get_vi kf))
  in
  let default_behavior = lazy (Cil.find_default_behavior kf.spec) in
  (* Do not call Kernel_function.get_spec: this would make an infinite
     recursion. *) 
  let generated_behavior () =
    { b_name = "generated";
      b_post_cond = [] ;
      b_assumes = [];
      b_requires = [];
      b_assigns = Lazy.force generated_assigns;
      b_extended = [] }
  in
  let modify_spec (spec:funspec) =
    let spec_behavior = spec.spec_behavior in
    (match spec_behavior with
    | [] ->
    (* there is no initial specification -> use generated_behavior *)
      if not (is_frama_c_builtin name) then begin
	Kernel.warning ~once:true
          "No code for function %a, default assigns generated"
          Kernel_function.pretty kf;
      end;
      spec.spec_behavior <- [ generated_behavior () ]
    | _ :: _ ->
      let assigns_of_behaviors bhvs_set =
        let res =
          List.fold_left
            (List.fold_left
               (fun acc a -> match a.b_assigns, acc with
                 | WritesAny, a | a, WritesAny -> a
	         | Writes l1, Writes l2 -> Writes (l1 @ l2)))
            WritesAny
            bhvs_set
        in
        match res with
            | WritesAny -> Lazy.force generated_assigns
            | Writes _ -> res
      in
      (* Note-1:
         looking at sets of complete behaviors:
         if there is one of these sets
         such that all of its behaviors have an assigns clause,
         no assigns clause (equivalent to assigns everything)
         shoud be generated. *)
      let complete_behaviors_with_assigns =
        List.fold_left
          (fun acc bhv_names ->
	    try
	      let bhvs = match bhv_names with
		| [] -> 
		  (* clause: complete behaviors; *)
		  List.filter
                    (fun b ->
                      if not (Cil.is_default_behavior b) then
                        if (b.b_assigns = WritesAny) then
		          (* there is one behavior without assigns clause *)
			  raise Not_found
                        else true
                      else false)
		    spec_behavior
		| _ :: _ ->  
		  (* clause: complete behaviors bhvs; *)
		  List.map
                    (fun x ->
		      let b = List.find (fun b -> b.b_name = x) spec_behavior in
                      if (b.b_assigns = WritesAny) then
			(* there is one behavior without any assigns clause *)
			raise Not_found;
		      b) 
		    bhv_names
	      in 
	      (* all behaviors of bhvs have an assigns clause *)
	      bhvs :: acc
	    with Not_found -> 
	      acc)
	  []
	  kf.spec.spec_complete_behaviors
      in
      (* Note-2: If in such case a more accurate assigns clauses needs to be
	 generated, it can be done without using the prototype, but only from
	 the union of the assigns clauses of that set. 
       *)
      let generated_assigns,new_assigns =
        match complete_behaviors_with_assigns with
	| [] ->
          let assigns_of_behavior =
            lazy (assigns_of_behaviors [ spec_behavior ])
          in
          let generated_assigns =
            (* If all named behaviors have assigns clause, take the union 
               of locations as the assigns clause, even if it is not
               advertised as complete behaviors. Not more arbitrary than
               using prototype to infer assigns.
             *)
            if 
              List.for_all 
                (fun b -> 
                  Cil.is_default_behavior b || b.b_assigns <> WritesAny) 
                spec_behavior
            then assigns_of_behavior
            else generated_assigns
          in
          generated_assigns, assigns_of_behavior
        | _ ->
          let new_assigns =
            lazy (assigns_of_behaviors complete_behaviors_with_assigns)
	  in 
	  new_assigns, new_assigns
      in
      let register_assigns b a = b.b_assigns <- a in
      if not (is_frama_c_builtin name) then begin
	(* Generates an "assigns" clause to behaviors without "assigns" 
	   clause *) 
        let set_assigns behavior = match behavior.b_assigns with
	  | WritesAny ->
	    let new_assigns =
              if Cil.is_default_behavior behavior then begin
	        Kernel.warning ~once:true
		  "No code for function %a, default assigns generated for \
default behavior"
		  Kernel_function.pretty kf;
	        Lazy.force generated_assigns
	      end else begin
		match Lazy.force default_behavior with
		| None ->
		  Kernel.warning ~once:true
		    "No code for function %a, default assigns generated \
                         for behavior %s"
		    Kernel_function.pretty kf behavior.b_name;
		  Lazy.force generated_assigns
		| Some a ->
		  Kernel.warning ~once:true
		    "No code for function %a, default assigns used \
                         for behavior %s"
		    Kernel_function.pretty kf
		    behavior.b_name;
		  a.b_assigns
	      end
	    in
	    register_assigns behavior new_assigns
	  | _ -> ()
	in
	List.iter set_assigns spec_behavior;
      end;
      if List.for_all (fun {b_assumes=a} -> a <> []) spec_behavior then begin
	match Lazy.force new_assigns with
          | WritesAny -> ()
	  | l ->
            let generated_behavior = generated_behavior () in
            register_assigns generated_behavior l;
	    spec.spec_behavior <- generated_behavior :: spec.spec_behavior
      end);
    spec
  in
  Kernel_function.set_spec kf modify_spec
	
let () = Kernel_function.populate_spec := populate_funspec

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

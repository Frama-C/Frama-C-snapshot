(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Cil

let mkterm tnode ty loc =
  { term_node = tnode;
    term_loc = loc;
    term_type = ty;
    term_name = [] }

let term_of_var v= Ast_info.variable_term v.vdecl (cvar_to_lvar v)

class annotateFunFromDeclspec =

  let recover_from_attr_param params attrparam =
    let rec aux = function
      | AInt i ->
        Ast_info.constant_term 
	  Cil_datatype.Location.unknown (My_bigint.of_int i)
      | AUnOp(Neg,AInt i) ->
        Ast_info.constant_term
	  Cil_datatype.Location.unknown (My_bigint.of_int (-i))
      | AStr s
      | ACons(s,[]) ->
        begin try
		let v = List.find (fun v -> v.vname = s) params in
		term_of_var v
          with Not_found -> failwith "No recovery" end
      | ABinOp(bop,attr1,attr2) ->
        mkterm
	  (TBinOp(bop,aux attr1,aux attr2)) 
	  Linteger 
	  Cil_datatype.Location.unknown
      | ACons _
      | ASizeOf _
      | ASizeOfE _
      | ASizeOfS _
      | AAlignOf _
      | AAlignOfE _
      | AAlignOfS _
      | AUnOp _
      | ADot _
      | AStar _
      | AAddrOf _
      | AIndex _
      | AQuestion _ -> failwith "No recovery" (* Not yet supported *)
    in
    aux attrparam
  in
  let recover_from_attribute params attr =
    match attr with
    | Attr(name,attrparams) ->
      begin try
              Some(name, List.map (recover_from_attr_param params) attrparams)
        with Failure "No recovery" -> None end
    | AttrAnnot _ -> None
  in

  (* Add precondition based on declspec on parameters *)
  let annotate_var params acc v =
    List.fold_left
      (fun acc attr ->
        match recover_from_attribute params attr with
        | None -> acc
        | Some(name,args) ->
          if name = "valid" || name = "valid_range" then
            let p = match name with
              | "valid" ->
                assert (args = []);
                let ts = Logic_const.tvar (cvar_to_lvar v) in
                Pvalid(ts)
              | "valid_range" ->
                let t1,t2 = match args with
                  | [ t1; t2 ] -> t1,t2
                  | _ -> assert false
                in
                Pvalid_range(term_of_var v,t1,t2)
              | _ -> assert false
            in
            let app =
              Logic_const.new_predicate (Logic_const.unamed p)
            in
            app :: acc
          else
            try
              let p =
                match Logic_env.find_all_logic_functions name with
                | [i] -> i
                | _ -> raise Not_found
                 (*
                   error "[Jessie] Rewrite: cannot find logic function %s@." name;
                   raise Exit
                  *)
              in
              assert (List.length p.l_profile = List.length(args) + 1);
              assert (List.length p.l_labels <= 1);
              let args = term_of_var v :: args in
              let app =
                Logic_const.new_predicate
                  (Logic_const.unamed (Papp(p,[],args)))
              in
              app :: acc
            with Not_found -> acc
      ) acc (typeAttrs v.vtype)
  in

  let annotate_fun v =
    let kf = Globals.Functions.get v in
    let params = Globals.Functions.get_params kf in
    let req = List.fold_left (annotate_var params) [] params in
    if req <> [] then
      (* add [req] to [b_requires] of default behavior *)
      let return_ty = getReturnType v.vtype in
      let loc = v.vdecl in
      let behavior = Cil.mk_behavior ~requires:req () in
      Kernel_function.set_spec
	kf
	(fun spec ->
	  spec.spec_behavior <-
            Logic_utils.merge_behaviors
            ~silent:false spec.spec_behavior [ behavior ] ;
	  let insert_spec behavior =
            let ens =
              List.fold_left
		(fun acc attr ->
		  match recover_from_attribute params attr with
		  | None -> acc
		  | Some(name,args) ->
                    if name = "valid" || name = "valid_range" then
                      let p = match name with
			| "valid" ->
                          assert (args = []);
                          let ts = Logic_const.tresult ~loc return_ty in
                          Pvalid(ts)
			| "valid_range" ->
                          let t1,t2 = match args with
                            | [ t1; t2 ] -> t1,t2
                            | _ -> assert false
                          in
                          let res = Logic_const.tresult ~loc return_ty in
                          Pvalid_range(res,t1,t2)
			| _ -> assert false
                      in
                      let app =
			Logic_const.new_predicate (Logic_const.unamed p)
                      in
                      (Normal, app) :: acc
                    else
                      try
			let p =
                          match Logic_env.find_all_logic_functions name with
                          | [i] -> i
                          | _ -> assert false
			in
			assert (List.length p.l_profile = List.length args + 1);
			assert (List.length p.l_labels <= 1);
			let res = Logic_const.tresult ~loc return_ty in
			let args = res :: args in
			let app =
                          Logic_const.new_predicate
                            (Logic_const.unamed (Papp(p,[],args)))
			in
			(Normal,app) :: acc
                      with Not_found -> acc)
		behavior.b_post_cond
		(typeAttrs return_ty)
            in
            behavior.b_post_cond <- ens;
	  in
	  List.iter insert_spec spec.spec_behavior;
	  spec)
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
  | GFun(f,_) ->
    annotate_fun f.svar;
    SkipChildren
  | GVarDecl(_,v,_)
  | GVar(v,_,_) (*as g*) ->
    if isFunctionType v.vtype && not v.vdefined then
      annotate_fun v;
    SkipChildren
    (* )
       else
       let inv = annotate_var [] [] v in
       let postaction gl =
       match inv with [] -> gl | _ ->
    (* Define a global string invariant *)
       let inv =
       List.map (fun p -> Logic_const.unamed p.ip_content) inv
       in
       let p = Logic_const.new_predicate (Logic_const.pands inv) in
       let globinv =
       Cil_const.make_logic_info (unique_logic_name ("valid_" ^ v.vname))
       in
       globinv.l_labels <- [ LogicLabel "Here" ];
       globinv.l_body <- LBpred (predicate v.vdecl p.ip_content);
       attach_globaction
       (fun () -> Logic_utils.add_logic_function globinv);
       gl @ [GAnnot(Dinvariant globinv,v.vdecl)]
       in
       ChangeDoChildrenPost ([g], postaction)
     *)
  | GAnnot _ -> DoChildren
  | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
  | GEnumTag _ | GAsm _ | GPragma _ | GText _ ->
    SkipChildren
end

let interprate file =
  let visitor = new annotateFunFromDeclspec in
  Visitor.visitFramacFile visitor file

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

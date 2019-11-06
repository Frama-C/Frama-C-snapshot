(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

let named_predicate_to_exp_ref
  : (kernel_function -> Env.t -> predicate -> exp * Env.t) ref
  = Extlib.mk_fun "named_predicate_to_exp_ref"

let term_to_exp_ref
  : (kernel_function -> Env.t -> term -> exp * Env.t) ref
  = Extlib.mk_fun "term_to_exp_ref"

(*****************************************************************************)
(************************** Auxiliary  functions* ****************************)
(*****************************************************************************)

(* @return true iff the result of the function is provided by reference as the
   first extra argument at each call *)
let result_as_extra_argument = Gmp_types.Z.is_t
(* TODO: to be extended to any compound type? E.g. returning a struct is not
   good practice... *)

(*****************************************************************************)
(****************** Generation of function bodies ****************************)
(*****************************************************************************)

(* Generate the block of code containing the statement assigning [e] to [ret_vi]
   (the result). *)
let generate_return_block ~loc env ret_vi e = match e.enode with
  | Lval (Var _, NoOffset) ->
    (* the returned value is a variable: Cil invariant preserved;
       no need of [ret_vi] *)
    let return_retres = Cil.mkStmt ~valid_sid:true (Return (Some e, loc)) in
    let b, env =
      Env.pop_and_get env return_retres ~global_clear:false Env.After
    in
    b.blocals <- b.blocals;
    b.bscoping <- true;
    b, env
  | _ ->
    (* the returned value is _not_ a variable: restore the invariant *)
    let init = AssignInit (SingleInit e) in
    let set =
      Cil.mkStmtOneInstr ~valid_sid:true (Local_init (ret_vi, init, loc))
    in
    let return =
      Cil.mkStmt ~valid_sid:true (Return (Some (Cil.evar ~loc ret_vi), loc))
    in
    let b, env = Env.pop_and_get env set ~global_clear:false Env.Middle in
    ret_vi.vdefined <- true;
    b.blocals <- ret_vi :: b.blocals;
    b.bstmts <- b.bstmts @ [ return ];
    b.bscoping <- true;
    b, env

(* Generate the function's body for predicates. *)
let pred_to_block ~loc kf env ret_vi p =
  Typing.type_named_predicate ~must_clear:false p;
  let e, env = !named_predicate_to_exp_ref kf env p in
  (* for predicate, since the result is either 0 or 1, return it directly (it
     cannot be provided as extra argument *)
  generate_return_block ~loc env ret_vi e

(* Generate the function's body for terms. *)
let term_to_block ~loc kf env ret_ty ret_vi t =
  Typing.type_term ~use_gmp_opt:false ~ctx:(Typing.number_ty_of_typ ret_ty) t;
  let e, env = !term_to_exp_ref kf env t in
  if Cil.isVoidType ret_ty then
    (* if the function's result is a GMP, it is the first parameter of the
       function (by reference). *)
    let set =
      let lv_star_ret = Cil.mkMem ~addr:(Cil.evar ~loc ret_vi) ~off:NoOffset in
      let star_ret = Cil.new_exp ~loc (Lval lv_star_ret) in
      Gmp.init_set ~loc lv_star_ret star_ret e
    in
    let return_void = Cil.mkStmt ~valid_sid:true (Return (None, loc)) in
    let b, env = Env.pop_and_get env set ~global_clear:false Env.Middle in
    b.bstmts <- b.bstmts @ [ return_void ];
    b.bscoping <- true;
    b, env
  else
    generate_return_block ~loc env ret_vi e

let generate_body ~loc kf env ret_ty ret_vi = function
  | LBnone | LBreads _ ->
    Options.abort
      "logic function or predicate without explicit definition are not part of \
       E-ACSL"
  | LBterm t -> term_to_block ~loc kf env ret_ty ret_vi t
  | LBpred p -> pred_to_block ~loc kf env ret_vi p
  | LBinductive _ -> Error.not_yet "inductive definition"

(* Generate a kernel function from a given logic info [li] *)
let generate_kf ~loc fname env ret_ty params_ty li =
  (* build the formal parameters *)
  let params, params_ty =
    List.fold_right2
      (fun lvi pty (params, params_ty) ->
        let ty = match pty with
          | Typing.Gmpz ->
            (* GMP's integer are arrays: consider them as pointers in function's
               parameters *)
            Gmp_types.Z.t_as_ptr ()
          | Typing.C_integer ik -> TInt(ik, [])
          | Typing.C_float ik -> TFloat(ik, [])
          (* for the time being, no reals but rationals instead *)
          | Typing.Rational -> Gmp_types.Q.t ()
          | Typing.Real -> Error.not_yet "real number"
          | Typing.Nan -> Typing.typ_of_lty lvi.lv_type
        in
        (* build the formals: cannot use [Cil.makeFormal] since the function
           does not yet exist *)
        let vi = Cil.makeVarinfo false true lvi.lv_name ty in
        vi :: params, (lvi.lv_name, ty, []) :: params_ty)
      li.l_profile
      params_ty
      ([], [])
  in
  (* build the varinfo storing the result *)
  let ret_vi, ret_ty, params_with_ret, params_ty_with_ret =
    let vname = "__retres" in
    if result_as_extra_argument ret_ty then
      let ret_ty_ptr = TPtr(ret_ty, []) (* call by reference *) in
      let vname = vname ^ "_arg" in
      let vi = Cil.makeVarinfo false true vname ret_ty_ptr in
      vi, Cil.voidType, vi :: params, (vname, ret_ty_ptr, []) :: params_ty
    else
      Cil.makeVarinfo false false vname ret_ty, ret_ty, params, params_ty
  in
  (* build the function's varinfo *)
  let vi =
    Cil.makeGlobalVar
      fname
      (TFun
         (ret_ty,
          Some params_ty_with_ret,
          false,
          li.l_var_info.lv_attr))
  in
  vi.vdefined <- true;
  (* create the fundec *)
  let fundec =
    { svar = vi;
      sformals = params_with_ret;
      slocals = []; (* filled later to break mutual dependencies between
                       creating this list and creating the kf *)
      smaxid = 0;
      sbody = Cil.mkBlock []; (* filled later; same as above *)
      smaxstmtid = None;
      sallstmts = [];
      sspec = Cil.empty_funspec () }
  in
  Cil.setMaxId fundec;
  let spec = Cil.empty_funspec () in
  Queue.add
    (fun () -> Globals.Functions.replace_by_definition spec fundec loc)
    (Env.get_visitor env)#get_filling_actions;
  (* create the kernel function itself *)
  let kf = { fundec = Definition(fundec, loc); spec } in
  (* closure generating the function's body.
     Delay its generation after filling the memoisation table (for termination
     of recursive function calls) *)
  let gen_body () =
    let env = Env.push env in
    let old_kf = Extlib.the (Env.current_kf env) in
    Env.set_current_kf env kf;
    (* fill the typing environment with the function's parameters
       before generating the code (code generation invokes typing) *)
    let env =
      let add env lvi vi =
        let i = Interval.interv_of_typ vi.vtype in
        Interval.Env.add lvi i;
        Env.Logic_binding.add_binding env lvi vi
      in
      List.fold_left2 add env li.l_profile params
    in
    let b, env = generate_body ~loc kf env ret_ty ret_vi li.l_body in
    fundec.sbody <- b;
    (* add the generated variables in the necessary lists *)
    (* TODO: factorized the code below that add the generated vars with method
       [add_generated_variables_in_function] in the main visitor *)
    let vars =
      let l = Env.get_generated_variables env in
      if ret_vi.vdefined then (ret_vi, Env.LFunction kf) :: l else l
    in
    let locals, blocks =
      List.fold_left
        (fun (local_vars, block_vars as acc) (v, scope) -> match scope with
        | Env.LFunction kf' when Kernel_function.equal kf kf' ->
          v :: local_vars, block_vars
        | Env.LLocal_block kf' when Kernel_function.equal kf kf' ->
          v :: local_vars, block_vars
        | _ -> acc)
        (fundec.slocals, fundec.sbody.blocals)
        vars
    in
    fundec.slocals <- locals;
    fundec.sbody.blocals <- blocks;
    List.iter
      (fun lvi ->
         Interval.Env.remove lvi;
         ignore (Env.Logic_binding.remove env lvi))
      li.l_profile;
    Env.set_current_kf
      env
      (Visitor_behavior.Get_orig.kernel_function (Env.get_behavior env) old_kf)
  in
  vi, kf, gen_body

(**************************************************************************)
(***************************** Memoization ********************************)
(**************************************************************************)

module Params_ty =
  Datatype.List_with_collections
    (Typing.Datatype)
    (struct let module_name = "E_ACSL.Logic_functions.Params_ty" end)

(* for each logic_info, associate its possible profiles, i.e. the types of its
   parameters + the generated varinfo for the function *)
let memo_tbl:
    kernel_function Params_ty.Hashtbl.t Logic_info.Hashtbl.t
  = Logic_info.Hashtbl.create 7

let reset () = Logic_info.Hashtbl.clear memo_tbl

let add_generated_functions globals =
  let rec aux acc = function
    | [] ->
      acc
    | GAnnot(Dfun_or_pred(li, loc), _) as g :: l ->
      let acc = g :: acc in
      (try
         (* add the declarations close to its corresponding logic function or
            predicate *)
         let params = Logic_info.Hashtbl.find memo_tbl li in
         let add_fundecl kf acc =
           GFunDecl(Cil.empty_funspec (), Kernel_function.get_vi kf, loc)
           :: acc
         in
         aux (Params_ty.Hashtbl.fold (fun _ -> add_fundecl) params acc) l
       with Not_found ->
         aux acc l)
    | g :: l ->
      aux (g :: acc) l
  in
  let rev_globals = aux [] globals in
  (* add the definitions at the end of [globals] *)
  let add_fundec kf globals =
    let fundec =
      try Kernel_function.get_definition kf
      with Kernel_function.No_Definition -> assert false
    in
    GFun(fundec, Location.unknown) :: globals
  in
  let rev_globals =
    Logic_info.Hashtbl.fold
      (fun _ -> Params_ty.Hashtbl.fold (fun _ -> add_fundec))
      memo_tbl
      rev_globals
  in
  List.rev rev_globals

let tapp_to_exp ~loc fname env t li params_ty args =
  let ret_ty = Typing.get_typ t in
  let gen tbl =
    let vi, kf, gen_body = generate_kf fname ~loc env ret_ty params_ty li in
    Params_ty.Hashtbl.add tbl params_ty kf;
    vi, gen_body
  in
  (* memoise the function's varinfo *)
  let fvi, gen_body =
    try
      let h = Logic_info.Hashtbl.find memo_tbl li in
      try
        let kf = Params_ty.Hashtbl.find h params_ty in
        Kernel_function.get_vi kf,
        (fun () -> ()) (* body generation already planified *)
      with Not_found -> gen h
    with Not_found ->
      let h = Params_ty.Hashtbl.create 7 in
      Logic_info.Hashtbl.add memo_tbl li h;
      gen h
  in
  (* the generation of the function body must be performed after memoizing the
     kernel function in order to handle recursive calls in finite time :-) *)
  gen_body ();
  (* create the function call for the tapp *)
  let mkcall vi =
    let mk_args types args =
      match types (* generated by E-ACSL: no need to unroll *) with
      | TFun(_, Some params, _, _) ->
        (* additional casts are necessary whenever the argument is GMP and the
           parameter is a (small) integralType: after handling the context in
           [Translate] through [add_cast], the GMP has been translated into a
           [long] (that is what provided the GMP API). This [long] must now be
           translated to the parameter's type. It cannot be done before since
           the exact type of the parameter is only computed when the function is
           generated *)
        List.map2
          (fun (_, newt, _) e -> Cil.mkCast ~force:false ~newt ~e)
          params
          args
      | _ -> assert false
    in
    if result_as_extra_argument ret_ty then
      let args = mk_args fvi.vtype (Cil.mkAddrOf ~loc (Cil.var vi) :: args) in
      Call(None, Cil.evar fvi, args, loc)
    else
      let args = mk_args fvi.vtype args in
      Call(Some (Cil.var vi), Cil.evar fvi, args, loc)
  in
  (* generate the varinfo storing the result of the call *)
  Env.new_var
    ~loc
    ~name:li.l_var_info.lv_name
    env
    (Some t)
    ret_ty
    (fun vi _ -> [ Cil.mkStmtOneInstr ~valid_sid:true (mkcall vi) ])

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

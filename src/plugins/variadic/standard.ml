(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Va_types
open Options
module Cil = Extends.Cil
module List = Extends.List
module Typ = Extends.Typ
module Build = Va_build


let params_types params =
  List.map (fun (_,typ,_) -> typ) params

let pp_prototype name f tparams =
  Format.fprintf f "%s(%a)"
    name
    (Pretty_utils.pp_list ~sep:", " Printer.pp_typ) tparams

let pp_overload name f l =
  let prototypes = List.map fst l in
  Pretty_utils.pp_list ~sep:"@\n" (pp_prototype name) f prototypes


let new_globals : (global list) ref = ref []


(* ************************************************************************ *)
(* Call translation                                                         *)
(* ************************************************************************ *)

exception Translate_call_exn

let can_cast given expected =
  let integral_rep ikind =
    Cil.bitsSizeOfInt ikind, Cil.isSigned ikind
  and expose t =
    Cil.type_remove_attributes_for_c_cast (Cil.unrollType t)
  in
  match expose given, expose expected with
  | (TInt (i1,a1) | TEnum({ekind=i1},a1)), 
    (TInt (i2,a2) | TEnum({ekind=i2},a2)) when not (Strict.get ()) ->
      integral_rep i1 = integral_rep i2 &&
      Cil_datatype.Attributes.equal a1 a2
  | TPtr _, TPtr _ -> true
  | _,_ -> Cil_datatype.Typ.equal given expected


(* cast the i-th argument exp to paramtyp *)
let cast_arg i paramtyp exp =
  let argtyp = Cil.typeOf exp in
  if not (can_cast argtyp paramtyp) then
    Self.warning ~current:true
      "Incorrect type for argument %d. \
       The argument will be cast from %a to %a."
      (i + 1)
      Printer.pp_typ argtyp Printer.pp_typ paramtyp;
  Cil.mkCast ~force:false ~e:exp ~newt:paramtyp


(* cast a list of args to the tparams list of types and remove unused args *)
let match_args tparams args =
  (* Remove unused arguments *)
  let paramcount = List.length tparams
  and argcount = List.length args in
  if argcount > paramcount  then
    Self.warning ~current:true
      "Too many arguments: expected %d, given %d. \
       Superfluous arguments will be removed."
      paramcount argcount
  else if argcount < paramcount then (
    Self.warning ~current:true
      "Not enough arguments: expected %d, given %d."
      paramcount argcount;
    raise Translate_call_exn
  );

  (* Translate params *)
  let new_args, unused_args = List.break paramcount args in
  List.mapi2 cast_arg tparams new_args, unused_args


(* translate a call by applying argument matching/pruning and changing
   callee *)
let match_call ~fundec stmt loc lval new_callee new_tparams args =
  let block = Cil.mkBlock [] in
  let block_stmt = {stmt with skind = Block block} in
  let new_args, unused_args = match_args new_tparams args in
  let call = Build.call ~loc lval new_callee new_args in
  let reads = List.map (Cil.mkPureExpr ~fundec ~loc) unused_args in
  block.bstmts <- reads @ [call];
  block_stmt



(* ************************************************************************ *)
(* Aggregator calls                                                         *)
(* ************************************************************************ *)

let find_null exp_list =
  List.ifind (fun e -> Cil.isZero (Cil.constFold false e)) exp_list


let aggregator_call ~fundec {a_target; a_pos; a_type; a_param} vf stmt =
  (* Extract call informations *)
  let lval, args, loc = match stmt.skind with
  | Instr(Call(lval, _, args, loc)) -> lval, args, loc
  | _ -> assert false
  and name = vf.vf_decl.vorig_name
  and tparams = Typ.params_types a_target.vtype 
  and pname, ptyp = a_param in

  (* Check argument count *)
  let argcount = List.length args
  and paramcount = List.length tparams in 
  if argcount < paramcount then begin
    Self.warning ~current:true
      "Not enough arguments: expected %d, given %d."
      paramcount argcount;
    raise Translate_call_exn;
  end;

  (* Compute the size of the aggregation *)
  let size = match a_type with 
  | EndedByNull ->
      begin try
        find_null (List.drop a_pos args) + 1
      with Not_found ->
        Self.warning ~current:true
          "Failed to find a sentinel (NULL pointer) in the argument list.";
        raise Translate_call_exn;
      end
  in

  (* Convert arguments *)
  let tparams_left = List.take a_pos tparams in
  let tparams_right = List.drop (a_pos + 1) tparams in
  let new_tparams = tparams_left @ List.make size ptyp @ tparams_right in
  let new_args, unused_args = match_args new_tparams args in

  (* Split the arguments *)
  let args_left, args_rem = List.break a_pos new_args in
  let args_middle, args_right = List.break size args_rem in

  (* Create the call code  *)
  Self.result ~current:true ~level:2
    "Translating call to %s to a call to %s."
    name a_target.vorig_name;
  let block = Cil.mkBlock [] in
  let block_stmt = {stmt with skind = Block block} in
  let pname = if pname = "" then "param" else pname in
  let vaggr, assigns = Build.array_init ~loc fundec block
    pname ptyp args_middle in
  let new_arg = Cil.mkAddrOrStartOf ~loc (Cil.var vaggr) in
  let new_args = args_left @ [new_arg] @ args_right in
  let new_args,_ = match_args tparams new_args in
  let call = Build.call ~loc lval a_target new_args in
  let reads = List.map (Cil.mkPureExpr ~fundec ~loc) unused_args in
  block.bstmts <- assigns @ reads @ [call];

  (* Return the created block *)
  block_stmt



(* ************************************************************************ *)
(* Overloads calls                                                          *)
(* ************************************************************************ *)

let rec check_arg_matching given expected =
  match Cil.unrollType given, Cil.unrollType expected with
  | (TInt _ | TEnum _), (TInt _ | TEnum _) -> true
  | TPtr _, _ when Cil.isVoidPtrType expected -> true
  | TPtr (t1, _), TPtr (t2, _) -> check_arg_matching t1 t2
  | _, _ -> not (Cil.need_cast given expected)


let rec check_call_matching tparams targs =
  match tparams, targs with
  (* No more args needed -> always valid *)
  | [], _ -> true
  (* Not enough input args *)
  | _, [] -> false
  | a1 :: l1, a2 :: l2 ->
      check_arg_matching a1 a2 && 
      check_call_matching l1 l2


let filter_matching_prototypes overload args =
  (* Find suitable candidates for this call *)
  let targs = List.map Cil.typeOf args in
  let check (tparams, vi) =
    if check_call_matching tparams targs then
      Some ((tparams, vi), List.length tparams)
    else
      None
  in
  let candidates = List.filter_map check overload in
  (* Keep only best candidates (those with most parameters) *)
  let max_params = List.fold_left (fun i (_,j) -> max i j) 0 candidates in
  let candidates = List.filter (fun (_,i) -> i = max_params) candidates in
  List.map fst candidates


let overloaded_call ~fundec overload vf stmt =
  (* Extract call informations *)
  let lval, args, loc = match stmt.skind with
  | Instr(Call(lval, _, args, loc)) -> lval, args, loc
  | _ -> assert false
  and name = vf.vf_decl.vorig_name in

  (* Find the matching prototype *)
  let tparams, new_callee =
    match filter_matching_prototypes overload args with
    | [] -> (* No matching prototype *)
        Self.warning ~current:true 
          "No matching prototype found for this call to %s. \
           Candidates were: \
           %a"
          name
          (pp_overload name) overload;
        raise Translate_call_exn;
    | [(tparams,vi)] -> (* Exactly one matching prototype *)
        tparams, vi
    | l -> (* Several matching prototypes *)
        Self.warning ~current:true
          "Ambiguous call to %s. Matching candidates are: \
           %a"
          name
          (pp_overload name) l;
        raise Translate_call_exn;
  in

  (* Rebuild the call *)
  Self.result ~current:true ~level:2
    "Translating call to the specialized version %a."
    (pp_prototype name) tparams;
  match_call ~fundec stmt loc lval new_callee tparams args



(* ************************************************************************ *)
(* Format functions calls                                                   *)
(* ************************************************************************ *)

(* --- Specification building --- *)

let find_global env name =
  try
    Some (Environment.find_global env name)
  with Not_found ->
    Self.warning
      "Unable to locate global %s which should be in the Frama-C LibC."
      name;
    None

let find_predicate name =
  match Logic_env.find_all_logic_functions name with
  | f :: _q -> Some f (* TODO: should we warn in case of overloading? *)
  | [] ->
    Self.warning
      "Unable to locate ACSL predicate %s."
      name;
    None

let find_field env structname fieldname =
  try
    let compinfo = Environment.find_struct env structname in
    Some (Cil.getCompField compinfo fieldname)
  with Not_found ->
    Self.warning
      "Unable to locate %s field %s."
      structname fieldname;
    None

let build_fun_spec env loc vf format_fun tvparams formals =
  let open Format_types in
  let _ = () in
  let fixed_params_count = Typ.params_count vf.vf_original_type in
  let sformals, vformals = List.break fixed_params_count formals in

  (* Spec *)
  let sources = ref []
  and dests = ref []
  and requires = ref []
  and ensures = ref [] in
  let elval lval =
    Build.logic_elval ~loc lval in
  let iterm lval =
    Logic_const.new_identified_term (elval lval)
  and insert x t = 
    t := x :: !t
  in
  let insert_source ?(indirect=false) lval =
    let itlval = iterm lval in
    let it_content = if indirect then
        { itlval.it_content with
          term_name = "indirect" :: itlval.it_content.term_name }
      else itlval.it_content
    in
    let itlval = { itlval with Cil_types.it_content } in
    insert itlval sources
  and insert_dest lval =
    insert (iterm lval) dests
  and insert_require pred =
    insert (Logic_const.new_predicate pred) requires
  and insert_ensure pred =
    insert (Normal, Logic_const.new_predicate pred) ensures
  in
  let add_lval ~indirect (lval,dir) =
    (* Add the lval to the list of sources/dests *)
    begin match dir with
    | (`ArgIn | `ArgInArray) -> insert_source ~indirect lval
    | (`ArgOut | `ArgOutArray) -> insert_dest lval
    | `ArgInOut -> insert_source ~indirect lval; insert_dest lval
    end
  in 
  let add_var (vi,dir) =
    (* Use the appropriate logical lval *)
    let lval = match dir with
    | `ArgIn -> Build.logic_var vi
    | (`ArgInArray | `ArgOutArray) -> Build.logic_varrange ~loc vi
    | (`ArgOut | `ArgInOut) -> Build.logic_varmem ~loc vi
    in
    (* Build requires/ensures *)
    let term = elval (Build.logic_var vi)
    and here = Logic_const.here_label in
    begin match dir with
    | `ArgInArray ->
        let pred = match Cil.unrollTypeDeep vi.vtype with
          | TPtr (TInt(IChar, _), _) ->
              find_predicate "valid_read_string"
          | TPtr (typ, _) when typ = Cil.theMachine.Cil.wcharType ->
              find_predicate "valid_wstring"
          | _ -> None
        in
        begin match pred with
        | Some logic_info ->
            let labels =
              List.map (fun l -> l,Logic_const.here_label) logic_info.l_labels
            in
            let p = Logic_const.papp ~loc (logic_info, labels, [term]) in
            insert_require p
        | None -> ()
        end

    | `ArgOut ->
        insert_require (Logic_const.pvalid ~loc (here,term));
        insert_ensure (Logic_const.pinitialized ~loc (here,term))

    | _ -> ()
    end;
    (* Cil.hasAttribute "const" *)
    add_lval (lval,dir)
  in

  (* Build variadic parameter source/dest list *)
  let dirs = List.map snd tvparams in
  let l = List.combine vformals dirs in
  List.iter (add_var ~indirect:false) l;

  (* Add format source and additionnal parameters *)
  add_var ~indirect:true (List.nth sformals format_fun.f_format_pos, `ArgInArray);
  List.iter (fun p -> add_var ~indirect:true (List.nth sformals p, `ArgIn))
    format_fun.f_additionnal_args;

  (* Add buffer source/dest *)
  begin match format_fun.f_buffer, format_fun.f_kind with
  | StdIO, ScanfLike ->
      begin match find_global env "__fc_stdin" with
      | Some vi -> add_var ~indirect:true (vi, `ArgInOut)
      | None -> ()
      end
  | StdIO, PrintfLike ->
      begin match find_global env "__fc_stdout" with
      | Some vi -> add_var ~indirect:true (vi, `ArgInOut)
      | None -> ()
      end
  | Arg i, ScanfLike -> 
      add_var ~indirect:true (List.nth sformals i, `ArgInArray)
  | Arg i, PrintfLike -> 
      add_var ~indirect:true (List.nth sformals i, `ArgOutArray)
  | Stream _i, _ -> () (*
      (* These generated dependencies doesn't really help analyses *)
      (* assigns *stream \from stream->__fc_stdio_id *)
      let stream = List.nth sformals i in
      add_var (stream, `ArgOut);
      begin match find_field env "__fc_FILE" "__fc_stdio_id" with
      | Some fieldinfo ->
          add_lval (Build.logic_varfield ~loc stream fieldinfo, `ArgIn)
      | None -> ()
      end
      *)
  | File _i, _ -> ()
  | Syslog, _ -> ()
  end;

  (* Add return value dest *)
  let rettyp = Cil.getReturnType vf.vf_decl.vtype in
  if not (Cil.isVoidType rettyp) then
    add_lval ~indirect:true (Build.logic_return rettyp, `ArgOut);

  (* Build the assign clause *)
  let froms = List.map (fun iterm -> iterm, From !sources) !dests in
  let assigns = Writes froms in

  (* Build the default behaviour *)
  let bhv = Cil.mk_behavior ~assigns
    ~requires:!requires ~post_cond:!ensures () in
  { (Cil.empty_funspec ()) with spec_behavior = [bhv] }


(* --- Call translation --- *)

let format_fun_call ~fundec env format_fun vf stmt =
  (* Extract call informations *)
  let lval, args, loc = match stmt.skind with
  | Instr(Call(lval, _, args, loc)) -> lval, args, loc
  | _ -> assert false
  and name = vf.vf_decl.vorig_name
  and params = Typ.params vf.vf_decl.vtype in
  (* Remove the va_param parameter added during the declaration visit *)
  let fixed_params_count = Typ.params_count vf.vf_original_type in
  let sparams = List.take fixed_params_count params in 

  (* Extract the format if possible *)
  let format =
    try
      let format_arg =
        try List.nth args format_fun.f_format_pos
        with  Failure _ (* nth *)-> Self.abort
          "The function %s does not have the expected number of arguments."
          name
      in
      match  Cil.static_string format_arg with
      | None -> raise Translate_call_exn (* No syntactic hint *)
      | Some s -> Format_parser.parse_format format_fun.f_kind s
    with
    | Format_parser.Invalid_format -> raise Translate_call_exn
  in

  (* Try to type expected parameters if possible *)
  let find_typedef = Environment.find_type env in
  let tvparams = Format_typer.type_format ~find_typedef format in
  let new_param i (typ,_dir) =
    let typ = if Cil.isIntegralType typ then
        Cil.integralPromotion typ
      else
        typ
    in
    "param" ^ string_of_int i, typ, []
  in
  let vparams = List.mapi new_param tvparams in
  let new_params = sparams @ vparams in

  (* Create the new callee *)
  let ret_typ, _, _, attributes = Cil.splitFunctionType vf.vf_decl.vtype in
  let new_callee_typ = TFun (ret_typ, Some new_params, false, attributes) in
  let mk_spec formals = build_fun_spec env loc vf format_fun tvparams formals in
  let new_callee, glob = Build.function_declaration ~loc:vf.vf_decl.vdecl
    name new_callee_typ mk_spec in
  new_globals := glob :: !new_globals;

  (* Translate the call *)
  Self.result ~current:true ~level:2
    "Translating call to %s to a call to the specialized version %s."
    name new_callee.vname;
  let tparams = params_types new_params in
  match_call ~fundec stmt loc lval new_callee tparams args



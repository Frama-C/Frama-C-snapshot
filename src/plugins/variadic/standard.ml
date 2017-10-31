(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let pp_prototype name fmt tparams =
  Format.fprintf fmt "%s(%a)"
    name
    (Pretty_utils.pp_list ~sep:", " Printer.pp_typ) tparams

let pp_overload name fmt l =
  let prototypes = List.map fst l in
  Pretty_utils.pp_list ~sep:"@\n" (pp_prototype name) fmt prototypes


let new_globals : (global list) ref = ref []


(* ************************************************************************ *)
(* Call translation                                                         *)
(* ************************************************************************ *)

exception Translate_call_exn

(* Extended integer types (e.g. int8_t, uint_least16_t, int_fast32_t)
   do not have their own character modifiers, but instead use macros that are
   converted into "standard" modifiers (e.g. "%hhd", "%hu", "%d", etc.).
   Therefore, we cannot enforce their types the same way as for e.g. size_t,
   which has its own modifier. We weaken the check, allowing a different name
   but still requiring same size and signedness. *)
let extended_integer_typenames =
  ["int8_t"; "uint8_t"; "int_least8_t"; "uint_least8_t";
   "int_fast8_t"; "uint_fast8_t";
   "int16_t"; "uint16_t"; "int_least16_t"; "uint_least16_t";
   "int_fast16_t"; "uint_fast16_t";
   "int32_t"; "uint32_t"; "int_least32_t"; "uint_least32_t";
   "int_fast32_t"; "uint_fast32_t";
   "int64_t"; "uint64_t"; "int_least64_t"; "uint_least64_t";
   "int_fast64_t"; "uint_fast64_t"]

let is_extended_integer_type t =
  match t with
  | TNamed (ti, _) -> List.mem ti.tname extended_integer_typenames
  | _ -> false

let can_cast given expected =
  let integral_rep ikind =
    Cil.bitsSizeOfInt ikind, Cil.isSigned ikind
  and expose t =
    Cil.type_remove_attributes_for_c_cast (Cil.unrollType t)
  in
  match expose given, expose expected with
  | (TInt (i1,a1) | TEnum({ekind=i1},a1)),
    (TInt (i2,a2) | TEnum({ekind=i2},a2))
    when not (Strict.get ()) || is_extended_integer_type given ->
    integral_rep i1 = integral_rep i2 &&
    Cil_datatype.Attributes.equal a1 a2
  | TPtr _, TPtr _ -> true
  | exposed_given, exposed_expected ->
    Cil_datatype.Typ.equal exposed_given exposed_expected


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
let match_call ~loc ~fundec scope mk_call new_callee new_tparams args =
  let new_args, unused_args = match_args new_tparams args in
  let call = mk_call (Cil.evar ~loc new_callee) new_args in
  let reads =
    List.map (fun e -> Cil.mkPureExprInstr ~fundec ~scope e) unused_args
  in
  reads @ [call]

(* ************************************************************************ *)
(* Aggregator calls                                                         *)
(* ************************************************************************ *)

let find_null exp_list =
  List.ifind (fun e -> Cil.isZero (Cil.constFold false e)) exp_list


let aggregator_call
    ~fundec {a_target; a_pos; a_type; a_param} scope loc mk_call vf args =
  let name = vf.vf_decl.vorig_name
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
  let pname = if pname = "" then "param" else pname in
  let vaggr, assigns =
    Build.array_init ~loc fundec scope pname ptyp args_middle
  in
  let new_arg = Cil.mkAddrOrStartOf ~loc (Cil.var vaggr) in
  let new_args = args_left @ [new_arg] @ args_right in
  let new_args,_ = match_args tparams new_args in
  let call = mk_call (Cil.evar ~loc a_target) new_args in
  let reads = List.map (Cil.mkPureExprInstr ~fundec ~scope ~loc) unused_args in
  assigns :: reads @ [call]

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
  | [], [] -> true
  | [], _
  (* too many args: this is allowed by the standard (the extra arguments
     are ignored), but in practice this leads to disambiguation issues in
     some cases (e.g. last argument is 0 instead of NULL), so we prefer to
     be strict *)
  (* Not enough input args *)
  | _, [] -> false
  | a1 :: l1, a2 :: l2 ->
    check_arg_matching a1 a2 &&
    check_call_matching l1 l2


let filter_matching_prototypes overload args =
  (* Find suitable candidates for this call *)
  let targs = List.map Cil.typeOf args in
  let check (tparams, _vi) = check_call_matching tparams targs in
  List.filter check overload


let overloaded_call ~fundec overload block loc mk_call vf args =
  let name = vf.vf_decl.vorig_name in

  (* Find the matching prototype *)
  let tparams, new_callee =
    match filter_matching_prototypes overload args with
    | [] -> (* No matching prototype *)
        Self.warning ~current:true
          "@[No matching prototype found for this call to %s.@.\
           Expected candidates:@.\
           @[<v>       %a@]@.\
           Given arguments:@.\
           @[<v>       %a@]"
          name (pp_overload name) overload
          (pp_prototype name) (List.map Cil.typeOf args);
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
  match_call ~loc ~fundec block mk_call new_callee tparams args



(* ************************************************************************ *)
(* Format functions calls                                                   *)
(* ************************************************************************ *)

(* --- Specification building --- *)

let rec static_string a = match a.enode with
  | Const (CStr s) -> Some (Format_string.String s)
  | Const (CWStr s) -> Some (Format_string.WString s)
  | CastE (_, e) -> static_string e
  | _ -> None

let find_global env name =
  try
    Some (Environment.find_global env name)
  with Not_found ->
    Self.warning ~once:true
      "Unable to locate global %s which should be in the Frama-C LibC. \
       Correct specifications can't be generated."
      name;
    None

let find_predicate name =
  match Logic_env.find_all_logic_functions name with
  | f :: _q -> Some f (* TODO: should we warn in case of overloading? *)
  | [] ->
    Self.warning ~once:true
      "Unable to locate ACSL predicate %s which should be in the Frama-C LibC. \
      Correct specifications can't be generated."
      name;
    None

let find_field env structname fieldname =
  try
    let compinfo = Environment.find_struct env structname in
    Some (Cil.getCompField compinfo fieldname)
  with Not_found ->
    Self.warning ~once:true
      "Unable to locate %s field %s."
      structname fieldname;
    None

let find_predicate_by_width typ narrow_name wide_name =
  match Cil.unrollTypeDeep typ with
  | TPtr (TInt(IChar, _), _) -> find_predicate narrow_name
  | TPtr (t, _) when
      (* drop attributes to remove 'const' qualifiers and fc_stdlib attributes *)
      Cil_datatype.Typ.equal
        (Cil.typeDeepDropAllAttributes (Cil.unrollTypeDeep t))
        Cil.theMachine.Cil.wcharType ->
    find_predicate wide_name
  | _ ->
    Self.warning ~current:true
      "expected single/wide character pointer type, got %a (%a, unrolled %a)"
      Printer.pp_typ typ Cil_types_debug.pp_typ typ Cil_types_debug.pp_typ (Cil.unrollTypeDeep typ);
    None

let build_fun_spec env loc vf format_fun tvparams formals =
  let open Format_types in
  let _ = () in
  let fixed_params_count = Typ.params_count vf.vf_original_type in
  let sformals, vformals = List.break fixed_params_count formals in
  let here = Logic_const.here_label in

  (* Spec *)
  let sources = ref []
  and dests = ref []
  and requires = ref []
  and ensures = ref [] in
  let iterm lval =
    Logic_const.new_identified_term (Build.tlval ~loc lval)
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
    | (`ArgIn | `ArgInArray _) -> insert_source ~indirect lval
    | (`ArgOut | `ArgOutArray) -> insert_dest lval
    | `ArgInOut -> insert_source ~indirect lval; insert_dest lval
    end
  in
  let add_var ?pos (vi,dir) =
    (* Use the appropriate logical lval *)
    let lval = match dir with
    | `ArgIn -> Build.lvar vi
    | (`ArgInArray _ | `ArgOutArray) -> Build.trange_from_vi ~loc vi
    | (`ArgOut | `ArgInOut) -> Build.tvarmem ~loc vi
    in
    (* Build requires/ensures *)
    let term = Build.tvar ~loc vi in
    begin match dir with
      | `ArgInArray None ->
        let pred =
          find_predicate_by_width vi.vtype "valid_read_string" "valid_read_wstring"
        in
        begin match pred with
          | Some logic_info ->
            let labels = List.map (fun _ -> here) logic_info.l_labels in
            let p = Logic_const.papp ~loc (logic_info, labels, [term]) in
            insert_require p
          | None -> ()
        end

      | `ArgInArray (Some precision) ->
        assert (pos <> None);
        let pred =
          find_predicate_by_width vi.vtype "valid_read_nstring" "valid_read_nwstring"
        in
        begin match pred with
          | Some logic_info ->
            let labels = List.map (fun _ -> here) logic_info.l_labels in
            let nterm = match precision with
              | PStar ->
                let n_vi = List.nth vformals (Extlib.the pos) in
                Logic_utils.numeric_coerce Linteger (Build.tvar ~loc n_vi)
              | PInt n -> Cil.lconstant ~loc (Integer.of_int n)
            in
            let p = Logic_const.papp ~loc (logic_info, labels, [term; nterm]) in
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
  let pos = ref (-1) in
  List.iter (incr pos; add_var ~indirect:false ~pos:!pos) l;

  (* Add format source and additional parameters *)
  let fmt_vi = List.nth sformals format_fun.f_format_pos in
  add_var ~indirect:true (fmt_vi, `ArgInArray None);

  (* Add buffer source/dest *)
  let add_stream vi =
    (* assigns stream->__fc_FILE_data
         \from stream->__fc_FILE_data, __fc_FILE_id *)
    begin match find_field env "__fc_FILE" "__fc_FILE_data" with
    | Some fieldinfo ->
        let varfield = Build.tvarfield ~loc vi fieldinfo in
        add_lval ~indirect:false (varfield, `ArgInOut)
    | None ->
        add_var ~indirect:false (vi, `ArgInOut)
    end;
    begin match find_field env "__fc_FILE" "__fc_FILE_id" with
    | Some fieldinfo ->
        let varfield = Build.tvarfield ~loc vi fieldinfo in
        add_lval ~indirect:true (varfield, `ArgIn)
    | None -> ()
    end
  in

  (* Add a bounded buffer *)
  let add_buffer vi_buffer vi_size =
    add_var ~indirect:true (vi_size, `ArgIn);
    (* this is an snprintf-like function; compute and add its precondition:
       \valid(s + (0..n-1)) || \valid(s + (0..format_length(format)-1)) *)
    let make_valid_range tvalid_length =
      let tvar = Build.tvar ~loc vi_buffer
      and tmin = Build.tzero ~loc
      and tmax = Build.tminus ~loc tvalid_length (Build.tone ~loc) in
      let toffs = Build.trange ~loc (Some tmin) (Some tmax) in
      let term = Build.tbinop ~loc PlusPI tvar toffs in
      Logic_const.pvalid ~loc (here, term)
    in
    let size_var = Build.tvar ~loc vi_size in
    let left_pred = make_valid_range size_var in
    let pred =
      find_predicate_by_width vi_buffer.vtype "format_length" "wformat_length"
    in
    match pred with
    | Some format_length ->
      let labels = List.map (fun _ -> here) format_length.l_labels in
      let fmt_var = Build.tvar ~loc fmt_vi in
      let flen_app =
        try Build.tapp ~loc format_length labels [fmt_var]
        with Build.NotAFunction ->
          Self.abort ~current:true
            "%a should be a logic function, not a predicate"
            Printer.pp_logic_var format_length.l_var_info
      in
      let right_pred = make_valid_range flen_app in
      let p = Logic_const.por ~loc (left_pred, right_pred) in
      insert_require p
    | None -> insert_require left_pred
  in

  begin match format_fun.f_buffer, format_fun.f_kind with
  | StdIO, ScanfLike ->
      begin match find_global env "__fc_stdin" with
      | Some vi -> add_stream vi
      | None -> ()
      end
  | StdIO, PrintfLike ->
      begin match find_global env "__fc_stdout" with
      | Some vi -> add_stream vi
      | None -> ()
      end
  | Arg (i, _), ScanfLike ->
      add_var ~indirect:true (List.nth sformals i, `ArgInArray None)
  | Arg (i, size_pos), PrintfLike ->
      add_var ~indirect:true (List.nth sformals i, `ArgOutArray);
      begin match size_pos with
      | Some n ->
        add_buffer (List.nth sformals i) (List.nth sformals n)
      | None -> ()
      end
  | Stream i, _ ->
      add_stream (List.nth sformals i)
  | File i, _ ->
      let file = List.nth sformals i in
      add_var ~indirect:true (file, `ArgIn);
  | Syslog, _ -> ()
  end;

  (* Add return value dest *)
  let rettyp = Cil.getReturnType vf.vf_decl.vtype in
  if not (Cil.isVoidType rettyp) then
    add_lval ~indirect:true (Build.tresult rettyp, `ArgOut);

  (* Build the assign clause *)
  let froms = List.map (fun iterm -> iterm, From !sources) !dests in
  let assigns = Writes froms in

  (* Build the default behaviour *)
  let bhv = Cil.mk_behavior ~assigns
    ~requires:!requires ~post_cond:!ensures () in
  { (Cil.empty_funspec ()) with spec_behavior = [bhv] }


(* --- Call translation --- *)

let format_fun_call ~fundec env format_fun scope loc mk_call vf args =
  let name = vf.vf_decl.vorig_name
  and params = Typ.params vf.vf_decl.vtype in
  (* Remove the va_param parameter added during the declaration visit *)
  let fixed_params_count = Typ.params_count vf.vf_original_type in
  let sparams = List.take fixed_params_count params in 

  (* Extract the format if possible *)
  let format =
    try
      let format_arg = List.nth args format_fun.f_format_pos in
      match static_string format_arg with
      | None ->
        Self.warning ~current:true
          "Call to function %s with non-static format argument:@ \
           no specification will be generated." name;
        raise Translate_call_exn (* No syntactic hint *)
      | Some s -> Format_parser.parse_format format_fun.f_kind s
    with
    | Format_parser.Invalid_format -> raise Translate_call_exn
  in

  (* Try to type expected parameters if possible *)
  let find_typedef = Environment.find_type env in
  let tvparams =
    try
      Format_typer.type_format ~find_typedef format
    with Format_typer.Type_not_found type_name ->
      Self.warning ~current:true
        "Unable to find type %s in the source code which should be used in \
         this call:@ no specification will be generated.@ \
         Note that due to cleanup, the type may have been defined in the \
         original code but not used anywhere."
        type_name;
      raise Translate_call_exn
  in
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
  vf.vf_specialization_count <- vf.vf_specialization_count + 1;
  let ret_typ, _, _, attributes = Cil.splitFunctionType vf.vf_decl.vtype in
  let new_callee_typ = TFun (ret_typ, Some new_params, false, attributes)
  and new_name = name ^ "_va_" ^ (string_of_int vf.vf_specialization_count)
  and mk_spec formals = build_fun_spec env loc vf format_fun tvparams formals
  in
  let new_callee, glob =
    Build.function_declaration ~vattr:[Attr ("fc_stdlib_generated", [])]
      ~loc:vf.vf_decl.vdecl name new_callee_typ mk_spec
  in
  new_callee.vname <- new_name;
  new_globals := glob :: !new_globals;

  (* Translate the call *)
  Self.result ~current:true ~level:2
    "Translating call to %s to a call to the specialized version %s."
    name new_callee.vname;
  let tparams = params_types new_params in
  match_call ~loc ~fundec scope mk_call new_callee tparams args

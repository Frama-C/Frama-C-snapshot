open Cil_types
open Crowbar

let loc = Cil_datatype.Location.unknown

let field_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'a' + (!count mod 26)) in
    incr count;
    String.make 1 c

let type_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'A' + (!count mod 26)) in
    let base = String.make 1 c in
    let res =
      if !count < 26 then base else base ^ "_" ^ (string_of_int (!count / 26))
    in
    incr count; res

type kind = Complete | FAM_array | FAM_struct | Incomplete

(* pointers are always complete. *)
let mk_ptr_type (is_gcc,typ, types,_) =
  (is_gcc,TPtr (typ,[]), types, Complete)

let gen_length =
  choose
    [ const None; const (Some 0); const (Some 1); ]

let mk_array_type (is_gcc, typ, types, kind) length =
  let kind =
    match kind, length with
    | Incomplete, _ -> Incomplete
    | FAM_array, _ -> Incomplete
    | FAM_struct, _ -> Incomplete
    | Complete, None -> FAM_array
    | Complete, Some 0 ->
      if is_gcc then FAM_array else Incomplete
    | Complete, Some _ -> Complete
  in
  let length =
    Extlib.opt_map (Cil.kinteger ~loc Cil.(theMachine.kindOfSizeOf)) length
  in
  (is_gcc, TArray (typ, length, { scache = Not_Computed }, []), types, kind)

let mk_named_type (is_gcc, ttype, types, kind) =
  let tname = type_name () in
  let typedef = { torig_name = tname; tname; ttype; treferenced = true } in
  (is_gcc, TNamed(typedef,[]), GType(typedef, loc) :: types, kind)

let mk_comp_type
    cstruct nb_fields (is_gcc, typ1, types1, kind1) (_, typ2, types2, kind2) =
  let mk_field ftype =
    let fname = field_name () in (fname, ftype, None, [], loc)
  in
  let mk_fields compinfo =
    match nb_fields with
    | 0 -> compinfo.cdefined <- false; []
    | 1 -> compinfo.cdefined <- true; [ mk_field typ1 ]
    | _ -> compinfo.cdefined <- true; [ mk_field typ1; mk_field typ2 ]
  in
  let compinfo =
    Cil.mkCompInfo cstruct (type_name()) mk_fields []
  in
  let kind =
   match cstruct, nb_fields, kind1, kind2 with
     | _, 0, _, _ -> Incomplete
     | _, _, Incomplete, _ -> Incomplete
     | _, _, FAM_struct, _ -> Incomplete
     | _, 1, Complete, _ -> Complete
     | true, 1, FAM_array, _ -> Incomplete
     | _, _, FAM_array, _ -> Incomplete
     | _, _, _, Incomplete -> Incomplete
     | _, _, _, FAM_struct -> Incomplete
     | true, _, Complete, FAM_array -> FAM_struct
     | _, _, _, FAM_array -> Incomplete
     | _, _, Complete, Complete -> Complete
  in
  let types =
    match nb_fields with
    | 0 -> []
    | 1 -> types1
    | _ -> types1 @ types2
  in
  let glob =
    if nb_fields = 0 then GCompTagDecl (compinfo, loc)
    else GCompTag (compinfo,loc)
  in
  (is_gcc, TComp (compinfo, { scache = Not_Computed }, []), glob :: types, kind)

let mk_enum_type is_def is_gcc =
  let ename = type_name () in
  let eihost =
    { eorig_name = ename; ename; eitems = []; eattr = []; ereferenced = true;
      ekind = IInt }
  in
  if is_def then begin
    let einame = field_name () in
    let eival = Cil.kinteger ~loc IInt 0 in
    let item = { eiorig_name = einame; einame; eival; eihost; eiloc = loc } in
    eihost.eitems <- [ item ]
  end;
  let glob =
    if is_def then GEnumTag(eihost,loc) else GEnumTagDecl(eihost,loc)
  in
  let kind =  if is_def then Complete else Incomplete in
  (is_gcc, TEnum (eihost, []), [ glob ], kind)

let gen_type =
  let open Crowbar in
  fix
    (fun gen_type ->
       choose
         [ map [bool] (fun is_gcc -> (is_gcc, TVoid [], [], Incomplete));
           map [bool] (fun is_gcc -> (is_gcc, TInt (IInt, []), [], Complete));
           map [ gen_type ] mk_ptr_type;
           map [ gen_type; gen_length ] mk_array_type;
           map [ gen_type ] mk_named_type;
           map [ bool; range 2; gen_type; gen_type ] mk_comp_type;
           map [ bool; bool ] mk_enum_type
         ])

let generate_failure_file is_complete =
  let count = ref 0 in
  let kind = if is_complete then "complete" else "incomplete" in
  fun (typ, types) ->
    incr count;
    let name = "test_case_" ^ kind ^ "_" ^ string_of_int !count ^ ".i" in
    let dirname = Filename.dirname Sys.executable_name in
    let name = Filepath.Normalized.of_string (dirname ^ "/" ^ name) in
    let out = open_out (name:>string) in
    let fmt = Format.formatter_of_out_channel out in
    let fundec = Cil.emptyFunction "f" in
    let s =
      Cil.mkPureExpr ~valid_sid:true ~fundec (Cil.new_exp ~loc (SizeOf typ))
    in
    let b = Cil.mkBlock [ s ] in
    fundec.sbody <- b;
    let file =
      { fileName = name;
        globals =
          List.rev types @ [ GFun (fundec, loc) ];
        globinit = None;
        globinitcalled = true
      }
    in
    Kernel.add_debug_keys Kernel.dkey_print_attrs;
    Format.fprintf fmt "%a@." Cil_printer.pp_file file;
    close_out out;
    Filepath.Normalized.to_pretty_string name

let test (allowZeroSizeArrays, typ, types, kind) =
  match kind with
  | Complete | FAM_struct ->
    if not (Cil.isCompleteType ~allowZeroSizeArrays typ) then begin
      let filename = generate_failure_file true (typ, types) in
      Crowbar.fail
        ("isCompleteType declared as incomplete a complete type. \
          See example in file '" ^ filename ^ "'.")
    end;
    true
  | Incomplete | FAM_array ->
    if Cil.isCompleteType typ then begin
      let filename = generate_failure_file false (typ, types) in
      Crowbar.fail
        ("isCompleteType declared as complate an incomplete type. \
          See example in file '" ^ filename ^
         "', which should trigger an error.")
    end;
    true

let () =
  Crowbar.add_test ~name:"mutable typeOffset"
    [ gen_type ] @@ (fun x -> Crowbar.check (test x))

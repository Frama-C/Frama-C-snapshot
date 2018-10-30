open Cil_types
open Crowbar

let field_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'a' + (!count mod 26)) in
    incr count;
    String.make 1 c

let struct_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'A' + (!count mod 26)) in
    let base = String.make 1 c in
    let res =
      if !count < 26 then base else base ^ "_" ^ (string_of_int (!count / 26))
    in
    incr count; res

type attr_kind = NoAttr | Const | Mutable

let attr_of_kind =
  function NoAttr | Const -> [] | Mutable -> [ Attr( Cil.frama_c_mutable, []) ]

let tattr_of_kind =
  function NoAttr | Mutable -> [] | Const -> [ Attr ("const",[]) ]

let merge_kind field_kind subobj_kind =
  match field_kind, subobj_kind with
  | _, NoAttr -> field_kind
  | _, Mutable -> Mutable
  | _, Const -> Const

let gen_attr =
  choose [ const NoAttr; const Const; const Mutable ]

let mk_type ftype attr =
  let tname = struct_name () in
  let fname = field_name () in
  let mk_type _ =
    [ fname, ftype, None, attr, Cil_datatype.Location.unknown ]
  in
  Cil.mkCompInfo true tname ~norig:tname mk_type []

let mk_int_type field_kind =
  let field_attr = attr_of_kind field_kind in
  let typ_attr = tattr_of_kind field_kind in
  [ mk_type (TInt (IInt, typ_attr)) field_attr ], field_kind

let mk_composite_type field_kind (subtypes, subkind) =
  let field_attr = attr_of_kind field_kind in
  let typ_attr = tattr_of_kind field_kind in
  let subtype = List.hd subtypes in
  let kind = merge_kind field_kind subkind in
  let field_type = TComp (subtype, { scache = Not_Computed }, typ_attr) in
  (mk_type field_type field_attr) :: subtypes, kind

let rec mk_offset { cfields } =
  let field = List.hd cfields in
  let offset =
    match field.ftype with TComp(comp,_,_) -> mk_offset comp | _ -> NoOffset
  in
  Field (field, offset)

let gen_type =
  let open Crowbar in
  fix
    (fun gen_type ->
       choose
         [ map [ gen_attr ] mk_int_type;
           map [ gen_attr; gen_type ] mk_composite_type ])

let generate_failure_file is_const =
  let count = ref 0 in
  let kind = if is_const then "const" else "mutable" in
  let loc = Cil_datatype.Location.unknown in
  fun types ->
    incr count;
    let name = "test_case_" ^ kind ^ "_" ^ string_of_int !count ^ ".i" in
    let dirname = Filename.dirname Sys.executable_name in
    let name = Filepath.Normalized.of_string (dirname ^ "/" ^ name) in
    let out = open_out (name:>string) in
    let fmt = Format.formatter_of_out_channel out in
    let typ = List.hd types in
    let x =
      Cil.makeGlobalVar "x" (TComp (typ, { scache = Not_Computed }, []))
    in
    let y =
      Cil.makeGlobalVar "y" (TInt (IInt,[]))
    in
    let lvx = Var x, mk_offset typ in
    let lvy = Var y, NoOffset in
    let lv, rv = if is_const then lvy, lvx else lvx, lvy in
    let instr = Set (lv, Cil.new_exp ~loc (Lval rv),loc) in
    let s = Cil.mkStmtOneInstr instr in
    let b = Cil.mkBlock [ s ] in
    let f = Cil.makeGlobalVar "f" (TFun (TVoid [], Some [], false, [])) in
    let fdef =
      { svar = f;
        sformals = [];
        slocals = [];
        smaxid = 0;
        sbody = b;
        smaxstmtid = None;
        sallstmts = [ s ];
        sspec = Cil.empty_funspec () }
    in
    let file =
      { fileName = name;
        globals =
          List.rev_map (fun typ -> GCompTag (typ,loc)) types @
          [ GVarDecl (x,loc); GVarDecl(y,loc); GFun (fdef, loc) ];
        globinit = None;
        globinitcalled = true
      }
    in
    Kernel.add_debug_keys Kernel.dkey_print_attrs;
    Format.fprintf fmt "%a@." Cil_printer.pp_file file;
    close_out out;
    Filepath.Normalized.to_pretty_string name

let test (types, kind) =
  let out_type = List.hd types in
  let offset = mk_offset out_type in
  let inner_type =
    Cil.typeOffset (TComp (out_type, { scache = Not_Computed }, [])) offset
  in
  match kind with
  | NoAttr | Mutable ->
    if Cil.typeHasAttribute "const" inner_type then begin
      let filename = generate_failure_file false types in
      Crowbar.fail
        ("typeOffset declared const a field that should have been mutable. \
          See example in file '" ^ filename ^ "'.")
    end;
    true
  | Const ->
    if not (Cil.typeHasAttribute "const" inner_type) then begin
      let filename = generate_failure_file true types in
      Crowbar.fail
        ("typeOffset should have marked a field as const. \
          See example in file '" ^ filename ^ "'.")
    end;
    true

let () = Crowbar.add_test ~name:"mutable typeOffset" [ gen_type ] @@ 
  (fun x -> Crowbar.check (test x))

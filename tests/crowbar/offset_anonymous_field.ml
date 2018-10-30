open Cil_types
open Crowbar

let field_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'a' + (!count mod 26)) in
    incr count;
    String.make 1 c

let anonFieldName =
  let count = ref 0 in
  fun () -> incr count; Format.sprintf "%s_%d" Cabs2cil.anonCompFieldName !count

let struct_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'A' + (!count mod 26)) in
    let base = String.make 1 c in
    let res =
      if !count < 26 then base else base ^ "_" ^ (string_of_int (!count / 26))
    in
    incr count; res

let mk_compinfo cstruct field1 field2 field3 =
  let tname = struct_name () in
  let mk_type _ = [ field1; field2; field3 ] in
  Cil.mkCompInfo cstruct tname ~norig:tname mk_type []

type result =
  { designator: string option;
    offsets: offset list Datatype.String.Map.t;
    mytype: Cil_types.typ;
    structs: compinfo list
  }

let int_result =
  { designator = None;
    offsets = Datatype.String.Map.empty;
    mytype = TInt (IInt,[]);
    structs = [] }

let mk_field { mytype } anon =
  let name =
    if anon then begin
      match mytype with
      | TComp (_, _, _) -> anonFieldName ()
      | _ -> Cil.missingFieldName
    end else field_name ()
  in
  name, mytype, None, [], Cil_datatype.Location.unknown

let lift_offset cstruct res1 anon1 field1 res2 anon2 field2 res3 anon3 field3 =
  let add_offsets anon field name offsets acc =
    if (not cstruct) && Datatype.String.Map.mem name acc then acc
    else
      let old_offsets =
        match Datatype.String.Map.find_opt name acc with
        | None -> []
        | Some l -> l
      in
      if anon then begin
        let offsets = List.map (fun o -> Field(field, o)) offsets in
        Datatype.String.Map.add name (old_offsets @ offsets) acc
      end else if field.fname = name then
        Datatype.String.Map.add name [Field(field, NoOffset)] acc
      else acc
  in
  let add_all_offsets anon field map acc =
    Datatype.String.Map.fold (add_offsets anon field) map acc
  in
  Datatype.String.Map.empty |>
  add_all_offsets anon1 field1 res1.offsets |>
  add_all_offsets anon2 field2 res2.offsets |>
  add_all_offsets anon3 field3 res3.offsets

let lift_designator anon designator field =
  if anon then begin
    match designator with
    | None -> bad_test ()
    | Some _ -> designator
  end else Some field.fname

let mk_composite_type choice cstruct res1 anon1 res2 anon2 res3 anon3 =
  let field1 = mk_field res1 anon1 in
  let field2 = mk_field res2 anon2 in
  let field3 = mk_field res3 anon3 in
  let info = mk_compinfo cstruct field1 field2 field3 in
  let field1, field2, field3 =
    match info.cfields with
    | [ field1; field2; field3 ] -> field1, field2, field3
    | _ -> bad_test()
  in
  let designator =
    match choice with
    | 0 -> lift_designator anon1 res1.designator field1
    | 1 -> lift_designator anon2 res2.designator field2
    | 2 -> lift_designator anon3 res3.designator field3
    | _ -> bad_test()
  in
  let offsets =
    lift_offset cstruct res1 anon1 field1 res2 anon2 field2 res3 anon3 field3
  in
  let mytype = TComp (info, { scache = Not_Computed }, []) in
  let structs = info :: res1.structs @ res2.structs @ res3.structs in
  { designator; mytype; structs; offsets }

let rec mk_offset { cfields } =
  let field = List.hd cfields in
  let offset =
    match field.ftype with TComp(comp,_,_) -> mk_offset comp | _ -> NoOffset
  in
  Field (field, offset)

let rec gen_type_l n =
  if n <= 0 then lazy (const int_result)
  else
    lazy
      (let open Crowbar in
       choose
         [ const int_result;
           map
             [ range 3; bool;
               gen_type (n-1); bool;
               gen_type (n-1); bool;
               gen_type (n-1); bool]
             mk_composite_type ])
and gen_type n = unlazy (gen_type_l n)

let generate_failure_file =
  let count = ref 0 in
  let loc = Cil_datatype.Location.unknown in
  fun offset types ->
    incr count;
    let name = "test_case_" ^ string_of_int !count ^ ".i" in
    let dirname = Filename.dirname Sys.executable_name in
    let name = Filepath.Normalized.of_string (dirname ^ "/" ^ name) in
    let out = open_out (name:>string) in
    let fmt = Format.formatter_of_out_channel out in
    let typ = List.hd types in
    let x =
      Cil.makeGlobalVar "x" (TComp (typ, { scache = Not_Computed }, []))
    in
    let lvx = Var x, offset in
    let typ = Cil.typeOfLval lvx in
    let init = Cil.makeZeroInit ~loc typ in
    let f = Cil.makeGlobalVar "f" (TFun (TVoid [], Some [], false, [])) in
    let fdef =
      { svar = f;
        sformals = [];
        slocals = [];
        smaxid = 0;
        sbody = Cil.mkBlock [];
        smaxstmtid = None;
        sallstmts = [ ];
        sspec = Cil.empty_funspec () }
    in
    let y = Cil.makeLocalVar fdef "y" typ in
    let init_instr = Local_init (y, AssignInit init, loc) in
    let instr = Set (lvx, Cil.evar ~loc y,loc) in
    let s1 = Cil.mkStmtOneInstr init_instr in
    let s2 = Cil.mkStmtOneInstr instr in
    let b = Cil.mkBlock [ s1; s2 ] in
    fdef.sallstmts <- [ s1; s2 ];
    fdef.sbody <- b;
    let file =
      { fileName = name;
        globals =
          List.rev_map (fun typ -> GCompTag (typ,loc)) types @
          [ GVarDecl (x,loc); GFun (fdef, loc) ];
        globinit = None;
        globinitcalled = true
      }
    in
    Kernel.add_debug_keys Kernel.dkey_print_attrs;
    Format.fprintf fmt "%a@." Cil_printer.pp_file file;
    close_out out;
    Filepath.Normalized.to_pretty_string name

let test { designator; offsets; structs } =
  match structs with
  | [] -> bad_test ()
  | comp :: _ ->
    (match designator with
     | None -> bad_test ()
     | Some field ->
       let offset, expected =
         match Datatype.String.Map.find_opt field offsets with
         | None | Some [] -> bad_test ()
         | Some (hd :: _ as l) -> hd, l
       in
       try
         let result = Cabs2cil.fieldsToInit comp designator in
         if List.length result <> List.length expected ||
            not (List.for_all2 Cil_datatype.Offset.equal result expected)
         then begin
           let filename = generate_failure_file offset structs in
           let pp_sep fmt () = Format.pp_print_string fmt " " in
           Crowbar.fail
             (Format.asprintf
                "fieldsToInit didn't find appropriate offset for %s in %s.\n\
                 expected offsets were %a\n\
                 returned offsets are  %a"
                   field filename
                   (Format.pp_print_list ~pp_sep Cil_printer.pp_offset) expected
                   (Format.pp_print_list ~pp_sep Cil_printer.pp_offset) result)
         end
       with Log.AbortFatal _ ->
         let filename = generate_failure_file offset structs in
         Crowbar.fail
           ("fieldsToInit failed on the test given in file " ^ filename ^
            ", for field " ^ field))
let () =
  Crowbar.add_test ~name:"designator and anonymous fields"
    [ gen_type 20 ] @@ test

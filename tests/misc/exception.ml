open Cabs
open Cil_types

let rec init_exn exn init acc =
  match init with
    | SingleInit init ->
        Cil.mkStmtOneInstr (Set(exn,init,Cil_datatype.Location.unknown))
        :: acc
    | CompoundInit (ct,initl) ->
        Cil.foldLeftCompound
          ~implicit:false
          ~doinit:(fun off' i' _ acc ->
                   init_exn (Cil.addOffsetLval off' exn) i' acc)
          ~ct ~initl ~acc

let add_throw_test f exn_type test init =
  let throw_block = Cil.mkBlock [] in
  let exn = Cil.makeLocalVar f ~scope:throw_block "exn" exn_type in
  let valid_sid = true in
  let set_exn_stmts = init_exn (Var exn, NoOffset) init [] in
  let loc = Cil_datatype.Location.unknown in
  let throw_stmt =
    Cil.mkStmt
      ~valid_sid
      (Throw (Some (Cil.evar ~loc exn, exn_type), loc))
  in
  throw_block.bstmts <- List.rev (throw_stmt :: set_exn_stmts);
  let new_body = Cil.mkStmt ~valid_sid (If(test, throw_block, f.sbody,loc)) in
  f.sbody <- Cil.mkBlock [ new_body ]

let add_my_exn my_exn f =
  let c = Cil.evar (List.hd f.sformals) in
  let exn_type = TComp(my_exn,{ scache = Not_Computed},[]) in
  let loc = Cil_datatype.Location.unknown in
  let init =
    CompoundInit(
        exn_type,
        [Field(List.hd my_exn.cfields, NoOffset), SingleInit (Cil.zero ~loc)])
  in
  add_throw_test f exn_type c init

let add_int_exn f =
  let c = Cil.evar (List.hd f.sformals) in
  let loc = Cil_datatype.Location.unknown in
  let test =
    Cil.new_exp ~loc (BinOp (Lt,c,Cil.kinteger ~loc IInt 50,Cil.intType))
  in
  add_throw_test f Cil.intType test (SingleInit (Cil.zero ~loc))

let add_int_ptr_exn glob f =
  let c = Cil.evar (List.hd f.sformals) in
  let loc = Cil_datatype.Location.unknown in
  let test =
    Cil.new_exp ~loc (BinOp (Gt,c,Cil.kinteger ~loc IInt 150, Cil.intType))
  in
  let init =
    SingleInit (Cil.new_exp ~loc (AddrOf(Var glob,NoOffset)))
  in
  add_throw_test f Cil.intPtrType test init

let add_catch my_exn my_exn2 f =
  let exn_type = TComp(my_exn, { scache = Not_Computed }, []) in
  let exn_type2 = TComp(my_exn2, {scache = Not_Computed }, []) in
  let exn_field = Field (List.hd my_exn.cfields, NoOffset) in
  let exn2_field = Field (List.hd my_exn2.cfields, NoOffset) in
  let loc = Cil_datatype.Location.unknown in
  let real_locals = f.sbody.blocals in
  let v1 = Cil.makeLocalVar f "exn" exn_type in
  let v2 = Cil.makeLocalVar f "y" Cil.intType in
  let v3 = Cil.makeLocalVar f "exn_aux" exn_type in
  let v4 = Cil.makeLocalVar f "exn2" exn_type2 in
  let v5 = Cil.makeLocalVar f "not_thrown" Cil.doubleType in
  f.sbody.blocals <- real_locals;
  let id_block =
    Cil.mkBlock [Cil.mkStmtOneInstr (Set (Cil.var v1, Cil.evar ~loc v3, loc))]
  in
  let convert_exn_block =
    Cil.mkBlock
      [ Cil.mkStmtOneInstr
          (Set ((Var v1, exn_field),
                Cil.new_exp ~loc (Lval (Var v4, exn2_field)),
                loc))]
  in
  let catch_stmt =
    Cil.mkStmt
      (TryCatch(
           f.sbody,
           [ Catch_exn (v1,[(v3,id_block); (v4, convert_exn_block)]),
             Cil.mkBlock 
               [ Cil.mkStmt
                   (Return
                      (Some (Cil.new_exp ~loc (Lval (Var v1, exn_field))),
                       loc))];
             Catch_exn (v2,[]),
             Cil.mkBlock
               [ Cil.mkStmt (Return (Some (Cil.evar ~loc v2),loc))];
             Catch_exn (v5,[]),
             Cil.mkBlock
               [ Cil.mkStmt (Return (Some (Cil.mone ~loc), loc))];
             Catch_all, Cil.mkBlock [ Cil.mkStmt (Throw (None, loc)) ]
           ],
           loc))
  in
  f.sbody <- Cil.mkBlock [ catch_stmt ]

let change_body my_exn my_exn2 glob f =
  match f.svar.vname with
    | "f1" -> add_my_exn my_exn f; File.must_recompute_cfg f
    | "f2" -> add_int_exn f; File.must_recompute_cfg f
    | "f3" -> add_int_ptr_exn glob f; File.must_recompute_cfg f
    | "f4" -> add_my_exn my_exn2 f; File.must_recompute_cfg f
    | "h" -> add_catch my_exn my_exn2 f; File.must_recompute_cfg f
    | _ -> ()

let add_exn ast =
  let my_exn = ref None in
  let my_exn2 = ref None in
  let glob = ref None in
  let treat_glob =
    function
    | GCompTag(ci,_) ->
        (match !my_exn with
           | None -> my_exn := Some ci
           | Some _ -> my_exn2 := Some ci)
    | GVar(v,_,_) when v.vname = "x" -> glob := Some v
    | GFun(f,_) ->
        change_body
          (Extlib.the !my_exn) (Extlib.the !my_exn2) (Extlib.the !glob) f
    | _ -> ()
  in
  List.iter treat_glob ast.globals

let loc = Cil_datatype.Location.unknown
let stmt stmt_node = { stmt_ghost = false; stmt_node }
let var v = { expr_loc = loc; expr_node = VARIABLE v }

let mk_exn_cabs b =
    { blabels = []; Cabs.battrs = [];
      Cabs.bstmts =
     [ stmt
         (IF (var "c",
              stmt (THROW (Some (var "x"),loc)),
              stmt (BLOCK (b,loc,loc)),loc))] }

let mk_catch_cabs b =
  { blabels = []; Cabs.battrs = [];
    Cabs.bstmts =
      [ stmt
        (TRY_CATCH
          (stmt (BLOCK (b,loc,loc)),
           [Some ([SpecType Tint],("x",JUSTBASE,[],loc)),
            stmt (
              RETURN (
                { expr_loc = loc; expr_node = CONSTANT (CONST_INT "3")}, loc));
            None,
            stmt (
              RETURN (
                { expr_loc = loc; expr_node = CONSTANT (CONST_INT "4")}, loc))],
           loc))]}

let add_exn_cabs (f,l) =
  let treat_one_global (b,d) =
    let d =
      match d with
      | FUNDEF (s,(t,("f",dt,a,l)),b,l1,l2) ->
        FUNDEF (s,(t,("f",dt,a,l)), mk_exn_cabs b,l1,l2)
      | FUNDEF (s,(t,("g",dt,a,l)),b,l1,l2) ->
        FUNDEF (s,(t,("g",dt,a,l)), mk_catch_cabs b,l1,l2)
      | _ -> d
    in
    b,d
  in
  (f, List.map treat_one_global l)

let () = Frontc.add_syntactic_transformation add_exn_cabs

let add_exn_cat = File.register_code_transformation_category "add_exn"

let () = File.add_code_transformation_before_cleanup add_exn_cat add_exn

      

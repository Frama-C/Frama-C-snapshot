open Cil_types
open Cil

let category = Kernel.register_category "refresh-test"

module Check(M: Datatype.S_with_collections) =
struct
    let check cat fold bhv =
      let f o c (orig, copy) = M.Set.add o orig, M.Set.add c copy in
      let (orig,copy) = fold bhv f (M.Set.empty, M.Set.empty) in
      let common = M.Set.inter orig copy in
      if not (M.Set.is_empty common) then begin
        Format.printf "ids for %s are not properly refreshed.@." cat;
      end;
      orig, copy, common
end

module CheckVarinfo = Check(Cil_datatype.Varinfo)

module CheckCompinfo = Check(Cil_datatype.Compinfo)

module CheckStmt = Check (Cil_datatype.Stmt)

module CheckLogic_var = Check(Cil_datatype.Logic_var)

let main () =
  Ast.compute ();
  let p = Project.create "p" in
  let vis = new Visitor.frama_c_refresh p in
  Format.printf "Start@.";
  File.init_project_from_visitor p vis;
  Cil_datatype.(
    let orig_id, copy_id, shared_id =
      CheckVarinfo.check "varinfo" fold_visitor_varinfo vis#behavior
    in
    if Kernel.is_debug_key_enabled category then begin
      Varinfo.Set.iter
        (fun x ->
          Format.printf "variable id %d (%s) is in orig@." x.vid x.vname)
        orig_id;
      Varinfo.Set.iter
        (fun x ->
          Format.printf "variable id %d (%s) is in copy@." x.vid x.vname)
        copy_id;
      Varinfo.Set.iter
        (fun x -> Format.printf "variable id %d (%s) is reused@." x.vid x.vname)
        shared_id;
      end;
    let _ =
      CheckCompinfo.check "compinfo" fold_visitor_compinfo vis#behavior
    in
    let _ =
      CheckStmt.check "stmt" fold_visitor_stmt vis#behavior;
    in
    let orig_id, copy_id, shared_id = 
      CheckLogic_var.check "logic var" fold_visitor_logic_var vis#behavior
    in
    if Kernel.is_debug_key_enabled category then begin
      Logic_var.Set.iter
        (fun x -> Format.printf "logic variable id %d (%s) is in orig@."
          x.lv_id x.lv_name)
        orig_id;
      Logic_var.Set.iter
        (fun x -> Format.printf "logic variable id %d (%s) is in copy@."
          x.lv_id x.lv_name)
        copy_id;
      Logic_var.Set.iter
        (fun x -> Format.printf "logic variable id %d (%s) is reused@."
          x.lv_id x.lv_name)
        shared_id;
    end
  );
  Project.on p !Db.Value.compute ();
  File.pretty_ast ~prj:p ()

let () = Db.Main.extend main

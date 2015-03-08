open Cil
open Cil_types

let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let get_rte_annotations = 
  Dynamic.get ~plugin:"RteGen" "get_rte_annotations" 
    (Datatype.func
       Cil_datatype.Stmt.ty
       (let module L = Datatype.List(Cil_datatype.Code_annotation) in L.ty))

let fetch_stmts_visitor () = object
  inherit nopCilVisitor
  val mutable stmts : stmt list = []
  method fetch_stmts () = List.rev stmts
  method vstmt stmt = stmts <- stmt :: stmts ; DoChildren
end

let get_stmts kf = 
  match kf.fundec with 
  | Definition (f,_) ->
    let vis = fetch_stmts_visitor () in
    let _ = visitCilFunction (vis :> cilVisitor) f in
    vis#fetch_stmts ()
  | _ -> []

let show_rte_of_kf kf = 
  let is_annot = ref false in
  Kernel.log "Rte-generated annotations for function %a" 
    Kernel_function.pretty kf ;
  List.iter
    (fun stmt ->
      let lannot = get_rte_annotations stmt in
      match lannot with
      | [] -> ()
      | lannot ->
	is_annot := true;
	Kernel.log "For Statement %a" 
	  (Printer.without_annot Printer.pp_stmt) stmt;
	List.iter
	  (fun a -> Kernel.log "%a" Printer.pp_code_annotation a) 
	  lannot)
    (get_stmts kf);
  if not !is_annot then Kernel.log "None"
      
let main () =
  Ast.compute () ;
  Kernel.SignedOverflow.on ();
  let do_rte = !Db.RteGen.do_rte in
  Globals.Functions.iter (fun kf -> do_rte kf);
  print () ;
  Globals.Functions.iter show_rte_of_kf

let () = Db.Main.extend main

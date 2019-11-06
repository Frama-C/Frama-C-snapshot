open Cil_types

class vis prj = object(self)
  inherit Visitor.frama_c_copy prj

  val mutable my_var = None

  method private create_block create s instr =
    let s1 = Cil.mkStmtOneInstr ~valid_sid:true instr in
    let b = Cil.mkBlock [s1] in
    if create then begin
      let f = Visitor_behavior.Get.fundec
          self#behavior (Extlib.the self#current_func) in
      let y = Cil.makeLocalVar f ~scope:b "y" (TInt(IInt,[])) in
      my_var <- Some y;
      let loc = Cil_datatype.Location.unknown in
      let s2 =
        Cil.mkStmtOneInstr ~valid_sid:true
          (Local_init(y,AssignInit(SingleInit(Cil.zero ~loc)),loc))
      in
      b.bstmts <- s2 :: b.bstmts;
      let b = Cil.transient_block b in
      s.skind <- Block b;
    end;
    Cil.JustCopy

  method! vstmt_aux s =
    match s.skind with
    | Instr (Local_init _ as instr) ->
      (try
         self#create_block true s instr
       with Log.AbortFatal _ ->
         Kernel.feedback "transient_block fatal error on %a as expected"
           Printer.pp_instr instr;
         let f = Visitor_behavior.Get.fundec
             self#behavior (Extlib.the self#current_func) in
         let y = Extlib.the my_var in
         f.slocals <-
           List.filter
             (fun v -> not (Cil_datatype.Varinfo.equal v y)) f.slocals;
         Cil.DoChildren)
    | Instr (Set ((Var { vorig_name = "x" }, NoOffset),_,_) as instr) ->
      self#create_block true s instr
    | Instr (Call _ as instr) -> self#create_block false s instr
    | _ -> Cil.DoChildren
end

let main () =
  Ast.compute ();
  let prj = File.create_project_from_visitor "test" (fun prj -> new vis prj) in
  File.pretty_ast ~prj ()

let () = Db.Main.extend main

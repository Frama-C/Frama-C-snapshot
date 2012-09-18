open Cil_types

module Options =
  Plugin.Register(struct let name = "Test"
                         let shortname= "test"
                         let help = "test" end)
  
class transform prj = object(_self)

  inherit Visitor.frama_c_copy prj 
  
  method vglob_aux = function
    
  | GFun (_fdec, _loc)  ->
 
    let mk_formal = function l -> begin match l with
	| GFun (fundec, loc) :: [] ->
	    Project.on 
	      prj
	      (fun () -> 	  
	        Options.feedback "current prj = %a"
                  Project.pretty (Project.current ());
	        ignore(Cil.makeFormalVar fundec "ok" Cil.intType))
	      ();
	  let svar = { fundec.svar with vtype = fundec.svar.vtype } in
	  let sformals = fundec.sformals in
	  let fundec = { fundec with sformals = sformals } in
	  let g = GFun({ fundec with svar = svar }, loc) in
	  [g]
	| _ -> assert false

    end
      in
      Cil.DoChildrenPost mk_formal



  | GVarDecl _
  | GVar _
  | GType _
  | GCompTag _
  | GCompTagDecl _ 
  | GEnumTag _
  | GEnumTagDecl _
  | GAsm _
  | GPragma _
  | GText _
  | GAnnot _ -> Cil.DoChildren



  
 end

let generate_code name =
  let transform prj = new transform prj in
  File.create_project_from_visitor name transform

let main () = 
  if Project.get_name (Project.current()) <> "test" then
    ignore (generate_code "test")

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make"
End:
*)

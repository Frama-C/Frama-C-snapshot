open Cil_types

module Options =
  Plugin.Register(struct let name = "Test"
                         let shortname= "test"
                         let help = "test" end)
  
class transform prj = object(_self)

  inherit Visitor.frama_c_copy prj 
  
  method! vglob_aux = function
    | GFun (_fdec, _loc) as g  -> 
      let mk_formal = function l -> begin match l with
      | GFun (fundec, loc) :: [] ->
	Project.on 
	  prj
	  (fun () -> 	 
	    Options.feedback "current prj = %a"
              Project.pretty (Project.current ());
	    ignore(Cil.makeFormalVar fundec "ok" Cil.intType))
	  ();
	let g = GFun({ fundec with svar = fundec.svar }, loc) in
	[g]
      | _ -> assert false
      end
      in 
      Cil.ChangeDoChildrenPost( [g], mk_formal)
   

    | GVarDecl (_fspec, _vi, _loc) as g ->
      let mk_gvar_decl = function l -> 
	begin match l with
	  | (GVarDecl (_fspec, vi, _loc) as g) :: [] ->
	    if (Cil.isFunctionType vi.vtype && 
		  not (Cil.Frama_c_builtins.mem vi.vname))
	    then
	      begin match vi.vtype with
		| TFun(typ, args, varity, attr) ->
		  let vtype = Cil.argsToList args in
		  let new_fun_typ =  TFun(
		    typ, Some (vtype @ [ "ok", Cil.intType, [] ]),
		    varity, attr)
		  in
                  vi.vtype <- new_fun_typ;
		  Project.on
		    prj
		    (fun () -> Cil.setFormalsDecl vi new_fun_typ;) ();
		  [ g ]
		| _ -> assert false
	      end
	    else
	      [g]
	  | _ -> assert false
	end
      in
      Cil.ChangeDoChildrenPost ([g], mk_gvar_decl)
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

  method! vinst = function
    | Call(_,{ Cil_types.enode = Lval (Var _, NoOffset)},_,_) as i ->
        let add_zero = function
          | [Call(res,f,args,loc)] ->
              let args =
                args @ [ Cil.zero ~loc ]
              in
              [Call(res,f,args,loc)]
          | _ -> assert false
        in
        Cil.ChangeDoChildrenPost([i], add_zero)
    | _ -> Cil.DoChildren
  
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
compile-command: "make -C ../.. tests/misc/Change_formals.cmo"
End:
*)

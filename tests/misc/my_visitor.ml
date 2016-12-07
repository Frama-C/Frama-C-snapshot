open Logic_const
open Cil
open Cil_types

module P = 
  Plugin.Register
    (struct
      let name = "My_visitor"
      let shortname = "my_vis"
      let help = ""
     end)

module S =
  P.True(struct let option_name = "-s" let help = "" end)

module S2 =
  P.False(struct let option_name = "-s2" let help = "" end)

let emitter1 = 
  Emitter.create "emitter1" [ Emitter.Code_annot ]
    ~correctness:[ S.parameter ] ~tuning:[]

let emitter2 = 
  Emitter.create "emitter2" [ Emitter.Code_annot ]
    ~correctness:[ S2.parameter ] ~tuning:[]

let emitter =
  let even = ref true in
  fun () ->
    let e = !even in
    even := not e;
    if !even then emitter1 else emitter2

let add_assert loc kf stmt =
  let x = Cil_const.make_logic_var_quant "x" Linteger in
  let e = emitter () in
  Annotations.add_assert e ~kf stmt
    (pforall ([x],prel(Req,
                       {term_name = [];
                        term_node = TLval (TVar x,TNoOffset);
                        term_type = Linteger;
                        term_loc = loc},
                       {term_name = [];
                        term_node = TLval (TVar x,TNoOffset);
                        term_type = Linteger;
                        term_loc = loc}
                      )));;

class foo = object (self)

  inherit Visitor.frama_c_inplace

  method! vstmt_aux stmt =
    let loc = Cil.CurrentLoc.get () in
    add_assert loc (Extlib.the self#current_kf) stmt;
    DoChildren

  method! vglob_aux _ = DoChildren

end;;

let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let main () =
  (* The initial AST *)
  print ();
  let file = Ast.get () in
  ignore (Cil.visitCilFileSameGlobals (new foo:>cilVisitor) file);
  (* The AST with all asserts *)
  print ();
  Kernel.SafeArrays.set false;
  Project.clear 
    ~selection:(State_selection.Static.with_dependencies S.self) ();
  (* The AST with 1/2 asserts *)
  print ()

let () = Db.Main.extend main

(* This other main is a simple test for deep copy. *)

let main () =
  let p = File.create_project_from_visitor "param" (new Visitor.frama_c_copy) in
  let selection = State_selection.singleton Kernel.LibEntry.self in
  Project.copy ~selection p;
  Kernel.LibEntry.on ();
  assert (Kernel.LibEntry.get ());
  assert (Project.on p ~selection (fun () -> not (Kernel.LibEntry.get ())) ())

let () = Db.Main.extend main

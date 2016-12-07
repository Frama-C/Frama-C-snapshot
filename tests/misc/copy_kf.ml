open Cil
open Cil_types

let clone name =
  let kf = Globals.Functions.find_by_name name in
  let vi = Globals.Functions.get_vi kf in
  let kf' =
    Visitor.visitFramacKf (new Visitor.frama_c_refresh (Project.current())) kf
  in
  let vi' = Kernel_function.get_vi kf' in
  vi'.vname <- "new_" ^ name;
  let ast = Ast.get () in
  let loc = Kernel_function.get_location kf' in
  let new_glob =
    List.fold_right
      (fun g acc ->
         match g with
         | GFun(f,_) when f.svar == vi ->
           g :: GFun(Kernel_function.get_definition kf', loc) :: acc
         | GFunDecl(_,vi'',_) when vi'' == vi && Ast.is_def_or_last_decl g ->
           g :: GFunDecl(Cil.empty_funspec(), vi', loc) :: acc
         | _ -> g::acc)
      ast.globals []
  in
  ast.globals <- new_glob;
  Ast.mark_as_grown ();
  kf'

let replace_call def proto =
  let vi = Kernel_function.get_vi proto in
  let vis = object
    inherit Visitor.frama_c_inplace
    method! vinst = function
      | Call(rcv,{enode=Lval(Var _,NoOffset); eloc=loc}, args, l) ->
        ChangeTo [Call(rcv, Cil.new_exp ~loc (Lval (Var vi,NoOffset)), args, l)]
      | _ -> SkipChildren
  end
  in
  let body = Kernel_function.get_definition def in
  ignore (Visitor.visitFramacFunction vis body)

let main () =
  let kff = clone "f" in
  Filecheck.check_ast "clone-f";
  Kernel.feedback "After cloning f:@\n%t"
    (fun fmt -> File.pretty_ast ~fmt ());
  let kfg = clone "g" in
  Filecheck.check_ast "clone-g";
  Kernel.feedback "After cloning g:@\n%t"
    (fun fmt -> File.pretty_ast ~fmt ());
  replace_call kff kfg;
  Filecheck.check_ast "stmt-replace";
  Kernel.feedback "After replacement:@\n%t"
    (fun fmt -> File.pretty_ast ~fmt ())

let () = Db.Main.extend main

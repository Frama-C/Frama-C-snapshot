open Cil_types

let run () =
  let file = Ast.get () in
  let kf = Globals.Functions.find_by_name "f" in
  let li = Cil_const.make_logic_info "i" in
  let lj = Cil_const.make_logic_info "j" in
  let lk = Cil_const.make_logic_info "k" in
  let ll = Cil_const.make_logic_info "l" in
  li.l_var_info.lv_type <- Linteger;
  lj.l_var_info.lv_type <- Linteger;
  lk.l_var_info.lv_type <- Linteger;
  ll.l_var_info.lv_type <- Linteger;
  li.l_type <- Some Linteger;
  lj.l_type <- Some Linteger;
  lk.l_type <- Some Linteger;
  ll.l_type <- Some Linteger;
  li.l_body <-
    LBterm
    (Logic_const.term
       (TBinOp
          (PlusA,
           Logic_const.term (Tapp(lj,[],[])) Linteger,
           Logic_const.term (Tapp(lk,[],[])) Linteger))
       Linteger);
  lj.l_body <- LBterm (Logic_const.term (Tapp(ll,[],[])) Linteger);
  lk.l_body <- LBterm (Logic_const.term (Tapp(ll,[],[])) Linteger);
  ll.l_body <- LBterm (Logic_const.tinteger 1);
  let post_cond =
    [Normal,
     Logic_const.new_predicate 
       (Logic_const.prel 
          (Req,
           Logic_const.term (Tapp(li,[],[])) Linteger,
           Logic_const.term (Tapp(li,[],[])) Linteger))]
  in
  let bhv = Cil.mk_behavior ~post_cond () in
  Annotations.add_behaviors Emitter.end_user kf [ bhv ];
  let loc = Cil_datatype.Location.unknown in
  let dli = Dfun_or_pred (li,loc) in
  let dlj = Dfun_or_pred (lj,loc) in
  let dlk = Dfun_or_pred (lk,loc) in
  let dll = Dfun_or_pred (ll,loc) in
  Annotations.add_global Emitter.end_user dli;
  Annotations.add_global Emitter.end_user dlj;
  Annotations.add_global Emitter.end_user dlk;
  Annotations.add_global Emitter.end_user dll;
  file.globals <- file.globals @ 
    [ GAnnot (dli,loc); 
      GAnnot (dlj,loc); 
      GAnnot (dll, loc); 
      GAnnot (dlk,loc) ];
  Logic_utils.add_logic_function li;
  Logic_utils.add_logic_function lj;
  Logic_utils.add_logic_function lk;
  Logic_utils.add_logic_function ll;
  File.pretty_ast ();
  File.reorder_ast ();
  File.pretty_ast ();
  Visitor.visitFramacFileSameGlobals
    (new File.check_file "reordered")
    (Ast.get())

let () = Db.Main.extend run

open Logic_ptree
open Cil_types
open Logic_typing

let type_foo ~typing_context ~loc l =
  let _loc = loc in
  let preds =
    List.map
      (typing_context.type_predicate
         typing_context typing_context.pre_state)
      l
  in
  Ext_preds preds

module Count = State_builder.Counter(struct let name = "Count" end)

module Bar_table =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Datatype.List(Cil_datatype.Predicate))
    (struct
        let name = "Bar_table"
        let dependencies = [ Ast.self; Count.self ]
        let size = 3
     end)

let type_bar ~typing_context ~loc l =
  let _loc = loc in
  let i = Count.next() in
  let p =
    List.map
      (typing_context.type_predicate
         typing_context
         (typing_context.post_state [Normal])) l
  in
  Bar_table.add i p;
  Ext_id i

let print_bar prt fmt ext =
  match ext with
  | Ext_id idx ->
    let l = Bar_table.find idx in
    Pretty_utils.pp_list
      ~pre:"@[<hov 2>" ~sep:",@ " ~suf:"@]" prt#predicate fmt l
  | Ext_preds _ | Ext_terms _ ->
    Kernel.fatal "bar extension should have ids as arguments"

let visit_bar vis ext =
  match ext with
  | Ext_id idx ->
    let l = Bar_table.find idx in
    let l' = Cil.mapNoCopy (Cil.visitCilPredicate vis) l in
    if Cil.is_copy_behavior vis#behavior then begin
      let idx' = Count.next () in
      Queue.add (fun () -> Bar_table.add idx' l') vis#get_filling_actions;
      Cil.ChangeTo(Ext_id idx')
    end else begin
      Bar_table.replace idx l';
      Cil.SkipChildren
    end
  | Ext_terms _ | Ext_preds _ ->
      Kernel.fatal "bar extension should have ids as arguments"

let type_baz ~typing_context ~loc l =
  if not (typing_context.is_loop ()) then
    typing_context.error loc "baz is a loop extension only"
  else
    let t =
      List.map
        (typing_context.type_term typing_context typing_context.pre_state) l
    in
    Ext_terms t

module Count_bla = State_builder.Counter(struct let name = "Count_bla" end)

module Bla_table =
  State_builder.Hashtbl(Datatype.Int.Hashtbl)(Cil_datatype.Predicate)
    (struct
      let name = "Bla_table"
      let dependencies = [ Ast.self; Count_bla.self ]
      let size = 3
    end)

let add_builtin () =
  let trace =
    { bl_name = "\\trace";
      bl_labels = []; bl_params = []; bl_type = None;
      bl_profile = [ "x", Linteger ] }
  in
  Logic_builtin.add trace

let () = add_builtin ()

let type_bla ~typing_context ~loc:_loc l =
  let type_predicate ctxt env p =
    match p.lexpr_node with
    | PLapp("\\trace", [], [pred]) ->
      let pred = typing_context.type_predicate typing_context env pred in
      let li = List.hd (ctxt.find_all_logic_functions "\\trace") in
      let i = Count.next () in
      let ti = Logic_const.tinteger ~loc:pred.pred_loc i in
      Bla_table.add i pred;
      Logic_const.papp ~loc:p.lexpr_loc (li,[],[ti])
    | _ -> typing_context.type_predicate ctxt env p
  in
  let ctxt = { typing_context with type_predicate } in
  let l =
    List.map (type_predicate ctxt ctxt.pre_state) l
  in
  Ext_preds l

let () =
  Logic_typing.register_behavior_extension "foo" type_foo;
  Logic_typing.register_behavior_extension "bar" type_bar;
  Logic_typing.register_behavior_extension "bla" type_bla;
  Cil_printer.register_behavior_extension "bar" print_bar;
  Cil.register_behavior_extension "bar" visit_bar;
  Logic_typing.register_behavior_extension "baz" type_baz

let run () =
  Ast.compute ();
  let debug = Kernel.debug_atleast 1 in
  let my_file = Extlib.temp_file_cleanup_at_exit ~debug "Extend" ".i" in
  let out = open_out my_file in
  let fmt = Format.formatter_of_out_channel out in
  File.pretty_ast ~fmt ();
  let prj = Project.create "reparsing" in
  Project.on prj add_builtin ();
  Project.on prj Kernel.Files.add my_file;
  Kernel.feedback "Reparsing file";
  (* Avoid having a temporary name in the oracle. *)
  Kernel.Verbose.set 0;
  Project.on prj Ast.compute ();
  File.pretty_ast ~prj ()

let () = Db.Main.extend run

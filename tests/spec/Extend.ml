open Extlib
open Cil_types
open Logic_typing

let type_foo ~typing_context ~loc bhv l =
  let _loc = loc in
  let preds =
    List.map 
      (Logic_const.new_predicate $ 
         (typing_context.type_predicate typing_context.pre_state))
      l
  in
  bhv.b_extended <- ("foo", 0, preds) :: bhv.b_extended

module Count = State_builder.Counter(struct let name = "Count" end)

module Bar_table =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Datatype.List(Cil_datatype.Predicate_named))
    (struct
        let name = "Bar_table"
        let dependencies = [ Ast.self; Count.self ]
        let size = 3
     end)

let type_bar ~typing_context ~loc bhv l =
  let _loc = loc in
  let i = Count.next() in
  let p =
    List.map
      (typing_context.type_predicate (typing_context.post_state [Normal])) l
  in
  Bar_table.add i p;
  bhv.b_extended <- ("bar", i, []) :: bhv.b_extended

let print_bar prt fmt (idx, _) =
  let l = Bar_table.find idx in
  Pretty_utils.pp_list
    ~pre:"@[<hov 2>" ~sep:",@ " ~suf:"@]" prt#predicate_named fmt l

let visit_bar vis (idx, _) =
  let l = Bar_table.find idx in
  let l' = Cil.mapNoCopy (Cil.visitCilPredicateNamed vis) l in
  if Cil.is_copy_behavior vis#behavior then begin
    let idx' = Count.next () in
    Queue.add (fun () -> Bar_table.add idx' l') vis#get_filling_actions;
    Cil.ChangeTo(idx',[])
  end else begin
    Bar_table.replace idx l';
    Cil.SkipChildren
  end

let () =
  Logic_typing.register_behavior_extension "foo" type_foo;
  Logic_typing.register_behavior_extension "bar" type_bar;
  Cil_printer.register_behavior_extension "bar" print_bar;
  Cil.register_behavior_extension "bar" visit_bar

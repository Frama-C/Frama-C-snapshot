open Cil_types

module Map = Cil_datatype.Varinfo.Map

module ConstantsDomain =
struct
  type t = int Map.t

  let top = Map.empty

  let pretty fmt v =
    let pp_entry vi x =
      Format.fprintf fmt "%s -> %d@." vi.vorig_name x
    in
    Map.iter pp_entry v

  let join v1 v2 =
    let merge_entry _vi o1 o2 =
      match o1, o2 with
      | None, _ | _, None -> None
      | Some x, Some x' -> if x = x' then Some x else None
    in
    Map.merge merge_entry v1 v2

  let widen v1 v2 =
    let same_entry vi x =
      Map.find_opt vi v2 = Some x
    in
    if Map.for_all same_entry v1 then
       None (* Inclusion *)
    else
      Some v2 (* No widening necessary *)

  exception Not_constant

  let rec eval v exp =
    match exp.enode with
    | Const (CInt64 (i,_,_)) ->
      (try Integer.to_int i with _ -> raise Not_constant)
    | Lval (Var vi, NoOffset) ->
      (try Map.find vi v with Not_found -> raise Not_constant)
    | SizeOf typ ->
      Cil.bytesSizeOf typ
    | UnOp (Neg, e, _) ->
      -(eval v e)
    | BinOp (PlusA, e1, e2, _) ->
      (eval v e1) + (eval v e2)
    | BinOp (MinusA, e1, e2, _) ->
      (eval v e1) - (eval v e2)
    | BinOp (Mult, e1, e2, _) ->
      (eval v e1) * (eval v e2)
    | BinOp (Div, e1, e2, _) ->
      let x = eval v e2 in
      if x <> 0 then (eval v e1) / x else raise Not_constant
    | _ ->
      raise Not_constant

  let eval_opt v exp =
    try Some (eval v exp) with Not_constant -> None

  let assume v exp kind =
    match exp.enode, kind with
    | BinOp (Eq, e1, e2, _), Interpreted_automata.Then
    | BinOp (Ne, e1, e2, _), Interpreted_automata.Else ->
      begin match eval_opt v e1, eval_opt v e2 with
        | None, None -> Some v
        | Some x, None ->
          begin match e2.enode with
            | Lval (Var vi, NoOffset) -> Some (Map.add vi x v)
            | _ -> Some v
          end
        | None, Some x ->
          begin match e1.enode with
            | Lval (Var vi, NoOffset) -> Some (Map.add vi x v)
            | _ -> Some v
          end
        | Some x, Some y ->
          if x = y then Some v else None
      end
    | _ -> Some v

  let assign v vi exp =
    try
      Map.add vi (eval v exp) v
    with Not_constant ->
      Map.remove vi v

  let transfer t v =
    let open Interpreted_automata in
    match t with
    | Skip | Return _ | Prop _ | Enter _ | Leave _ -> Some v
    | Guard (exp, kind, _) -> assume v exp kind
    | Instr (Set ((Var vi, NoOffset), exp, _), _) -> Some (assign v vi exp)
    | Instr (Local_init (vi, AssignInit (SingleInit exp), _), _) ->
      Some (assign v vi exp)
    | Instr (Local_init (_vi, AssignInit (CompoundInit _), _), _) -> Some v
    | Instr ((Call _ | Local_init _ | Set _ | Asm _), _) -> Some top
    | Instr ((Cil_types.Skip _ | Code_annot _), _) -> Some v
end


module Dataflow = Interpreted_automata.Dataflow (ConstantsDomain)

let run () =
  let main_kf, _ = Globals.entry_point () in
  let results = Dataflow.fixpoint main_kf ConstantsDomain.top in
  let result = Interpreted_automata.(
      Vertex.Hashtbl.find results (get_automaton main_kf).return_point)
  in
  Kernel.result "Results at the end of function %s:@.%a"
    (Kernel_function.get_name main_kf)
    ConstantsDomain.pretty result

let () =
  Db.Main.extend run

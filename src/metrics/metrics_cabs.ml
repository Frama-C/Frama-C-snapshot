(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Implementation of cyclomatic complexity measures on CAbs' AST *)
open Cabs
open Metrics_base
open Metrics_base.BasicMetrics

class metricsCabsVisitor = object(self)
  inherit Cabsvisit.nopCabsVisitor

  (* Global metrics store for this Cabs AST *)
  val global_metrics = ref empty_metrics
  (* Local metrics in computation *)
  val local_metrics = ref empty_metrics

  (* Was last statement a case ? *)
  val was_case = ref false

  (* Local metrics are kept stored after computation in this map of maps.
     Its storing hierachy is as follows: filename -> function_name -> metrics *)
  val mutable metrics_map:
      (BasicMetrics.t Datatype.String.Map.t) Datatype.String.Map.t =
    Datatype.String.Map.empty

  val functions_no_source: (string, int) Hashtbl.t = Hashtbl.create 97
  val functions_with_source: (string, int) Hashtbl.t = Hashtbl.create 97
  val mutable standalone = true

  (* Getters/setters *)
  method functions_no_source = functions_no_source
  method functions_with_source = functions_with_source
  method set_standalone v = standalone <- v
  method get_metrics = !global_metrics
  method private update_metrics_map filename strmap =
    metrics_map <- Datatype.String.Map.add filename strmap metrics_map

  (* Utility methods to increase metrics counts *)
  method private incr_both_metrics f =
    apply_then_set f global_metrics;
    apply_then_set f local_metrics

  method add_to_functions_with_source (funcname:string) =
    Hashtbl.add functions_with_source funcname 0;
    Hashtbl.remove functions_no_source funcname;

  method private record_and_clear metrics =
    let filename = metrics.cfile_name
    and funcname = metrics.cfunc_name in
    (try
       let fun_tbl = Datatype.String.Map.find filename metrics_map in
       self#update_metrics_map filename
         (Datatype.String.Map.add funcname !local_metrics fun_tbl);
     with
       | Not_found ->
         let new_stringmap =
           Datatype.String.Map.add funcname !local_metrics Datatype.String.Map.empty in
         self#update_metrics_map filename new_stringmap;
    );
    local_metrics := empty_metrics;

  method! vdef def =
    match def with
      | FUNDEF (_, sname, _, _, _) ->
        begin
          let funcname = Metrics_base.extract_fundef_name sname in
          local_metrics :=
            {!local_metrics with
              cfile_name = get_filename def;
              cfunc_name = funcname;
              cfuncs = 1; (* Only one function is indeed being defined here *)};
          Metrics_parameters.debug
            ~level:1 "Definition of function %s encountered@." funcname;
          apply_then_set incr_funcs global_metrics;
          self#add_to_functions_with_source funcname;
          (* On return record the analysis of the function. *)
          Cil.ChangeDoChildrenPost
            ([def],
             fun _ ->
               begin
                 if !local_metrics <> empty_metrics
                 then self#record_and_clear !local_metrics;
                 [def]
               end
            );
        end
      | DECDEF _
      | TYPEDEF _
      | ONLYTYPEDEF _
      | GLOBASM _
      | PRAGMA _
      | LINKAGE _
      | CUSTOM _
      | GLOBANNOT _ -> Cil.DoChildren;

  method! vexpr expr =
    (match expr.expr_node with
      | NOTHING -> ()
      | UNARY (unop, _) ->
        begin
          match unop with
            | PREINCR
            | POSINCR
            | PREDECR
            | POSDECR -> self#incr_both_metrics incr_assigns
            | MINUS
            | PLUS
            | NOT
            | BNOT -> ()
            | MEMOF -> self#incr_both_metrics incr_ptrs
            | ADDROF -> ()
        end
      | LABELADDR _ -> ()
      | BINARY (bop, _, _) ->
        begin
          match bop with
            | ADD | SUB | MUL | DIV | MOD
            | BAND | BOR | XOR
            | SHL | SHR | EQ | NE | LT
            | GT | LE | GE -> ()
            | AND | OR -> self#incr_both_metrics incr_dpoints
            | ASSIGN
            | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN
            | DIV_ASSIGN | BOR_ASSIGN | XOR_ASSIGN
            | SHL_ASSIGN | SHR_ASSIGN | BAND_ASSIGN
            | MOD_ASSIGN ->
              self#incr_both_metrics incr_assigns;
        end
      | CAST _ -> ()
      | CALL  _ -> self#incr_both_metrics incr_calls;
      | QUESTION _ ->
        self#incr_both_metrics incr_dpoints;
        self#incr_both_metrics incr_ifs;
      | COMMA _
      | CONSTANT _
      | PAREN _
      | VARIABLE _
      | EXPR_SIZEOF _
      | TYPE_SIZEOF _
      | EXPR_ALIGNOF _
      | TYPE_ALIGNOF _
      | INDEX _
      | MEMBEROF _
      | MEMBEROFPTR _
      | GNU_BODY _
      | EXPR_PATTERN _ -> ());
    Cil.DoChildren

  (* Allows to count only one control-flow branch per case lists *)
  method private set_case stmt =
    match stmt.stmt_node with
      | CASERANGE _ | CASE _ -> was_case := true;
      | DEFAULT _
      | _ -> was_case := false

  method! vstmt stmt =
    self#incr_both_metrics incr_slocs;
    (match stmt.stmt_node with
      | DEFAULT _ -> () (* The default case is not counted as a path choice
                           point *)
      | CASERANGE _
      | CASE _ ->
        if not !was_case then self#incr_both_metrics incr_dpoints;
      | IF _ ->
        self#incr_both_metrics incr_ifs;
        self#incr_both_metrics incr_dpoints;
      | NOP _
      | COMPUTATION _
      | BLOCK _ -> ()
      (* Next 3 are all loop instructions *)
      | WHILE _
      | DOWHILE _
      | FOR _ ->
        self#incr_both_metrics incr_loops;
        self#incr_both_metrics incr_dpoints;
      | BREAK _
      | CONTINUE _ -> ()
      | RETURN _ -> self#incr_both_metrics incr_exits;
      | SWITCH _ -> ()
      | LABEL _ -> ()
      | GOTO _
      | COMPGOTO _ -> self#incr_both_metrics incr_gotos;
      | DEFINITION _
      | ASM _
      | SEQUENCE _
      | TRY_EXCEPT _
      | TRY_FINALLY _
      | CODE_ANNOT _
      | CODE_SPEC _ -> ());
    self#set_case stmt;
    Cil.DoChildren

  method private stats_of_filename filename =
    try Datatype.String.Map.find filename metrics_map
    with
      | Not_found ->
        Metrics_parameters.fatal "Metrics for file %s not_found@." filename

  method pp_file_metrics fmt filename =
    Format.fprintf fmt "@[<v 0>%a@]"
      (fun fmt filename ->
        let fun_tbl = self#stats_of_filename filename in
        Datatype.String.Map.iter (fun _fun_name fmetrics ->
          Format.fprintf fmt "@ %a" pp_base_metrics fmetrics)
          fun_tbl;
      ) filename

  method pp_detailed_text_metrics fmt () =
    Datatype.String.Map.iter
      (fun filename _func_tbl ->
        Format.fprintf fmt "%a" self#pp_file_metrics filename) metrics_map

end
;;


(** Halstead metrics computation *)
module Halstead = struct
(* We follow http://www.verifysoft.com/en_halstead_metrics.html
   for the classification of operands and operators
   operands = ids, typenames, typespecs, constants
*)

let update_val value key tbl =
  try
    let v = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (v + value);
  with
    | Not_found -> Hashtbl.add tbl key value
;;

let update_val_incr key tbl = update_val 1 key tbl;;

type operand_tbl = {
  var_tbl : (string, int) Hashtbl.t;
  cst_tbl : (Cabs.constant, int) Hashtbl.t;
}
;;

type operator_tbl = {
  knownop_tbl : (string, int) Hashtbl.t;
  otherop_tbl  : (string, int) Hashtbl.t;
  reserved_tbl : (string, int) Hashtbl.t;
  tspec_tbl : (Cabs.typeSpecifier, int) Hashtbl.t;
}
;;

let id_from_init iname =
  match (fst iname) with
    | s, _, _, _ -> s
;;

class halsteadCabsVisitor = object(self)

  inherit Cabsvisit.nopCabsVisitor

  val operand_tbl = {
    var_tbl = Hashtbl.create 7;
    cst_tbl = Hashtbl.create 7;
  }

  val operator_tbl = {
    knownop_tbl = Hashtbl.create 7;
    otherop_tbl = Hashtbl.create 7;
    reserved_tbl = Hashtbl.create 7;
    tspec_tbl = Hashtbl.create 7;
  }

  method get_operator_tbl () = operator_tbl
  method get_operand_tbl () = operand_tbl

  method add_paren () =
    update_val_incr "(" operator_tbl.otherop_tbl;
    update_val_incr ")" operator_tbl.otherop_tbl;

  method! vexpr e =
    match e.Cabs.expr_node with
      | UNARY _ ->
        let unop = fst (Cprint.get_operator e) in
        update_val_incr unop operator_tbl.knownop_tbl;
        Cil.DoChildren;
      | BINARY _ ->
        let binop = fst (Cprint.get_operator e) in
        update_val_incr binop operator_tbl.knownop_tbl;
        Cil.DoChildren;
      | QUESTION _ ->
        update_val_incr "?" operator_tbl.otherop_tbl;
        update_val_incr ":" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | COMMA elist ->
        let n = List.length elist in
        if (n > 1) then
        update_val (n - 1) "," operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | CONSTANT c ->
        update_val_incr c operand_tbl.cst_tbl;
        Cil.DoChildren;
      | PAREN _ ->
        self#add_paren ();
        Cil.DoChildren;
      | VARIABLE s ->
        update_val_incr s operand_tbl.var_tbl;
        Cil.DoChildren;
      | EXPR_SIZEOF _ ->
        update_val_incr "sizeof" operator_tbl.reserved_tbl;
        Cil.DoChildren;
      | TYPE_SIZEOF _ ->
        update_val_incr "sizeof" operator_tbl.reserved_tbl;
        Cil.DoChildren;
      | INDEX _ ->
        update_val_incr "[]" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | _ -> Cil.DoChildren;


  method! vstmt s =
    let reserved rstr =
      update_val_incr rstr operator_tbl.reserved_tbl;
      Cil.DoChildren;
    in
    match s.Cabs.stmt_node with
      | BLOCK _ ->
        update_val_incr "{" operator_tbl.otherop_tbl;
        update_val_incr "}" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | SEQUENCE _ ->
        print_string "seq\n";
        update_val_incr ";" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | IF _ -> self#add_paren (); reserved "if";
      | WHILE _ -> self#add_paren (); reserved "while";
      | DOWHILE _ ->
        update_val_incr "do" operator_tbl.reserved_tbl;
        self#add_paren ();
        reserved "while";
      | FOR _ ->
        self#add_paren ();
        update_val 2 ";" operator_tbl.otherop_tbl;
        reserved "for";
      | BREAK _ -> reserved "break";
      | CONTINUE _ -> reserved "continue";
      | RETURN _ -> reserved "return";
      | SWITCH _ -> self#add_paren (); reserved "switch";
      | CASE _ -> reserved "case";
      | CASERANGE _ ->
        update_val_incr "..." operator_tbl.otherop_tbl;
        update_val 2 ";" operator_tbl.otherop_tbl;
        reserved "case";
      | DEFAULT _ -> reserved "default";
      | LABEL _ ->
        update_val_incr ":" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | GOTO (s, _) ->
        let lname = Format.sprintf "label_%s" s in
        update_val_incr lname operand_tbl.var_tbl;
        reserved "goto";

      | COMPGOTO _ ->
        update_val_incr "*" operator_tbl.otherop_tbl;
        reserved "goto";

      | DEFINITION _ -> Cil.DoChildren;
      | ASM _ -> reserved "asm";
      | TRY_EXCEPT _ ->
        update_val_incr "except" operator_tbl.reserved_tbl;
        reserved "try";
      | TRY_FINALLY _ ->
        update_val_incr "finally" operator_tbl.reserved_tbl;
        reserved "try";
      | _ -> Cil.DoChildren;

  method! vtypespec tspec =
    update_val_incr tspec operator_tbl.tspec_tbl;
    Cil.DoChildren;

  method! vspec spec =
    let reserved rstr =
      update_val_incr rstr operator_tbl.reserved_tbl;
    in
    let do_spec s =
      match s with
        | SpecTypedef -> reserved "typedef"
        | SpecInline -> reserved "inline"
        | SpecStorage AUTO -> reserved "auto"
        | SpecStorage STATIC -> reserved "static"
        | SpecStorage EXTERN -> reserved "extern"
        | SpecStorage REGISTER -> reserved "register"
        | SpecCV CV_CONST -> reserved "const"
        | SpecCV CV_VOLATILE -> reserved "volatile"
        | SpecCV CV_RESTRICT -> reserved "restrict"
        | _ -> ()
    in List.iter do_spec spec; Cil.DoChildren;

  method! vdecltype tdecl =
    match tdecl with
      | JUSTBASE ->
        Cil.SkipChildren;
      | PARENTYPE _  ->
        self#add_paren ();
        Cil.DoChildren;
      | ARRAY _ ->
        update_val_incr "array" operator_tbl.reserved_tbl;
        Cil.DoChildren;
      | PTR _ ->
        update_val_incr "*" operator_tbl.otherop_tbl;
        Cil.DoChildren;
      | PROTO _ ->
        Cil.SkipChildren;


  method! vinitexpr ie =
    ( match ie with
      | COMPOUND_INIT l ->
        let n = List.length l in
        if n > 0 then
        update_val n "," operator_tbl.otherop_tbl;
      | _ -> ());
    Cil.DoChildren

  method! vblock b =
    if b.bstmts <> [] then (
      let n = List.length b.bstmts in
      update_val n ";" operator_tbl.otherop_tbl);
    if b.battrs <> [] then
      update_val (List.length b.battrs) "," operator_tbl.otherop_tbl;
    Cil.DoChildren;

  method! vdef d =
    match d with
      | FUNDEF (bl, (_, (fname, dtype, _, nloc)), b, loc1, loc2) ->
        Cil.ChangeDoChildrenPost(
          [FUNDEF(bl, ([], (fname, dtype, [], nloc)), b, loc1, loc2)],
          fun x -> x)

      | DECDEF (_, (_, name_list), _) ->
        let n =
          List.fold_left
            (fun acc n ->
              update_val_incr (id_from_init n) operand_tbl.var_tbl;
              acc + 1 )
          (-1) name_list in
        begin
          assert(n >= 0);
          if (n > 0) then update_val n "," operator_tbl.otherop_tbl;
          Cil.DoChildren;
        end

      | _ -> Cil.DoChildren

end
;;



let compose _x1 y1 (x2, y2) = (1 + x2), (y1 + y2);;
let fold x y = Hashtbl.fold compose x y;;

let compute_operators operator_tbl =
  let x, y =
    fold operator_tbl.tspec_tbl (
      fold operator_tbl.otherop_tbl (
        fold operator_tbl.reserved_tbl (
          fold operator_tbl.knownop_tbl (0,0))))
  in (float_of_int x), (float_of_int y)
;;

let compute_operands operand_tbl =
  let x, y =
    fold operand_tbl.cst_tbl (
      fold operand_tbl.var_tbl (0,0))
  in (float_of_int x), (float_of_int y)
;;

type halstead_metrics = {
  distinct_operators : float;
  total_operators : float;
  distinct_operands : float;
  total_operands : float;
  program_length : float;
  program_volume : float;
  program_level : float;
  vocabulary_size : float;
  difficulty_level : float;
  effort_to_implement : float;
  time_to_implement : float;
  bugs_delivered : float;
}

let get_metrics cabs_visitor =
  let operator_tbl = cabs_visitor#get_operator_tbl () in
  let operand_tbl = cabs_visitor#get_operand_tbl () in
  let distinct_operators, total_operators = compute_operators operator_tbl
  and distinct_operands, total_operands = compute_operands operand_tbl in
  let program_length = total_operands +. total_operators in
  let vocabulary_size = distinct_operands +. distinct_operators in
  let log2 x = (Pervasives.log x) /. (Pervasives.log 2.0) in
  let program_volume = program_length *. (log2 vocabulary_size) in
  let difficulty_level =
    (distinct_operators /. 2.) *. (total_operands /. distinct_operands) in
  let program_level = 1. /. difficulty_level in
  let effort_to_implement = program_volume *. difficulty_level in
  let time_to_implement = effort_to_implement /. 18. in
  let bugs_delivered = (effort_to_implement ** (2./.3.)) /. 3000. in
  { distinct_operators = distinct_operators;
    total_operators = total_operators;
    distinct_operands = distinct_operands;
    total_operands = total_operands;
    program_length = program_length;
    program_volume = program_volume;
    program_level = program_level;
    vocabulary_size = vocabulary_size;
    difficulty_level = difficulty_level;
    effort_to_implement = effort_to_implement;
    time_to_implement = time_to_implement;
    bugs_delivered = bugs_delivered;
  }
;;

let to_list hmetrics =
 [ [ "Total operators"; float_to_string hmetrics.total_operators; ];
   [ "Distinct operators"; float_to_string hmetrics.distinct_operators; ];
   [ "Total_operands"; float_to_string hmetrics.total_operands; ];
   [ "Distinct operands"; float_to_string hmetrics.distinct_operands; ];
   [ "Program length"; float_to_string hmetrics.program_length; ];
   [ "Vocabulary size"; float_to_string hmetrics.vocabulary_size; ];
   [ "Program volume"; float_to_string hmetrics.program_volume; ];
   [ "Effort"; float_to_string hmetrics.effort_to_implement; ];
   [ "Program level"; float_to_string hmetrics.program_level; ];
   [ "Difficulty level"; float_to_string hmetrics.difficulty_level; ];
   [ "Time to implement"; float_to_string hmetrics.time_to_implement; ];
   [ "Bugs delivered"; float_to_string hmetrics.bugs_delivered; ];
 ]
;;

let pp_metrics ppf cabs_visitor =
  let metrics = get_metrics cabs_visitor in
  (* Compute the metrics from the informations gathered by the visitor. *)
  let minutes = (int_of_float metrics.time_to_implement) / 60 in
  let _hours, _minutes = minutes / 60, minutes mod 60 in

  let operator_tbl = cabs_visitor#get_operator_tbl () in
  let operand_tbl = cabs_visitor#get_operand_tbl () in

  let dummy_cst cst =
    { expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos);
      expr_node = CONSTANT cst;
    }
  and simple_pp_htbl ppf htbl =
    Hashtbl.iter (fun k v -> Format.fprintf ppf "%s: %d@ " k v) htbl in
  (* Halstead metrics' bugs delivered statistics is said to be underapproximated
     for C. Hence the "lower bound" commentary on the output next to "bugs
     delivered".
  *)
  let title = "Halstead metrics"
  and stats = "Global statistics (Halstead)"
  and operator_sec = "Operators"
  and operand_sec = "Operands" in
  Format.fprintf ppf
    "@[<v 0>%a@ %a@ @ \
       %a@ \
       @[<v 2>%a@ %a%a%a%a@]@ \
       @[<v 2>%a@ %a%a@]@ \
     @]"
    (mk_hdr 1) title
    (fun ppf l ->
      List.iter (fun rowl ->
        Format.fprintf ppf "@[<hov>";
        (match rowl with
          | title :: contents ->
            Format.fprintf ppf "%s:@ " title;
            List.iter (fun s -> Format.fprintf ppf "%s@ " s) contents;
          | [] -> ());
        Format.fprintf ppf "@]@ ";
      ) l) (to_list metrics)
    (mk_hdr 1) stats

    (mk_hdr 2) operator_sec
    (* Operators table *)
    simple_pp_htbl operator_tbl.reserved_tbl
    simple_pp_htbl operator_tbl.otherop_tbl
    simple_pp_htbl operator_tbl.knownop_tbl
    (fun ppf htbl ->
      Hashtbl.iter
        (fun k v ->
          Format.fprintf ppf "%a: %d@ " Cprint.print_type_spec k v) htbl)
    operator_tbl.tspec_tbl
    (* Operands *)
    (mk_hdr 2) operand_sec
    simple_pp_htbl operand_tbl.var_tbl
    (fun ppf htbl ->
      Hashtbl.iter
        (fun k v ->
          Format.fprintf ppf "%a: %d@ " Cprint.print_expression (dummy_cst k) v)
        htbl)
    operand_tbl.cst_tbl;
;;

let compute_metrics () =
  (* Run the visitor on all files *)
  let cabs_files = Ast.UntypedFiles.get () in
  let cabs_visitor = new halsteadCabsVisitor in
  List.iter (fun file ->
    ignore (Cabsvisit.visitCabsFile (cabs_visitor:>Cabsvisit.cabsVisitor) file))
    cabs_files ;
  Metrics_parameters.result "%a" pp_metrics cabs_visitor

let get_metrics () =
  let cabs_files = Ast.UntypedFiles.get () in
  let cabs_visitor = new halsteadCabsVisitor in
  List.iter (fun file ->
    ignore (Cabsvisit.visitCabsFile (cabs_visitor:>Cabsvisit.cabsVisitor) file))
    cabs_files ;
  get_metrics cabs_visitor
;;

end

let compute_on_cabs () =
  try
    let cabs_files = Ast.UntypedFiles.get () in
    let cabs_visitor = new metricsCabsVisitor in
    List.iter (fun file ->
      Metrics_parameters.debug
	~level:2 "Compute Cabs metrics for file %s@." (fst file);
      ignore
	(Cabsvisit.visitCabsFile (cabs_visitor:>Cabsvisit.cabsVisitor) file);
    )
      cabs_files
    ;
    if Metrics_parameters.ByFunction.get () then
    Metrics_parameters.result
      "@[<v 0>Cabs:@ %a@]" cabs_visitor#pp_detailed_text_metrics ();
    Halstead.compute_metrics ();
  with
    | Ast.NoUntypedAst ->
      Metrics_parameters.warning
        "@[<v 0> Project has no untyped AST. Only metrics over normalized CIL \
                 AST are available. \
         @]@."

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

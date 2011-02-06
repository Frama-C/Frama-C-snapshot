(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Format
open Cil
open Cil_types
open Cil_datatype
open Db.Metrics

let name = "metrics"

type int8 = int*int*int*int*int*int*int*int

module LastResult =
  State_builder.Option_ref
    (Datatype.Make
       (struct
	 include Datatype.Serializable_undefined
	 type t = Db.Metrics.t
	 let name = name
	 let structural_descr = Structural_descr.Abstract
	 let reprs =
	   [ { sloc = -1;
	       call_statements = -1;
	       goto_statements = -1;
	       assign_statements = -1;
	       if_statements = -1;
	       loop_statements = -1;
	       mem_access = -1;
	       functions_without_source = Varinfo.Hashtbl.create 7;
	       functions_with_source = Varinfo.Hashtbl.create 7;
	       (* ABP added 2 fields below*)
	       function_definitions = -1;
	       cyclos = -1 } ]
	 let mem_project = Datatype.never_any_project
	end))
    (struct
       let dependencies = [ Ast.self ]
       let name = name
       let kind = `Internal
     end)

let pretty_set fmt s =
  Format.fprintf fmt "@[";
  Varinfo.Hashtbl.iter
    (fun f n ->
       Format.fprintf fmt "%s %s (%d call%s);@ "
	 f.vname
         (if f.vaddrof then "(address taken)" else "")
	 n (if n > 1 then "s" else ""))
    s;
  Format.fprintf fmt "@]"

let number_entry_points fs =
  Varinfo.Hashtbl.fold
    (fun f n acc -> if n = 0 && not f.vaddrof then succ acc else acc)
    fs
    0

let pretty_entry_points fmt fs =
  let print =
    Varinfo.Hashtbl.iter
      (fun f n ->
	 if n = 0 && not f.vaddrof then Format.fprintf fmt "%s;@ " f.vname)
  in
  Format.fprintf fmt "@[";
  print fs;
  Format.fprintf fmt "@]"

let pretty fmt =
  let m = LastResult.get () in
  Format.fprintf fmt
    "@[Defined function (%d):@\n  @[%a@]@\nUndefined functions (%d):@\n  @[%a@]@\nPotential entry points (%d):@\n  @[%a@]@\nSLOC: %d@\nNumber of if statements: %d@\nNumber of assignments: %d@\nNumber of loops: %d@\nNumber of calls: %d@\nNumber of gotos: %d@\nNumber of pointer access: %d@]"
    (Varinfo.Hashtbl.length m.functions_with_source)
    pretty_set m.functions_with_source
    (Varinfo.Hashtbl.length m.functions_without_source)
    pretty_set m.functions_without_source
    (number_entry_points m.functions_with_source)
    pretty_entry_points m.functions_with_source
    m.sloc
    m.if_statements
    m.assign_statements
    m.loop_statements
    m.call_statements
    m.goto_statements
    m.mem_access

let dump () =
  let filename = Metrics_parameters.Dump.get () in
  try
    let cout = open_out_bin filename in
    let fmt = Format.formatter_of_out_channel cout in
      pretty fmt;
      close_out cout
  with Sys_error _ as e ->
    Metrics_parameters.warning "Cannot open file \"%s\" for dumping metrics: %s"
      filename (Printexc.to_string e)

let null_position : Lexing.position =
  { Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let null_location : Cil_types.location = (null_position, null_position)

let fun_equal (a:global) (b:global) =
  match a with
      GFun (af,_) ->
	begin
	  match b with
	      GFun (bf,_) -> (af == bf)
	    | _ -> false
	end
    | _ -> false

let file_of glob =
  (* returns the file name to which belongs glob *)
  let res = ref "" in
  let all_files = Globals.FileIndex.get_files () in
    begin
      for j = 0 to (List.length all_files)-1 do
	let f = (List.nth all_files j) in
        let (_,globs) = Globals.FileIndex.find f in
	  for i = 0 to (List.length globs)-1 do
	    let elt = ((List.nth globs i) :> global) in
	      if (fun_equal elt glob) then
		res:=f
	    done
	done;
	!res
    end

let image_int8 (a:int8) =
  let (a1,a2,a3,a4,a5,a6,a7,a8) = a in
    "(" ^ (string_of_int a1) ^ "," ^
      (string_of_int a2) ^ "," ^
      (string_of_int a3) ^ "," ^
      (string_of_int a4) ^ "," ^
      (string_of_int a5) ^ "," ^
      (string_of_int a6) ^ "," ^
      (string_of_int a7) ^ "," ^
      (string_of_int a8) ^ ")"

let plus (a:int8) (b:int8) =
  let (a1,a2,a3,a4,a5,a6,a7,a8) = a in
  let (b1,b2,b3,b4,b5,b6,b7,b8) = b in
    (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7,a8+b8)

class slocVisitor = object(self)
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ())
  val mutable current_file_name:string = ""
  val mutable current_function_name:string = ""
  val mutable sloc = 0
  val mutable ifs = 0
  val mutable loops = 0
  val mutable calls = 0
  val mutable gotos = 0
  val mutable assigns = 0
  val mutable exits = 0
  val mutable funcs = 0
    (* table of all statistics per module and per function:
       stats(file,f) = (#ifs, #assign, #loop, #calls, #gotos, #pointers, #exits, cyclo)
    *)
  val mutable stats:
    (string*string*(int*int*int*int*int*int*int*int)) list ref = ref []

  method stats_of_fic f =
    (* get only the stats that are relative to file fic. remove them
       from stats and return them *)
    let prop element =
      let (fic,_,_) = element in
      (f=fic)
    in
    let (good, bad) = (List.partition prop !stats) in
    stats <- ref bad;
    good

  method complete_stats () =
    (* When using this visitor no cyclomatic complexity is calculated
       during the traversal of the AST. It has to be calculated AFTER
       cisiting the entire global function. We revisit on site every tuple
       of stats and calculate the last item. *)
    let tout = !stats in
    if tout <> [] then
      let res = ref [] in
      let do_it e =
	let (fic,func,(a,b,c,d,e,f,g,h)) = e in
	begin
	  if h <> 0 then
	    prerr_endline "metrics.complete_stats ERROR";
	  res := List.append !res [(fic,func,(a,b,c,d,e,f,g,a+c-g+2))]
	end
      in List.iter do_it tout;
      stats := !res

  method print_stats fmt =
    let print_item e =
      let _, func, (a,b,c,d,e,f,g,h) = e in
      Metrics_parameters.debug
	"stats: func: %s@\n\
val: ifs %d@\n\
     assigns %d@\n\
     loops %d@\n\
     calls %d@\n\
     gotos %d@\n\
     mems %d@\n\
     exits %d@\n\
     cyclo %d"
	func a b c d e f g h;
      fprintf fmt "<tr>\n";
      fprintf fmt "<td> %s </td>\n" func;
      fprintf fmt "<td> %d </td>\n" a;
      fprintf fmt "<td> %d </td>\n" b;
      fprintf fmt "<td> %d </td>\n" c;
      fprintf fmt "<td> %d </td>\n" d;
      fprintf fmt "<td> %d </td>\n" e;
      fprintf fmt "<td> %d </td>\n" f;
      fprintf fmt "<td> %d </td>\n" g;
      fprintf fmt "<td> %d </td>\n" h;
      fprintf fmt "</tr>\n";
    in
    while List.length !stats > 0 do
      let first = List.hd !stats in
      let (fic,_,_) = first in
      let fic_stats = (self#stats_of_fic fic) in
      (* print header specific to fic *)
      fprintf fmt "<h3> %s </h3>\n" fic;
      fprintf fmt "	<br>\n";
      fprintf fmt "<table style=\"width: 252px; height: 81px;\" border=\"1\">\n";
      fprintf fmt "  <tbody>\n";
      fprintf fmt "    <tr>\n";
      fprintf fmt "      <th>Function</th>\n";
      fprintf fmt "      <th>#If stmts<br>\n";
      fprintf fmt "      <th>#Assignments<br>\n";
      fprintf fmt "      <th>#Loops<br>\n";
      fprintf fmt "      <th>#Calls<br>\n";
      fprintf fmt "      <th>#Gotos<br>\n";
      fprintf fmt "      <th>#Pointer accesses<br>\n";
      fprintf fmt "      <th>#Exits<br>\n";
      fprintf fmt "      <th>Cyclomatic value<br>\n";
      fprintf fmt "      </th>\n";
      fprintf fmt "    </tr>\n";
      List.iter print_item fic_stats;
      (* print trailer specific to fic *)
      fprintf fmt "  </tbody>\n";
      fprintf fmt "</table>\n"
    done

  method add_item (a:string) (b:string) (c:int8) l =
    (* add a new item to the list stats *)
    if (List.length l) = 0 then
      [(a,b,c)]
    else (* there's at least 1 element *)
      let premier = (List.hd l) in
      let (x,y,z) = premier in
      let reste = (List.tl l) in
      if (x,y)=(a,b) then
	List.append [(x,y,(plus c z))] reste
      else
	List.append [premier] (self#add_item a b c reste)

  method add_stat (a,b,c) =
    (* add one new item to stats *)
    stats := (self#add_item a b c !stats)

  method assigns = assigns
  method calls = calls
  method gotos = gotos
  method loops = loops
  method ifs = ifs
  method exits = exits
  method funcs = funcs
  val mutable standalone = true
  method set_standalone v = begin standalone <- v end
  val mutable mem_access = 0
  method mem_access = mem_access
  val functions_no_source = Varinfo.Hashtbl.create 97
  val functions_with_source = Varinfo.Hashtbl.create 97
  method functions_no_source = functions_no_source
  method functions_with_source = functions_with_source
  method vvdec vi =
    if isFunctionType vi.vtype then
      if not (Varinfo.Hashtbl.mem functions_with_source vi) then
	Varinfo.Hashtbl.replace functions_no_source vi
	  (try Varinfo.Hashtbl.find functions_no_source vi with Not_found -> 0);
    DoChildren

  method vfunc fdec =
    current_file_name <- file_of (GFun (fdec, null_location));
    current_function_name <- fdec.svar.vname;
    self#add_stat (current_file_name,current_function_name,(0,0,0,0,0,0,0,0));
    funcs <- funcs+1;
    let n =
      try
	let n = Varinfo.Hashtbl.find functions_no_source fdec.svar in
	Varinfo.Hashtbl.remove functions_no_source fdec.svar;
	n
      with Not_found ->
	0
    in
    let n =
      try Varinfo.Hashtbl.find functions_with_source fdec.svar + n
      with Not_found -> n
    in
    Varinfo.Hashtbl.replace functions_with_source fdec.svar n;
    DoChildren

  method vlval (host,_) =
    begin
      match host with
      | Mem _ -> mem_access <- mem_access + 1;
	  self#add_stat (current_file_name,current_function_name,(0,0,0,0,0,1,0,0))
      | _ -> ()
    end;
    DoChildren

  method sloc = sloc
  method vstmt s =
    sloc <- sloc + 1 ;
    begin match s.skind with
    | If _ ->
	ifs <- ifs + 1;
	self#add_stat (current_file_name,current_function_name,(1,0,0,0,0,0,0,0))
    | Loop _ ->
	loops <- loops + 1;
	self#add_stat (current_file_name,current_function_name,(0,0,1,0,0,0,0,0))
    | Goto _ ->
	gotos <- gotos + 1;
	self#add_stat (current_file_name,current_function_name,(0,0,0,0,1,0,0,0))
    | Return _ ->
	exits <- exits + 1;
	self#add_stat (current_file_name,current_function_name,(0,0,0,0,0,0,1,0))
    | _ -> ()
    end;
    DoChildren

  method find_global_function (v:varinfo) =
    (* return a pair (found,spec_or_body) *)
    let found:bool ref = ref false in
    let spec:bool ref = ref false in
    iterGlobals (Ast.get()) (
      function glob ->
	match glob with
	| GFun (s,_) -> (* function with code *)
	    if (s.svar==v) then found:=true;spec:=false
	| GVarDecl (_,s,_) -> (* function w/o code *)
	    if (s==v) then found:=true;spec:=true
	| _ -> ());
    (!found,!spec)

  method image (glob:global) =
    (* extract just the name of the global , for printing purposes *)
    match glob with
    | GVar (v,_,_) -> v.vname ^ " (GVar) "
    | GVarDecl (_,v,_) -> v.vname ^ " (GVarDecl) "
    | GFun (fdec, _) -> fdec.svar.vname ^ " (GFun) "
    | GType (ty, _) -> ty.tname
    | GCompTag (ci, _) | GCompTagDecl (ci, _) -> ci.cname
    | GEnumTagDecl (ei,_) | GEnumTag (ei,_) -> ei.ename
    | GAsm (_,_) | GPragma _ | GText _ -> ""
    | GAnnot (an,_) ->
	match an with
	  Dfun_or_pred (li,_) -> li.l_var_info.lv_name
	| Daxiomatic (s,_,_) -> s
	| Dtype (lti,_) ->  lti.lt_name
	| Dlemma (ln, _, _, _, _, _) ->  ln
	| Dinvariant (toto,_) -> toto.l_var_info.lv_name
	| Dtype_annot (ta,_) -> ta.l_var_info.lv_name

  method images (globs:global list) =
    (* extract just the names of the globals, for printing purposes *)
    let les_images = List.map self#image globs in
    String.concat "," les_images

  method print_all =
    prerr_endline ("* print_all");
    let all_files = Globals.FileIndex.get_files () in
    let print_one fic =
      let (_,glob) = Globals.FileIndex.find fic in
      prerr_endline ("* " ^ fic ^ " : " ^ (self#images glob))
    in
    List.iter print_one all_files

  method vinst i =
    begin match i with
    | Call(_, e, _, _) ->
	calls <- calls + 1;
	self#add_stat (current_file_name,current_function_name,(0,0,0,1,0,0,0,0));
	(match e.enode with
	 | Lval(Var v, NoOffset) ->
	     let next tbl =
	       Varinfo.Hashtbl.replace tbl v (succ (Varinfo.Hashtbl.find tbl v))
	     in
	     begin
	       try next functions_with_source;
	       with Not_found ->
		 try next functions_no_source;
		 with Not_found ->
		   (* if this iterator is called on a specific global
		      function only, it might not find the target of this call
		      so we check if this function is w/ or w/o source and
		      add 1 to the number of calls accordingly.
		   *)
		   (* self#print_all; *)
		   if not standalone then
		     let (ya,codeless) = self#find_global_function v in
		     if ya then
		       begin
			 if codeless then
			   Varinfo.Hashtbl.replace functions_with_source v 0
			 else
			   Varinfo.Hashtbl.replace functions_no_source v 0
		       end
		     else
		       Metrics_parameters.fatal "Got no source for %s" v.vname
		   else
		     Metrics_parameters.fatal "Got no source for %s" v.vname
	     end
	 | _ -> ());
	DoChildren
    | Set _ ->
	assigns <- succ assigns;
	self#add_stat (current_file_name,current_function_name,(0,1,0,0,0,0,0,0));
	DoChildren
    | _ ->     DoChildren
    end

end

(* This may be used to generate code associated to prototypes.

let find_lvals_to_assign vi =
  let rec rec_find_lvals lval =
    let typ = typeOfLval lval in
    if isArithmeticType typ then [lval]
    else if isPointerType typ then
      rec_find_lvals (mkMem ~addr:(Lval lval) ~off:NoOffset)
    else assert false
  in
  if isPointerType vi.vtype then
    (* find the lvals of basic types *)
    rec_find_lvals (mkMem ~addr:(Lval (Var vi,NoOffset)) ~off:NoOffset)
  else []

let make_body_from_prototype vi =
  vi.vstorage <- NoStorage;
  let new_fundec =
    { svar  = vi;
      smaxid = 0;
      slocals = [];
      sformals = [];
      sbody = mkBlock [];
      smaxstmtid = None;
      sallstmts = [];
      sspec =   {requires = None;
                 assigns = None;
                 ensures = None;
                 decreases = None}
    }
  in
  (* formal might have no name, let's fix the type to generate a name:*)
  vi.vtype <- begin match vi.vtype with
  | TFun (typ, None, b, attr) ->  vi.vtype
  | TFun (typ, Some args, b, attr) ->
      let counter = ref 0 in
      let named_args =
        List.map
          (fun (n,t,a) ->
             (if n= "" then
                begin incr counter;
                  "Frama_C_formals_"^(string_of_int !counter)
                end
              else n),
             t,a)
          args
      in
      TFun (typ, Some named_args, b, attr)
  | _ -> assert false
  end;
  setFunctionTypeMakeFormals new_fundec vi.vtype;
  let fresh_global = GFun (new_fundec,vi.vdecl) in
  let fresh_volatile =
    makeLocalVar
      new_fundec
      "Frama_C_entropy_source"
    (typeAddAttributes [Attr ("volatile",[])] (TInt(IULongLong, [])) )
  in
  let volatile_lval = Lval(Var fresh_volatile,NoOffset) in
  List.iter (fun formal ->
               let lvals_to_assign = find_lvals_to_assign formal in
               let stmts =
                 List.map
                   (fun lval_to_assign ->
                      mkStmtOneInstr
                        (Set (lval_to_assign,
                              volatile_lval,
                              vi.vdecl)))
                   lvals_to_assign
               in
               let conditional
               new_fundec.sbody.bstmts <- new_fundec.sbody.bstmts@stmts;
            )
    new_fundec.sformals;

  Format.printf "Made: <@\n %a@\n>@." d_global fresh_global;
  new_fundec

class turn_prototype_into_body protos_vi turn_into = object(self)
  inherit nopCilVisitor

  method vglob glob =
    match glob with
    | GVarDecl (fspec, vi, loc) when Cil_datatype.Varinfo.Set.mem vi protos_vi ->
        assert (isFunctionType vi.vtype);
        ChangeTo [GFun(make_body_from_prototype vi,loc)]

    | _ -> SkipChildren

end

*)

let compute () =
  let file = Ast.get () in
    let v = new slocVisitor in
      v#set_standalone true; (* measure the entire code *)
      visitCilFileSameGlobals (v:>cilVisitor) file;
      v#complete_stats ();
      LastResult.set
	{ call_statements = v#calls;
	  goto_statements = v#gotos;
	  assign_statements = v#assigns;
	  if_statements = v#ifs;
	  mem_access = v#mem_access;
	  loop_statements = v#loops;
	  function_definitions = v#funcs;
	  sloc = v#sloc;
	  functions_without_source =  v#functions_no_source;
	  functions_with_source =  v#functions_with_source;
	  cyclos = (v#ifs +v#loops) - v#exits +2*v#funcs
	 };
      (* print results on HTML file *)
      let cout = open_out "metrics.html" in
      let fmt = formatter_of_out_channel cout in
	(* header *)
	fprintf fmt "<!DOCTYPE HTML PUBLIC >\n";
	fprintf fmt "<html>\n";
	fprintf fmt "<head>\n";
	fprintf fmt "</head>\n";
	fprintf fmt "<body>\n";
	fprintf fmt "<div style=\"text-align: left;\">\n";
	fprintf fmt "<h1><span style=\"font-weight: bold;\">Metrics</span></h1>\n";
	fprintf fmt "<h2>Synthetic results</h2>\n";
	fprintf fmt "<br>\n";
	(* *)	(* global stats *)
	fprintf fmt "<span style=\"font-weight: bold;\">Defined function</span> (%d):<br>\n"
	  (Varinfo.Hashtbl.length v#functions_with_source);
	(* *)
	fprintf fmt "@[&nbsp; %a@]@ <br>\n" pretty_set v#functions_with_source;
	fprintf fmt "<br>\n";
	fprintf fmt "<span style=\"font-weight: bold;\">Undefined functions</span> (%d):<br>\n" (Varinfo.Hashtbl.length v#functions_no_source);
	(* *)
	fprintf fmt "@[&nbsp; %a@]@ <br>\n" pretty_set v#functions_no_source;
	fprintf fmt "<br>\n";
	(* *)
	fprintf fmt "<span style=\"font-weight: bold;\">Potential entry points</span> (%d):<br>\n" (number_entry_points v#functions_with_source);
	(* *)
	fprintf fmt "@[&nbsp; %a@]@ <br>\n" pretty_entry_points v#functions_with_source;
	fprintf fmt "<br>\n";
	(* TBD other gloabl stats *)
	fprintf fmt "<span style=\"font-weight: bold;\">SLOC:</span> (%d)<br>\n" v#sloc;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of if statements:</span> (%d)<br>\n" v#ifs;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of assignments:</span> (%d)<br>\n" v#assigns;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of loops:</span> (%d)<br>\n" v#loops;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of calls:</span> (%d)<br>\n" v#calls;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of gotos:</span> (%d)<br>\n" v#gotos;
	fprintf fmt "<span style=\"font-weight: bold;\">Number of pointer access:</span> (%d)<br>\n" v#mem_access;
	fprintf fmt "<br>\n";
	fprintf fmt "<h2>Detailed results</h2>\n";
	fprintf fmt "<br>\n";
	(* detailed stats *)
	v#print_stats fmt;
	close_out cout;
	Metrics_parameters.feedback "Metrics printed to file metrics.html"

let main () =
  if Metrics_parameters.is_on () then begin
    !Db.Metrics.compute ();
    if Metrics_parameters.Print.get () then
      Metrics_parameters.result "Syntactic metrics@\n %t" !Db.Metrics.pretty;
    if Metrics_parameters.Dump.get () <> "" then
      !Db.Metrics.dump ()
  end

let () = Db.Main.extend main

let () =
  Db.register
    (Db.Journalize
       ("Metrics.compute", Datatype.func Datatype.unit Datatype.unit))
    Db.Metrics.compute compute;
  Db.register
    (Db.Journalize
       ("Metrics.pretty", Datatype.func Datatype.formatter Datatype.unit))
    Db.Metrics.pretty pretty;
  Db.register
    (Db.Journalize ("Metrics.dump", Datatype.func Datatype.unit Datatype.unit))
    Db.Metrics.dump dump;
  Db.register Db.Journalization_not_required
    Db.Metrics.last_result LastResult.get

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

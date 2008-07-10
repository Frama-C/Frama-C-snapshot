open Format
open Project

(*prover*)
let provers = [Ergo ; Simplify ; Z3 ; Yices ; Cvc3]

(*color*)
let valid_color = "#00FF00"
let invalid_color = "#FF8093"
let failed_color = "#FF0000"
let unknown_color = "#FFFF80"
let cannotdecide_color = "#FF80FF"
let running_color = "#8080FF"
let timeout_color = "#FF8093"

(* toggle *)
let toggle_lemma l = l.lemma_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value)) l.lemma_tags

let toggle_function f =  f.function_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		assert false) f.function_tags

let toggle_behavior b = b.behavior_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value))  b.behavior_tags

let toggle_goal g = g.goal_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value))  g.goal_tags

let use_prover p =
  match p with 
    | Ergo -> ("--why",".why","",67) 
    | Simplify -> ("--simplify", ".sx","",66)
    | Z3 -> ("--smtlib",".smt","z3",67)
    | Yices -> ("--smtlib",".smt","",67)
    | Cvc3 -> ("--smtlib",".smt","cvc3",67)
    | Other -> assert false

open Wserver

let launch_goal  prover context g =  
  let (option,endoffile,opt,size) = use_prover prover in
  let (reademe,writeme) = Unix.pipe () in
  eprintf "why -no-prelude %s %s %s@." option context g.goal_file;
  let _ = Unix.create_process
    "why" [| "why"; "-no-prelude"; option; context; g.goal_file  |]
    Unix.stdin Unix.stdout Unix.stderr in
  let _ = Unix.wait() in
  eprintf "dp %s -timeout 10 %s@." opt ((String.sub g.goal_file 0 
	  (String.length g.goal_file  - 4))^"_why"^endoffile) ;
  let _ = 
    if opt = "" 
    then 
      Unix.create_process
	"dp" 
	[| "dp" ; "-timeout" ; "10" ; 
	   (String.sub g.goal_file 0 
	      (String.length g.goal_file  - 4))^"_why"^endoffile  |]
	Unix.stdin writeme Unix.stderr 
    else
      Unix.create_process
	"dp" 
	[| "dp" ; "-smt-solver" ; opt  ;"-timeout" ; "10" ; 
	   (String.sub g.goal_file 0 
	      (String.length g.goal_file  - 4))^"_why"^endoffile  |]
	Unix.stdin writeme Unix.stderr 
  in  
  let _ = Unix.wait() in
  let size = size+(String.length g.goal_file) in
  let buff = String.make size  '!' in
  let _ = Unix.read reademe buff 0 size in
  eprintf "%s@." buff;
  (*    let buff = String.make size '?' in
	let _ = Unix.read reademe buff 0 size in
    eprintf "%s@." buff;*)
  Unix.close writeme;
  Unix.close reademe;
  let proof = List.filter (fun (s,_) -> s!=prover) g.proof in
  (g.proof <-
     match String.get buff ((String.length buff)-1) with 
       | '.' -> (prover,("valid","10","",""))::proof
       | '*' -> (prover,("invalid","10","",""))::proof  
       | '?' -> (prover,("cannotdecide","10","",""))::proof  
       | '#' -> (prover,("timeout","10","",""))::proof  
       | '!' -> (prover,("failure","10","",""))::proof 
       | c -> Format.eprintf "erreur : %c@." c;
	   assert false)
     
let version () = 
  printf "This is WhyWeb version %s, compiled on %s
Copyright (c) 2008 - Claude Marché
This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)
" Version.version Version.date;
  exit 0

let port = ref 2372
 
let spec =
 [ "-version", Arg.Unit version, 
   "  prints version and exit" ;
   "-port", Arg.Int ((:=) port), 
   "  set server port (default " ^ string_of_int !port ^ ")" ;
]

let file = ref None

let add_file f = 
  if not (Sys.file_exists f) then begin
    eprintf "whyweb: %s: no such file@." f;
    exit 1
  end;
  file := Some f

let usage = "whyweb [options]"

let () = Arg.parse spec add_file usage

let file = match !file with
  | None -> ()
  | Some f -> Arg.usage spec usage; exit 1

let proj = ref (Project.create "")

let proj_file = ref ""

let coms = Hashtbl.create 1023

let file = ref ""
let line = ref 0
let beginning = ref 0
let ending = ref 0
let current_line = ref 0
let current_page = ref ""


let launch_goal prover g =
  let proof = g.proof in
  g.proof <- (prover,("running","10","",""))::proof;
  launch_goal prover !proj.project_context_file g

let launch_lemmas prover lemmas =
  List.iter (fun l -> launch_goal prover l.lemma_goal) lemmas

let launch_behavior prover b =
  List.iter (launch_goal prover)  b.behavior_goals

let launch_function prover f =
  List.iter (launch_behavior prover) f.function_behaviors

let launch_functions prover functions =
  List.iter (launch_function prover) functions

let interp_com c =
  try
    let (c,loc) = Hashtbl.find coms c in
    begin
    match c with
      | `ToggleFunction f ->()
      | `ToggleLemma l -> ()
      | `ToggleBehavior b -> ()
      | `ToggleGoal g -> ()
      | `OpenFunction f -> toggle_function f
      | `OpenLemma l -> toggle_lemma l
      | `OpenBehavior b -> toggle_behavior b
      | `OpenGoal g -> toggle_goal g
      | `LaunchErgo g -> let _ =  Thread.create (launch_goal Ergo) g in ()
      | `LaunchErgoLemma -> let _ =  
	    Thread.create (launch_lemmas Ergo) !proj.project_lemmas in ()
      | `LaunchErgoFunctions -> let _ =  
	  Thread.create (launch_functions Ergo) !proj.project_functions in ()
      | `LaunchErgoBehavior b -> 
	  let _ =  Thread.create (launch_behavior Ergo)  b in ()
      | `LaunchErgoFunction f -> 
	  let _ =  Thread.create (launch_function Ergo) f  in ()
      | `LaunchSimplify g -> 
	  let _ = Thread.create (launch_goal Simplify) g in ()
      | `LaunchSimplifyLemma -> let _ =  
	    Thread.create (launch_lemmas Simplify) !proj.project_lemmas in ()
      | `LaunchSimplifyFunctions -> let _ =  
	  Thread.create (launch_functions Simplify) !proj.project_functions 
	in ()
      | `LaunchSimplifyBehavior b -> 
	  let _ =  Thread.create (launch_behavior Simplify)  b in ()
      | `LaunchSimplifyFunction f -> 
	  let _ =  Thread.create (launch_function Simplify) f  in ()
      | `LaunchZ3 g -> let _ =  Thread.create (launch_goal Z3) g in ()
      | `LaunchZ3Lemma -> let _ =  
	    Thread.create (launch_lemmas Z3) !proj.project_lemmas in ()
      | `LaunchZ3Functions -> let _ =  
	  Thread.create (launch_functions Z3) !proj.project_functions in ()
      | `LaunchZ3Behavior b -> 
	  let _ =  Thread.create (launch_behavior Z3)  b in ()
      | `LaunchZ3Function f -> 
	  let _ =  Thread.create (launch_function Z3) f  in ()
      | `LaunchYices g -> let _ =  Thread.create (launch_goal Yices) g in ()
      | `LaunchYicesLemma -> let _ =  
	    Thread.create (launch_lemmas Yices) !proj.project_lemmas in ()
      | `LaunchYicesFunctions -> let _ =  
	  Thread.create (launch_functions Yices) !proj.project_functions 
	in ()
      | `LaunchYicesBehavior b -> 
	  let _ =  Thread.create (launch_behavior Yices)  b in ()
      | `LaunchYicesFunction f -> 
	  let _ =  Thread.create (launch_function Yices) f  in ()
      | `LaunchCvc3 g -> let _ =  Thread.create (launch_goal Cvc3) g in ()
      | `LaunchCvc3Lemma -> let _ =  
	    Thread.create (launch_lemmas Cvc3) !proj.project_lemmas in ()
      | `LaunchCvc3Functions -> let _ =  
	  Thread.create (launch_functions Cvc3) !proj.project_functions in ()
      | `LaunchCvc3Behavior b -> 
	  let _ =  Thread.create (launch_behavior Cvc3)  b in ()
      | `LaunchCvc3Function f -> 
	  let _ =  Thread.create (launch_function Cvc3) f  in ()
      | `Save -> Project.save !proj !proj.project_name
    end;
    loc
  with Not_found -> ("",0,0,0)

let com_count = ref 0

let reg_com c =
  incr com_count;
  let loc = match c with     
    | `ToggleFunction f ->   
	let (_,lin,_,_) = f.function_loc in
	current_line := lin; 
	f.function_loc
    | `ToggleLemma l -> 
	let (_,lin,_,_) = l.lemma_loc in
	current_line := lin;
	l.lemma_loc
    | `ToggleBehavior b -> 
	let (_,lin,_,_) = b.behavior_loc in
	current_line := lin;
	b.behavior_loc
    | `ToggleGoal g -> 
	let (_,lin,_,_) = g.goal_expl.Logic_decl.vc_loc in
	current_line := lin;
	g.goal_expl.Logic_decl.vc_loc   
    | `OpenFunction _
    | `OpenLemma _
    | `OpenBehavior _
    | `OpenGoal _ -> (!file,!line,!beginning,!ending)
    | `LaunchErgo _ -> (!file,!line,!beginning,!ending) 
    | `LaunchErgoLemma -> (!file,!line,!beginning,!ending)
    | `LaunchErgoFunctions -> (!file,!line,!beginning,!ending)
    | `LaunchErgoBehavior  _ -> (!file,!line,!beginning,!ending)
    | `LaunchErgoFunction  _ -> (!file,!line,!beginning,!ending)
    | `LaunchSimplify _ -> (!file,!line,!beginning,!ending) 
    | `LaunchSimplifyLemma -> (!file,!line,!beginning,!ending)
    | `LaunchSimplifyFunctions -> (!file,!line,!beginning,!ending)
    | `LaunchSimplifyBehavior  _ -> (!file,!line,!beginning,!ending)
    | `LaunchSimplifyFunction  _ -> (!file,!line,!beginning,!ending)
    | `LaunchZ3 _ -> (!file,!line,!beginning,!ending) 
    | `LaunchZ3Lemma -> (!file,!line,!beginning,!ending)
    | `LaunchZ3Functions -> (!file,!line,!beginning,!ending)
    | `LaunchZ3Behavior  _ -> (!file,!line,!beginning,!ending)
    | `LaunchZ3Function  _ -> (!file,!line,!beginning,!ending)
    | `LaunchYices _ -> (!file,!line,!beginning,!ending) 
    | `LaunchYicesLemma -> (!file,!line,!beginning,!ending)
    | `LaunchYicesFunctions -> (!file,!line,!beginning,!ending)
    | `LaunchYicesBehavior  _ -> (!file,!line,!beginning,!ending)
    | `LaunchYicesFunction  _ -> (!file,!line,!beginning,!ending)
    | `LaunchCvc3 _ -> (!file,!line,!beginning,!ending) 
    | `LaunchCvc3Lemma -> (!file,!line,!beginning,!ending)
    | `LaunchCvc3Functions -> (!file,!line,!beginning,!ending)
    | `LaunchCvc3Behavior  _ -> (!file,!line,!beginning,!ending)
    | `LaunchCvc3Function  _ -> (!file,!line,!beginning,!ending)
    | `Save -> (!file,!line,!beginning,!ending)
  in
  let n = string_of_int !com_count in
  Hashtbl.add coms n (c,loc);
  !proj.project_name ^ "?" ^ n
  

(* main *)

open Wserver
open Format

let main_page msg =
  wprint "<h1 align=center>Welcome to the World-Why-Web</h1><hr>";
  wprint "<h2>List of current projects</h2>";
  wprint "<ol>";
  wprint "<li> <a href=\"?test\">test</a>";
  wprint "<li> <a href=\"?bench/java/good/why/Gcd.wpr\">Gcd</a>";
  wprint "<li> <a href=\"?bench/java/good/why/Lesson1.wpr\">Lesson1</a>";
  wprint "</ol>";
  if msg <> "" then
    begin
      wprint "<hr>%s" msg;
    end

let load_prj file =
  eprintf "Reading file %s@." file;
  try
    proj := Project.load file;
    proj_file := file;
  with
      Sys_error _ -> 
	let msg = "Cannot open file " ^ file in
	eprintf "%s@." msg;
	main_page msg;
	raise Exit
	
let toogletooltip b s =
  (if b then "close" else "open") ^ s
  
let rec find_prover l = 
  match  l with
    | (prover,(status,_,_,_))::l -> (prover,status)::(find_prover l)
    | [] -> []

let validity nl valid = 
  let(valid,color) =
    match valid with 
    | "valid" -> "Valid", valid_color
    | "timeout" -> "TimeOut", timeout_color
    | "cannotdecide" -> "Unknown", cannotdecide_color
    | "invalid" ->  "Invalid", invalid_color
    | "failure"-> "Failed", failed_color
    | "running" -> "Running", running_color
    | _ -> assert false 
  in
  wprint "</td><td bgcolor=\"%s\"><a href=\"%s\">%s</a></td>" 
   color nl valid

let goal_validity_bool prover g = 
  let list_prover = find_prover g.proof in
  let valid =  try 
      List.assoc prover list_prover 
    with Not_found -> "invalid" 
  in 
  valid

let goal_validity prover g nl = validity nl (goal_validity_bool prover g)

let goal_is_valid prover g = 
  let (v,_,_,_) = try List.assoc prover g.proof  
    with Not_found -> ("invalid","","","")
  in
  v = "valid"

let behavior_validity prover b =
  List.for_all (goal_is_valid prover)  b.behavior_goals 

let function_validity prover f =
  List.for_all (behavior_validity prover) f.function_behaviors


let goal g indice indent =
  let s =fprintf str_formatter "%s" 
    (Explain.msg_of_kind g.goal_expl.Logic_decl.vc_kind);
    flush_str_formatter ()
  in
  let nt = reg_com (`ToggleGoal g) in 
  let no = reg_com (`OpenGoal g) in 
  let call_provers = 
    List.map (fun prover -> (prover,
			     match prover with 
			       | Ergo -> reg_com (`LaunchErgo g)
			       | Simplify -> reg_com (`LaunchSimplify g)
			       | Z3 -> reg_com (`LaunchZ3 g) 
			       | Yices -> reg_com (`LaunchYices g) 
			       | Cvc3 -> reg_com (`LaunchCvc3 g)
			       | _ -> assert false)) provers in
  let op = List.assoc "ww_open" g.goal_tags = "true" in
  if op 
  then wprint "<tr><td>&nbsp &nbsp %s<a href=\"%s#%d\">-</a>" indent no !current_line
  else wprint "<tr><td>&nbsp &nbsp %s<a href=\"%s#%d\">+</a>" indent no !current_line; 
  wprint "%d : <a href=\"%s#%d\">%s</a>
" indice nt !current_line s;
  if op 
  then
    begin
      try
	wprint "<div class=\"thePO\"><pre>";
	let fi = open_in g.goal_file in
	begin
	  try 
	    while true do  
	      wprint "%s
" (input_line fi);
	    done
	  with End_of_file -> close_in fi
	end;
	wprint "</pre></div>";
      with Sys_error _ -> 
	wprint "File %s don't exist" g.goal_file;
    end;
  List.iter (fun (prover,launcher) -> goal_validity prover g launcher ) 
    call_provers;
  wprint "</tr>
"

let string_of_addr addr = 
  match addr with 
    | Unix.ADDR_UNIX s -> s
    | Unix.ADDR_INET (ie,i) -> 
	(Unix.string_of_inet_addr ie)^":"^string_of_int(i)

let () =
  Wserver.f None !port 60 None
    (fun (addr,req) script cont ->
       eprintf "add : %s@." (string_of_addr addr);
       eprintf "request: %a@." (Pp.print_list Pp.comma pp_print_string) req;
       eprintf "script: `%s'@." script;
       eprintf "cont: `%s'@." cont;
       http "";
       current_page := "http://localhost:2372/" ^ script;
       wprint "
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
<head>
<META HTTP-EQUIV=\"Refresh\" CONTENT=\"10; URL=%s\"> 
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
<title> 
WhyWeb
</title><style type=\"text/css\">
.lemenu {
  position: absolute;
  top: 4em;
  overflow: auto; 
  width: 50%%;
  height: 93%%;
}

.thePO {
   overflow: auto; 
  width: 95%%;
  height: 95%%;
}

h1 {   
  height: 5%%;
}

a{
text-decoration: none;
color: black; }


.content {
  margin:0em 0px 0em 50%%;
  overflow: auto; 
  height: 93%%;
}
</style>
</head>
<body>" !current_page;
       try
	 if script = "" then 
	   begin
	     if cont = "" then (main_page ""; raise Exit);
	     load_prj cont;
	   end
	 else 
	   if !proj.project_name <> script then
	     begin
	       eprintf "Previous project: `%s'@." !proj.project_name;
	       eprintf "TODO: save previous project@.";
	       main_page "Argument is not the currently opened project";
	       raise Exit
	     end;
	 let (loc_file,loc_line,loc_beginning,loc_ending) = interp_com cont in
	 file := loc_file;
	 line := loc_line;
	 beginning := loc_beginning;
	 ending := loc_ending;
	 Hashtbl.clear coms;
	 com_count := 0;
	 wprint "<h1 align=center> Project name: %s</h1>

<div class=\"lemenu\">" !proj.project_name;
     let ns = reg_com (`Save) in
wprint "<center><a href=\"%s\">Save Project</a></center>
<table border=\"1\"  cellpadding=\"0\" cellspacing=\"0\">" ns;
	 wprint "<tr><th></th>";
	 List.iter (fun prover -> 
		      wprint "<th>%s</th>" (Project.provers_name prover)) 
	   provers;
	 wprint "</tr>
";
	 wprint "<tr><th>Lemmas</th>"; 
	 let call_provers = 
	   List.map (fun prover -> 
		       (prover,
			match prover with 
			  | Ergo -> reg_com (`LaunchErgoLemma)
			  | Simplify -> reg_com (`LaunchSimplifyLemma)
			  | Z3 -> reg_com (`LaunchZ3Lemma) 
			  | Yices -> reg_com (`LaunchYicesLemma) 
			  | Cvc3 -> reg_com (`LaunchCvc3Lemma)
			  | Other -> assert false)) provers in
	 List.iter (fun (prover,launch) ->
		     if (List.for_all 
			   (fun l -> 
			      "valid" = 
			       (goal_validity_bool prover l.lemma_goal)) 
			   !proj.project_lemmas) 
		     then validity launch "valid"
		     else validity launch "invalid" ) call_provers;
	 wprint "</tr>
";
	 let indice = ref 1 in
 	 List.iter 
 	   (fun l ->  
 	      let nt = reg_com (`ToggleLemma l) in  
 	      let no = reg_com (`OpenLemma l) in  
	      let op = List.assoc "ww_open" l.lemma_tags = "true" in 
 	      if op  
 	      then 
		begin
		  wprint "<tr><td><a href=\"%s#%d\">-</a>" no !current_line; 
		end
 	      else wprint "<tr><td><a href=\"%s#%d\">+</a>" no !current_line;
 	      wprint "%d : <a href=\"%s#%d\">%s</a></td>" !indice nt  
 		!current_line l.lemma_name; 
 	      wprint "</tr> 
 "; 
 	      indice := !indice +1; 
 	      if op then  
 		begin  
 		  goal l.lemma_goal 1 "";  
 		end  
 	   ) 
 	   !proj.project_lemmas; 
 wprint "<tr><td></td><td></td></tr>
";
	 wprint "<tr><th>Functions</th>
"; 
	 let call_provers = 
	   List.map (fun prover -> 
		       (prover,
			match prover with 
			  | Ergo -> reg_com(`LaunchErgoFunctions)
			  | Simplify -> reg_com(`LaunchSimplifyFunctions)
			  | Z3 -> reg_com(`LaunchZ3Functions) 
			  | Yices -> reg_com(`LaunchYicesFunctions) 
			  | Cvc3 -> reg_com(`LaunchCvc3Functions)
			  | Other -> assert false)) provers
	 in
	 List.iter (fun (prover,launch) ->
		      if (List.for_all (function_validity prover) 
			    !proj.project_functions) 
	 then validity launch "valid" 
	 else validity launch "invalid") call_provers;
	 wprint "</tr>
";
	 let indice = ref 1 in
	 List.iter
	   (fun f -> 
	      let nt = reg_com (`ToggleFunction f) in 
	      let no = reg_com (`OpenFunction f) in
	      let call_provers = 
		List.map (fun prover -> 
			    (prover,
			     match prover with 
			       | Ergo -> reg_com (`LaunchErgoFunction f)
			       | Simplify -> 
				   reg_com (`LaunchSimplifyFunction f)
			       | Z3 -> reg_com (`LaunchZ3Function f) 
			       | Yices -> reg_com (`LaunchYicesFunction f) 
			       | Cvc3 -> reg_com (`LaunchCvc3Function f)
			       | Other -> assert false)) 
		  provers in	 
	      let op = List.assoc "ww_open" f.function_tags = "true" in
	      if op
	      then wprint "<tr><td><a href=\"%s#%d\">-</a>" no !current_line
	      else wprint "<tr><td><a href=\"%s#%d\">+</a>" no !current_line;
	      wprint "%d : <a href=\"%s#%d\">%s</a></td>" !indice nt !current_line f.function_name;
	      List.iter (fun (prover,launch) ->
			   if (function_validity prover f)
			   then validity launch "valid"
			   else validity launch "invalid") call_provers;
	      wprint "</tr>
";
	      indice := !indice +1;
	      if op then
		begin
		  let indice = ref 1 in
		  List.iter
		    (fun b ->
		       let nt = reg_com (`ToggleBehavior b) in 
		       let no = reg_com (`OpenBehavior b) in 
		       let call_provers = 
			 List.map (fun prover -> 
				     (prover,
				      match prover with 
					| Ergo -> 
					    reg_com (`LaunchErgoBehavior b)
					| Simplify -> 
					    reg_com (`LaunchSimplifyBehavior b)
					| Z3 -> 
					    reg_com (`LaunchZ3Behavior b) 
					| Yices -> 
					    reg_com (`LaunchYicesBehavior b) 
					| Cvc3 -> 
					    reg_com (`LaunchCvc3Behavior b)
					| Other -> assert false))
			   provers in
		       let op = 
			 List.assoc "ww_open" b.behavior_tags = "true" in
		       if op 
		       then 
			 wprint "<tr><td>&nbsp &nbsp<a href=\"%s#%d\">-</a>" no
			   !current_line
		       else 
			 wprint "<tr><td>&nbsp &nbsp<a href=\"%s#%d\">+</a>" no
			   !current_line;
		       wprint "%d : <a href=\"%s#%d\">%s</a></td>" 
			 !indice nt !current_line b.behavior_name;
		       List.iter (fun (prover,launch) ->
				    if (behavior_validity prover b)
				    then validity launch "valid"
				    else validity launch "invalid") 
			 call_provers;
		       wprint "</tr>
";
		       indice := !indice +1;
		       if op then
			 begin
			   let indice = ref 0 in
			   List.iter(fun x ->   indice :=  !indice +1;goal x !indice "&nbsp &nbsp") b.behavior_goals;
			 end)
		    f.function_behaviors;
		end)
	   !proj.project_functions;
	 raise Exit
       with
	   Exit -> 
	     begin
	       wprint "
</table>


</div>

<div class=\"content\"> <pre>
";
	       if !file = "" then ()
	       else 
		 let fi = open_in !file in
		 begin
		   try 
		     let l = ref 1 in
		     let c = ref 1 in
		     let in_select = ref false in
		     wprint "<a name=\"%d\"></a>" !l;
		     while true do  
		       let char = input_char fi in
		       if char = '\n'
		       then
			 begin
			   l := !l+1;
			   if not !in_select then c := 0 else ();
			   wprint "<a name=\"%d\"></a>" !l
			 end
		       else ();
		       if (!l = !line && !c = !beginning)
		       then
			 begin
			   wprint "<FONT style=\"BACKGROUND-COLOR: #FFCC99\">";
			   in_select := true
			 end
		       else ();
		       wprint "%c" char;
		       if (!c = !ending) then wprint "</font>" else ();
		       c := !c+1
		     done
		   with End_of_file -> close_in fi
		 end;
		 wprint "</pre></div>
</body>
</html>"
	     end)

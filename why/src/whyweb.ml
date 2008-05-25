
open Format

let version () = 
  printf "This is WhyWeb version %s, compiled on %s
Copyright (c) 2008 - Claude Marché
This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)
" Version.version Version.date;
  exit 0

let spec =
 [ "-version", Arg.Unit version, 
   "  prints version and exit" ]

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

open Project

let coms = Hashtbl.create 1023

let file = ref ""
let line = ref 0
let beginning = ref 0
let ending = ref 0
let current_line = ref 0

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
  
let goal g =
  let s =fprintf str_formatter "%s" 
    (Explain.msg_of_kind g.goal_expl.Logic_decl.vc_kind);
    flush_str_formatter ()
  in
  let nt = reg_com (`ToggleGoal g) in 
  let no = reg_com (`OpenGoal g) in 
  let op = List.assoc "ww_open" g.goal_tags = "true" in
  if op 
  then wprint "<li> <a href=\"%s#%d\">-</a>" no !current_line
  else wprint "<li> <a href=\"%s#%d\">+</a>" no !current_line; 
  wprint "<a href=\"%s#%d\">%s</a> </li>" nt !current_line s;
  if op 
  then
    begin
      try
	wprint "<pre>";
	let fi = open_in g.goal_file in
	begin
	  try 
	    while true do  
	      wprint "%s" (input_line fi);
	    done
	  with End_of_file -> close_in fi
	end;
	wprint "</pre>";
      with Sys_error _ -> 
	wprint "File %s don't exist" g.goal_file;
    end


let () =
  Wserver.f None 2372 60 None
    (fun (addr,req) script cont ->
       (*
	 eprintf "request: %a@." (Pp.print_list Pp.comma pp_print_string) req;
       *)
       eprintf "script: `%s'@." script;
       eprintf "cont: `%s'@." cont;
       http ""; 
       wprint "
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
<title> 
WhyWeb
</title><style type=\"text/css\">
.lemenu {
  position: absolute;
  top: 4em;
  left: -2em; 
  overflow: auto; 
  width: 50%%;
  height: 93%%;
}

h1 {   
  height: 5%%;
}

.content {
  margin:0em 0px 0em 50%%;
  overflow: auto; 
  height: 93%%;
}
</style>
</head>
<body>";
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

<div class=\"lemenu\">
<ul class=\"menu\">" !proj.project_name;
	 
	 wprint "<h2>Lemmas</h2>";
	 wprint "<ol>";
	 List.iter
	   (fun l -> 
	      let nt = reg_com (`ToggleLemma l) in 
	      let no = reg_com (`OpenLemma l) in 
	      let op = List.assoc "ww_open" l.lemma_tags = "true" in
	      if op 
	      then wprint "<li> <a href=\"%s#%d\">-</a>" no !current_line
	      else wprint "<li> <a href=\"%s#%d\">+</a>" no !current_line;
	      wprint "<a href=\"%s#%d\">%s</a> </li>" nt !current_line 
		l.lemma_name;
	      if op then 
		begin 
		  wprint "<ul>"; 
		  goal l.lemma_goal; 
		  wprint "</ul>" 
		end 
	   )
	   !proj.project_lemmas;
	 wprint "</ol>";
	 
	 wprint "<h2>Functions</h2>";
	 wprint "<ol>";
	 List.iter
	   (fun f -> 
	      let nt = reg_com (`ToggleFunction f) in 
	      let no = reg_com (`OpenFunction f) in 
	      let op = List.assoc "ww_open" f.function_tags = "true" in
	      if op 
	      then wprint "<li> <a href=\"%s#%d\">-</a>" no !current_line
	      else wprint "<li> <a href=\"%s#%d\">+</a>" no !current_line;
	      wprint "<a href=\"%s#%d\">%s</a> </li>" nt !current_line 
		f.function_name;
	      if op then
		begin
		  wprint "<ol>";
		  List.iter
		    (fun b ->
		       let nt = reg_com (`ToggleBehavior b) in 
		       let no = reg_com (`OpenBehavior b) in 
		       let op = 
			 List.assoc "ww_open" b.behavior_tags = "true" in
		       if op 
		       then 
			 wprint "<li><a href=\"%s#%d\">-</a>" no !current_line
		       else 
			 wprint "<li><a href=\"%s#%d\">+</a>" no !current_line;
		       wprint "<a href=\"%s#%d\">%s</a></li>" nt !current_line 
			 b.behavior_name;
		       if op then
			 begin
			   wprint "<ol>";
			   List.iter goal b.behavior_goals;
			   wprint "</ol>"
			 end)
		    f.function_behaviors;
		  wprint "</ol>"
		end)
	   !proj.project_functions;
	 wprint "</ol>";
	 raise Exit
       with
	   Exit -> 
	     begin
	       wprint "
</ul>
</div>

<div class=\"content\"> <pre>
";
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

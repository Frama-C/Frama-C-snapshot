(* Some useful function to use the graphical representation of a slicing
* project. (see tests/slicing/anim.ml for a test) *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let use_dot =
  Sys.os_type <> "Win32" 
  && (Unix.WEXITED 0) = Unix.system ("which dot > /dev/null 2>&1") 

(* function to append [string_of_int n] on 3 char to basename. *)
let nth_name basename n = 
  assert (n < 1000);
  let str_n = string_of_int n in
  let str_n = if n < 100  then ("0"^str_n) else str_n in
  let str_n = if n < 10   then ("0"^str_n) else str_n in
  basename^"."^str_n
;;

(* generate the nth .jpg file (generate to .dot file and then remove it) *)
let print_proj basename title proj n =
  let name = nth_name basename n in
  let dot_name = (name^".dot") in
  let jpg_name = (name^".jpg") in
  !Db.Slicing.Project.print_dot ~filename:dot_name ~title:title proj;
  if use_dot then
    ignore (Sys.command ("dot -Tjpg -o "^jpg_name^" "^dot_name^" 2>/dev/null"));
  Sys.remove dot_name;
  n+1
;;

(* apply all requests of the project and generate a .jpg file for each step.
* (begin at number [n])
*)
let build_all_graphs basename title proj first_n =
  Format.printf "Processing %s : " basename;
  let rec next n =
    Format.printf ".@?";
    try
      !Db.Slicing.Request.apply_next_internal proj;
      let title = title^" ("^(string_of_int (n - first_n))^")" in
      let n = print_proj basename title proj n in
      next n
    with Not_found -> n
  in
  let next_num = next first_n in Format.printf "@."; next_num
;;

let all_files basename = basename^".*.jpg ";;
let display_cmd basename = "display -resize 1000x500 "^(all_files basename);;
let clean_cmd basename = "rm -f "^(all_files basename);;
let build_anim_cmd basename=
  "convert -delay 10 -loop 1 "^(all_files basename)^" "^basename^".gif";;

let print_help basename =
  let display_cmd = display_cmd basename in
  let clean_cmd = clean_cmd basename in
  Format.printf "To display '%s' use :@\n\t%s@\n" basename display_cmd;
  Format.printf "\t- use : Space/Backspace to see next/previous step@\n";
  Format.printf "\t- use : 'q' to quit@\n@@\n";
  Format.printf
    "After that, you can clear the generated files with:@\n\t%s@." clean_cmd

let remove_all_files basename = 
  Format.printf "removing generated .jpg files@.";
  ignore (Sys.command (clean_cmd basename))

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

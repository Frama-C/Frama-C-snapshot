{
  let debug = true

  let string_if_not_zero nbr = if nbr = 0 then "" else "."^string_of_int nbr

  type state = CMD_STATE | ARG_STATE | SRC_STATE | OPT_STATE | PPC_STATE | GCC_STATE
  type token = TOP_TOKEN | CFG_TOKEN | CMD_TOKEN

  let head =
    "#!/bin/sh\n" ^
    "if [ $# -lt 5 ] ; then\n" ^
    "  echo `basename $0`\": a subshell build by make_run_script.\"\n" ^
    "  exit 1\n" ^
    "fi\n" ^
    "#comparaison\n" ^
    "Compare() {\n" ^
    "  if [ -e $1$2 ]; then\n" ^
    "    File=`basename $1$2 .log`.oracle\n" ^
    "    Dir=`dirname $1`\n" ^
    "    Dir=`dirname $Dir`\n" ^
    "    File=\"$Dir/oracle/$File\"\n" ^
    "    if [ -e ${File} ]; then\n" ^
    "      if ! diff -b -B --brief $1$2 ${File} >/dev/null\n" ^
    "      then\n" ^
    "         echo \". KO:  diff -b -B $1$2 ${File}\"\n" ^
    "      fi\n" ^
    "    else\n" ^
    "       echo \". NO oracle ${File}\"\n" ^
    "    fi\n" ^
    "  fi\n" ^
    "}\n" ^
    "# input file\n" ^
    "Src=$1\n" ^
    "shift\n" ^
    "# prefix for the out files\n" ^
    "PreFix=$1\n" ^
    "shift\n" ^
    "# extension for out files issued from stdout\n" ^
    "PostFix1=$1\n" ^
    "shift\n" ^
    "# extension for out files issued from stderr\n" ^
    "PostFix2=$1\n" ^
    "shift\n" ^
    "# command running the test\n" ^
    "Cmd=$1\n" ^
    "shift\n" ^
    "#\n" ^
    "Result=0\n"
      
  let line_nbr, lex_token, cmd_state, cmd_head, cmd_nbr, cmd_str =
    ref 1, ref TOP_TOKEN, ref CMD_STATE, ref false, ref 0, ref ""

  (* GCC: check the compilation of the source file*)
  let flush_gcc_line cmd nbr =
(*    Printf.printf"# check the compilation of the source file.\n";
    Printf.printf"#\n#echo \"- Test %d: compilation checking...\"\n" nbr;
    Printf.printf"echo \"gcc %s -c ${Src} -o ${PreFix}.o\"\n" cmd;
    Printf.printf"gcc %s -c ${Src} -o ${PreFix}.o 2> /dev/null\n" cmd;
    Printf.printf"Res=$?\n";
    Printf.printf"rm -f ${PreFix}.o\n";
    Printf.printf"if [ \"${Res}\" != 0 ] ; then\n";
    Printf.printf"  echo \"# compilation problem with: gcc %s -c ${Src} -o ${PreFix}.o\"\n" cmd;
    Printf.printf"fi\n"
*)
    ()
  let flush_redirection nbr_ =
    let nbr = string_if_not_zero nbr_ in
      Printf.printf" > ${PreFix}%s${PostFix1} 2> ${PreFix}%s${PostFix2}\n" 
        nbr nbr;
      Printf.printf"Res=$?\n";
      Printf.printf"if [ \"${Res}\" != 0 ] ; then\n";
      Printf.printf"  Result=${Res}\n";
      Printf.printf"  echo \"# abort(${Res}) on test No %d\"\n" nbr_;
      Printf.printf"else\n";
      Printf.printf"  Compare ${PreFix}%s${PostFix1}\n" nbr;
      Printf.printf"  Compare ${PreFix}%s${PostFix2}\n" nbr;
      Printf.printf"fi\n"
      
  (* default binary, options and source file are still into the command.                  *)
  (* SRC: <extra-opt>* <extra-src-file>*                                                  *)
  (* cmd= <default-tool> <default-opt> SRC                            <default-src-file>  *)
  (* cmd= <default-tool> <default-opt> <extra-opt>* <extra-src-file>* <default-src-file>  *)
  let flush_src_line cmd nbr =
    Printf.printf"#\n#echo \"- Test %d: running...\"\n" nbr;
    Printf.printf"echo \"${Cmd} %s ${Src}\"\n" cmd;
    Printf.printf"${Cmd} $* %s ${Src}" cmd;
    flush_redirection nbr
      
  (* default binary and source file are still into the command.       *)
  (* OPT: <opt>* <extra-src-file>*                                    *)
  (* cmd= <default-tool> OPT                      <default-src-file>  *)
  (* cmd= <default-tool> <opt>* <extra-src-file>* <default-src-file>  *)
  let flush_opt_line cmd nbr =
    Printf.printf"#\n#echo \"- Test %d: running...\"\n" nbr;
    Printf.printf"echo \"${Cmd} %s ${Src}\"\n" cmd;
    Printf.printf"${Cmd} %s ${Src}" cmd;
    flush_redirection nbr
      
  (* default binary is still into the command. *)
  (* ARG: <opt>* <src-file>*                   *)
  (* cmd= <default-tool> ARG                   *)
  (* cmd= <default-tool> <opt>* <src-file>*    *)
  let flush_arg_line cmd nbr =
    Printf.printf"#\n#echo \"- Test %d: running...\"\n" nbr;
    Printf.printf"echo \"${Cmd} %s\"\n" cmd;
    Printf.printf"${Cmd} %s" cmd;
    flush_redirection nbr
      
  (* CMD: <command> <opt>* <src-file>*  *)
  (* cmd= CMD                           *)
  (* cmd= <command> <opt>* <src-file>   *)
  let flush_cmd_line cmd nbr =
    Printf.printf"#\n#echo \"- Test %d: running...\"\n" nbr;
    Printf.printf"echo \"%s\"\n" cmd;
    Printf.printf"%s" cmd;
    flush_redirection nbr
    
  let flush_cmd state =
    
    if not !cmd_head
    then Printf.printf"%s" head;
    (match state with
       | PPC_STATE -> flush_gcc_line "" !cmd_nbr; Printf.printf"${Cmd} $* ${Src}"
       | GCC_STATE -> flush_gcc_line !cmd_str !cmd_nbr
       | OPT_STATE -> 
           flush_opt_line !cmd_str !cmd_nbr;  
           incr cmd_nbr
       | ARG_STATE -> flush_arg_line !cmd_str !cmd_nbr;
           incr cmd_nbr
       | SRC_STATE -> flush_src_line !cmd_str !cmd_nbr;
           incr cmd_nbr
       | CMD_STATE -> flush_cmd_line !cmd_str !cmd_nbr;
           incr cmd_nbr);
    cmd_head := true;
    cmd_str := ""
      
  let debug_cmd txt =
    if debug
    then
      Printf.printf"#%s\n" txt
    else
      ()
      
  let state_cmd token info =
    lex_token := token;
    debug_cmd info
      
  let start_cmd state token info=
    cmd_state := state;
    state_cmd token info
      
  let start_cfg state token info =
    if state = !cmd_state
    then flush_cmd !cmd_state;
    start_cmd state token info
      
  let build_cmd s =
    cmd_str := !cmd_str ^ s
    
  exception Eof
  exception ConfigNotFound
}
rule token_top = parse  
    '\n'                                              { if (5 > !line_nbr)
                                                        then line_nbr := 1 + !line_nbr
                                                        else raise ConfigNotFound }
  |  [^ '\n']                                         {  }
  |  eof                                              { debug_cmd "end of file"; raise Eof }
  | "/*" ([' ' '\t']*) "run.config" ([' ' '\t' '\n']) { line_nbr := 1; start_cmd PPC_STATE CFG_TOKEN "parsing config"; }

and token_cfg = parse 
  |  '\n'                            {  }
  |  [^ '*' '\n' ' ' '\t' ]+         {  }
  |  ['*' ' ' '\t']                  {  }
  |  eof                             { debug_cmd "end of file"; raise Eof }
  |  ('\n' | ['*' ' ' '\t'])* "GCC:" { start_cmd GCC_STATE CMD_TOKEN "* GCC:" }
  |  ('\n' | ['*' ' ' '\t'])* "CMD:" { start_cmd CMD_STATE CMD_TOKEN "* CMD:"}
  |  ('\n' | ['*' ' ' '\t'])* "OPT:" { start_cmd OPT_STATE CMD_TOKEN "* OPT:" }
  |  ('\n' | ['*' ' ' '\t'])* "ARG:" { start_cmd ARG_STATE CMD_TOKEN "* ARG:"}
  |  ('\n' | ['*' ' ' '\t'])* "SRC:" { start_cmd SRC_STATE CMD_TOKEN "* SRC:"}
  | "*/"                             { start_cfg PPC_STATE TOP_TOKEN "end of config" }

and token_cmd = parse 
    '\\' '\n' [' ' '\t']*    {  }
  |  '\n'                  { flush_cmd !cmd_state; state_cmd CFG_TOKEN "* end of line"}
  |  ('\\' as c)           { build_cmd (String.make 1 c) }
  |  ([^ '\n' '\\']+ as s) { build_cmd s }
  |  eof                   { flush_cmd !cmd_state; raise Eof }
  | "*/"                   { flush_cmd !cmd_state; state_cmd TOP_TOKEN "end of parsing"}

{
  let _ = ( try 
              let std_channel = 
                if Array.length Sys.argv = 1 then stdin
                else open_in Sys.argv.(1) 
              in
              let lexbuffer = Lexing.from_channel std_channel
              in while true do
                  let lexfun = (match !lex_token with
                                  | TOP_TOKEN -> token_top
                                  | CFG_TOKEN -> token_cfg
                                  | CMD_TOKEN -> token_cmd)
                  in lexfun lexbuffer
                done
            with Eof -> debug_cmd "#end of file"
              | ConfigNotFound -> ()
              | Failure(s) -> Printf.printf"#ERROR\n");
    print_newline();
    if not !cmd_head
    then exit 1;
    Printf.printf"#return last abort value.\n";
    Printf.printf"exit ${Result}\n";
    flush stdout;
}
   

# 27 "src/wp/driver.mll"
 

  open Qed.Logic
  open Lexing
  open Cil_types
  open LogicBuiltins

  type token =
    | EOF
    | KEY of string
    | BOOLEAN
    | INTEGER
    | REAL
    | INT of ikind
    | FLT of fkind
    | ID of string
    | LINK of string

  let keywords = [ 
    "library" , KEY "library" ;
    "type" , KEY "type" ;
    "ctor" , KEY "ctor" ;
    "logic" , KEY "logic" ;
    "predicate" , KEY "predicate" ;
    "boolean" , BOOLEAN ;
    "integer" , INTEGER ;
    "real" , REAL ;
    "char" , INT IChar ;
    "short" , INT IShort ;
    "int" , INT IInt ;
    "unsigned" , INT IUInt ;
    "float" , FLT FFloat ;
    "double" , FLT FDouble ;
  ]
  
  let ident x = try List.assoc x keywords with Not_found -> ID x

  let newline lexbuf =
    lexbuf.lex_curr_p <-
      { lexbuf.lex_curr_p with pos_lnum = succ lexbuf.lex_curr_p.pos_lnum }


# 45 "src/wp/driver.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\248\255\075\000\160\000\016\000\003\000\254\255\255\255\
    \251\255\027\000\252\255\250\000\249\255\052\000\252\255\253\255\
    \012\000\255\255\254\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\007\000\005\000\007\000\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\009\000\000\000\255\255\000\000\014\000\000\000\000\000\
    \255\255\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\006\000\000\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\002\000\005\000\000\000\010\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\008\000\018\000\000\000\000\000\015\000\009\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\016\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\000\000\000\000\
    \000\000\000\000\011\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \007\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\255\255\012\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\017\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\000\000\000\000\000\000\
    \000\000\011\000\000\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\005\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\005\000\255\255\009\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\016\000\255\255\255\255\013\000\004\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\013\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\255\255\255\255\
    \255\255\255\255\002\000\255\255\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\255\255\255\255\255\255\255\255\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\009\000\011\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\013\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\255\255\255\255\255\255\
    \255\255\011\000\255\255\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec tok lexbuf =
    __ocaml_lex_tok_rec lexbuf 0
and __ocaml_lex_tok_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 74 "src/wp/driver.mll"
        ( EOF )
# 210 "src/wp/driver.ml"

  | 1 ->
# 75 "src/wp/driver.mll"
         ( newline lexbuf ; tok lexbuf )
# 215 "src/wp/driver.ml"

  | 2 ->
# 76 "src/wp/driver.mll"
           ( tok lexbuf )
# 220 "src/wp/driver.ml"

  | 3 ->
# 77 "src/wp/driver.mll"
                        ( newline lexbuf ; tok lexbuf )
# 225 "src/wp/driver.ml"

  | 4 ->
# 78 "src/wp/driver.mll"
         ( comment lexbuf )
# 230 "src/wp/driver.ml"

  | 5 ->
let
# 79 "src/wp/driver.mll"
             a
# 236 "src/wp/driver.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 79 "src/wp/driver.mll"
               ( ident a )
# 240 "src/wp/driver.ml"

  | 6 ->
let
# 80 "src/wp/driver.mll"
                  a
# 246 "src/wp/driver.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 80 "src/wp/driver.mll"
                         ( LINK a )
# 250 "src/wp/driver.ml"

  | 7 ->
# 81 "src/wp/driver.mll"
      ( KEY (Lexing.lexeme lexbuf) )
# 255 "src/wp/driver.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_tok_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 13
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 84 "src/wp/driver.mll"
        ( failwith "Unterminated comment" )
# 266 "src/wp/driver.ml"

  | 1 ->
# 85 "src/wp/driver.mll"
         ( tok lexbuf )
# 271 "src/wp/driver.ml"

  | 2 ->
# 86 "src/wp/driver.mll"
         ( newline lexbuf ; comment lexbuf )
# 276 "src/wp/driver.ml"

  | 3 ->
# 87 "src/wp/driver.mll"
      ( comment lexbuf )
# 281 "src/wp/driver.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

# 89 "src/wp/driver.mll"
 

  let pretty fmt = function
    | EOF -> Format.pp_print_string fmt "<eof>"
    | KEY a | ID a -> Format.fprintf fmt "'%s'" a
    | LINK s -> Format.fprintf fmt "\"%s\"" s
    | BOOLEAN | INTEGER | REAL | INT _ | FLT _  -> 
	Format.pp_print_string fmt "<type>"

  type input = {
    lexbuf : Lexing.lexbuf ;
    mutable current : token ;
  }

  let skip input =
    if input.current <> EOF then input.current <- tok input.lexbuf

  let token input = input.current

  let key input a = match token input with
    | KEY b when a=b -> skip input ; true
    | _ -> false

  let skipkey input a = match token input with
    | KEY b when a=b -> skip input
    | _ -> failwith (Printf.sprintf "Missing '%s'" a)

  let ident input = match token input with
    | ID x -> skip input ; x
    | _ -> failwith "missing identifier"

  let kind input = 
    let kd = match token input with
      | INTEGER -> Z
      | REAL -> R
      | BOOLEAN -> A
      | INT i -> I (Ctypes.c_int i)
      | FLT f -> F (Ctypes.c_float f)
      | ID _ -> A
      | _ -> failwith "<type> expected"
    in skip input ; kd

  let parameter input =
    let k = kind input in
    match token input with
      | ID _ -> skip input ; k
      | _ -> k

  let rec parameters input =
    if key input ")" then [] else
      let p = parameter input in
      if key input "," then p :: parameters input else 
	if key input ")" then [p] else
	  failwith "Missing ',' or ')'"

  let signature input =
    if key input "(" then parameters input else []

  let rec depend input =
    match token input with
      | ID a | LINK a ->
	  skip input ;
	  ignore (key input ",") ;
	  a :: depend input
      | _ -> []

  let link input =
    match token input with
      | LINK f | ID f -> skip input ; f
      | _ -> failwith "Missing link symbol"

  let op = {
    inversible = false ;
    associative = false ;
    commutative = false ;
    idempotent = false ;
    neutral = E_none ;
    absorbant = E_none ;
  }

  let op_elt input =
    ignore (key input ":") ;
    let op = link input in
    skipkey input ":" ;
    match op with
      | "0" -> E_int 0
      | "1" -> E_int 1
      | "-1" -> E_int (-1)
      | _ -> 
	  try E_const (LogicBuiltins.symbol op)
	  with Not_found -> 
	    failwith (Printf.sprintf "Symbol '%s' undefined" op)
    
  let rec op_link bal op input =
    match token input with
      | LINK f ->
	  skip input ;
	  bal,Operator op,f
      | ID "left" -> skip input ; skipkey input ":" ;
	  op_link Lang.Left op input
      | ID "right" -> skip input ; skipkey input ":" ;
	  op_link Lang.Right op input
      | ID "associative" -> skip input ; skipkey input ":" ; 
	  op_link bal { op with associative = true } input
      | ID "commutative" -> skip input ; skipkey input ":" ; 
	  op_link bal { op with commutative = true } input
      | ID "ac" -> skip input ; skipkey input ":" ;
	  op_link bal { op with commutative = true ; associative = true } input
      | ID "idempotent" -> skip input ; skipkey input ":" ;
	  op_link bal { op with idempotent = true } input
      | ID "inversible" -> skip input ; skipkey input ":" ;
	  op_link bal { op with inversible = true } input
      | ID "neutral" -> 
	  skip input ; let e = op_elt input in
	  op_link bal { op with neutral = e } input
      | ID "absorbant" ->
	  skip input ; let e = op_elt input in
	  op_link bal { op with absorbant = e } input
      | ID t -> failwith (Printf.sprintf "Unknown tag '%s'" t)
      | _ -> failwith "Missing <tag> or <link>"

  let logic_link input =
    match token input with
      | LINK f -> 
	  skip input ; 
	  Lang.Nary,Function,f
      | ID "constructor" -> 
	  skip input ; skipkey input ":" ; 
	  Lang.Nary,Function,link input
      | ID "injective" -> 
	  skip input ; skipkey input ":" ; 
	  Lang.Nary,Injection,link input
      | _ -> op_link Lang.Left op input

  let rec parse theory input =
    match token input with
      | EOF -> ()
      | KEY "library" ->
	  skip input ;
	  let name = link input in
	  ignore (key input ":") ;
	  let depends = depend input in
	  ignore (key input ";") ;
	  add_library name depends ;
	  parse name input 
      | KEY "type" ->
	  skip input ;
	  let name = ident input in
	  skipkey input "=" ;
	  let link = link input in
	  add_type name ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "ctor" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let link = link input in
	  add_ctor name args ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "logic" ->
	  skip input ;
	  let result = kind input in
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let balance,category,link = logic_link input in
	  add_logic result name args ~theory ~category ~balance ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "predicate" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let link = link input in
	  add_predicate name args ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | _ -> failwith "Unexpected entry"

  let load file =
    try
      let inc = open_in file in
      let lex = Lexing.from_channel inc in
      lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p with Lexing.pos_fname = file } ;
      let input = { current = tok lex ; lexbuf = lex } in
      try
	parse "driver" input ;
	close_in inc
      with Failure msg ->
	close_in inc ;
	let source = lex.Lexing.lex_start_p in
	Wp_parameters.error ~source "(Driver Error) %s (at %a)" msg
          pretty (token input)
    with exn ->
      Wp_parameters.error "Error in driver '%s': %s" file (Printexc.to_string exn)

  (*TODO[LC] Think about projectification ... *)
  let loaded = ref false
  let load_drivers () =
    if not !loaded then
      begin
	List.iter
	  (fun file -> 
	     let path = Wp_parameters.find_lib file in
	     let echo = if Wp_parameters.has_dkey "driver" then path else file in
	     Wp_parameters.feedback "Loading driver '%s'" echo ;
	     load path)
	  (Wp_parameters.Drivers.get ()) ;
	loaded := true ;
	if Wp_parameters.has_dkey "driver" then LogicBuiltins.dump () ;
      end


# 505 "src/wp/driver.ml"

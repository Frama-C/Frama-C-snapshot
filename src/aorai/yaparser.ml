type token =
  | CALL_OF
  | RETURN_OF
  | CALLORRETURN_OF
  | IDENTIFIER of (string)
  | INT of (string)
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | LBRACELBRACE
  | RBRACERBRACE
  | RARROW
  | TRUE
  | FALSE
  | NOT
  | DOT
  | AMP
  | COLON
  | SEMI_COLON
  | COMMA
  | PIPE
  | CARET
  | QUESTION
  | COLUMNCOLUMN
  | EQ
  | LT
  | GT
  | LE
  | GE
  | NEQ
  | PLUS
  | MINUS
  | SLASH
  | STAR
  | PERCENT
  | OR
  | AND
  | OTHERWISE
  | EOF

open Parsing;;
# 30 "src/aorai/yaparser.mly"
open Logic_ptree
open Parsing
open Promelaast
open Bool3
open Format

let to_seq c =
  [{ condition = Some c;
     nested = [];
     min_rep = Some Data_for_aorai.cst_one;
     max_rep = Some Data_for_aorai.cst_one;
   }]

let is_no_repet (min,max) =
  let is_one c = Extlib.may_map Data_for_aorai.is_cst_one ~dft:false c in
  is_one min && is_one max

let observed_states      = Hashtbl.create 1
let prefetched_states    = Hashtbl.create 1

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))
;;

let fetch_and_create_state name =
  Hashtbl.remove prefetched_states name ;
  try
    Hashtbl.find observed_states name
  with
    Not_found -> 
      let s = Data_for_aorai.new_state name in
      Hashtbl.add observed_states name s; s
;;

let prefetch_and_create_state name =
    if (Hashtbl.mem prefetched_states name) or 
      not (Hashtbl.mem observed_states name) 
    then
      begin
	let s= fetch_and_create_state name in 
	Hashtbl.add prefetched_states name name;
	s
      end 
    else
      (fetch_and_create_state name)
;;

type pre_cond = Behavior of string | Pre of Promelaast.condition

# 98 "src/aorai/yaparser.ml"
let yytransl_const = [|
  257 (* CALL_OF *);
  258 (* RETURN_OF *);
  259 (* CALLORRETURN_OF *);
  262 (* LCURLY *);
  263 (* RCURLY *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* LSQUARE *);
  267 (* RSQUARE *);
  268 (* LBRACELBRACE *);
  269 (* RBRACERBRACE *);
  270 (* RARROW *);
  271 (* TRUE *);
  272 (* FALSE *);
  273 (* NOT *);
  274 (* DOT *);
  275 (* AMP *);
  276 (* COLON *);
  277 (* SEMI_COLON *);
  278 (* COMMA *);
  279 (* PIPE *);
  280 (* CARET *);
  281 (* QUESTION *);
  282 (* COLUMNCOLUMN *);
  283 (* EQ *);
  284 (* LT *);
  285 (* GT *);
  286 (* LE *);
  287 (* GE *);
  288 (* NEQ *);
  289 (* PLUS *);
  290 (* MINUS *);
  291 (* SLASH *);
  292 (* STAR *);
  293 (* PERCENT *);
  294 (* OR *);
  295 (* AND *);
  296 (* OTHERWISE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  260 (* IDENTIFIER *);
  261 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\006\000\006\000\
\003\000\003\000\007\000\008\000\008\000\009\000\009\000\009\000\
\011\000\011\000\012\000\012\000\013\000\013\000\013\000\013\000\
\013\000\015\000\015\000\016\000\016\000\010\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\018\000\
\018\000\018\000\020\000\020\000\020\000\020\000\021\000\021\000\
\021\000\021\000\022\000\022\000\022\000\023\000\023\000\023\000\
\023\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\004\000\000\000\002\000\003\000\001\000\
\002\000\001\000\004\000\003\000\001\000\005\000\003\000\002\000\
\001\000\003\000\000\000\001\000\001\000\003\000\006\000\005\000\
\004\000\002\000\003\000\000\000\003\000\002\000\000\000\001\000\
\001\000\001\000\005\000\003\000\004\000\004\000\004\000\004\000\
\004\000\001\000\001\000\002\000\003\000\003\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\002\000\
\001\000\003\000\003\000\004\000\001\000\002\000\005\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\074\000\000\000\003\000\000\000\000\000\
\000\000\002\000\010\000\000\000\000\000\000\000\009\000\008\000\
\000\000\004\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\063\000\000\000\000\000\042\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\062\000\000\000\069\000\016\000\000\000\011\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\044\000\
\064\000\000\000\000\000\000\000\000\000\034\000\032\000\033\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\047\000\066\000\073\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\000\
\050\000\051\000\052\000\053\000\054\000\056\000\057\000\059\000\
\060\000\061\000\000\000\067\000\040\000\041\000\039\000\000\000\
\000\000\025\000\000\000\027\000\020\000\000\000\000\000\018\000\
\014\000\000\000\000\000\036\000\000\000\068\000\000\000\071\000\
\024\000\000\000\038\000\037\000\000\000\029\000\023\000\035\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\006\000\013\000\017\000\011\000\022\000\
\023\000\062\000\133\000\134\000\038\000\039\000\057\000\130\000\
\073\000\040\000\041\000\042\000\043\000\044\000\045\000"

let yysindex = "\004\000\
\006\255\000\000\069\255\000\000\003\255\000\000\012\255\073\255\
\076\255\000\000\000\000\096\255\088\255\253\254\000\000\000\000\
\105\255\000\000\080\255\127\255\121\255\252\254\000\000\129\255\
\131\255\135\255\137\255\010\255\000\000\102\255\080\255\000\000\
\000\000\102\255\156\255\025\255\160\255\251\254\048\255\208\255\
\000\000\078\255\000\000\005\255\000\000\000\000\138\255\000\000\
\253\254\000\000\166\255\171\255\173\255\055\255\102\255\175\255\
\191\255\193\255\036\255\208\255\005\255\182\255\194\255\000\000\
\000\000\025\255\005\255\192\255\008\255\000\000\000\000\000\000\
\000\000\102\255\102\255\168\255\168\255\168\255\168\255\168\255\
\168\255\168\255\168\255\168\255\168\255\168\255\168\255\203\255\
\000\000\000\000\201\255\202\255\204\255\050\255\220\255\028\255\
\000\000\080\255\222\255\000\000\000\000\000\000\080\255\000\000\
\016\255\237\255\168\255\168\255\255\254\048\255\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\233\255\000\000\000\000\000\000\000\000\102\255\
\241\255\000\000\235\255\000\000\000\000\224\255\245\255\000\000\
\000\000\002\000\010\000\000\000\161\255\000\000\063\255\000\000\
\000\000\235\255\000\000\000\000\011\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\255\000\000\
\019\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\153\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\113\255\112\000\033\000\
\000\000\255\255\000\000\187\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\119\255\000\000\060\255\046\000\083\255\000\000\000\000\
\000\000\000\000\221\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\121\000\000\000\000\000\
\000\000\014\000\000\000\000\000\000\000\000\000\112\255\000\000\
\000\000\000\000\000\000\000\000\000\000\080\000\089\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\121\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\121\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\020\001\000\000\000\000\024\001\000\000\
\241\000\017\001\229\255\194\000\000\000\236\255\000\000\134\255\
\000\000\226\255\000\000\000\000\185\000\228\255\000\000"

let yytablesize = 413
let yytable = "\060\000\
\069\000\061\000\019\000\063\000\001\000\140\000\008\000\067\000\
\145\000\059\000\020\000\058\000\029\000\064\000\087\000\107\000\
\048\000\054\000\049\000\070\000\141\000\055\000\088\000\151\000\
\102\000\087\000\095\000\071\000\058\000\108\000\072\000\012\000\
\066\000\088\000\096\000\056\000\021\000\105\000\109\000\003\000\
\132\000\035\000\003\000\036\000\100\000\112\000\113\000\114\000\
\115\000\116\000\117\000\118\000\119\000\110\000\111\000\025\000\
\026\000\027\000\028\000\029\000\036\000\128\000\030\000\094\000\
\031\000\074\000\075\000\129\000\055\000\032\000\033\000\034\000\
\007\000\074\000\075\000\150\000\138\000\139\000\061\000\008\000\
\025\000\026\000\027\000\028\000\029\000\074\000\075\000\030\000\
\035\000\031\000\036\000\017\000\014\000\017\000\032\000\033\000\
\034\000\055\000\055\000\016\000\074\000\075\000\025\000\026\000\
\027\000\058\000\029\000\143\000\018\000\030\000\149\000\083\000\
\084\000\035\000\086\000\036\000\032\000\033\000\034\000\031\000\
\019\000\031\000\019\000\031\000\072\000\072\000\024\000\072\000\
\072\000\072\000\046\000\072\000\050\000\031\000\047\000\035\000\
\072\000\036\000\051\000\072\000\072\000\089\000\052\000\072\000\
\053\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
\072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
\065\000\072\000\072\000\072\000\058\000\029\000\068\000\148\000\
\107\000\091\000\072\000\058\000\029\000\072\000\092\000\107\000\
\093\000\072\000\097\000\072\000\072\000\072\000\072\000\072\000\
\072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
\065\000\065\000\035\000\065\000\036\000\065\000\098\000\065\000\
\099\000\035\000\103\000\036\000\104\000\106\000\124\000\065\000\
\065\000\125\000\126\000\065\000\127\000\065\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\065\000\065\000\070\000\070\000\131\000\070\000\135\000\070\000\
\146\000\070\000\076\000\077\000\078\000\079\000\080\000\081\000\
\137\000\070\000\070\000\142\000\144\000\070\000\128\000\070\000\
\070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
\070\000\070\000\070\000\070\000\058\000\058\000\129\000\058\000\
\005\000\058\000\101\000\058\000\120\000\121\000\122\000\123\000\
\147\000\152\000\001\000\058\000\058\000\006\000\019\000\058\000\
\010\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
\015\000\090\000\058\000\037\000\058\000\058\000\055\000\055\000\
\136\000\055\000\000\000\055\000\000\000\055\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\000\065\000\000\000\
\000\000\055\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\055\000\000\000\000\000\055\000\000\000\055\000\055\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\046\000\046\000\000\000\
\046\000\000\000\046\000\000\000\046\000\000\000\045\000\045\000\
\000\000\045\000\000\000\045\000\046\000\045\000\000\000\000\000\
\046\000\000\000\000\000\000\000\000\000\045\000\000\000\000\000\
\046\000\045\000\000\000\046\000\000\000\021\000\021\000\000\000\
\021\000\045\000\021\000\000\000\045\000\000\000\028\000\028\000\
\000\000\028\000\000\000\028\000\021\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\021\000\028\000\000\000\021\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\000\000\028\000"

let yycheck = "\030\000\
\006\001\030\000\006\001\031\000\001\000\007\001\004\001\036\000\
\131\000\030\000\014\001\004\001\005\001\034\000\010\001\008\001\
\021\001\008\001\023\001\025\001\022\001\012\001\018\001\146\000\
\009\001\010\001\054\000\033\001\004\001\022\001\036\001\020\001\
\008\001\018\001\055\000\026\001\040\001\066\000\069\000\037\001\
\013\001\034\001\037\001\036\001\009\001\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\074\000\075\000\001\001\
\002\001\003\001\004\001\005\001\036\001\012\001\008\001\009\001\
\010\001\038\001\039\001\018\001\009\001\015\001\016\001\017\001\
\004\001\038\001\039\001\013\001\107\000\108\000\107\000\004\001\
\001\001\002\001\003\001\004\001\005\001\038\001\039\001\008\001\
\034\001\010\001\036\001\009\001\020\001\011\001\015\001\016\001\
\017\001\038\001\039\001\004\001\038\001\039\001\001\001\002\001\
\003\001\004\001\005\001\128\000\021\001\008\001\141\000\034\001\
\035\001\034\001\037\001\036\001\015\001\016\001\017\001\007\001\
\009\001\009\001\011\001\011\001\006\001\007\001\022\001\009\001\
\010\001\011\001\004\001\013\001\004\001\021\001\014\001\034\001\
\018\001\036\001\008\001\021\001\022\001\004\001\008\001\025\001\
\008\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\006\001\007\001\
\005\001\009\001\010\001\011\001\004\001\005\001\007\001\007\001\
\008\001\004\001\018\001\004\001\005\001\021\001\004\001\008\001\
\004\001\025\001\004\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\006\001\007\001\034\001\009\001\036\001\011\001\008\001\013\001\
\008\001\034\001\021\001\036\001\011\001\014\001\004\001\021\001\
\022\001\009\001\009\001\025\001\009\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\006\001\007\001\009\001\009\001\009\001\011\001\
\009\001\013\001\027\001\028\001\029\001\030\001\031\001\032\001\
\004\001\021\001\022\001\011\001\004\001\025\001\012\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\006\001\007\001\018\001\009\001\
\021\001\011\001\009\001\013\001\084\000\085\000\086\000\087\000\
\007\001\007\001\000\000\021\001\022\001\021\001\009\001\025\001\
\005\000\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\009\000\049\000\036\001\019\000\038\001\039\001\006\001\007\001\
\103\000\009\001\255\255\011\001\255\255\013\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\009\001\255\255\
\255\255\025\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\033\001\255\255\255\255\036\001\255\255\038\001\039\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\006\001\007\001\255\255\
\009\001\255\255\011\001\255\255\013\001\255\255\006\001\007\001\
\255\255\009\001\255\255\011\001\021\001\013\001\255\255\255\255\
\025\001\255\255\255\255\255\255\255\255\021\001\255\255\255\255\
\033\001\025\001\255\255\036\001\255\255\006\001\007\001\255\255\
\009\001\033\001\011\001\255\255\036\001\255\255\006\001\007\001\
\255\255\009\001\255\255\011\001\021\001\255\255\255\255\255\255\
\025\001\255\255\255\255\255\255\255\255\021\001\255\255\255\255\
\033\001\025\001\255\255\036\001\255\255\255\255\255\255\255\255\
\255\255\033\001\255\255\255\255\036\001"

let yynames_const = "\
  CALL_OF\000\
  RETURN_OF\000\
  CALLORRETURN_OF\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  LBRACELBRACE\000\
  RBRACERBRACE\000\
  RARROW\000\
  TRUE\000\
  FALSE\000\
  NOT\000\
  DOT\000\
  AMP\000\
  COLON\000\
  SEMI_COLON\000\
  COMMA\000\
  PIPE\000\
  CARET\000\
  QUESTION\000\
  COLUMNCOLUMN\000\
  EQ\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  NEQ\000\
  PLUS\000\
  MINUS\000\
  SLASH\000\
  STAR\000\
  PERCENT\000\
  OR\000\
  AND\000\
  OTHERWISE\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'states) in
    Obj.repr(
# 112 "src/aorai/yaparser.mly"
                   (
  List.iter
    (fun(key, ids) ->
       match key with
           "init"   ->
             List.iter
               (fun id -> 
                 try
	           (Hashtbl.find observed_states id).init <- True
                 with
	             Not_found ->
                       Aorai_option.abort "Error: no state '%s'\n" id)
               ids
         | "accept" ->
             List.iter
               (fun id -> try
	          (Hashtbl.find observed_states id).acceptation <- True
                with Not_found ->
                  Aorai_option.abort "no state '%s'\n" id) ids
         | "deterministic" -> Aorai_option.Deterministic.set true;
         | oth      -> Aorai_option.abort "unknown option '%s'\n" oth
    ) _1;
    let states=
      Hashtbl.fold
        (fun _ st l ->
	   if st.acceptation=Undefined or st.init=Undefined then
	     begin
	       Aorai_option.abort
                 "Error: the state '%s' is used but never defined.\n" st.name
	     end;
	   st::l)
        observed_states []
    in
    (try
       Hashtbl.iter 
         (fun _ st -> if st.init=True then raise Exit) observed_states;
       Aorai_option.abort "Automaton does not declare an initial state"
     with Exit -> ());
    if Hashtbl.length prefetched_states >0 then 
      begin
	let r = Hashtbl.fold
	  (fun s n _ -> 
            s^"Error: the state '"^n^"' is used but never defined.\n")
	  prefetched_states 
	  ""
	in
	Aorai_option.abort "%s" r
      end;
    (states, _2)
  )
# 456 "src/aorai/yaparser.ml"
               : Promelaast.parsed_automaton))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 166 "src/aorai/yaparser.mly"
                   ( _1@[_2] )
# 464 "src/aorai/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 167 "src/aorai/yaparser.mly"
                   ( [_1] )
# 471 "src/aorai/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_identifiers) in
    Obj.repr(
# 171 "src/aorai/yaparser.mly"
                                                  ( (_2, _3) )
# 479 "src/aorai/yaparser.ml"
               : 'option))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "src/aorai/yaparser.mly"
                ( [] )
# 485 "src/aorai/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'id_list) in
    Obj.repr(
# 176 "src/aorai/yaparser.mly"
                  ( _2 )
# 492 "src/aorai/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'id_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 180 "src/aorai/yaparser.mly"
                             ( _1@[_3] )
# 500 "src/aorai/yaparser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "src/aorai/yaparser.mly"
                             ( [_1] )
# 507 "src/aorai/yaparser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'states) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 185 "src/aorai/yaparser.mly"
                 ( _1@_2 )
# 515 "src/aorai/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 186 "src/aorai/yaparser.mly"
          ( _1 )
# 522 "src/aorai/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 190 "src/aorai/yaparser.mly"
                                            (
      let start_state = fetch_and_create_state _1 in
      let (_, transitions) =
        List.fold_left
          (fun (otherwise, transitions) (cross,stop_state) ->
            if otherwise then
              Aorai_option.abort
                "'other' directive in definition of %s \
                transitions is not the last one" start_state.name
            else begin
              let trans =
                { start=start_state; stop=stop_state;
	          cross=cross;       numt=(-1) }::transitions
              in
              let otherwise = 
                match cross with 
                  | Otherwise -> true 
                  | Seq _ -> false
              in otherwise, trans
            end)
          (false,[]) _3
      in
      List.rev transitions
  )
# 553 "src/aorai/yaparser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 216 "src/aorai/yaparser.mly"
                                ( _1@[_3] )
# 561 "src/aorai/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 217 "src/aorai/yaparser.mly"
               ( [_1] )
# 568 "src/aorai/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'seq_elt) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 223 "src/aorai/yaparser.mly"
      ( (Seq _2, prefetch_and_create_state _5) )
# 576 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 224 "src/aorai/yaparser.mly"
                                ((Otherwise, prefetch_and_create_state _3) )
# 583 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 225 "src/aorai/yaparser.mly"
                      ( (Seq (to_seq PTrue), prefetch_and_create_state _2) )
# 590 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq_elt) in
    Obj.repr(
# 229 "src/aorai/yaparser.mly"
            ( _1 )
# 597 "src/aorai/yaparser.ml"
               : 'non_empty_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'seq_elt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 230 "src/aorai/yaparser.mly"
                           ( _1 @ _3 )
# 605 "src/aorai/yaparser.ml"
               : 'non_empty_seq))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "src/aorai/yaparser.mly"
                  ( [] )
# 611 "src/aorai/yaparser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_seq) in
    Obj.repr(
# 235 "src/aorai/yaparser.mly"
                  ( _1 )
# 618 "src/aorai/yaparser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_cond) in
    Obj.repr(
# 239 "src/aorai/yaparser.mly"
                ( to_seq _1 )
# 625 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'non_empty_seq) in
    Obj.repr(
# 240 "src/aorai/yaparser.mly"
                                  ( _2 )
# 632 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'pre_cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'seq) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'post_cond) in
    Obj.repr(
# 242 "src/aorai/yaparser.mly"
      ( let pre_cond = 
          match _2 with
            | Behavior b -> PCall(_1,Some b)
            | Pre c -> PAnd (PCall(_1,None), c)
        in
        let post_cond = 
          match _6 with
            | None -> PReturn _1
            | Some c -> PAnd (PReturn _1,c)
        in
        (to_seq pre_cond) @ _4 @ to_seq post_cond 
      )
# 653 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'non_empty_seq) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'post_cond) in
    Obj.repr(
# 255 "src/aorai/yaparser.mly"
      ( let post_cond = 
          match _5 with
            | None -> PReturn _1
            | Some c -> PAnd (PReturn _1,c)
        in
        (to_seq (PCall (_1, None))) @ _3 @ to_seq post_cond 
      )
# 668 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'post_cond) in
    Obj.repr(
# 263 "src/aorai/yaparser.mly"
      ( let post_cond = 
          match _4 with
            | None -> PReturn _1
            | Some c -> PAnd (PReturn _1,c)
        in
        (to_seq (PCall (_1, None))) @ to_seq post_cond
      )
# 682 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 273 "src/aorai/yaparser.mly"
                            ( Behavior _2 )
# 689 "src/aorai/yaparser.ml"
               : 'pre_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'single_cond) in
    Obj.repr(
# 274 "src/aorai/yaparser.mly"
                                          ( Pre _2 )
# 696 "src/aorai/yaparser.ml"
               : 'pre_cond))
; (fun __caml_parser_env ->
    Obj.repr(
# 278 "src/aorai/yaparser.mly"
                  ( None )
# 702 "src/aorai/yaparser.ml"
               : 'post_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'single_cond) in
    Obj.repr(
# 279 "src/aorai/yaparser.mly"
                                          ( Some _2 )
# 709 "src/aorai/yaparser.ml"
               : 'post_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'repetition) in
    Obj.repr(
# 283 "src/aorai/yaparser.mly"
                     (
    let min, max = _2 in
    match _1 with
      | [ s ] when Data_for_aorai.is_single s ->
        [ { s with min_rep = min; max_rep = max } ]
      | l ->
        if is_no_repet (min,max) then
          l (* [ a; [b;c]; d] is equivalent to [a;b;c;d] *)
        else [ { condition = None; nested = l; min_rep = min; max_rep = max } ] 
  )
# 726 "src/aorai/yaparser.ml"
               : 'seq_elt))
; (fun __caml_parser_env ->
    Obj.repr(
# 297 "src/aorai/yaparser.mly"
      ( Some Data_for_aorai.cst_one, Some Data_for_aorai.cst_one )
# 732 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    Obj.repr(
# 298 "src/aorai/yaparser.mly"
         ( Some Data_for_aorai.cst_one, None)
# 738 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    Obj.repr(
# 299 "src/aorai/yaparser.mly"
         ( None, None )
# 744 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    Obj.repr(
# 300 "src/aorai/yaparser.mly"
             ( None, Some Data_for_aorai.cst_one )
# 750 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'arith_relation) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 301 "src/aorai/yaparser.mly"
                                                      ( Some _2, Some _4 )
# 758 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 302 "src/aorai/yaparser.mly"
                                 ( Some _2, Some _2 )
# 765 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    Obj.repr(
# 303 "src/aorai/yaparser.mly"
                                       ( Some _2, None )
# 772 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 304 "src/aorai/yaparser.mly"
                                       ( None, Some _3 )
# 779 "src/aorai/yaparser.ml"
               : 'repetition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 308 "src/aorai/yaparser.mly"
      ( POr (PCall (_3,None), PReturn _3) )
# 786 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 309 "src/aorai/yaparser.mly"
                                      ( PCall (_3,None) )
# 793 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 310 "src/aorai/yaparser.mly"
                                        ( PReturn _3 )
# 800 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    Obj.repr(
# 311 "src/aorai/yaparser.mly"
         ( PTrue )
# 806 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    Obj.repr(
# 312 "src/aorai/yaparser.mly"
          ( PFalse )
# 812 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'single_cond) in
    Obj.repr(
# 313 "src/aorai/yaparser.mly"
                    ( PNot _2 )
# 819 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'single_cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_cond) in
    Obj.repr(
# 314 "src/aorai/yaparser.mly"
                                ( PAnd (_1,_3) )
# 827 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'single_cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_cond) in
    Obj.repr(
# 315 "src/aorai/yaparser.mly"
                               ( POr (_1,_3) )
# 835 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'single_cond) in
    Obj.repr(
# 316 "src/aorai/yaparser.mly"
                              ( _2 )
# 842 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 317 "src/aorai/yaparser.mly"
                   ( _1 )
# 849 "src/aorai/yaparser.ml"
               : 'single_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 321 "src/aorai/yaparser.mly"
                                     ( PRel(Eq, _1, _3) )
# 857 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 322 "src/aorai/yaparser.mly"
                                     ( PRel(Lt, _1, _3) )
# 865 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 323 "src/aorai/yaparser.mly"
                                     ( PRel(Gt, _1, _3) )
# 873 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 324 "src/aorai/yaparser.mly"
                                     ( PRel(Le, _1, _3) )
# 881 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 325 "src/aorai/yaparser.mly"
                                     ( PRel(Ge, _1, _3) )
# 889 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 326 "src/aorai/yaparser.mly"
                                      ( PRel(Neq, _1, _3) )
# 897 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 327 "src/aorai/yaparser.mly"
                              ( PRel (Neq, _1, PCst(IntConstant "0")) )
# 904 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 331 "src/aorai/yaparser.mly"
                                           ( PBinop(Badd,_1,_3) )
# 912 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 332 "src/aorai/yaparser.mly"
                                            ( PBinop(Bsub,_1,_3) )
# 920 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 333 "src/aorai/yaparser.mly"
                                    ( _1 )
# 927 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 337 "src/aorai/yaparser.mly"
                                             ( PBinop(Bdiv,_1,_3) )
# 935 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 338 "src/aorai/yaparser.mly"
                                            ( PBinop(Bmul, _1, _3) )
# 943 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 339 "src/aorai/yaparser.mly"
                                               ( PBinop(Bmod, _1, _3) )
# 951 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 340 "src/aorai/yaparser.mly"
                    ( _1 )
# 958 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 345 "src/aorai/yaparser.mly"
        ( PCst (IntConstant _1) )
# 965 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 346 "src/aorai/yaparser.mly"
              ( PUnop (Uminus, PCst (IntConstant _2)) )
# 972 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 347 "src/aorai/yaparser.mly"
                      ( _1 )
# 979 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 348 "src/aorai/yaparser.mly"
                                 ( _2 )
# 986 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 353 "src/aorai/yaparser.mly"
                          ( PField(_1,_3) )
# 994 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 354 "src/aorai/yaparser.mly"
                                           ( PArrget(_1,_3) )
# 1002 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 355 "src/aorai/yaparser.mly"
                    (_1)
# 1009 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 359 "src/aorai/yaparser.mly"
                ( PUnop (Ustar,_2) )
# 1016 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 360 "src/aorai/yaparser.mly"
                                            ( PPrm(_1,_5) )
# 1024 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 361 "src/aorai/yaparser.mly"
               ( PVar _1 )
# 1031 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 362 "src/aorai/yaparser.mly"
                         ( _2 )
# 1038 "src/aorai/yaparser.ml"
               : 'access_leaf))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Promelaast.parsed_automaton)

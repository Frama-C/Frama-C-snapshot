(*
class located :
  loc:Loc.position option -> object method loc : Loc.position end
class typed : typ:Jc_env.jc_type -> object method typ : Jc_env.jc_type end
class logic_labeled :
  logic_label:Jc_env.logic_label option ->
  object
    val mutable llab : Jc_env.logic_label option
    method logic_label : Jc_env.logic_label option
    method set_logic_label : Jc_env.logic_label option -> unit
  end
class name_labeled :
  name_label:string -> object method name_label : string end
class regioned :
  region:Jc_env.region ->
  object
    val mutable r : Jc_env.region
    method region : Jc_env.region
    method set_region : Jc_env.region -> unit
  end
*)


class identifier :
  ?loc:Loc.position ->
  string -> object method loc : Loc.position method name : string end

class ['a] node_located :
  ?loc:Loc.position ->
  'a -> object method loc : Loc.position method node : 'a end

class ptype :
  ?loc:Loc.position ->
  Jc_ast.ptype_node ->
  object method loc : Loc.position method node : Jc_ast.ptype_node end

class pexpr :
  ?loc:Loc.position ->
  Jc_ast.pexpr_node ->
  object method loc : Loc.position method node : Jc_ast.pexpr_node end

class pexpr_with :
  ?loc:Loc.position ->
  ?node:Jc_ast.pexpr_node ->
  < loc : Loc.position; node : Jc_ast.pexpr_node; .. > -> pexpr

class nexpr :
  ?loc:Loc.position ->
  ?logic_label:Jc_env.logic_label ->
  Jc_ast.nexpr_node ->
  object
    val mutable llab : Jc_env.logic_label option
    method loc : Loc.position
    method logic_label : Jc_env.logic_label option
    method node : Jc_ast.nexpr_node
    method set_logic_label : Jc_env.logic_label option -> unit
  end

(*
class nexpr_with :
  ?loc:Loc.position ->
  ?node:Jc_ast.nexpr_node ->
  < loc : Loc.position; logic_label : Jc_env.logic_label;
    node : Jc_ast.nexpr_node; .. > ->
  nexpr
*)

class pattern :
  ?loc:Loc.position ->
  typ:Jc_env.jc_type ->
  Jc_ast.pattern_node ->
  object
    method loc : Loc.position
    method node : Jc_ast.pattern_node
    method typ : Jc_env.jc_type
  end

(*
class pattern_with :
  ?loc:Loc.position ->
  ?node:Jc_ast.pattern_node ->
  ?typ:Jc_env.jc_type ->
  < loc : Loc.position; node : Jc_ast.pattern_node; typ : Jc_env.jc_type;
    .. > ->
  pattern
*)


class term :
  ?loc:Loc.position ->
  typ:Jc_env.jc_type ->
  ?name_label:string ->
  ?region:Jc_env.region ->
  Jc_ast.term_node ->
  object
    val mutable r : Jc_env.region
    method loc : Loc.position
    method name_label : string
    method node : Jc_ast.term_node
    method region : Jc_env.region
    method set_region : Jc_env.region -> unit
    method typ : Jc_env.jc_type
  end

class term_with :
  ?loc:Loc.position ->
  ?typ:Jc_env.jc_type ->
  ?name_label:string ->
  ?region:Jc_env.region ->
  ?node:Jc_ast.term_node ->
  < loc : Loc.position; name_label : string; node : Jc_ast.term_node;
    region : Jc_env.region; typ : Jc_env.jc_type; .. > ->
  term

class term_var :
  ?loc:Loc.position -> ?name_label:string -> Jc_env.var_info -> term

class expr :
  ?loc:Loc.position ->
  typ:Jc_env.jc_type ->
  ?name_label:string ->
  ?region:Jc_env.region ->
  ?original_type:Jc_env.jc_type ->
  Jc_ast.expr_node ->
  object
    val mutable r : Jc_env.region
    method loc : Loc.position
    method name_label : string
    method node : Jc_ast.expr_node
    method original_type : Jc_env.jc_type
    method region : Jc_env.region
    method set_region : Jc_env.region -> unit
    method typ : Jc_env.jc_type
  end
class expr_with :
  ?loc:Loc.position ->
  ?typ:Jc_env.jc_type ->
  ?name_label:string ->
  ?region:Jc_env.region ->
  ?node:Jc_ast.expr_node ->
  ?original_type:Jc_env.jc_type ->
  < loc : Loc.position; name_label : string; node : Jc_ast.expr_node;
    original_type : Jc_env.jc_type; region : Jc_env.region;
    typ : Jc_env.jc_type; .. > ->
  expr


class assertion :
  ?name_label:string ->
  ?loc:Loc.position ->
  Jc_ast.assertion_node ->
  object
    method loc : Loc.position
    method name_label : string
    method node : Jc_ast.assertion_node
  end


class assertion_with :
  ?loc:Loc.position ->
  ?name_label:string ->
  ?node:Jc_ast.assertion_node ->
  < loc : Loc.position; name_label : string; node : Jc_ast.assertion_node;
    .. > ->
  assertion

class ['a] ptag :
  ?loc:Loc.position ->
  'a Jc_ast.ptag_node ->
  object method loc : Loc.position method node : 'a Jc_ast.ptag_node end

(*
class ['a] ptag_with :
  ?loc:Loc.position ->
  ?node:'a Jc_ast.ptag_node ->
  < loc : Loc.position; node : 'a Jc_ast.ptag_node; .. > -> ['a] ptag

*)


class tag :
  ?loc:Loc.position ->
  Jc_ast.tag_node ->
  object method loc : Loc.position method node : Jc_ast.tag_node end

(*
class tag_with :
  ?loc:Loc.position ->
  ?node:Jc_ast.tag_node ->
  < loc : Loc.position; node : Jc_ast.tag_node; .. > -> tag
*)

class ['a] decl :
  ?loc:Loc.position ->
  'a Jc_ast.decl_node ->
  object method loc : Loc.position method node : 'a Jc_ast.decl_node end

(*

class ['a] decl_with :
  ?loc:Loc.position ->
  ?node:'a Jc_ast.decl_node ->
  < loc : Loc.position; node : 'a Jc_ast.decl_node; .. > -> ['a] decl
module Const :
  sig
    val mkvoid : Jc_ast.const
    val mknull : Jc_ast.const
    val mkboolean : bool -> Jc_ast.const
    val mkint : ?value:int -> ?valuestr:string -> unit -> Jc_ast.const
    val mkreal : ?value:float -> ?valuestr:string -> unit -> Jc_ast.const
  end
val oo : 'a option -> 'a -> 'a
*)

module PExpr :
  sig
(*
    val mk : ?loc:Loc.position -> node:Jc_ast.pexpr_node -> unit -> pexpr
*)
    val mkconst : const:Jc_ast.const -> ?loc:Loc.position -> unit -> pexpr

    val mkvoid : ?loc:Loc.position -> unit -> pexpr

    val mknull : ?loc:Loc.position -> unit -> pexpr

    val mkboolean : value:bool -> ?loc:Loc.position -> unit -> pexpr

    val mkint :
      ?value:int -> ?valuestr:string -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkreal :
      ?value:float -> ?valuestr:string -> ?loc:Loc.position -> unit -> pexpr
*)
    val mkbinary :
      expr1:Jc_ast.pexpr ->
      op:Jc_ast.bin_op ->
      expr2:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
(*
    val mkbinary_list :
      default:Jc_ast.pexpr ->
      op:Jc_ast.bin_op ->
      ?expr1:Jc_ast.pexpr ->
      ?expr2:Jc_ast.pexpr ->
      ?list:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> Jc_ast.pexpr
*)
    val mkand :
      ?expr1:Jc_ast.pexpr ->
      ?expr2:Jc_ast.pexpr ->
      ?list:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> Jc_ast.pexpr

    val mkor :
      ?expr1:Jc_ast.pexpr ->
      ?expr2:Jc_ast.pexpr ->
      ?list:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> Jc_ast.pexpr

    val mkadd :
      ?expr1:Jc_ast.pexpr ->
      ?expr2:Jc_ast.pexpr ->
      ?list:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> Jc_ast.pexpr

    val mklabel :
      label:string -> expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkvar : name:string -> ?loc:Loc.position -> unit -> pexpr

    val mkderef :
      expr:Jc_ast.pexpr -> field:string -> ?loc:Loc.position -> unit -> pexpr

    val mkunary :
      expr:Jc_ast.pexpr ->
      op:Jc_ast.pexpr_unary_op -> ?loc:Loc.position -> unit -> pexpr

    val mkapp :
      fun_name:string ->
      ?labels:Jc_env.logic_label list ->
      ?args:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> pexpr

    val mkassign :
      location:Jc_ast.pexpr ->
      value:Jc_ast.pexpr ->
      ?field:string ->
      ?op:Jc_ast.bin_op -> ?loc:Loc.position -> unit -> pexpr

    val mkinstanceof :
      expr:Jc_ast.pexpr -> typ:string -> ?loc:Loc.position -> unit -> pexpr

    val mkcast :
      expr:Jc_ast.pexpr -> typ:string -> ?loc:Loc.position -> unit -> pexpr

    val mkquantifier :
      quantifier:Jc_ast.quantifier ->
      typ:Jc_ast.ptype ->
      vars:string list ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkforall :
      typ:Jc_ast.ptype ->
      vars:string list ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkexists :
      typ:Jc_ast.ptype ->
      vars:string list ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
    val mkold : expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
*)

    val mkat :
      expr:Jc_ast.pexpr ->
      label:Jc_env.logic_label -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkoffset :
      kind:Jc_ast.offset_kind ->
      expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
    val mkoffset_min :
      expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
*)

    val mkoffset_max :
      expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkif :
      condition:Jc_ast.pexpr ->
      expr_then:Jc_ast.pexpr ->
      ?expr_else:pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkblock :
      exprs:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkdecl :
      typ:Jc_ast.ptype ->
      var:string -> ?init:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
*)
    val mklet :
      ?typ:Jc_ast.ptype ->
      var:string ->
      ?init:Jc_ast.pexpr ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mklet_nodecl :
      ?typ:Jc_ast.ptype ->
      var:string ->
      ?init:Jc_ast.pexpr ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkrange :
      ?left:Jc_ast.pexpr ->
      ?right:Jc_ast.pexpr ->
      ?locations:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkalloc :
      ?count:pexpr -> typ:string -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkfree : expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr
    val mkmutable :
      expr:Jc_ast.pexpr ->
      tag:Jc_ast.pexpr Jc_ast.ptag -> ?loc:Loc.position -> unit -> pexpr
    val mktag_equality :
      tag1:Jc_ast.pexpr Jc_ast.ptag ->
      tag2:Jc_ast.pexpr Jc_ast.ptag -> ?loc:Loc.position -> unit -> pexpr
    val mkmatch :
      expr:Jc_ast.pexpr ->
      cases:(Jc_ast.ppattern * Jc_ast.pexpr) list ->
      ?loc:Loc.position -> unit -> pexpr
*)

    val mkassert : expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkwhile :
      ?condition:pexpr ->
      ?invariant:(string list * Jc_ast.pexpr) list ->
      ?variant:Jc_ast.pexpr ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkfor :
      ?inits:Jc_ast.pexpr list ->
      ?condition:pexpr ->
      ?updates:Jc_ast.pexpr list ->
      ?invariant:pexpr ->
      ?variant:Jc_ast.pexpr ->
      body:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkreturn : ?expr:pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkbreak : ?label:string -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkcontinue : ?label:string -> ?loc:Loc.position -> unit -> pexpr
    val mkgoto : label:string -> ?loc:Loc.position -> unit -> pexpr

*)

    val mktry :
      expr:Jc_ast.pexpr ->
      ?catches:(Jc_ast.identifier * string * Jc_ast.pexpr) list ->
      ?finally:pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkthrow :
      exn:Jc_ast.identifier ->
      ?argument:pexpr -> ?loc:Loc.position -> unit -> pexpr

(*
    val mkpack :
      expr:Jc_ast.pexpr ->
      ?tag:Jc_ast.identifier -> ?loc:Loc.position -> unit -> pexpr
    val mkunpack :
      expr:Jc_ast.pexpr ->
      ?tag:Jc_ast.identifier -> ?loc:Loc.position -> unit -> pexpr
*)

    val mkswitch :
      expr:Jc_ast.pexpr ->
      ?cases:(Jc_ast.pexpr option list * Jc_ast.pexpr) list ->
      ?loc:Loc.position -> unit -> pexpr

    val mkcatch :
      exn:'a ->
      ?name:string ->
      ?body:pexpr -> ?loc:Loc.position -> unit -> 'a * string * pexpr

    val mkshift :
      expr:Jc_ast.pexpr ->
      offset:Jc_ast.pexpr ->
      ?list:Jc_ast.pexpr list -> ?loc:Loc.position -> unit -> Jc_ast.pexpr

    val mknot : expr:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkeq :
      expr1:Jc_ast.pexpr ->
      expr2:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkimplies :
      expr1:Jc_ast.pexpr ->
      expr2:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkiff :
      expr1:Jc_ast.pexpr ->
      expr2:Jc_ast.pexpr -> ?loc:Loc.position -> unit -> pexpr

    val mkincr_heap :
      expr:Jc_ast.pexpr ->
      field:string ->
      ?op:Jc_ast.pexpr_unary_op -> ?loc:Loc.position -> unit -> pexpr

  end


module PDecl :
  sig
(*
    val mk : ?loc:Loc.position -> node:'a -> unit -> 'a node_located
*)

    val mkfun_def :
      ?result_type:ptype ->
      name:Jc_ast.identifier ->
      ?params:(Jc_ast.ptype * string) list ->
      ?clauses:'a Jc_ast.clause list ->
      ?body:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mklemma_def :
      name:string ->
      ?axiom:bool ->
      ?labels:Jc_env.logic_label list ->
      body:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mklogic_var_def :
      typ:Jc_ast.ptype ->
      name:string ->
      ?body:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mklogic_def :
      ?typ:Jc_ast.ptype ->
      name:string ->
      ?labels:Jc_env.logic_label list ->
      ?params:(Jc_ast.ptype * string) list ->
      ?reads:'a list ->
      ?body:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mklogic_type :
      name:string ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkvar_def :
      typ:Jc_ast.ptype ->
      name:string ->
      ?init:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkglobal_inv_def :
      name:string ->
      body:'a ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mktag_def :
      name:string ->
      ?params:string list ->
      ?super:string * Jc_ast.ptype list ->
      ?fields:(bool * Jc_ast.ptype * string) list ->
      ?invariants:(Jc_ast.identifier * string * 'a) list ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkenum_type_def :
      name:string ->
      left:Num.num ->
      right:Num.num ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkexception_def :
      name:string ->
      ?arg_type:Jc_ast.ptype ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkvariant_type_def :
      name:string ->
      ?tags:Jc_ast.identifier list ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkinvariant_policy_def :
      value:Jc_env.inv_sem ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkseparation_policy_def :
      value:Jc_env.separation_sem ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkannotation_policy_def :
      value:Jc_env.annotation_sem ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

    val mkabstract_domain_def :
      value:Jc_env.abstract_domain ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

(*

    val mkint_model_def :
      value:Jc_env.int_model ->
      ?loc:Loc.position -> unit -> 'a Jc_ast.decl_node node_located

*)

    val mkrequires : 'a -> 'a Jc_ast.clause

    val mkbehavior :
      ?loc:Loc.position ->
      name:string ->
      ?throws:Jc_ast.identifier ->
      ?assumes:pexpr ->
      ?requires:pexpr ->
      ?assigns:Loc.position * pexpr list ->
      ?ensures:pexpr -> unit -> pexpr Jc_ast.clause

(*
    val mkbehavior_with :
      ?loc:Loc.position ->
      ?name:string ->
      ?throws:Jc_ast.identifier option ->
      ?assumes:'a option ->
      ?requires:'a option ->
      ?assigns:(Loc.position * 'a list) option ->
      ?ensures:'a -> 'a Jc_ast.clause -> 'a Jc_ast.clause
*)

    val mkassigns :
      ?loc:Loc.position ->
      ?locations:'a list -> unit -> Loc.position * 'a list

(*
    val mktag_invariant : name:'a -> var:'b -> body:'c -> 'a * 'b * 'c
    val behavior_ensures : 'a Jc_ast.clause -> 'a
*)
  end


module Expr :
  sig
    val mk :
      ?loc:Loc.position ->
      typ:Jc_env.jc_type ->
      ?name_label:string ->
      ?region:Jc_env.region ->
      ?original_type:Jc_env.jc_type -> node:Jc_ast.expr_node -> unit -> expr
    val mklet :
      var:Jc_env.var_info ->
      ?init:Jc_ast.expr ->
      body:Jc_ast.expr ->
      ?loc:Loc.position ->
      ?name_label:string ->
      ?region:Jc_env.region -> ?original_type:Jc_env.jc_type -> unit -> expr
    val mkvar :
      var:Jc_env.var_info ->
      ?loc:Loc.position ->
      ?name_label:string ->
      ?region:Jc_env.region -> ?original_type:Jc_env.jc_type -> unit -> expr
    val is_app : < node : Jc_ast.expr_node; .. > -> bool
  end



module Term :
  sig
(*
    val mk :
      ?loc:Loc.position ->
      typ:Jc_env.jc_type ->
      ?name_label:string ->
      ?region:Jc_env.region -> node:Jc_ast.term_node -> unit -> term
*)

    val mkvar :
      var:Jc_env.var_info ->
      ?loc:Loc.position ->
      ?name_label:string -> ?region:Jc_env.region -> unit -> term
  end

module Assertion :
  sig
(*
    val mk :
      ?loc:Loc.position ->
      ?name_label:string -> node:Jc_ast.assertion_node -> unit -> assertion
    val fake : ?loc:'a -> ?name_label:'b -> value:'c -> unit -> 'c
*)

    val mktrue : ?loc:Loc.position -> ?name_label:string -> unit -> assertion

(*
    val mkfalse :
      ?loc:Loc.position -> ?name_label:string -> unit -> assertion
*)

    val mkand :
      conjuncts:Jc_ast.assertion list ->
      ?loc:Loc.position -> ?name_label:string -> unit -> assertion

  end

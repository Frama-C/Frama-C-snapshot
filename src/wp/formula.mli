(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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


open Cil_types
open Ctypes


(* -------------------------------------------------------------------------- *)
(** Logic Formulae                                                            *)
(* -------------------------------------------------------------------------- *)

(** {2 Logic types }*)

type tau =
  | Integer
  | Real
  | Boolean
  | Pointer of tau
  | Set     of tau
  | Record  of Cil_types.compinfo
  | Array   of Ctypes.arrayinfo
  | ADT     of string * (tau list)

type kind =
  | Model of tau
  | Acsl of tau * Cil_types.logic_type



(** {2 Pure types} *)

type m_boolean  (** The set of two elements [{true,false}]. *)
type m_integer  (** Natural signed integers ({b Z}). *)
type m_real     (** Real numbers ({b R}). *)
type m_abstract (** Abstract Data Types (user-defined WHY-types). *)

type m_name    (** type name [data_lib.why] *)

type m_pointer (** type pointer [data_lib.why] *)
type m_array   (** type 'a farray [array.why] *)
type m_record  (** type record [data_lib.why] *)
type m_urecord (** type urecord [data_lib.why] *)
type m_set     (** type 'a set [data_lib.why] *)
type m_zone    (** type of elementary regions *)



(** {2 Arithmetics Operators} *)

type int_op = Iadd | Isub | Imul | Idiv | Imod
type real_op = Radd | Rsub | Rmul | Rdiv
type cmp_op = Ceq | Cneq | Clt | Cleq

(** {2 Declarations} *)

type section =
  | S_Type         (** LOGIC type definition *)
  | S_Cons         (** LOGIC type constructor *)
  | S_Logic_Sig    (** Signature of Functions and Predicates *)
  | S_Logic_Def    (** Definitions for Functions and Predicates *)
  | S_Logic_Prop   (** Axioms on Functions and Predicates *)
  | S_Model_Sig    (** Signature of Functions and Predicates *)
  | S_Model_Def    (** Definitions for Functions and Predicates *)
  | S_Model_Prop   (** Axioms on Functions and Predicates *)
  | S_User_Sig     (** Signature of User-defined function and predicates *)
  | S_User_Prop    (** Axioms on User-defined function and predicates *)

type ('x,'t,'p) item =
  | Type of int
  | Cons of int
  | Function of tau list * tau
  | Predicate of tau list
  | FunctionDef of 'x list * tau * 't
  | PredicateDef of 'x list * 'p
  | Axiom of 'p
  | Trecord of compinfo

type ('x,'t,'p) declaration =
    {
      d_section : section ;
      d_name    : string ;
      d_title   : (Format.formatter -> unit) ;
      d_descr   : (Format.formatter -> unit) ;
      d_source  : Lexing.position option ;
      d_item    : ('x,'t,'p) item ;
    }

(** {2 Signature for logic formulae} *)

module type S =
sig

  type var
  type 'a term
  type pred

 
  type abstract = m_abstract term
  type integer = m_integer term
  type real = m_real term
  type boolean = m_boolean term
  type record =  m_record term
  type urecord = m_array term
  type array =  m_array term
  type set = m_set term
  type name = m_integer term

  type decl = (var,abstract,pred) declaration

  val e_int  : int -> integer
  val e_call : string -> abstract list -> abstract
  val p_call : string -> abstract list -> pred
  val wrap   : 'a term -> abstract
  val unwrap : abstract -> 'a term

  (** {2 Global Declarations} *)

  val clear : unit -> unit
  val on_clear : (unit -> unit) -> unit
  val fresh_name : string -> string -> string
  val add_declaration : decl -> unit
  val has_declaration : string -> bool
  val iter_all : (string -> unit) -> (decl -> unit) -> unit

  (** {2 Functors and Types for Declaration} *)

  module type Identifiable =
  sig
    type t
    module H : Hashtbl.S
    val index : t -> H.key
    val prefix : string
    val basename : t -> string
    val location : t -> Lexing.position option
    val pp_title : Format.formatter -> t -> unit
    val pp_descr : Format.formatter -> t -> unit
  end

  module type Registry =
  sig
    type t
    val define : t -> unit
    val get_definition : t -> decl
    val on_definition : (t -> decl -> unit) -> unit
  end

  module type Declarator =
  sig
    include Identifiable
    val clear : unit -> unit
    val section : section
    val declare : t -> string -> (var,abstract,pred) item
  end

  module DRegister
    (D : Declarator) :
    (Registry with type t = D.t)

  (** {2 Build-int Identifiables and Registry} *)

  module Varinfo   : Identifiable with type t = varinfo
  module Varaddr   : Identifiable with type t = varinfo
  module Fieldinfo : Identifiable with type t = fieldinfo
  module Compinfo  : Identifiable with type t = compinfo
  module Arrayinfo : Identifiable with type t = arrayinfo
  module Logicvar  : Identifiable with type t = logic_var
  module LTypeinfo : Identifiable with type t = logic_type
  module Cobject   : Identifiable with type t = Ctypes.c_object
  module ArrayDim  : Identifiable with type t = Ctypes.c_object * int

  val adt_decl : Cil_types.logic_type_info -> string


    (**Be careful, this one is only used for debuging message!
       Do not use for extraction *)
  val pp_tau : Format.formatter -> tau -> unit

  (** {2 Terms} *)

  val e_true    : boolean
  val e_false   : boolean

  val e_float   : float -> real
  val e_icst    : string -> integer
  val e_rcst    : string -> real
  val e_int64   : int64 -> integer

  (** {2 Arithmetics} *)

  val e_ineg : integer -> integer
  val e_rneg : real -> real
  val e_iop  : int_op -> integer -> integer -> integer
  val e_rop  : real_op -> real -> real -> real
  val e_icmp : cmp_op -> integer -> integer -> boolean
  val e_rcmp : cmp_op -> real -> real -> boolean
  val p_icmp : cmp_op -> integer -> integer -> pred
  val p_rcmp : cmp_op -> real -> real -> pred

  val e_bnot   : integer -> integer
  val e_band   : integer -> integer -> integer
  val e_bor    : integer -> integer -> integer
  val e_bxor   : integer -> integer -> integer
  val e_lshift : integer -> integer -> integer
  val e_rshift : integer -> integer -> integer

  val integer_of_real : real -> integer
  val real_of_integer : integer -> real

  (** {2 Booleans} *)

  val e_bool : boolean -> integer
  val e_not  : boolean -> boolean
  val e_and  : boolean -> boolean -> boolean
  val e_or   : boolean -> boolean -> boolean

  (** {2 Conditional} *)

  val e_cond : boolean -> 'a term -> 'a term -> 'a term
  val p_cond : boolean -> pred -> pred -> pred

  (** {2 records field} *)

  val e_getfield: Cil_types.fieldinfo -> record -> abstract
  val e_setfield: Cil_types.fieldinfo -> record -> abstract -> record


  val e_access : array -> integer -> abstract
  val e_update : array -> integer -> abstract -> array

  (** {2 Predicates} *)

  val p_true    : pred
  val p_false   : pred
  val p_bool    : boolean -> pred
  val p_and     : pred -> pred -> pred
  val p_or      : pred -> pred -> pred
  val p_xor     : pred -> pred -> pred
  val p_not     : pred -> pred
  val p_implies : pred -> pred -> pred
  val p_iff     : pred ->pred -> pred
  val p_eq      : 'a term -> 'a term -> pred
  val p_neq     : 'a term -> 'a term -> pred
  val p_conj    : pred list -> pred
  val p_disj    : pred list -> pred
  val p_named   : string -> pred -> pred

  (** {2 Utilities} *)

  val is_true : pred -> bool
  val is_false : pred -> bool
  val huge_term : int -> 'a term -> bool
  val huge_pred : int -> pred -> bool

  (** {2 Variables}

        Pools are used to generate fresh free variables.  Do not mix
        non-closed terms from different pools.
  *)

  type pool

  val pool : unit -> pool
  val p_fresh : pool -> string -> kind ->  var
  val p_freshen : pool -> var -> var

  val var : var -> 'a term
  val eq_var : var -> var -> bool
  val name_of_var : var -> string
  val basename_of_var : var -> string
  val tau_of_var : var -> tau
  val kind_of_var : var  -> kind

  val term_has_var : var list -> 'a term -> bool (* any of vars *)
  val pred_has_var : var list -> pred -> bool    (* any of vars *)

  val term_calls : string -> 'a term -> bool
  val pred_calls : string -> pred -> bool

  val term_closed : 'a term -> bool
  val pred_closed : pred -> bool
  val freevars : pred -> var list

  val p_forall : var list -> pred -> pred
  val p_exists : var list -> pred -> pred
  val p_subst  : (var -> var option) -> var -> 'a term -> pred -> pred
  val e_subst  : (var -> var option) -> var -> 'a term -> 'b term -> 'b term

  (** Requires domain to be disjoint from co-domain *)
  val e_rename : (var * var) list -> 'a term -> 'a term

  (** Returns true when the two terms are syntactically equals *)
  val equal_terms : 'a term -> 'a term -> bool

  (** {3 Alpha conversion} *)

  (** Maping from old var to new var *)
  type alpha
  (** Empty mapping *)
  val empty_alpha : alpha
  val fold_alpha : (var -> var -> 'a -> 'a) -> alpha -> 'a -> 'a

  (** [alpha', p' = p_more_alpha_cv alpha p] build p' from p by renaming
  * all the variable v into v' according to the mapping alpha.
  * Add new mappings for the variables that are not already mapped. *)
  val p_more_alpha_cv : alpha -> pred -> alpha * pred

  (** easier to use when doing a simple alpha conversion.
  * @return the new predicate and the newly created variables. *)
  val p_alpha_cv : pred -> var list * pred


  (** {2 Pretty printers} *)

  val pp_var : Format.formatter -> var -> unit
  val pp_section : Format.formatter -> string -> unit
  val pp_term : Format.formatter -> 'a term -> unit
  val pp_pred : Format.formatter -> pred -> unit
  val pp_decl : Format.formatter -> decl -> unit
  val pp_goal : Format.formatter -> string -> pred -> unit
  val pp_vkind : Format.formatter -> kind -> unit


  (** {2 FOL Helpers} *)

  val e_app0 : string -> 'a term
  val e_app1 : string -> 'a term -> 'b term
  val e_app2 : string -> 'a term -> 'b term -> 'c term
  val e_app3 : string -> 'a term -> 'b term -> 'c term -> 'd term
  val e_app4 :
    string -> 'a term -> 'b term -> 'c term -> 'd term -> 'e term
  val e_app5 :
    string -> 'a term -> 'b term -> 'c term -> 'd term -> 'e term ->
    'f term

  val p_app0 : string -> pred
  val p_app1 : string -> 'a term -> pred
  val p_app2 : string -> 'a term -> 'b term -> pred
  val p_app3 : string -> 'a term -> 'b term -> 'c term -> pred
  val p_app4 : string ->
    'a term -> 'b term -> 'c term -> 'd term -> pred
  val p_app5 : string ->
    'a term -> 'b term -> 'c term -> 'd term -> 'e term -> pred

  val dummy : unit -> pred

  val i_zero : integer
  val r_zero : real
  val i_one  : integer
  val i_add  : integer -> integer -> integer
  val i_mult : integer -> integer -> integer
  val i_sub  : integer -> integer -> integer


  (** {2 Integer Logic Cast} *)

  val guard : Ctypes.c_int -> integer -> pred
  val modulo : Ctypes.c_int -> integer -> integer
  val i_convert: Ctypes.c_int -> Ctypes.c_int -> integer -> integer

 (**{2 Fol data } *)
  type interval = {
    inf : integer option ;
    sup : integer option ;
  }

  type 'a assigned =
    | Aloc of c_object * 'a
    | Arange of c_object * 'a * interval

   type havoc =
    | Fresh of var
    | Update of var * ((var * var) list  -> abstract)

  val pp_interval : Format.formatter -> interval -> unit

  (** {2 Sub-arrays} *)

  val set_range_index : array -> interval -> array

  (** {2 Set and Range as first Class Value} *)

  val empty : set
    (**[empty()] returns the polymorphic empty set. *)

  val singleton : abstract -> set
    (** [singleton a] returns the singleton set containning [a]. *)

  val union : set -> set -> set
    (** [union s0 s1] returns the union set of [s0] and [s1]. *)

  val unions : set list -> set

  val inter : set -> set -> set
    (**[inter s0 s1] returns the intersection set of [s0] and [s1]. *)

  val remove : set -> set -> set
    (** [remove s0 s1] returns a set [s'] such as \{b in s' | b in s0
        and !(b in s1) \}.*)

  val set_of_list : abstract list -> set

  val add_set : set -> set -> set

  val mult_set : set -> set -> set

  val neg_set : set -> set

  val interval : interval -> set
    (** [range l h] returns the integer set [s] such as \{ i in s | l <=
        i <= h \}. *)
    (** {2 Formats} *)

  (** {2 Record as First Class Value}*)

  val acc_field :
    record -> Cil_types.fieldinfo -> abstract
    (** Takes a record term of type (tau_of F.fcomp) and
        returns the term of type (tau_of F) for its field 'F' *)

  val upd_field :
    record -> Cil_types.fieldinfo -> abstract  -> record
    (** Takes a record term of type (tau_of F.fcomp) and
        a new term of type (tau_of F) for its field 'F'
        and return the updated record. *)

  (** {2 Array as first Class Value} *)

  val acc_index : array -> integer -> abstract
    (** Takes term of type ['a farray] and returns the ['a] at index [i]. *)

  val upd_index : array -> integer -> abstract -> array



  (** {2 Buitin Indexd Declaration } *)
  module type Indexed =
  sig
    include Registry
    val get_ind : t -> integer
    val has_ind : t -> pred -> bool
  end

  module Dindex (I : Identifiable):
  sig include Indexed with type t = I.t end

  module Findex : Indexed with type t = fieldinfo
  module Xindex : Indexed with type t = varinfo
  module Aindex : Indexed with type t = varinfo
  module Tindex : Indexed with type t = compinfo
  module LTindex : Indexed with type t = logic_type

end


module type Logic =
sig
  module F: S

  (** {2 Constrained Terms Construction} *)
  type context
  type bindings

  val closed : bindings
  val close : bindings -> F.pred -> F.pred

  val push : string -> F.pool -> bindings -> context
  val pop : string -> context -> bindings
  val kill: string -> context -> unit
  val flush : string -> context -> F.pred -> F.pred (* pop and close *)

  val term_such_that : tau -> ('a F.term -> F.pred) -> 'a F.term

  val forall : F.var list -> F.pred -> F.pred
  val exists : F.var list -> F.pred -> F.pred
  val subst : F.var -> 'a F.term -> F.pred -> F.pred
  val fresh : string -> kind -> F.var
  val alpha : F.var -> F.var option
  val pool : unit -> F.pool
  val vkind_of_var : F.var -> kind
  val has_context_vars : F.var list -> F.pred -> bool
  val has_type : F.abstract -> logic_type -> F.pred
  val is_comp : Cil_types.compinfo -> F.abstract -> F.pred
  val is_array: Ctypes.arrayinfo -> F.abstract -> F.pred

  (** {2 Generalized substitutions} *)


  val apply : (F.var * F.var) list -> 'a F.term -> 'a F.term

  val havoc_static : F.havoc list -> F.pred -> F.pred
  val havoc_inductive : F.havoc list -> F.pred -> F.pred

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

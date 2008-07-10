load "intLib";
load "bossLib";

(* why: THEORY *)

(*
BEGIN
  IMPORTING why_arrays, why_array_pred, why_int_array_pred

  unit: TYPE = int
  unit: unit = 0
*)
new_type("unit",0);

(*
  c,x,y : VAR int
  nzy : VAR nzint
*)
new_type("nzint", 1); (* NON-Zero Integer in PVS *)

(*
  %% integer division and modulo
  div(x, nzy) : int = floor(x / nzy)
  mod(x, nzy) : int = x - nzy * div(x, nzy)
*)

(* HOL has its own definition of div(), mod() with [arbint, arbnum]
val div_def = Define `div(x, nzy) = floor(x / nzy)`;
val mod_def = Define `mod(x, nzy) = x - nzy * div(x, nzy)`;
*)

(*
  %% well founded relations
  zwf(c) : [int, int -> bool] = (lambda x,y: c <= y and x < y)
  zwf_zero : [int, int -> bool] = zwf(0)

  zwf_wf : AXIOM forall c: well_founded?(zwf(c))
*)

val zwf_def = Define `zwf(c) = \x y. c<=y /\ x < y`;
val zwf_zero_def = Define `zwf_zero = zwf(0)`;
val zwf_wf_def = new_axiom("zwf_wf", Term `!c. WF(zwf(c))`); 

(*
END why
*)
(*
why_arrays[T: TYPE]: THEORY
BEGIN
*)

load "pred_setLib";
open pred_setTheory;
overload_on ("below", Term `pred_set$count`);

(* below(n) = {0,1,...,n-1} *)
(* warray = num -> num -> `a  *)
(*  warray: TYPE = [ n:nat, [ below(n) -> T ] ]

type_abbrev("warray", Type`[nat, below(n)->'a]`;
*)
(* Ignore this
  i,j : VAR int
  v : VAR T
*)
(*  array_length(t:warray) : int = proj_1(t)
DEFINITION of proj_1()
*)

Hol_datatype `warray = <| length:num;
			  index:num->'a
	                |>`;


(* NOT needed: val array_length_def = Define `array_length(Arr l f ) = l`; *)
(*  access(t:warray, i:below(proj_1(t))) : T = proj_2(t)(i)
DEFINITION of proj_2()
*)

(* NOT needed: val access_def = Define `access(Arr l f, i) = if i<l then f(i) else ARB`; *)

val access_def = Define `access A i = if i<A.length then A.index(i) else ARB`;

(*  store(t:warray, i:below(proj_1(t)), v): warray = 
    (proj_1(t), 
     (LAMBDA (j:below(proj_1(t))): IF j = i THEN v ELSE proj_2(t)(j) ENDIF))
*)
(* val store_def = Define `store(Arr l f, i, v) = 
	Arr l (\j. if i<l /\ j<l  then (if i=j then v else f(j) ) else ARB)`; 
*)


val store_def = 
 Define 
   `store A i v =
      A with index := \j. if i<A.length /\ j<A.length
                            then (if i=j then v else A.index(j))
	                    else ARB `;

(*A with index := !j. if i=j then v else A.index(j)`;*)

(*
END why_arrays
*)

(* % predicates over integers arrays
why_int_array_pred: THEORY
BEGIN

  IMPORTING why_arrays
*)

(*  t,u : VAR warray[int]
  i,j,k : VAR int
*)

(*  % t[i..j] is sorted
  sorted_array(t:warray[int], i, j) : bool =
    forall (k:below(proj_1(t)-1)): 
    i <= k and k < j implies access(t,k) <= access(t,k+1)
*)
val sorted_array_def = 
 Define 
   `sorted_array t i j =
	!k. i<=k /\ k<j ==> access t k <= access t k+1`;


(*
END why_int_array_pred
*)

(*
% predicates over arrays
why_array_pred[T: TYPE]: THEORY
BEGIN

  IMPORTING why_arrays[T]

  t,u,v : VAR warray[T]
  i,j,k,l,r : VAR int
*)

(*  % t[i..j] = u[i..j]
  array_id(t:warray[T], u:warray[T], 
	   i:below(min(proj_1(t),proj_1(u))),
	   j:below(min(proj_1(t),proj_1(u)))) : bool = 
    forall k: i <= k and k <= j implies access(t,k) = access(u,k)
*)
val array_id_def = 
 Define 
   `array_id t u i j =
      i < MIN t.length u.length /\
      j < MIN t.length u.length /\
     !k. i<=k /\ k<=j ==> (access t k = access u k)`;

(*  % swap of elements at i and j
  exchange(t, u, i, j) : bool = 
    array_length(t) = array_length(u) and
    0 <= i and i < array_length(t) and 0 <= j and j < array_length(t) and
    access(t,i) = access(u,j) and
    access(t,j) = access(u,i) and
    forall k: 0 <= k and k < array_length(t) implies 
      k /= i implies k /= j implies access(t,k) = access(u,k)
*)

val exchange_def = 
 Define 
   `exchange t u i j =
	(t.length = u.length)  /\
	0 <= i /\ i < t.length /\
        0 <= j /\ j < t.length /\
	(access t i = access u j) /\
	(access t j = access u i) /\
	!k. 0 <= k /\ k < t.length /\
            ~(k=i) /\ ~(k=j) 
          ==> (access t k = access u k)`;


(*
  % permut is the smallest equivalence relation containing exchange
  permut(t, u) : bool
*)

val permut_def = new_constant("permut", Type`:(('a) warray) -> (('a) warray) -> bool`);


(*  permut_exchange : 
    AXIOM forall t,u,i,j: exchange(t,u,i,j) implies permut(t,u)
*)

val permut_exchange_def = 
    new_axiom("permut_exchange", Term `!t u i j. exchange t u i j ==> permut t u`);

(*  permut_refl     : 
    AXIOM forall t: permut(t,t)
*)

val permut_refl_def = 
    new_axiom("permut_refl", Term `!t. permut t t`);

(*  permut_sym      : 
    AXIOM forall t,u: permut(t,u) implies permut(u,t)
*)

val permut_sym_def = 
    new_axiom("permut_sym", Term `!t u. permut t u ==> permut u t`);

(*  permut_trans    : 
    AXIOM forall t,u,v : permut(t,u) implies permut(u,v) implies permut(t,v)
*)

val permut_trans_def = 
    new_axiom("permut_trans", Term `!t u v. 
		permut t u ==> permut u v==>permut t v`);


(*  % sub_permut(t,u,i,j) defines a permutation on the sub-array i..j,
  % other elements being equal
  sub_permut(l, r, t, u) : bool
*)

val sub_permut_def = new_constant("sub_permut", Type`:(('a) warray) -> (('a) warray) -> num -> num -> bool`);

(*  sub_permut_exchange : 
    AXIOM forall l,r,t,u,i,j: 
      l <= i and i <= r implies l <= j and j <= r implies 
      exchange(t,u,i,j) implies sub_permut(l,r,t,u)
*)
val sub_permut_exchange_def = 
    new_axiom("sub_permut_exchange", Term`!l r t u i j.
	 l<=i /\ i<=r  ==>  l<=j /\ j<=r  
	==> exchange t u i j ==> sub_permut t u l r`);

(* 
  sub_permut_refl     : 
    AXIOM forall l,r,t: sub_permut(l,r,t,t)
  sub_permut_sym      : 
    AXIOM forall l,r,t,u: sub_permut(l,r,t,u) implies sub_permut(l,r,u,t)
  sub_permut_trans    : 
    AXIOM forall l,r,t,u,v : sub_permut(l,r,t,u) implies sub_permut(l,r,u,v) 
	                     implies sub_permut(l,r,t,v)
END why_array_pred
*)

val sub_permut_refl_def = 
    new_axiom("sub_permut_refl", Term `!l r t. sub_permut l r t t`);

val sub_permut_sym_def = 
    new_axiom("sub_permut_sym", Term `!l r t u. 
	sub_permut l r t u ==> sub_permutl r u t`);

val sub_permut_trans_def = 
    new_axiom("sub_permut_trans", Term `!l r t u v. 
	sub_permut l r t u ==> sub_permut l r u v ==> sub_permut l r t v`);

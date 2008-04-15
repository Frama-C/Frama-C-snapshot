(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: theory_filtering.ml,v 1.12 2008/11/05 14:03:18 filliatr Exp $ i*)

(*s Harvey's output *)

open Ident
open Options
open Misc
open Error
open Logic
open Logic_decl
open Env
open Cc
open Format
open Pp
open Hashtbl
open Set
open Unionfind 

let threshold = 1

let axiomCounter = ref 0

let reducedQueue =  Queue.create()

module Int_map = Map.Make(struct type t=int let compare= compare end)

module Int_set = Set.Make(struct type t=int let compare= compare end)

module Theory_container = 
struct 

  let m = ref Int_map.empty 

  let uniqueNumberGenerator () = 
     incr axiomCounter; !axiomCounter
      
  (**
     @param f is the formula we want to store
     @return n is the index where the formula has been stored
  **)
  let add (logic,d) = 
    let n  = uniqueNumberGenerator () in 
    m := Int_map.add n (logic,Int_set.empty,d)  !m ;
    n
          

  let reset () = 
    m := Int_map.empty ;
    Queue.clear reducedQueue ;
    axiomCounter := 0


  let  depends_on n =
    try 
      let (_,_,d) =  Int_map.find n !m in 
      d
    with 
	Not_found -> 
	  Printf.printf "Number %d not found \n" n;
	  raise Exit
    
  let  produces n =
    try 
      let (_,p,_) =  Int_map.find n !m in 
      p
    with 
	Not_found -> 
	  Printf.printf "Number %d not found \n" n;
	  raise Exit

  let axiom n =
    try 
      let (l,_,_) =  Int_map.find n !m in 
      l
    with 
	Not_found -> 
	  Printf.printf "Number %d not found \n" n;
	  raise Exit

  let set_produce n s  = 
    let l = axiom n in 
    let d = depends_on n in
    m := Int_map.add n (l,s,d)  !m 

end 
  



module  Symbol_container = 
struct 
  module Id_map = Map.Make(struct type t=string let compare= compare end)

  let m = ref Id_map.empty

  let add logic n= 
    m := Id_map.add logic n !m 

  let reset () = 
    m := Id_map.empty
  
  let index_of logic =
    try 
       Id_map.find logic !m
    with 
	Not_found -> 
	  Printf.printf "Symbol %s not found \n" logic;
	  raise Exit
end



(**
   collects the functionnal  symbols
   of the term given in parameter 
   @paramater f : the formula we are parsing   
   @returns the StringSet that contains all the symbols 
   of the formula
**)	
let functional_symbols  f  = (*: Logic_decl.t -> StringSet*) 
  (** symbols the result **)
  let symbolsSet  = ref Int_set.empty  in 
  let rec collect formula  = 
    match formula with 
      | Tconst (ConstInt n) -> ()
      | Tconst (ConstBool _) -> () 
      | Tconst ConstUnit -> ()
      | Tconst (ConstFloat _) -> ()
      | Tderef _ -> ()
      | Tapp (id, [a; b; c], _) when id == if_then_else -> 
	  collect a; 
	  collect b;
	  collect c  
      | Tapp (id, tl, _) when is_relation id || is_arith id ->
	  symbolsSet  := Int_set.add (Symbol_container.index_of (Ident.string id))  !symbolsSet ;
	  List.iter collect tl 
      | Tapp (id, [], i) -> 
	  symbolsSet  := Int_set.add (Symbol_container.index_of (Ident.string id)) !symbolsSet
      | Tapp (id, tl, i) ->
	  symbolsSet  := Int_set.add (Symbol_container.index_of (Ident.string id)) !symbolsSet;
	  List.iter collect tl 
      | _ -> ()
  in
  collect f ; 
  !symbolsSet


let symbols  f  =
  (** symbols the result **)
  let symbolsSet  = ref Int_set.empty  in 
  let rec collect formula  = 
    let rec collectIntoAList  l = match l with 
	[] -> () 
      |  p :: r -> 
	   collect p ;
	   collectIntoAList r in 
    match formula with 
      | Papp (id, [a; b], _) when is_eq id || is_neq id || id == t_zwf_zero->
	  symbolsSet  := Int_set.union (functional_symbols a) 
	    !symbolsSet ;
	  symbolsSet  := Int_set.union (functional_symbols b) 
	    !symbolsSet 
      | Pand (_, _, a, b) | Forallb (_, a, b)  | Por (a, b) | Piff (a, b) | 
	    Pimplies (_, a, b) ->
	  collect a;
	      collect b
      | Papp (id, tl, i) -> 
	  let rec functional_symbolsFromList  l  =  
	    match l with 
		[] -> Int_set.empty
	      | t :: q ->  Int_set.union  (functional_symbols t)
		  (functional_symbolsFromList q) in
	  symbolsSet  := Int_set.union (functional_symbolsFromList tl) 
	    !symbolsSet ;   
	  symbolsSet  := Int_set.add  (Symbol_container.index_of (Ident.string id))  !symbolsSet 
      | Pif (a, b, c) ->
	  symbolsSet  := Int_set.union (functional_symbols a) 
	    !symbolsSet ;
	  collect b;
	  collect c
      | Pnot a ->
	  collect a;
      | Forall (_,id,n,t,_,p) | Exists (id,n,t,p) ->    
	  collect p
      | Pfpi _ ->
	  failwith "fpi not yet suported "
      | Pnamed (_, p) -> (* TODO: print name *)
	  collect p 
      |_ -> ()
  in
  collect f ; 
  !symbolsSet


let rec add_relevant_in elt s = 
  if Int_set.mem elt s then
    s
  else  
    Int_set.add elt (Int_set.fold add_relevant_in (Theory_container.depends_on elt) s)
      

let rank n s = 
  let s1 = Theory_container.produces n in 
  let r = 
    if Int_set.subset s1 s then 1 else 0 in 
  (*Printf.printf "rank %d \n" r ;*)
  r


let filter rs selectedAx notYetSelectedAx = 
  let irrelevant = ref notYetSelectedAx in
  let relevant = ref selectedAx in
  let new_rs = ref rs in 
  let f ax =
    let r = rank ax rs in 
    if r >= threshold then
      begin 
	relevant := add_relevant_in ax !relevant ;
	irrelevant := Int_set.remove ax !irrelevant;
	new_rs := Int_set.union rs (Theory_container.produces ax)
      end ; 
    if debug then begin 
	Printf.printf "ax (%d) has rank %d \n" ax r
      end    
  in
  Int_set.iter f notYetSelectedAx ;
  (!new_rs,!relevant,!irrelevant)


let display_symb_of set =
  Int_set.iter (fun s -> Printf.printf "%d " s) set 
 
let display (q,s) n = 
  let di = function 
    | Dtype (_, _, id) -> Printf.printf  "type %s (%d) : "  id 
    | Dlogic (_, id, t) -> Printf.printf  "arit %s (%d) : " id   
    | Dpredicate_def (_, id, d) -> 
	let id = Ident.string id in
	Printf.printf  "def_pred %s (%d): " id 
    | Dinductive_def(loc, ident, inddef) ->
	failwith "Theory filtering: inductive def not yet supported"
    | Dfunction_def (_, id, d) -> 
	let id = Ident.string id in
	Printf.printf  "def_func %s (%d): " id 
    | Daxiom (_, id, p)          -> Printf.printf  "axiom %s (%d): "  id 
    | Dgoal (_, expl, id, s)   -> Printf.printf  "goal %s (%d):"  id 
  in 
  if debug then begin 
    (di q) n;
    display_symb_of s; Printf.printf "\n" 
  end

let managesGoal id ax (hyps,concl) =   
  let rec symb_of_seq = function
    | [] -> symbols concl
    | Svar (id, v) :: q ->  symb_of_seq  q 
    | Spred (_,p) :: q -> 
	Int_set.union 
	  (symbols p) 
	  (symb_of_seq q)
  in
  let setOfSymbols =  ref (symb_of_seq hyps) in 
  let n = Theory_container.add  (ax,!setOfSymbols) in 
  display (ax,!setOfSymbols) n ;
  (*Theory_container.set_produce n setOfSymbols ; *)
  let allax = ref Int_set.empty  in 
  for i= 1 to n-1 do
    allax := Int_set.add i !allax 
  done;
  let rel = ref Int_set.empty in
  let irrel = allax in 
  for i=1 to 2 do (* include predicates/functions and their properties *)
    let (rs,r,ir) = filter !setOfSymbols !rel  !irrel in
    rel := r ;
    setOfSymbols := rs ; 
    irrel :=  ir 
      (*Printf.printf "Relevant of %d : " n;
      Int_set.iter (fun t-> Printf.printf "%d " t) rel ;
      Printf.printf " \n " 
    *)
  done;
  Int_set.iter 
    (fun t-> Queue.add (Theory_container.axiom t) reducedQueue)
    !rel 
  
  

let declare_axiom id ax p =
  let setOfSymbols = symbols p in 
  let n = Theory_container.add  (ax,setOfSymbols) in 
  Theory_container.set_produce n setOfSymbols ;
  display (ax,setOfSymbols) n


let declare_function id ax (_,_,e) = 
  let setOfSymbols =  functional_symbols e in 
  let n = Theory_container.add  (ax,setOfSymbols) in 
  Theory_container.set_produce n (Int_set.singleton n) ;
  Symbol_container.add id n;
  display (ax,setOfSymbols) n


let declare_predicate id ax (_,p) = 
  let setOfSymbols = symbols p in 
  let n = Theory_container.add  (ax,setOfSymbols) in 
  Theory_container.set_produce n (Int_set.singleton n) ;
  Symbol_container.add id n;
  display (ax,setOfSymbols) n

let declare_arity id ax = 
  let n = Theory_container.add (ax,Int_set.empty)  in 
  Symbol_container.add id n ;
  display (ax,Int_set.empty) n


let declare_type id ax = 
  let n = Theory_container.add (ax,Int_set.empty)   in
  Symbol_container.add id n ;
  display (ax,Int_set.empty) n

    
let launcher decl = match decl with   
  | Dtype (_, _, id) as ax -> (*Printf.printf  "Dtype %s \n"  id ;*) 
      declare_type id ax 
  | Dlogic (_, id, t) as ax -> (* Printf.printf  "Dlogic %s \n"  id ;*) 
      declare_arity  id ax
  | Dpredicate_def (_, id, d) as ax -> 
      (*Printf.printf  "Dpredicate_def %s \n"  *)
      let id = Ident.string id in
      declare_predicate id ax d.scheme_type 
  | Dinductive_def(loc, ident, inddef) ->
      failwith "Theory filtering: inductive def not yet supported"
  | Dfunction_def (_, id, d)  as ax ->
      (*Printf.printf  "Dfunction_def %s \n"  id ;*)  
      let id = Ident.string id in
      declare_function id ax d.scheme_type 
  | Daxiom (_, id, p) as ax         -> (*Printf.printf  "Daxiom %s \n"  id ; *)
      declare_axiom  id ax p.scheme_type 
  | Dgoal (_, expl, id, s)  as ax -> (*Printf.printf  "Dgoal %s \n"  id ; *)
      managesGoal id ax s.Env.scheme_type 




(**
   @param q is a logic_decl Queue 
   @returns the pruned theory 
**)

let reduce q = 
  Theory_container.reset();
  Symbol_container.reset();  
  Queue.iter launcher q ;
  reducedQueue
  

  

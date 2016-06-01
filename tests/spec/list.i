/*@ type List<A> = \list<A>; */

//@ logic List<integer> \empty_integer_list =  [| |] ;

/*@ predicate is_empty_list_1(\list<integer> l1) = l1 == \Nil; */ 
/*@ predicate is_empty_list_2(\list<integer> l1) = l1 == [| |]; */ 
/*@ predicate is_empty_list_3(\list<integer> l1) = l1 == \empty_integer_list; */ 
/*@ logic \list<integer> empty_1 = [| |] ; */ 
/*@ logic \list<integer> empty_2 = \Nil ; */
/*@ logic \list<integer> empty_3 = \empty_integer_list ; */ 

/*@ logic \list<\list<integer> > list_of_list_1 = [| [| |] |] ; */ 


/*@ logic \list<integer> call_to_1(integer id) = [| id |]; */ 
/*@ logic \list<integer> call_to_2(integer id) = \Cons(id, \Nil) ; */ 
/*@ logic \list<integer> call_to_3(integer id) = \Cons(id, \empty_integer_list) ; */ 

/*@ logic \list<integer> list_1(int e1, integer e2) = [| e1, e2 |]; */ 
/*@ logic \list<integer> list_2(integer e1, integer e2) = \Cons(e1, \Cons(e2, \Nil)); */ 
/*@ logic \list<integer> list_3(integer e1, integer e2) = \Cons(e1, \Cons(e2, \empty_integer_list)); */ 

/*@ logic \list<integer> concat_1(\list<integer> seq1, \list<integer> seq2) = seq1 ^ seq2  ;*/ 
/*@ logic \list<integer> concat_2(\list<integer> seq1, \list<integer> seq2) = \concat(seq1 ,seq2) ; */ 

/*@ logic \list<integer> repeat_1(\list<integer> seq, integer n) = \repeat(seq, n) ; */ 
/*@ logic \list<integer> repeat_2(\list<integer> seq, integer n) = seq *^ n ; */ 

/*@ logic \list<integer> add_call_to_1(integer id, \list<integer> seq) = seq ^ [| id |] ; */ 
/*@ logic \list<integer> add_call_to_2(integer id, \list<integer> seq) = seq ^ \Cons(id, \Nil); */ 
/*@ logic \list<integer> add_call_to_3(integer id, \list<integer> seq) = seq ^ call_to_1(id) ; */ 


/*@ predicate Padd_id{L1,L2}(integer id, \list<integer> seq) = \at(seq,L1) == (\at(seq,L2) ^ [| id |]); */ 
/*@ predicate Prepeat{L1,L2}(integer times, \list<integer> seq) = \at(seq,L1) == (\at(seq,L2) *^ times ); */ 

//@ ghost int seq;
//@ axiomatic Ghost { logic \list<integer> ghost_seq reads seq ; }

/*@ assigns seq ;
  @ ensures Padd_id{Here,Pre}(id, ghost_seq);
  @*/
void add_id(int id);

//@ lemma length: \length(ghost_seq) >=0;
//@ lemma nth: \nth(\Cons(1,ghost_seq),0) == 1;

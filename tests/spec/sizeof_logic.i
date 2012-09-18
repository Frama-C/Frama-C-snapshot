/*@ lemma bad: \forall integer x; sizeof(x) == sizeof(int); */

/*@ lemma bad2: sizeof(integer) == sizeof(int); */

/*@ lemma good: \forall short x; sizeof(x) <= sizeof(int); */ 

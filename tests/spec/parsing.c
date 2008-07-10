/* cf bug 298 */
/*@ axiom bidon{Here} :
  @   \forall int *t; ! (t[0] > 0);
  @*/

/*@ axiom bidon1{Here} :
  @   \forall int *t; ! t[0] == 0;
  @*/

/*@ axiom bidon2{Here} :
  @   \forall int *t; (! t[0]) == 0;
  @*/

/*@ axiom bidon3{Here} :
  @   \forall int *t; ! t[0] >= 0;
  @*/

/*@ axiom bidon4{Here}:
  @   \forall int *t; (! t[0]) < 0;
  @*/

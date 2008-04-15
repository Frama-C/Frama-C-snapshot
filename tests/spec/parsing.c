/* cf bug 298 */
/*@ lemma bidon{Here} :
  @   \forall int *t; ! (t[0] > 0);
  @*/

/*@ lemma bidon1{Here} :
  @   \forall int *t; ! t[0] == 0;
  @*/

/*@ lemma bidon2{Here} :
  @   \forall int *t; (! t[0]) == 0;
  @*/

/*@ lemma bidon3{Here} :
  @   \forall int *t; ! t[0] >= 0;
  @*/

/*@ lemma bidon4{Here}:
  @   \forall int *t; (! t[0]) < 0;
  @*/

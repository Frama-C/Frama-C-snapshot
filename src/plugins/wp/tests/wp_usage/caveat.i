/* run.config
   OPT:
   OPT: -wp-log refusage -wp-model Caveat
*/

/* run.config_qualif
   DONTRUN:
*/

typedef struct S { int f ; int g ; } ;

/*@
  ensures a->f == \old(a->f)+1 ;
  ensures a->g == \old(a->g)+1 ;
  ensures *r == \old(a->f + a->g) ;
 */
void implicit(struct S * a , int * r)
{
  int * p = &(a->f) ;
  int * q = &(a->g) ;
  *r = *p + *q ;
  (*p)++;
  (*q)++;
}

/*@
  requires \separated(a,r);
  ensures a->f == \old(a->f)+1 ;
  ensures a->g == \old(a->g)+1 ;
  ensures *r == \old(a->f + a->g) ;
 */
void explicit(struct S * a , int * r)
{
  int * p = &(a->f) ;
  int * q = &(a->g) ;
  *r = *p + *q ;
  (*p)++;
  (*q)++;
}

//@ predicate OBS(integer f,integer g,integer r);

/*@
  requires KO: OBS(a->f,a->g,*r);
  ensures  KO: OBS(a->f,a->g,*r);
 */
void observer(struct S * a , int * r)
{
  int * p = &(a->f) ;
  int * q = &(a->g) ;
  *r = *p + *q ;
  (*p)++;
  (*q)++;
}

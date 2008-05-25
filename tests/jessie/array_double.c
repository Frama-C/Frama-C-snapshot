typedef struct
{
  char   Texte[10][10] ;
} T_CompositionPage ;

T_CompositionPage Page;

//void StrCpy(char* s);

//@ predicate eq_message{L}(char *s, integer m);

void f();

//@ ensures eq_message(Page.Texte[9],0);
void f() {
  //  StrCpy(Page.Texte[9]);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_double"
End:
*/

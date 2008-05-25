
/*****************************************************************************/
/* typed memory model                                                        */
/*****************************************************************************/

/*@ logic int offset_min(char* p); */

/*@ logic int offset_max(char* p); */

/*****************************************************************************/
/* strings                                                                   */
/*****************************************************************************/

// Should be "reads s[0..]". Replaced by "reads *s" while grammar for location 
// is still evolving.
/*@ logic int strlen{L}(char* s) reads *s; */

/*@ axiom strlen_def1{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i < strlen(s) ==> s[i] != 0;
  @*/

/*@ axiom strlen_def2{L} :
  @   \forall char* s; s[strlen(s)] == 0;
  @*/

/*@ axiom strlen_not_zero{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i <= strlen(s) && s[i] != 0 ==> i < strlen(s);
  @*/

/*@ axiom strlen_zero{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i <= strlen(s) && s[i] == 0 ==> i == strlen(s);
  @*/

/*@ axiom strlen_sup{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i && s[i] == 0 ==> 0 <= strlen(s) <= i;
  @*/

/*@ axiom strlen_shift{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i <= strlen(s) ==> strlen(s+i) == strlen(s)-i;
  @*/

/*@ axiom strlen_create{L} :
  @   \forall char* s; \forall int i;
  @     0 <= i && s[i] == '\0' ==> 0 <= strlen(s) <= i;
  @*/

/*@ axiom strlen_create2{L} :
  @   \forall char* s; \forall int i; \forall int k;
  @     k <= i && s[i] == '\0' ==> 0 <= strlen(s+k) <= i - k;
  @*/

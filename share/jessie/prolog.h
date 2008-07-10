
#define FRAMA_C_STRING __declspec(string)


/*****************************************************************************/
/* typed memory model                                                        */
/*****************************************************************************/

//@ logic int offset_min(char* p);

//@ logic int offset_max(char* p);

/*****************************************************************************/
/* strings                                                                   */
/*****************************************************************************/

//@ logic int strlen{L}(char* s) reads s[0..];

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
  @     0 <= k <= i && s[i] == '\0' ==> 0 <= strlen(s+k) <= i - k;
  @*/

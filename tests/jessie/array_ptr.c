
int ga[10];

/*@ ensures \forall int i; 0 <= i < 10 ==> ga[i] == i;
  @ */
void init() {
  int i;
  /*@ loop invariant i >= 0 && \forall int k; 0 <= k < i ==> ga[k] == k; */
  for (i = 0; i < 10; i++) ga[i] = i;
}

/*@ ensures \forall int i; 0 <= i < 10 ==> ga[i] == i;
  @ */
void initp() {
  int i;
  int* p = ga;
  /*@ loop invariant i >= 0 && \forall int k; 0 <= k < i ==> ga[k] == k; */
  for (i = 0; i < 10; i++) p[i] = i;
}

/*@ ensures \forall int i; 0 <= i < 10 ==> ga[i] == i;
  @ */
void initparith() {
  int i;
  int* p = ga;
  /*@ loop invariant i >= 0 && p == ga + i 
    @         && \forall int k; 0 <= k < i ==> ga[k] == k; */
  for (i = 0; i < 10; i++, p++) *p = i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_ptr"
End:
*/

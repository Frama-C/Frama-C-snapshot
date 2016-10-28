/* run.config
   STDOPT: +"tests/syntax/bts1553.i -kernel-msg-key file -kernel-msg-key=-file:transformation"
   COMMENT: this file is also parsed together with bts1553.i
*/

struct a {
    int b;
};

extern struct a *d[] = {&(struct a){1}};

extern struct a *e[] = {&(struct a){2}};

void foo(int c) {
  struct a* *p = c ? d :e;
}

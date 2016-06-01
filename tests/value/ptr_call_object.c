/* run.config*
   STDOPT: +"-slevel 2"
*/

struct obj {
  int (*next)();
  int i;
};

int next_1(struct obj *p, struct obj s) {
  Frama_C_show_each_p_in_next_1(p, s);
  return 1;
}
 
int next_0(struct obj *p, struct obj s) {
  Frama_C_show_each_p_in_next_0(p, s);
  return 0;
}

//@ assigns \result \from \nothing;
int rand();

int main() {
  struct obj o1, o0;
  struct obj *p;
  
  o1.next = &next_1;
  o0.next = &next_0;
 
  p = rand () ? &o1 : &o0;
  
  //@slevel merge;
    
  if (p->next(p, *p)){  // p must be precise in each call, including as a formal
    Frama_C_show_each_x(p); // only p == &o1 is possible
  }
}

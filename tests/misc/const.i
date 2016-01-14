/* run.config
   STDOPT: #"-const-writable" +"-then -const-readonly"
*/
extern const int G;
extern const int I=2;
int J = 8;

volatile v;
int X;

const struct {
  int i1;
  int i2;
} s = { 3, 4};

const int t[10] = {1, 2, 3, 4, 5, 6};

void const_formal(int const i)
{
  Frama_C_show_each(i);
  if (v) i = 0;
}

void pointer_to_const(const int *p) {
  Frama_C_show_each(*p);
  *p = 0; // Invalid access through the formal itself
  Frama_C_show_each_dead();
}

void const_destination(int *p) {
  Frama_C_show_each(*p);
  *p = 0; // Invalid access through the variable pointed
  Frama_C_show_each(p);
}

void modify_I (){
  Frama_C_show_each(I);
  if (v) I++;
  if (v) pointer_to_const(&I);
  if (v) const_destination(&I);
}

void modify_J (){
  Frama_C_show_each(J);
  if (v) J++;
  if (v) pointer_to_const(&J);
  if (v) const_destination(&J);
}

void modify_s (){
  Frama_C_show_each(s.i1);
  if (v) s.i1 ++;
  if (v) pointer_to_const(&s.i2);
  if (v) const_destination(&s.i2);
}

void modify_t(){
  Frama_C_show_each(t[5]);
  if (v) t[5] ++;
  if (v) pointer_to_const(&t[3]);
  if (v) const_destination(&t[2]);

}

// we can reduce G, even though it is constant
void constrain_G () {
  int r;
  if (G == 1) {
    r = G + 2;
  } else {
    //@ assert G == 4;
    r = G + 1;
  }
  Frama_C_show_each(G);
}

// Validity in the logic must correspond to the C part: check that the l-value
// is not const
void pointer_to_const_logic(const int *p) {
  if (v) *p = 12;
}

void main () {
  const_formal(G);
  const_formal(42);

  modify_I();
  modify_J();
  modify_s();
  modify_t();
  constrain_G ();

  pointer_to_const_logic (&J);
}

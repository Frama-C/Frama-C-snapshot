/* run.config*
   STDOPT: +"-eva-warn-copy-indeterminate=-@all -then -eva-warn-copy-indeterminate=@all"
*/

// Comments are given for cases where -eva-warn-copy-indeterminate is not set

volatile v;

void main() {
  int x;
  unsigned int y;
  float f;

  x = 1;
  f = *(float *)&x; // cast
  Frama_C_dump_each(); // Use Frama_C_dump_each because it does not cast the values, unlike Frama_C_show_each

  x = v;
  //@ assert ASSUME : 0 <= x <= 1120000000;
  f = *(float *)&x; // cast
  Frama_C_dump_each();

  x = v;
  //@ assert ASSUME : -10 <= x <= 10;
  f = *(float *)&x; // cast, infinite-nan
  Frama_C_dump_each();

  x = v;
  //@ assert ASSUME : -10 <= x <= 10;
  y = *(unsigned int *)&x; // no cast;
  Frama_C_dump_each();

  f = v;
  //@ assert ASSUME: 3 <= f <= 5;
  x = *(int *)&f; // cast
  Frama_C_dump_each();

  x = (int)&x;
  f = *(float *)&x; // no cast, pointers. We produce a GM anyway;
  Frama_C_dump_each();

  *(short *)&x = 1;
  *((short *)&x+1) = 2;
  f = *(float *)&x; // no cast, multiple ranges;
  Frama_C_dump_each();

  int l;
  if (v) {
    l = v;
    //@ assert ASSUME: 5 <= l <= 15;
  }
  f = *(float *)&l; // cast + uninitialized alarm
  Frama_C_dump_each();

}

/* run.config
   OPT: -val -no-results -then -float-hex -main mainbis
*/

int b;
extern float f1, f2, f3, f4;
extern double d1, d2, d3;
extern int i;
volatile unsigned int c;

/*@ requires 0. <= f1 <= 8. && 0. <= d1 <= 8. ; */
void main1()
{
  if ((int)f1 >= 2)
    Frama_C_show_each_float_(f1);

  if ((int)f1 <= 4)
    Frama_C_show_each_float_(f1);

  if ((int)f1 != 0)
    Frama_C_show_each_float_(f1);

  if ((int)f1 == 3)
    Frama_C_show_each_float_(f1);

  if ((int)d1 >= 2)
    Frama_C_show_each_double(d1);

  if ((int)d1 <= 4)
    Frama_C_show_each_double(d1);

  if ((int)d1 != 0)
    Frama_C_show_each_double(d1);

  if ((int)d1 == 3)
    Frama_C_show_each_double(d1);

  switch ((char)d1)
  {
   case 0: 
     b = 0;
     break;
   case 1: 
     b = 1;
     break;
   case 2: 
     b = 2;
     break;
   case 3: 
     b = 3;
     break;
   case 4: 
     b = 4;
     break;
   case 5: 
     b = 5;
     break;
   case 6: 
     b = 6;
     break;
   case 7: 
     b = 7;
     break;
   case 8:
     b = 8;
     break;
   default : 
     b = 999;
     break;
  }
  Frama_C_show_each(d1, b);
}

void main2() {
  if ((double)f1 > 1.17) {
    Frama_C_show_each_float_(f1);
  }
  if (d1 > (float)1.17) {
    Frama_C_show_each_double(d1);
  }
  if (d2 > 1.17) {
    Frama_C_show_each_double(d2);
  }
}

/* Reduction by numeric predicates in the logic, with arguments of different
   type */

/*@ requires -1000. <= f4 <= 1000; // Must fit in an int
    requires -1000. <= d2 <= 1000; */
void main3() {

  // Float/real, cast to bigger float
  //@ assert f1 > 10.; // Ok
  //@ assert (double)f2 > 10.; // Ok
  //@ assert d1 > 10.; // Ok

  // Float/integer
  //@ assert f3 > 10; // Ok

  // Integer/real
  //@ assert i > 50.;  // TODO

 
  //@ assert (int)f4 > 10;
  //@ assert (int)d2 > 10;

  Frama_C_dump_each();
}

void main() {
  switch(c) {
  case 1:
    main1 (); break;
  case 2:
    main2 (); break;
  case 3:
    main3 (); break;
  }
}

void mainbis() { // Only to get hex floating-point display
  main();
}

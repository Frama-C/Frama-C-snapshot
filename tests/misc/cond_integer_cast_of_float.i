/* run.config
   OPT: -val -float-hex -no-results
*/

int b;

/*@ requires 0. <= f <= 8. && 0. <= d <= 8. ; */
main(float f, double d)
{
  if ((int)f >= 2)
    Frama_C_dump_each();

  if ((int)f <= 4)
    Frama_C_dump_each();

  if ((int)f != 0)
    Frama_C_dump_each();

  if ((int)f == 3)
    Frama_C_dump_each();

  if ((int)d >= 2)
    Frama_C_dump_each();

  if ((int)d <= 4)
    Frama_C_dump_each();

  if ((int)d != 0)
    Frama_C_dump_each();

  if ((int)d == 3)
    Frama_C_dump_each();

  switch ((char)d)
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
  Frama_C_dump_each();
}

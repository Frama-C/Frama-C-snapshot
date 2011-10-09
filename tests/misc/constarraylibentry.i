/* run.config
   OPT: -memory-footprint 1 -val -lib-entry
*/

const int t[] = { 1, 2, 3, 4, 5 } ;

const int t2[3][3] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 } ;

typedef const int tt3[3];

tt3 t3[3] = { 10, 20, 30, 40, 50, 60, 70, 80, 90 } ;

main()
{
  Frama_C_dump_each();
}

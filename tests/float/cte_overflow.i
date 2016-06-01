/* run.config*
   STDOPT: #"-warn-decimal-float all"
*/

int volatile v;

int main()
{
   double t=0.0;

   if (v) {
     t = 1e500 * 1e500;
     Frama_C_dump_each(); // does not execute
   }

   if (v) {
     t = 1e80f * 1e500f;
     Frama_C_dump_each(); // does not execute
   }
   return 0;
}

/* run.config*
   STDOPT: #"-no-warn-signed-overflow"
   STDOPT: #"-warn-signed-overflow"
*/

long long int LL_ABS(long long int a) {
  return ((a) >= 0LL ? (a) : -(a));
}

/*@
  requires num: -9223372036854775807LL <= numerateur <=   9223372036854775807LL;
  requires denom: -9223372036854775807LL <= denominateur <= 9223372036854775807LL;
  */


long long int div64 (long long int numerateur, long long int denominateur) 
{
	long long int loc_num;
	long long int loc_den;
	long long int signe_negatif;

	signe_negatif = (numerateur ^ denominateur) & 0x8000000000000000;	

	loc_num = LL_ABS(numerateur);
	loc_den = LL_ABS(denominateur);
        Frama_C_show_each(numerateur, loc_num, denominateur, loc_den);
	return 0LL;
}


void main(long long int v1, long long int v2) {
 unsigned long long i;
 i = 0xFFFF804000000000UL;
 unsigned long j= ((((((256ULL) >> 8) * 0xffff000000000000UL) | (256ULL << 39) )) + (1ULL << 39)/2ULL); 
 Frama_C_show_each_f(sizeof(long),i,j);

 div64(v1, v2);
}

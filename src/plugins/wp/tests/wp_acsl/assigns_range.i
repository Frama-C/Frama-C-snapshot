/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -wp -wp-par 1 -wp-prop qed_ko -wp-steps 50
*/

int t1[20], t2[20], t3[20], t4[20], t5[20];

/*@ requires 0 <= i <20;
    assigns t1[i];
*/
void assigns_t1_an_element(int i);

/*@ requires 0 <= i && i <= j && j < 20 ; 
    assigns t2[i..j];
*/
void assigns_t2_bound (int i, int j);

/*@ requires 0 <= i < 20 ; 
    assigns t3[i..];
*/
void assigns_t3_inf_bound (int i);

/*@ requires 0 <= j <20 ; 
    assigns t4[..j];
*/
void assigns_t4_sup_bound (int j);

/*@ assigns t5[..];
*/
void assigns_t5_unbound (void);

/*@ requires 0 <= i && i <= j && j < 20 ; 
    assigns qed_ok: t1[(i-1)..(i+1)], t2[..], t3[i..], t4[..j], t5[..99] ; 
*/
void call_assigns_all(int i, int j)
{
  assigns_t1_an_element(i); 
  assigns_t2_bound(i,j);
  assigns_t3_inf_bound(i);
  assigns_t4_sup_bound(j);
  assigns_t5_unbound();
}

/*@ requires 5 <= i && i <= 7 ; 
    assigns qed_ko: t1[i-1]; // <- false property (wrong index)
*/
void call_assigns_t1(int i)
{
  assigns_t1_an_element(i);
}

/*@ requires 5 <= i && i <= 7 ; 
    assigns qed_ko: t1[i-1]; // <- false property (wrong base)
*/
void call_assigns_t2(int i)
{
  assigns_t2_bound(i,i+2);
}

/*@ requires 0 <= i && i <= j && j < 20 ; 
    assigns qed_ko: t4[i..];  // <- false property (wrong lower bound)
*/
void call_assigns_t4(int i, int j)
{
  assigns_t4_sup_bound(j);
}


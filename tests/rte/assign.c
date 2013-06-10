/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-all -print -rte-precond -journal-disable
*/

int global_x, global_y;

//@ assigns \nothing;
void g();

//@ assigns \nothing;
int fnd1();

//@ assigns global_x ;
int fnd2();

//@ assigns global_x; assigns global_y;
int fnd3();

int fnd4();

/*@ assigns global_x;
  @ behavior normal : assumes cond; assigns \nothing ;
  @ behavior other : assumes !cond; assigns global_x ;
@*/
int fnd5(int cond);

//@ assigns \nothing;
int fnd6();

//@ assigns *x \from *y ;
int fnd7(int* x, int* y);


int rte (int cond) { 
  int a,b; 
  g();
  if (fnd1() && fnd2() && fnd3() && fnd4() 
      && fnd5(cond) && fnd6() && fnd7(&a,&b)) 
    return 1; else return 0; }

/* run.config
   OPT: -check -main main -slice-pragma main -ulevel 10 -journal-disable -then-on 'Slicing export' -print
*/
int choix ;
int state = 1;
int cumul =0 ;
int step =0;

//initialisation


/*@
ensures \result==0 || \result==1 || \result==2 ;
 */
int choisir() ;

void lecture() {
choix = choisir() ;
}

void fsm_transition() {
  switch (state) {
  case 1:
    if (choix == 2) {
      cumul = cumul +2 ;
      state = 2 ;
    }
    else
      cumul++;
    break ;
  case 2:
    if ((step==50) && (choix==1))
      state = 3 ;
    else
      cumul++ ;
    break ;
  case 3: if ((choix==0) && (cumul==10)) state = 1;
 default: break ;
  }
}

int main() {

  while (step>=0){
  lecture() ;
    fsm_transition() ;
    if (state == 3) {
      /*@ slice pragma ctrl ;*/
      break ;
    }
    step ++ ;
   }
  return 0 ;
}

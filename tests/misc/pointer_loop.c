/* run.config
   OPT: -memory-footprint 1 -val -out -input -deps -journal-disable
 */
int base0=7;
int base1=1;
int base2=2;
int *base_p[2]={&base1,&base2};
int *ioCtrl_p;
void main ()
{ short int i;
  int uiNmbrOfElements = 2;

  for (i = 0; i < uiNmbrOfElements; i++)
    {
      ioCtrl_p = base_p[i];
      *ioCtrl_p = 3+i;
      }
}


struct auIoCtrl;
typedef struct auIoSlot {
  int uiNmbrOfElements;
  struct auIoCtrl *const *ioCtrl_p;
} auIoSlot_t;
typedef struct auIoCtrl {
  const auIoSlot_t *slot_p;
  int inDriverStatus;
} auIoCtrl_t;

auIoCtrl_t i_auIoCtrl[2];
static auIoCtrl_t *const auIoCtrl_p[2] = {
  &i_auIoCtrl[0],
  &i_auIoCtrl[1]
};
const auIoSlot_t i_auIoSlot[2] = {
  { 2, &auIoCtrl_p[0]},
  { 0, (void *) 0}
};

void f(void)
{
  int i;
  enum counter j; // specific test for pointer_loop.c:42: error: storage size of 'j' isn't known
  i=0;
  (i_auIoSlot[i].ioCtrl_p[0])->inDriverStatus = 0;
  (i_auIoSlot[i].ioCtrl_p[1])->inDriverStatus = 0;
  for(j = 0; j < 2; j++) {
    (i_auIoSlot[i].ioCtrl_p[j])->inDriverStatus = 1;

    }

}

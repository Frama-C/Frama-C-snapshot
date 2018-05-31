/* run.config*
   COMMENT: this line preserves location...
 */
int base0=7;
int base1=1;
int base2=2;
int *base_p[2]={&base1,&base2};
int *Ctrl_p;
void main ()
{ short int i;
  int Elements = 2;

  for (i = 0; i < Elements; i++)
    {
      Ctrl_p = base_p[i];
      *Ctrl_p = 3+i;
      }
}


struct Ctrl;
typedef struct Slot {
  int Elements;
  struct Ctrl *const *Ctrl_p;
} Slot_t;
typedef struct Ctrl {
  const Slot_t *slot_p;
  int Status;
} Ctrl_t;

Ctrl_t Ctrl[2];
static Ctrl_t *const ACtrl[2] = {
  &Ctrl[0],
  &Ctrl[1]
};
const Slot_t Slot[2] = {
  { 2, &ACtrl[0]},
  { 0, (void *) 0}
};

void f(void)
{
  int i;
  int j;
  i=0;
  (Slot[i].Ctrl_p[0])->Status = 0;
  (Slot[i].Ctrl_p[1])->Status = 0;
  for(j = 0; j < 2; j++) {
    (Slot[i].Ctrl_p[j])->Status = 1;

    }

}

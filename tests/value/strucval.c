/* run.config*

*/
typedef enum {
    BDP_BITE_NORMAL, BDP_BITE_ANORMAL, BDP_BITE_TRUC
} BDP_Te_FunctionCode;

typedef struct {
   BDP_Te_FunctionCode FunctionCode;
   unsigned short int MachineNumber; /* machine number */
   unsigned long int Line;       /* line number*/
} BDP_Ts_SharedData;

#define BNR_Ct_MachineNumber 1456

//@ assigns \nothing;
extern void h(const BDP_Ts_SharedData sd);

void main()
{
  BDP_Ts_SharedData SharedData;

      SharedData.FunctionCode = BDP_BITE_NORMAL;
      SharedData.MachineNumber = BNR_Ct_MachineNumber;
      SharedData.Line = __LINE__;

      h(SharedData);
}

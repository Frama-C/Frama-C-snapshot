/* run.config
   DONTRUN: cast problem
*/
typedef struct {
int I0;
int I1;
int O0;
} T;

/*@ requires \valid(C);
  @ assigns C->O0;
  @ ensures C->O0 == (int)(C->I1 && C->I0);
  @*/
void ComputeCommands (T *C){
C->O0 = (C->I1) && (C->I0);
} 

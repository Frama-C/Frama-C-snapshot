
/* run.config_phoare
  OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

/*
 * To test [switch] in CFG : only need to test with one model.
 * To view the graph :  
   FCT=simple; ./frama-c -wp -wp-dot -wp-fct $FCT tests/wp/wp_switch.c; \
             zgrviewer $FCT.cfg.dot
 */

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 2;
  behavior x2 : assumes x == 2; ensures \result == 3;
  behavior df : assumes x != 1 && x !=2; ensures \result == 11;
  complete behaviors; disjoint behaviors;
*/

int simple (int x) {
  int res = 0;
  switch (x) {
    case 1: res = 1; break;
    case 2: res = 2; break;
    default : res = 10;
  }
  res = res+1;
  return res;
}
/*@ 
  behavior x1 : assumes x == 1; ensures \result == 2;
  behavior x2 : assumes x == 2; ensures \result == 3;
  behavior df : assumes x != 1 && x !=2; ensures \result == 1;
  complete behaviors; disjoint behaviors;
*/

int no_default (int x) {
  int res = 0;
  switch (x) {
    case 1: res = 1; break;
    case 2: res = 2; break;
  }
  res = res+1;
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 3;
  behavior x2 : assumes x == 2; ensures \result == 2;
  behavior df : assumes x != 1 && x !=2; ensures \result == 1;
  complete behaviors; disjoint behaviors;
*/
int no_break (int x) {
  int res = 0;
  switch (x) {
    case 1: res++;
    case 2: res++;
  }
  res++;
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior x2_KO : assumes x == 2; ensures \result == 2;
  behavior df : assumes x != 1 && x !=2; ensures \result == 0;
  complete behaviors; disjoint behaviors;
*/
int same_case (int x) {
  int res = 0;
  switch (x) {
    case 1: res=1; break;
    case 2: res=2; break;
    case 1+1: res=4; break;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1 || x == 2; ensures \result == 1;
  behavior x2 : assumes x == 4; ensures \result == 4;
  behavior df : assumes x != 1 && x !=2 && x != 4; ensures \result == 0;
  complete behaviors; disjoint behaviors;
*/
int multi_case (int x) {
  int res = 0;
  switch (x) {
    case 1: case 2: res=1; break;
    case 4: res = 4;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior df : assumes x != 1; ensures \result == 2;
  complete behaviors; disjoint behaviors;
*/
int case_and_default (int x) {
  int res = 0;
  switch (x) {
    case 1: res=1; break;
    case 2: 
    default: res = 2;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior df : assumes x != 1; ensures \result == 2;
  complete behaviors; disjoint behaviors;
*/
int dead_inst (int x) {
  int res = 0;
  switch (x) {
    case 1: res=1; break;
    default: res = 2; break;
	     res = 3;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior x2 : assumes x == 2; ensures \result == 0;
  behavior df : assumes x != 1 && x !=2; ensures \result == 3;
  complete behaviors; disjoint behaviors;
*/
int empty_case (int x) {
  int res = 0;
  switch (x) {
    case 1: res=1; break;
    case 2: break;
    default: res=3;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior df : assumes x != 1; ensures \result == 2;
  complete behaviors; disjoint behaviors;
*/
int default_before (int x) {
  int res = 0;
  switch (x) {
    default: res=2; break;
    case 1: res=1; break;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 1;
  behavior df : assumes x != 1; ensures \result == 2;
  complete behaviors; disjoint behaviors;
*/
int default_before_no_break (int x) {
  int res = 0;
  switch (x) {
    default: res++;
    case 1: res++;
  }
  return res;
}

/*@ 
  behavior x1 : assumes x == 1; ensures \result == 5;
  behavior x2 : assumes x == 2; ensures \result == 1;
  behavior x3 : assumes x == 3; ensures \result == 11;
  behavior df : assumes x != 1 && x !=2 && x !=3; ensures \result == 0;
  complete behaviors; disjoint behaviors;
*/
int another_label (int x) {
  int res = 0;
  switch (x) {
    case 1: res=5; break;
    case 2: L: res++; break;
    case 3: res = 10; goto L;
  }
  return res;
}


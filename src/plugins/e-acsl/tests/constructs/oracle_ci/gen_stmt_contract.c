/* Generated by Frama-C */
#include "stdio.h"
#include "stdlib.h"
int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,(size_t)8);
  int x = 0;
  int y = 2;
  /*@ ensures x ≡ 1; */
  {
    x = 1;
    __e_acsl_assert(x == 1,(char *)"Postcondition",(char *)"main",
                    (char *)"x == 1",8);
  }
  /*@ ensures x ≡ 2;
      ensures y ≡ 2;
      ensures x ≡ 2 ∧ y ≡ 2; */
  {
    int __gen_e_acsl_and;
    x = 2;
    __e_acsl_assert(x == 2,(char *)"Postcondition",(char *)"main",
                    (char *)"x == 2",11);
    __e_acsl_assert(y == 2,(char *)"Postcondition",(char *)"main",
                    (char *)"y == 2",12);
    if (x == 2) __gen_e_acsl_and = y == 2; else __gen_e_acsl_and = 0;
    __e_acsl_assert(__gen_e_acsl_and,(char *)"Postcondition",(char *)"main",
                    (char *)"x == 2 && y == 2",13);
  }
  /*@ requires x ≡ 2; */
  {
    __e_acsl_assert(x == 2,(char *)"Precondition",(char *)"main",
                    (char *)"x == 2",17);
    x ++;
  }
  /*@ requires x ≡ 3;
      requires y ≡ 2; */
  {
    __e_acsl_assert(x == 3,(char *)"Precondition",(char *)"main",
                    (char *)"x == 3",20);
    __e_acsl_assert(y == 2,(char *)"Precondition",(char *)"main",
                    (char *)"y == 2",21);
    x += y;
  }
  /*@ behavior b1:
        requires x ≡ 5;
        ensures x ≡ 3;
      
      behavior b2:
        requires x ≡ 3 + y;
        requires y ≡ 2;
        ensures x ≡ y + 1;
  */
  {
    __e_acsl_assert(x == 5,(char *)"Precondition",(char *)"main",
                    (char *)"x == 5",25);
    __e_acsl_assert((long)x == 3L + y,(char *)"Precondition",(char *)"main",
                    (char *)"x == 3 + y",28);
    __e_acsl_assert(y == 2,(char *)"Precondition",(char *)"main",
                    (char *)"y == 2",29);
    x = 3;
    __e_acsl_assert(x == 3,(char *)"Postcondition",(char *)"main",
                    (char *)"x == 3",26);
    __e_acsl_assert((long)x == y + 1L,(char *)"Postcondition",(char *)"main",
                    (char *)"x == y + 1",30);
  }
  /*@ behavior b1:
        assumes x ≡ 1;
        requires x ≡ 0;
      
      behavior b2:
        assumes x ≡ 3;
        assumes y ≡ 2;
        requires x ≡ 3;
        requires x + y ≡ 5;
  */
  {
    {
      int __gen_e_acsl_implies;
      int __gen_e_acsl_and_2;
      int __gen_e_acsl_implies_2;
      int __gen_e_acsl_and_3;
      int __gen_e_acsl_implies_3;
      if (! (x == 1)) __gen_e_acsl_implies = 1;
      else __gen_e_acsl_implies = x == 0;
      __e_acsl_assert(__gen_e_acsl_implies,(char *)"Precondition",
                      (char *)"main",(char *)"x == 1 ==> x == 0",35);
      if (x == 3) __gen_e_acsl_and_2 = y == 2; else __gen_e_acsl_and_2 = 0;
      if (! __gen_e_acsl_and_2) __gen_e_acsl_implies_2 = 1;
      else __gen_e_acsl_implies_2 = x == 3;
      __e_acsl_assert(__gen_e_acsl_implies_2,(char *)"Precondition",
                      (char *)"main",(char *)"x == 3 && y == 2 ==> x == 3",
                      39);
      if (x == 3) __gen_e_acsl_and_3 = y == 2; else __gen_e_acsl_and_3 = 0;
      if (! __gen_e_acsl_and_3) __gen_e_acsl_implies_3 = 1;
      else __gen_e_acsl_implies_3 = x + (long)y == 5L;
      __e_acsl_assert(__gen_e_acsl_implies_3,(char *)"Precondition",
                      (char *)"main",
                      (char *)"x == 3 && y == 2 ==> x + y == 5",40);
    }
    x += y;
  }
  /*@ requires x ≡ 5; */
  {
    __e_acsl_assert(x == 5,(char *)"Precondition",(char *)"main",
                    (char *)"x == 5",43);
    /*@ requires y ≡ 2; */
    {
      __e_acsl_assert(y == 2,(char *)"Precondition",(char *)"main",
                      (char *)"y == 2",44);
      x += y;
    }
  }
  /*@ requires x ≡ 7;
      ensures x ≡ 7; */
  {
    __e_acsl_assert(x == 7,(char *)"Precondition",(char *)"main",
                    (char *)"x == 7",47);
    __retres = 0;
    goto return_label;
    __e_acsl_assert(x == 7,(char *)"Postcondition",(char *)"main",
                    (char *)"x == 7",48);
  }
  return_label: return __retres;
}



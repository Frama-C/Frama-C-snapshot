/* run.config*
   STDOPT: +"-wlevel 1"
*/

#include "__fc_builtin.h"

int test1(int n)
{
  int i = 0;
  while (1) {
    Frama_C_show_each_1("On exit path", i);
    if (i >= n) {
      Frama_C_show_each_2("On exit path", i);
      return i;
    }
    Frama_C_show_each_3("Not on exit path", i);
    i++;
  }
}

int test2(int n)
{
  int i = 0;
  while (1) {
    Frama_C_show_each_4("On exit path", i);
    int j = 0;
    while (1) {
      Frama_C_show_each_5("On exit path", i, j);
      if (j >= n) {
        Frama_C_show_each_6("Not on exit path", i, j);
        break;
      }
      if (i + j >= 2 * n) {
        Frama_C_show_each_7("On exit path", i, j);
        return i;
      }
      Frama_C_show_each_8("Not on exit path", i, j);
      j++;
    }
    Frama_C_show_each_9("Not on exit path", i);
    i++;
  }
}

int test3(int n)
{
  int i = 0;
  while (1) {
    Frama_C_show_each_10("On exit path", i);
    int j = 0;
    while (1) {
      Frama_C_show_each_11("On exit path", i, j);
      if (j >= n) {
        Frama_C_show_each_12("On exit path", i, j);
        break;
      }
      Frama_C_show_each_13("On exit path", i, j);
      j++;
    }
    if (i >= n) {
      Frama_C_show_each_14("On exit path", i);
      return i;
    }
    Frama_C_show_each_15("Not on exit path", i);
    i++;
  }
}

void main(void)
{
  test1(10);
  test2(10);
  test3(10);
}


/* run.config
   STDOPT:
*/
#include "share/builtin.h"

const char* static_str = "Hello World\n";
const char* zero_str = "abc\0\0\0abc";

#define TSZ 12
const char* tab_str[TSZ] = 
  {
    "" , // 0
    "a", // 1
    "aa" , // 2
    "aaa" , // 3
    "aaaa" , // 4
    "aaaaa" , // 5
    "aaaaaa" , // 6
    /* hole */
    "aaaaaaaaa" , // 9
    "aaaaaaaaaa" ,
    "aaaaaaaaaaa",
    "aaaaaaaaaaaa" ,
    "aaaaaaaaaaaaa" }; // 13

char unterminated_string[12] = "unterminated";

int main (int c) {
  const char* loc_str = "Bonjour Monde\n";
  char loc_char_array[5];
  size_t sz1,sz2,sz3,sz4,sz5, szu;
  int x = 0xabcdef00;
  int z = 0x12345600;
  int i;
  char *str;

  if (c & 1) 
    {
      szu = Frama_C_strlen(unterminated_string);
      Frama_C_dump_each();
    }

  str = Frama_C_nondet(0,1) ? static_str : loc_str;
  sz1 = Frama_C_strlen(str);  
  //@ assert(sz1 == 12) || (sz1 == 14);
  str = &x;
  str = Frama_C_nondet(0,1) ? str : str + 3;
  sz2 = Frama_C_strlen(str);
  //@ assert(sz2 == 0) ; // no, could also do an RTE
  i = Frama_C_interval(0,TSZ-1);
  str = tab_str[i];
  sz3 = Frama_C_strlen(str);
  //@ assert (sz3 >= 0) && (sz3 <= 13);
  loc_char_array[3] = '\0';
  sz4 = Frama_C_strlen(loc_char_array);
  //@ assert (sz4 >=0) && (sz4 <=3);
  sz5 = Frama_C_strlen(zero_str);
  //@ assert(sz5 == 3);
  return 0;
}

/* run.config*

*/

int main() {

  // String literals are lvalues
  char (*p)[4] = &("bar");
  //wchar_t (*q)[4] = &(L"foO"); // Does not work yet

  if((*p)[1] != 'a') return -1;
  //if((*q)[1] != 'o') {};
  return 0;
}

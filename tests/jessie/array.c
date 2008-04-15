int foo(int* arr, int n) {
   int i = 0;
   while (1) {
        if (i < n) {
	    arr[i] = 0;
        }
        i = i + 1;
   }
   return i;
}

void f(int i, int j, int k) {
  j = 0; k = 10;
  for (i = 0; i < 10; i++) {
    j++; k--;
  }
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array"
End:
*/

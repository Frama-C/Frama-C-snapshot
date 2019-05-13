/* run.config
   DONTRUN: test run by main.c
*/

struct s **	main3(
                 struct s *p1, struct s s2
                 )	;

// tests to avoid false positives
void f() {
  int (*false_positive)();
}

void g() {
  int i = 0;
  false_positive(i);
}

void h() {
  (void)false_positive((int)42);
}

//void false_positive();

 void false_positive(); // this is a "voluntary" false negative (space before):
                        // it allows us to avoid false positives more easily

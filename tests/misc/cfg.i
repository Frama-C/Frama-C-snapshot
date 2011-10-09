/* run.config
   DONTRUN: cannot find entry point: main
*/

struct bar {
     int x;
};
struct foo {
     struct bar b;
     int y;
};

int rand(void);

void f(void) {
     int t = rand();
     struct foo f = {
         .b = {
             .x = (t?2:3),
         },
         .y = 42
     };
     return;
}


/* run.config*
   DONTRUN:
*/

int Ato[100];
int Afrom[100];

void main(int count) {
  int*to = &Ato;
  int*from = &Afrom;
//@ assert count > 0 ;
switch (count % 8)  /* count > 0 assumed */
 {
   case 0:        do {  *to = *from++;
   case 7:              *to = *from++;
   case 6:              *to = *from++;
   case 5:              *to = *from++;
   case 4:              *to = *from++;
   case 3:              *to = *from++;
   case 2:              *to = *from++;
   case 1:              *to = *from++;
                     } while ((count -= 8) > 0);
 }
}

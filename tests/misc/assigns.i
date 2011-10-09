/* run.config
   OPT: -memory-footprint 1 -journal-disable -main F2 -lib-entry -out
*/

int G;

//@ assigns s[..];
void F1(char *s);

char T[100];

void F2(int c)
{
    if (c) F1(T);
}

/* run.config
   COMMENT: Test call to Callgraph.Uses.iter_on_callers/callees (through Inout)
   OPT: -inout
*/

/*@ assigns *p \from x; */
extern void f(int x, int *p);

int main(void)
{
    int *q;
    f(0, q);
    return 0;
}

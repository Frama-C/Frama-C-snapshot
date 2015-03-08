/* run.config
 OPT: -cpp-extra-args="-CC" -print
*/
#define X 1 /* multi-line

           comment */ + 2 + /*
             bla
           */ 3

#define Y 5 + \
          6 + \
          7

/*@ ensures \result == X+Y; */
int main(void)
{
    return X+Y;
}


/* run.config
   STDOPT: +"-slevel 20"
*/

/* Problem with Value's memory model, that does not guarantee that we call
  Cvalue.V.widen with two arguments that are guaranteed to be increasing.
  Csmith-found examples reduced by creduce. */

union {
    char f0;
    int f1;
    short f3;
} a;

int b, c, d;

void main1 () {
    for (;; d++)
    {
        d = 0;
        for (; d < 3; d++) {
            c = 0;
            for (; c < 1; c++);
            c = 0;
            for (; c < 2; c++);
            for (; a.f3 < 1; a.f3 = 1)
                --b;
            a.f0 = 0;
        }
        a.f1 = 1;
    }
}

union {
    int f0;
    int f4;
} u;

union U4 {
    int f0:13;
    char f2;
    int f4;
};

int g, i, j, k;

void main2 () {   
    union U4 u4 = 1;
    for (;; --u.f0)
    {
        i = 0;
        for (; i < 3; i++)
        {
            j = 0;
            for (; j < 2; j++);
        }
        g = fn1 ();
        k = fn2 () || 0;
        if (g)
        {   
            for (; u4.f2 = 0;);
            if (u.f4)
                break;
        }
        else
            for (u4.f4 = 0; u4.f4 < 38; ++u4.f4);
    }
}

void main() {
  main1();
  main2();
}

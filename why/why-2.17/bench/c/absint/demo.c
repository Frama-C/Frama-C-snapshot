int f(int);

int* g(int* p, int n, int d) {

    int* r;

    if (p == 0) {
        p = (int*)malloc(n * sizeof(int));
    }
    p += d;
    while (n--) {
        *p++ = 0; 
        if (f(*p)) {
            r = p;
        }
    }
    return r;
}

char* h(char* p, int s) {


    char* q;
    if (s > 0) {
        q = p+s;
        while (p < q) {
            p += 
                f(q-p);
        }
        return q;
    } else {
        q = (char*)malloc(s * sizeof(char));
        while (f(*q) > 0) {
            *q++ = '\0';
        }
        return q;
    }
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int -d demo.c" */
/* End: */

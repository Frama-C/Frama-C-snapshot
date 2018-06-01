struct foo
{
        unsigned        bar  : 16, : 0;
        unsigned        bla  : 11, : 1;
        unsigned        bli  : 4,  : 0;
};

unsigned f(struct foo s) { return s.bla; }

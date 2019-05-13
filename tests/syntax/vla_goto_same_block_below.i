volatile int nondet ;

int main() {

int i = 42;
if (nondet) goto toto; // KO: we are jumping over
                       // a constructor/destructor definition

char vla[i];

toto: ;

return 0;

}

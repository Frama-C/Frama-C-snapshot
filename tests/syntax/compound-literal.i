typedef struct {
    unsigned foo;
} Foo;

extern void process(int, Foo);

void foo(int dummy) {
        process(dummy++, (Foo) {.foo = 0});
}

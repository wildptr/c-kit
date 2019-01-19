#define f {__VA_ARGS__}
#define g(x) {x, __VA_ARGS__}
#define h0(...) {__VA_ARGS__}
#define h1(x, ...) {x, __VA_ARGS__}
#define fake(__VA_ARGS__) {__VA_ARGS__}
#define bad(__VA_ARGS__, ...) {__VA_ARGS__}
f;
g(1);
h0();
h0(1);
h0(1,2,3);
h1();
h1(1);
h1(1,2,3);
fake(1);
bad(1,2,3);

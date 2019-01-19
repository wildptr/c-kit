#define f(x) x(x)
f(
#ifdef f
f
#else
g
#endif
)

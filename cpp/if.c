#if 1 + 2 == 3
#define OK1
#elif 1
#undef OK1
#else
#undef OK1
#endif

#if 0
#undef OK2
#elif 1
#define OK2
#else
#undef OK2
#endif

#if 0
#undef OK3
#elif 0
#undef OK3
#else
#define OK3
#endif

#if defined OK1 && defined OK2 && defined OK3
pass
#endif

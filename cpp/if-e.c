#if 0+ /* #if with syntax error in condition */
#endif 0+ /* extra tokens */

#if 1
#else
#else /* duplicate #else */
#endif

/* #else/#elif/#endif without #if */
#elif 0
#else
#endif

/* #if with empty condition */
#if

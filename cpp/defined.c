#define id(x) x
/* this should evaluate to 1 */
#if !id(defined UNDEFINED)
"!id(defined UNDEFINED)";
#endif
/* GCC fails with error "operator 'defined' requires an identifier".
   GCC seems to be expanding macro arguments the normal way inside #if */
#if id(defined __STDC__)
"id(defined __STDC__)";
#endif

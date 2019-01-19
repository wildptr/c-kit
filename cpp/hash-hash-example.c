#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)

char p[] = join(x, y); 
/* char p[] = "x ## y"; */

#define t(x,y,z) x ## y ## z

int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),
            t(10,,), t(,11,), t(,,12), t(,,) };
/* int j[] = { 123, 45, 67, 89,
               10, 11, 12, }; */

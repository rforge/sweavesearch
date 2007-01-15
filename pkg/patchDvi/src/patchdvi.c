#include <R.h>
#include <Rdefines.h>

static int parmSizes[256] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 1, 2,
    	               3, 4, 8, 1, 2, 3, 4, 8, 0, 44,
    	               0, 0, 0, 1, 2, 3, 4, 0, 1, 2,
    	               3, 4, 0, 1, 2, 3, 4, 1, 2, 3, 
    	               4, 0, 1, 2, 3, 4, 0, 1, 2, 3,
    	               4, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	               0, 0, 0, 0, 0, 1, 2, 3, 4, 1,
    	               2, 3, 4,15,16,17,18,14,28, 5,
    	               9, 0, 0, 0, 0, 0, 0};
    	               
    	               
#define INIT_SIZE 1024

SEXP dviSpecials(SEXP dvi)
{
    char *bytes = RAW(dvi), *stop = RAW(dvi) + length(dvi);
    PROTECT_INDEX ires;
    SEXP result, c;
    int used = 0;
    
    PROTECT_WITH_INDEX(result = allocVector(STRSXP, INIT_SIZE), &ires);
    
    while (bytes < stop) {
    	int parmsize = parmSizes[*bytes];
    	int k = 0;
    	if (*bytes < 239) {
    	    /* do nothing */
    	} else if (*bytes < 243) {
    	    if (used >= length(result)) 
    	    	REPROTECT(result = lengthgets(result, 2*length(result)), &ires);
	    for (int i == 0; i < parmsize; i++) 
		k += *(bytes + i) << (8*i);
	    c = allocString(k); /* adds zero terminator */
	    if (c == R_NilValue) error("out of memory")
	    else {
		memcpy(CHAR(c), bytes + parmsize, k);
		SET_STRING_ELT(result, used++, c);
	    }
    	} else if (*bytes < 248) 
    	    k = *(bytes + parmsize - 1);
    	else if (*bytes == 249) break;
    	
    	bytes += parmsize + k;
    }
    result = lengthgets(result, used);
    UNPROTECT(1);
    return result;
}
    
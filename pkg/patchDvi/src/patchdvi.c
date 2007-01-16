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
    	               9, 0, 0, 0, 0, 0};
    	               
    	               
#define INIT_SIZE 1024

SEXP dviSpecials(SEXP dvi)
{
    unsigned char *bytes = RAW(dvi), *stop = RAW(dvi) + length(dvi);
    PROTECT_INDEX ires;
    SEXP result, c;
    int used = 0;
    
    PROTECT_WITH_INDEX(result = allocVector(STRSXP, INIT_SIZE), &ires);
    
    while (bytes < stop) {
    	int parmsize = parmSizes[(int)*bytes];
    	int k = 0;
    	if ((int)(*bytes) < 239) {
    	    /* do nothing */
    	} else if ((int)(*bytes) < 243) {
    	    if (used >= length(result)) 
    	    	REPROTECT(result = lengthgets(result, 2*length(result)), ires);
	    for (int i = 0; i < parmsize; i++) 
		k = (k << 8) + *(bytes + i + 1);
	    c = allocString(k); /* adds zero terminator */
	    if (c == R_NilValue) error("out of memory");
	    else {
		memcpy(CHAR(c), bytes + 1 + parmsize, k);
		SET_STRING_ELT(result, used++, c);
	    }
    	} else if ((int)(*bytes) < 248) 
    	    k = *(bytes + parmsize);
    	else if ((int)(*bytes) == 249) break;
    	
    	bytes += 1 + parmsize + k;
    }
    result = lengthgets(result, used);
    UNPROTECT(1);
    return result;
}

#define DVI_NOP 138

SEXP setDviSpecials(SEXP dvi, SEXP specials)
{
    unsigned char *bytes = RAW(dvi), *stop = RAW(dvi) + length(dvi);
    int used = 0;
    int k, parmsize, len;
    SEXP c;
    
    while (bytes < stop) {
    	parmsize = parmSizes[(int)*bytes];
    	k = 0;
    	if ((int)(*bytes) < 239) {
    	    /* do nothing */
    	} else if ((int)(*bytes) < 243) {
    	    if (used >= length(specials)) {
    	    	warning("More specials than values; only partial substitution");
    	    	break;
    	    }
	    for (int i = 0; i < parmsize; i++) 
		k = (k << 8) + *(bytes + i + 1);
	    if ((c = STRING_ELT(specials, used)) != NA_STRING) {
	    	if ((len = length(STRING_ELT(specials, used))) > k) 
	    	    error("Special %d too long", used);
	    	memcpy(bytes + 1 + parmsize, CHAR(c), len);
	    	for (int i = len; i < k; i++)
	    	    *(bytes + 1 + parmsize + i) = DVI_NOP;
	    }
    	} else if ((int)(*bytes) < 248) 
    	    k = *(bytes + parmsize);
    	else if ((int)(*bytes) == 249) break;
    	
    	bytes += 1 + parmsize + k;
    }
    return dvi;
}

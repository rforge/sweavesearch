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
    	               0, 0, 0, 0, 0, 0};
    	               
    	               
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

SEXP setDviSpecials(SEXP dvi_in, SEXP specials)
{
    unsigned char *bytes = RAW(dvi_in), *output, 
                  *stop = RAW(dvi_in) + length(dvi_in), *src;
    
    int opcode;
    int used = 0;
    int skew = 0, prevskew = 0;
    int bufsize = length(dvi_in);
    
    int kin, kout, parmsize, outsize = 0;
    int recin = 0, recout = 0;
    int prevptr, offset;
    
    PROTECT_INDEX ires;    
    SEXP c, dvi_out;
    
    PROTECT_WITH_INDEX(dvi_out = allocVector(RAWSXP, bufsize), &ires);    
    output = RAW(dvi_out);
 
 #define RESIZEOUT(newsize) (REPROTECT(dvi_out = lengthgets(dvi_out, bufsize = (newsize)), ires),\
                             output = RAW(dvi_out))
                             
 #define PUTBYTES(src, size) (outsize + (size) > bufsize ? RESIZEOUT(outsize + (size) + 256) : 0,\
                              memcpy(output + outsize, (src), (size)), outsize += (size))
 
    while (bytes < stop) {
        opcode = (int)*(bytes + recin);
    	parmsize = parmSizes[opcode];
    	PUTBYTES(bytes + recin, 1 + parmsize);
    	
    	kin = kout = 0;
	if (opcode == 139  /* bop */ 
           || opcode == 248  /* post */
    	   || opcode == 249) { /* post_post */
    	        
    	    if (opcode == 139) offset = 41;
    	    else offset = 1;
    	    
    	    if (prevskew) {
            	prevptr = 0;
            	for (int i = 0; i < 4; i++)
	    	    prevptr = (prevptr << 8) + *(bytes + recin + offset + i);    	
	    	prevptr += prevskew;
	    	for (int i = 0; i < 4; i++) 
	    	    *(output + recout + offset + i) = (prevptr >> (8*(3-i))) & 0xFF;
	    }
	    prevskew = skew;
	} else if (opcode < 239) {
    	    /* do nothing */
    	} else if (opcode < 243) {
    	    if (used >= length(specials)) {
    	    	warning("not enough new specials in setDviSpecials");
    	    	break;
    	    }
	    for (int i = 0; i < parmsize; i++) 
		kin = (kin << 8) + *(bytes + recin + i + 1);
	    if ((c = STRING_ELT(specials, used++)) != NA_STRING) {
	    	kout = length(c);
	    	src = CHAR(c);
	    } else {
	    	kout = kin;
	    	src = bytes + recin + 1 + parmsize;
	    }
	    PUTBYTES(src, kout);
	    
	    if (kout != kin) {
	    	skew += kout - kin;
	    	for (int i = 0; i < parmsize; i++) 
	            *(output + recout + i + 1) = (kout >> (8*(parmsize-i-1))) & 0xFF;
	    }
    	} else if (opcode < 248) {
    	    kin = kout = *(bytes + recin + parmsize);
    	    PUTBYTES(bytes + recin + 1 + parmsize, kout);
	}
	    
    	recin += 1 + parmsize + kin;
    	recout += 1 + parmsize + kout;
    	
    	if (opcode == 249) {
    	    if ((kout = stop - bytes - recin)) 
    	    	PUTBYTES(bytes + recin, kout);
    	    while (outsize & 3)	/* Must be multiple of 4 bytes long */
    	    	PUTBYTES(bytes + recin, 1);
	    if (outsize < bufsize) RESIZEOUT(outsize);
    	    break;
    	}
    }
    if (used < length(specials))
    	error("too many new specials");
    UNPROTECT(1);
    return dvi_out;
}

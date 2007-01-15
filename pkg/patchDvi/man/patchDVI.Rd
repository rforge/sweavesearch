\name{patchDVI}
\alias{patchDVI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
patchDVI(f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{ ~~Describe \code{f} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(f) {
    concords <- list()

    size <- file.info(f)$size
    con <- file(f, "rb")
    bytes <- readBin(con, "raw", size)
    close(con)
        
    parmsizes <- c( rep(0, 128), 1:4, 8, 1:4, 8,   # 0 to 137
                    0, 44, 0,0,0, 1:4, 0, 1:4, 0,  # 138 to 152
                    1:4, 1:4, 0, 1:4, 0, 1:4, 	   # 153 to 170
                    rep(0, 64), 1:4, 1:4, 15:18,   # 171 to 246
                    14, 28, 5, 9, rep(NA, 6))      # 247 to 255
                    
    pos <- 0
    opcode <- 0
    hits <- 0
    misses <- 0
    while (pos < size && opcode != 249) {
        pos <- pos+1
    	opcode <- as.integer(bytes[pos])
    	parmsize <- parmsizes[opcode + 1]
    	
    	if (opcode < 239) { # do nothing 
    	} else if (opcode \%in\% 239:242) {   # xxxi
    	    k <- readBin(bytes[pos + (1:parmsize)], "integer", size=parmsize, endian="big")
    	    special <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + k)], k)

	    if (length(grep("^concordance:", special))) {
	    	special <- strsplit(special, ":")[[1]]
	    	concordance <- cumsum(as.integer(strsplit(special[4], " ")[[1]]))
	    	keep <- !duplicated(concordance)
	    	concord <- approxfun(concordance[keep], seq(along=concordance)[keep], "constant", rule=2)
	    	concords[[special[2]]] <- list(newname=special[3], concord=concord)
	    }
    	    if (length(grep("^src:", special))) {
    	    	filestart <- regexpr("[^0-9]*$", substr(special, 5, 10000))
    	    	concord <- which(names(concords) == substr(special, 4 + filestart, 10000))
    	    	if (filestart > 1 && length(concord)) {
    	    	    newstart <- substr(special, 5, 3 + filestart)
    	    	    newspecial <- paste("src:", 
    	    	                 concords[[concord]]$concord(as.integer(newstart)),
    	    	                 concords[[concord]]$newname, sep="")
    	    	    if ((knew <- nchar(newspecial)) > k) {
    	    	    	# bad news:  the patch won't fit!!
    	    	    	warning(paste(special, "can't be changed to", newspecial))
    	    	    	bytes[pos:(pos+parmsize+k)] <- as.raw(138) # nop
    	    	    	misses <- misses + 1
    	    	    } else {
    	    	    	bytes[pos + 1:parmsize] <- writeBin(knew, bytes, "integer", size=parmsize)
    	    	    	bytes[pos + parmsize + 1:knew] <- writeChar(newspecial, bytes,, NULL)
    	    	    	if (knew < k)
    	    	    	    bytes[pos + parmsize + knew + 1:(k-knew)] <- as.raw(138)
    	    	    	hits <- hits + 1
    	    	    }
    	    	} 
    	    }	    	
      	    parmsize <- parmsize + k  	    	     	
    	} else if (opcode \%in\% 243:246) { # fnt def i
    	    a <- as.integer(bytes[pos + parmsize ])
    	    parmsize <- parmsize + a 
    	} else if (opcode == 247) {       # pre
    	    k <- as.integer(bytes[pos + parmsize])
    	    parmsize <- parmsize + k
    	}
	pos <- pos + parmsize
    }
    writeBin(bytes, paste("new",f,sep="")) 	
    c(hits=hits, misses=misses)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

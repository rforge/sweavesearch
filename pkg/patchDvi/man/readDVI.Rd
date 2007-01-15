\name{readDVI}
\alias{readDVI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readDVI(f)
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
    while (pos < size) {
        pos <- pos+1
    	opcode <- as.integer(bytes[pos])
    	parmsize <- parmsizes[opcode + 1]
    	
    	if (opcode < 239L) { # do nothing 
    	} else if (opcode < 243L) {   # xxxi
    	    k <- readBin(bytes[pos + (1:parmsize)], "integer", size=parmsize, endian="big")
    	    special <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + k)], k)
    	    cat("special at pos=", pos," is ", special, "\n")
    	    
    	    parmsize <- parmsize + k
    	} else if (opcode < 247L) { # fnt def i
    	    a <- as.integer(bytes[pos + parmsize ])
    	    fntname <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + a)], a)
    	    cat("fnt def at pos=", pos, " is ", fntname, "\n")
    	    parmsize <- parmsize + a 
    	} else if (opcode == 247L) {       # pre
    	    k <- as.integer(bytes[pos + parmsize])
    	    comment <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + k)], k)
    	    cat("preamble at pos=", pos, " with comment ", comment, "\n")
    	    parmsize <- parmsize + k
    	} else if (opcode == 249L) break
	pos <- pos + parmsize
    }
    	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

SweaveMiktex <- function(Rnw, 
                         main=outputname, 
                         cmd="texify",
                         options="--tex-option=-c-style-errors --tex-option=--src-specials",
                         includedir="--tex-option=--include-directory=",
                         stylepath=FALSE,
                         source.code=NULL,
                         ...) {
    if (!is.null(source.code))
    	try(source(source.code, local=TRUE))
    if (sub(".*\\.tex$", "TeX", Rnw, ignore.case = TRUE) == "TeX") 
    	outputname <- Rnw
    else
    	outputname <- Sweave(Rnw, stylepath=stylepath, ...)
    cmd <- paste(cmd, " ", options, " ", includedir,
                 file.path(R.home("share"), "texmf "),
                 main, sep="")    	
    cat(cmd, "\n")
    result <- system(cmd, intern=FALSE, show=TRUE)
    if (result != 0) Sys.sleep(5)
    patchDVI(sub("\\.tex", ".dvi", main, ignore.case = TRUE))
}

readDVI <- function(f, show=c("bop", "special", "fntdef", "preamble")) {
    size <- file.info(f)$size
    con <- file(f, "rb")
    bytes <- readBin(con, "raw", size)
    close(con)
    
    parmsizes <- c( rep(0, 128), 1:4, 8, 1:4, 8,   # 0 to 137
                    0, 44, 0,0,0, 1:4, 0, 1:4, 0,  # 138 to 152
                    1:4, 1:4, 0, 1:4, 0, 1:4, 	   # 153 to 170
                    rep(0, 64), 1:4, 1:4, 15:18,   # 171 to 246
                    14, 28, 5, 9, rep(NA, 5))      # 247 to 255
                    
    pos <- 0
    opcode <- 0
    while (pos < size) {
        pos <- pos+1
    	opcode <- as.integer(bytes[pos])
    	parmsize <- parmsizes[opcode + 1]
    	
    	if (opcode < 139L) { # do nothing
    	} else if (opcode == 139L && "bop" %in% show) {  # bop
    	    counters <- readBin(bytes[pos + 1:(parmsize-4)], "integer", n= 10, size=4, endian="big")
    	    prev <- readBin(bytes[pos + 40 + 1:4], "integer", size=4, endian="big")
    	    cat("bop at pos=", pos, ": counters=", paste(counters, collapse=" "), " prev=", prev, "\n")
    	} else if (opcode < 239L) { # do nothing 
    	} else if (opcode < 243L) {   # xxxi
    	    k <- readBin(bytes[pos + (1:parmsize)], "integer", size=parmsize, endian="big")
    	    if ("special" %in% show) {
    	    	special <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + k)], k)
    	    	cat("special at pos=", pos," is ", special, "\n")
    	    }
    	    parmsize <- parmsize + k
    	} else if (opcode < 247L) { # fnt def i
    	    a <- as.integer(bytes[pos + parmsize ])
    	    if ("fntdef" %in% show) {
     	    	fntname <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + a)], a)
   	    	cat("fnt def at pos=", pos, " is ", fntname, "\n")
   	    }
    	    parmsize <- parmsize + a 
    	} else if (opcode == 247L) {       # pre
    	    k <- as.integer(bytes[pos + parmsize])
	    if ("preamble" %in% show) {
    	    	comment <- readChar(bytes[(pos + parmsize + 1):(pos + parmsize + k)], k)        	    
    	    	cat("preamble at pos=", pos, " with comment ", comment, "\n")
	    }
    	    parmsize <- parmsize + k
    	} else if (opcode == 249L) break
	pos <- pos + parmsize
    }
}

DVIspecials <- function(f) {
    size <- file.info(f)$size
    con <- file(f, "rb")
    bytes <- readBin(con, "raw", size)
    close(con)
    .Call(dviSpecials, bytes)
}

setDVIspecials <- function(f, newspecials, newname=f) {
    size <- file.info(f)$size
    con <- file(f, "r+b")
    on.exit(close(con))
    bytes <- readBin(con, "raw", size)
    bytes <- .Call(setDviSpecials, bytes, as.character(newspecials))
    close(con)
    con <- file(newname, "wb")
    writeBin(bytes, con)
}

patchDVI <- function(f, newname=f) {
    specials <- DVIspecials(f)
    
    concordind <- grep("^concordance:", specials)
    concords <- specials[concordind]
    
    parseConcord <- function(split) {
    	oldname <- split[2]
    	newname <- split[3]
    	values <- as.integer(strsplit(split[4], " ")[[1]])
    	firstline <- values[1]
    	rledata <- matrix(values[-1], nrow=2)
    	rle <- structure(list(lengths=rledata[1,], values=rledata[2,]), class="rle")
    	diffs <- inverse.rle(rle)
	concord <- c(firstline, firstline + cumsum(diffs))
    	list(oldname=oldname, newname=newname, concord=concord)
    }
    concords <- strsplit(concords, ":")
    concords <- lapply(concords, parseConcord)
    names(concords) <- sapply(concords, function(x) x$oldname)


    srcrefind <- grep("^src:", specials)
    srcrefs <- specials[srcrefind]
    
    linenums <- sub("^src:([0-9]+).*$", "\\1", srcrefs)
    filenames <- normalizePath(substr(srcrefs, 5+nchar(linenums), 10000))
    changed <- rep(FALSE, length(filenames))
    for (n in names(concords)) {
    	subset <- filenames == normalizePath(n)
    	linenums[subset] <- concords[[n]]$concord[as.integer(linenums[subset])]
    	filenames[subset] <- concords[[n]]$newname
    	changed[subset] <- TRUE
    }
    
    newrefs <- ifelse(changed, paste("src:", linenums, filenames, sep=""), srcrefs)
    
    specials[srcrefind] <- newrefs
    specials[concordind] <- NA
    
    if (any(changed) || length(concordind))
    	setDVIspecials(f, specials, newname)
    	
    paste(sum(changed) + length(concordind), "patches made.")
}


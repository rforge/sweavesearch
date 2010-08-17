SweavePDFMiktex <- function( Rnw, main=outputname,  
                             cmd="texify --pdf", 
                             options="--tex-option=-c-style-errors --tex-option=-synctex=-1",
                             includedir="--tex-option=-include-directory=",
                             stylepath=FALSE,
                             source.code=NULL,
                             make=1,
                             preview=NULL,
                             ...) {
    if (!is.null(source.code) && file.exists(source.code))
    	try(source(source.code, local=TRUE))
    if (sub(".*\\.tex$", "TeX", Rnw, ignore.case = TRUE) == "TeX") 
    	outputname <- Rnw
    else
    	outputname <- SweaveAll(Rnw, make=make, stylepath=stylepath, ...)[1]
    
    cmd <- paste(cmd, " ", options, " ", includedir,
                 file.path(R.home("share"), "texmf "),
                 main, sep="")
    cat(cmd, "\n")
    result <- system(cmd, intern=FALSE, show=TRUE)
    if (result != 0) Sys.sleep(5)
    patchSynctex(sub("\\.tex$", ".synctex", main, ignore.case = TRUE)) 
    pdf <- sub("\\.tex$", ".pdf", main, ignore.case = TRUE)
    if (!is.null(preview)) {
    	cmd <- sprintf(preview, shQuote(pdf))
    	cat(cmd, "\n")
    	system(cmd, wait=FALSE, invisible=FALSE)
    }
    
}

SweavePDF <- function( Rnw, main=outputname,
                       texinputs=NULL,
                       source.code=NULL,
                       make=1,
                       links = NULL,
                       preview = NULL,
                       ... ) {
    if (!is.null(source.code) && file.exists(source.code))
    	try(source(source.code, local=TRUE))
    if (sub(".*\\.tex$", "TeX", Rnw, ignore.case = TRUE) == "TeX") 
    	outputname <- Rnw
    else
    	outputname <- SweaveAll(Rnw, make=make, ...)[1]
    texi2dvi(main, pdf=TRUE, texinputs=texinputs, links=links)
    patchSynctex(sub("\\.tex$", ".synctex", main, ignore.case=TRUE))
    pdf <- sub("\\.tex$", ".pdf", main, ignore.case = TRUE)
    if (!is.null(preview)) {
    	cmd <- sprintf(preview, shQuote(pdf))
    	cat(cmd, "\n")
    	system(cmd, wait=FALSE, invisible=FALSE)
    }   
}

rawToLines <- function(raw) {
    temp <- tempfile()
    on.exit(unlink(temp))
    writeBin(raw, temp)
    readLines(temp)
}

pdfEOF <- function(con) {
    seek(con, -1024, "end")
    tail <- rawToChar(readBin(con, "raw", 1024), multiple=TRUE)
    stop <- rev(which(tail == "F"))
    for (last in stop) {
	if (all(tail[last - 4:0] == c("%", "%", "E", "O", "F")))
	    return( last - 4 + seek(con, 0) - 1024 - 1 )
    }	    
    stop("%%EOF marker not found")
}

pdfStartxrefs <- function(con, eof=pdfEOF(con)) {
    seek(con, eof - 20)
    tail <- rawToLines(readBin(con, "raw", 20))
    startxref <- which(tail == "startxref")
    as.numeric(tail[startxref+1])
}

pdfXrefblock <- function(con, start=pdfStartxrefs(con, eof), eof=pdfEOF(con)) {
    seek(con, start)
    xrefs <- rawToLines(readBin(con, "raw", eof - start))
    trailer <- which(xrefs == "trailer")[1]
    tail <- xrefs[trailer:length(xrefs)]
    xrefs <- xrefs[1:(trailer-1)]
    line <- 2
    offsets <- numeric(0)
    generations <- numeric(0)
    free <- logical(0)
    maxobj <- 0
    while (line <= length(xrefs)) {
	  head <- scan(temp <- textConnection(xrefs[line]), 
	               what=list(first=0, count=0),
	               quiet = TRUE)
	  close(temp)
	  newmax <- head$first + head$count 
	  if (newmax > maxobj) {
	      length(offsets) <- newmax
	      length(generations) <- newmax
	      length(free) <- newmax
	  }
	  if (head$count > 0) {
	      body <- scan(temp <- textConnection(xrefs[line + 1:head$count]),
	                   what = list(offsets=0, generations=0, free="n"),
	                   quiet = TRUE)
	      close(temp)
	      inds <- head$first + 1:head$count
	      offsets[inds] <- body$offsets
	      generations[inds] <- body$generations
	      free[inds] <- body$free == "f"
	  }
	  line <- line + head$count + 1
    }
    # Now clean up the tail dictionary
    stop <- which(tail == "startxref")[1]
    tail <- tail[2:(stop-1)]
    tail[1] <- sub("^<< *", "", tail[1])
    tail[length(tail)] <- sub(" *>>$", "", tail[length(tail)])    
    list(xrefs = data.frame(offsets = offsets, generations = generations, free=free),
         dict = tail)
         
}

dictVal <- function(dict, key) {
    dict <- unlist(strsplit(paste(dict, collapse=" "), "/"))
    dict <- grep(paste("^", key, sep=""), dict, value=TRUE)
    return(substr(dict, nchar(key)+1, nchar(dict)))
}

pdfXrefblocks <- function(con, collapse = TRUE) {
    result <- list()
    eof <- pdfEOF(con)
    start <- pdfStartxrefs(con, eof)
    rows <- NULL
    repeat {
        block <- pdfXrefblock(con, start, eof)
        if (collapse) {
            if (is.null(rows)) {
		rows <- as.numeric(dictVal(block$dict, "Size"))
		NAs <- rep(NA_real_, rows)
		xref <- data.frame(offsets = NAs, generations = NAs, free= as.logical(NAs))
	    }
            replace <- ((1:rows) <= nrow(block$xrefs)) & is.na(xref$offsets)
            xref[replace,] <- block$xrefs[replace,]
       	} else
            result <- c(result, list(block))
        start <- as.numeric(dictVal(block$dict, "Prev"))
        if (!length(start)) break
    }
    if (collapse) 
        return(xref)
    else
        return(result)
}

pdfFindobj <- function(con, pattern) {
    xrefs <- pdfXrefblocks(con)
    xrefs <- subset(xrefs, !xrefs$free)
    o <- order(xrefs$offsets)
    xrefs <- xrefs[o,]
    result <- character(0)
    for (i in 1:nrow(xrefs)) {
    	seek(con, xrefs$offsets[i])
    	head <- readChar(con, nchar(pattern) + 30)
    	head <- sub("^[[:space:][:digit:]]*obj[[:space:]]*", "", head)
    	if (substr(head, 1, nchar(pattern)) == pattern) {
            seek(con, xrefs$offsets[i])
            obj <- rawToLines(readBin(con, "raw", xrefs$offsets[i+1]-xrefs$offsets[i]))
            # FIXME:  this will match any endobj, not just the one we want
            stop <- grep("endobj", obj)
            obj <- paste(obj[1:stop], collapse=" ")
            obj <- sub("^[[:space:][:digit:]]*obj[[:space:]]*", "", obj)
            obj <- sub("[[:space:]]*endobj.*", "", obj)
            result <- c(result, obj)
        }
    }
    result
}

pdfobjs <- function(file, pattern) {
    if (is.character(file)) {
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!isOpen(file, "rb")) {
        open(file, "rb")
        on.exit(close(file))
    }
    pdfFindobj(file, pattern)
}

syncFiles <- function(lines) {
    inputs <- grep("^Input:", lines)
    filenames <- sub("^Input:[[:digit:]]*:","",lines[inputs])
    filepaths <- dirname(filenames)
    filenames <- basename(filenames)
    nodot <- grep("^[^.]*$", filenames)
    filenames[nodot] <- paste(filenames[nodot], ".tex", sep="")
    filenums <- sub("^Input:","",lines[inputs])
    filenums <- as.numeric(sub(":.*","",filenums))
    o <- order(filenums)
    data.frame(tag=filenums[o], path=filepaths[o], name=filenames[o])
}

parseConcords <- function(lines) {
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
    concords <- strsplit(lines, ":")
    concords <- lapply(concords, parseConcord)
    names(concords) <- sapply(concords, function(x) x$oldname)
    concords
}

patchSynctex <- function(f, newname=f) {
    compressed <- FALSE
    if (!file.exists(f)) {
    	f <- paste(f, ".gz", sep="")
    	if (file.exists(f)) {
    	    compressed <- TRUE
    	    force(newname)
    	    f <- gzfile(f)
    	}
    }
    lines <- try(readLines(f), silent=TRUE)
    if (inherits(lines, "try-error")) {
    	message(f," cannot be read, no patching done.")
    	return()
    }
    files <- syncFiles(lines)
    pdfname <- file.path(files$path[1], paste(sub(".tex", "", files$name[1]), ".pdf", sep=""))
    concords <- parseConcords(pdfobjs(pdfname, "concordance:"))

    re <- "^([vhxkgr$[(])([[:digit:]]+),([[:digit:]]+)([^[:digit:]].*)"
    srcrefind <- grep(re, lines)
    srcrefs <- lines[srcrefind]
    
    ops <- sub(re, "\\1", srcrefs)
    tags <- sub(re, "\\2", srcrefs)
    linenums <- sub(re, "\\3", srcrefs)
    rest <- sub(re, "\\4", srcrefs)
     
    changed <- rep(FALSE, length(tags))
    newtags <- c()
    maxtag <- max(files$tag)
    for (n in names(concords)) {
        maxtag <- maxtag + 1
        newtags <- c(newtags, maxtag)
        names(newtags)[length(newtags)] <- concords[[n]]$newname
        tag <- files$tag[files$name == n]
        if (length(tag) == 1) {
    	    subset <- tags == tag
    	    linenums[subset] <- concords[[n]]$concord[as.integer(linenums[subset])]
    	    tags[subset] <- newtags[concords[[n]]$newname]
    	    changed[subset] <- TRUE
    	}
    }
    
    if (any(changed)) {
    	newrefs <- ifelse(changed, paste(ops, tags, ",", linenums, rest, sep=""), srcrefs)
    	lines[srcrefind] <- newrefs
    	firstInput <- grep("^Input:", lines)[1]
    	lines <- c(lines[1:firstInput],
    	           paste("Input:", newtags, ":", names(newtags), sep=""),
    	           lines[(firstInput+1):length(lines)])
    }
    writeLines(lines, if (compressed) gzfile(newname) else newname)
    changes <- sum(changed) + length(newtags)
    msg <- paste(changes, "patches made.") 
    if (!changes)
    	msg <- paste(msg, "Did you set \\SweaveOpts{concordance=TRUE}?")
    msg
}

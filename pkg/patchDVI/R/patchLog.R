# Patch a latex log file.  This borrows from logParser.js from TeXWorks,
# which had this author information and a GPL license.

#// TeXworksScript
#// Title: Errors, warnings, badboxes
#// Description: Looks for errors, warnings or badboxes in the LaTeX terminal output
#// Author: Jonathan Kew, Stefan Löffler, Antonio Macrì, Henrik Skov Midtiby
#// Version: 0.8.2
#// Date: 2012-03-26

patchLog <- function(f, newname=f, concords = NULL, max_print_line = 79) {

    force(newname)
    lines <- readLines(f)
    
    # These patterns recognize all errors generated with \errmessage,
    # that is, starting with "!" and containing "l.\d+".
    # The macro \GenericError uses \errmessage internally.
    # Macros \@latex@error and \(Class|Package)Error use \GenericError.
    
    errormsg <- regexpr("^!\\s+", lines)
    errorline <- regexpr("l\\.(\\d+)", lines, perl = TRUE)
    
    badbox <- regexpr("^(?:Under|Over)full \\\\[hv]box\\s*\\([^)]+\\) in paragraph at lines (\\d+)--(\\d+)$",
                      lines, perl = TRUE)
    
    filestart <- gregexpr('\\("((?:\\./|/|\\.\\\\|[a-zA-Z]:\\\\|[a-zA-Z]:/|\\\\\\\\)(?:[^"]|$)+)"|\\(((?:\\./|/|\\.\\\\|[a-zA-Z]:\\\\|[a-zA-Z]:/|\\\\\\\\)[^ ()$]+)',
		       lines, perl = TRUE)
    lineswithfile <- which(sapply(filestart, function(x) length(x) > 1 || x != -1))
    filestartline <- rep.int(lineswithfile, sapply(filestart[lineswithfile], length))
    filestartcol <- unlist(lapply(filestart[lineswithfile], function(x) attr(x, "capture.start")[,2]))
    filenamelen <- unlist(lapply(filestart[lineswithfile], function(x) attr(x, "capture.length")[,2]))
    filename <- substr(lines[filestartline], filestartcol, filestartcol + filenamelen - 1)

    realnames <- unique(filename)
    realnames <- realnames[file.exists(realnames)]
    
    keep <- filename %in% realnames
    filestartline <- filestartline[keep]
    filestartcol <- filestartcol[keep]
    filenamelen <- filenamelen[keep]
    filename <- myNormalizePath(filename[keep])

    openparen <- gregexpr("(", lines, fixed = TRUE)
    closeparen <- gregexpr(")", lines, fixed = TRUE)

    # Need to ignore the text in badbox warnings which may contain parens
    badboxtext <- which(badbox != -1) + 1
    lastline <- badboxtext
    repeat {
	continued <- nchar(lines[lastline], type="bytes") >= 79	
	lastline <- setdiff(lastline[continued] + 1, badboxtext)
	if (!length(lastline)) break
	badboxtext <- union(badboxtext, lastline)
    }

    openparen[badboxtext] <- list(-1)
    closeparen[badboxtext] <- list(-1)

    lineswithopen <- which(sapply(openparen, function(x) length(x) > 1 || x != -1))
    lineswithclose <- which(sapply(closeparen, function(x) length(x) > 1 || x != -1))

    openlines <- rep.int(lineswithopen, sapply(openparen[lineswithopen], length))
    closelines <- rep.int(lineswithclose, sapply(closeparen[lineswithclose], length))

    opencols <- do.call(c, openparen[lineswithopen])
    closecols <- do.call(c, closeparen[lineswithclose])

    parenlocs <- rbind( cbind(openlines, opencols, 1), cbind(closelines, closecols, -1) )
    o <- order(parenlocs[,1], parenlocs[,2])
    parenlocs <- parenlocs[o,]
    parenlines <- parenlocs[,1]
    parencols <- parenlocs[,2]

    #parendepth is the parenthesis depth just after that character
    parendepth <- cumsum(parenlocs[,3])

    fileineffect <- character(length(lines))
    inds <- seq_along(parendepth)

    for (i in seq_along(filename)) {
	j <- which(parenlines == filestartline[i] & parencols + 1 == filestartcol[i])
	depth <- parendepth[j]
	popped <- which(inds > j & parendepth == depth - 1)

	fileendline <- if (length(popped)) parenlines[min(popped)] else length(lines)
	fileineffect[filestartline[i]:fileendline] <- filename[i]
    }
    
    if (!length(concords)) { # The run failed, so get them from the source
        concordance <- grepl("-concordance.tex$", filename)
        if (any(concordance))
    	    concords <- readConcords( unique(filename[concordance]) )
    }
    
    if (length(concords)) {
	# Now start patching.
	names(concords) <- myNormalizePath(names(concords), mustWork = FALSE)
	patchable <- fileineffect %in% names(concords)

	# First, the badbox lines
	dofix <- which(patchable & (badbox != -1))
	if (length(dofix)) {
	    firststart <- attr(badbox, "capture.start")[dofix,1]
	    firststop <- firststart + attr(badbox, "capture.length")[dofix,1] - 1
	    secondstart <- attr(badbox, "capture.start")[dofix, 2]
	    secondstop  <- secondstart + attr(badbox, "capture.length")[dofix, 2] - 1
	    first <- as.numeric(substr(lines[dofix], firststart, firststop))
	    second <- as.numeric(substr(lines[dofix], secondstart, secondstop))
	    for (f in unique(fileineffect[dofix])) {
		thisfile <- fileineffect[dofix] == f
		first[thisfile] <- concords[[f]]$concord[first[thisfile]]
		second[thisfile] <- concords[[f]]$concord[second[thisfile]]
	    }
	    mysubstr(lines[dofix], secondstart, secondstop) <- as.character(second)
	    mysubstr(lines[dofix], firststart, firststop) <- as.character(first)
	}

	# Now, the error lines
	dofix <- which(patchable & (errorline != -1))
	if (length(dofix)) {
	    start <- attr(errorline, "capture.start")[dofix]
	    stop <- start + attr(errorline, "capture.length")[dofix] - 1
	    num <- as.numeric(substr(lines[dofix], start, stop))
	    for (f in unique(fileineffect[dofix])) {
		thisfile <- fileineffect[dofix] == f
		num[thisfile] <- concords[[f]]$concord[num[thisfile]]
	    }
	    mysubstr(lines[dofix], start, stop) <- as.character(num)
	}

	# And finally, the filenames
	dofix <- filename %in% names(concords)
	if (any(dofix)) {
	    start <- filestartcol[dofix]
	    stop <- start + filenamelen[dofix] - 1
	    withPath <- function(s) {
		okay <- grepl("[/\\]", s)
		s[!okay] <- paste0("./", s[!okay])
	    }
	    mysubstr(lines[filestartline[dofix]], start, stop) <- sapply(concords[filename[dofix]], 
	                                      function(x) withPath(x$newsrc))
	}
    }
    writeLines(lines, newname)
    invisible(concords)
}

`mysubstr<-` <- function(x, start, stop, value) 
    paste0(substr(x, 1, start-1), value, substr(x, stop+1, nchar(x)))
    
myNormalizePath <- function(f, mustWork = NA) {
    filename <- normalizePath(f, mustWork = mustWork)
    if (.Platform$OS.type == "windows") {
    	upperdrive <- grepl("^[[:upper:]]:", filename)
    	substr(filename[upperdrive], 1,1) <- tolower(substr(filename[upperdrive], 1,1))
    }
    filename
}

readConcords <- function(files) {
    concords <- character(length(files))
    for (i in seq_along(files)) {
    	lines <- readLines(files[i])
    	if (grepl("\\Sconcordance{", lines[1], fixed=TRUE)) {
    	    lines[1] <- sub("\\Sconcordance{", "", lines[1], fixed = TRUE)
    	    lines[length(lines)] <- sub("}$", "", lines[length(lines)])
    	    lines <- sub("%$", "", lines)
    	    concords[i] <- paste(lines, collapse="")
    	}
    }
    parseConcords(concords)
}
    
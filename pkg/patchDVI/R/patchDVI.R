SweaveMiktex <- function( Rnw, main=outputname) {
    if (sub(".*\\.tex$", "TeX", Rnw) == "TeX") 
    	outputname <- Rnw
    else
    	outputname <- Sweave(Rnw)
    result <- system(paste("latex -include-directory=", file.path(R.home("share"), "texmf"),
                           " --src ", main, sep=""), intern=FALSE, show=TRUE)
    if (result != 0) Sys.sleep(10)
    patchDVI(sub("\\.tex", ".dvi", main))
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
    .Call(setDviSpecials, bytes, as.character(newspecials))
    if (newname == f)
    	seek(con, 0)
    else {
    	close(con)
    	con <- file(newname, "wb")
    }
    writeBin(bytes, con)
}

patchDVI <- function(f, newname=f) {
    specials <- DVIspecials(f)
    
    concords <- grep("^concordance:", specials, value=TRUE)
    
    parseConcord <- function(split) {
    	oldname <- split[2]
    	newname <- split[3]
    	concordance <- cumsum(as.integer(strsplit(split[4], " ")[[1]]))
    	keep <- !duplicated(concordance)
    	concord <- approxfun(concordance[keep], seq(along=concordance)[keep], "constant", rule=2)
    	list(oldname=oldname, newname=newname, concord=concord)
    }
    concords <- strsplit(concords, ":")
    concords <- lapply(concords, parseConcord)
    names(concords) <- sapply(concords, function(x) x$oldname)


    srcrefind <- grep("^src:", specials)
    srcrefs <- specials[srcrefind]
    
    linenums <- sub("^src:([0-9]+).*$", "\\1", srcrefs)
    filenames <- substr(srcrefs, 5+nchar(linenums), 10000)
    changed <- rep(FALSE, length(filenames))
    for (n in names(concords)) {
    	subset <- filenames == n
    	linenums[subset] <- concords[[n]]$concord(as.integer(linenums[subset]))
    	filenames[subset] <- concords[[n]]$newname
    	changed[subset] <- TRUE
    }
    
    newrefs <- ifelse(changed, paste("src:", linenums, filenames, sep=""), NA)
    if (any(toolong <- nchar(newrefs) > nchar(srcrefs))) {
    	warning(paste(srcrefs[toolong], "can't be changed to", newrefs[toolong]))
    	newrefs[toolong] <- ""    
    }
    
    specials <- rep(NA, length(specials))
    specials[srcrefind] <- newrefs
    
    if (any(!is.na(specials)))
    	setDVIspecials(f, specials, newname)
    	
    paste(sum(!is.na(specials)), "patches made.")
}

Sweave <- function(file, driver=RweaveLatex(),
                   syntax=getOption("SweaveSyntax"), ...)
{
    if(is.character(driver))
        driver <- get(driver, mode="function")()
    else if(is.function(driver))
        driver <- driver()


    if(is.null(syntax))
        syntax <- SweaveGetSyntax(file)
    if(is.character(syntax))
        syntax <- get(syntax, mode="list")

    drobj <- driver$setup(file=file, syntax=syntax, ...)
    on.exit(driver$finish(drobj, error=TRUE))

    text <- SweaveReadFile(file, syntax)
    syntax <- attr(text, "syntax")

    mode <- "doc"
    chunknr <- 0
    chunk <- NULL

    namedchunks <- list()
    for(line in text){
        if(any(grep(syntax$doc, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "doc"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            if (!is.null(driver$skipline)) drobj <- driver$skipline(drobj)
            chunk <- NULL
        }
        else if(any(grep(syntax$code, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "code"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "code"
            }
            if (!is.null(driver$skipline)) drobj <- driver$skipline(drobj)
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts,
                                            drobj$options,
                                            driver$checkopts)
            chunk <- NULL
            chunknr <- chunknr+1
            chunkopts$chunknr <- chunknr
        }
        else{
            if(mode=="code" && any(grep(syntax$coderef, line))){
                chunkref <- sub(syntax$coderef, "\\1", line)
                if(!(chunkref %in% names(namedchunks)))
                    warning(gettextf("reference to unknown chunk '%s'",
                                     chunkref), domain = NA)
                line <- namedchunks[[chunkref]]
            }

            if(is.null(chunk))
                chunk <- line
            else
                chunk <- c(chunk, line)
        }
    }
    if(!is.null(chunk)){
        if(mode=="doc") drobj <- driver$writedoc(drobj, chunk)
        else drobj <- driver$runcode(drobj, chunk, chunkopts)
    }

    on.exit()
    driver$finish(drobj)
}

###**********************************************************


RweaveLatex <- function()
{
    list(setup = RweaveLatexSetup,
         runcode = RweaveLatexRuncode,
         writedoc = RweaveLatexWritedoc,
         finish = RweaveLatexFinish,
         checkopts = RweaveLatexOptions,
         skipline = RweaveSkipLine)
}

RweaveLatexSetup <-
    function(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, keep.source=FALSE, split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE)
{
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.tex$", "", output))
    }
    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")

    if(stylepath){
        styfile <- file.path(R.home("share"), "texmf", "Sweave")
        if(.Platform$OS.type == "windows")
            styfile <- gsub("\\\\", "/", styfile)
        if(any(grep(" ", styfile)))
            warning(gettextf("path to '%s' contains spaces,\n", styfile),
                    gettext("this may cause problems when running LaTeX"),
                    domain = NA)
    }
    else
        styfile <- "Sweave"

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=eval,
                    fig=FALSE, pdf=pdf, eps=eps,
                    width=6, height=6, term=TRUE,
                    echo=echo, keep.source=keep.source, results="verbatim", 
                    split=split, strip.white="true", include=TRUE,
                    pdf.version="1.1", pdf.encoding="default",
                    concordance=FALSE)

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveLatexOptions(options)

    list(output=output, styfile=styfile, havesty=FALSE, haveconcordance=FALSE,
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(), linesout=integer(0),
         srcfile=file)
}

makeRweaveLatexCodeRunner <- function(evalFunc=RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    RweaveLatexRuncode <- function(object, chunk, options)
      {
          if(!(options$engine %in% c("R", "S"))){
              return(object)
          }
          
          if(!object$quiet){
              cat(formatC(options$chunknr, width=2), ":")
              if(options$echo) cat(" echo")
              if(options$keep.source) cat(" keep.source")
              if(options$eval){
                  if(options$print) cat(" print")
                  if(options$term) cat(" term")
                  cat("", options$results)
                  if(options$fig){
                      if(options$eps) cat(" eps")
                      if(options$pdf) cat(" pdf")
                  }
              }
              if(!is.null(options$label))
                cat(" (label=", options$label, ")", sep="")
              cat("\n")
          }

          chunkprefix <- RweaveChunkPrefix(options)

          if(options$split){
              chunkout <- object$chunkout[[chunkprefix]]
              if(is.null(chunkout)){
                  chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
                  if(!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
              }
          }
          else
            chunkout <- object$output

	  saveopts <- options(keep.source=options$keep.source)
	  on.exit(options(saveopts))
	  
          SweaveHooks(options, run=TRUE)

          chunkexps <- try(parse(text=chunk), silent=TRUE)
          RweaveTryStop(chunkexps, options)
          openSinput <- FALSE
          openSchunk <- FALSE
          
          linesout <- rep(0, length(chunk))

          if(length(chunkexps)==0) {
            object$linesout <- c(object$linesout, linesout)
            return(object)
          }
            
	  srcrefs <- attr(chunkexps, "srcref")
	  lastshown <- 0
	  thisline <- 1
          for(nce in 1:length(chunkexps))
            {
                ce <- chunkexps[[nce]]
                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                    srcfile <- attr(srcref, "srcfile")
                    dce <- getSrcLines(srcfile, lastshown+1, srcref[3])
	    	    leading <- srcref[1]-lastshown
	    	    lastshown <- srcref[3]
	    	    thisline <- srcref[3]
	    	    while (length(dce) && length(grep("^[ \\t]*$", dce[1]))) {
	    		dce <- dce[-1]
	    		leading <- leading - 1
	    	    }
	    	} else {
                    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                    leading <- 1
                }
                if(object$debug)
                  cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                if(options$echo && length(dce)){
                    if(!openSinput){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline] <- linesout[thisline] + 1    
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Sinput}",
                            file=chunkout, append=TRUE)
                        openSinput <- TRUE
                    }
		    cat("\n", paste(getOption("prompt"), dce[1:leading], sep="", collapse="\n"),
		    	file=chunkout, append=TRUE, sep="")
                    if (length(dce) > leading)
                    	cat("\n", paste(getOption("continue"), dce[-(1:leading)], sep="", collapse="\n"),
                    	    file=chunkout, append=TRUE, sep="")
                    linesout[thisline] <- linesout[thisline] + length(dce)
                }

                                        # tmpcon <- textConnection("output", "w")
                                        # avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL
                if(options$eval) err <- evalFunc(ce, options)
                cat("\n") # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if(length(output)==1 & output[1]=="") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                  cat(paste(output, collapse="\n"))

                if(length(output)>0 & (options$results != "hide")){

                    if(openSinput){
                        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                        linesout[thisline] <- linesout[thisline] + 2
                        openSinput <- FALSE
                    }
                    if(options$results=="verbatim"){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline] <- linesout[thisline] + 1
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Soutput}\n",
                            file=chunkout, append=TRUE)
                        linesout[thisline] <- linesout[thisline] + 1
                    }

                    output <- paste(output,collapse="\n")
                    if(options$strip.white %in% c("all", "true")){
                        output <- sub("^[[:space:]]*\n", "", output)
                        output <- sub("\n[[:space:]]*$", "", output)
                        if(options$strip.white=="all")
                          output <- sub("\n[[:space:]]*\n", "\n", output)
                    }
                    cat(output, file=chunkout, append=TRUE)
                    linesout[thisline] <- linesout[thisline] + 
                                            sum(strsplit(output, NULL)[[1]] == "\n")

                    remove(output)

                    if(options$results=="verbatim"){
                        cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
                        linesout[thisline] <- linesout[thisline] + 2
                    }
                }
            }

          if(openSinput){
              cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
              linesout[thisline] <- linesout[thisline] + 2
          }

          if(openSchunk){
              cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
              linesout[thisline] <- linesout[thisline] + 1
          }

          if(is.null(options$label) & options$split)
            close(chunkout)

          if(options$split & options$include){
              cat("\\input{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
              linesout <- rep(0, length(chunk))
              linesout[thisline] <- 1
          }

          if(options$fig && options$eval){
              if(options$eps){
                  grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                        width=options$width, height=options$height,
                                        paper="special", horizontal=FALSE)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$pdf){
                  grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                                 width=options$width, height=options$height,
                                 version=options$pdf.version,
                                 encoding=options$pdf.encoding)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$include) {
                  cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                      file=object$output, append=TRUE)
                  linesout[thisline] <- linesout[thisline] + 1
              }
          }
          object$linesout <- c(object$linesout, linesout)
          return(object)
      }
    RweaveLatexRuncode
}

RweaveLatexRuncode <- makeRweaveLatexCodeRunner()

RweaveLatexWritedoc <- function(object, chunk)
{
    linesout <- rep(1, length(chunk))
    
    if(any(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE

    if(!object$havesty){
 	begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
 	which <- grep(begindoc, chunk)
 	if (length(which)) {
            chunk[which] <- sub(begindoc,
                                paste("\\\\usepackage{",
                                object$styfile,
                                "}\n\\\\begin{document}", sep=""),
                                chunk[which])
            linesout[which] <- linesout[which] + 1
            object$havesty <- TRUE        
        }
    }

    while(any(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1]])
        cmd <- substr(chunk[pos[1]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0), because sub() will fail
            if(length(val)==0) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
    }
    while(any(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        if (!is.null(object$options$concordance) 
              && object$options$concordance 
              && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep=".")
            chunk[pos[1]] <- sub(object$syntax$docopt, 
                                 paste("\\\\input{", prefix, "}", sep=""),
                                 chunk[pos[1]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }
    
    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)
    
    return(object)
}

RweaveLatexFinish <- function(object, error=FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$srcfile
    if(!object$quiet && !error)
        cat("\n",
            gettextf("You can now run LaTeX on '%s'", outputname),
            "\n", sep = "")
    close(object$output)
    if(length(object$chunkout) > 0)
        for(con in object$chunkout) close(con)
    if (object$haveconcordance) {
    	concordance <- paste(strwrap(paste(object$linesout, collapse=" ")), collapse=" %\n")
    	special <- paste("\\special{concordance:", outputname, ":", inputname, ":%\n",
    			 concordance,"}\n", sep="")
    	cat(special, file=object$concordfile)
    }
    invisible(outputname)
}

RweaveSkipLine <- function(object)
{
    object$linesout <- c(object$linesout, 0)
    return(object)
}

SweaveGetSyntax <- utils:::SweaveGetSyntax
SweaveReadFile <- utils:::SweaveReadFile
SweaveParseOptions <- utils:::SweaveParseOptions

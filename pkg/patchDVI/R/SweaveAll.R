knitAll <- function(RnwFiles = NULL, make=1, PostKnitHook=NULL, 
                      force = TRUE, verbose = FALSE, 
                      weave=knitr::knit,
                                 ...) 
    SweaveAll(SweaveFiles = RnwFiles,
              make = make,
              PostSweaveHook = PostKnitHook,
              force = force,
              verbose = verbose,
              weave = weave,
              ...)

SweaveAll <- function(SweaveFiles = NULL, make=1, PostSweaveHook=NULL, 
                      force = TRUE, verbose = FALSE, 
                      weave=utils::Sweave, ...) {
    updateSweaveFiles <- function(new, old) {
        if (make) {
            newfiles <- setdiff(new, old)
            if (length(newfiles)) {
                if (make == 1) {
                    tex <- paste(tools::file_path_sans_ext(newfiles), ".tex", sep="")
                    newfiles <- newfiles[!file_test("-f", tex) | file_test("-nt", newfiles, tex)]
                } 
                if (verbose && length(newfiles))
                    cat("Added to SweaveFiles: ", paste(newfiles, collapse = ","), "\n")
                SweaveFiles <<- c(old, newfiles)
            }
        }        
    }
    getVar <- function(names, default) {
        for (n in names)
            if (exists(n, envir=globalenv(), inherits = FALSE)) 	
                return(get(n, envir=globalenv(), inherits = FALSE))
        default
    }
    i <- 0
    result <- character()
    if (!force) {
        updateSweaveFiles(SweaveFiles, NULL)
    }
    while (i < length(SweaveFiles)) {
        i <- i+1
        suppressWarnings(remove(".SweaveFiles", ".TexRoot", ".PostSweaveHook", ".SweaveMake",
                                envir=globalenv()))
        thisfile <- weave(SweaveFiles[i], ...)
    	result <- c(result, thisfile)
    	.PostSweaveHook <- getVar(c(".PostSweaveHook", ".PostKnitHook"), PostSweaveHook)
        make <- getVar(c(".SweaveMake", ".knitMake"), make)
    	if (!is.null(.PostSweaveHook)) {
    	    .PostSweaveHook <- match.fun(.PostSweaveHook)
    	    .PostSweaveHook(thisfile)
    	}
        updates <- getVar(c(".SweaveFiles", ".knitFiles"), NULL)
    	if (!is.null(updates))
    	  updateSweaveFiles(updates, SweaveFiles)
        TexRoot <- getVar(".TexRoot", NULL)
        if (!is.null(TexRoot)) 
            result <- c(TexRoot, result[result != TexRoot])
    }
    result
}

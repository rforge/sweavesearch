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
    	.PostSweaveHook <- PostSweaveHook
    	if (exists(".PostSweaveHook", envir=globalenv(), inherits = FALSE)) 	
    	    .PostSweaveHook <- get(".PostSweaveHook", envir=globalenv(), inherits = FALSE)
    	if (exists(".SweaveMake", envir=globalenv(), inherits = FALSE))
    	    make <- get(".SweaveMake", envir=globalenv(), inherits = FALSE)
    	if (!is.null(.PostSweaveHook)) {
    	    .PostSweaveHook <- match.fun(.PostSweaveHook)
    	    .PostSweaveHook(thisfile)
    	}
    	if (exists(".SweaveFiles", envir = globalenv(), inherits = FALSE))
    	  updateSweaveFiles(get(".SweaveFiles", globalenv(), inherits = FALSE), SweaveFiles)
        if (exists(".TexRoot", envir=globalenv(), inherits = FALSE)) {
            TexRoot <- get(".TexRoot", globalenv(), inherits = FALSE)
            result <- c(TexRoot, result[result != TexRoot])
        }
    }
    result
}

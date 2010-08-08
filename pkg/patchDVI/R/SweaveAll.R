SweaveAll <- function(SweaveFiles, make=1, ...) {
    i <- 0
    result <- character()
    while (i < length(SweaveFiles)) {
        i <- i+1
        suppressWarnings(remove(".SweaveFiles", ".TexRoot", envir=globalenv()))
    	result <- c(result, Sweave(SweaveFiles[i], ...))
    	if (make && exists(".SweaveFiles", envir=globalenv())) {
    	    newfiles <- setdiff(get(".SweaveFiles", globalenv()), SweaveFiles)
            if (length(newfiles)) {
            	if (make == 1) {
            	    tex <- paste(tools::file_path_sans_ext(newfiles), ".tex", sep="")
            	    SweaveFiles <- c(SweaveFiles, newfiles[file_test("-nt", newfiles, tex)])
            	} else 
            	    SweaveFiles <- c(SweaveFiles, newfiles)
            }
        }
        if (exists(".TexRoot", envir=globalenv())) {
            TexRoot <- get(".TexRoot", globalenv())
            result <- c(TexRoot, result[result != TexRoot])
        }
    }
    result
}

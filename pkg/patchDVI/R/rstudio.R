knitInRStudio <- function(SweaveFiles = NULL, 
			  force = FALSE, verbose = FALSE) {
    inRStudio <- Sys.getenv("RSTUDIO") == "1"
    if (inRStudio) {
	.childOutput <- character() # Just in case...
	knitr::knit_hooks$set(childOutput = function(before, options, envir)   {
	    if (!before) {
		cat(.childOutput, sep = "\n")
	    }
	})
	
	if (missing(SweaveFiles) && exists(".SweaveFiles", envir=globalenv(), inherits = FALSE))
	    SweaveFiles <- get(".SweaveFiles", envir=globalenv(), inherits = FALSE)
	if (length(SweaveFiles))
    	    .childOutput <- system2("Rscript", 
    	        paste("-e \"patchDVI::SweaveAll(c(", 
    	            paste0("'", SweaveFiles, "'", collapse = ","),
    		    "), weave = knitr::knit, force =", force, ", verbose = ", verbose, ")\""),
    		stdout = TRUE, stderr = TRUE)
    }
}
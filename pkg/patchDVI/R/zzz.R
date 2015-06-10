JSweave <- function(file, ...) {
  tex <- sub("[.][RrSs](nw|tex)$", ".tex", file)
  # Check for commands
  cmds <- Sys.which(c("uplatex", "dvipdfmx"))
  if (any(nchar(cmds) == 0)) {
    warning(gettextf("Vignette '%s' requires 'uplatex' and 'dvipdfmx'", basename(file)))
    lines <- "\\documentclass{article}\\begin{document}
    This document requires ``uplatex'' and ``dvipdfmx''
    \\end{document}"
    writeLines(lines, tex)
    return()
  }
  
  # Need two runs to make TOC.  Skip dvipdfm on the first run,
  # skip Sweave on the second run
  SweaveDVIPDFM(file, latex = "uplatex", dvipdfm = "echo", 
    	          encoding = "UTF-8")
  SweaveDVIPDFM(tex, latex = "uplatex", dvipdfm = "dvipdfmx", 
		  encoding = "UTF-8")
}

.onLoad <- function(libname, pkgname) {
  vignetteEngine("JSweave", weave = JSweave, tangle = Stangle)
}

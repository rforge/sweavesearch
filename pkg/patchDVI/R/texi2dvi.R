# This is extracted from the file in the original copyright notice below,
# and modified slightly for patchDVI.

#  File src/library/tools/R/utils.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


## x without y, as in the examples of ?match.
`%w/o%` <-
function(x, y)
    x[!x %in% y]


texi2dvi <-
function(file, pdf = FALSE, clean = FALSE, quiet = FALSE,
         texi2dvi = getOption("texi2dvi"),
         texinputs = NULL, index = TRUE,
         links = NULL )
{
    ## Run texi2dvi on a latex file, or emulate it.

    if(is.null(texi2dvi) || !nzchar(texi2dvi))
        texi2dvi <- Sys.which("texi2dvi")

    envSep <- .Platform$path.sep
    texinputs0 <- texinputs
    Rtexmf <- file.path(R.home("share"), "texmf")
    if (getRversion() < "2.12.0")
    	Rtexinputs <- Rtexmf
    else
	Rtexinputs <- file.path(Rtexmf, "tex", "latex")
    ## "" forces use of default paths.
    texinputs <- paste(c(texinputs, Rtexinputs, ""), 
                      collapse = envSep)
    ## not clear if this is needed, but works
    if(.Platform$OS.type == "windows")
        texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
    if (getRversion() < "2.12.0")
        Rbstinputs <- Rtexmf
    else 
    	Rbstinputs <- file.path(Rtexmf, "bibtex", "bst")
    bstinputs <- paste(c(texinputs, Rbstinputs, ""), 
                      collapse = envSep)

    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA)
    if(is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    } else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    Sys.setenv(TEXINPUTS = paste(otexinputs, texinputs, sep = envSep))
    bibinputs <- Sys.getenv("BIBINPUTS", unset = NA)
    if(is.na(bibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        bibinputs <- "."
    } else on.exit(Sys.setenv(BIBINPUTS = bibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paste(bibinputs, texinputs, sep = envSep))
    obstinputs <- Sys.getenv("BSTINPUTS", unset = NA)
    if(is.na(obstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        obstinputs <- "."
    } else on.exit(Sys.setenv(BSTINPUTS = obstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paste(obstinputs, bstinputs, sep = envSep))

    if(index && nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        ## switch off the use of texindy in texi2dvi >= 1.157
        Sys.setenv(TEXINDY = "false")
        on.exit(Sys.unsetenv("TEXINDY"), add = TRUE)
        opt_pdf <- if(pdf) "--pdf" else ""
        opt_quiet <- if(quiet) "--quiet" else ""
        opt_extra <- ""
        out <- tools:::.shell_with_capture(paste(shQuote(texi2dvi), "--help"))
        if(length(grep("--no-line-error", out$stdout)))
            opt_extra <- "--no-line-error"
        ## (Maybe change eventually: the current heuristics for finding
        ## error messages in log files should work for both regular and
        ## file line error indicators.)

        file.create(".timestamp")
        out <- tools:::.shell_with_capture(paste(shQuote(texi2dvi), opt_pdf,
                                         opt_quiet, opt_extra,
                                         shQuote(file)))

        ## We cannot necessarily rely on out$status, hence let us
        ## analyze the log files in any case.
        errors <- character()
        ## (La)TeX errors.
        log <- paste(tools:::file_path_sans_ext(file), "log", sep = ".")
        if(file_test("-f", log)) {
            lines <- tools:::.get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                errors <- paste("LaTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
        }
        ## BibTeX errors.
        log <- paste(tools:::file_path_sans_ext(file), "blg", sep = ".")
        if(file_test("-f", log)) {
            lines <- tools:::.get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                errors <- paste("BibTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
        }

        msg <- ""
        if(out$status) {
            ## <NOTE>
            ## If we cannot rely on out$status, we could test for
            ##   if(out$status || length(errors))
            ## But shouldn't we be able to rely on out$status on Unix?
            ## </NOTE>
            msg <- gettextf("Running 'texi2dvi' on '%s' failed.", file)
            ## Error messages from GNU texi2dvi are rather terse, so
            ## only use them in case no additional diagnostics are
            ## available (e.g, makeindex errors).
            if(length(errors))
                msg <- paste(msg, errors, sep = "\n")
            else if(length(out$stderr))
                msg <- paste(msg, "Messages:",
                             paste(out$stderr, collapse = "\n"),
                             sep = "\n")
            if(!quiet)
                msg <- paste(msg, "Output:",
                             paste(out$stdout, collapse = "\n"),
                             sep = "\n")
        }

        ## Clean up as needed.
        if(clean) {
            out_file <- paste(tools:::file_path_sans_ext(file),
                              if(pdf) "pdf" else "dvi",
                              sep = ".")
            files <- list.files(all.files = TRUE) %w/o% c(".", "..",
                                                          out_file)
            file.remove(files[file_test("-nt", files, ".timestamp")])
        }
        file.remove(".timestamp")

        if(nzchar(msg))
            stop(msg, domain = NA)
        else if(!quiet)
            message(paste(paste(out$stderr, collapse = "\n"),
                          paste(out$stdout, collapse = "\n"),
                          sep = "\n"))
    } else if(index && nzchar(texi2dvi)) { # MiKTeX on Windows
        extra <- ""
        if (is.null(links))
            opt_links <- if(pdf) "--tex-option=-synctex=-1" else "--tex-option=--src-specials"
        else
            opt_links <- links
        ext <- if(pdf) "pdf" else "dvi"
        opt_pdf <- if(pdf) "--pdf" else ""
        file.create(".timestamp")
        opt_quiet <- if(quiet) "--quiet" else ""

        ## look for MiKTeX (which this almost certainly is)
        ## and set the path to R's style files.
        ## -I works in MiKTeX >= 2.4, at least
        ## http://docs.miktex.org/manual/texify.html
        cmd <- paste(shQuote(texi2dvi), "--version")
        if (!quiet) message(cmd, "\n")
        ver <- system(cmd, intern = TRUE)
        if(length(grep("MiKTeX", ver[1L]))) {
            ## AFAICS need separate -I for each element of texinputs.
            texinputs <- c(texinputs0, 
                           Rtexinputs,
                           Rbstinputs)
            paths <- paste ("-I", shQuote(texinputs))
            extra <- paste(extra, paste(paths, collapse = " "))
        }
        ## this only gives a failure in some cases, e.g. not for bibtex errors.
        cmd <- paste(shQuote(texi2dvi), opt_quiet, opt_pdf, opt_links,
                     shQuote(file), extra)
        if (!quiet) message(cmd, "\n")
        system(cmd, intern=TRUE, ignore.stderr=TRUE)
        msg <- ""
        ## (La)TeX errors.
        log <- paste(tools:::file_path_sans_ext(file), "log", sep = ".")
        if(file_test("-f", log)) {
            lines <- tools:::.get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                msg <- paste(msg, "LaTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }
        ## BibTeX errors.
        log <- paste(tools:::file_path_sans_ext(file), "blg", sep = ".")
        if(file_test("-f", log)) {
            lines <- tools:::.get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                msg <- paste(msg, "BibTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }

        if(nzchar(msg))
            msg <- paste(gettextf("running 'texi2dvi' on '%s' failed", file),
                         msg, "", sep = "\n")
        if(clean) {
            out_file <- paste(tools:::file_path_sans_ext(file), ext, sep = ".")
            files <- list.files(all.files = TRUE) %w/o% c(".", "..",
                                                          out_file)
            file.remove(files[file_test("-nt", files, ".timestamp")])
        }
        file.remove(".timestamp")

        if(nzchar(msg)) stop(msg, call. = FALSE, domain = NA)
    } else {
        ## Do not have texi2dvi or don't want to index
        ## Needed on Windows except for MiKTeX
        ## Note that this does not do anything about running quietly,
        ## nor cleaning, but is probably not used much anymore.

        ## If it is called with MiKTeX then TEXINPUTS etc will be ignored.

        if (is.null(links))
            opt_links <- if(pdf) "--synctex=-1" else "--src-specials"
        else
            opt_links <- links
        texfile <- shQuote(file)
        base <- tools:::file_path_sans_ext(file)
        idxfile <- paste(base, ".idx", sep="")
        latex <- if(pdf) Sys.getenv("PDFLATEX", "pdflatex")
        else  Sys.getenv("LATEX", "latex")
        bibtex <- Sys.getenv("BIBTEX", "bibtex")
        makeindex <- Sys.getenv("MAKEINDEX", "makeindex")
        cmd <- paste(shQuote(latex), "-interaction=nonstopmode", opt_links, texfile)
        if (!quiet) message(cmd, "\n")
        if(system(cmd))
            stop(gettextf("unable to run '%s' on '%s'", latex, file),
                 domain = NA)
        nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined",
                             readLines(paste(base, ".log", sep = ""))))
        for(iter in 1L:10L) { ## safety check
            ## This might fail as the citations have been included in the Rnw
            if(nmiss) system(paste(shQuote(bibtex), shQuote(base)))
            nmiss_prev <- nmiss
            if(index && file.exists(idxfile)) {
                cmd <- paste(shQuote(makeindex), shQuote(idxfile))
                if (!quiet) message(cmd, "\n")
                if(system(cmd))
                    stop(gettextf("unable to run '%s' on '%s'",
                                  makeindex, idxfile),
                         domain = NA)
            }
            paste(shQuote(latex), "-interaction=nonstopmode", opt_links, texfile)
            if (!quiet) message(cmd, "\n")
            if(system(cmd))
                stop(gettextf("unable to run %s on '%s'", latex, file), domain = NA)
            Log <- readLines(paste(base, ".log", sep = ""))
            nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined", Log))
            if(nmiss == nmiss_prev &&
               !length(grep("Rerun to get", Log)) ) break
        }
    }
}


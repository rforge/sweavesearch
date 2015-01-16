\name{SweaveMiktex}
\alias{SweaveMiktex}
\alias{SweavePDFMiktex}
\alias{SweaveDVI}
\alias{SweavePDF}
\alias{SweaveDVIPDFM}
\title{Simple function to run MikTeX}
\description{
This is a simple script to run Sweave, then MikTeX, then patchDVI. 
}
\usage{

SweaveMiktex(Rnw, main = outputname, 
    cmd = "texify",
    options = "--tex-option=-src-specials --tex-option=-interaction=nonstopmode",
    includedir = "--tex-option=--include-directory=",
    stylepath = FALSE, 
    source.code = NULL, 
    make = 1, 
    preview = 'yap "\%s"', 
    patchLog=TRUE,
    sleep = 0, ...)
		  
SweavePDFMiktex(Rnw, main = outputname,  
    cmd = "texify --pdf", 
    options = "--tex-option=-synctex=-1 --tex-option=-interaction=nonstopmode",
    includedir = "--tex-option=-include-directory=",
    stylepath = FALSE, 
    source.code = NULL, 
    make = 1, 
    preview = NULL, 
    patchLog = TRUE, 
    sleep = 0, ...)
		  
SweaveDVI(Rnw, main=outputname, 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    links = NULL,
    preview = NULL, 
    patchLog = TRUE, ...)

SweavePDF(Rnw, main=outputname, 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    links = NULL,
    preview = NULL, 
    patchLog = TRUE, ...)
	  
SweaveDVIPDFM(Rnw, main = outputname, 
    latex = "latex", latexOpts = "-synctex=1 -interaction=nonstopmode", 
    dvipdfm = "dvipdfm", dvipdfmOpts = "", 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    preview = NULL, 
    patchLog = TRUE, ...)
}
\arguments{
  \item{Rnw}{ The .Rnw input file }
  \item{main}{ The .tex file on which to run latex, defaulting to the output file from Sweave }
  \item{cmd}{ The LaTeX command to run }
  \item{options}{ Options to pass to LaTeX }
  \item{includedir}{ The prefix for an option to give the location of Sweave.sty }
  \item{stylepath}{ \code{stylepath} argument to pass to \code{\link{Sweave}} }
  \item{source.code}{ If not \code{NULL}, a filename to source before running \code{\link{Sweave}} }
  \item{make}{ Value to pass to \code{\link{SweaveAll}} to control which Sweave files are processed }
  \item{preview}{ Command to use to preview result, or \code{NULL} for no preview }
  \item{patchLog}{ Whether to attempt to patch the console log and \file{.log} file }
  \item{sleep}{ Number of seconds to sleep if an error is detected in the run }
  \item{texinputs}{ Extra paths to pass to \code{\link[tools]{texi2dvi}} }
  \item{links}{ If not \code{NULL}, an option to turn on production of source links
      via Synctex or source specials}
  \item{latex,latexOpts}{ The function to call to convert the \file{.tex} to \file{.dvi},
      and options to put on the command line.}
  \item{dvipdfm, dvipdfmOpts}{ The function to call to convert the \file{.dvi} to \file{.pdf},
      and options to put on the command line.}
  \item{...}{ Additional options to pass to \code{\link{SweaveAll}} }
}
\details{
If the \code{Rnw} argument is a filename ending in \file{.tex}, then 
weaving is skipped, but the \file{.dvi} file is still patched (because
one of the other files in the project might have come from Sweave).

The \code{source.code} argument may be used to temporarily install code before
running the document through the weaver (e.g. \code{\link{Sweave}}).  
It could install
an experimental version of \code{\link{Sweave}}, or set up some variables 
to be used by other code chunks, etc.  (Note that things are sourced locally,
whereas \code{\link{Sweave}} runs things in the global environment, 
so the file will need to make explicit assignments there, and it should
avoid using names that clash with the argument names to \code{SweaveMiktex}
or \code{SweavePDFMiktex}.)

The functions invoke Latex differently.  \code{SweaveMiktex} is designed
to call Miktex's \code{texify} command to produce a \file{.dvi} file.  \code{SweavePDFMiktex}
does the same, but produces a \file{.pdf}.  \code{SweaveDVI} and \code{SweavePDF}
are similar, but use a modified version of \R{}'s \code{\link[tools]{texi2dvi}} 
function to invoke Latex. \code{SweaveDVIPDFM} does the conversion to 
\file{.pdf} in two stages.

The \code{patchLog} option makes use of the new \code{\link{patchLog}} function.
}

\value{
These functions are run for the side effects.
}

\examples{
\dontrun{
SweaveMiktex("chapter.Rnw", "book.tex")
}
}
\keyword{ utilities }

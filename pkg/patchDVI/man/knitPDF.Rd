\name{knitPDF}
\alias{knitMiktex}
\alias{knitPDFMiktex}
\alias{knitDVI}
\alias{knitPDF}
\alias{knitDVIPDFM}
\title{Simple functions to knit a file and run it through LaTeX}
\description{
This is a simple script to run knit, then LaTeX, then patchDVI. 
}
\usage{

knitMiktex(Rnw, main, 
    cmd = "texify",
    options = "--tex-option=-src-specials --tex-option=-interaction=nonstopmode",
    includedir = "--tex-option=--include-directory=",
    stylepath = FALSE, 
    source.code = NULL, 
    make = 1, 
    preview = 'yap "\%s"', 
    patchLog=TRUE,
    sleep = 0, 
    weave = knitr::knit,
    ...)
		  
knitPDFMiktex(Rnw, main,  
    cmd = "texify --pdf", 
    options = "--tex-option=-synctex=-1 --tex-option=-interaction=nonstopmode",
    includedir = "--tex-option=-include-directory=",
    stylepath = FALSE, 
    source.code = NULL, 
    make = 1, 
    preview = NULL, 
    patchLog = TRUE, 
    sleep = 0, 
    weave = knitr::knit,
    ...)
		  
knitDVI(Rnw, main, 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    links = NULL,
    preview = NULL, 
    patchLog = TRUE, 
    weave = knitr::knit,
    ...)

knitPDF(Rnw, main, 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    links = NULL,
    preview = NULL, 
    patchLog = TRUE, 
    weave = knitr::knit, 
    ...)
	  
knitDVIPDFM(Rnw, main, 
    latex = "latex", latexOpts = "-synctex=1 -interaction=nonstopmode", 
    dvipdfm = "dvipdfm", dvipdfmOpts = "", 
    texinputs = NULL, 
    source.code = NULL, 
    make = 1, 
    preview = NULL, 
    patchLog = TRUE, 
    weave = knitr::knit,
    ...)
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
  \item{weave}{ The function to carry out the weaving. }
  \item{...}{ Additional options to pass to \code{\link{SweaveAll}} }
}
\details{
If the \code{Rnw} argument is a filename ending in \file{.tex}, then 
knitting is skipped, but the output file is still patched (because
one of the other files in the project might have come from knitr).

The \code{source.code} argument may be used to temporarily install code before
running the document through the weaver (e.g. \code{\link[knitr]{knit}}).  
It could install
an experimental version of \code{\link[knitr]{knit}}, or set up some variables 
to be used by other code chunks, etc.

The functions invoke Latex differently.  \code{knitMiktex} is designed
to call Miktex's \code{texify} command to produce a \file{.dvi} file.  \code{knitPDFMiktex}
does the same, but produces a \file{.pdf}.  \code{knitDVI} and \code{knitPDF}
are similar, but use a modified version of \R{}'s \code{\link[tools]{texi2dvi}} 
function to invoke Latex. \code{knitDVIPDFM} does the conversion to 
\file{.pdf} in two stages.

The \code{patchLog} option makes use of the \code{\link{patchLog}} function.
}

\value{
These functions are run for the side effects.
}

\examples{
\dontrun{
knitPDF("chapter.Rnw", "book.tex")
}
}
\keyword{ utilities }

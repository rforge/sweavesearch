\name{SweaveMiktex}
\alias{SweaveMiktex}
\alias{SweavePDFMiktex}
\alias{SweaveDVI}
\alias{SweavePDF}
\title{Simple function to run MikTeX}
\description{
This is a simple script to run Sweave, then MikTeX, then patchDVI. 
}
\usage{

SweaveMiktex(Rnw, main=outputname, 
                  cmd="texify",
                  options="--tex-option=-c-style-errors --tex-option=--src-specials",
                  includedir="--tex-option=--include-directory=",
                  stylepath=FALSE, source.code=NULL, make=1, ...)
SweavePDFMiktex(Rnw, main=outputname,  
                  cmd="texify --pdf", 
                  options="--tex-option=-c-style-errors --tex-option=-synctex=-1",
                  includedir="--tex-option=-include-directory=",
                  stylepath=FALSE, source.code=NULL, make=1, ...)
SweaveDVI(Rnw, main=outputname, 
          texinputs = NULL, 
          source.code = NULL, 
          stylepath = FALSE, 
          make = 1, 
          links = NULL, ...)

SweavePDF(Rnw, main=outputname, 
          texinputs = NULL, 
          source.code = NULL, 
          stylepath = FALSE, 
          make = 1, 
          links = NULL, ...)

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
  \item{texinputs}{ Extra paths to pass to \code{\link[tools]{texi2dvi}} }
  \item{links}{ If not \code{NULL}, an option to turn on production of source links
      via Synctex or source specials}
  \item{...}{ Additional options to pass to \code{\link{Sweave}} }
}
\details{
If the \code{Rnw} argument is a filename ending in \file{.tex}, then 
\code{Sweave} is skipped, but the \file{.dvi} file is still patched (because
one of the other files in the project might have come from Sweave).

The \code{source.code} argument may be used to temporarily install code before
running the document through \code{\link{Sweave}}.  It could install
an experimental version of \code{\link{Sweave}}, or set up some variables 
to be used by other code chunks, etc.  (Note that things are sourced locally,
whereas \code{\link{Sweave}} runs things in the global environment, 
so the file will need to make explicit assignments there, and it should
avoid using names that clash with the argument names to \code{SweaveMiktex}
or \code{SweavePDFMiktex}.)

The three functions invoke Latex differently.  \code{SweaveMiktex} is designed
to call Miktex's \code{texify} command to produce a \file{.dvi} file.  \code{SweavePDFMiktex}
does the same, but produces a \file{.pdf}.  \code{SweaveDVI} and \code{SweavePDF}
are similar, but use a modified version of \R{}'s \code{\link[tools]{texi2dvi}} 
function to invoke Latex.
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

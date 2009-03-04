\name{SweaveMiktex}
\alias{SweaveMiktex}
\alias{SweavePDFMiktex}
\title{Simple function to run MikTeX}
\description{
This is a simple script to run Sweave, then MikTeX, then patchDVI. 
}
\usage{
SweaveMiktex(Rnw, main=outputname, 
                  cmd="texify",
                  options="--tex-option=-c-style-errors --tex-option=--src-specials",
                  includedir="--tex-option=--include-directory=",
                  stylepath=FALSE, ...)
SweavePDFMiktex(Rnw, main=outputname,  
                  cmd="texify --pdf", 
                  options="--tex-option=-c-style-errors --tex-option=-synctex=-1",
                  includedir="--tex-option=-include-directory=",
                  stylepath=FALSE, ...)
}
\arguments{
  \item{Rnw}{ The .Rnw input file }
  \item{main}{ The .tex file on which to run latex, defaulting to the output file from Sweave }
  \item{cmd}{ The LaTeX command to run }
  \item{options}{ Options to pass to LaTeX }
  \item{includedir}{ The prefix for an option to give the location of Sweave.sty }
  \item{stylepath}{ \code{stylepath} argument to pass to \code{\link{Sweave}} }
  \item{...}{ Additional options to pass to \code{\link{Sweave}} }
}
\details{
If the \code{Rnw} argument is a filename ending in \file{.tex}, then 
\code{Sweave} is skipped, but the \file{.dvi} file is still patched (because
one of the other files in the project might have come from Sweave).
}

\value{
This function is run for the side effects.
}

\examples{
\dontrun{
SweaveMiktex("chapter.Rnw", "book.tex")
}
}
\keyword{ utilities }

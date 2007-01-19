\name{SweaveMiktex}
\alias{SweaveMiktex}
\title{Simple function to run MikTeX}
\description{
This is a simple script to run Sweave, then MikTeX, then patchDVI. 
}
\usage{
SweaveMiktex(Rnw, main = outputname)
}
\arguments{
  \item{Rnw}{ The .Rnw input file }
  \item{main}{ The .tex file on which to run latex, defaulting to the output file from Sweave }
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

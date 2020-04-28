\name{knitInRStudio}
\alias{knitInRStudio}
\title{
Trigger processing of chapter files from RStudio.
}
\description{
If this function is executed within RStudio, it will 
knit all child 
from the main file 
of a large \file{.Rnw}-style knitr document.
}
\usage{
knitInRStudio(SweaveFiles, force = FALSE, verbose = FALSE)
}
\arguments{
  \item{SweaveFiles}{
The files to process.  If missing, the value is set
from a \code{.SweaveFiles} variable in the global environment.
}
  \item{force}{
If \code{TRUE}, all files are knitted; otherwise, only those
that are newer than the corresponding \file{.tex} file.
}
  \item{verbose}{
Add some debugging information to the output.
}
}
\details{
This does nothing if not run from within RStudio.  If run
there, it starts a second R process to do the knitting.  It's 
assumed that this call will exist in a code chunk in another
knitr document.

Set \code{childOutput = TRUE} in the options to the code
chunk containing this if you wish to see the output of the
child knitr runs in the same log as the main one.
}
\value{
The concatenated output from the child knitting, and
possibly some verbose output.
}
\author{
Duncan Murdoch
}

\seealso{
\code{\link{knitAll}}, which does a similar thing outside
of RStudio.
}

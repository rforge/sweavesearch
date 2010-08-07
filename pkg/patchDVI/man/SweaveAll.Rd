\name{SweaveAll}
\alias{SweaveAll}
\title{
Apply Sweave to a collection of files.
}
\description{
This function allows a master Sweave input file to specify other files in the 
project.  All modified files will be run through \code{\link{Sweave}}.
}
\usage{
SweaveAll(SweaveFiles, stylepath = FALSE, make = 1, ...)
}
\arguments{
  \item{SweaveFiles}{character vector naming the files to 
  always run through \code{\link{Sweave}}.  
}
  \item{stylepath}{
  logical value to pass to \code{Sweave}.
}
  \item{make}{
  integer indicating which files to run \code{Sweave} on.  See below.
}
  \item{\dots}{
  Extra parameters to pass to \code{Sweave}.
}
}
\details{
This function runs \code{Sweave} repeatedly.  It starts by running \code{Sweave}
on each file listed in \code{SweaveFiles}.

Following those runs, the \code{make} parameter controls which other
files are run.  The default value of \code{1} works as follows:  If
any of the Sweave code in the previous files set the global variable
\code{.SweaveFiles} to a character vector of filenames, then any new
files in that variable that were not already processed are checked. If
they are newer than the corresponding \file{.tex} file, or the
\file{.tex} file does not exist, they are processed.  They may also
return new filenames in the global \code{.SweaveFiles} variable and
those will be processed as well.

Setting \code{make = 2} forces all mentioned files to be processed.
With \code{make = 0}, only the original vector of files passed to
\code{SweaveAll} will be processed.

\code{SweaveAll} also checks for a global variable named \code{.TexRoot}.
If present, it will be placed in the first position in the result vector.
(If it was already there, later copies will be removed.)
}
\value{
The names of all files produced.  The first element of
\code{Sweavefiles} will produce the first element of the result if
\code{.TexRoot} was not specified; other elements will depend on which
files were processed.
}
\author{
Duncan Murdoch
}
\examples{
\dontrun{
# Run Sweave on a main file, and obtain other files from there.
SweaveAll("main.Rnw")
}}

\keyword{ utilities }

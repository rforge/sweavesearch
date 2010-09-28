\name{SweaveAll}
\alias{SweaveAll}
\alias{.SweaveFiles}
\alias{.TexRoot}
\alias{.PostSweaveHook}
\title{
Apply Sweave to a collection of files.
}
\description{
This function allows a master Sweave input file to specify other files in the 
project.  All modified files will be run through \code{\link{Sweave}}.  Following
the run, several post-processing steps may be performed.
}
\usage{
SweaveAll(SweaveFiles, make = 1, PostSweaveHook = NULL, ...)
}
\arguments{
  \item{SweaveFiles}{character vector naming the files to 
  always run through \code{\link{Sweave}}.  
}
  \item{make}{
  integer indicating which files to run \code{Sweave} on.  See below.
}
  \item{PostSweaveHook}{
  default hook to be called after each \code{Sweave} call.  See below.
}
  \item{\dots}{
  Extra parameters to pass to \code{Sweave}.
}
}
\details{
This function runs \code{Sweave} repeatedly. It starts by running \code{Sweave}
on each file listed in \code{SweaveFiles}.

Before each run, it removes the global variables
\code{.PostSweaveHook}, \code{.SweaveFiles}, and \code{.TexRoot}.  The
individual Sweave files may set these variables to control post-
processing as described below.

Following each run, \code{SweaveAll} checks whether the Sweave code
chunks created a variable \code{.PostSweaveHook} in the global
environment.  If present, it should be a function (or the name of a
function; \code{\link{match.fun}} is used to do the lookup) taking a
single argument.  If not present, the \code{PostSweaveHook} argument
will be used as a default.  \code{SweaveAll} will pass the name of the
\file{.tex} output file to this function, which can do any required
postprocessing.  For example, we have used it to remove local
pathnames from output strings.

\code{SweaveAll} also checks for \code{.SweaveFiles}, which if present
should be a character vector of filenames.  These are collected, and
after the \code{Sweave} runs specified by \code{SweaveFiles}, the
\code{make} parameter controls which other files are run.  The default
value of \code{1} works as follows:  any new files added via
\code{.SweaveFiles} that were not already processed are checked. If
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

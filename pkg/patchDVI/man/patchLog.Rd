\name{patchLog}
\encoding{UTF-8}
\alias{patchLog}
\title{
Patch a LaTeX log file.
}
\description{
This function parses a standard LaTeX log file and applies
patches to it based on an Sweave concordance, so that error
messages and warnings refer to the \file{.Rnw} file rather than the
\file{.tex} file.
}
\usage{
patchLog(f, newname = f, concords = NULL, max_print_line = 79, Cstyle = NA)
}
\arguments{
  \item{f}{
The filename of the log file.
}
  \item{newname}{
The filename to write the patched file to.
}
  \item{concords}{
Concordances read from the \file{.dvi} or \file{.pdf}.  If \code{NULL},
an attempt is made to read these from the source.
}
  \item{max_print_line}{
The line length at which the latex compiler will wrap error messages.
}
  \item{Cstyle}{
If \code{TRUE}, only look for C-style error messages \code{"filename:num: msg".
If \code{FALSE}, look for classic messages \code{"! msg"}.  If
\code{NA}, look for both, pick the more popular style.
}
\details{
The parsing is based on the log parser written by 
Jonathan Kew, Stefan Löffler, Antonio Macrì, and Henrik Skov Midtiby for 
TeXWorks.  It assumes error lines are reported in the standard format;
C style errors are not currently supported.
}
\value{
This function is called mainly for the side effect of writing the
new log file.  It returns the \code{concords} object invisibly.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{patchDVI}}, \code{\link{patchSynctex}}
}
\keyword{utilities}

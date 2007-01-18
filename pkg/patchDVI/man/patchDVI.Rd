\name{patchDVI}
\alias{patchDVI}
\alias{patchDVI-package}
\title{patch Sweave source references into .dvi file}
\description{
This package is a temporary package to test some code that allows
source references in .dvi files to refer to the actual source file, 
rather than the intermediate .tex file.  Dvi previewers such as yap
and xdvi can make use of these source references to jump directly
to the part of a document corresponding to a particular source line, and
the previewer can send messages to editors to jump to the source
corresponding to a location in the preview. 
}
\usage{
patchDVI(f, newname=f)
}
\arguments{
  \item{f}{ A character value giving the name of the .dvi file to patch }
  \item{newname}{ Output will go to this file; by default to overwrite the original }
}
\details{
This function works with patched versions of \code{Sweave} and some related functions;
if successful, these changes will eventually be merged into R.

Using \code{patchDVI} requires a change to the usual Sweave workflow, as follows:

1.  Write the source file in .Rnw.  Include the \\SweaveOpts{concordance=TRUE} option to
enable the patching.

2.  Use the Sweave from this package to process the file, and convert to .tex format.  (See
its web page for details of what difference this makes.

3.  Use latex to process the .tex file into a .dvi file.

4.  Run \code{patchDVI} on the .dvi file to patch in the Rnw links.

5.  Use the previewer on the patched .dvi file.

All the information needed for the patching is stored in the .dvi file, so these 5 steps
can be run from the command line, there's no need to keep an R session running (though they'll 
all work from within one session as well).  At some distant date in the future maybe
the previewers will learn how to read the concordance info, and step 4 will no longer be 
necessary.
}
\value{
A string containing a message about what was done.
}
\references{ The yap previewer is part of MikTeX, from www.miktex.org. }
\author{ Duncan Murdoch }
\seealso{\code{\link{DVIspecials}} for the function to read .dvi files,
\code{\link{SweaveMiktex}} for a simple script to run Sweave, MikTeX and patchDVI.}
\examples{
\dontrun{
patchDVI("mypaper.dvi")
}
}
\keyword{utilities}

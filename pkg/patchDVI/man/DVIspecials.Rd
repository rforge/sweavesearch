\name{DVIspecials}
\alias{DVIspecials}
\alias{setDVIspecials}
\title{ Get and set DVI specials }
\description{
These functions get and set the \\specials values in .dvi files.
}
\usage{
DVIspecials(f)
setDVIspecials(f, newspecials, newname=f)
}
\arguments{
  \item{f}{ character filename of .dvi file to patch }
  \item{newspecials}{ character vector of new specials to put into the file }
  \item{newname}{ the name of the file to write }
}
\details{
If an entry of the \code{newspecials} vector is \code{NA}, that special
is removed.  
}
\value{
\code{DVIspecials} returns all the specials from the file in a character vector.

\code{setDVIspecials} is called for the side effect of patching the .dvi file;
it returns \code{NULL} invisibly.
}
\author{ Duncan Murdoch }
\seealso{ \code{\link{patchDVI}} }
\examples{
\dontrun{
x <- DVIspecials("test.dvi")
x[1] <- "new value"
setDVIspecials("test.dvi", x)
}
}
\keyword{ utilities }

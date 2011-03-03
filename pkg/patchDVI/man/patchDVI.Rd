\name{patchDVI}
\alias{patchDVI}
\alias{patchSynctex}
\alias{patchDVI-package}
\title{patch Sweave source references into .dvi file}
\description{
This package allows
source references in .dvi files to refer to the actual source file, 
rather than the intermediate .tex file.  Dvi previewers such as yap
and xdvi can make use of these source references to jump directly
to the part of a document corresponding to a particular source line, and
the previewer can send messages to editors to jump to the source
corresponding to a location in the preview. 
}
\usage{
patchDVI(f, newname=f)
patchSynctex(f, newname=f, uncompress="pdftk \%s output \%s uncompress")
}
\arguments{
  \item{f}{ A character value giving the name of the .dvi or .synctex file to patch }
  \item{newname}{ Output will go to this file; by default to overwrite the original }
  \item{uncompress}{ External command to uncompress a compressed PDF file. }
}
\details{
This function works with \code{Sweave} with option \code{concordance=TRUE}
and some related functions.

Using \code{patchDVI} requires a change to the usual Sweave workflow, as follows:

1.  Write the source file in .Rnw.  Include the \preformatted{\SweaveOpts{concordance=TRUE}} option to
enable the patching.

2.  Use Sweave to convert the file to .tex format.

3.  Use latex to process the .tex file into a .dvi file or .pdf file.

4.  Run \code{patchDVI} on the .dvi file or the .synctex file to patch in the Rnw links.

5.  Use the previewer on the patched .dvi file or the original .pdf file.

All the information needed for the patching is stored in the .dvi file, so these 5 steps
can be run from the command line, there's no need to keep an R session running (though they'll 
all work from within one session as well).  At some distant date in the future maybe
the previewers will learn how to read the concordance info, and step 4 will no longer be 
necessary.
}
\note{

In a .dvi file, the concordance is stored in a TeX "special" in the following format (as of version 0.5):
\code{concordance:<oldname>:<newname>:<firstline> <rle pairs>*}.  Here \code{<oldname>}
is the name of the .tex file to replace, \code{<newname>} is the name of the .Rnw file
to substitute for it, \code{<firstline>} is the line number from the .Rnw file of the
first line in the .tex file.  

The \code{<rle pairs>} are a sequence of pairs
(length, value), separated by spaces (no parentheses), with the length indicating
the number of repetitions of the value.  Using \code{\link{inverse.rle}} these are converted
to a sequence of differences in the line numbers of the concordance.

For example, a concordance might contain
\preformatted{\Sconcordance{\concordance:optimization.tex:optimization.Rnw:1 2 1 2 4 39 1 1 4 ...}} 
saying that source references to optimization.tex should be replaced with
optimization.Rnw.  Line 1 of the .tex file corresponds to line 1 of the .Rnw file.
The diffs for successive lines are \code{1 1 4 4 1 <repeated 39 times> 4 ...}, so 
the corresponding lines in the .Rnw file (including the first) are \code{1 2 3 7 11 12 13 ...}.

The same format is used in an unreferenced indirect object in a .pdf file.  This is not always
successfully read by the patching code, because some versions of \code{pdflatex} 
compress parts of the file, and \code{patchDVI} currently can't read compressed parts
of files.  We suggest installing the \code{pdftk} open source PDF toolkit 
(available from \url{http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/}) to decompress
the file, and/or use the Latex command \verb{\pdfobjcompresslevel=0} at the start
of your document.  The \code{uncompress} argument can be modified to call a different
uncompress utility.

This format is subject to change in future R releases.
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

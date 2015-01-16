\name{defSconcordance}
\alias{defSconcordance}
\alias{useknitr}
\title{
Support for non-Sweave concordances.
}
\description{
Sweave automatically inserts concordances into the output file,
so they will be embedded in the \file{.dvi} or \file{.pdf} output using the LaTeX macro \verb{\\Sconcordance} from
the \file{Sweave.sty} style file.
Other vignette processing engines (e.g. knitr) do not 
use \file{Sweave.sty}, so \code{defSconcordance}
defines the macro explicitly.

The \code{useknitr} function is specific to knitr documents.
If \code{writeMacro} is \code{TRUE}, it calls \code{defSconcordance}
to write the macro definition to the output file.  If \code{writeMacro} is not specified, it guesses the value by looking for 
\verb{\\begin{document}} somewhere in the first 100 lines of the 
file.  
It always writes the \verb{\\input} line for the concordances into the file. 
}
\usage{
defSconcordance()
useknitr(writeMacro)
}
\arguments{
  \item{writeMacro}{
  logical; whether to call defSconcordance
  }
}
\details{
This function is not necessary when using Sweave, it is only for other vignette engines.

With knitr, the concordances would be written to a file with a
name typically be of the form \file{basename-concordance.tex}, where
the input file was \file{basename.Rnw}.  You would insert it with code like
\preformatted{
<<results="asis">>=
patchDVI::useknitr()
@
}

If the document does use the \file{Sweave.sty} style, or if it
is being included in another larger document, do 
not use \code{defSconcorance}:  this can be forced by
calling \code{patchDVI::useknitr(writeMacro = FALSE)}.
}
\value{
These functions are called for the side effect of writing
out the LaTeX code.
}
\author{
Duncan Murdoch
}
\examples{
defSconcordance()
}


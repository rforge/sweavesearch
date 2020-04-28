\name{needsPackages}
\alias{needsPackages}
\title{
Check for and optionally install packages used by document.
}
\description{
This function checks for needed packages
}
\usage{
needsPackages(pkgs, install = TRUE, 
                    update = FALSE, 
                    load = FALSE, 
                    attach = FALSE)
}
\arguments{
  \item{pkgs}{
A list of names of packages to check.
}
  \item{install}{
Whether to install missing packages.
}
  \item{update}{
Whether to update all packages that can be updated.
  }
  \item{load}{
Whether to load namespaces for packages.
  }
  \item{attach}{
Whether to run library calls.
  }
}
\details{
Each of the boolean arguments can be a vector
of the same length as \code{pkgs}, applying separately
to each entry.
}
\value{
Called for the side effects.
}

\examples{
# This shouldn't do anything, as patchDVI must already be
# installed!
needsPackages("patchDVI")
}

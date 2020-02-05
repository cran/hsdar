\name{read.ASD}
\alias{read.ASD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read ASD binary file
}
\description{
Read spectra stored in ASD binary files using the package 'asdreader'.
}
\usage{
  read.ASD(f, type = "reflectance", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
    Vector with files names to be read.
}
\item{type}{
  Character vector, which type of spectra to return. See ?get_spectra for options.
}
\item{...}{
  Additional arguments passed to get_spectra. Currently ignored.
}
}


\author{
Lukas Lehnert
}
\value{
  Object of class \code{Speclib}.
}
\seealso{
  \code{\link{speclib}}
}

\keyword{ utilities }

\name{Nri-methods}
\alias{$,Nri-method}
\alias{as.matrix,Nri-method}
\alias{as.data.frame,Nri-method}
\alias{show,Nri-method}
\alias{print,Nri-method}
\alias{wavelength,Nri-method}
\alias{[,Nri,ANY,ANY-method}
\alias{[,Nri,ANY,ANY,ANY-method}
\alias{getFiniteNri}
%\alias{as.matrix.nri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods for * Nri-class
}
\description{
Methods to handle data in objects of class Nri.
}
\usage{
\S4method{as.matrix}{Nri}(x, ..., named_matrix = TRUE)

\S4method{as.data.frame}{Nri}(x, na.rm = FALSE, ...)

\S4method{wavelength}{Nri}(object)

getFiniteNri(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,object}{
List of class 'Nri'
}
\item{na.rm}{Remove indices containing NA-values. Note that if TRUE, all indices are removed which have at least one NA value.}
  \item{named_matrix}{
Flag if column and row names are set to band indices used for the calculation of the nri-values.
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
%  \item{named_matrix}{
%Flag if column names should be set to band names and row names to ids of spectra.
%}

}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glm.nri}}, \code{\link{glm}}, \code{\link{nri}}
}
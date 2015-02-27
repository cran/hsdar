\name{attribute}
\alias{attribute}
\alias{attribute.speclib}
\alias{attribute<-}
\alias{attribute,Speclib-method}
\alias{attribute<-,Speclib,data.frame-method}
\alias{attribute<-,Speclib,matrix-method}
% \alias{setattributes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling attributes of spectra
}
\description{
Returning and setting attributes of spectra in speclib.% \code{setattributes} is a more comfortable and save function to set attributes of speclib using string matching. 
}
\usage{
\S4method{attribute}{Speclib}(object)

\S4method{attribute}{Speclib,data.frame}(object) <- value

\S4method{attribute}{Speclib,matrix}(object) <- value
%setattributes(x, attributes, matchcol = NULL, fuzzy = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{Speclib}.
}
  \item{value}{
Data frame with \code{nrow(value) == nspectra(object)}, or NULL.
}
%\item{attributes}{
%Data frame with \code{nrow(attributes) >= nspectra(x)}.
%}
% \item{matchcol}{
% Column in \code{attribute} to match against id of speclib. May be character string or integer. If NULL, function searches for column "id".
% }
% \item{fuzzy}{
% If TRUE, fuzzy matching of column names is used. Matching of IDs is NOT affected by this parameter!
% }
% \item{...}{
% Further arguments passed to \code{\link{agrep}}.
% }
}
\value{
For \code{attribute<-}, the updated object. \code{attribute} returns a data frame with attribute data.% The return value of \code{setattributes} is an object of class speclib.
}
\author{
Lukas Lehnert
}

\seealso{
\code{\linkS4class{Speclib}}%, \code{\link{agrep}}
}
\examples{
data(spectral_data)

## Returning attributes
attribute(spectral_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

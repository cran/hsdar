\name{apply.Speclib}
\alias{apply.Speclib}
\alias{apply,Speclib-method}

\title{
Apply function for class Speclib
}
\description{
Apply function to all spectra or a subset of spectra
}
\usage{
\S4method{apply}{Speclib}(X, FUN, byattributes = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Object of class \code{Speclib}
}
  \item{FUN}{
Function to be applied. Matched with \code{\link{match.fun}}.
}
  \item{byattributes}{
Currently not implemeted
}
\item{...}{
Further arguments passed to FUN.
}
}

\value{
Object of class Speclib.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{apply}}, \code{\link{match.fun}}, \code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

mean <- apply(spectral_data, FUN=mean)
plot(mean)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}


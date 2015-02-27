\name{dim.speclib}
\alias{dim.speclib}
\alias{dim,Speclib-method}
\alias{nspectra}
\alias{nbands}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dimensions of Speclib
}
\description{
Get dimension(s) of Speclib
}
\usage{
\S4method{dim}{Speclib}(x)

nspectra(x)
nbands(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
}

\value{
Vector of length = 2 or single integer value.
}
\author{
Lukas Lehnert
}


\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

dim(spectral_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

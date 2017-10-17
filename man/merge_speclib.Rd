\name{merge}
\alias{merge,Speclib,Speclib-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge speclibs
}
\description{
Merge 2 \code{Speclib}s and their SI data 
}
\usage{
\S4method{merge}{Speclib,Speclib}(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
1st Object of class \code{Speclib} to be merged.
}
  \item{y}{
2nd Object of class \code{Speclib} to be merged.
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
}

\value{
Object of class \code{Speclib}.
}
\author{
Lukas Lehnert
}

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)
sp1 <- spectral_data[c(1:10),]
sp2 <- spectral_data[c(11:20),]

speclib_merged <- merge(sp1, sp2)
}
\keyword{utilities}

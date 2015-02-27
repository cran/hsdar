\name{merge}
\alias{merge,Speclib,Speclib-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge speclibs
}
\description{
Merge 2 speclibs and their attributes data 
}
\usage{
\S4method{merge}{Speclib,Speclib}(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
  \item{y}{
Object of class \code{Speclib}. Speclib to add
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
}

\value{
Object of class Speclib.
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
